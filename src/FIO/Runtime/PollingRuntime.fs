module FIO.Runtime.Polling

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

module private PollingTuning =
    let BatchSize = 256

    let PendingQueueCapacityMultiplier = 2

    let ChannelSpinWaitIterations = 256

    let ChannelSpinMissThreshold = 4_096

    let ChannelYieldMissThreshold = 65_536

    let FiberSpinWaitIterations = 128

    let FiberSpinMissThreshold = 512

    let FiberColdMissThreshold = 65_536

    let ColdSleepMilliseconds = 1

[<Struct>]
type private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

type private EvaluationWorkerConfig =
    {
        Runtime: PollingRuntime
        ActiveWorkItemQueue: MailboxQueue<WorkItem>
        BlockingWorker: BlockingWorker
        EvaluationSteps: int
    }

and internal BlockingWorkerConfig =
    {
        ActiveWorkItemQueue: MailboxQueue<WorkItem>
        BlockingEntryQueue: MailboxQueue<BlockingEntry>
    }

and [<Struct>] internal BlockingEntry =
    {
        Item: BlockingItem
        MissCount: int
    }

and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync workItem config.EvaluationSteps config.ActiveWorkItemQueue config.BlockingWorker

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}"
        <| fun cancellationToken ->
            task {
                let mutable loop = true

                while loop && not cancellationToken.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemQueue.WaitToReadAsync cancellationToken

                    if not hasWorkItem || cancellationToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemQueue.ReadAsync()

                        if not (workItem.FiberContext.IsCompleted()) then
                            let fiberContext = workItem.FiberContext
                            try
                                do! processWorkItem workItem
                            with ex ->
                                try
                                    fiberContext.Complete(Error(defectError fiberContext ex))
                                with _ ->
                                    ()

                                Console.Error.WriteLine
                                    $"FIO EvaluationWorker-{workerId} recovered from an unhandled effect exception: {ex}"
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

and internal BlockingWorker(config: BlockingWorkerConfig, workerId: int) =
    let batchSize = PollingTuning.BatchSize

    let pendingQueueCapacity =
        batchSize * PollingTuning.PendingQueueCapacityMultiplier

    let mutable preferChannelFirst = true

    let channelPending = Queue<BlockingEntry>(pendingQueueCapacity)

    let fiberPending = Queue<BlockingEntry>(pendingQueueCapacity)

    let hasPending () =
        channelPending.Count > 0 || fiberPending.Count > 0

    let enqueuePending (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel _ -> channelPending.Enqueue entry
        | BlockingFiber _ -> fiberPending.Enqueue entry

    let tryTakePending (preferChannel: bool, entry: byref<BlockingEntry>) =
        if preferChannel then
            if channelPending.Count > 0 then
                entry <- channelPending.Dequeue()
                true
            elif fiberPending.Count > 0 then
                entry <- fiberPending.Dequeue()
                true
            else
                false
        else if fiberPending.Count > 0 then
            entry <- fiberPending.Dequeue()
            true
        elif channelPending.Count > 0 then
            entry <- channelPending.Dequeue()
            true
        else
            false

    let isReady (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel(channel, _) -> channel.Count > 0
        | BlockingFiber(fiberContext, _) -> fiberContext.IsTerminal()

    let getWorkItem (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel(_, workItem) -> workItem
        | BlockingFiber(_, workItem) -> workItem

    let queueActive (workItem: WorkItem) =
        task {
            let addVt = config.ActiveWorkItemQueue.WriteAsync workItem

            if not addVt.IsCompletedSuccessfully then
                do! addVt.AsTask()
        }

    let applyBackoff (maxChannelMiss: int, maxFiberMiss: int, minFiberMiss: int) (cancellationToken: CancellationToken) =
        task {
            if maxChannelMiss > 0 then
                if maxChannelMiss < PollingTuning.ChannelSpinMissThreshold then
                    Thread.SpinWait PollingTuning.ChannelSpinWaitIterations
                elif maxChannelMiss < PollingTuning.ChannelYieldMissThreshold then
                    Thread.Yield() |> ignore
                else
                    do! Task.Delay(PollingTuning.ColdSleepMilliseconds, cancellationToken)
            elif maxFiberMiss > 0 then
                if maxFiberMiss < PollingTuning.FiberSpinMissThreshold then
                    Thread.SpinWait PollingTuning.FiberSpinWaitIterations
                elif minFiberMiss < PollingTuning.FiberColdMissThreshold || channelPending.Count > 0 then
                    do! Task.Yield()
                else
                    do! Task.Delay(PollingTuning.ColdSleepMilliseconds, cancellationToken)
        }

    let processBatch (cancellationToken: CancellationToken) =
        task {
            let mutable processed = 0
            let mutable maxChannelMiss = 0
            let mutable maxFiberMiss = 0
            let mutable minFiberMiss = Int32.MaxValue
            let mutable entry = Unchecked.defaultof<BlockingEntry>
            let mutable hasEntry = true
            let mutable preferChannel = preferChannelFirst

            while processed < batchSize && hasEntry do
                hasEntry <- tryTakePending (preferChannel, &entry)
                preferChannel <- not preferChannel

                if hasEntry then
                    processed <- processed + 1
                    let blockedFiber = (getWorkItem entry).FiberContext

                    if blockedFiber.IsCompleted() then
                        ()
                    elif blockedFiber.IsInterrupted() || isReady entry then
                        do! queueActive (getWorkItem entry)
                    else
                        let missed = { entry with MissCount = entry.MissCount + 1 }
                        enqueuePending missed

                        match missed.Item with
                        | BlockingChannel _ ->
                            if missed.MissCount > maxChannelMiss then
                                maxChannelMiss <- missed.MissCount
                        | BlockingFiber _ ->
                            if missed.MissCount > maxFiberMiss then
                                maxFiberMiss <- missed.MissCount

                            if missed.MissCount < minFiberMiss then
                                minFiberMiss <- missed.MissCount

            preferChannelFirst <- not preferChannelFirst

            if maxChannelMiss > 0 || maxFiberMiss > 0 then
                do! applyBackoff (maxChannelMiss, maxFiberMiss, minFiberMiss) cancellationToken
        }

    let tryDrainIncoming () =
        let mutable drained = 0
        let mutable entry = Unchecked.defaultof<BlockingEntry>

        while drained < batchSize && config.BlockingEntryQueue.TryRead(&entry) do
            enqueuePending entry
            drained <- drained + 1

    let waitForFirstIfNeeded (cancellationToken: CancellationToken) =
        task {
            if hasPending () then
                return true
            else
                let! hasBlockingItem = config.BlockingEntryQueue.WaitToReadAsync cancellationToken

                if not hasBlockingItem || cancellationToken.IsCancellationRequested then
                    return false
                else
                    let! blockingEntry = config.BlockingEntryQueue.ReadAsync()
                    enqueuePending blockingEntry
                    return true
        }

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}"
        <| fun cancellationToken ->
            task {
                let mutable loop = true

                while loop && not cancellationToken.IsCancellationRequested do
                    let! hasItemToProcess = waitForFirstIfNeeded cancellationToken

                    if not hasItemToProcess || cancellationToken.IsCancellationRequested then
                        loop <- false
                    else
                        tryDrainIncoming ()
                        do! processBatch cancellationToken
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

    member internal _.RescheduleForBlocking blockingItem =
        config.BlockingEntryQueue.WriteAsync { Item = blockingItem; MissCount = 0 }

/// A multi-threaded runtime with custom fibers and linear-time handling of blocked fibers (polling).
and PollingRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemQueue = MailboxQueue<WorkItem>()

    let blockingEntryQueue = MailboxQueue<BlockingEntry>()



    let struct (blockingWorkers, evaluationWorkers) =
        WorkerBuilders.buildPairedWorkers
            config.BlockingWorkers
            config.EvaluationWorkers
            (fun i ->
                new BlockingWorker(
                    {
                        ActiveWorkItemQueue = activeWorkItemQueue
                        BlockingEntryQueue = blockingEntryQueue
                    },
                    i
                ))
            (fun i blockingWorker ->
                new EvaluationWorker(
                    {
                        Runtime = this
                        ActiveWorkItemQueue = activeWorkItemQueue
                        BlockingWorker = blockingWorker
                        EvaluationSteps = config.EvaluationSteps
                    },
                    i
                ))

    do this.StopWorkers <- fun () ->
        blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
        evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    override _.Name = "PollingRuntime"

    /// Creates the runtime with the default worker configuration.
    new() = new PollingRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal this.InterpretAsync
        (workItem: WorkItem)
        (evaluationSteps: int)
        (activeWorkItemQueue: MailboxQueue<WorkItem>)
        (blockingWorker: BlockingWorker) =
        let mutable state =
            InterpreterState(
                workItem.Effect,
                workItem.ContStack,
                workItem.FiberContext,
                workItem.InterruptionSuppressed)

        let mutable currentEvaluationSteps = evaluationSteps
        let currentFiberContext = workItem.FiberContext

        let mutable completionAction = NoCompletion

        let inline onSuccessComplete value =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteSuccess value

        let inline onErrorComplete error =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteFailure error

        task {
            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ ->
                            raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error error ->
                            processOutcome
                                &state
                                onSuccessComplete
                                onErrorComplete
                                (OutcomeInterrupted error)
                    elif currentEvaluationSteps = 0 then
                        if activeWorkItemQueue.Count > 0 then
                            let newWorkItem =
                                resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                            do! activeWorkItemQueue.WriteAsync newWorkItem
                            state.Completed <- true
                        else
                            currentEvaluationSteps <- evaluationSteps
                    else
                        currentEvaluationSteps <- currentEvaluationSteps - 1
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone ->
                            ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleWriteChan(message, channel, reportAccepted) ->
                                match tryWriteChannel channel message reportAccepted with
                                | Written result ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded result)
                                | MustWait ->
                                    parkUntilWritable
                                        channel
                                        state.Effect
                                        currentFiberContext
                                        state.ContStack
                                        state.InterruptionSuppressed
                                        (fun workItem -> activeWorkItemQueue.WriteAsync workItem |> ignore)
                                    state.Completed <- true
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead(&value) then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                else
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    do! blockingWorker.RescheduleForBlocking <| BlockingChannel(channel, newWorkItem)
                                    state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext, daemon) ->
                                attachFork currentFiberContext fiberContext daemon (state.InterruptionSuppressed > 0)
                                if daemon then this.TrackDaemon fiberContext
                                let workItem = WorkItemPool.Rent(effect, fiberContext, ContStackPool.Rent())
                                do! activeWorkItemQueue.WriteAsync workItem
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded fiber)
                            | HandleJoinFiber fiberContext ->
                                if fiberContext.IsTerminal() then
                                    let! value = fiberContext.Task
                                    processResult
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        value
                                else
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    do! blockingWorker.RescheduleForBlocking <| BlockingFiber(fiberContext, newWorkItem)
                                    state.Completed <- true
                            | HandleJoinFirst fiberContexts ->
                                match tryFindTerminalIndex fiberContexts with
                                | index when index >= 0 ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box index))
                                | _ ->
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    parkJoinFirstOnHooks fiberContexts currentFiberContext state.InterruptionSuppressed newWorkItem activeWorkItemQueue
                                    state.Completed <- true
                            | HandleJoinAllFailFast fiberContexts ->
                                match tryCompleteJoinAll fiberContexts with
                                | ValueSome outcome ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box outcome))
                                | ValueNone ->
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    parkJoinAllFailFastOnQueue fiberContexts currentFiberContext state.InterruptionSuppressed newWorkItem activeWorkItemQueue
                                    state.Completed <- true
                            | HandleAwaitTask(awaited, onError) ->
                                let waited = awaitedTask awaited state.InterruptionSuppressed currentFiberContext

                                if waited.IsCompletedSuccessfully then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded waited.Result)
                                else
                                    parkOnTask
                                        waited
                                        currentFiberContext
                                        state.ContStack
                                        state.InterruptionSuppressed
                                        onError
                                        (fun workItem -> activeWorkItemQueue.WriteAsync workItem |> ignore)

                                    state.Completed <- true
                match completionAction with
                | CompleteSuccess value ->
                    do! currentFiberContext.CompleteAndReschedule(Ok value, activeWorkItemQueue)
                | CompleteFailure error ->
                    do! currentFiberContext.CompleteAndReschedule(Error error, activeWorkItemQueue)
                | NoCompletion -> ()

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack
                WorkItemPool.Return workItem
        }

    /// Schedules the given effect on a new fiber and returns its handle at once; it never waits for, interrupts, or
    /// discards a fiber already running, so call it as often as you like.
    override this.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        let fiber = new Fiber<'A, 'E>()
        this.Track fiber.Context

        let workItem =
            WorkItemPool.Rent(effect.UpcastBoth(), fiber.Context, ContStackPool.Rent())

        activeWorkItemQueue.WriteAsync workItem |> ignore

        fiber
