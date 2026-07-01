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
        <| fun cancelToken ->
            task {
                let mutable loop = true

                while loop && not cancelToken.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemQueue.WaitToReadAsync cancelToken

                    if not hasWorkItem || cancelToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemQueue.ReadAsync()

                        if not (workItem.FiberContext.IsCompleted()) then
                            try
                                do! processWorkItem workItem
                            with ex ->
                                try
                                    workItem.FiberContext.Complete(Error(ex :> obj))
                                with _ ->
                                    ()

                                raise ex
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

    let applyBackoff (maxChannelMiss: int, maxFiberMiss: int) =
        task {
            if maxChannelMiss > 0 then
                if maxChannelMiss < PollingTuning.ChannelSpinMissThreshold then
                    Thread.SpinWait PollingTuning.ChannelSpinWaitIterations
                elif maxChannelMiss < PollingTuning.ChannelYieldMissThreshold then
                    Thread.Yield() |> ignore
                else
                    do! Task.Yield()
            elif maxFiberMiss > 0 then
                if maxFiberMiss < PollingTuning.FiberSpinMissThreshold then
                    Thread.SpinWait PollingTuning.FiberSpinWaitIterations
                else
                    do! Task.Yield()
        }

    let processBatch () =
        task {
            let mutable processed = 0
            let mutable maxChannelMiss = 0
            let mutable maxFiberMiss = 0
            let mutable entry = Unchecked.defaultof<BlockingEntry>
            let mutable hasEntry = true

            while processed < batchSize && hasEntry do
                hasEntry <- tryTakePending (preferChannelFirst, &entry)

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

            preferChannelFirst <- not preferChannelFirst

            if maxChannelMiss > 0 || maxFiberMiss > 0 then
                do! applyBackoff (maxChannelMiss, maxFiberMiss)
        }

    let tryDrainIncoming () =
        let mutable drained = 0
        let mutable entry = Unchecked.defaultof<BlockingEntry>

        while drained < batchSize && config.BlockingEntryQueue.TryRead(&entry) do
            enqueuePending entry
            drained <- drained + 1

    let waitForFirstIfNeeded (cancelToken: CancellationToken) =
        task {
            if hasPending () then
                return true
            else
                let! hasBlockingItem = config.BlockingEntryQueue.WaitToReadAsync cancelToken

                if not hasBlockingItem || cancelToken.IsCancellationRequested then
                    return false
                else
                    let! blockingEntry = config.BlockingEntryQueue.ReadAsync()
                    enqueuePending blockingEntry
                    return true
        }

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}"
        <| fun cancelToken ->
            task {
                let mutable loop = true

                while loop && not cancelToken.IsCancellationRequested do
                    let! hasItemToProcess = waitForFirstIfNeeded cancelToken

                    if not hasItemToProcess || cancelToken.IsCancellationRequested then
                        loop <- false
                    else
                        tryDrainIncoming ()
                        do! processBatch ()
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

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

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

    override _.Name = "PollingRuntime"

    interface IDisposable with

        member _.Dispose () =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    /// Creates the runtime with the default worker configuration.
    new() = new PollingRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal _.InterpretAsync
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

        let inline onSuccessComplete value =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete <| Ok value

        let inline onErrorComplete error =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete <| Error error

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
                            let newWorkItem = WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                            newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
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
                            | HandleWriteChan(message, channel) ->
                                let writeTask = channel.WriteAsync message
                                if not writeTask.IsCompletedSuccessfully then
                                    do! writeTask
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded message)
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
                                        WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! blockingWorker.RescheduleForBlocking <| BlockingChannel(channel, newWorkItem)
                                    state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext) ->
                                let _ = setupForkRegistration currentFiberContext fiberContext
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
                                        WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! blockingWorker.RescheduleForBlocking <| BlockingFiber(fiberContext, newWorkItem)
                                    state.Completed <- true
                            | HandleAwaitTask(awaited, onError) ->
                                let waited =
                                    if state.InterruptionSuppressed > 0 then
                                        awaited
                                    else
                                        awaited.WaitAsync currentFiberContext.CancellationToken

                                if waited.IsCompletedSuccessfully then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded waited.Result)
                                else
                                    let fiberContext = currentFiberContext
                                    let suppressed = state.InterruptionSuppressed
                                    let contStack = state.ContStack

                                    let resume () =
                                        let resumeEffect =
                                            if waited.IsCompletedSuccessfully then
                                                Success waited.Result
                                            elif waited.IsCanceled
                                                 && fiberContext.CancellationToken.IsCancellationRequested then
                                                Interrupt(ExplicitInterrupt, "Task has been cancelled.")
                                            else
                                                let ex =
                                                    match waited.Exception with
                                                    | null -> OperationCanceledException() :> exn
                                                    | aggregate ->
                                                        match aggregate.InnerException with
                                                        | null -> aggregate :> exn
                                                        | inner -> inner
                                                let error =
                                                    try onError ex
                                                    with _ -> ex :> obj
                                                Failure error

                                        let resumeWorkItem =
                                            {
                                                Effect = resumeEffect
                                                FiberContext = fiberContext
                                                ContStack = contStack
                                                InterruptionSuppressed = suppressed
                                            }

                                        try
                                            activeWorkItemQueue.WriteAsync resumeWorkItem |> ignore
                                        with _ ->
                                            ()

                                    waited.GetAwaiter().OnCompleted(Action resume)
                                    state.Completed <- true
                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack
                WorkItemPool.Return workItem
        }

    member private _.Reset () =
        activeWorkItemQueue.Clear()
        blockingEntryQueue.Clear()

    override _.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        lock runLock <| fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task.GetAwaiter().GetResult() |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.Cancel()
            | None -> ()

            this.Reset()
            let fiber = new Fiber<'A, 'E>()
            currentFiber <- Some fiber.Context

            let workItem =
                WorkItemPool.Rent(effect.UpcastBoth(), fiber.Context, ContStackPool.Rent())

            activeWorkItemQueue.WriteAsync workItem |> ignore

            fiber
