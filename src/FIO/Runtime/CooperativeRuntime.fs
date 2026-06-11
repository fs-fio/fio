module FIO.Runtime.Cooperative

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

module private CooperativePolling =
    let BatchSize = 256
    let PendingQueueCapacityMultiplier = 2
    let ChannelSpinWaitIterations = 256
    let ChannelSpinMissThreshold = 4_096
    let ChannelYieldMissThreshold = 65_536
    let FiberSpinWaitIterations = 128
    let FiberSpinMissThreshold = 512
    let FiberYieldMissThreshold = 8_192
    let FiberDelayStep1MissThreshold = 16_384
    let FiberDelayStep2MissThreshold = 32_768
    let FiberDelayStep1Ms = 1
    let FiberDelayStep2Ms = 2
    let FiberDelayStep3Ms = 4

type private EvaluationWorkerConfig =
    {
        Runtime: CooperativeRuntime
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        BlockingWorker: BlockingWorker
        EWSteps: int
    }

and internal BlockingWorkerConfig =
    {
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        ActiveBlockingItemChan: UnboundedChannel<BlockingEntry>
    }

and [<Struct>] internal BlockingEntry =
    {
        Item: BlockingItem
        MissCount: int
    }

and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan, config.BlockingWorker)

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}"
        <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemChan.WaitToReadAsync token

                    if not hasWorkItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemChan.ReadAsync()

                        if not (workItem.FiberContext.IsTerminal()) then
                            try
                                do! processWorkItem workItem
                            with exn ->
                                try
                                    workItem.FiberContext.Complete(Error(exn :> obj))
                                with _ ->
                                    ()

                                raise exn
            }

    interface IDisposable with

        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

and internal BlockingWorker(config: BlockingWorkerConfig, workerId: int) =

    let batchSize = CooperativePolling.BatchSize

    let pendingQueueCapacity =
        batchSize * CooperativePolling.PendingQueueCapacityMultiplier

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
        | BlockingChannel(chan, _) -> chan.Count > 0
        | BlockingFiber(fiberContext, _) -> fiberContext.IsTerminal()

    let getWorkItem (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel(_, workItem) -> workItem
        | BlockingFiber(_, workItem) -> workItem

    let queueActive (workItem: WorkItem) =
        task {
            let addVt = config.ActiveWorkItemChan.WriteAsync workItem

            if not addVt.IsCompletedSuccessfully then
                do! addVt.AsTask()
        }

    let applyBackoff (maxChannelMiss: int, maxFiberMiss: int) =
        task {
            if maxChannelMiss > 0 then
                if maxChannelMiss < CooperativePolling.ChannelSpinMissThreshold then
                    Thread.SpinWait CooperativePolling.ChannelSpinWaitIterations
                elif maxChannelMiss < CooperativePolling.ChannelYieldMissThreshold then
                    Thread.Yield() |> ignore
                else
                    do! Task.Yield()
            elif maxFiberMiss > 0 then
                if maxFiberMiss < CooperativePolling.FiberSpinMissThreshold then
                    Thread.SpinWait CooperativePolling.FiberSpinWaitIterations
                elif maxFiberMiss < CooperativePolling.FiberYieldMissThreshold then
                    do! Task.Yield()
                else
                    let delayMs =
                        if maxFiberMiss < CooperativePolling.FiberDelayStep1MissThreshold then
                            CooperativePolling.FiberDelayStep1Ms
                        elif maxFiberMiss < CooperativePolling.FiberDelayStep2MissThreshold then
                            CooperativePolling.FiberDelayStep2Ms
                        else
                            CooperativePolling.FiberDelayStep3Ms

                    do! Task.Delay delayMs
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

                    if (getWorkItem entry).FiberContext.IsTerminal() then
                        ()
                    elif isReady entry then
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

        while drained < batchSize && config.ActiveBlockingItemChan.TryRead(&entry) do
            enqueuePending entry
            drained <- drained + 1

    let waitForFirstIfNeeded (ct: CancellationToken) =
        task {
            if hasPending () then
                return true
            else
                let! hasBlockingItem = config.ActiveBlockingItemChan.WaitToReadAsync ct

                if not hasBlockingItem || ct.IsCancellationRequested then
                    return false
                else
                    let! blockingEntry = config.ActiveBlockingItemChan.ReadAsync()
                    enqueuePending blockingEntry
                    return true
        }

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}"
        <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasItemToProcess = waitForFirstIfNeeded token

                    if not hasItemToProcess || token.IsCancellationRequested then
                        loop <- false
                    else
                        tryDrainIncoming ()
                        do! processBatch ()
            }

    interface IDisposable with

        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

    member internal _.RescheduleForBlocking blockingItem =
        config.ActiveBlockingItemChan.WriteAsync { Item = blockingItem; MissCount = 0 }

and CooperativeRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    let activeBlockingItemChan = UnboundedChannel<BlockingEntry>()

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let struct (blockingWorkers, evaluationWorkers) =
        WorkerBuilders.buildPairedWorkers
            config.BWC
            config.EWC
            (fun i ->
                new BlockingWorker(
                    {
                        ActiveWorkItemChan = activeWorkItemChan
                        ActiveBlockingItemChan = activeBlockingItemChan
                    },
                    i
                ))
            (fun i blockingWorker ->
                new EvaluationWorker(
                    {
                        Runtime = this
                        ActiveWorkItemChan = activeWorkItemChan
                        BlockingWorker = blockingWorker
                        EWSteps = config.EWS
                    },
                    i
                ))

    override _.Name = "CooperativeRuntime"

    interface IDisposable with

        member _.Dispose () =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    new() = new CooperativeRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal _.InterpretAsync
        (
            workItem: WorkItem,
            evalSteps: int,
            activeWorkItemChan: UnboundedChannel<WorkItem>,
            blockingWorker: BlockingWorker
        ) =
        let mutable state =
            InterpreterState(workItem.Eff, workItem.Stack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEWSteps = evalSteps
        let currentFiberContext = workItem.FiberContext

        let inline onSuccessComplete value =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete(Ok value)

        let inline onErrorComplete error =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete(Error error)

        task {
            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ -> raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error error -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeInterrupt error)
                    elif currentEWSteps = 0 then
                        if activeWorkItemChan.Count > 0 then
                            let newWorkItem = WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                            newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                            do! activeWorkItemChan.WriteAsync newWorkItem
                            state.Completed <- true
                        else
                            currentEWSteps <- evalSteps
                    else
                        currentEWSteps <- currentEWSteps - 1

                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                let writeTask = chan.WriteAsync msg

                                if not writeTask.IsCompletedSuccessfully then
                                    do! writeTask

                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess msg)
                            | HandleReceiveChan chan ->
                                let mutable value = Unchecked.defaultof<_>

                                if chan.UnboundedChannel.TryRead(&value) then
                                    processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! blockingWorker.RescheduleForBlocking <| BlockingChannel(chan, newWorkItem)
                                    state.Completed <- true
                            | HandleForkEffect(eff, fiber, fiberContext) ->
                                let _registration = setupForkRegistration currentFiberContext fiberContext
                                let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                                do! activeWorkItemChan.WriteAsync workItem
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess fiber)
                            | HandleJoinFiber fiberContext ->
                                if fiberContext.IsTerminal() then
                                    let! value = fiberContext.Task
                                    processResult &state onSuccessComplete onErrorComplete value
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed

                                    do! blockingWorker.RescheduleForBlocking <| BlockingFiber(fiberContext, newWorkItem)

                                    state.Completed <- true
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            task
                                        else
                                            task.WaitAsync currentFiberContext.CancellationToken

                                    processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested
                                    ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupt (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Task has been cancelled."
                                        )))
                                | exn -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeError (onError exn))

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
        }

    member private _.Reset() =
        activeWorkItemChan.Clear()
        activeBlockingItemChan.Clear()

    override _.Run<'A, 'E>(eff: FIO<'A, 'E>) : Fiber<'A, 'E> =
        lock runLock (fun () ->
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
                WorkItemPool.Rent(eff.UpcastBoth(), fiber.Context, ContStackPool.Rent())

            activeWorkItemChan.WriteAsync workItem |> ignore

            fiber)
