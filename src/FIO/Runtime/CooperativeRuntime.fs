/// Provides the cooperative (work-stealing) runtime for interpreting FIO effects.
module FIO.Runtime.Cooperative

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

/// Backoff tuning constants for cooperative blocking workers.
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

/// Evaluation worker configuration.
type private EvaluationWorkerConfig =
    {
        Runtime: CooperativeRuntime
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        BlockingWorker: BlockingWorker
        EWSteps: int
        Admission: IFiberAdmission
        Monitor: WorkerHealthMonitor
        RestartPolicy: WorkerRestartPolicy
    }

/// Blocking worker configuration.
and internal BlockingWorkerConfig =
    {
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        ActiveBlockingItemChan: UnboundedChannel<BlockingEntry>
        Monitor: WorkerHealthMonitor
        RestartPolicy: WorkerRestartPolicy
    }

/// Blocking item with miss count for backoff.
and [<Struct>] internal BlockingEntry = { Item: BlockingItem; MissCount: int }

/// Worker that evaluates FIO effects.
and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(
            workItem,
            config.EWSteps,
            config.ActiveWorkItemChan,
            config.BlockingWorker,
            config.Admission
        )

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker config.Monitor config.RestartPolicy $"EvaluationWorker-{workerId}" Evaluation ignore
        <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync token

                    if not hasWorkItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemChan.TakeAsync()

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

/// Handles blocked effects via linear-time polling with progressive backoff.
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
            let addVt = config.ActiveWorkItemChan.AddAsync workItem

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

        while drained < batchSize && config.ActiveBlockingItemChan.TryTake(&entry) do
            enqueuePending entry
            drained <- drained + 1

    let waitForFirstIfNeeded (ct: CancellationToken) =
        task {
            if hasPending () then
                return true
            else
                let! hasBlockingItem = config.ActiveBlockingItemChan.WaitToTakeAsync ct

                if not hasBlockingItem || ct.IsCancellationRequested then
                    return false
                else
                    let! blockingEntry = config.ActiveBlockingItemChan.TakeAsync()
                    enqueuePending blockingEntry
                    return true
        }

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker config.Monitor config.RestartPolicy $"BlockingWorker-{workerId}" Blocking (fun () ->
            channelPending.Clear()
            fiberPending.Clear()
            preferChannelFirst <- true)
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

    /// Submits a blocking item to the blocking worker for periodic polling.
    /// <param name="blockingItem">The blocking item to reschedule.</param>
    member internal _.RescheduleForBlocking blockingItem =
        config.ActiveBlockingItemChan.AddAsync { Item = blockingItem; MissCount = 0 }

/// Runtime using custom fibers with linear-time polling for blocked fiber handling.
and CooperativeRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    let activeBlockingItemChan = UnboundedChannel<BlockingEntry>()

    let admission = FiberAdmission.fromConfig config

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let blockingWorkers =
        List.init config.BWC
        <| fun i ->
            new BlockingWorker(
                {
                    ActiveWorkItemChan = activeWorkItemChan
                    ActiveBlockingItemChan = activeBlockingItemChan
                    Monitor = this.Monitor
                    RestartPolicy = WorkerRestartPolicy.Default
                },
                i
            )

    let evaluationWorkers =
        List.init config.EWC
        <| fun i ->
            let blockingWorker = blockingWorkers.[i % blockingWorkers.Length]

            new EvaluationWorker(
                {
                    Runtime = this
                    ActiveWorkItemChan = activeWorkItemChan
                    BlockingWorker = blockingWorker
                    EWSteps = config.EWS
                    Admission = admission
                    Monitor = this.Monitor
                    RestartPolicy = WorkerRestartPolicy.Default
                },
                i
            )

    override _.Name = "CooperativeRuntime"

    interface IDisposable with
        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

            match admission with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    new() = new CooperativeRuntime(WorkerConfig.Default)

    /// Interprets an FIO effect with step-limited evaluation.
    [<TailCall>]
    member internal _.InterpretAsync
        (
            workItem: WorkItem,
            evalSteps: int,
            activeWorkItemChan: UnboundedChannel<WorkItem>,
            blockingWorker: BlockingWorker,
            admission: IFiberAdmission
        ) =
        let mutable state =
            InterpreterState(workItem.Eff, workItem.Stack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEWSteps = evalSteps
        let currentFiberContext = workItem.FiberContext

        let inline onSuccessComplete res =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete(Ok res)

        let inline onErrorComplete err =
            ContStackPool.Return state.ContStack
            currentFiberContext.Complete(Error err)

        task {
            let mutable workItemOwnership = true

            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ -> raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error err -> processInterruptError &state onErrorComplete err
                    elif currentEWSteps = 0 then
                        let newWorkItem = WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                        newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                        do! activeWorkItemChan.AddAsync newWorkItem
                        workItemOwnership <- false
                        state.Completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1

                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.SendAsync msg
                                processSuccess &state onSuccessComplete msg
                            | HandleReceiveChan chan ->
                                let mutable res = Unchecked.defaultof<_>

                                if chan.UnboundedChannel.TryTake(&res) then
                                    processSuccess &state onSuccessComplete res
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(ReceiveChan chan, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! blockingWorker.RescheduleForBlocking <| BlockingChannel(chan, newWorkItem)
                                    workItemOwnership <- false
                                    state.Completed <- true
                            | HandleForkEffect(eff, fiber, fiberContext) ->
                                if admission.TryAcquire() then
                                    fiberContext.SetOnTerminal(fun () -> admission.Release())
                                    let _registration = setupForkRegistration currentFiberContext fiberContext
                                    let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                                    do! activeWorkItemChan.AddAsync workItem
                                    processSuccess &state onSuccessComplete fiber
                                else
                                    do! Task.Yield()
                            | HandleForkTask(taskFactory, onError, fiber, fiberContext) ->
                                if admission.TryAcquire() then
                                    fiberContext.SetOnTerminal(fun () -> admission.Release())
                                    let registration = setupForkRegistration currentFiberContext fiberContext

                                    do!
                                        Task.Run(fun () ->
                                            task {
                                                try
                                                    try
                                                        let t = taskFactory ()
                                                        let! result = t.WaitAsync fiberContext.CancellationToken
                                                        fiberContext.Complete(Ok result)
                                                    with
                                                    | :? OperationCanceledException ->
                                                        fiberContext.Complete(
                                                            Error(
                                                                onError (
                                                                    FiberInterruptedException(
                                                                        fiberContext.Id,
                                                                        ExplicitInterrupt,
                                                                        "Task has been cancelled."
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    | exn -> fiberContext.Complete(Error(onError exn))
                                                finally
                                                    registration.Dispose()
                                            }
                                            :> Task)

                                    processSuccess &state onSuccessComplete fiber
                                else
                                    do! Task.Yield()
                            | HandleJoinFiber fiberContext ->
                                if fiberContext.IsTerminal() then
                                    let! res = fiberContext.Task
                                    InterpreterCore.processResult &state onSuccessComplete onErrorComplete res
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(JoinFiber fiberContext, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed

                                    do! blockingWorker.RescheduleForBlocking <| BlockingFiber(fiberContext, newWorkItem)

                                    workItemOwnership <- false
                                    state.Completed <- true
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! res =
                                        if state.InterruptionSuppressed > 0 then
                                            task
                                        else
                                            task.WaitAsync currentFiberContext.CancellationToken

                                    processSuccess &state onSuccessComplete res
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested
                                    ->
                                    processInterruptError
                                        &state
                                        onErrorComplete
                                        (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Task has been cancelled."
                                        ))
                                | exn -> processError &state onErrorComplete (onError exn)

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
        }

    member private _.Reset() =
        activeWorkItemChan.Clear()
        activeBlockingItemChan.Clear()
        admission.Reset()

    /// Submits an effect for evaluation and returns a Fiber handle.
    /// <param name="eff">The effect to interpret.</param>
    /// <returns>A fiber whose task completes with the effect's result.</returns>
    /// <remarks>
    /// Run is intended for sequential invocation on a given runtime instance.
    /// If a prior fiber on this runtime is still running, Run blocks the calling
    /// thread until that fiber completes. Workers run on dedicated LongRunning
    /// threads, so this blocking does not starve the runtime itself, but
    /// calling Run concurrently from thread-pool threads is not recommended.
    /// Concurrency within a single Run is provided by fibers.
    /// </remarks>
    override _.Run<'R, 'E>(eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match this.Monitor.State with
            | Faulted reason -> raise (RuntimeFaultedException reason)
            | Degraded _
            | Healthy ->

                match currentFiber with
                | Some fiberContext when not (fiberContext.IsTerminal()) ->
                    fiberContext.Task.GetAwaiter().GetResult() |> ignore
                | _ -> ()

                match currentFiber with
                | Some fiberContext -> fiberContext.CancelToken()
                | None -> ()

                this.Reset()
                let fiber = new Fiber<'R, 'E>()
                currentFiber <- Some fiber.Context

                // Acquire a permit for the root fiber.
                admission.TryAcquire() |> ignore
                fiber.Context.SetOnTerminal(fun () -> admission.Release())

                let workItem =
                    WorkItemPool.Rent(eff.UpcastBoth(), fiber.Context, ContStackPool.Rent())

                activeWorkItemChan.AddAsync workItem |> ignore

                fiber)
