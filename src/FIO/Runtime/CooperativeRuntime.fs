/// <summary>Provides the cooperative FIO runtime, which interprets effects on dedicated worker threads with linear-time blocked-fiber polling.</summary>
module FIO.Runtime.Cooperative

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

/// <summary>Represents polling configuration constants for the cooperative runtime's blocking worker backoff strategy.</summary>
module private CooperativePolling =
    /// <summary>Returns the maximum number of blocking entries processed per polling batch.</summary>
    let BatchSize = 256
    /// <summary>Returns the multiplier applied to <c>BatchSize</c> to derive the initial pending-queue capacity.</summary>
    let PendingQueueCapacityMultiplier = 2
    /// <summary>Returns the number of spin-wait iterations used for channel-blocked entries below the spin-miss threshold.</summary>
    let ChannelSpinWaitIterations = 256
    /// <summary>Returns the miss count below which channel-blocked entries use spin-waiting.</summary>
    let ChannelSpinMissThreshold = 4_096
    /// <summary>Returns the miss count below which channel-blocked entries use thread-yield instead of task-yield.</summary>
    let ChannelYieldMissThreshold = 65_536
    /// <summary>Returns the number of spin-wait iterations used for fiber-blocked entries below the spin-miss threshold.</summary>
    let FiberSpinWaitIterations = 128
    /// <summary>Returns the miss count below which fiber-blocked entries use spin-waiting.</summary>
    let FiberSpinMissThreshold = 512
    /// <summary>Returns the miss count below which fiber-blocked entries use task-yield instead of timed delay.</summary>
    let FiberYieldMissThreshold = 8_192
    /// <summary>Returns the miss count below which fiber-blocked entries use the first delay step.</summary>
    let FiberDelayStep1MissThreshold = 16_384
    /// <summary>Returns the miss count below which fiber-blocked entries use the second delay step.</summary>
    let FiberDelayStep2MissThreshold = 32_768
    /// <summary>Returns the first-step delay in milliseconds for fiber-blocked entries.</summary>
    let FiberDelayStep1Ms = 1
    /// <summary>Returns the second-step delay in milliseconds for fiber-blocked entries.</summary>
    let FiberDelayStep2Ms = 2
    /// <summary>Returns the third-step delay in milliseconds for fiber-blocked entries.</summary>
    let FiberDelayStep3Ms = 4

/// <summary>Represents the configuration passed to each evaluation worker in the cooperative runtime.</summary>
type private EvaluationWorkerConfig =
    {
        /// <summary>Represents the owning cooperative runtime.</summary>
        Runtime: CooperativeRuntime
        /// <summary>Represents the shared channel from which runnable work items are taken.</summary>
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        /// <summary>Represents the blocking worker to which blocked fibers are dispatched.</summary>
        BlockingWorker: BlockingWorker
        /// <summary>Represents the number of evaluation steps per work item before rescheduling.</summary>
        EWSteps: int
        /// <summary>Represents the admission controller that limits concurrent fibers.</summary>
        Admission: IFiberAdmission
        /// <summary>Represents the health monitor for fault tracking.</summary>
        Monitor: WorkerHealthMonitor
        /// <summary>Represents the restart policy applied on worker faults.</summary>
        RestartPolicy: WorkerRestartPolicy
    }

/// <summary>Represents the configuration passed to each blocking worker in the cooperative runtime.</summary>
and internal BlockingWorkerConfig =
    {
        /// <summary>Represents the shared channel to which ready work items are dispatched.</summary>
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        /// <summary>Represents the channel from which new blocking entries are received.</summary>
        ActiveBlockingItemChan: UnboundedChannel<BlockingEntry>
        /// <summary>Represents the health monitor for fault tracking.</summary>
        Monitor: WorkerHealthMonitor
        /// <summary>Represents the restart policy applied on worker faults.</summary>
        RestartPolicy: WorkerRestartPolicy
    }

/// <summary>Represents a blocking item paired with the number of consecutive polling misses it has accumulated.</summary>
and [<Struct>] internal BlockingEntry = { /// <summary>Represents the underlying blocking item (channel or fiber).</summary>
                                          Item: BlockingItem
                                          /// <summary>Represents the cumulative number of polling iterations where this entry was not ready.</summary>
                                          MissCount: int }

/// <summary>Represents an evaluation worker that takes runnable work items from a shared channel and interprets them on the cooperative runtime.</summary>
/// <param name="config">The evaluation worker configuration.</param>
/// <param name="workerId">The zero-based index identifying this worker.</param>
and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    /// <summary>Transforms a work item by interpreting its effect tree on the cooperative runtime.</summary>
    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(
            workItem,
            config.EWSteps,
            config.ActiveWorkItemChan,
            config.BlockingWorker,
            config.Admission
        )

    /// <summary>Represents the cancellation source and background task for this evaluation worker.</summary>
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

    /// <summary>Provides resource cleanup for the evaluation worker.</summary>
    interface IDisposable with

        /// <summary>Transforms the worker by cancelling its background task and releasing resources.</summary>
        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

/// <summary>Represents a blocking worker that polls blocked fibers and channel-blocked work items, rescheduling them when they become ready.</summary>
/// <param name="config">The blocking worker configuration.</param>
/// <param name="workerId">The zero-based index identifying this worker.</param>
and internal BlockingWorker(config: BlockingWorkerConfig, workerId: int) =

    /// <summary>Represents the maximum entries processed per polling batch.</summary>
    let batchSize = CooperativePolling.BatchSize

    /// <summary>Represents the initial capacity for each pending queue.</summary>
    let pendingQueueCapacity =
        batchSize * CooperativePolling.PendingQueueCapacityMultiplier

    /// <summary>Represents whether the next batch prefers channel-blocked entries first.</summary>
    let mutable preferChannelFirst = true

    /// <summary>Represents the queue of channel-blocked entries awaiting polling.</summary>
    let channelPending = Queue<BlockingEntry>(pendingQueueCapacity)

    /// <summary>Represents the queue of fiber-blocked entries awaiting polling.</summary>
    let fiberPending = Queue<BlockingEntry>(pendingQueueCapacity)

    /// <summary>Returns whether either pending queue contains entries to poll.</summary>
    /// <returns><c>true</c> when at least one entry is pending.</returns>
    let hasPending () =
        channelPending.Count > 0 || fiberPending.Count > 0

    /// <summary>Transforms a blocking entry by routing it to the appropriate pending queue based on its blocking kind.</summary>
    let enqueuePending (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel _ -> channelPending.Enqueue entry
        | BlockingFiber _ -> fiberPending.Enqueue entry

    /// <summary>Returns whether a pending entry was dequeued, preferring the queue indicated by <paramref name="preferChannel"/>.</summary>
    /// <param name="preferChannel">When <c>true</c>, channel-blocked entries are tried first.</param>
    /// <param name="entry">Receives the dequeued entry when the method returns <c>true</c>.</param>
    /// <returns><c>true</c> if an entry was dequeued; <c>false</c> when both queues are empty.</returns>
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

    /// <summary>Returns whether the blocking entry's channel has a pending message or its fiber has reached a terminal state.</summary>
    /// <param name="entry">The blocking entry to test.</param>
    /// <returns><c>true</c> when the entry's work item can be rescheduled.</returns>
    let isReady (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel(chan, _) -> chan.Count > 0
        | BlockingFiber(fiberContext, _) -> fiberContext.IsTerminal()

    /// <summary>Returns the work item carried by the given blocking entry.</summary>
    /// <param name="entry">The blocking entry to extract from.</param>
    /// <returns>The work item associated with this blocking entry.</returns>
    let getWorkItem (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel(_, workItem) -> workItem
        | BlockingFiber(_, workItem) -> workItem

    /// <summary>Creates a task that enqueues the given work item onto the active work item channel for evaluation.</summary>
    /// <param name="workItem">The work item to enqueue.</param>
    /// <returns>A task that completes once the work item has been enqueued.</returns>
    let queueActive (workItem: WorkItem) =
        task {
            let addVt = config.ActiveWorkItemChan.AddAsync workItem

            if not addVt.IsCompletedSuccessfully then
                do! addVt.AsTask()
        }

    /// <summary>Creates a task that applies an adaptive backoff delay based on the worst-case miss counts observed in the current batch.</summary>
    /// <param name="maxChannelMiss">The highest miss count among channel-blocked entries in this batch.</param>
    /// <param name="maxFiberMiss">The highest miss count among fiber-blocked entries in this batch.</param>
    /// <returns>A task that completes after the appropriate backoff delay.</returns>
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

    /// <summary>Creates a task that polls up to one batch of pending entries, rescheduling ready items and applying backoff for unready ones.</summary>
    /// <returns>A task that completes after the batch has been processed and any backoff delay applied.</returns>
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

    /// <summary>Transforms newly arrived blocking entries from the incoming channel into the appropriate pending queues, draining up to one batch.</summary>
    let tryDrainIncoming () =
        let mutable drained = 0
        let mutable entry = Unchecked.defaultof<BlockingEntry>

        while drained < batchSize && config.ActiveBlockingItemChan.TryTake(&entry) do
            enqueuePending entry
            drained <- drained + 1

    /// <summary>Returns whether the worker has entries to process, blocking on the incoming channel when both pending queues are empty.</summary>
    /// <param name="ct">The cancellation token to observe while waiting.</param>
    /// <returns>A task that completes with <c>true</c> when at least one entry is pending, or <c>false</c> on cancellation.</returns>
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

    /// <summary>Represents the cancellation source and background task for this blocking worker.</summary>
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

    /// <summary>Provides resource cleanup for the blocking worker.</summary>
    interface IDisposable with

        /// <summary>Transforms the worker by cancelling its background task and releasing resources.</summary>
        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

    /// <summary>Creates a task that enqueues a blocking item for polling by this worker.</summary>
    /// <param name="blockingItem">The blocking item to enqueue.</param>
    /// <returns>A value task that completes once the item has been enqueued.</returns>
    member internal _.RescheduleForBlocking blockingItem =
        config.ActiveBlockingItemChan.AddAsync { Item = blockingItem; MissCount = 0 }

/// <summary>Represents the FIO runtime that uses custom fibers and a linear-time polling blocking worker for handling fibers blocked on channels or other fibers.</summary>
/// <param name="config">The worker configuration controlling evaluation worker count, step budget, and blocking worker count.</param>
and CooperativeRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    /// <summary>Represents the shared channel from which evaluation workers take runnable work items.</summary>
    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    /// <summary>Represents the channel through which blocked items are dispatched to blocking workers.</summary>
    let activeBlockingItemChan = UnboundedChannel<BlockingEntry>()

    /// <summary>Represents the admission controller that limits concurrent fibers.</summary>
    let admission = FiberAdmission.fromConfig config

    /// <summary>Represents the fiber context of the most recently started root fiber.</summary>
    let mutable currentFiber: FiberContext option = None

    /// <summary>Represents the lock that serializes calls to <c>Run</c>.</summary>
    let runLock = obj ()

    /// <summary>Represents the blocking workers owned by this runtime.</summary>
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

    /// <summary>Represents the evaluation workers owned by this runtime.</summary>
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

    /// <summary>Returns the human-readable name of this runtime.</summary>
    /// <returns>The runtime's name.</returns>
    override _.Name = "CooperativeRuntime"

    /// <summary>Provides resource cleanup for the cooperative runtime.</summary>
    interface IDisposable with

        /// <summary>Transforms the runtime by disposing all workers and the admission controller.</summary>
        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

            match admission with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    /// <summary>Creates a cooperative runtime configured with default worker counts derived from the available processors.</summary>
    new() = new CooperativeRuntime(WorkerConfig.Default)

    /// <summary>Transforms a work item by interpreting its effect tree for up to the given number of evaluation steps, rescheduling or blocking as needed.</summary>
    /// <param name="workItem">The work item to evaluate.</param>
    /// <param name="evalSteps">The maximum number of evaluation steps before rescheduling.</param>
    /// <param name="activeWorkItemChan">The channel to which rescheduled work items are dispatched.</param>
    /// <param name="blockingWorker">The blocking worker to which blocked fibers are dispatched.</param>
    /// <param name="admission">The admission controller for forked fibers.</param>
    /// <returns>A task that completes when the work item has been fully evaluated, rescheduled, or blocked.</returns>
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

    /// <summary>Transforms the runtime by resetting all workers and internal state to their initial configuration.</summary>
    member private _.Reset() =
        activeWorkItemChan.Clear()
        activeBlockingItemChan.Clear()
        admission.Reset()

    /// <summary>Creates a new fiber that interprets the given effect on this runtime's worker pool.</summary>
    /// <typeparam name="'R">The success result type produced by the effect.</typeparam>
    /// <typeparam name="'E">The typed error type the effect may fail with.</typeparam>
    /// <param name="eff">The effect to interpret.</param>
    /// <returns>A fiber that runs <paramref name="eff"/> and exposes its terminal state.</returns>
    /// <remarks><c>Run</c> is intended for sequential invocation; if a prior fiber on this runtime is still running, the call blocks until it completes. Concurrency within a single <c>Run</c> is supplied by forked fibers.</remarks>
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

                admission.TryAcquire() |> ignore
                fiber.Context.SetOnTerminal(fun () -> admission.Release())

                let workItem =
                    WorkItemPool.Rent(eff.UpcastBoth(), fiber.Context, ContStackPool.Rent())

                activeWorkItemChan.AddAsync workItem |> ignore

                fiber)
