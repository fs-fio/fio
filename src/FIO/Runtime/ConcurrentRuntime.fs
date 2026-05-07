/// <summary>Provides the concurrent (event-driven) runtime for interpreting FIO effects.</summary>
module FIO.Runtime.Concurrent

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks

/// <summary>Represents the configuration passed to each evaluation worker in the concurrent runtime.</summary>
type private EvaluationWorkerConfig =
    {
        /// <summary>Represents the owning concurrent runtime.</summary>
        Runtime: ConcurrentRuntime
        /// <summary>Represents the shared channel from which runnable work items are taken.</summary>
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        /// <summary>Represents the blocking worker to which channel-blocking events are dispatched.</summary>
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

/// <summary>Represents the configuration passed to each blocking worker in the concurrent runtime.</summary>
and private BlockingWorkerConfig =
    {
        /// <summary>Represents the shared channel to which ready work items are dispatched.</summary>
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        /// <summary>Represents the channel from which channel-blocking events are received.</summary>
        ActiveBlockingEventChan: UnboundedChannel<Channel<obj>>
        /// <summary>Represents the health monitor for fault tracking.</summary>
        Monitor: WorkerHealthMonitor
        /// <summary>Represents the restart policy applied on worker faults.</summary>
        RestartPolicy: WorkerRestartPolicy
    }

/// <summary>Represents the deferred completion action to apply after the interpreter loop finishes a work item.</summary>
and private CompletionAction =
    /// <summary>Represents that no completion is pending for this work item.</summary>
    | NoCompletion
    /// <summary>Represents a pending success completion carrying the result value.</summary>
    | CompleteSuccess of obj
    /// <summary>Represents a pending failure completion carrying the error value.</summary>
    | CompleteFailure of obj

/// <summary>Represents an evaluation worker that takes runnable work items from a shared channel and interprets them on the concurrent runtime.</summary>
/// <param name="config">The evaluation worker configuration.</param>
/// <param name="workerId">The zero-based index identifying this worker.</param>
and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    /// <summary>Transforms a work item by interpreting its effect tree on the concurrent runtime.</summary>
    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan, config.Admission)

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
                                    do!
                                        workItem.FiberContext.CompleteAndReschedule(
                                            Error(exn :> obj),
                                            config.ActiveWorkItemChan
                                        )
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

/// <summary>Represents a blocking worker that processes channel-blocking events by rescheduling blocked work items when their channels have pending messages.</summary>
/// <param name="config">The blocking worker configuration.</param>
/// <param name="workerId">The zero-based index identifying this worker.</param>
and private BlockingWorker(config: BlockingWorkerConfig, workerId: int) =

    /// <summary>Creates a task that reschedules blocked work items on the given channel until no more matches can be made.</summary>
    /// <param name="blockingChan">The channel whose blocked work items are to be rescheduled.</param>
    /// <returns>A task that completes when no further rescheduling is possible for this channel.</returns>
    let processBlockingChannel (blockingChan: Channel<obj>) =
        task {
            try
                let mutable keepProcessing = true

                while keepProcessing do
                    let! rescheduled =
                        blockingChan.TryRescheduleNextBlockingWorkItem config.ActiveWorkItemChan

                    let hasPendingMessages = blockingChan.Count > 0
                    let hasBlockedWorkItems = blockingChan.BlockingWorkItemCount > 0
                    keepProcessing <- rescheduled && hasPendingMessages && hasBlockedWorkItems
            finally
                // Lost-signal race guard
                // While this worker held signalProcessing=1, concurrent senders may have
                // called TryBeginSignalProcessing and failed, meaning their new messages
                // were never signaled to the blocking worker. Releasing the lock first
                // (EndSignalProcessing), then re-checking, closes this window: if messages
                // and blocked items still exist, we re-acquire the lock and re-enqueue so
                // the blocking worker processes them. Without this re-check, those messages
                // would be stranded and blocked receivers would never be rescheduled.
                blockingChan.EndSignalProcessing()

            if
                blockingChan.Count > 0
                && blockingChan.BlockingWorkItemCount > 0
                && blockingChan.TryBeginSignalProcessing()
            then
                do! config.ActiveBlockingEventChan.AddAsync blockingChan
        }

    /// <summary>Represents the cancellation source and background task for this blocking worker.</summary>
    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker config.Monitor config.RestartPolicy $"BlockingWorker-{workerId}" Blocking ignore
        <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToTakeAsync token

                    if not hasBlockingItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! blockingChanEvent = config.ActiveBlockingEventChan.TakeAsync()
                        do! processBlockingChannel blockingChanEvent
            }

    /// <summary>Provides resource cleanup for the blocking worker.</summary>
    interface IDisposable with

        /// <summary>Transforms the worker by cancelling its background task and releasing resources.</summary>
        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

/// <summary>Represents the FIO runtime that uses custom fibers with event-driven, constant-time blocked-fiber handling.</summary>
/// <param name="config">The worker configuration controlling evaluation worker count, step budget, and blocking worker count.</param>
and ConcurrentRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    /// <summary>Represents the shared channel from which evaluation workers take runnable work items.</summary>
    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    /// <summary>Represents the channel through which channel-blocking events are dispatched to blocking workers.</summary>
    let activeBlockingEventChan = UnboundedChannel<Channel<obj>>()

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
                    ActiveBlockingEventChan = activeBlockingEventChan
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
    override _.Name = "ConcurrentRuntime"

    /// <summary>Provides resource cleanup for the concurrent runtime.</summary>
    interface IDisposable with

        /// <summary>Transforms the runtime by disposing all workers and the admission controller.</summary>
        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

            match admission with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    /// <summary>Creates a concurrent runtime configured with default worker counts derived from the available processors.</summary>
    new() = new ConcurrentRuntime(WorkerConfig.Default)

    /// <summary>Transforms a work item by interpreting its effect tree for up to the given number of evaluation steps, rescheduling or blocking as needed.</summary>
    /// <param name="workItem">The work item to evaluate.</param>
    /// <param name="evalSteps">The maximum number of evaluation steps before rescheduling.</param>
    /// <param name="activeWorkItemChan">The channel to which rescheduled work items are dispatched.</param>
    /// <param name="admission">The admission controller for forked fibers.</param>
    /// <returns>A task that completes when the work item has been fully evaluated, rescheduled, or blocked.</returns>
    [<TailCall>]
    member internal _.InterpretAsync
        (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>, admission: IFiberAdmission)
        =
        let mutable state =
            InterpreterState(workItem.Eff, workItem.Stack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEWSteps = evalSteps
        let currentFiberContext = workItem.FiberContext
        let mutable completionAction = NoCompletion

        let inline onSuccessComplete res =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteSuccess res

        let inline onErrorComplete err =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteFailure err

        // Signals a blocking worker if the channel has both pending messages and blocked work items,
        // enabling constant-time rescheduling of blocked fibers.
        let signalBlockingWorkerIfPending (chan: Channel<obj>) =
            task {
                if
                    chan.Count > 0
                    && chan.BlockingWorkItemCount > 0
                    && chan.TryBeginSignalProcessing()
                then
                    do! activeBlockingEventChan.AddAsync chan
            }

        task {
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
                        state.Completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1

                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.SendAsync msg
                                do! signalBlockingWorkerIfPending chan
                                processSuccess &state onSuccessComplete msg
                            | HandleReceiveChan chan ->
                                let mutable res = Unchecked.defaultof<_>

                                if chan.UnboundedChannel.TryTake(&res) then
                                    processSuccess &state onSuccessComplete res
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(ReceiveChan chan, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! chan.AddBlockingWorkItem newWorkItem
                                    do! signalBlockingWorkerIfPending chan
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
                                                        let! res = t.WaitAsync fiberContext.CancellationToken

                                                        do!
                                                            fiberContext.CompleteAndReschedule(
                                                                Ok res,
                                                                activeWorkItemChan
                                                            )
                                                    with
                                                    | :? OperationCanceledException ->
                                                        do!
                                                            fiberContext.CompleteAndReschedule(
                                                                Error(
                                                                    onError
                                                                    <| FiberInterruptedException(
                                                                        fiberContext.Id,
                                                                        ExplicitInterrupt,
                                                                        "Task has been cancelled."
                                                                    )
                                                                ),
                                                                activeWorkItemChan
                                                            )
                                                    | exn ->
                                                        do!
                                                            fiberContext.CompleteAndReschedule(
                                                                Error(onError exn),
                                                                activeWorkItemChan
                                                            )
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
                                    do! fiberContext.AddBlockingWorkItem newWorkItem
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemChan
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

                match completionAction with
                | CompleteSuccess res -> do! currentFiberContext.CompleteAndReschedule(Ok res, activeWorkItemChan)
                | CompleteFailure err -> do! currentFiberContext.CompleteAndReschedule(Error err, activeWorkItemChan)
                | NoCompletion -> ()

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
        }

    /// <summary>Transforms the runtime by resetting all workers and internal state to their initial configuration.</summary>
    member private _.Reset() =
        activeWorkItemChan.Clear()
        activeBlockingEventChan.Clear()
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
