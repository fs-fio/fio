/// <summary>Provides the concurrent (event-driven) runtime for interpreting FIO effects.</summary>
module FIO.Runtime.Concurrent

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System

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
    }

/// <summary>Represents the configuration passed to each blocking worker in the concurrent runtime.</summary>
and private BlockingWorkerConfig =
    {
        /// <summary>Represents the shared channel to which ready work items are dispatched.</summary>
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        /// <summary>Represents the channel from which channel-blocking events are received.</summary>
        ActiveBlockingEventChan: UnboundedChannel<Channel<obj>>
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
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan)

    /// <summary>Represents the cancellation source and background task for this evaluation worker.</summary>
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
                                    do!
                                        workItem.FiberContext.CompleteAndReschedule(
                                            Error(exn :> obj),
                                            config.ActiveWorkItemChan
                                        )
                                with _ ->
                                    ()

                                raise exn
            }

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
                do! config.ActiveBlockingEventChan.WriteAsync blockingChan
        }

    /// <summary>Represents the cancellation source and background task for this blocking worker.</summary>
    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}"
        <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToReadAsync token

                    if not hasBlockingItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! blockingChanEvent = config.ActiveBlockingEventChan.ReadAsync()
                        do! processBlockingChannel blockingChanEvent
            }

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

    /// <summary>Represents the fiber context of the most recently started root fiber.</summary>
    let mutable currentFiber: FiberContext option = None

    /// <summary>Represents the lock that serializes calls to <c>Run</c>.</summary>
    let runLock = obj ()

    /// <summary>Represents the blocking and evaluation workers owned by this runtime, paired by round-robin index.</summary>
    let struct (blockingWorkers, evaluationWorkers) =
        WorkerBuilders.buildPairedWorkers
            config.BWC
            config.EWC
            (fun i ->
                new BlockingWorker(
                    {
                        ActiveWorkItemChan = activeWorkItemChan
                        ActiveBlockingEventChan = activeBlockingEventChan
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

    /// <summary>Returns the human-readable name of this runtime.</summary>
    /// <returns>The runtime's name.</returns>
    override _.Name = "ConcurrentRuntime"

    interface IDisposable with

        /// <summary>Transforms the runtime by disposing all workers.</summary>
        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    /// <summary>Creates a concurrent runtime configured with default worker counts derived from the available processors.</summary>
    new() = new ConcurrentRuntime(WorkerConfig.Default)

    /// <summary>Transforms a work item by interpreting its effect tree for up to the given number of evaluation steps, rescheduling or blocking as needed.</summary>
    /// <param name="workItem">The work item to evaluate.</param>
    /// <param name="evalSteps">The maximum number of evaluation steps before rescheduling.</param>
    /// <param name="activeWorkItemChan">The channel to which rescheduled work items are dispatched.</param>
    /// <returns>A task that completes when the work item has been fully evaluated, rescheduled, or blocked.</returns>
    [<TailCall>]
    member internal _.InterpretAsync
        (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>)
        =
        let mutable state =
            InterpreterState(workItem.Eff, workItem.Stack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEWSteps = evalSteps
        let currentFiberContext = workItem.FiberContext
        let mutable completionAction = NoCompletion

        let inline onSuccessComplete value =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteSuccess value

        let inline onErrorComplete error =
            ContStackPool.Return state.ContStack
            completionAction <- CompleteFailure error

        // Signals a blocking worker if the channel has both pending messages and blocked work items,
        // enabling constant-time rescheduling of blocked fibers.
        let signalBlockingWorkerIfPending (chan: Channel<obj>) =
            task {
                if
                    chan.Count > 0
                    && chan.BlockingWorkItemCount > 0
                    && chan.TryBeginSignalProcessing()
                then
                    do! activeBlockingEventChan.WriteAsync chan
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
                        | Error error -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeInterrupt error)
                    elif currentEWSteps = 0 then
                        let newWorkItem = WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                        newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                        do! activeWorkItemChan.WriteAsync newWorkItem
                        state.Completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1

                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.WriteAsync msg
                                do! signalBlockingWorkerIfPending chan
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess msg)
                            | HandleReceiveChan chan ->
                                let mutable value = Unchecked.defaultof<_>

                                if chan.UnboundedChannel.TryRead(&value) then
                                    processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(ReadChan chan, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! chan.AddBlockingWorkItem newWorkItem
                                    do! signalBlockingWorkerIfPending chan
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
                                        WorkItemPool.Rent(JoinFiber fiberContext, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! fiberContext.AddBlockingWorkItem newWorkItem
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemChan
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

                match completionAction with
                | CompleteSuccess value -> do! currentFiberContext.CompleteAndReschedule(Ok value, activeWorkItemChan)
                | CompleteFailure error -> do! currentFiberContext.CompleteAndReschedule(Error error, activeWorkItemChan)
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

    /// <summary>Creates a new fiber that interprets the given effect on this runtime's worker pool.</summary>
    /// <typeparam name="'A">The success result type produced by the effect.</typeparam>
    /// <typeparam name="'E">The typed error type the effect may fail with.</typeparam>
    /// <param name="eff">The effect to interpret.</param>
    /// <returns>A fiber that runs <paramref name="eff"/> and exposes its terminal state.</returns>
    /// <remarks><c>Run</c> is intended for sequential invocation; if a prior fiber on this runtime is still running, the call blocks until it completes. Concurrency within a single <c>Run</c> is supplied by forked fibers.</remarks>
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
