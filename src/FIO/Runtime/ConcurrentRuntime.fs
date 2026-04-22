/// Provides the concurrent (event-driven) runtime for interpreting FIO effects.
module FIO.Runtime.Concurrent

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks

/// Evaluation worker configuration.
type private EvaluationWorkerConfig =
    {
        Runtime: ConcurrentRuntime
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        BlockingWorker: BlockingWorker
        EWSteps: int
    }

/// Blocking worker configuration.
and private BlockingWorkerConfig =
    {
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        ActiveBlockingEventChan: UnboundedChannel<Channel<obj>>
    }

/// Tracks deferred fiber completion.
and private CompletionAction =
    | NoCompletion
    | CompleteSuccess of obj
    | CompleteFailure of obj

/// Worker that evaluates FIO effects.
and private EvaluationWorker(config: EvaluationWorkerConfig) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan)

    let cts = new CancellationTokenSource()

    let mutable workerTask: Task = Unchecked.defaultof<_>

    let startWorker () =
        workerTask <-
            Task.Factory
                .StartNew(
                    Func<Task>(fun () ->
                        task {
                            let token = cts.Token
                            let mutable loop = true

                            while loop && not token.IsCancellationRequested do
                                try
                                    let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync token

                                    if not hasWorkItem || token.IsCancellationRequested then
                                        loop <- false
                                    else
                                        let! workItem = config.ActiveWorkItemChan.TakeAsync()

                                        if not (workItem.FiberContext.IsTerminal()) then
                                            do! processWorkItem workItem
                                with
                                | :? OperationCanceledException -> loop <- false
                                | exn ->
                                    System.Console.Error.WriteLine
                                        $"[FIO] EvaluationWorker caught unhandled exception: {exn}"
                        }
                        :> Task),
                    CancellationToken.None,
                    TaskCreationOptions.LongRunning,
                    TaskScheduler.Default
                )
                .Unwrap()

    do startWorker ()

    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

/// Handles blocked effects via event-driven channel notifications for constant-time rescheduling.
and private BlockingWorker(config: BlockingWorkerConfig) =

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
                blockingChan.EndSignalProcessing()

            if
                blockingChan.Count > 0
                && blockingChan.BlockingWorkItemCount > 0
                && blockingChan.TryBeginSignalProcessing()
            then
                do! config.ActiveBlockingEventChan.AddAsync blockingChan
        }

    let cts = new CancellationTokenSource()

    let mutable workerTask: Task = Unchecked.defaultof<_>

    let startWorker () =
        workerTask <-
            Task.Factory
                .StartNew(
                    Func<Task>(fun () ->
                        task {
                            let token = cts.Token
                            let mutable loop = true

                            while loop && not token.IsCancellationRequested do
                                try
                                    let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToTakeAsync token

                                    if not hasBlockingItem || token.IsCancellationRequested then
                                        loop <- false
                                    else
                                        let! blockingChanEvent = config.ActiveBlockingEventChan.TakeAsync()
                                        do! processBlockingChannel blockingChanEvent
                                with
                                | :? OperationCanceledException -> loop <- false
                                | exn ->
                                    System.Console.Error.WriteLine
                                        $"[FIO] BlockingWorker caught unhandled exception: {exn}"
                        }
                        :> Task),
                    CancellationToken.None,
                    TaskCreationOptions.LongRunning,
                    TaskScheduler.Default
                )
                .Unwrap()

    do startWorker ()

    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

/// Runtime using custom fibers with event-driven, constant-time blocked fiber handling.
and ConcurrentRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    let activeBlockingEventChan = UnboundedChannel<Channel<obj>>()

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let blockingWorkers =
        List.init config.BWC
        <| fun _ ->
            new BlockingWorker(
                {
                    ActiveWorkItemChan = activeWorkItemChan
                    ActiveBlockingEventChan = activeBlockingEventChan
                }
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
                }
            )

    override _.Name = "ConcurrentRuntime"

    interface IDisposable with
        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    new() = new ConcurrentRuntime(WorkerConfig.Default)

    /// Interprets an FIO effect with step-limited evaluation.
    [<TailCall>]
    member internal _.InterpretAsync
        (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>)
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
                                let registration =
                                    currentFiberContext.CancellationToken.Register(fun () ->
                                        fiberContext.Interrupt(
                                            ParentInterrupted currentFiberContext.Id,
                                            "Parent fiber was interrupted."
                                        ))

                                fiberContext.AddRegistration registration
                                let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                                do! activeWorkItemChan.AddAsync workItem
                                processSuccess &state onSuccessComplete fiber
                            | HandleForkTask(taskFactory, onError, fiber, fiberContext) ->
                                let registration =
                                    currentFiberContext.CancellationToken.Register(fun () ->
                                        fiberContext.Interrupt(
                                            ParentInterrupted currentFiberContext.Id,
                                            "Parent fiber was interrupted."
                                        ))

                                fiberContext.AddRegistration registration

                                do!
                                    Task.Run(fun () ->
                                        task {
                                            try
                                                try
                                                    let t = taskFactory ()
                                                    let! res = t.WaitAsync fiberContext.CancellationToken
                                                    do! fiberContext.CompleteAndReschedule(Ok res, activeWorkItemChan)
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

    member private _.Reset() =
        activeWorkItemChan.Clear()
        activeBlockingEventChan.Clear()

    override _.Run<'R, 'E>(eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task |> Async.AwaitTask |> Async.RunSynchronously |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.CancelToken()
            | None -> ()

            this.Reset()
            let fiber = new Fiber<'R, 'E>()
            currentFiber <- Some fiber.Context

            let workItem =
                WorkItemPool.Rent(eff.UpcastBoth(), fiber.Context, ContStackPool.Rent())

            activeWorkItemChan.AddAsync workItem |> ignore

            fiber)
