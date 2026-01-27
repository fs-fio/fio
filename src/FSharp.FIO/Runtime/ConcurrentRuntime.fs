/// <summary>
/// Provides the concurrent (advanced, event-driven) runtime for interpreting FIO effects, enabling scalable and highly concurrent execution.
/// </summary>
module FSharp.FIO.Runtime.Concurrent

open FSharp.FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Configuration for an evaluation worker.
/// </summary>
type private EvaluationWorkerConfig =
    { /// <summary>
      /// The runtime instance.
      /// </summary>
      Runtime: ConcurrentRuntime
      /// <summary>
      /// Channel for active work items.
      /// </summary>
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      /// <summary>
      /// The blocking worker for handling blocked effects.
      /// </summary>
      BlockingWorker: BlockingWorker
      /// <summary>
      /// Number of evaluation steps per work item.
      /// </summary>
      EWSteps: int }

/// <summary>
/// Configuration for a blocking worker.
/// </summary>
and private BlockingWorkerConfig =
    { /// <summary>
      /// Channel for active work items.
      /// </summary>
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      /// <summary>
      /// Channel for blocking events from channels.
      /// </summary>
      ActiveBlockingEventChan: UnboundedChannel<Channel<obj>> }

/// <summary>
/// Internal type for tracking deferred fiber completion.
/// </summary>
and private CompletionAction =
    /// <summary>
    /// No completion action needed.
    /// </summary>
    | NoCompletion
    /// <summary>
    /// Complete the fiber with a success value.
    /// </summary>
    | CompleteSuccess of obj
    /// <summary>
    /// Complete the fiber with a failure value.
    /// </summary>
    | CompleteFailure of obj

/// <summary>
/// Worker that evaluates FIO effects.
/// </summary>
/// <param name="config">The evaluation worker configuration.</param>
and private EvaluationWorker (config: EvaluationWorkerConfig) =
    
    /// <summary>
    /// Processes a work item by interpreting its effect.
    /// </summary>
    /// <param name="workItem">The work item to process.</param>
    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync (workItem, config.EWSteps, config.ActiveWorkItemChan)

    let cts = new CancellationTokenSource()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cts.Token.IsCancellationRequested do
                    try
                        let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync()
                        if not hasWorkItem || cts.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! workItem = config.ActiveWorkItemChan.TakeAsync()
                            if not (workItem.FiberContext.Completed()) then
                                do! processWorkItem workItem
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker()

    interface IDisposable with
        member _.Dispose () =
            cts.Cancel()
            cts.Dispose()

/// <summary>
/// Worker that handles blocked effects waiting for channel events.
/// </summary>
/// <param name="config">The blocking worker configuration.</param>
and private BlockingWorker (config: BlockingWorkerConfig) =

    /// <summary>
    /// Processes a blocking channel event.
    /// </summary>
    /// <param name="blockingChan">The channel that received an event.</param>
    let processBlockingChannel (blockingChan: Channel<obj>) =
        task {
            let! rescheduled = blockingChan.TryRescheduleNextBlockingWorkItem config.ActiveWorkItemChan
            if not rescheduled || blockingChan.BlockingWorkItemCount > 0 then
                do! config.ActiveBlockingEventChan.AddAsync blockingChan
        }

    let cancellationTokenSource = new CancellationTokenSource()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cancellationTokenSource.Token.IsCancellationRequested do
                    try
                        let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToTakeAsync()
                        if not hasBlockingItem || cancellationTokenSource.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! blockingChanEvent = config.ActiveBlockingEventChan.TakeAsync()
                            do! processBlockingChannel blockingChanEvent
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel()
            cancellationTokenSource.Dispose()

/// <summary>
/// The concurrent runtime for FIO, interpreting effects using event-driven concurrency.
/// </summary>
/// <param name="config">The worker configuration.</param>
and ConcurrentRuntime (config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()
    let activeBlockingEventChan = UnboundedChannel<Channel<obj>>()
    let mutable currentFiber: FiberContext option = None
    let runLock = obj()

    let blockingWorker =
        new BlockingWorker({
            ActiveWorkItemChan = activeWorkItemChan
            ActiveBlockingEventChan = activeBlockingEventChan
        })

    let evaluationWorkers =
        List.init config.EWC <| fun _ ->
            new EvaluationWorker({
                Runtime = this
                ActiveWorkItemChan = activeWorkItemChan
                BlockingWorker = blockingWorker
                EWSteps = config.EWS
            })

    override _.Name =
        "ConcurrentRuntime"

    interface IDisposable with
        member _.Dispose () =
            (blockingWorker :> IDisposable).Dispose()
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    /// <summary>
    /// Creates a new ConcurrentRuntime with default configuration.
    /// </summary>
    new() =
        new ConcurrentRuntime
            { EWC =
                let coreCount = Environment.ProcessorCount - 1
                if coreCount >= 2 then coreCount else 2
              BWC = 1
              EWS = 200 }

    /// <summary>
    /// Interprets an FIO effect asynchronously with step-limited evaluation.
    /// </summary>
    /// <param name="workItem">The work item containing the effect to interpret.</param>
    /// <param name="evalSteps">The maximum number of evaluation steps before rescheduling.</param>
    /// <param name="activeQueue">The active work item queue for rescheduling.</param>
    [<TailCall>]
    member internal _.InterpretAsync (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>) =
        let mutable currentEff = workItem.Eff
        let mutable currentContStack = workItem.Stack
        let mutable currentEWSteps = evalSteps
        let mutable currentFiberContext = workItem.FiberContext
        let mutable completionAction = NoCompletion
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    completionAction <- CompleteSuccess res
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()
                    match stackFrame.Cont with
                    | SuccessCont cont ->
                        currentEff <- cont res
                        loop <- false
                    | FailureCont _ ->
                        ()

        let inline processError err =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    completionAction <- CompleteFailure err
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()
                    match stackFrame.Cont with
                    | SuccessCont _ ->
                        ()
                    | FailureCont cont ->
                        currentEff <- cont err
                        loop <- false

        let inline processInterruptError err =
            ContStackPool.Return currentContStack
            completionAction <- CompleteFailure err
            completed <- true

        let inline processResult res =
            match res with
            | Ok res ->
                processSuccess res
            | Error err ->
                processError err

        task {
            try
                while not completed do
                    if currentFiberContext.CancellationToken.IsCancellationRequested then
                        match! currentFiberContext.Task with
                        | Ok _ ->
                            raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error err ->
                            processInterruptError err
                    elif currentEWSteps = 0 then
                        let newWorkItem = WorkItemPool.Rent(currentEff, currentFiberContext, currentContStack)
                        do! activeWorkItemChan.AddAsync newWorkItem
                        completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1
                        match currentEff with
                        | Success res ->
                            processSuccess res
                        | Failure err ->
                            processError err
                        | InterruptFiber(cause, msg, fiberContext) ->
                            fiberContext.Interrupt(cause, msg)
                            processSuccess()
                        | InterruptSelf(cause, msg) ->
                            currentFiberContext.Interrupt(cause, msg)
                            processSuccess()
                        | Action(func, onError) ->
                            try
                                let res = func()
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | SendChan(msg, chan) ->
                            do! chan.SendAsync msg
                            do! activeBlockingEventChan.AddAsync chan
                            processSuccess msg
                        | ReceiveChan chan ->
                            let mutable res = Unchecked.defaultof<_>
                            if chan.UnboundedChannel.TryTake(&res) then
                                processSuccess res
                            else
                                let newWorkItem = WorkItemPool.Rent(ReceiveChan chan, currentFiberContext, currentContStack)
                                do! chan.AddBlockingWorkItem newWorkItem
                                completed <- true
                        | ForkEffect(eff, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                            do! activeWorkItemChan.AddAsync workItem
                            processSuccess fiber
                        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt (ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        try
                                            do! t.WaitAsync fiberContext.CancellationToken
                                            do! fiberContext.CompleteAndReschedule (Ok (), activeWorkItemChan)
                                        with
                                        | :? OperationCanceledException ->
                                            do! fiberContext.CompleteAndReschedule (Error (onError <|
                                                FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled.")), activeWorkItemChan)
                                        | exn ->
                                            do! fiberContext.CompleteAndReschedule (Error (onError exn), activeWorkItemChan)
                                    finally
                                        registration.Dispose()
                                } :> Task)
                            processSuccess fiber
                        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt (ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        try
                                            let! res = t.WaitAsync fiberContext.CancellationToken
                                            do! fiberContext.CompleteAndReschedule (Ok res, activeWorkItemChan)
                                        with
                                        | :? OperationCanceledException ->
                                            do! fiberContext.CompleteAndReschedule (Error (onError <|
                                                FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled.")), activeWorkItemChan)
                                        | exn ->
                                            do! fiberContext.CompleteAndReschedule (Error (onError exn), activeWorkItemChan)
                                    finally
                                        registration.Dispose()
                                } :> Task)
                            processSuccess fiber
                        | JoinFiber fiberContext ->
                            if fiberContext.Completed() then
                                let! res = fiberContext.Task
                                processResult res
                            else
                                let newWorkItem = WorkItemPool.Rent(JoinFiber fiberContext, currentFiberContext, currentContStack)
                                do! fiberContext.AddBlockingWorkItem newWorkItem
                                do fiberContext.TryRescheduleBlockingWorkItems activeWorkItemChan |> ignore
                                completed <- true
                        | AwaitTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | AwaitGenericTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | ChainSuccess(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame (SuccessCont cont))
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame (FailureCont cont))
                match completionAction with
                | CompleteSuccess res ->
                    do! currentFiberContext.CompleteAndReschedule(Ok res, activeWorkItemChan)
                | CompleteFailure err ->
                    do! currentFiberContext.CompleteAndReschedule(Error err, activeWorkItemChan)
                | NoCompletion ->
                    ()
                return ()
            finally
                if not completed then
                    ContStackPool.Return currentContStack
                // Return work item when fiber completes (success/failure) but NOT when rescheduling
                // Work items that block (channel/fiber join) or reschedule (step count expires)
                // create new work items, so the original should be returned to pool
                let shouldReturnWorkItem = completionAction <> NoCompletion
                if shouldReturnWorkItem then
                    WorkItemPool.Return workItem
        }

    /// <summary>
    /// Resets the runtime state by clearing work item channels.
    /// </summary>
    member private _.Reset () =
        activeWorkItemChan.Clear()
        activeBlockingEventChan.Clear()

    /// <summary>
    /// Runs an FIO effect and returns a fiber representing its execution.
    /// </summary>
    /// <param name="eff">The FIO effect to run.</param>
    /// <returns>A fiber representing the running effect.</returns>
    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.Completed()) ->
                fiberContext.Task
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> ignore
            | _ -> ())

        this.Reset()
        let fiber = new Fiber<'R, 'E>()

        lock runLock (fun () ->
            currentFiber <- Some fiber.Internal)

        let workItem = WorkItemPool.Rent(eff.Upcast(), fiber.Internal, ContStackPool.Rent())
        activeWorkItemChan.AddAsync workItem
        |> ignore
        fiber
