(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

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
    { Runtime: ConcurrentRuntime
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      BlockingWorker: BlockingWorker
      EWSteps: int }

/// <summary>
/// Configuration for a blocking worker.
/// </summary>
and private BlockingWorkerConfig =
    { ActiveWorkItemChan: UnboundedChannel<WorkItem>
      ActiveBlockingEventChan: UnboundedChannel<Channel<obj>> }

/// <summary>
/// Internal type for tracking deferred fiber completion.
/// </summary>
and private CompletionAction =
    | NoCompletion
    | CompleteSuccess of obj
    | CompleteFailure of obj

/// <summary>
/// Worker that evaluates FIO effects.
/// </summary>
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
and private BlockingWorker (config: BlockingWorkerConfig) =

    /// <summary>
    /// Processes a blocking channel event.
    /// </summary>
    /// <param name="blockingChan">The channel that received an event.</param>
    let processBlockingChannel (blockingChan: Channel<obj>) =
        task {
            if blockingChan.BlockingWorkItemCount > 0 then
                do! blockingChan.RescheduleNextBlockingWorkItem
                    <| config.ActiveWorkItemChan
            else
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
and ConcurrentRuntime (config: WorkerConfig) as this =
    inherit FWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()
    let activeBlockingEventChan = UnboundedChannel<Channel<obj>>()

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
        "Concurrent"

    interface IDisposable with
        member _.Dispose () =
            (blockingWorker :> IDisposable).Dispose()
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

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
                    let stackFrame = pop currentContStack
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
                    let stackFrame = pop currentContStack
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
                        let workItem = WorkItemPool.Rent(currentEff, currentFiberContext, currentContStack)
                        do! activeWorkItemChan.AddAsync workItem
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
                                let workItem = WorkItemPool.Rent(ReceiveChan chan, currentFiberContext, currentContStack)
                                do! chan.AddBlockingWorkItem workItem
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
                                let workItem = WorkItemPool.Rent(JoinFiber fiberContext, currentFiberContext, currentContStack)
                                do! fiberContext.AddBlockingWorkItem workItem
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
                            currentContStack.Add(ContStackFrame (SuccessCont cont))
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add(ContStackFrame (FailureCont cont))
                // Handle deferred fiber completion
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
                // Return WorkItem to pool only if fiber completed (not rescheduled, not blocked)
                if completed && completionAction <> NoCompletion then
                    WorkItemPool.Return workItem
        }

    /// <summary>
    /// Resets the runtime state by clearing work item channels.
    /// </summary>
    member private _.Reset () =
        activeWorkItemChan.Clear()
        activeBlockingEventChan.Clear()

    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        this.Reset()
        let fiber = new Fiber<'R, 'E>()
        let workItem = WorkItemPool.Rent(eff.Upcast(), fiber.Internal, ContStackPool.Rent())
        activeWorkItemChan.AddAsync workItem
        |> ignore
        fiber
