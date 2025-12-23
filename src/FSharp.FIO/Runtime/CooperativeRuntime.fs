(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides the cooperative (work-stealing) runtime for interpreting FIO effects, enabling concurrent and asynchronous execution across multiple workers.
/// </summary>
module FSharp.FIO.Runtime.Cooperative

open FSharp.FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Configuration for an evaluation worker.
/// </summary>
type private EvaluationWorkerConfig =
    { Runtime: Runtime
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      BlockingWorker: BlockingWorker
      EWSteps: int }

/// <summary>
/// Configuration for a blocking worker.
/// </summary>
and private BlockingWorkerConfig =
    { ActiveWorkItemChan: UnboundedChannel<WorkItem>
      ActiveBlockingDataChan: UnboundedChannel<BlockingData> }

/// <summary>
/// Worker that evaluates FIO effects.
/// </summary>
and private EvaluationWorker (config: EvaluationWorkerConfig) =

    let processWorkItem (workItem: WorkItem) =
        task {
            match! config.Runtime.InterpretAsync workItem config.EWSteps with
            | Success res, _, Evaluated ->
                workItem.FiberContext.Complete <| Ok res
            | Failure err, _, Evaluated ->
                workItem.FiberContext.Complete <| Error err
            | eff, stack, RescheduleForRunning ->
                do! config.ActiveWorkItemChan.AddAsync
                    <| { Eff = eff; FiberContext = workItem.FiberContext; Stack = stack; PrevAction = RescheduleForRunning }
            | eff, stack, RescheduleForBlocking blockingItem ->
                do! config.BlockingWorker.RescheduleForBlocking
                    <| { BlockingItem = blockingItem
                         WaitingWorkItem = { Eff = eff; FiberContext = workItem.FiberContext; Stack = stack; PrevAction = RescheduleForBlocking blockingItem } }
            | _ ->
                printfn "ERROR: EvaluationWorker: Unexpected state encountered during effect interpretation!"
                invalidOp "EvaluationWorker: Unexpected state encountered during effect interpretation!"
        }

    let cancellationTokenSource = new CancellationTokenSource ()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cancellationTokenSource.Token.IsCancellationRequested do
                    try
                        let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync ()
                        if not hasWorkItem || cancellationTokenSource.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! workItem = config.ActiveWorkItemChan.TakeAsync ()
                            // Skip orphaned work items (fiber already completed)
                            if not <| workItem.FiberContext.Completed () then
                                do! processWorkItem workItem
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel ()
            cancellationTokenSource.Dispose ()

/// <summary>
/// Worker that handles blocked effects waiting for resources.
/// </summary>
and private BlockingWorker (config: BlockingWorkerConfig) =

    /// <summary>
    /// Processes blocking data for a channel.
    /// </summary>
    /// <param name="blockingData">The blocking data.</param>
    /// <param name="blockingChan">The channel being waited on.</param>
    let processBlockingChannel blockingData (blockingChan: Channel<obj>) =
        task {
            if blockingChan.Count > 0 then
                do! config.ActiveWorkItemChan.AddAsync blockingData.WaitingWorkItem
            else
                do! config.ActiveBlockingDataChan.AddAsync blockingData
        }

    /// <summary>
    /// Processes blocking data for a fiber context.
    /// </summary>
    /// <param name="blockingData">The blocking data.</param>
    /// <param name="fiberContext">The fiber context being waited on.</param>
    let processBlockingFiberContext blockingData (fiberContext: FiberContext) =
        task {
            if fiberContext.Completed () then
                do! config.ActiveWorkItemChan.AddAsync blockingData.WaitingWorkItem
            else
                do! config.ActiveBlockingDataChan.AddAsync blockingData
        }

    /// <summary>
    /// Processes blocking data by dispatching to the appropriate handler.
    /// </summary>
    /// <param name="blockingData">The blocking data to process.</param>
    let rec processBlockingData blockingData =
        task {
            match blockingData.BlockingItem with
            | BlockingChannel blockingChan ->
                do! processBlockingChannel blockingData blockingChan
            | BlockingFiberContext fiberContext ->
                do! processBlockingFiberContext blockingData fiberContext
        }
    
    let cancellationTokenSource = new CancellationTokenSource ()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cancellationTokenSource.Token.IsCancellationRequested do
                    try
                        let! hasBlockingItem = config.ActiveBlockingDataChan.WaitToTakeAsync ()
                        if not hasBlockingItem || cancellationTokenSource.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! blockingData = config.ActiveBlockingDataChan.TakeAsync ()
                            do! processBlockingData blockingData
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel ()
            cancellationTokenSource.Dispose ()

    /// <summary>
    /// Reschedules a work item that is blocked on a resource.
    /// </summary>
    /// <param name="blockingData">The blocking data for the work item.</param>
    member internal _.RescheduleForBlocking blockingData =
        config.ActiveBlockingDataChan.AddAsync blockingData

/// <summary>
/// The cooperative runtime for FIO, interpreting effects concurrently using work-stealing.
/// </summary>
and Runtime (config: WorkerConfig) as this =
    inherit FWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem> ()
    let activeBlockingDataChan = UnboundedChannel<BlockingData> ()

    let blockingWorker =
        new BlockingWorker({
            ActiveWorkItemChan = activeWorkItemChan
            ActiveBlockingDataChan = activeBlockingDataChan
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
        "Cooperative"

    interface IDisposable with
        member _.Dispose () =
            (blockingWorker :> IDisposable).Dispose ()
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose ())

    new() =
        new Runtime
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
    [<TailCall>]
    member internal _.InterpretAsync workItem evalSteps =
        let mutable currentEff = workItem.Eff
        let mutable currentContStack = workItem.Stack
        let mutable currentPrevAction = workItem.PrevAction
        let mutable currentEWSteps = evalSteps
        let mutable currentFiberContext = workItem.FiberContext
        let mutable result = Unchecked.defaultof<_>
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    result <- Success res, ContStackPool.Rent (), Evaluated
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop currentContStack
                    match stackFrame.Cont with
                    | SuccessCont cont ->
                        currentEff <- cont res
                        currentPrevAction <- Evaluated
                        loop <- false
                    | FailureCont _ ->
                        ()

        let inline processError err =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    result <- Failure err, ContStackPool.Rent (), Evaluated
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop currentContStack
                    match stackFrame.Cont with
                    | SuccessCont _ ->
                        ()
                    | FailureCont cont ->
                        currentEff <- cont err
                        currentPrevAction <- Evaluated
                        loop <- false

        let inline processInterrupt () =
            ContStackPool.Return currentContStack
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
                        processInterrupt()
                    elif currentEWSteps = 0 then
                        result <- currentEff, currentContStack, RescheduleForRunning
                        completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1
                        match currentEff with
                        | Success res ->
                            processSuccess res
                        | Failure err ->
                            processError err
                        | Interruption(cause, msg) ->
                            currentFiberContext.Interrupt(cause, msg)
                            processInterrupt()
                        | Action(func, onError) ->
                            try
                                let res = func()
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | SendChan(msg, chan) ->
                            do! chan.SendAsync msg
                            processSuccess msg
                        | ReceiveChan chan ->
                            let mutable res = Unchecked.defaultof<_>
                            if chan.UnboundedChannel.TryTake(&res) then
                                processSuccess res
                            else
                                let newPrevAction = RescheduleForBlocking (BlockingChannel chan)
                                currentPrevAction <- newPrevAction
                                result <- ReceiveChan chan, currentContStack, newPrevAction
                                completed <- true
                        | ConcurrentEffect(eff, fiber, fiberContext) ->
                            currentFiberContext.CancellationToken.Register(
                                fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            |> ignore
                            do! activeWorkItemChan.AddAsync
                                    { Eff = eff;
                                      FiberContext = fiberContext;
                                      Stack = ContStackPool.Rent();
                                      PrevAction = currentPrevAction }
                            processSuccess fiber
                        | ConcurrentTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            currentFiberContext.CancellationToken.Register(
                                fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            |> ignore
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        do! t
                                        fiberContext.Complete(Ok ())
                                    with
                                    | :? OperationCanceledException ->
                                        fiberContext.Complete(Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                    | exn ->
                                        fiberContext.Complete(Error (onError exn))
                                } :> Task)
                            processSuccess fiber
                        | ConcurrentGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            currentFiberContext.CancellationToken.Register(
                                fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            |> ignore
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        let! result = t
                                        fiberContext.Complete (Ok result)
                                    with
                                    | :? OperationCanceledException ->
                                        fiberContext.Complete (Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                    | exn ->
                                        fiberContext.Complete (Error (onError exn))
                                } :> Task)
                            processSuccess fiber
                        | AwaitFiberContext fiberContext ->
                            if fiberContext.Completed() then
                                let! res = fiberContext.Task
                                processResult res
                            else
                                let newPrevAction = RescheduleForBlocking (BlockingFiberContext fiberContext)
                                currentPrevAction <- newPrevAction
                                result <- AwaitFiberContext fiberContext, currentContStack, newPrevAction
                                completed <- true
                        | AwaitTPLTask(task, onError) ->
                            try
                                let! res = task
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | AwaitGenericTPLTask(task, onError) ->
                            try
                                let! res = task
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | ChainSuccess(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add(ContStackFrame (SuccessCont cont))
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add(ContStackFrame (FailureCont cont))
                return result
            finally
                if not completed then
                    ContStackPool.Return currentContStack
        }

    /// <summary>
    /// Resets the runtime state by clearing work item channels.
    /// </summary>
    member private _.Reset () =
        activeWorkItemChan.Clear()
        activeBlockingDataChan.Clear()

    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        this.Reset()
        let fiber = new Fiber<'R, 'E>()
        activeWorkItemChan.AddAsync
            { Eff = eff.Upcast();
              FiberContext = fiber.Internal;
              Stack = ContStackPool.Rent();
              PrevAction = Evaluated }
        |> ignore
        fiber
