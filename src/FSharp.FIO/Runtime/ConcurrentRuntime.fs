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

type private EvaluationWorkerConfig =
    { Runtime: Runtime
      ActiveWorkItemChan: InternalChannel<WorkItem>
      BlockingWorker: BlockingWorker
      EWSteps: int }

and private BlockingWorkerConfig =
    { ActiveWorkItemChan: InternalChannel<WorkItem>
      ActiveBlockingEventChan: InternalChannel<Channel<obj>> }

and private EvaluationWorker (config: EvaluationWorkerConfig) =
    
    let processWorkItem (workItem: WorkItem) =
        task {
            match! config.Runtime.InterpretAsync workItem config.EWSteps with
            | Success res, _, Evaluated ->
                do! workItem.CompleteAndReschedule (Ok res) config.ActiveWorkItemChan
            | Failure err, _, Evaluated ->
                do! workItem.CompleteAndReschedule (Error err) config.ActiveWorkItemChan
            | eff, stack, RescheduleForRunning ->
                do! config.ActiveWorkItemChan.AddAsync
                    <| WorkItem.Create (eff, workItem.IFiber, stack, RescheduleForRunning)
            | _, _, Skipped ->
                ()
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
                            if not workItem.IFiber.Completed then
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

and private BlockingWorker (config: BlockingWorkerConfig) =

    let processBlockingChannel (blockingChan: Channel<obj>) =
        task {
            if blockingChan.BlockingWorkItemCount > 0 then
                do! blockingChan.RescheduleNextBlockingWorkItem
                    <| config.ActiveWorkItemChan
            else
                do! config.ActiveBlockingEventChan.AddAsync blockingChan
        }

    let cancellationTokenSource = new CancellationTokenSource ()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cancellationTokenSource.Token.IsCancellationRequested do
                    try
                        // When the BlockingWorker receives a channel, it is an "event" that the
                        // the given channel now has received one element, that the next blocking effect can retrieve.
                        let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToTakeAsync ()
                        if not hasBlockingItem || cancellationTokenSource.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! blockingChanEvent = config.ActiveBlockingEventChan.TakeAsync ()
                            do! processBlockingChannel blockingChanEvent
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
/// Represents the concurrent runtime for FIO, interpreting effects using event-driven concurrency and advanced scheduling.
/// </summary>
and Runtime (config: WorkerConfig) as this =
    inherit FWorkerRuntime(config)

    let activeWorkItemChan = InternalChannel<WorkItem> ()
    let activeBlockingEventChan = InternalChannel<Channel<obj>> ()

    // Note: BWC (Blocking Worker Count) is currently limited to 1
    // Multiple blocking workers would require event distribution logic
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
            // Dispose workers to stop background threads
            (blockingWorker :> IDisposable).Dispose ()
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose ())

    new() =
        new Runtime
            { EWC =
                let coreCount = Environment.ProcessorCount - 1
                if coreCount >= 2 then coreCount else 2
              BWC = 1
              EWS = 200 }

    [<TailCall>]
    member internal _.InterpretAsync workItem evalSteps =
        let mutable currentEff = workItem.Eff
        let mutable currentContStack = workItem.Stack
        let mutable currentEWSteps = evalSteps
        let mutable currentInternalFiber = workItem.IFiber
        let mutable result = Unchecked.defaultof<_>
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    result <- Success res, ContStackPool.Rent(), Evaluated
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop currentContStack
                    match stackFrame.ContType with
                    | SuccessCont ->
                        currentEff <- stackFrame.Cont res
                        loop <- false
                    | FailureCont ->
                        ()

        let inline processError err =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    result <- Failure err, ContStackPool.Rent(), Evaluated
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop currentContStack
                    match stackFrame.ContType with
                    | SuccessCont ->
                        ()
                    | FailureCont ->
                        currentEff <- stackFrame.Cont err
                        loop <- false

        let inline processIntterrupt () =
            ContStackPool.Return currentContStack
            let err = Error (OperationCanceledException "Effect execution was interrupted.")
            result <- Failure err, ContStackPool.Rent(), Evaluated
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
                    if currentInternalFiber.CancellationToken.IsCancellationRequested then
                        processIntterrupt ()
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
                        | Interrupted ->
                            processIntterrupt ()
                        | Action (func, onError) ->
                            try
                                let res = func ()
                                processSuccess res
                            with exn ->
                                processError
                                <| onError exn
                        | SendChan (msg, chan) ->
                            do! chan.SendAsync msg
                            do! activeBlockingEventChan.AddAsync chan
                            processSuccess msg
                        | ReceiveChan chan ->
                            let mutable res = Unchecked.defaultof<_>
                            if chan.Internal.TryTake(&res) then
                                processSuccess res
                            else
                                do! chan.AddBlockingWorkItem
                                    <| WorkItem.Create (ReceiveChan chan, workItem.IFiber, currentContStack, Skipped)
                                let newStack = ContStackPool.Rent()
                                result <- Success (), newStack, Skipped
                                completed <- true
                        | ConcurrentEffect (eff, fiber, ifiber) ->
                            // Register parent cancellation to cancel child
                            currentInternalFiber.CancellationToken.Register(fun () -> ifiber.Interrupt())
                                |> ignore
                            do! activeWorkItemChan.AddAsync
                                <| WorkItem.Create (eff, ifiber, ContStackPool.Rent(), workItem.PrevAction)
                            processSuccess fiber
                        | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
                            do! Task.Run(fun () ->
                                task {
                                    let t = lazyTask ()
                                    try
                                        do! t
                                        do! ifiber.CompleteAndReschedule (Ok ()) activeWorkItemChan
                                    with
                                    | :? OperationCanceledException ->
                                        do! ifiber.CompleteAndReschedule (Error (onError <| TaskCanceledException "Task has been cancelled.")) activeWorkItemChan
                                    | exn ->
                                        do! ifiber.CompleteAndReschedule (Error <| onError exn) activeWorkItemChan
                                } :> Task)
                            processSuccess fiber
                        | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
                            do! Task.Run(fun () ->
                                task {
                                    let t = lazyTask ()
                                    try
                                        let! result = t
                                        do! ifiber.CompleteAndReschedule (Ok result) activeWorkItemChan
                                    with
                                    | :? OperationCanceledException ->
                                        do! ifiber.CompleteAndReschedule (Error (onError <| TaskCanceledException "Task has been cancelled.")) activeWorkItemChan
                                    | exn ->
                                        do! ifiber.CompleteAndReschedule (Error <| onError exn) activeWorkItemChan
                                } :> Task)
                            processSuccess fiber
                        | AwaitFiber ifiber ->
                            if ifiber.IsInterrupted then
                                processError (box (OperationCanceledException()))
                            elif ifiber.Completed then
                                let! res = ifiber.Task
                                processResult res
                            else
                                do! ifiber.AddBlockingWorkItem (WorkItem.Create (AwaitFiber ifiber, workItem.IFiber, currentContStack, Skipped))
                                // Atomically check and reschedule if fiber completed during AddBlockingWorkItem
                                let! _ = ifiber.TryRescheduleBlockingWorkItems activeWorkItemChan
                                let newStack = ContStackPool.Rent()
                                result <- Success (), newStack, Skipped
                                completed <- true
                        | AwaitTPLTask (task, onError) ->
                            try
                                let! res = task
                                processSuccess res
                            with exn ->
                                processError <| onError exn
                        | AwaitGenericTPLTask (task, onError) ->
                            try
                                let! res = task
                                processSuccess res
                            with exn ->
                                processError <| onError exn
                        | ChainSuccess (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add
                            <| ContStackFrame (SuccessCont, cont)
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add
                            <| ContStackFrame (FailureCont, cont)
                return result
            finally
                // Only return the stack if processing didn't complete normally
                // If completed=true, the stack was either already returned (Evaluated)
                // or passed to next work item (RescheduleForRunning/Skipped)
                if not completed then
                    ContStackPool.Return currentContStack
        }

    member private _.Reset () =
        activeWorkItemChan.Clear ()
        activeBlockingEventChan.Clear ()

    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        this.Reset ()
        let fiber = Fiber<'R, 'E> ()
        activeWorkItemChan.AddAsync
        <| WorkItem.Create (eff.Upcast (), fiber.Internal, ContStackPool.Rent(), Evaluated)
        |> ignore
        fiber
