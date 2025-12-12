(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2025 - Daniel "iyyel" Larsen and Technical University of Denmark (DTU) *)
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

type private EvaluationWorkerConfig =
    { Runtime: Runtime
      ActiveWorkItemChan: InternalChannel<WorkItem>
      BlockingWorker: BlockingWorker
      EWSteps: int }

and private BlockingWorkerConfig =
    { ActiveWorkItemChan: InternalChannel<WorkItem>
      ActiveBlockingDataChan: InternalChannel<BlockingData> }

and private EvaluationWorker (config: EvaluationWorkerConfig) =

    let processWorkItem (workItem: WorkItem) =
        task {
            match! config.Runtime.InterpretAsync workItem config.EWSteps with
            | Success res, _, Evaluated ->
                do! workItem.Complete <| Ok res
            | Failure err, _, Evaluated ->
                do! workItem.Complete <| Error err
            | eff, stack, RescheduleForRunning ->
                do! config.ActiveWorkItemChan.AddAsync
                    <| WorkItem.Create (eff, workItem.IFiber, stack, RescheduleForRunning)
            | eff, stack, RescheduleForBlocking blockingItem ->
                do! config.BlockingWorker.RescheduleForBlocking
                    <| BlockingData.Create (blockingItem,
                        WorkItem.Create (eff, workItem.IFiber, stack, RescheduleForBlocking blockingItem))
            | _ ->
                printfn "ERROR: EvaluationWorker: Unexpected state encountered during effect interpretation!"
                invalidOp "EvaluationWorker: Unexpected state encountered during effect interpretation!"
        }

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop do
                    let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync ()
                    if not hasWorkItem then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemChan.TakeAsync ()
                        do! processWorkItem workItem
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    let cancellationTokenSource = new CancellationTokenSource ()
    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel ()

and private BlockingWorker (config: BlockingWorkerConfig) =
    
    let processBlockingChannel blockingData (blockingChan: Channel<obj>) =
        if blockingChan.Count > 0 then
            config.ActiveWorkItemChan.AddAsync blockingData.WaitingWorkItem
        else
            config.ActiveBlockingDataChan.AddAsync blockingData
            
    let processBlockingIFiber blockingData (ifiber: InternalFiber) =
        if ifiber.Completed then
            config.ActiveWorkItemChan.AddAsync blockingData.WaitingWorkItem
        else
            config.ActiveBlockingDataChan.AddAsync blockingData

    let rec processBlockingData blockingData =
        task {
            match blockingData.BlockingItem with
            | BlockingChannel blockingChan ->
                do! processBlockingChannel blockingData blockingChan
            | BlockingIFiber blockingIFiber ->
                do! processBlockingIFiber blockingData blockingIFiber
        }
    
    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop do
                    let! hasBlockingItem = config.ActiveBlockingDataChan.WaitToTakeAsync ()
                    if not hasBlockingItem then
                        loop <- false 
                    else
                        let! blockingData = config.ActiveBlockingDataChan.TakeAsync ()
                        do! processBlockingData blockingData
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    let cancellationTokenSource = new CancellationTokenSource ()
    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel ()

    member internal _.RescheduleForBlocking blockingData =
        config.ActiveBlockingDataChan.AddAsync blockingData

/// <summary>
/// Represents the cooperative runtime for FIO, interpreting effects concurrently using work-stealing and multiple workers.
/// </summary>
and Runtime (config: WorkerConfig) as this =
    inherit FWorkerRuntime(config)
    
    let activeWorkItemChan = InternalChannel<WorkItem> ()
    let activeBlockingDataChan = InternalChannel<BlockingData> ()

    let createBlockingWorkers count =
        List.init count <| fun _ ->
            new BlockingWorker({
                ActiveWorkItemChan = activeWorkItemChan
                ActiveBlockingDataChan = activeBlockingDataChan
            })

    let createEvaluationWorkers runtime blockingWorker evalSteps count =
        List.init count <| fun _ -> 
            new EvaluationWorker({ 
                Runtime = runtime
                ActiveWorkItemChan = activeWorkItemChan
                BlockingWorker = blockingWorker
                EWSteps = evalSteps 
            })

    do
        let blockingWorkers = createBlockingWorkers config.BWC
        // Currently we take head of the list, as the IntermediateRuntime
        // only supports a single blocking worker.
        createEvaluationWorkers this (List.head blockingWorkers) config.EWS config.EWC
        |> ignore

    override _.Name =
        "Cooperative"

    new() =
        Runtime
            { EWC =
                let coreCount = Environment.ProcessorCount - 1
                if coreCount >= 2 then coreCount else 2
              BWC = 1
              EWS = 200 }

    [<TailCall>]
    member internal _.InterpretAsync workItem evalSteps =
        let mutable currentEff = workItem.Eff
        let mutable currentContStack = workItem.Stack
        let mutable currentPrevAction = workItem.PrevAction
        let mutable currentEWSteps = evalSteps
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
                        currentPrevAction <- Evaluated
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
                        currentPrevAction <- Evaluated
                        loop <- false

        let inline processResult res =
            match res with
            | Ok res ->
                processSuccess res
            | Error err ->
                processError err

        task {
            while not completed do
                if currentEWSteps = 0 then
                    result <- currentEff, currentContStack, RescheduleForRunning
                    completed <- true
                else
                    currentEWSteps <- currentEWSteps - 1
                    match currentEff with
                    | Success res ->
                        processSuccess res
                    | Failure err ->
                        processError err
                    | Action (func, onError) ->
                        try 
                            let res = func ()
                            processSuccess res
                        with exn ->
                            processError
                            <| onError exn
                    | SendChan (msg, chan) ->
                        do! chan.SendAsync msg
                        processSuccess msg
                    | ReceiveChan chan ->
                        if chan.Count > 0 then
                            let! res = chan.ReceiveAsync ()
                            processSuccess res
                        else
                            let newPrevAction = RescheduleForBlocking <| BlockingChannel chan
                            currentPrevAction <- newPrevAction
                            result <- ReceiveChan chan, currentContStack, newPrevAction
                            completed <- true
                    | ConcurrentEffect (eff, fiber, ifiber) ->
                        do! activeWorkItemChan.AddAsync
                            <| WorkItem.Create (eff, ifiber, ContStackPool.Rent(), currentPrevAction)
                        processSuccess fiber
                    | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
                        do! Task.Run(fun () -> 
                            (lazyTask ()).ContinueWith((fun (t: Task) ->
                                if t.IsFaulted then
                                    ifiber.Complete
                                    <| Error (onError t.Exception.InnerException)
                                elif t.IsCanceled then
                                    ifiber.Complete
                                    <| Error (onError <| TaskCanceledException "Task has been cancelled.")
                                elif t.IsCompleted then
                                    ifiber.Complete
                                    <| Ok ()
                                else
                                    ifiber.Complete
                                    <| Error (onError <| InvalidOperationException "Task not completed.")),
                                CancellationToken.None,
                                TaskContinuationOptions.RunContinuationsAsynchronously,
                                TaskScheduler.Default) :> Task)
                        processSuccess fiber
                    | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
                        do! Task.Run(fun () -> 
                            (lazyTask ()).ContinueWith((fun (t: Task<obj>) ->
                                if t.IsFaulted then
                                    ifiber.Complete
                                    <| Error (onError t.Exception.InnerException)
                                elif t.IsCanceled then
                                    ifiber.Complete
                                    <| Error (onError <| TaskCanceledException "Task has been cancelled.")
                                elif t.IsCompleted then
                                    ifiber.Complete
                                    <| Ok t.Result
                                else
                                    ifiber.Complete
                                    <| Error (onError <| InvalidOperationException "Task not completed.")),
                                CancellationToken.None,
                                TaskContinuationOptions.RunContinuationsAsynchronously,
                                TaskScheduler.Default) :> Task)
                        processSuccess fiber
                    | AwaitFiber ifiber ->
                        if ifiber.Completed then
                            let! res = ifiber.Task
                            processResult res
                        else
                            let newPrevAction = RescheduleForBlocking <| BlockingIFiber ifiber
                            currentPrevAction <- newPrevAction
                            result <- AwaitFiber ifiber, currentContStack, newPrevAction
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
        }
        
    member private _.Reset () =
        activeWorkItemChan.Clear ()
        activeBlockingDataChan.Clear ()

    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        this.Reset ()
        let fiber = Fiber<'R, 'E> ()
        activeWorkItemChan.AddAsync
        <| WorkItem.Create (eff.Upcast (), fiber.Internal, ContStackPool.Rent(), Evaluated)
        |> ignore
        fiber
