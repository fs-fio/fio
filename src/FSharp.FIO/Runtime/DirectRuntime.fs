(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides the direct runtime for interpreting FIO effects, executing effects on the current thread.
/// </summary>
module FSharp.FIO.Runtime.Direct

open FSharp.FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Represents the direct runtime for FIO, interpreting effects on the current thread.
/// </summary>
type Runtime () =
    inherit FRuntime ()

    override _.Name =
        "Direct"

    [<TailCall>]
    member private this.InterpretAsync eff =
        let mutable currentEff = eff
        let mutable contStack = ContStackPool.Rent()
        let mutable result = Unchecked.defaultof<_>
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if contStack.Count = 0 then
                    result <- Ok res
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop contStack
                    match stackFrame.ContType with
                    | SuccessCont ->
                        currentEff <- stackFrame.Cont res
                        loop <- false
                    | FailureCont ->
                        ()

        let inline processError err =
            let mutable loop = true
            while loop do
                if contStack.Count = 0 then
                    result <- Error err
                    completed <- true
                    loop <- false
                else
                    let stackFrame = pop contStack
                    match stackFrame.ContType with
                    | SuccessCont ->
                        ()
                    | FailureCont ->
                        currentEff <- stackFrame.Cont err
                        loop <- false

        let inline processResult res =
            match res with
            | Ok res ->
                processSuccess res
            | Error err ->
                processError err

        task {
            while not completed do
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
                    let! res = chan.ReceiveAsync ()
                    processSuccess res
                | ConcurrentEffect (eff, fiber, ifiber) ->
                    // This runs the task on a separate thread pool with proper error handling
                    (Task.Run(fun () ->
                        task {
                            let! res = this.InterpretAsync eff
                            do! ifiber.Complete res
                        } :> Task
                    )).ContinueWith(
                        (fun (t: Task) ->
                            if t.IsFaulted then
                                // InterpretAsync threw an unexpected exception (shouldn't happen by design)
                                // Complete the fiber with the error to prevent unobserved exceptions
                                ifiber.Complete(Error (t.Exception.GetBaseException() :> obj))
                                |> ignore
                        ),
                        CancellationToken.None,
                        TaskContinuationOptions.OnlyOnFaulted,
                        TaskScheduler.Default
                    ) |> ignore
                    processSuccess fiber
                | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
                    Task.Run(fun () ->
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
                            TaskScheduler.Default) :> Task
                    ) |> ignore
                    processSuccess fiber
                | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
                    Task.Run(fun () ->
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
                            TaskScheduler.Default) :> Task
                    ) |> ignore
                    processSuccess fiber
                | AwaitFiber ifiber ->
                    let! res = ifiber.Task
                    processResult res
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
                    contStack.Add
                    <| ContStackFrame (SuccessCont, cont)
                | ChainError (eff, cont) ->
                    currentEff <- eff
                    contStack.Add
                    <| ContStackFrame (FailureCont, cont)

            ContStackPool.Return contStack
            return result
        }

    override this.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        let fiber = Fiber<'R, 'E> ()
        task {
            let! res = this.InterpretAsync
                       <| eff.Upcast ()
            do! fiber.Internal.Complete res
        } |> ignore
        fiber
