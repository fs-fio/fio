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
open System.Threading.Tasks

/// <summary>
/// Represents the direct runtime for FIO, interpreting effects on the current thread.
/// </summary>
type Runtime () =
    inherit FRuntime ()

    override _.Name =
        "Direct"

    [<TailCall>]
    member private this.InterpretAsync eff (currentInternalFiber: InternalFiber) =
        let mutable currentEff = eff
        let mutable currentContStack = ContStackPool.Rent ()
        let mutable result = Unchecked.defaultof<_>
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    result <- Ok res
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
                    result <- Error err
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
                    if currentInternalFiber.CancellationToken.IsCancellationRequested then
                        processInterrupt ()
                    else
                        match currentEff with
                        | Success res ->
                            processSuccess res
                        | Failure err ->
                            processError err
                        | Interruption (cause, msg) ->
                            currentInternalFiber.Interrupt cause msg
                            processInterrupt ()
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
                            // Register cancellation BEFORE scheduling child to avoid race
                            let registration =
                                currentInternalFiber.CancellationToken.Register(
                                    fun () -> ifiber.Interrupt (ParentInterrupted currentInternalFiber.Id) "Parent fiber was interrupted.")

                            Task.Run(fun () ->
                                task {
                                    try
                                        try
                                            let! res = this.InterpretAsync eff ifiber
                                            do! ifiber.Complete res
                                        with exn ->
                                            // InterpretAsync threw an unexpected exception (shouldn't happen by design)
                                            // Complete the fiber with the error to prevent unobserved exceptions
                                            do! ifiber.Complete <| Error exn
                                    finally
                                        // Always dispose registration when child completes
                                        registration.Dispose ()
                                } :> Task) |> ignore
                            processSuccess fiber
                        | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
                            // Register cancellation BEFORE scheduling child to avoid race
                            let registration =
                                currentInternalFiber.CancellationToken.Register(
                                    fun () -> ifiber.Interrupt (ParentInterrupted currentInternalFiber.Id) "Parent fiber was interrupted.")

                            Task.Run(fun () ->
                                task {
                                    try
                                        let t = lazyTask ()
                                        try
                                            do! t
                                            do! ifiber.Complete (Ok ())
                                        with
                                        | :? OperationCanceledException ->
                                            do! ifiber.Complete (Error (onError <| FiberInterruptedException (ifiber.Id, ExplicitInterrupt, "Task has been cancelled.")))
                                        | exn ->
                                            do! ifiber.Complete (Error <| onError exn)
                                    finally
                                        // Always dispose registration when child completes
                                        registration.Dispose ()
                                } :> Task) |> ignore
                            processSuccess fiber
                        | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
                            // Register cancellation BEFORE scheduling child to avoid race
                            let registration =
                                currentInternalFiber.CancellationToken.Register(
                                    fun () -> ifiber.Interrupt (ParentInterrupted currentInternalFiber.Id) "Parent fiber was interrupted.")

                            Task.Run(fun () ->
                                task {
                                    try
                                        let t = lazyTask ()
                                        try
                                            let! result = t
                                            do! ifiber.Complete (Ok result)
                                        with
                                        | :? OperationCanceledException ->
                                            do! ifiber.Complete (Error (onError <| FiberInterruptedException (ifiber.Id, ExplicitInterrupt, "Task has been cancelled.")))
                                        | exn ->
                                            do! ifiber.Complete (Error <| onError exn)
                                    finally
                                        // Always dispose registration when child completes
                                        registration.Dispose ()
                                } :> Task) |> ignore
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
                            currentContStack.Add
                            <| ContStackFrame (SuccessCont cont)
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add
                            <| ContStackFrame (FailureCont cont)
                return result
            finally
                ContStackPool.Return currentContStack
        }

    override this.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        let fiber = new Fiber<'R, 'E> ()
        task {
            try
                let! res = this.InterpretAsync (eff.Upcast ()) fiber.Internal
                do! fiber.Internal.Complete res
            finally
                (fiber.Internal :> IDisposable).Dispose ()
        } |> ignore
        fiber
