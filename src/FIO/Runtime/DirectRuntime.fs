/// Direct runtime for interpreting FIO effects on the current thread.
module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading.Tasks

/// Runtime that interprets FIO effects on the current thread using .NET Tasks.
type DirectRuntime() =
    inherit FIORuntime()

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    override _.Name = "DirectRuntime"

    /// Interprets an FIO effect iteratively with an explicit continuation stack.
    [<TailCall>]
    member private this.InterpretAsync(eff, currentFiberContext: FiberContext) =
        let mutable state =
            InterpreterState(eff, ContStackPool.Rent(), currentFiberContext, 0)

        let mutable result = Unchecked.defaultof<Result<obj, obj>>

        let inline onSuccessComplete res = result <- Ok res
        let inline onErrorComplete err = result <- Error err

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
                    else
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.SendAsync msg
                                processSuccess &state onSuccessComplete msg
                            | HandleReceiveChan chan ->
                                let! res = chan.ReceiveAsync()
                                processSuccess &state onSuccessComplete res
                            | HandleForkEffect(eff, fiber, fiberContext) ->
                                let registration =
                                    currentFiberContext.CancellationToken.Register(fun () ->
                                        fiberContext.Interrupt(
                                            ParentInterrupted currentFiberContext.Id,
                                            "Parent fiber was interrupted."
                                        ))

                                fiberContext.AddRegistration registration

                                Task.Run(fun () ->
                                    task {
                                        try
                                            try
                                                let! res = this.InterpretAsync(eff, fiberContext)
                                                fiberContext.Complete res
                                            with exn ->
                                                fiberContext.Complete(Error exn)
                                        finally
                                            registration.Dispose()
                                    }
                                    :> Task)
                                |> ignore

                                processSuccess &state onSuccessComplete fiber
                            | HandleForkTask(taskFactory, onError, fiber, fiberContext) ->
                                let registration =
                                    currentFiberContext.CancellationToken.Register(fun () ->
                                        fiberContext.Interrupt(
                                            ParentInterrupted currentFiberContext.Id,
                                            "Parent fiber was interrupted."
                                        ))

                                fiberContext.AddRegistration registration

                                Task.Run(fun () ->
                                    task {
                                        try
                                            try
                                                let t = taskFactory ()
                                                let! result = t.WaitAsync fiberContext.CancellationToken
                                                fiberContext.Complete(Ok result)
                                            with
                                            | :? OperationCanceledException ->
                                                fiberContext.Complete(
                                                    Error(
                                                        onError (
                                                            FiberInterruptedException(
                                                                fiberContext.Id,
                                                                ExplicitInterrupt,
                                                                "Task has been cancelled."
                                                            )
                                                        )
                                                    )
                                                )
                                            | exn -> fiberContext.Complete(Error(onError exn))
                                        finally
                                            registration.Dispose()
                                    }
                                    :> Task)
                                |> ignore

                                processSuccess &state onSuccessComplete fiber
                            | HandleJoinFiber fiberContext ->
                                let! res = fiberContext.Task
                                InterpreterCore.processResult &state onSuccessComplete onErrorComplete res
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

                return result
            finally
                ContStackPool.Return state.ContStack
        }

    override this.Run<'R, 'E>(eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task |> Async.AwaitTask |> Async.RunSynchronously |> ignore
            | _ -> ()

            let fiber = new Fiber<'R, 'E>()
            currentFiber <- Some fiber.Context

            task {
                let! res = this.InterpretAsync(eff.UpcastBoth(), fiber.Context)
                fiber.Context.Complete res
            }
            |> ignore

            fiber)
