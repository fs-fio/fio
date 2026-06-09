module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading.Tasks

type DirectRuntime() =
    inherit FIORuntime()

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    override _.Name = "DirectRuntime"

    [<TailCall>]
    member private this.InterpretAsync(eff, currentFiberContext: FiberContext) =
        let mutable state =
            InterpreterState(eff, ContStackPool.Rent(), currentFiberContext, 0)

        let mutable result = ValueNone

        let inline onSuccessComplete value = result <- ValueSome(Ok value)
        let inline onErrorComplete error = result <- ValueSome(Error error)

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
                    else
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.WriteAsync msg
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess msg)
                            | HandleReceiveChan chan ->
                                let! value = chan.ReadAsync()
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                            | HandleForkEffect(eff, fiber, fiberContext) ->
                                let registration = setupForkRegistration currentFiberContext fiberContext

                                Task.Run(fun () ->
                                    task {
                                        try
                                            try
                                                let! value = this.InterpretAsync(eff, fiberContext)
                                                fiberContext.Complete value
                                            with exn ->
                                                fiberContext.Complete(Error exn)
                                        finally
                                            registration.Dispose()
                                    }
                                    :> Task)
                                |> ignore

                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess fiber)
                            | HandleJoinFiber fiberContext ->
                                let! value = fiberContext.Task
                                processResult &state onSuccessComplete onErrorComplete value
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

                return result.Value
            finally
                ContStackPool.Return state.ContStack
        }

    override this.Run<'A, 'E> (eff: FIO<'A, 'E>) : Fiber<'A, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task.GetAwaiter().GetResult() |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.Cancel()
            | None -> ()

            let fiber = new Fiber<'A, 'E>()
            currentFiber <- Some fiber.Context

            let t =
                task {
                    let! value = this.InterpretAsync(eff.UpcastBoth(), fiber.Context)
                    fiber.Context.Complete value
                }

            t.ContinueWith(
                (fun (t: Task) ->
                    if t.IsFaulted then
                        let exn: exn =
                            match t.Exception with
                            | null -> upcast InvalidOperationException "DirectRuntime task faulted without exception."
                            | aggr when aggr.InnerExceptions.Count = 1 -> aggr.InnerExceptions[0]
                            | aggr -> upcast aggr

                        fiber.Context.Complete(Error(box exn))),
                TaskContinuationOptions.OnlyOnFaulted
            )
            |> ignore

            fiber)
