module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks

/// A single-threaded runtime that runs effects synchronously, waiting on blocked fibers. Handy for tests and simple programs.
type DirectRuntime() =
    inherit FIORuntime()

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    override _.Name = "DirectRuntime"

    [<TailCall>]
    member private this.InterpretAsync effect (currentFiberContext: FiberContext) =
        let mutable state =
            InterpreterState(effect, ContStackPool.Rent(), currentFiberContext, 0)

        let mutable result = ValueNone

        let inline onSuccessComplete value =
            result <- ValueSome <| Ok value

        let inline onErrorComplete error =
            result <- ValueSome <| Error error

        task {
            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ ->
                            raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error error ->
                            processOutcome
                                &state
                                onSuccessComplete
                                onErrorComplete
                                (OutcomeInterrupted error)
                    else
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone ->
                            ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleWriteChan(message, channel) ->
                                do! channel.WriteAsync message
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded message)
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead &value then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                else
                                    try
                                        let mutable waiting = true
                                        while waiting do
                                            let! _ =
                                                if state.InterruptionSuppressed > 0 then
                                                    channel.Queue.WaitToReadAsync CancellationToken.None
                                                else
                                                    channel.Queue.WaitToReadAsync currentFiberContext.CancellationToken
                                            if channel.Queue.TryRead &value then
                                                waiting <- false
                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            (OutcomeSucceeded value)
                                    with
                                    | :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            (OutcomeInterrupted (FiberInterruptedException(
                                                currentFiberContext.Id,
                                                ExplicitInterrupt,
                                                "Fiber was interrupted while blocked on a channel read."
                                            )))
                            | HandleForkEffect(effect, fiber, fiberContext) ->
                                let registration = setupForkRegistration currentFiberContext fiberContext
                                Task.Run(fun () ->
                                    task {
                                        try
                                            try
                                                let! value = this.InterpretAsync effect fiberContext
                                                fiberContext.Complete value
                                            with exn ->
                                                fiberContext.Complete <| Error exn
                                        finally
                                            registration.Dispose()
                                    }
                                    :> Task)
                                |> ignore
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded fiber)
                            | HandleJoinFiber fiberContext ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            fiberContext.Task
                                        else
                                            fiberContext.Task.WaitAsync currentFiberContext.CancellationToken
                                    processResult
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        value
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupted (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Fiber was interrupted while blocked on a fiber join."
                                        )))
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            task
                                        else
                                            task.WaitAsync currentFiberContext.CancellationToken
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupted (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Task has been cancelled."
                                        )))
                                | ex ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeFailed (onError ex))
                return result.Value
            finally
                ContStackPool.Return state.ContStack
        }

    override this.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        lock runLock <| fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task.GetAwaiter().GetResult() |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.Cancel()
            | None -> ()

            let fiber = new Fiber<'A, 'E>()
            currentFiber <- Some fiber.Context

            let task =
                task {
                    let! value = this.InterpretAsync (effect.UpcastBoth()) fiber.Context
                    fiber.Context.Complete value
                }

            task.ContinueWith(
                (fun (task: Task) ->
                    if task.IsFaulted then
                        let exn: exn =
                            match task.Exception with
                            | null ->
                                upcast InvalidOperationException "DirectRuntime task faulted without exception."
                            | aggr when aggr.InnerExceptions.Count = 1 ->
                                aggr.InnerExceptions[0]
                            | aggr ->
                                upcast aggr
                        fiber.Context.Complete(Error(box exn))),
                TaskContinuationOptions.OnlyOnFaulted) |> ignore

            fiber
