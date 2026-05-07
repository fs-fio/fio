/// <summary>Provides the direct FIO runtime, which interprets effects on .NET tasks without dedicated worker threads.</summary>
module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading.Tasks

/// <summary>Represents the FIO runtime that interprets effects directly on the calling thread, dispatching forked work onto .NET tasks.</summary>
type DirectRuntime() =
    inherit FIORuntime()

    /// <summary>Represents the fiber context of the most recently started root fiber.</summary>
    let mutable currentFiber: FiberContext option = None

    /// <summary>Represents the lock that serializes calls to <c>Run</c>.</summary>
    let runLock = obj ()

    /// <summary>Returns the human-readable name of this runtime.</summary>
    /// <returns>The runtime's name.</returns>
    override _.Name = "DirectRuntime"

    /// <summary>Transforms an effect tree into a completed <c>Result</c> by interpreting each node on the calling task.</summary>
    /// <param name="eff">The type-erased effect to evaluate.</param>
    /// <param name="currentFiberContext">The fiber context that owns this evaluation.</param>
    /// <returns>A task that completes with <c>Ok</c> on success or <c>Error</c> on failure or interruption.</returns>
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
                                let registration = setupForkRegistration currentFiberContext fiberContext

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
                                let registration = setupForkRegistration currentFiberContext fiberContext

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

    /// <summary>Creates a new fiber that interprets the given effect on this runtime.</summary>
    /// <typeparam name="'R">The success result type produced by the effect.</typeparam>
    /// <typeparam name="'E">The typed error type the effect may fail with.</typeparam>
    /// <param name="eff">The effect to interpret.</param>
    /// <returns>A fiber that runs <paramref name="eff"/> and exposes its terminal state.</returns>
    /// <remarks><c>Run</c> is intended for sequential invocation; if a prior fiber is still running, the call blocks until it completes. Concurrency within a single <c>Run</c> is supplied by forked fibers, not by concurrent calls to <c>Run</c>.</remarks>
    override this.Run<'R, 'E>(eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task.GetAwaiter().GetResult() |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.CancelToken()
            | None -> ()

            let fiber = new Fiber<'R, 'E>()
            currentFiber <- Some fiber.Context

            task {
                let! res = this.InterpretAsync(eff.UpcastBoth(), fiber.Context)
                fiber.Context.Complete res
            }
            |> ignore

            fiber)
