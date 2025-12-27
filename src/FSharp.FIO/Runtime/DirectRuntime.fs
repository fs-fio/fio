/// <summary>
/// Provides the direct runtime for interpreting FIO effects, executing effects on the current thread.
/// </summary>
module FSharp.FIO.Runtime.Direct

open FSharp.FIO.DSL

open System
open System.Threading.Tasks

/// <summary>
/// The direct runtime for FIO, interpreting effects on the current thread.
/// </summary>
type DirectRuntime () =
    inherit FRuntime ()

    override _.Name =
        "DirectRuntime"

    /// <summary>
    /// Interprets an FIO effect asynchronously within the given fiber context.
    /// </summary>
    /// <param name="eff">The effect to interpret.</param>
    /// <param name="currentFiberContext">The fiber context for execution.</param>
    [<TailCall>]
    member private this.InterpretAsync (eff, currentFiberContext: FiberContext) =
        let mutable currentEff = eff
        let mutable currentContStack = ContStackPool.Rent()
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

        let inline processInterruptError err =
            ContStackPool.Return currentContStack
            result <- Error err
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
                    else
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
                                processError(onError exn)
                        | SendChan(msg, chan) ->
                            do! chan.SendAsync msg
                            processSuccess msg
                        | ReceiveChan chan ->
                            let! res = chan.ReceiveAsync()
                            processSuccess res
                        | ForkEffect(eff, fiber, fiberContext) ->
                            let registration =
                                currentFiberContext.CancellationToken.Register(
                                    fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            Task.Run(fun () ->
                                task {
                                    try
                                        try
                                            let! res = this.InterpretAsync(eff, fiberContext)
                                            fiberContext.Complete res
                                        with exn ->
                                            fiberContext.Complete (Error exn)
                                    finally
                                        registration.Dispose()
                                } :> Task) |> ignore
                            processSuccess fiber
                        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration =
                                currentFiberContext.CancellationToken.Register(
                                    fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            Task.Run(fun () ->
                                task {
                                    try
                                        let t = taskFactory()
                                        try
                                            do! t.WaitAsync fiberContext.CancellationToken
                                            fiberContext.Complete(Ok ())
                                        with
                                        | :? OperationCanceledException ->
                                            fiberContext.Complete (Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                        | exn ->
                                            fiberContext.Complete (Error (onError exn))
                                    finally
                                        registration.Dispose()
                                } :> Task) |> ignore
                            processSuccess fiber
                        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration =
                                currentFiberContext.CancellationToken.Register(
                                    fun () -> fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            Task.Run(fun () ->
                                task {
                                    try
                                        let t = taskFactory()
                                        try
                                            let! result = t.WaitAsync fiberContext.CancellationToken
                                            fiberContext.Complete(Ok result)
                                        with
                                        | :? OperationCanceledException ->
                                            fiberContext.Complete (Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                        | exn ->
                                            fiberContext.Complete (Error (onError exn))
                                    finally
                                        registration.Dispose()
                                } :> Task) |> ignore
                            processSuccess fiber
                        | JoinFiber fiberContext ->
                            let! res = fiberContext.Task
                            processResult res
                        | AwaitTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with exn ->
                                processError(onError exn)
                        | AwaitGenericTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with exn ->
                                processError(onError exn)
                        | ChainSuccess(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add(ContStackFrame (SuccessCont cont))
                        | ChainError(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Add(ContStackFrame (FailureCont cont))
                return result
            finally
                ContStackPool.Return currentContStack
        }

    override this.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        let fiber = new Fiber<'R, 'E>()
        task {
            try
                let! res = this.InterpretAsync (eff.Upcast(), fiber.Internal)
                fiber.Internal.Complete res
            finally
                (fiber.Internal :> IDisposable).Dispose()
        } |> ignore
        fiber
