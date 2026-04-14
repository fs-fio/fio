/// <summary>
/// Provides the direct runtime for interpreting FIO effects, executing effects on the current thread.
/// </summary>
module FIO.Runtime.Direct

open FIO.DSL

open System
open System.Threading.Tasks

/// <summary>
/// The direct runtime for FIO, interpreting effects on the current thread.
/// </summary>
type DirectRuntime() =
    inherit FIORuntime()

    let mutable currentFiber: FiberContext option = None
    let runLock = obj ()

    override _.Name = "DirectRuntime"

    /// <summary>
    /// Interprets an FIO effect asynchronously within the given fiber context.
    /// </summary>
    /// <param name="eff">The effect to interpret.</param>
    /// <param name="currentFiberContext">The fiber context for execution.</param>
    [<TailCall>]
    member private this.InterpretAsync(eff, currentFiberContext: FiberContext) =
        let mutable currentEff = eff
        let mutable currentContStack = ContStackPool.Rent()
        let mutable result = Unchecked.defaultof<_>
        let mutable completed = false
        let mutable interruptionSuppressed = 0

        let inline processSuccess res =
            let mutable loop = true

            while loop do
                if currentContStack.Count = 0 then
                    result <- Ok res
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()

                    match stackFrame.Cont with
                    | SuccessCont cont ->
                        currentEff <- cont res
                        loop <- false
                    | FailureCont _ -> ()
                    | FinalizerCont finalizer ->
                        interruptionSuppressed <- interruptionSuppressed + 1
                        let onFinSuccess: obj -> FIO<obj, obj> = fun _ -> FinalizerResult(Ok res)
                        let onFinError: obj -> FIO<obj, obj> = fun finErr -> FinalizerResult(Error finErr)
                        currentContStack.Push(ContStackFrame(FailureCont onFinError))
                        currentContStack.Push(ContStackFrame(SuccessCont onFinSuccess))
                        currentEff <- finalizer
                        loop <- false

        let inline processError err =
            let mutable loop = true

            while loop do
                if currentContStack.Count = 0 then
                    result <- Error err
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()

                    match stackFrame.Cont with
                    | SuccessCont _ -> ()
                    | FailureCont cont ->
                        currentEff <- cont err
                        loop <- false
                    | FinalizerCont finalizer ->
                        interruptionSuppressed <- interruptionSuppressed + 1
                        let onFinDone: obj -> FIO<obj, obj> = fun _ -> FinalizerResult(Error err)
                        currentContStack.Push(ContStackFrame(FailureCont onFinDone))
                        currentContStack.Push(ContStackFrame(SuccessCont onFinDone))
                        currentEff <- finalizer
                        loop <- false

        let inline processInterruptError err =
            let mutable loop = true

            while loop do
                if currentContStack.Count = 0 then
                    result <- Error err
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()

                    match stackFrame.Cont with
                    | SuccessCont _
                    | FailureCont _ -> ()
                    | FinalizerCont finalizer ->
                        interruptionSuppressed <- interruptionSuppressed + 1
                        let resumeInterrupt: obj -> FIO<obj, obj> = fun _ -> ResumeInterrupt err
                        currentContStack.Push(ContStackFrame(FailureCont resumeInterrupt))
                        currentContStack.Push(ContStackFrame(SuccessCont resumeInterrupt))
                        currentEff <- finalizer
                        loop <- false

        let inline processResult res =
            match res with
            | Ok res -> processSuccess res
            | Error err -> processError err

        task {
            try
                while not completed do
                    if
                        interruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ -> raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error err -> processInterruptError err
                    else
                        match currentEff with
                        | Success res -> processSuccess res
                        | Failure err -> processError err
                        | InterruptFiber(cause, msg, fiberContext) ->
                            fiberContext.Interrupt(cause, msg)
                            processSuccess ()
                        | InterruptSelf(cause, msg) ->
                            currentFiberContext.Interrupt(cause, msg)
                            processInterruptError (FiberInterruptedException(currentFiberContext.Id, cause, msg) :> obj)
                        | Action(func, onError) ->
                            try
                                let res = func ()
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | SendChan(msg, chan) ->
                            do! chan.SendAsync msg
                            processSuccess msg
                        | ReceiveChan chan ->
                            let! res = chan.ReceiveAsync()
                            processSuccess res
                        | ForkEffect(eff, fiber, fiberContext) ->
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

                            processSuccess fiber
                        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
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
                                        let t = taskFactory ()

                                        try
                                            do! t.WaitAsync fiberContext.CancellationToken
                                            fiberContext.Complete(Ok())
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

                            processSuccess fiber
                        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
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
                                        let t = taskFactory ()

                                        try
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

                            processSuccess fiber
                        | JoinFiber fiberContext ->
                            let! res = fiberContext.Task
                            processResult res
                        | AwaitTPLTask(task, onError) ->
                            try
                                if interruptionSuppressed > 0 then
                                    do! task
                                else
                                    do! task.WaitAsync currentFiberContext.CancellationToken

                                processSuccess ()
                            with
                            | :? OperationCanceledException when
                                currentFiberContext.CancellationToken.IsCancellationRequested
                                ->
                                processInterruptError (
                                    FiberInterruptedException(
                                        currentFiberContext.Id,
                                        ExplicitInterrupt,
                                        "Task has been cancelled."
                                    )
                                )
                            | exn -> processError (onError exn)
                        | AwaitGenericTPLTask(task, onError) ->
                            try
                                let! res =
                                    if interruptionSuppressed > 0 then
                                        task
                                    else
                                        task.WaitAsync currentFiberContext.CancellationToken

                                processSuccess res
                            with
                            | :? OperationCanceledException when
                                currentFiberContext.CancellationToken.IsCancellationRequested
                                ->
                                processInterruptError (
                                    FiberInterruptedException(
                                        currentFiberContext.Id,
                                        ExplicitInterrupt,
                                        "Task has been cancelled."
                                    )
                                )
                            | exn -> processError (onError exn)
                        | ChainSuccess(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame(SuccessCont cont))
                        | ChainError(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame(FailureCont cont))
                        | OnFinalize(eff, finalizer) ->
                            currentContStack.Push(ContStackFrame(FinalizerCont finalizer))
                            currentEff <- eff
                        | ResumeInterrupt err ->
                            interruptionSuppressed <- interruptionSuppressed - 1
                            processInterruptError err
                        | FinalizerResult r ->
                            interruptionSuppressed <- interruptionSuppressed - 1

                            match r with
                            | Ok res -> processSuccess res
                            | Error err -> processError err

                return result
            finally
                ContStackPool.Return currentContStack
        }

    /// <summary>
    /// Runs an FIO effect and returns a fiber representing its execution.
    /// </summary>
    /// <param name="eff">The FIO effect to run.</param>
    /// <returns>A fiber representing the running effect.</returns>
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
