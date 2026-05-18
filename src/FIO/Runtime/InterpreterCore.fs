/// <summary>Represents shared interpreter logic used by all FIO runtimes to evaluate effect trees.</summary>
module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System.Threading.Tasks

/// <summary>Represents the mutable state carried through a single evaluation step of the interpreter.</summary>
[<Struct; NoComparison; NoEquality>]
type InterpreterState =
    /// <summary>Represents the current effect being evaluated.</summary>
    val mutable Eff: FIO<obj, obj>
    /// <summary>Represents the continuation stack for pending success, error, and finalizer continuations.</summary>
    val mutable ContStack: ContStack
    /// <summary>Represents the fiber context owning this evaluation.</summary>
    val mutable FiberContext: FiberContext
    /// <summary>Represents whether the interpreter has finished processing the current work item.</summary>
    val mutable Completed: bool
    /// <summary>Represents the count of nested finalizer scopes that suppress interruption checks.</summary>
    val mutable InterruptionSuppressed: int

    /// <summary>Creates an interpreter state initialized with the given effect, continuation stack, fiber context, and interruption suppression count.</summary>
    /// <param name="eff">The effect to begin evaluating.</param>
    /// <param name="contStack">The initial continuation stack.</param>
    /// <param name="fiberContext">The fiber context owning this evaluation.</param>
    /// <param name="interruptionSuppressed">The initial interruption suppression depth.</param>
    new(eff, contStack, fiberContext, interruptionSuppressed) =
        {
            Eff = eff
            ContStack = contStack
            FiberContext = fiberContext
            Completed = false
            InterruptionSuppressed = interruptionSuppressed
        }

/// <summary>Represents an effect case that requires runtime-specific handling after shared interpretation.</summary>
[<Struct; NoComparison; NoEquality>]
type RuntimeCase =
    /// <summary>Represents a send operation carrying a message and the target channel.</summary>
    | HandleSendChan of msg: obj * chan: Channel<obj>
    /// <summary>Represents a receive operation on a channel.</summary>
    | HandleReceiveChan of chan: Channel<obj>
    /// <summary>Represents a fork of a child effect into a new fiber.</summary>
    | HandleForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    /// <summary>Represents a fork of a .NET task factory into a new fiber.</summary>
    | HandleForkTask of
        taskFactory: (unit -> Task<obj>) *
        onError: (exn -> obj) *
        fiber: obj *
        fiberContext: FiberContext
    /// <summary>Represents a join that waits for a fiber to reach its terminal state.</summary>
    | HandleJoinFiber of fiberContext: FiberContext
    /// <summary>Represents an await of an in-flight .NET task.</summary>
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

/// <summary>Transforms a success value by unwinding the continuation stack until a matching success or finalizer continuation is found.</summary>
/// <param name="state">The mutable interpreter state carrying the continuation stack and completion flag.</param>
/// <param name="onComplete">The callback invoked when no more continuations remain and the fiber should complete with a success value.</param>
/// <param name="res">The success value to propagate.</param>
let inline processSuccess (state: byref<InterpreterState>) ([<InlineIfLambda>] onComplete: obj -> unit) (res: obj) =
    let mutable loop = true

    while loop do
        if state.ContStack.Count = 0 then
            onComplete res
            state.Completed <- true
            loop <- false
        else
            let stackFrame = state.ContStack.Pop()

            match stackFrame.Cont with
            | SuccessCont cont ->
                try
                    state.Eff <- cont res
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | FailureCont _ -> ()
            | FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                let onFinSuccess: obj -> FIO<obj, obj> = fun _ -> FinalizerResult(Ok res)
                let onFinError: obj -> FIO<obj, obj> = fun finErr -> FinalizerResult(Error finErr)
                state.ContStack.Push(ContStackFrame(FailureCont onFinError))
                state.ContStack.Push(ContStackFrame(SuccessCont onFinSuccess))
                state.Eff <- finalizer
                loop <- false

/// <summary>Transforms an error value by unwinding the continuation stack until a matching failure or finalizer continuation is found.</summary>
/// <param name="state">The mutable interpreter state carrying the continuation stack and completion flag.</param>
/// <param name="onComplete">The callback invoked when no more continuations remain and the fiber should complete with an error value.</param>
/// <param name="err">The error value to propagate.</param>
let inline processError (state: byref<InterpreterState>) ([<InlineIfLambda>] onComplete: obj -> unit) (err: obj) =
    let mutable loop = true

    while loop do
        if state.ContStack.Count = 0 then
            onComplete err
            state.Completed <- true
            loop <- false
        else
            let stackFrame = state.ContStack.Pop()

            match stackFrame.Cont with
            | SuccessCont _ -> ()
            | FailureCont cont ->
                try
                    state.Eff <- cont err
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                let onFinDone: obj -> FIO<obj, obj> = fun _ -> FinalizerResult(Error err)
                state.ContStack.Push(ContStackFrame(FailureCont onFinDone))
                state.ContStack.Push(ContStackFrame(SuccessCont onFinDone))
                state.Eff <- finalizer
                loop <- false

/// <summary>Transforms an interruption error by first collecting pending finalizers from nested <c>OnFinalize</c> wrappers, then unwinding the continuation stack to run each finalizer.</summary>
/// <param name="state">The mutable interpreter state carrying the continuation stack and completion flag.</param>
/// <param name="onComplete">The callback invoked when no more continuations remain and the fiber should complete with the interruption error.</param>
/// <param name="err">The interruption error to propagate after all finalizers have run.</param>
let inline processInterruptError
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onComplete: obj -> unit)
    (err: obj)
    =
    let mutable unwinding = true

    while unwinding do
        match state.Eff with
        | OnFinalize(eff, finalizer) ->
            state.ContStack.Push(ContStackFrame(FinalizerCont finalizer))
            state.Eff <- eff
        | _ -> unwinding <- false

    let mutable loop = true

    while loop do
        if state.ContStack.Count = 0 then
            onComplete err
            state.Completed <- true
            loop <- false
        else
            let stackFrame = state.ContStack.Pop()

            match stackFrame.Cont with
            | SuccessCont _
            | FailureCont _ -> ()
            | FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                let resumeInterrupt: obj -> FIO<obj, obj> = fun _ -> ResumeInterrupt err
                state.ContStack.Push(ContStackFrame(FailureCont resumeInterrupt))
                state.ContStack.Push(ContStackFrame(SuccessCont resumeInterrupt))
                state.Eff <- finalizer
                loop <- false

/// <summary>Transforms a <c>Result</c> by dispatching to <c>processSuccess</c> on <c>Ok</c> or <c>processError</c> on <c>Error</c>.</summary>
/// <param name="state">The mutable interpreter state.</param>
/// <param name="onSuccessComplete">The callback invoked when the fiber completes with a success value.</param>
/// <param name="onErrorComplete">The callback invoked when the fiber completes with an error value.</param>
/// <param name="res">The result to dispatch.</param>
let inline processResult
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (res: Result<obj, obj>)
    =
    match res with
    | Ok res -> processSuccess &state onSuccessComplete res
    | Error err -> processError &state onErrorComplete err

/// <summary>Transforms the current effect in the interpreter state by handling all cases shared across runtimes, returning <c>ValueSome</c> with a <c>RuntimeCase</c> for cases that require runtime-specific handling.</summary>
/// <param name="state">The mutable interpreter state to advance.</param>
/// <param name="onSuccessComplete">The callback invoked when the fiber completes with a success value.</param>
/// <param name="onErrorComplete">The callback invoked when the fiber completes with an error value.</param>
/// <returns><c>ValueNone</c> when the case was fully handled; <c>ValueSome</c> carrying a <c>RuntimeCase</c> when the caller must apply runtime-specific logic.</returns>
let inline handleSharedCase
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    : RuntimeCase voption =
    match state.Eff with
    | Success res ->
        processSuccess &state onSuccessComplete res
        ValueNone
    | Failure err ->
        processError &state onErrorComplete err
        ValueNone
    | InterruptFiber(cause, msg, fiberContext) ->
        fiberContext.Interrupt(cause, msg)
        processSuccess &state onSuccessComplete ()
        ValueNone
    | InterruptSelf(cause, msg) ->
        state.FiberContext.Interrupt(cause, msg)

        processInterruptError
            &state
            onErrorComplete
            (FiberInterruptedException(state.FiberContext.Id, cause, msg) :> obj)

        ValueNone
    | Action(func, onError) ->
        try
            let res = FiberAmbient.withContext state.FiberContext func
            processSuccess &state onSuccessComplete res
        with exn ->
            let err =
                try
                    onError exn
                with _ ->
                    exn :> obj

            processError &state onErrorComplete err

        ValueNone
    | ChainSuccess(eff, cont) ->
        state.Eff <- eff
        state.ContStack.Push(ContStackFrame(SuccessCont cont))
        ValueNone
    | ChainError(eff, cont) ->
        state.Eff <- eff
        state.ContStack.Push(ContStackFrame(FailureCont cont))
        ValueNone
    | OnFinalize(eff, finalizer) ->
        state.ContStack.Push(ContStackFrame(FinalizerCont finalizer))
        state.Eff <- eff
        ValueNone
    | ResumeInterrupt err ->
        state.InterruptionSuppressed <- state.InterruptionSuppressed - 1
        processInterruptError &state onErrorComplete err
        ValueNone
    | FinalizerResult r ->
        state.InterruptionSuppressed <- state.InterruptionSuppressed - 1

        match r with
        | Ok res -> processSuccess &state onSuccessComplete res
        | Error err -> processError &state onErrorComplete err

        ValueNone
    | SendChan(msg, chan) -> ValueSome(HandleSendChan(msg, chan))
    | ReceiveChan chan -> ValueSome(HandleReceiveChan chan)
    | ForkEffect(eff, fiber, fiberContext) -> ValueSome(HandleForkEffect(eff, fiber, fiberContext))
    | ForkTask(taskFactory, onError, fiber, fiberContext) ->
        ValueSome(HandleForkTask(taskFactory, onError, fiber, fiberContext))
    | JoinFiber fiberContext -> ValueSome(HandleJoinFiber fiberContext)
    | AwaitTask(task, onError) -> ValueSome(HandleAwaitTask(task, onError))

/// <summary>Creates a cancellation-token registration that interrupts the child fiber when the parent fiber is cancelled.</summary>
/// <param name="parentContext">The parent fiber context whose cancellation triggers child interruption.</param>
/// <param name="childContext">The child fiber context to interrupt on parent cancellation.</param>
/// <returns>The cancellation registration, which is also stored on the child context for disposal.</returns>
let inline setupForkRegistration (parentContext: FiberContext) (childContext: FiberContext) =
    let registration =
        parentContext.CancellationToken.Register(fun () ->
            childContext.Interrupt(ParentInterrupted parentContext.Id, "Parent fiber was interrupted."))

    childContext.AddRegistration registration
    registration
