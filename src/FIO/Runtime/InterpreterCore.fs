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
    /// <summary>Represents a join that waits for a fiber to reach its terminal state.</summary>
    | HandleJoinFiber of fiberContext: FiberContext
    /// <summary>Represents an await of an in-flight .NET task.</summary>
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

/// <summary>Represents the current outcome being propagated by the interpreter as it unwinds the continuation stack.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Outcome =
    /// <summary>Represents a success outcome carrying its result value.</summary>
    | OutcomeSuccess of res: obj
    /// <summary>Represents a typed error outcome carrying its error value.</summary>
    | OutcomeError of err: obj
    /// <summary>Represents an interruption outcome carrying the interruption error to surface after finalizers run.</summary>
    | OutcomeInterrupt of intrErr: obj

/// <summary>Transforms the current outcome by unwinding the continuation stack and running any nested finalizers until the outcome is dispatched into a continuation or surfaced as the fiber's terminal state.</summary>
/// <param name="state">The mutable interpreter state carrying the continuation stack, current effect, and interruption suppression depth.</param>
/// <param name="onSuccessComplete">The callback invoked when the fiber completes with a success value.</param>
/// <param name="onErrorComplete">The callback invoked when the fiber completes with an error or interruption value.</param>
/// <param name="initialOutcome">The outcome to propagate.</param>
let inline processOutcome
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (initialOutcome: Outcome)
    =
    let mutable outcome = initialOutcome
    let mutable loop = true

    while loop do
        match outcome with
        | OutcomeInterrupt _ ->
            let mutable unwinding = true

            while unwinding do
                match state.Eff with
                | OnFinalize(eff, finalizer) ->
                    state.ContStack.Push(ContStackFrame(FinalizerCont finalizer))
                    state.Eff <- eff
                | _ -> unwinding <- false
        | _ -> ()

        if state.ContStack.Count = 0 then
            match outcome with
            | OutcomeSuccess res -> onSuccessComplete res
            | OutcomeError err -> onErrorComplete err
            | OutcomeInterrupt err -> onErrorComplete err

            state.Completed <- true
            loop <- false
        else
            let stackFrame = state.ContStack.Pop()

            match outcome, stackFrame.Cont with
            | OutcomeSuccess res, SuccessCont cont ->
                try
                    state.Eff <- cont res
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | OutcomeError err, FailureCont cont ->
                try
                    state.Eff <- cont err
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | OutcomeSuccess _, FailureCont _
            | OutcomeError _, SuccessCont _
            | OutcomeInterrupt _, SuccessCont _
            | OutcomeInterrupt _, FailureCont _ -> ()
            | OutcomeSuccess res, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(ContStackFrame(PostFinalizerCont(PostFinalizerSuccess res)))
                state.Eff <- finalizer
                loop <- false
            | OutcomeError err, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(ContStackFrame(PostFinalizerCont(PostFinalizerError err)))
                state.Eff <- finalizer
                loop <- false
            | OutcomeInterrupt err, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(ContStackFrame(PostFinalizerCont(PostFinalizerInterrupt err)))
                state.Eff <- finalizer
                loop <- false
            | OutcomeInterrupt _, PostFinalizerCont _ -> ()
            | OutcomeSuccess _, PostFinalizerCont saved ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed - 1

                outcome <-
                    match saved with
                    | PostFinalizerSuccess savedRes -> OutcomeSuccess savedRes
                    | PostFinalizerError savedErr -> OutcomeError savedErr
                    | PostFinalizerInterrupt savedErr -> OutcomeInterrupt savedErr
            | OutcomeError _, PostFinalizerCont saved ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed - 1

                match saved with
                | PostFinalizerSuccess _ -> ()
                | PostFinalizerError savedErr -> outcome <- OutcomeError savedErr
                | PostFinalizerInterrupt savedErr -> outcome <- OutcomeInterrupt savedErr

/// <summary>Transforms a <c>Result</c> by dispatching to <c>processOutcome</c> with the matching outcome.</summary>
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
    | Ok res -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess res)
    | Error err -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeError err)

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
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess res)
        ValueNone
    | Failure err ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeError err)
        ValueNone
    | Interrupt(cause, msg) ->
        state.FiberContext.Interrupt(cause, msg)

        processOutcome
            &state
            onSuccessComplete
            onErrorComplete
            (OutcomeInterrupt(FiberInterruptedException(state.FiberContext.Id, cause, msg) :> obj))

        ValueNone
    | FiberCancellationToken ->
        processOutcome
            &state
            onSuccessComplete
            onErrorComplete
            (OutcomeSuccess(state.FiberContext.CancellationToken :> obj))

        ValueNone
    | Action(func, onError) ->
        try
            let res = func ()
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess res)
        with exn ->
            let err =
                try
                    onError exn
                with _ ->
                    exn :> obj

            processOutcome &state onSuccessComplete onErrorComplete (OutcomeError err)

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
    | SendChan(msg, chan) -> ValueSome(HandleSendChan(msg, chan))
    | ReceiveChan chan -> ValueSome(HandleReceiveChan chan)
    | ForkEffect(eff, fiber, fiberContext) -> ValueSome(HandleForkEffect(eff, fiber, fiberContext))
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
