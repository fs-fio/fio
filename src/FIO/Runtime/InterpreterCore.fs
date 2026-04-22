/// Shared interpreter logic for FIO runtimes.
/// All functions are inline with [<InlineIfLambda>] callbacks for zero-overhead abstraction.
/// Process functions take byref<InterpreterState> — safe because they are synchronous (no yield points).
module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System.Threading.Tasks

/// Bundles the shared mutable interpreter locals.
[<Struct; NoComparison; NoEquality>]
type InterpreterState =
    val mutable Eff: FIO<obj, obj>
    val mutable ContStack: ContStack
    val mutable FiberContext: FiberContext
    val mutable Completed: bool
    val mutable InterruptionSuppressed: int

    new(eff, contStack, fiberContext, interruptionSuppressed) =
        {
            Eff = eff
            ContStack = contStack
            FiberContext = fiberContext
            Completed = false
            InterruptionSuppressed = interruptionSuppressed
        }

/// Identifies runtime-specific effect cases that handleSharedCase cannot process.
[<Struct; NoComparison; NoEquality>]
type RuntimeCase =
    | HandleSendChan of msg: obj * chan: Channel<obj>
    | HandleReceiveChan of chan: Channel<obj>
    | HandleForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | HandleForkTask of
        taskFactory: (unit -> Task<obj>) *
        onError: (exn -> obj) *
        fiber: obj *
        fiberContext: FiberContext
    | HandleJoinFiber of fiberContext: FiberContext
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

/// Walks the continuation stack on the success path.
/// Pops SuccessCont (invokes), skips FailureCont, runs FinalizerCont.
/// On empty stack, calls onComplete to perform runtime-specific fiber completion.
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

/// Walks the continuation stack on the error path.
/// Skips SuccessCont, pops FailureCont (invokes), runs FinalizerCont.
/// On empty stack, calls onComplete to perform runtime-specific fiber completion.
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

/// Walks the continuation stack on the interruption path.
/// Skips both SuccessCont and FailureCont, only runs FinalizerCont.
/// On empty stack, calls onComplete to perform runtime-specific fiber completion.
let inline processInterruptError
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onComplete: obj -> unit)
    (err: obj)
    =
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

/// Dispatches a Result to processSuccess or processError.
let inline processResult
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (res: Result<obj, obj>)
    =
    match res with
    | Ok res -> processSuccess &state onSuccessComplete res
    | Error err -> processError &state onErrorComplete err

/// Handles the 10 shared DU cases that are identical across all runtimes.
/// Returns ValueNone if the case was handled, or ValueSome(RuntimeCase) for runtime-specific dispatch.
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
            let res = func ()
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
