module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System.Threading.Tasks

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

[<Struct; NoComparison; NoEquality>]
type RuntimeCase =
    | HandleSendChan of msg: obj * chan: Channel<obj>
    | HandleReceiveChan of chan: Channel<obj>
    | HandleForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | HandleJoinFiber of fiberContext: FiberContext
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

[<Struct; NoComparison; NoEquality>]
type internal Outcome =
    | OutcomeSuccess of value: obj
    | OutcomeError of error: obj
    | OutcomeInterrupt of intrErr: obj

let inline processOutcome
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (initialOutcome: Outcome)
    =
    let mutable outcome = initialOutcome
    let mutable loop = true

    match initialOutcome with
    | OutcomeInterrupt _ ->
        let mutable unwinding = true

        while unwinding do
            match state.Eff with
            | OnFinalize(eff, finalizer) ->
                state.ContStack.Push(FinalizerCont finalizer)
                state.Eff <- eff
            | _ -> unwinding <- false
    | _ -> ()

    while loop do
        if state.ContStack.Count = 0 then
            match outcome with
            | OutcomeSuccess value -> onSuccessComplete value
            | OutcomeError error -> onErrorComplete error
            | OutcomeInterrupt error -> onErrorComplete error

            state.Completed <- true
            loop <- false
        else
            let cont = state.ContStack.Pop()

            match outcome, cont with
            | OutcomeSuccess value, SuccessCont cont ->
                try
                    state.Eff <- cont value
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | OutcomeError error, FailureCont cont ->
                try
                    state.Eff <- cont error
                with exn ->
                    state.Eff <- Failure(exn :> obj)

                loop <- false
            | OutcomeSuccess _, FailureCont _
            | OutcomeError _, SuccessCont _
            | OutcomeInterrupt _, SuccessCont _
            | OutcomeInterrupt _, FailureCont _ -> ()
            | OutcomeSuccess value, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerSuccess value))
                state.Eff <- finalizer
                loop <- false
            | OutcomeError error, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerError error))
                state.Eff <- finalizer
                loop <- false
            | OutcomeInterrupt error, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerInterrupt error))
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

let inline processResult
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (value: Result<obj, obj>)
    =
    match value with
    | Ok value -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
    | Error error -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeError error)

let inline handleSharedCase
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    : RuntimeCase voption =
    match state.Eff with
    | Success value ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
        ValueNone
    | Failure error ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeError error)
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
            let value = func ()
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
        with exn ->
            let error =
                try
                    onError exn
                with _ ->
                    exn :> obj

            processOutcome &state onSuccessComplete onErrorComplete (OutcomeError error)

        ValueNone
    | ChainSuccess(eff, cont) ->
        state.Eff <- eff
        state.ContStack.Push(SuccessCont cont)
        ValueNone
    | ChainError(eff, cont) ->
        state.Eff <- eff
        state.ContStack.Push(FailureCont cont)
        ValueNone
    | ChainBoth(eff, successCont, errorCont) ->
        state.Eff <- eff
        state.ContStack.Push(FailureCont errorCont)
        state.ContStack.Push(SuccessCont successCont)
        ValueNone
    | OnFinalize(eff, finalizer) ->
        state.ContStack.Push(FinalizerCont finalizer)
        state.Eff <- eff
        ValueNone
    | Suspend thunk ->
        state.Eff <- thunk ()
        ValueNone
    | WriteChan(msg, chan) -> ValueSome(HandleSendChan(msg, chan))
    | ReadChan chan -> ValueSome(HandleReceiveChan chan)
    | ForkEffect(eff, fiber, fiberContext) -> ValueSome(HandleForkEffect(eff, fiber, fiberContext))
    | JoinFiber fiberContext -> ValueSome(HandleJoinFiber fiberContext)
    | AwaitTask(task, onError) -> ValueSome(HandleAwaitTask(task, onError))

let inline setupForkRegistration (parentContext: FiberContext) (childContext: FiberContext) =
    let registration =
        parentContext.CancellationToken.Register(fun () ->
            childContext.Interrupt(ParentInterrupted parentContext.Id, "Parent fiber was interrupted."))

    childContext.AddRegistration registration
    registration
