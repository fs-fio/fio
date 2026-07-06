module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Struct; NoComparison; NoEquality>]
type InterpreterState =
    val mutable Effect: FIO<obj, obj>
    val mutable ContStack: Stack<Cont>
    val mutable FiberContext: FiberContext
    val mutable Completed: bool
    val mutable InterruptionSuppressed: int

    new(effect, contStack, fiberContext, interruptionSuppressed) =
        {
            Effect = effect
            ContStack = contStack
            FiberContext = fiberContext
            Completed = false
            InterruptionSuppressed = interruptionSuppressed
        }

[<Struct; NoComparison; NoEquality>]
type RuntimeCase =
    | HandleWriteChan of message: obj * channel: Channel<obj>
    | HandleReadChan of channel: Channel<obj>
    | HandleForkEffect of effect: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | HandleJoinFiber of fiberContext: FiberContext
    | HandleJoinFirst of fiberContexts: FiberContext list
    | HandleJoinAllFailFast of allFiberContexts: FiberContext[]
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

[<Struct; NoComparison; NoEquality>]
type internal Outcome =
    | OutcomeSucceeded of value: obj
    | OutcomeFailed of error: obj
    | OutcomeInterrupted of interruptError: obj

let inline processOutcome
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (initialOutcome: Outcome) =
    let mutable outcome = initialOutcome
    let mutable loop = true

    match initialOutcome with
    | OutcomeInterrupted _ ->
        let mutable unwinding = true
        while unwinding do
            match state.Effect with
            | OnFinalize(effect, finalizer) ->
                state.ContStack.Push(FinalizerCont finalizer)
                state.Effect <- effect
            | _ -> unwinding <- false
    | _ -> ()

    while loop do
        if state.ContStack.Count = 0 then
            match outcome with
            | OutcomeSucceeded value -> onSuccessComplete value
            | OutcomeFailed error -> onErrorComplete error
            | OutcomeInterrupted error -> onErrorComplete error

            state.Completed <- true
            loop <- false
        else
            let cont = state.ContStack.Pop()

            match outcome, cont with
            | OutcomeSucceeded value, SuccessCont cont ->
                try
                    state.Effect <- cont value
                with ex ->
                    state.Effect <- Failure(ex :> obj)

                loop <- false
            | OutcomeFailed error, FailureCont cont ->
                try
                    state.Effect <- cont error
                with ex ->
                    state.Effect <- Failure(ex :> obj)

                loop <- false
            | OutcomeSucceeded _, FailureCont _
            | OutcomeFailed _, SuccessCont _
            | OutcomeInterrupted _, SuccessCont _
            | OutcomeInterrupted _, FailureCont _ -> ()
            | OutcomeSucceeded value, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerSucceeded value))
                state.Effect <- finalizer
                loop <- false
            | OutcomeFailed error, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerFailed error))
                state.Effect <- finalizer
                loop <- false
            | OutcomeInterrupted error, FinalizerCont finalizer ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed + 1
                state.ContStack.Push(PostFinalizerCont(PostFinalizerInterrupted error))
                state.Effect <- finalizer
                loop <- false
            | OutcomeInterrupted _, PostFinalizerCont _ -> ()
            | OutcomeSucceeded _, PostFinalizerCont saved ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed - 1
                outcome <-
                    match saved with
                    | PostFinalizerSucceeded savedRes -> OutcomeSucceeded savedRes
                    | PostFinalizerFailed savedErr -> OutcomeFailed savedErr
                    | PostFinalizerInterrupted savedErr -> OutcomeInterrupted savedErr
            | OutcomeFailed _, PostFinalizerCont saved ->
                state.InterruptionSuppressed <- state.InterruptionSuppressed - 1
                match saved with
                | PostFinalizerSucceeded _ -> ()
                | PostFinalizerFailed savedErr -> outcome <- OutcomeFailed savedErr
                | PostFinalizerInterrupted savedErr -> outcome <- OutcomeInterrupted savedErr

let inline processResult
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (value: Result<obj, obj>) =
    match value with
    | Ok value ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
    | Error error ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed error)

let inline handleSharedCase
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    : RuntimeCase voption =
    match state.Effect with
    | Success value ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
        ValueNone
    | Failure error ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed error)
        ValueNone
    | Interrupt(cause, message) ->
        state.FiberContext.Interrupt(cause, message)
        processOutcome
            &state
            onSuccessComplete
            onErrorComplete
            (OutcomeInterrupted(FiberInterruptedException(state.FiberContext.Id, cause, message) :> obj))
        ValueNone
    | FiberCancellationToken ->
        processOutcome
            &state
            onSuccessComplete
            onErrorComplete
            (OutcomeSucceeded(state.FiberContext.CancellationToken :> obj))
        ValueNone
    | Action(func, onError) ->
        try
            let value = func ()
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
        with ex ->
            let error =
                try
                    onError ex
                with _ ->
                    ex :> obj
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed error)
        ValueNone
    | ChainSuccess(effect, cont) ->
        state.Effect <- effect
        state.ContStack.Push(SuccessCont cont)
        ValueNone
    | ChainError(effect, cont) ->
        state.Effect <- effect
        state.ContStack.Push(FailureCont cont)
        ValueNone
    | ChainBoth(effect, successCont, errorCont) ->
        state.Effect <- effect
        state.ContStack.Push(FailureCont errorCont)
        state.ContStack.Push(SuccessCont successCont)
        ValueNone
    | OnFinalize(effect, finalizer) ->
        state.ContStack.Push(FinalizerCont finalizer)
        state.Effect <- effect
        ValueNone
    | Suspend effect ->
        state.Effect <- effect ()
        ValueNone
    | WriteChan(value, channel) ->
        ValueSome(HandleWriteChan(value, channel))
    | ReadChan channel ->
        ValueSome(HandleReadChan channel)
    | ForkEffect(effect, fiber, fiberContext) ->
        ValueSome(HandleForkEffect(effect, fiber, fiberContext))
    | JoinFiber fiberContext ->
        ValueSome(HandleJoinFiber fiberContext)
    | JoinFirst fiberContexts ->
        ValueSome(HandleJoinFirst fiberContexts)
    | JoinAllFailFast fiberContexts ->
        ValueSome(HandleJoinAllFailFast fiberContexts)
    | AwaitTask(task, onError) ->
        ValueSome(HandleAwaitTask(task, onError))

let inline setupForkRegistration (parentContext: FiberContext) (childContext: FiberContext) =
    let registration =
        parentContext.CancellationToken.Register(fun () ->
            childContext.Interrupt(ParentInterrupted parentContext.Id, "Parent fiber was interrupted."))
    childContext.AddRegistration registration
    registration

let parkBlockingWaiter
    (fiberContext: FiberContext)
    (suppressed: int)
    (workItem: WorkItem)
    (reschedule: WorkItem -> unit) =
    let waiter = BlockingWaiter workItem
    if suppressed = 0 then
        let registration =
            fiberContext.CancellationToken.Register(fun () ->
                try
                    if waiter.TryClaim() then
                        reschedule waiter.WorkItem
                with _ ->
                    ())
        waiter.SetRegistration registration
    waiter

[<MethodImpl(MethodImplOptions.NoInlining)>]
let tryCompleteJoinAll (fiberContexts: FiberContext[]) =
    let mutable firstFailure = -1
    let mutable allSettled = true

    for i in 0 .. fiberContexts.Length - 1 do
        let fiberContext = fiberContexts[i]

        if fiberContext.IsTerminal() && fiberContext.Task.IsCompleted then
            if firstFailure = -1 then
                match fiberContext.Task.Result with
                | Error _ -> firstFailure <- i
                | Ok _ -> ()
        else
            allSettled <- false

    if firstFailure >= 0 then ValueSome(ValueSome firstFailure)
    elif allSettled then ValueSome ValueNone
    else ValueNone

let registerJoinAllLatch (fiberContexts: FiberContext[]) (signal: unit -> unit) =
    let latch = JoinAllLatch(fiberContexts.Length, signal)

    for fiberContext in fiberContexts do
        fiberContext.SetOnTerminal <| fun () -> latch.OnChildTerminal fiberContext

[<MethodImpl(MethodImplOptions.NoInlining)>]
let tryFindTerminalIndex (fiberContexts: FiberContext list) =
    let mutable index = 0
    let mutable result = -1
    let mutable remaining = fiberContexts

    while result < 0 && not remaining.IsEmpty do
        if remaining.Head.IsTerminal() then
            result <- index
        else
            index <- index + 1
            remaining <- remaining.Tail

    result

[<MethodImpl(MethodImplOptions.NoInlining)>]
let parkJoinFirstOnQueue
    (fiberContexts: FiberContext list)
    (currentFiberContext: FiberContext)
    (suppressed: int)
    (workItem: WorkItem)
    (activeWorkItemQueue: MailboxQueue<WorkItem>) =
    let waiter =
        parkBlockingWaiter currentFiberContext suppressed workItem <| fun wi ->
            activeWorkItemQueue.WriteAsync wi |> ignore

    for fiberContext in fiberContexts do
        let addVt = fiberContext.AddBlockingWorkItem waiter

        if not addVt.IsCompletedSuccessfully then
            addVt.AsTask() |> ignore

    for fiberContext in fiberContexts do
        let rescheduleVt = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemQueue

        if not rescheduleVt.IsCompletedSuccessfully then
            rescheduleVt.AsTask() |> ignore

[<MethodImpl(MethodImplOptions.NoInlining)>]
let parkJoinFirstOnHooks
    (fiberContexts: FiberContext list)
    (currentFiberContext: FiberContext)
    (suppressed: int)
    (workItem: WorkItem)
    (activeWorkItemQueue: MailboxQueue<WorkItem>) =
    let waiter =
        parkBlockingWaiter currentFiberContext suppressed workItem (fun wi ->
            activeWorkItemQueue.WriteAsync wi |> ignore)

    for fiberContext in fiberContexts do
        fiberContext.SetOnTerminal <| fun () ->
            if waiter.TryClaim() then
                activeWorkItemQueue.WriteAsync waiter.WorkItem |> ignore

        if fiberContext.IsTerminal() && waiter.TryClaim() then
            activeWorkItemQueue.WriteAsync waiter.WorkItem |> ignore

[<MethodImpl(MethodImplOptions.NoInlining)>]
let parkJoinAllFailFastOnQueue
    (fiberContexts: FiberContext[])
    (currentFiberContext: FiberContext)
    (suppressed: int)
    (workItem: WorkItem)
    (activeWorkItemQueue: MailboxQueue<WorkItem>) =
    let waiter =
        parkBlockingWaiter currentFiberContext suppressed workItem (fun wi ->
            activeWorkItemQueue.WriteAsync wi |> ignore)

    registerJoinAllLatch fiberContexts <| fun () ->
        if waiter.TryClaim() then
            activeWorkItemQueue.WriteAsync waiter.WorkItem |> ignore

[<MethodImpl(MethodImplOptions.NoInlining)>]
let awaitJoinAllSettled
    (fiberContexts: FiberContext[])
    (suppressed: bool)
    (currentFiberContext: FiberContext)
    (cancellationToken: CancellationToken) =
    task {
        let completionSource =
            TaskCompletionSource<unit> TaskCreationOptions.RunContinuationsAsynchronously

        registerJoinAllLatch fiberContexts (fun () -> completionSource.TrySetResult() |> ignore)

        let! _ =
            if suppressed then completionSource.Task
            else completionSource.Task.WaitAsync cancellationToken

        match tryCompleteJoinAll fiberContexts with
        | ValueSome outcome ->
            return OutcomeSucceeded <| box outcome
        | ValueNone ->
            return
                OutcomeInterrupted(
                    FiberInterruptedException(
                        currentFiberContext.Id,
                        ExplicitInterrupt,
                        "JoinAllFailFast signalled without a settled outcome."))
    }
