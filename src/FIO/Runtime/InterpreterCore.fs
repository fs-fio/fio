module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System
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
    | HandleWriteChan of message: obj * channel: Channel<obj> * reportAccepted: bool
    | HandleReadChan of channel: Channel<obj>
    | HandleForkEffect of effect: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext * daemon: bool
    | HandleJoinFiber of fiberContext: FiberContext
    | HandleJoinFirst of fiberContexts: FiberContext list
    | HandleJoinAllFailFast of allFiberContexts: FiberContext[]
    | HandleAwaitTask of task: Task<obj> * onError: (exn -> obj)

[<Struct; NoComparison; NoEquality>]
type internal Outcome =
    | OutcomeSucceeded of value: obj
    | OutcomeFailed of error: obj
    | OutcomeInterrupted of interruptError: obj

let interruptionFor (fiberContext: FiberContext) (fallbackMessage: string) : obj =
    let task = fiberContext.Task
    if task.IsCompletedSuccessfully then
        match task.Result with
        | Error error -> error
        | Ok _ -> FiberInterruptedException(fiberContext.Id, ExplicitInterrupt, fallbackMessage) :> obj
    else
        FiberInterruptedException(fiberContext.Id, ExplicitInterrupt, fallbackMessage) :> obj

// A region that ends while its fiber is interrupted ends the fiber there, as in ZIO, rather than running the
// next continuation first. Rarely taken, so kept out of the inlined interpreter loop.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let interruptedOnRegionExit (fiberContext: FiberContext) (outcome: Outcome) =
    match outcome with
    | OutcomeInterrupted _ -> outcome
    | OutcomeSucceeded _
    | OutcomeFailed _ -> OutcomeInterrupted(interruptionFor fiberContext "Fiber was interrupted in an uninterruptible region.")

// Pushes the finalizers of the effects an interrupted fiber is abandoning.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let unwindFinalizers (state: byref<InterpreterState>) =
    let mutable unwinding = true
    while unwinding do
        match state.Effect with
        | OnFinalize(effect, finalizer) ->
            state.ContStack.Push(FinalizerCont finalizer)
            state.Effect <- effect
        | _ -> unwinding <- false

// Finalizer and suppression frames, which are rarer than ChainCont. processOutcome is inlined at every call site
// of every runtime's loop, and with these arms inline the Polling and Signaling loops crossed the JIT's basic-block
// limit and were compiled without optimization. Returns true when the fiber has a new effect to run.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let processRegionCont (state: byref<InterpreterState>) (cont: Cont) (outcome: byref<Outcome>) =
    match cont with
    | RestoreSuppressionCont level ->
        state.InterruptionSuppressed <- level

        if level = 0 && state.FiberContext.CancellationToken.IsCancellationRequested then
            outcome <- interruptedOnRegionExit state.FiberContext outcome

        false
    | FinalizerCont finalizer ->
        let level = state.InterruptionSuppressed
        state.InterruptionSuppressed <- level + 1

        let saved =
            match outcome with
            | OutcomeSucceeded value -> PostFinalizerSucceeded value
            | OutcomeFailed error -> PostFinalizerFailed error
            | OutcomeInterrupted error -> PostFinalizerInterrupted error

        state.ContStack.Push(PostFinalizerCont(saved, level))
        state.Effect <- finalizer
        true
    | PostFinalizerCont(saved, level) ->
        state.InterruptionSuppressed <- level

        match outcome with
        | OutcomeSucceeded _ ->
            outcome <-
                match saved with
                | PostFinalizerSucceeded savedRes -> OutcomeSucceeded savedRes
                | PostFinalizerFailed savedErr -> OutcomeFailed savedErr
                | PostFinalizerInterrupted savedErr -> OutcomeInterrupted savedErr
        | OutcomeFailed _ ->
            match saved with
            | PostFinalizerSucceeded _ -> ()
            | PostFinalizerFailed savedErr -> outcome <- OutcomeFailed savedErr
            | PostFinalizerInterrupted savedErr -> outcome <- OutcomeInterrupted savedErr
        | OutcomeInterrupted _ ->
            match saved with
            | PostFinalizerSucceeded _
            | PostFinalizerFailed _ -> ()
            | PostFinalizerInterrupted savedErr -> outcome <- OutcomeInterrupted savedErr

        if level = 0 && state.FiberContext.CancellationToken.IsCancellationRequested then
            outcome <- interruptedOnRegionExit state.FiberContext outcome

        false
    | AcquiredCont(onAcquired, level) ->
        state.InterruptionSuppressed <- level

        match outcome with
        | OutcomeSucceeded resource ->
            // Release is registered in the same step that makes the fiber interruptible again, so an interruption
            // deferred during acquire, or one taking effect now, still finds it on the stack.
            try
                state.Effect <- onAcquired resource
                unwindFinalizers &state
            with ex ->
                state.Effect <- Interrupt(Defect ex, ex.Message)

            if level = 0 && state.FiberContext.CancellationToken.IsCancellationRequested then
                outcome <- interruptedOnRegionExit state.FiberContext outcome
                false
            else
                true
        | OutcomeFailed _
        | OutcomeInterrupted _ ->
            if level = 0 && state.FiberContext.CancellationToken.IsCancellationRequested then
                outcome <- interruptedOnRegionExit state.FiberContext outcome

            false
    | ChainCont _ -> false

let inline processOutcome
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (initialOutcome: Outcome) =
    let mutable outcome = initialOutcome
    let mutable loop = true

    match initialOutcome with
    | OutcomeInterrupted _ -> unwindFinalizers &state
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

            // Nested matches, not a tuple: a reference tuple allocated one per continuation popped, and a
            // struct tuple slowed the park-heavy benchmarks.
            match cont with
            | ChainCont(onSuccess, onFailure) ->
                match outcome with
                | OutcomeSucceeded value when not (obj.ReferenceEquals(onSuccess, null)) ->
                    try
                        state.Effect <- onSuccess value
                    with ex ->
                        state.Effect <- Interrupt(Defect ex, ex.Message)

                    loop <- false
                | OutcomeFailed error when not (obj.ReferenceEquals(onFailure, null)) ->
                    try
                        state.Effect <- onFailure error
                    with ex ->
                        state.Effect <- Interrupt(Defect ex, ex.Message)

                    loop <- false
                | _ -> ()
            | _ ->
                if processRegionCont &state cont &outcome then
                    loop <- false

let inline processResult
    (state: byref<InterpreterState>)
    ([<InlineIfLambda>] onSuccessComplete: obj -> unit)
    ([<InlineIfLambda>] onErrorComplete: obj -> unit)
    (value: Result<obj, obj>) =
    match value with
    | Ok value ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
    | Error error ->
        match error with
        | :? FiberInterruptedException ->
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeInterrupted error)
        | _ ->
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
        let token =
            if state.InterruptionSuppressed > 0 then
                CancellationToken.None
            else
                state.FiberContext.CancellationToken

        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded(token :> obj))
        ValueNone
    | Action(func, onError) ->
        let mutable thrown: exn = null
        let mutable value = Unchecked.defaultof<obj>

        try
            value <- func ()
        with ex ->
            thrown <- ex

        if isNull thrown then
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
        else
            let mutable mapped = false
            let mutable error = Unchecked.defaultof<obj>

            try
                error <- onError thrown
                mapped <- true
            with _ ->
                ()

            if mapped then
                processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed error)
            else
                state.Effect <- Interrupt(Defect thrown, thrown.Message)
        ValueNone
    | ChainSuccess(effect, cont) ->
        state.Effect <- effect
        state.ContStack.Push(ChainCont(cont, Unchecked.defaultof<_>))
        ValueNone
    | ChainError(effect, cont) ->
        state.Effect <- effect
        state.ContStack.Push(ChainCont(Unchecked.defaultof<_>, cont))
        ValueNone
    | ChainBoth(effect, successCont, errorCont) ->
        // One frame for both handlers, so a failure of the success handler is not caught by the
        // error handler (ZIO's foldZIO semantics).
        state.Effect <- effect
        state.ContStack.Push(ChainCont(successCont, errorCont))
        ValueNone
    | OnFinalize(effect, finalizer) ->
        state.ContStack.Push(FinalizerCont finalizer)
        state.Effect <- effect
        ValueNone
    | Suspend effect ->
        try
           state.Effect <- effect ()
        with ex ->
           state.Effect <- Interrupt(Defect ex, ex.Message)
        ValueNone
    | WithSuppression(update, body) ->
        let outer = state.InterruptionSuppressed
        state.ContStack.Push(RestoreSuppressionCont outer)
        try
            state.InterruptionSuppressed <- update outer
            state.Effect <- body outer
        with ex ->
            state.Effect <- Interrupt(Defect ex, ex.Message)
        ValueNone
    | AcquireRelease(acquire, onAcquired) ->
        let outer = state.InterruptionSuppressed
        state.ContStack.Push(AcquiredCont(onAcquired, outer))
        state.InterruptionSuppressed <- outer + 1
        state.Effect <- acquire
        ValueNone
    | WriteChan(message, channel, reportAccepted) ->
        ValueSome(HandleWriteChan(message, channel, reportAccepted))
    | ReadChan channel ->
        ValueSome(HandleReadChan channel)
    | ForkEffect(effect, fiber, fiberContext, daemon) ->
        ValueSome(HandleForkEffect(effect, fiber, fiberContext, daemon))
    | JoinFiber fiberContext ->
        ValueSome(HandleJoinFiber fiberContext)
    | JoinFirst fiberContexts ->
        ValueSome(HandleJoinFirst fiberContexts)
    | JoinAllFailFast fiberContexts ->
        ValueSome(HandleJoinAllFailFast fiberContexts)
    | AwaitTask(task, onError) ->
        ValueSome(HandleAwaitTask(task, onError))

let inline attachFork (parentContext: FiberContext) (childContext: FiberContext) (daemon: bool) (uninterruptible: bool) =
    if not daemon then
        let scope =
            if uninterruptible then parentContext.ProtectedChildScopeToken else parentContext.ChildScopeToken

        // Interrupting a child interrupts its own children from inside this callback, one level of stack each.
        let registration =
            scope.Register(fun () ->
                if RuntimeHelpers.TryEnsureSufficientExecutionStack() then
                    childContext.Interrupt(ParentInterrupted parentContext.Id, "Parent fiber scope closed.")
                else
                    ThreadPool.UnsafeQueueUserWorkItem(
                        WaitCallback(fun _ ->
                            try
                                childContext.Interrupt(ParentInterrupted parentContext.Id, "Parent fiber scope closed.")
                            with _ ->
                                ()),
                        null)
                    |> ignore)
        childContext.AddRegistration registration
        childContext.AttachTo parentContext

let inline defectError (fiberContext: FiberContext) (ex: exn) : obj =
    FiberInterruptedException(fiberContext.Id, Defect ex, ex.Message) :> obj

let inline awaitTaskFailureOutcome (fiberContext: FiberContext) (onError: exn -> obj) (ex: exn) =
    try
        OutcomeFailed(onError ex)
    with _ ->
        OutcomeInterrupted(defectError fiberContext ex)

let inline resumeWith (state: byref<InterpreterState>) (workItem: WorkItem) =
    workItem.InterruptionSuppressed <- state.InterruptionSuppressed
    workItem

let inline awaitedTask (awaited: Task<'T>) (suppressed: int) (fiberContext: FiberContext) : Task<'T> =
    if suppressed > 0 then
        awaited
    else
        awaited.WaitAsync fiberContext.CancellationToken

let settledTaskEffect (waited: Task<obj>) (fiberContext: FiberContext) (onError: exn -> obj) : FIO<obj, obj> =
    if waited.IsCompletedSuccessfully then
        Success waited.Result
    elif waited.IsCanceled && fiberContext.CancellationToken.IsCancellationRequested then
        match interruptionFor fiberContext "Task has been cancelled." with
        | :? FiberInterruptedException as interruption -> Interrupt(interruption.cause, interruption.message)
        | _ -> Interrupt(ExplicitInterrupt, "Task has been cancelled.")
    else
        let ex =
            match waited.Exception with
            | null -> OperationCanceledException() :> exn
            | aggregate ->
                match aggregate.InnerException with
                | null -> aggregate :> exn
                | inner -> inner

        try
            Failure(onError ex)
        with _ ->
            Interrupt(Defect ex, ex.Message)

[<Struct; NoComparison; NoEquality>]
type WriteAttempt =
    | Written of result: obj
    | MustWait

// A dropping channel also uses the Wait full mode, so a failed TryWrite means "wait" only for a
// Write to a bounded channel; a dropping channel dropped the message, and a TryWrite reports it.
let inline tryWriteChannel (channel: Channel<obj>) (message: obj) (reportAccepted: bool) =
    let accepted = channel.Queue.TryWrite message

    if not accepted && not reportAccepted && channel.Mode = Bounded then
        MustWait
    else
        Written(if reportAccepted then box accepted else message)

// WaitToWriteAsync only says there may be room, and another writer can take it first, so the write
// is rerun rather than completed on wake.
let parkUntilWritable
    (channel: Channel<obj>)
    (writeEffect: FIO<obj, obj>)
    (fiberContext: FiberContext)
    (contStack: Stack<Cont>)
    (suppressed: int)
    (reschedule: WorkItem -> unit) =
    let cancellationToken =
        if suppressed > 0 then CancellationToken.None else fiberContext.CancellationToken

    let waited = channel.Queue.WaitToWriteAsync(cancellationToken).AsTask()

    let resume () =
        let effect =
            if suppressed = 0 && fiberContext.CancellationToken.IsCancellationRequested then
                match interruptionFor fiberContext "Fiber was interrupted while blocked on a channel write." with
                | :? FiberInterruptedException as interruption -> Interrupt(interruption.cause, interruption.message)
                | _ -> Interrupt(ExplicitInterrupt, "Fiber was interrupted while blocked on a channel write.")
            else
                writeEffect

        try
            reschedule
                {
                    Effect = effect
                    FiberContext = fiberContext
                    ContStack = contStack
                    InterruptionSuppressed = suppressed
                }
        with _ ->
            ()

    waited.GetAwaiter().OnCompleted(Action resume)

let inline parkOnTask
    (waited: Task<obj>)
    (fiberContext: FiberContext)
    (contStack: Stack<Cont>)
    (suppressed: int)
    (onError: exn -> obj)
    ([<InlineIfLambda>] reschedule: WorkItem -> unit) =
    let resume () =
        let resumeWorkItem =
            {
                Effect = settledTaskEffect waited fiberContext onError
                FiberContext = fiberContext
                ContStack = contStack
                InterruptionSuppressed = suppressed
            }

        try
            reschedule resumeWorkItem
        with _ ->
            ()

    waited.GetAwaiter().OnCompleted(Action resume)

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
