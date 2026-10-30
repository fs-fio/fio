module internal FIO.Runtime.InterpreterCore

open FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Struct; NoComparison; NoEquality>]
type InterpreterState =
    val mutable Effect: FIOBase
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
    | HandleForkEffect of effect: FIOBase * fiber: obj * fiberContext: FiberContext * daemon: bool
    | HandleJoinFiber of fiberContext: FiberContext
    | HandleJoinFirst of fiberContexts: FiberContext list
    | HandleJoinAllFailFast of allFiberContexts: FiberContext[]
    | HandleAwaitTask of task: Task<obj> * node: FIOBase

[<Struct; NoComparison; NoEquality>]
type internal Outcome =
    | OutcomeSucceeded of value: obj
    | OutcomeFailed of error: obj
    | OutcomeInterrupted of interruptError: obj

let interruptionFor (fiberContext: FiberContext) (fallbackMessage: string) =
    let task = fiberContext.Task
    if task.IsCompletedSuccessfully then
        match task.Result with
        | Error error -> error
        | Ok _ -> FiberInterruptedException(fiberContext.Id, ExplicitInterrupt, fallbackMessage) :> obj
    else
        FiberInterruptedException(fiberContext.Id, ExplicitInterrupt, fallbackMessage) :> obj

let interruptedEffect (fiberContext: FiberContext) (fallbackMessage: string) : FIOBase =
    match interruptionFor fiberContext fallbackMessage with
    | :? FiberInterruptedException as interruption -> InterruptNode<obj, obj>(interruption.cause, interruption.message)
    | _ -> InterruptNode<obj, obj>(ExplicitInterrupt, fallbackMessage)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let defectEffect (ex: exn) : FIOBase =
    InterruptNode<obj, obj>(Defect ex, ex.Message)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let interruptedOnRegionExit (fiberContext: FiberContext) (outcome: Outcome) =
    match outcome with
    | OutcomeInterrupted _ -> outcome
    | OutcomeSucceeded _
    | OutcomeFailed _ -> OutcomeInterrupted(interruptionFor fiberContext "Fiber was interrupted in an uninterruptible region.")

let inline isFinalizerNode (node: FIOBase) =
    match node.Kind with
    | NodeKind.OnFinalize
    | NodeKind.OnExit -> true
    | _ -> false

[<MethodImpl(MethodImplOptions.NoInlining)>]
let unwindFinalizers (state: byref<InterpreterState>) =
    let mutable unwinding = true
    while unwinding do
        let node = state.Effect
        if isFinalizerNode node then
            state.ContStack.Push(FinalizerCont node)
            state.Effect <- node.Child
        else
            unwinding <- false

// Out of line: processOutcome is inlined into every runtime's loop, and with these rarer arms inline a loop can
// cross the JIT's basic-block limit and run unoptimized. Returns true when an effect is set.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let runExitFinalizer (state: byref<InterpreterState>) (node: FIOBase) (saved: PostFinalizerSaved) level =
    state.ContStack.Push(PostFinalizerCont(saved, level))

    state.Effect <-
        try
            node.Ops.RunExit(node, saved)
        with ex ->
            InterruptNode<obj, obj>(Defect ex, ex.Message)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let processRegionCont (state: byref<InterpreterState>) (cont: Cont) (outcome: byref<Outcome>) =
    match cont with
    | RestoreSuppressionCont level ->
        state.InterruptionSuppressed <- level

        if level = 0 && state.FiberContext.CancellationToken.IsCancellationRequested then
            outcome <- interruptedOnRegionExit state.FiberContext outcome

        false
    | FinalizerCont node ->
        let level = state.InterruptionSuppressed
        state.InterruptionSuppressed <- level + 1

        let saved =
            match outcome with
            | OutcomeSucceeded value -> PostFinalizerSucceeded value
            | OutcomeFailed error -> PostFinalizerFailed error
            | OutcomeInterrupted error -> PostFinalizerInterrupted error

        match node.Kind with
        | NodeKind.OnExit ->
            runExitFinalizer &state node saved level
        | _ ->
            state.ContStack.Push(PostFinalizerCont(saved, level))
            state.Effect <- cast<FIOBase> node.B

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
    | AcquiredCont(node, level) ->
        state.InterruptionSuppressed <- level

        match outcome with
        | OutcomeSucceeded resource ->
            try
                state.Effect <- node.Ops.OnAcquired(node, resource)
                unwindFinalizers &state
            with ex ->
                state.Effect <- InterruptNode<obj, obj>(Defect ex, ex.Message)

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
    | MappedCont node ->
        match outcome with
        | OutcomeSucceeded value ->
            try
                outcome <- OutcomeSucceeded(node.Ops.Apply(node, value))
                false
            with ex ->
                state.Effect <- InterruptNode<obj, obj>(Defect ex, ex.Message)
                true
        | OutcomeFailed _
        | OutcomeInterrupted _ -> false
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

            match cont with
            | ChainCont(onSuccess, onFailure) ->
                match outcome with
                | OutcomeSucceeded value when not (obj.ReferenceEquals(onSuccess, null)) ->
                    try
                        state.Effect <- onSuccess.Ops.Continue(onSuccess, value)
                    with ex ->
                        state.Effect <- defectEffect ex

                    loop <- false
                | OutcomeFailed error when not (obj.ReferenceEquals(onFailure, null)) ->
                    try
                        state.Effect <- onFailure.Ops.Fail(onFailure, error)
                    with ex ->
                        state.Effect <- defectEffect ex

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
    ([<InlineIfLambda>] onErrorComplete: obj -> unit) =
    let node = state.Effect

    match node.Kind with
    | NodeKind.Success ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded node.A)
        ValueNone
    | NodeKind.Failure ->
        processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed node.A)
        ValueNone
    | NodeKind.Interrupt ->
        let cause = cast<InterruptionCause> node.A
        let message = cast<string> node.B
        state.FiberContext.Interrupt(cause, message)
        processOutcome
            &state
            onSuccessComplete
            onErrorComplete
            (OutcomeInterrupted(FiberInterruptedException(state.FiberContext.Id, cause, message) :> obj))
        ValueNone
    | NodeKind.FiberCancellationToken ->
        let token =
            if state.InterruptionSuppressed > 0 then
                CancellationToken.None
            else
                state.FiberContext.CancellationToken

        processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded(token :> obj))
        ValueNone
    | NodeKind.Action ->
        let mutable thrown = null
        let mutable value = Unchecked.defaultof<obj>

        try
            value <- node.Ops.Invoke node
        with ex ->
            thrown <- ex

        if isNull thrown then
            processOutcome &state onSuccessComplete onErrorComplete (OutcomeSucceeded value)
        else
            let mutable mapped = false
            let mutable error = Unchecked.defaultof<obj>

            try
                error <- node.Ops.OnError(node, thrown)
                mapped <- true
            with _ ->
                ()

            if mapped then
                processOutcome &state onSuccessComplete onErrorComplete (OutcomeFailed error)
            else
                state.Effect <- InterruptNode<obj, obj>(Defect thrown, thrown.Message)
        ValueNone
    | NodeKind.Bind ->
        state.Effect <- node.Child
        state.ContStack.Push(ChainCont(node, Unchecked.defaultof<_>))
        ValueNone
    | NodeKind.Catch ->
        state.Effect <- node.Child
        state.ContStack.Push(ChainCont(Unchecked.defaultof<_>, node))
        ValueNone
    | NodeKind.Fold ->
        // One frame for both handlers: a failure of the success handler is not caught by the error handler (foldZIO).
        state.Effect <- node.Child
        state.ContStack.Push(ChainCont(node, node))
        ValueNone
    | NodeKind.OnFinalize
    | NodeKind.OnExit ->
        state.ContStack.Push(FinalizerCont node)
        state.Effect <- node.Child
        ValueNone
    | NodeKind.Suspend ->
        try
           state.Effect <- node.Ops.Force node
        with ex ->
           state.Effect <- InterruptNode<obj, obj>(Defect ex, ex.Message)
        ValueNone
    | NodeKind.WithSuppression ->
        let outer = state.InterruptionSuppressed
        state.ContStack.Push(RestoreSuppressionCont outer)
        try
            state.InterruptionSuppressed <- cast<int -> int> node.A outer
            state.Effect <- node.Ops.Body(node, outer)
        with ex ->
            state.Effect <- InterruptNode<obj, obj>(Defect ex, ex.Message)
        ValueNone
    | NodeKind.AcquireRelease ->
        let outer = state.InterruptionSuppressed
        state.ContStack.Push(AcquiredCont(node, outer))
        state.InterruptionSuppressed <- outer + 1
        state.Effect <- node.Child
        ValueNone
    | NodeKind.Map ->
        state.ContStack.Push(MappedCont node)
        state.Effect <- node.Child
        ValueNone
    | NodeKind.WriteChan
    | NodeKind.TryWriteChan ->
        ValueSome(HandleWriteChan(node.A, cast<Channel<obj>> node.B, node.Kind.IsTryWriteChan))
    | NodeKind.ReadChan ->
        ValueSome(HandleReadChan(cast<Channel<obj>> node.A))
    | NodeKind.ForkEffect
    | NodeKind.ForkDaemonEffect ->
        let struct (fiber, fiberContext) = node.Ops.ForkTarget node
        ValueSome(HandleForkEffect(node.Child, fiber, fiberContext, node.Kind.IsForkDaemonEffect))
    | NodeKind.JoinFiber ->
        ValueSome(HandleJoinFiber(cast<FiberContext> node.A))
    | NodeKind.JoinFirst ->
        ValueSome(HandleJoinFirst(cast<FiberContext list> node.A))
    | NodeKind.JoinAllFailFast ->
        ValueSome(HandleJoinAllFailFast(cast<FiberContext[]> node.A))
    | NodeKind.AwaitTask ->
        ValueSome(HandleAwaitTask(cast<Task<obj>> node.A, node))

// Generic so that this file gains no module-level static initializer. Interrupting a child interrupts its own
// children from inside this callback, one level of stack each.
type internal ForkScopeCallback<'Marker> private () =
    static member val OnClosed: Action<obj> =
        Action<obj>(fun state ->
            let child = state :?> FiberContext
            let parentId = child.Parent.Id
            if RuntimeHelpers.TryEnsureSufficientExecutionStack() then
                child.Interrupt(ParentInterrupted parentId, "Parent fiber scope closed.")
            else
                ThreadPool.UnsafeQueueUserWorkItem(
                    WaitCallback(fun _ ->
                        try
                            child.Interrupt(ParentInterrupted parentId, "Parent fiber scope closed.")
                        with _ ->
                            ()),
                    null)
                |> ignore)

let inline attachFork (parentContext: FiberContext) (childContext: FiberContext) (daemon: bool) (uninterruptible: bool) =
    if not daemon then
        let scope =
            if uninterruptible then parentContext.ProtectedChildScopeToken else parentContext.ChildScopeToken

        childContext.AttachTo parentContext
        let registration = scope.Register(ForkScopeCallback<FiberContext>.OnClosed, box childContext)
        childContext.AddRegistration registration

let inline defectError (fiberContext: FiberContext) (ex: exn) =
    FiberInterruptedException(fiberContext.Id, Defect ex, ex.Message) :> obj

let inline resumeWith (state: byref<InterpreterState>) (workItem: WorkItem) =
    workItem.InterruptionSuppressed <- state.InterruptionSuppressed
    workItem

let inline awaitedTask (awaited: Task<'T>) (suppressed: int) (fiberContext: FiberContext) =
    if suppressed > 0 then
        awaited
    else
        awaited.WaitAsync fiberContext.CancellationToken

let inline private endsByInterruption (waited: Task<obj>) (fiberContext: FiberContext) (suppressed: int) =
    waited.IsCanceled && suppressed = 0 && fiberContext.CancellationToken.IsCancellationRequested

let inline private failureOf (waited: Task<obj>) : exn =
    match waited.Exception with
    | null -> TaskCanceledException waited
    | aggregate ->
        match aggregate.InnerException with
        | null -> aggregate
        | inner -> inner

let settledTaskEffect (waited: Task<obj>) (fiberContext: FiberContext) (suppressed: int) (node: FIOBase) : FIOBase =
    if waited.IsCompletedSuccessfully then
        SuccessNode<obj, obj>(waited.Result)
    elif endsByInterruption waited fiberContext suppressed then
        interruptedEffect fiberContext "Task has been cancelled."
    else
        let ex = failureOf waited

        try
            FailureNode<obj, obj>(node.Ops.OnError(node, ex))
        with _ ->
            InterruptNode<obj, obj>(Defect ex, ex.Message)

let inline awaitTaskFailureOutcome (fiberContext: FiberContext) (node: FIOBase) (ex: exn) =
    try
        OutcomeFailed(node.Ops.OnError(node, ex))
    with _ ->
        OutcomeInterrupted(defectError fiberContext ex)

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
    (writeEffect: FIOBase)
    (fiberContext: FiberContext)
    (contStack: Stack<Cont>)
    (suppressed: int)
    (reschedule: WorkItem -> unit) =
    let cancellationToken =
        if suppressed > 0 then CancellationToken.None else fiberContext.CancellationToken

    let waited = channel.Queue.WaitToWriteAsync(cancellationToken).AsTask()

    let resume () =
        let effect =
            try
                if suppressed = 0 && fiberContext.CancellationToken.IsCancellationRequested then
                    interruptedEffect fiberContext "Fiber was interrupted while blocked on a channel write."
                else
                    writeEffect
            with ex ->
                InterruptNode<obj, obj>(Defect ex, ex.Message)

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
    (node: FIOBase)
    ([<InlineIfLambda>] reschedule: WorkItem -> unit) =
    let resume () =
        let effect =
            try
                settledTaskEffect waited fiberContext suppressed node
            with ex ->
                InterruptNode<obj, obj>(Defect ex, ex.Message)

        let resumeWorkItem =
            {
                Effect = effect
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
    (reschedule: Action<WorkItem>) =
    let waiter = BlockingWaiter workItem
    if suppressed = 0 then
        let registration =
            fiberContext.CancellationToken.Register(fun () ->
                try
                    if waiter.TryClaim() then
                        reschedule.Invoke waiter.WorkItem
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
