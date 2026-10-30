namespace FIO.DSL

open System
open System.Threading
open System.Buffers.Binary
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Runtime.ExceptionServices

// The mapper for effects that cannot fail: a throwing mapper becomes a Defect in the interpreter. It
// rethrows via ExceptionDispatchInfo because a plain raise would reset the original stack trace.
type internal Rethrow<'A>() =
    static let instance: exn -> 'A =
        fun ex ->
            ExceptionDispatchInfo.Capture(ex).Throw()
            Unchecked.defaultof<'A>
    static member Instance = instance

[<Struct>]
type internal ChannelMode =
    | Unbounded
    | Bounded
    | Dropping
    | Sliding

[<AutoOpen>]
module internal NodeCasts =

    let inline cast<'T when 'T : not struct> (value: obj) : 'T =
#if DEBUG
        value :?> 'T
#else
        Unsafe.As<'T> value
#endif

    // A null effect throws where it appears, so it never reaches an interpreter read that no handler covers.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let raiseNullEffect () : unit =
        raise (NullReferenceException())

[<Struct; RequireQualifiedAccess; NoEquality; NoComparison>]
type internal NodeKind =
    | Success
    | Failure
    | Interrupt
    | FiberCancellationToken
    | Action
    | Bind
    | Catch
    | Fold
    | Map
    | OnFinalize
    | OnExit
    | Suspend
    | WithSuppression
    | AcquireRelease
    | WriteChan
    | TryWriteChan
    | ReadChan
    | ForkEffect
    | ForkDaemonEffect
    | JoinFiber
    | JoinFirst
    | JoinAllFailFast
    | AwaitTask

[<Struct>]
type internal PostFinalizerSaved =
    | PostFinalizerSucceeded of value: obj
    | PostFinalizerFailed of error: obj
    | PostFinalizerInterrupted of error: obj

and [<Struct>] internal Cont =
    | ChainCont of onSuccess: FIOBase * onFailure: FIOBase
    | FinalizerCont of node: FIOBase
    | PostFinalizerCont of saved: PostFinalizerSaved * level: int
    | RestoreSuppressionCont of level: int
    | AcquiredCont of node: FIOBase * level: int
    | MappedCont of node: FIOBase

and internal WorkItem =
    {
        mutable Effect: FIOBase
        mutable FiberContext: FiberContext
        mutable ContStack: Stack<Cont>
        mutable InterruptionSuppressed: int
    }

and [<Sealed>] internal BlockingWaiter(workItem: WorkItem) =

    let mutable claimed = 0

    let mutable registration = Unchecked.defaultof<CancellationTokenRegistration>

    member _.WorkItem =
        workItem

    member _.SetRegistration (value: CancellationTokenRegistration) =
        registration <- value

    member _.TryClaim () =
        if tryClaim &claimed then
            registration.Dispose()
            true
        else
            false

/// The outcome of running a fiber to completion: success, failure, or interruption.
and FiberResult<'A, 'E> =
    /// The fiber completed successfully with a value.
    | Succeeded of value: 'A
    /// The fiber failed with a typed error.
    | Failed of error: 'E
    /// The fiber was interrupted before producing a result.
    | Interrupted of ex: FiberInterruptedException

and [<Sealed; AllowNullLiteral>] internal MailboxQueue<'A> private (channel: Channels.Channel<'A>) =
    let reader = channel.Reader
    let writer = channel.Writer

    new() = MailboxQueue<'A>(Channel.CreateUnbounded<'A>())

    static member internal Bounded (capacity: int, fullMode: BoundedChannelFullMode) =
        MailboxQueue<'A>(Channel.CreateBounded<'A>(BoundedChannelOptions(capacity, FullMode = fullMode)))

    member internal _.Count =
        reader.Count

    member internal _.WriteAsync value =
        writer.WriteAsync value

    member internal _.WriteAsync (value, cancellationToken: CancellationToken) =
        writer.WriteAsync(value, cancellationToken)

    member internal _.TryWrite value =
        writer.TryWrite value

    member internal _.WaitToWriteAsync (cancellationToken: CancellationToken) =
        writer.WaitToWriteAsync cancellationToken

    member internal _.TryRead (value: byref<'A>) =
        reader.TryRead &value

    member internal _.WaitToReadAsync (cancellationToken: CancellationToken) =
        reader.WaitToReadAsync cancellationToken

and [<Sealed>] internal BlockingWorkItemSlot() =

    [<VolatileField>]
    let mutable queue: MailboxQueue<BlockingWaiter> = null

    member _.Count =
        let queue' = Volatile.Read &queue
        if isNull queue' then 0
        else queue'.Count

    member _.TryGet () =
        Volatile.Read &queue

    member _.GetOrCreate () =
        initIfNull &queue (fun () -> MailboxQueue<BlockingWaiter>())

and private FiberContextState =
    | Running = 0
    | Completed = 1
    | Interrupted = 2

and [<Sealed; AllowNullLiteral>] internal FiberContext() =
    [<VolatileField>]
    let mutable idHigh = 0L

    let mutable idLow = 0L

    let mutable state = int FiberContextState.Running

    [<VolatileField>]
    let mutable blockingWorkItemQueue: MailboxQueue<BlockingWaiter> = null

    let resultSource =
        TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously

    let cancelSource = new CancellationTokenSource()

    [<VolatileField>]
    let mutable registration: IDisposable = null

    [<VolatileField>]
    let mutable childScope = null

    [<VolatileField>]
    let mutable protectedScope = null

    let cancelScope (source: CancellationTokenSource) =
        match source with
        | null -> ()
        | source ->
            // A child whose interrupt throws must not stop the scope's other children or the completion that cancels it.
            try
                source.Cancel(throwOnFirstException = false)
            with _ ->
                ()

    [<VolatileField>]
    let mutable parent = null

    // Scoped children that have not yet unwound.
    [<VolatileField>]
    let mutable outstanding = 0

    [<VolatileField>]
    let mutable completing = 0

    [<VolatileField>]
    let mutable published = 0

    let mutable pendingValue = Unchecked.defaultof<Result<obj, obj>>

    [<VolatileField>]
    let mutable rescheduleSink: Action<WorkItem> = null

    [<VolatileField>]
    let mutable disposed = 0

    [<VolatileField>]
    let mutable onTerminalCallback = Unchecked.defaultof<_>

    [<VolatileField>]
    let mutable onTerminalFired = 0

    [<VolatileField>]
    let mutable onUnwoundCallback = null

    static member inline private Split (guid: Guid) : struct (int64 * int64) =
        let mutable guid = guid
        let bytes = MemoryMarshal.AsBytes(MemoryMarshal.CreateSpan(&guid, 1))
        struct (BinaryPrimitives.ReadInt64LittleEndian(bytes.Slice(0, 8)), BinaryPrimitives.ReadInt64LittleEndian(bytes.Slice(8, 8)))

    static member inline private Compose (low: int64, high: int64) : Guid =
        let mutable guid = Guid.Empty
        let bytes = MemoryMarshal.AsBytes(MemoryMarshal.CreateSpan(&guid, 1))
        BinaryPrimitives.WriteInt64LittleEndian(bytes.Slice(0, 8), low)
        BinaryPrimitives.WriteInt64LittleEndian(bytes.Slice(8, 8), high)
        guid

    // A v4 Guid has its version nibble in byte 7 and its variant bits in byte 8, so 0 means "unset" in each half.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member private _.CreateId () : Guid =
        let fresh = Guid.NewGuid()
        let struct (low, high) = FiberContext.Split fresh

        if Interlocked.CompareExchange(&idLow, low, 0L) = 0L then
            Volatile.Write(&idHigh, high)
            fresh
        else
            let mutable spinner = SpinWait()
            while Volatile.Read &idHigh = 0L do
                spinner.SpinOnce()

            FiberContext.Compose(Volatile.Read &idLow, Volatile.Read &idHigh)

    member internal this.Id : Guid =
        let high = Volatile.Read &idHigh
        if high <> 0L then
            FiberContext.Compose(Volatile.Read &idLow, high)
        else
            this.CreateId()

    member internal _.Task =
        resultSource.Task

    member internal _.CancellationToken =
        cancelSource.Token

    member internal this.SetOnTerminal (callback: unit -> unit) =
        Interlocked.Exchange<unit -> unit>(&onTerminalCallback, callback) |> ignore
        if this.IsTerminal() then
            this.InvokeOnTerminal()

    member internal this.SetOnUnwound (callback: Action<FiberContext>) =
        Interlocked.Exchange<Action<FiberContext>>(&onUnwoundCallback, callback) |> ignore
        if Volatile.Read &published = 2 then
            this.InvokeOnUnwound()

    member internal this.AddBlockingWorkItem (waiter: BlockingWaiter) =
        let queue = this.GetOrCreateBlockingQueue()
        queue.WriteAsync waiter

    member private _.RescheduleBlockingWorkItems (reschedule: Action<WorkItem>) =
        let queue = Volatile.Read &blockingWorkItemQueue
        if not (isNull queue) then
            let mutable waiter = Unchecked.defaultof<_>
            while queue.TryRead &waiter do
                if waiter.TryClaim() then
                    reschedule.Invoke waiter.WorkItem

    member internal this.RescheduleOnEnd (reschedule: Action<WorkItem>) =
        Interlocked.Exchange(&rescheduleSink, reschedule) |> ignore
        if this.IsTerminal() then
            this.RescheduleBlockingWorkItems reschedule

    member internal _.ChildScopeToken =
        (initIfNull &childScope (fun () -> new CancellationTokenSource())).Token

    member internal _.ProtectedChildScopeToken =
        (initIfNull &protectedScope (fun () -> new CancellationTokenSource())).Token

    member internal _.RegisterChild () =
        Interlocked.Increment &outstanding |> ignore

    member internal _.AttachTo (newParent: FiberContext) =
        parent <- newParent
        newParent.RegisterChild()

    member internal _.Parent =
        Volatile.Read &parent

    member private _.CancelChildScope () =
        cancelScope (Volatile.Read &childScope)

    member internal this.AddRegistration (value: IDisposable) =
        if not (isNull (Interlocked.CompareExchange(&registration, value, null))) then
            raise (InvalidOperationException "A fiber context accepts exactly one scope registration.")

        if this.IsTerminal() then
            this.DisposeRegistrations()

    member internal _.IsCompleted () =
        Volatile.Read &state = int FiberContextState.Completed

    member internal _.IsInterrupted () =
        Volatile.Read &state = int FiberContextState.Interrupted

    member internal _.IsTerminal () =
        Volatile.Read &state <> int FiberContextState.Running

    member private this.Publish (value, reschedule: Action<WorkItem>) =
        let previous = transitionFrom &state (int FiberContextState.Running) (int FiberContextState.Completed)
        this.DisposeRegistrations()

        if previous = int FiberContextState.Running then
            resultSource.TrySetResult value |> ignore
        else
            let mutable spinner = SpinWait()
            while not resultSource.Task.IsCompleted do
                spinner.SpinOnce()

        this.InvokeOnTerminal()

        match if isNull reschedule then Volatile.Read &rescheduleSink else reschedule with
        | null -> ()
        | reschedule -> this.RescheduleBlockingWorkItems reschedule

        match Volatile.Read &parent with
        | null -> ()
        | scope -> scope.OnChildUnwound()

        Interlocked.Exchange(&published, 2) |> ignore
        this.ReleaseIfRequested()
        this.InvokeOnUnwound()

    member private this.TryFinish (reschedule: Action<WorkItem>) =
        if Volatile.Read &completing = 1
           && Volatile.Read &outstanding = 0
           && tryClaim &published then
            this.Publish(pendingValue, reschedule)

    member internal this.Abandon () =
        if Volatile.Read &completing = 1 && tryClaim &published then
            this.Publish(pendingValue, null)

    member private this.OnChildUnwound () =
        Interlocked.Decrement &outstanding |> ignore

        if RuntimeHelpers.TryEnsureSufficientExecutionStack() then
            this.TryFinish null
        else
            ThreadPool.UnsafeQueueUserWorkItem(WaitCallback(fun _ -> this.TryFinish null), null) |> ignore

    member internal this.Complete value =
        this.CompleteInternal(value, null)

    member internal this.CompleteAndReschedule (value, reschedule: Action<WorkItem>) =
        this.CompleteInternal(value, reschedule)
        ValueTask.CompletedTask

    member private this.CompleteInternal (value, reschedule: Action<WorkItem>) =
        // A later completion, such as a worker's crash handler, must not replace the value still waiting to be published.
        if Volatile.Read &completing = 0 then
            pendingValue <- value

        if Interlocked.Exchange(&completing, 1) = 0 then
            this.CancelChildScope()
            cancelScope (Volatile.Read &protectedScope)
            this.TryFinish reschedule

    member internal this.Interrupt (?cause, ?message) =
        let cause = defaultArg cause ExplicitInterrupt
        let message = defaultArg message "Fiber was interrupted."
        if Volatile.Read &completing = 0
           && tryTransition &state (int FiberContextState.Running) (int FiberContextState.Interrupted) then
            let interruptError = Error(FiberInterruptedException(this.Id, cause, message) :> obj)
            resultSource.TrySetResult interruptError |> ignore

            try
                cancelSource.Cancel(throwOnFirstException = false)
            with _ ->
                ()

            this.CancelChildScope()
            this.DisposeRegistrations()
            this.InvokeOnTerminal()

            match Volatile.Read &rescheduleSink with
            | null -> ()
            | reschedule -> this.RescheduleBlockingWorkItems reschedule

    member private _.InvokeOnTerminal () =
        let callback = onTerminalCallback

        if not (isNull (box callback)) && tryClaim &onTerminalFired then
            try callback ()
            with _ -> ()

    member private this.InvokeOnUnwound () =
        match Interlocked.Exchange(&onUnwoundCallback, null) with
        | null -> ()
        | callback ->
            try callback.Invoke this
            with _ -> ()

    member private _.GetOrCreateBlockingQueue () : MailboxQueue<BlockingWaiter> =
        initIfNull &blockingWorkItemQueue (fun () -> MailboxQueue<BlockingWaiter>())

    member private _.DisposeRegistrations () =
        match Interlocked.Exchange(&registration, null) with
        | null -> ()
        | victim ->
            try
                victim.Dispose()
            with :? ObjectDisposedException ->
                ()

    member private _.ReleaseIfRequested () =
        if Volatile.Read &disposed = 2 && Interlocked.CompareExchange(&disposed, 1, 2) = 2 then
            cancelSource.Dispose()

            match Volatile.Read &childScope with
            | null -> ()
            | source -> source.Dispose()

            match Volatile.Read &protectedScope with
            | null -> ()
            | source -> source.Dispose()

    interface IDisposable with

        member this.Dispose () =
            Interlocked.CompareExchange(&disposed, 2, 0) |> ignore

            if Volatile.Read &published = 2 then
                this.ReleaseIfRequested()

and [<Sealed>] internal JoinAllLatch(count: int, signal: unit -> unit) =

    let mutable remaining = count

    let mutable settled = 0

    member _.OnChildTerminal (fiberContext: FiberContext) =

        let mutable spinner = SpinWait()
        while not fiberContext.Task.IsCompleted do
            spinner.SpinOnce()

        let failed =
            match fiberContext.Task.Result with
            | Error _ -> true
            | Ok _ -> false

        if failed then
            if tryClaim &settled then
                signal ()
        elif Interlocked.Decrement &remaining = 0 && tryClaim &settled then
            signal ()

/// A running fiber (green thread) executing an effect. Join, await, interrupt, or poll it for its result; disposing it
/// releases its resources once it has finished.
and [<Sealed>] Fiber<'A, 'E> internal () =
    let fiberContext = new FiberContext()

    /// This fiber's unique identifier.
    member _.Id : Guid =
        fiberContext.Id

    /// A cancellation token tied to this fiber's lifetime; cancelled when the fiber is interrupted.
    member _.CancellationToken : CancellationToken =
        fiberContext.CancellationToken

    static member inline private ClassifyResult (result: Result<obj, obj>) : FiberResult<'A, 'E> =
        match result with
        | Ok value -> Succeeded(value :?> 'A)
        | Error error ->
            match error with
            | :? FiberInterruptedException as ex -> Interrupted ex
            | _ -> Failed(error :?> 'E)

    /// Returns a task that completes with this fiber's result, for interop with task-based code.
    member _.Task () : Task<FiberResult<'A, 'E>> =
        task {
            let! result = fiberContext.Task
            return Fiber<'A, 'E>.ClassifyResult result
        }

    /// Returns an effect that waits for this fiber to complete and yields its success value.
    member _.Join () : FIO<'A, 'E> =
        JoinFiberNode<'A, 'E>(fiberContext)

    /// Returns an effect that interrupts this fiber with the given cause and message.
    member _.Interrupt (cause: InterruptionCause) (message: string) : FIO<unit, 'E> =
        ActionNode<unit, 'E>((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)

    /// Returns an effect that interrupts this fiber with an explicit-interrupt cause.
    member this.InterruptNow () : FIO<unit, 'E> =
        this.Interrupt ExplicitInterrupt "Fiber interrupted"

    /// Returns an effect that waits for this fiber and yields its full result (success, failure, or interruption).
    member this.Await<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        let task = fiberContext.Task
        if task.IsCompletedSuccessfully then
            AwaitTaskNode<FiberResult<'A, 'E>, 'E2>(
                Task.FromResult(box (Fiber<'A, 'E>.ClassifyResult task.Result)),
                Rethrow<_>.Instance)
        else
            AwaitTaskNode<FiberResult<'A, 'E>, 'E2>(boxTask (this.Task()), Rethrow<_>.Instance)

    /// Returns an effect that interrupts this fiber, then waits for and yields its result.
    member this.InterruptAwait<'E2> (cause: InterruptionCause) (message: string) : FIO<FiberResult<'A, 'E>, 'E2> =
        ActionNode<unit, 'E2>((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)
            .FlatMap <| fun () -> this.Await()

    /// Returns an effect that interrupts this fiber with an explicit-interrupt cause, then yields its result.
    member this.InterruptAwaitNow<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        this.InterruptAwait ExplicitInterrupt "Fiber interrupted"

    /// Returns an effect that yields this fiber's result if it has completed, or None if it is still running.
    member _.Poll<'E2> () : FIO<FiberResult<'A, 'E> option, 'E2> =
        ActionNode<FiberResult<'A, 'E> option, 'E2>((fun () ->
            if not (fiberContext.IsTerminal()) then
                None
            else
                Some(Fiber<'A, 'E>.ClassifyResult fiberContext.Task.Result)),
            Rethrow<_>.Instance)

    /// Returns an effect that waits for this fiber and continues with the matching handler for success, failure, or interruption.
    member this.JoinWith<'A1, 'E1>
        (onSucceeded: 'A -> FIO<'A1, 'E1>)
        (onFailed: 'E -> FIO<'A1, 'E1>)
        (onInterrupted: FiberInterruptedException -> FIO<'A1, 'E1>)
        : FIO<'A1, 'E1> =
        this.Await().FlatMap <| fun result ->
            match result with
            | Succeeded value -> onSucceeded value
            | Failed error -> onFailed error
            | Interrupted ex -> onInterrupted ex

    /// Returns true if this fiber ran to completion (success or failure), as opposed to being interrupted.
    member _.IsCompleted () : bool =
        fiberContext.IsCompleted()

    /// Returns true if this fiber was interrupted.
    member _.IsInterrupted () : bool =
        fiberContext.IsInterrupted()

    /// Returns true if this fiber is no longer running — either completed or interrupted.
    member _.IsTerminal () : bool =
        fiberContext.IsTerminal()

    /// Blocks the calling thread until this fiber completes and returns its result. Prefer Await inside effects.
    member this.UnsafeResult () : FiberResult<'A, 'E> =
        this.Task()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    /// Blocks the calling thread until this fiber completes and returns its success value, raising if it failed or was interrupted.
    member this.UnsafeSuccess () : 'A =
        match this.UnsafeResult() with
        | Succeeded value ->
            value
        | Failed error ->
            raise (InvalidOperationException $"Fiber failed with error: {error}")
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Blocks the calling thread until this fiber completes and returns its error, raising if it succeeded or was interrupted.
    member this.UnsafeError () : 'E =
        match this.UnsafeResult() with
        | Succeeded value ->
            raise (InvalidOperationException $"Fiber succeeded with value: {value}")
        | Failed error ->
            error
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Blocks the calling thread until this fiber completes and prints its result.
    member this.UnsafePrintResult () : unit =
        printfn "%A" (this.UnsafeResult())

    member internal _.Context =
        fiberContext

    override this.ToString () : string =
        this.Id.ToString()

    interface IDisposable with

        member _.Dispose () : unit =
            (fiberContext :> IDisposable).Dispose()

/// A typed, asynchronous channel for passing messages between fibers.
and [<Sealed; AllowNullLiteral>] Channel<'A> private
    (id: Guid,
    valueQueue: MailboxQueue<obj>,
    blockingSlot: BlockingWorkItemSlot,
    mode: ChannelMode) =
    [<VolatileField>]
    let mutable upcastChannel = null

    static let bounded (capacity: int) (fullMode: BoundedChannelFullMode) =
        if capacity < 1 then
            raise (ArgumentOutOfRangeException(nameof capacity, capacity, "A channel's capacity must be at least 1."))

        MailboxQueue<obj>.Bounded(capacity, fullMode)

    /// Creates a new, empty channel with no capacity limit.
    new() = Channel(Guid.NewGuid(), MailboxQueue<obj>(), BlockingWorkItemSlot(), Unbounded)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel suspends
    /// until a message is read. A capacity below 1 throws ArgumentOutOfRangeException.
    static member Bounded (capacity: int) : Channel<'A> =
        Channel<'A>(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.Wait, BlockingWorkItemSlot(), Bounded)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel drops the
    /// new message. A capacity below 1 throws ArgumentOutOfRangeException.
    static member Dropping (capacity: int) : Channel<'A> =
        Channel<'A>(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.Wait, BlockingWorkItemSlot(), Dropping)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel drops the
    /// oldest message. A capacity below 1 throws ArgumentOutOfRangeException.
    static member Sliding (capacity: int) : Channel<'A> =
        Channel<'A>(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.DropOldest, BlockingWorkItemSlot(), Sliding)

    /// This channel's unique identifier.
    member _.Id : Guid =
        id

    /// The number of messages currently buffered in this channel.
    member _.Count : int =
        valueQueue.Count

    /// Returns an effect that writes a message to this channel and yields it; when the channel is full it suspends
    /// (Bounded), drops the message (Dropping) or displaces the oldest one (Sliding).
    member this.Write<'E> (message: 'A) : FIO<'A, 'E> =
        WriteChanNode<'A, 'E>(box message, this.Upcast(), false)

    /// Returns an effect that writes a message to this channel without ever suspending, yielding whether it was
    /// written: false from a full Bounded or Dropping channel, true from a full Sliding one, which displaces its oldest.
    member this.TryWrite<'E> (message: 'A) : FIO<bool, 'E> =
        WriteChanNode<bool, 'E>(box message, this.Upcast(), true)

    /// Returns an effect that reads the next message from this channel, suspending the fiber until one is available.
    member this.Read<'E> () : FIO<'A, 'E> =
        ReadChanNode<'A, 'E>(this.Upcast())

    /// Returns an effect that reads the next message if this channel holds one, yielding None instead of suspending.
    member _.TryRead<'E> () : FIO<'A option, 'E> =
        ActionNode<'A option, 'E>((fun () ->
            let mutable message = Unchecked.defaultof<obj>
            if valueQueue.TryRead &message then Some(message :?> 'A) else None),
            Rethrow<_>.Instance)

    member internal _.BlockingWorkItemCount =
        blockingSlot.Count

    member internal _.AddBlockingWorkItem (waiter: BlockingWaiter) =
        let queue = blockingSlot.GetOrCreate()
        queue.WriteAsync waiter

    member internal _.TryDequeueBlockingWorkItem (workItem: byref<WorkItem>) =
        let queue = blockingSlot.TryGet()
        if isNull queue then
            false
        else
            let mutable waiter = Unchecked.defaultof<_>
            let mutable found = false
            while not found && queue.TryRead &waiter do
                if waiter.TryClaim() then
                    workItem <- waiter.WorkItem
                    found <- true
            found

    member internal _.Queue =
        valueQueue

    member internal _.Mode =
        mode

    member internal _.Upcast () =
        initIfNull &upcastChannel (fun () -> Channel<obj>(id, valueQueue, blockingSlot, mode))

/// The non-generic base type that every effect derives from; it has no public members.
and [<AbstractClass; NoEquality; NoComparison>] FIOBase =
    val internal Kind: NodeKind
    val internal A: obj
    val internal B: obj

    internal new (kind: NodeKind, a: obj, b: obj) = { Kind = kind; A = a; B = b }

    member inline internal this.Child : FIOBase =
        cast<FIOBase> this.A

    member inline internal this.Ops : NodeOps =
        cast<NodeOps> this.B

/// A lazy, type-safe description of an effect that, when run, either succeeds with a value or fails with a typed error.
and [<AbstractClass; NoEquality; NoComparison>] FIO<'A, 'E> =
    inherit FIOBase

    internal new (kind: NodeKind, a: obj, b: obj) = { inherit FIOBase(kind, a, b) }

    /// Returns an effect that passes this effect's success value into the given function.
    member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        BindNode<'A, 'A1, 'E>(this, cont)

    /// Returns an effect that recovers from this effect's error with the given handler.
    member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        CatchNode<'A, 'E, 'E1>(this, onError)

    /// Returns an effect that runs the given finalizer after this effect on success, failure, and interruption alike.
    /// A finalizer that fails replaces a success with its failure, but not an earlier failure or interruption.
    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> =
        FinalizeNode<'A, 'E>(this, finalizer)

    /// Returns an effect that runs the given finalizer after this effect with its outcome: success, failure, or
    /// interruption and its cause. Like Ensuring, it runs uninterruptibly, and its failure replaces only a success.
    member this.OnExit (finalizer: FiberResult<'A, 'E> -> FIO<unit, 'E>) : FIO<'A, 'E> =
        ExitNode<'A, 'E>(this, finalizer)

    /// Returns an effect that runs this effect on a new fiber scoped to this one, which interrupts and awaits it when it
    /// finishes, and interrupts it at once when interrupted unless it was forked uninterruptibly. See ForkDaemon.
    member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        SuspendNode<Fiber<'A, 'E>, 'E1>(fun () -> ForkNode<'A, 'E, 'E1>(this, new Fiber<'A, 'E>(), false))

    /// Returns an effect that runs this effect on a new unscoped fiber, yielding its handle: it is neither interrupted
    /// nor awaited when this fiber finishes, so its lifetime and finalizers are the caller's to manage.
    member this.ForkDaemon<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        SuspendNode<Fiber<'A, 'E>, 'E1>(fun () -> ForkNode<'A, 'E, 'E1>(this, new Fiber<'A, 'E>(), true))

    /// Returns an effect that applies the given function to this effect's success value.
    member this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> =
        MapNode<'A, 'A1, 'E>(this, mapper)

    /// Returns an effect that applies the given function to this effect's error.
    member this.MapError<'E1> (mapper: 'E -> 'E1) : FIO<'A, 'E1> =
        CatchNode<'A, 'E, 'E1>(this, fun error -> FailureNode<'A, 'E1>(mapper error))

    /// Returns an effect that maps this effect's success value and error with the two given functions.
    member this.MapBoth<'A1, 'E1> (successMapper: 'A -> 'A1) (errorMapper: 'E -> 'E1) : FIO<'A1, 'E1> =
        FoldNode<'A, 'E, 'A1, 'E1>(
            this,
            (fun value -> SuccessNode<'A1, 'E1>(successMapper value)),
            (fun error -> FailureNode<'A1, 'E1>(errorMapper error)))

    /// Returns an effect that always succeeds, capturing this effect's outcome as a Result.
    member this.Result<'E1> () : FIO<Result<'A, 'E>, 'E1> =
        FoldNode<'A, 'E, Result<'A, 'E>, 'E1>(
            this,
            (fun value -> SuccessNode<Result<'A, 'E>, 'E1>(Ok value)),
            (fun error -> SuccessNode<Result<'A, 'E>, 'E1>(Error error)))

    /// Returns an effect that always succeeds, yielding Some on success and None on failure.
    member this.Option<'E1> () : FIO<'A option, 'E1> =
        FoldNode<'A, 'E, 'A option, 'E1>(
            this,
            (fun value -> SuccessNode<'A option, 'E1>(Some value)),
            (fun _ -> SuccessNode<'A option, 'E1>(None)))

    /// Returns an effect that always succeeds, capturing this effect's outcome as a Choice.
    member this.Choice<'E1> () : FIO<Choice<'A, 'E>, 'E1> =
        FoldNode<'A, 'E, Choice<'A, 'E>, 'E1>(
            this,
            (fun value -> SuccessNode<Choice<'A, 'E>, 'E1>(Choice1Of2 value)),
            (fun error -> SuccessNode<Choice<'A, 'E>, 'E1>(Choice2Of2 error)))

and [<AbstractClass; AllowNullLiteral>] internal NodeOps() =

    abstract Continue: node: FIOBase * value: obj -> FIOBase
    default _.Continue (_, _) = raise (InvalidOperationException "The node has no success continuation.")

    abstract Fail: node: FIOBase * error: obj -> FIOBase
    default _.Fail (_, _) = raise (InvalidOperationException "The node has no failure continuation.")

    abstract Apply: node: FIOBase * value: obj -> obj
    default _.Apply (_, _) = raise (InvalidOperationException "The node has no mapper.")

    abstract Invoke: node: FIOBase -> obj
    default _.Invoke _ = raise (InvalidOperationException "The node has no function.")

    abstract OnError: node: FIOBase * ex: exn -> obj
    default _.OnError (_, _) = raise (InvalidOperationException "The node has no error mapping.")

    abstract Force: node: FIOBase -> FIOBase
    default _.Force _ = raise (InvalidOperationException "The node has no thunk.")

    abstract Body: node: FIOBase * level: int -> FIOBase
    default _.Body (_, _) = raise (InvalidOperationException "The node has no body.")

    abstract OnAcquired: node: FIOBase * value: obj -> FIOBase
    default _.OnAcquired (_, _) = raise (InvalidOperationException "The node acquires nothing.")

    abstract RunExit: node: FIOBase * saved: PostFinalizerSaved -> FIOBase
    default _.RunExit (_, _) = raise (InvalidOperationException "The node has no exit finalizer.")

    abstract ForkTarget: node: FIOBase -> struct (obj * FiberContext)
    default _.ForkTarget _ = raise (InvalidOperationException "The node forks nothing.")

and [<Sealed>] internal SuccessNode<'A, 'E>(value: 'A) =
    inherit FIO<'A, 'E>(NodeKind.Success, box value, null)

and [<Sealed>] internal FailureNode<'A, 'E>(error: 'E) =
    inherit FIO<'A, 'E>(NodeKind.Failure, box error, null)

and [<Sealed>] internal InterruptNode<'A, 'E>(cause: InterruptionCause, message: string) =
    inherit FIO<'A, 'E>(NodeKind.Interrupt, cause, message)

and [<Sealed>] internal CancellationTokenNode<'E> private () =
    inherit FIO<CancellationToken, 'E>(NodeKind.FiberCancellationToken, null, null)

    [<DefaultValue>]
    static val mutable private instance: CancellationTokenNode<'E>

    static member Instance =
        let current = CancellationTokenNode<'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = CancellationTokenNode<'E>()
            CancellationTokenNode<'E>.instance <- created
            created
        else
            current

and [<Sealed>] internal ActionNode<'A, 'E>(func: unit -> 'A, onError: exn -> 'E) =
    inherit FIO<'A, 'E>(NodeKind.Action, func, ActionOps<'A, 'E>.Instance)

    member _.OnError = onError

and [<Sealed>] internal ActionOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: ActionOps<'A, 'E>

    static member Instance =
        let current = ActionOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = ActionOps<'A, 'E>()
            ActionOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.Invoke node =
        box ((cast<unit -> 'A> node.A) ())

    override _.OnError (node, ex) =
        box ((cast<ActionNode<'A, 'E>> node).OnError ex)

and [<Sealed>] internal BindNode<'A, 'B, 'E>(inner: FIO<'A, 'E>, cont: 'A -> FIO<'B, 'E>) =
    inherit FIO<'B, 'E>(NodeKind.Bind, inner, BindOps<'A, 'B, 'E>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    member _.Cont = cont

and [<Sealed>] internal BindOps<'A, 'B, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: BindOps<'A, 'B, 'E>

    static member Instance =
        let current = BindOps<'A, 'B, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = BindOps<'A, 'B, 'E>()
            BindOps<'A, 'B, 'E>.instance <- created
            created
        else
            current

    override _.Continue (node, value) =
        let effect = (cast<BindNode<'A, 'B, 'E>> node).Cont (value :?> 'A)

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal CatchNode<'A, 'E, 'E1>(inner: FIO<'A, 'E>, handler: 'E -> FIO<'A, 'E1>) =
    inherit FIO<'A, 'E1>(NodeKind.Catch, inner, CatchOps<'A, 'E, 'E1>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    member _.Handler = handler

and [<Sealed>] internal CatchOps<'A, 'E, 'E1> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: CatchOps<'A, 'E, 'E1>

    static member Instance =
        let current = CatchOps<'A, 'E, 'E1>.instance
        if obj.ReferenceEquals(current, null) then
            let created = CatchOps<'A, 'E, 'E1>()
            CatchOps<'A, 'E, 'E1>.instance <- created
            created
        else
            current

    override _.Fail (node, error) =
        let effect = (cast<CatchNode<'A, 'E, 'E1>> node).Handler (error :?> 'E)

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal FoldNode<'A, 'E, 'B, 'E1>
    (inner: FIO<'A, 'E>, onSuccess: 'A -> FIO<'B, 'E1>, onFailure: 'E -> FIO<'B, 'E1>) =
    inherit FIO<'B, 'E1>(NodeKind.Fold, inner, FoldOps<'A, 'E, 'B, 'E1>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    member _.OnSuccess = onSuccess

    member _.OnFailure = onFailure

and [<Sealed>] internal FoldOps<'A, 'E, 'B, 'E1> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: FoldOps<'A, 'E, 'B, 'E1>

    static member Instance =
        let current = FoldOps<'A, 'E, 'B, 'E1>.instance
        if obj.ReferenceEquals(current, null) then
            let created = FoldOps<'A, 'E, 'B, 'E1>()
            FoldOps<'A, 'E, 'B, 'E1>.instance <- created
            created
        else
            current

    override _.Continue (node, value) =
        let effect = (cast<FoldNode<'A, 'E, 'B, 'E1>> node).OnSuccess (value :?> 'A)

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

    override _.Fail (node, error) =
        let effect = (cast<FoldNode<'A, 'E, 'B, 'E1>> node).OnFailure (error :?> 'E)

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal MapNode<'A, 'B, 'E>(inner: FIO<'A, 'E>, mapper: 'A -> 'B) =
    inherit FIO<'B, 'E>(NodeKind.Map, inner, MapOps<'A, 'B, 'E>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    member _.Mapper = mapper

and [<Sealed>] internal MapOps<'A, 'B, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: MapOps<'A, 'B, 'E>

    static member Instance =
        let current = MapOps<'A, 'B, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = MapOps<'A, 'B, 'E>()
            MapOps<'A, 'B, 'E>.instance <- created
            created
        else
            current

    override _.Apply (node, value) =
        box ((cast<MapNode<'A, 'B, 'E>> node).Mapper (value :?> 'A))

and [<Sealed>] internal UnitNode<'A, 'E>(inner: FIO<'A, 'E>) =
    inherit FIO<unit, 'E>(NodeKind.Map, inner, UnitOps<'A, 'E>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

and [<Sealed>] internal UnitOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: UnitOps<'A, 'E>

    static member Instance =
        let current = UnitOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = UnitOps<'A, 'E>()
            UnitOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.Apply (_, _) =
        box ()

and [<Sealed>] internal AsNode<'A, 'B, 'E>(inner: FIO<'A, 'E>, value: 'B) =
    inherit FIO<'B, 'E>(NodeKind.Map, inner, AsOps<'A, 'B, 'E>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    let boxed = box value

    member _.Boxed = boxed

and [<Sealed>] internal AsOps<'A, 'B, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: AsOps<'A, 'B, 'E>

    static member Instance =
        let current = AsOps<'A, 'B, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = AsOps<'A, 'B, 'E>()
            AsOps<'A, 'B, 'E>.instance <- created
            created
        else
            current

    override _.Apply (node, _) =
        (cast<AsNode<'A, 'B, 'E>> node).Boxed

and [<Sealed>] internal FinalizeNode<'A, 'E>(inner: FIO<'A, 'E>, finalizer: FIO<unit, 'E>) =
    inherit FIO<'A, 'E>(NodeKind.OnFinalize, inner, finalizer)

    do
        if obj.ReferenceEquals(inner, null) || obj.ReferenceEquals(finalizer, null) then
            raiseNullEffect ()

and [<Sealed>] internal ExitNode<'A, 'E>(inner: FIO<'A, 'E>, finalizer: FiberResult<'A, 'E> -> FIO<unit, 'E>) =
    inherit FIO<'A, 'E>(NodeKind.OnExit, inner, ExitOps<'A, 'E>.Instance)

    do
        if obj.ReferenceEquals(inner, null) then
            raiseNullEffect ()

    member _.Finalizer = finalizer

and [<Sealed>] internal ExitOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: ExitOps<'A, 'E>

    static member Instance =
        let current = ExitOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = ExitOps<'A, 'E>()
            ExitOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.RunExit (node, saved) =
        let outcome =
            match saved with
            | PostFinalizerSucceeded value -> Succeeded(value :?> 'A)
            | PostFinalizerFailed error -> Failed(error :?> 'E)
            | PostFinalizerInterrupted error -> Interrupted(error :?> FiberInterruptedException)

        let effect = (cast<ExitNode<'A, 'E>> node).Finalizer outcome

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal SuspendNode<'A, 'E>(thunk: unit -> FIO<'A, 'E>) =
    inherit FIO<'A, 'E>(NodeKind.Suspend, thunk, SuspendOps<'A, 'E>.Instance)

and [<Sealed>] internal SuspendOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: SuspendOps<'A, 'E>

    static member Instance =
        let current = SuspendOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = SuspendOps<'A, 'E>()
            SuspendOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.Force node =
        let effect = (cast<unit -> FIO<'A, 'E>> node.A) ()

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal SuppressionNode<'A, 'E>(update: int -> int, body: int -> FIO<'A, 'E>) =
    inherit FIO<'A, 'E>(NodeKind.WithSuppression, update, SuppressionOps<'A, 'E>.Instance)

    member _.Body = body

and [<Sealed>] internal SuppressionOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: SuppressionOps<'A, 'E>

    static member Instance =
        let current = SuppressionOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = SuppressionOps<'A, 'E>()
            SuppressionOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.Body (node, level) =
        let effect = (cast<SuppressionNode<'A, 'E>> node).Body level

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal AcquireReleaseNode<'A, 'A1, 'E>(acquire: FIO<'A1, 'E>, onAcquired: 'A1 -> FIO<'A, 'E>) =
    inherit FIO<'A, 'E>(NodeKind.AcquireRelease, acquire, AcquireReleaseOps<'A, 'A1, 'E>.Instance)

    do
        if obj.ReferenceEquals(acquire, null) then
            raiseNullEffect ()

    member _.OnAcquired = onAcquired

and [<Sealed>] internal AcquireReleaseOps<'A, 'A1, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: AcquireReleaseOps<'A, 'A1, 'E>

    static member Instance =
        let current = AcquireReleaseOps<'A, 'A1, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = AcquireReleaseOps<'A, 'A1, 'E>()
            AcquireReleaseOps<'A, 'A1, 'E>.instance <- created
            created
        else
            current

    override _.OnAcquired (node, value) =
        let effect = (cast<AcquireReleaseNode<'A, 'A1, 'E>> node).OnAcquired (value :?> 'A1)

        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

        effect

and [<Sealed>] internal WriteChanNode<'A, 'E>(message: obj, channel: Channel<obj>, reportAccepted: bool) =
    inherit FIO<'A, 'E>((if reportAccepted then NodeKind.TryWriteChan else NodeKind.WriteChan), message, channel)

and [<Sealed>] internal ReadChanNode<'A, 'E>(channel: Channel<obj>) =
    inherit FIO<'A, 'E>(NodeKind.ReadChan, channel, null)

and [<Sealed>] internal ForkNode<'A, 'E, 'E1>(effect: FIO<'A, 'E>, fiber: Fiber<'A, 'E>, daemon: bool) =
    inherit FIO<Fiber<'A, 'E>, 'E1>(
        (if daemon then NodeKind.ForkDaemonEffect else NodeKind.ForkEffect),
        effect,
        ForkOps<'A, 'E, 'E1>.Instance)

    do
        if obj.ReferenceEquals(effect, null) then
            raiseNullEffect ()

    member _.Fiber = fiber

and [<Sealed>] internal ForkOps<'A, 'E, 'E1> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: ForkOps<'A, 'E, 'E1>

    static member Instance =
        let current = ForkOps<'A, 'E, 'E1>.instance
        if obj.ReferenceEquals(current, null) then
            let created = ForkOps<'A, 'E, 'E1>()
            ForkOps<'A, 'E, 'E1>.instance <- created
            created
        else
            current

    override _.ForkTarget node =
        let fiber = (cast<ForkNode<'A, 'E, 'E1>> node).Fiber
        struct (box fiber, fiber.Context)

and [<Sealed>] internal JoinFiberNode<'A, 'E>(fiberContext: FiberContext) =
    inherit FIO<'A, 'E>(NodeKind.JoinFiber, fiberContext, null)

and [<Sealed>] internal JoinFirstNode<'E>(fiberContexts: FiberContext list) =
    inherit FIO<int, 'E>(NodeKind.JoinFirst, fiberContexts, null)

and [<Sealed>] internal JoinAllFailFastNode<'E>(fiberContexts: FiberContext[]) =
    inherit FIO<int voption, 'E>(NodeKind.JoinAllFailFast, fiberContexts, null)

and [<Sealed>] internal AwaitTaskNode<'A, 'E>(task: Task<obj>, onError: exn -> 'E) =
    inherit FIO<'A, 'E>(NodeKind.AwaitTask, task, AwaitTaskOps<'A, 'E>.Instance)

    member _.OnError = onError

and [<Sealed>] internal AwaitTaskOps<'A, 'E> private () =
    inherit NodeOps()

    [<DefaultValue>]
    static val mutable private instance: AwaitTaskOps<'A, 'E>

    static member Instance =
        let current = AwaitTaskOps<'A, 'E>.instance
        if obj.ReferenceEquals(current, null) then
            let created = AwaitTaskOps<'A, 'E>()
            AwaitTaskOps<'A, 'E>.instance <- created
            created
        else
            current

    override _.OnError (node, ex) =
        box ((cast<AwaitTaskNode<'A, 'E>> node).OnError ex)

and [<Sealed; AbstractClass>] internal NullSuccess<'A, 'E> private () =
    static member val Instance : FIO<'A, 'E> = SuccessNode<'A, 'E>(Unchecked.defaultof<'A>)
