namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent
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

[<Struct>]
type internal PostFinalizerSaved =
    | PostFinalizerSucceeded of value: obj
    | PostFinalizerFailed of error: obj
    | PostFinalizerInterrupted of error: obj

and [<Struct>] internal Cont =
    // Either handler may be null: a FlatMap has no failure handler, a CatchAll no success handler.
    | ChainCont of successCont: (obj -> FIO<obj, obj>) * failureCont: (obj -> FIO<obj, obj>)
    | FinalizerCont of finalizer: FIO<obj, obj>
    | PostFinalizerCont of saved: PostFinalizerSaved * level: int
    | RestoreSuppressionCont of level: int
    // Its fields share ChainCont's and RestoreSuppressionCont's storage, so Cont does not grow.
    | AcquiredCont of successCont: (obj -> FIO<obj, obj>) * level: int

and internal WorkItem =
    {
        mutable Effect: FIO<obj, obj>
        mutable FiberContext: FiberContext
        mutable ContStack: Stack<Cont>
        mutable InterruptionSuppressed: int
    }

and [<Sealed>] internal BlockingWaiter(workItem: WorkItem) =

    let mutable claimed = 0

    let mutable registration: CancellationTokenRegistration = Unchecked.defaultof<CancellationTokenRegistration>

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

and internal BlockingItem =
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

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

    member internal _.ReadAsync () =
        reader.ReadAsync()

    member internal _.TryRead (value: byref<'A>) =
        reader.TryRead &value

    member internal _.WaitToReadAsync (cancellationToken: CancellationToken) =
        reader.WaitToReadAsync cancellationToken

    member internal _.Clear () =
        let mutable value = Unchecked.defaultof<'A>
        while reader.TryRead &value do
            ()

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
    let id = Guid.NewGuid()
    let mutable state = int FiberContextState.Running

    [<VolatileField>]
    let mutable blockingWorkItemQueue: MailboxQueue<BlockingWaiter> = null

    let resultSource =
        TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously

    let cancelSource = new CancellationTokenSource()

    [<VolatileField>]
    let mutable registrations: ConcurrentBag<IDisposable> = null

    // One signal that interrupts every scoped child. Deliberately separate from cancelSource, whose
    // token is handed to user code by FIO.cancellationToken: a fiber finishing normally must not
    // present itself as cancelled.
    [<VolatileField>]
    let mutable childScope: CancellationTokenSource = null

    // The scope of children forked while this fiber was uninterruptible (a finalizer, say). It is
    // cancelled when the fiber exits, not when it is interrupted, so the work such a region forks
    // (a Timeout, a ZipPar) is not cut short by the interruption the region defers.
    [<VolatileField>]
    let mutable protectedScope: CancellationTokenSource = null

    let cancelScope (source: CancellationTokenSource) =
        match source with
        | null -> ()
        | source ->
            try
                source.Cancel(throwOnFirstException = false)
            with :? ObjectDisposedException ->
                ()

    // Set on a scoped child so it can tell its parent it has finished unwinding.
    [<VolatileField>]
    let mutable parent: FiberContext = null

    // Scoped children that have not yet unwound.
    [<VolatileField>]
    let mutable outstanding = 0

    // This fiber's effect has finished and a result is waiting on its children.
    [<VolatileField>]
    let mutable completing = 0

    [<VolatileField>]
    let mutable published = 0

    let mutable pendingValue = Unchecked.defaultof<Result<obj, obj>>

    [<VolatileField>]
    let mutable pendingQueue: MailboxQueue<WorkItem> = null

    [<VolatileField>]
    let mutable disposed = 0

    [<VolatileField>]
    let mutable onTerminalCallback: (unit -> unit) voption = ValueNone

    [<VolatileField>]
    let mutable onTerminalFired = 0

    [<VolatileField>]
    let mutable onUnwoundCallback: Action<FiberContext> = null

    member internal _.Id =
        id

    member internal _.Task =
        resultSource.Task

    member internal _.CancellationToken =
        cancelSource.Token

    member internal this.SetOnTerminal (callback: unit -> unit) =
        onTerminalCallback <- ValueSome callback
        if this.IsTerminal() then
            this.InvokeOnTerminal()

    // Runs once this fiber and its scoped subtree have fully unwound, which for an interrupted fiber is
    // later than its published result. The barrier orders the write before the read of `published`.
    member internal this.SetOnUnwound (callback: Action<FiberContext>) =
        onUnwoundCallback <- callback
        Interlocked.MemoryBarrier()
        if Volatile.Read &published = 1 then
            this.InvokeOnUnwound()

    member internal this.AddBlockingWorkItem (waiter: BlockingWaiter) =
        let queue = this.GetOrCreateBlockingQueue()
        queue.WriteAsync waiter

    member internal _.RescheduleBlockingWorkItems (activeWorkItemQueue: MailboxQueue<WorkItem>) =
        task {
            let queue = Volatile.Read &blockingWorkItemQueue
            if not (isNull queue) then
                let mutable waiter = Unchecked.defaultof<_>
                while queue.TryRead &waiter do
                    if waiter.TryClaim() then
                        do! activeWorkItemQueue.WriteAsync waiter.WorkItem
        }

    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemQueue: MailboxQueue<WorkItem>) =
        if not (this.IsTerminal()) then
            ValueTask<bool> false
        else
            ValueTask<bool>(task {
                do! this.RescheduleBlockingWorkItems activeWorkItemQueue
                return true
            })

    member internal _.ChildScopeToken =
        (initIfNull &childScope (fun () -> new CancellationTokenSource())).Token

    member internal _.ProtectedChildScopeToken =
        (initIfNull &protectedScope (fun () -> new CancellationTokenSource())).Token

    member internal _.RegisterChild () =
        Interlocked.Increment &outstanding |> ignore

    member internal _.AttachTo (newParent: FiberContext) =
        parent <- newParent
        newParent.RegisterChild()

    member private _.CancelChildScope () =
        cancelScope (Volatile.Read &childScope)

    member internal this.AddRegistration (registration: IDisposable) =
        let bag = initIfNull &registrations (fun () -> ConcurrentBag<IDisposable>())
        bag.Add registration
        if this.IsTerminal() then
            let mutable victim = Unchecked.defaultof<_>
            while bag.TryTake &victim do
                try
                    victim.Dispose()
                with :? ObjectDisposedException ->
                    ()

    member internal _.IsCompleted () =
        Volatile.Read &state = int FiberContextState.Completed

    member internal _.IsInterrupted () =
        Volatile.Read &state = int FiberContextState.Interrupted

    member internal _.IsTerminal () =
        Volatile.Read &state <> int FiberContextState.Running

    member private this.Publish value =
        transitionFrom &state (int FiberContextState.Running) (int FiberContextState.Completed) |> ignore
        this.DisposeRegistrations()
        resultSource.TrySetResult value |> ignore
        this.InvokeOnTerminal()

        match Volatile.Read &pendingQueue with
        | null -> ()
        | queue ->
            let blocked = Volatile.Read &blockingWorkItemQueue
            if not (isNull blocked) && blocked.Count > 0 then
                this.RescheduleBlockingWorkItems queue |> ignore

        match Volatile.Read &parent with
        | null -> ()
        | scope -> scope.OnChildUnwound()

        this.InvokeOnUnwound()

    member private this.TryFinish () =
        if Volatile.Read &completing = 1
           && Volatile.Read &outstanding = 0
           && tryClaim &published then
            this.Publish pendingValue

    member private this.OnChildUnwound () =
        Interlocked.Decrement &outstanding |> ignore
        this.TryFinish()

    member internal this.Complete value =
        this.CompleteInternal(value, null)

    member internal this.CompleteAndReschedule (value, activeWorkItemQueue) =
        this.CompleteInternal(value, activeWorkItemQueue)
        ValueTask.CompletedTask

    member private this.CompleteInternal (value, activeWorkItemQueue: MailboxQueue<WorkItem>) =
        pendingValue <- value
        Volatile.Write(&pendingQueue, activeWorkItemQueue)

        if Interlocked.Exchange(&completing, 1) = 0 then
            this.CancelChildScope()
            cancelScope (Volatile.Read &protectedScope)
            this.TryFinish()

    member internal this.Interrupt (?cause, ?message) =
        let cause = defaultArg cause ExplicitInterrupt
        let message = defaultArg message "Fiber was interrupted."
        if Volatile.Read &completing = 0
           && tryTransition &state (int FiberContextState.Running) (int FiberContextState.Interrupted) then
            let interruptError = Error(FiberInterruptedException(id, cause, message) :> obj)
            resultSource.TrySetResult interruptError |> ignore
            cancelSource.Cancel(throwOnFirstException = false)
            this.CancelChildScope()
            this.DisposeRegistrations()
            this.InvokeOnTerminal()

    member internal _.Cancel () =
        try
            cancelSource.Cancel(throwOnFirstException = false)
        with :? ObjectDisposedException ->
            ()

    member private _.InvokeOnTerminal () =
        match onTerminalCallback with
        | ValueSome callback when tryClaim &onTerminalFired ->
            try callback ()
            with _ -> ()
        | _ -> ()

    // Taking the callback out is what makes it run once, whether the publisher or SetOnUnwound gets here first.
    member private this.InvokeOnUnwound () =
        match Interlocked.Exchange(&onUnwoundCallback, null) with
        | null -> ()
        | callback ->
            try callback.Invoke this
            with _ -> ()

    member private _.GetOrCreateBlockingQueue () : MailboxQueue<BlockingWaiter> =
        initIfNull &blockingWorkItemQueue (fun () -> MailboxQueue<BlockingWaiter>())

    member private _.DisposeRegistrations () =
        let bag = Volatile.Read &registrations
        if not (isNull bag) then
            let mutable registration = Unchecked.defaultof<_>
            while bag.TryTake &registration do
                try
                    registration.Dispose()
                with :? ObjectDisposedException ->
                    ()

    member private _.Dispose disposing =
        if tryClaim &disposed then
            if disposing then
                cancelSource.Dispose()

                match Volatile.Read &childScope with
                | null -> ()
                | source -> source.Dispose()

                match Volatile.Read &protectedScope with
                | null -> ()
                | source -> source.Dispose()

    override this.Finalize () =
        this.Dispose false

    interface IDisposable with

        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

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

/// A running fiber (green thread) executing an effect. Join, await, interrupt, or poll it for its result.
and [<Sealed>] Fiber<'A, 'E> internal () =
    let fiberContext = new FiberContext()

    /// This fiber's unique identifier.
    member _.Id =
        fiberContext.Id

    /// A cancellation token tied to this fiber's lifetime; cancelled when the fiber is interrupted.
    member _.CancellationToken =
        fiberContext.CancellationToken

    /// Returns a task that completes with this fiber's result, for interop with task-based code.
    member _.Task () =
        task {
            match! fiberContext.Task with
            | Ok value ->
                return Succeeded(value :?> 'A)
            | Error error ->
                match error with
                | :? FiberInterruptedException as ex ->
                    return Interrupted ex
                | _ ->
                    return Failed(error :?> 'E)
        }

    /// Returns an effect that waits for this fiber to complete and yields its success value.
    member _.Join () : FIO<'A, 'E> =
        JoinFiber fiberContext

    /// Returns an effect that interrupts this fiber with the given cause and message.
    member _.Interrupt (cause: InterruptionCause) (message: string) : FIO<unit, 'E> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)

    /// Returns an effect that interrupts this fiber with an explicit-interrupt cause.
    member this.InterruptNow () : FIO<unit, 'E> =
        this.Interrupt ExplicitInterrupt "Fiber interrupted"

    /// Returns an effect that waits for this fiber and yields its full result (success, failure, or interruption).
    member this.Await<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        AwaitTask(boxTask (this.Task()), Rethrow<_>.Instance)

    /// Returns an effect that interrupts this fiber, then waits for and yields its result.
    member this.InterruptAwait<'E2> (cause: InterruptionCause) (message: string) : FIO<FiberResult<'A, 'E>, 'E2> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)
            .FlatMap <| fun () -> this.Await()

    /// Returns an effect that interrupts this fiber with an explicit-interrupt cause, then yields its result.
    member this.InterruptAwaitNow<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        this.InterruptAwait ExplicitInterrupt "Fiber interrupted"

    /// Returns an effect that yields this fiber's result if it has completed, or None if it is still running.
    member _.Poll<'E2> () : FIO<FiberResult<'A, 'E> option, 'E2> =
        Action((fun () ->
            if not (fiberContext.IsTerminal()) then
                None
            else
                match fiberContext.Task.Result with
                | Ok value ->
                    Some(Succeeded(value :?> 'A))
                | Error error ->
                    match error with
                    | :? FiberInterruptedException as ex ->
                        Some(Interrupted ex)
                    | _ ->
                        Some(Failed(error :?> 'E))),
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
    member _.IsCompleted () =
        fiberContext.IsCompleted()

    /// Returns true if this fiber was interrupted.
    member _.IsInterrupted () =
        fiberContext.IsInterrupted()

    /// Returns true if this fiber is no longer running — either completed or interrupted.
    member _.IsTerminal () =
        fiberContext.IsTerminal()

    /// Blocks the calling thread until this fiber completes and returns its result. Prefer Await inside effects.
    member this.UnsafeResult () =
        this.Task()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    /// Blocks the calling thread until this fiber completes and returns its success value, raising if it failed or was interrupted.
    member this.UnsafeSuccess () =
        match this.UnsafeResult() with
        | Succeeded value ->
            value
        | Failed error ->
            raise (InvalidOperationException $"Fiber failed with error: {error}")
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Blocks the calling thread until this fiber completes and returns its error, raising if it succeeded or was interrupted.
    member this.UnsafeError () =
        match this.UnsafeResult() with
        | Succeeded value ->
            raise (InvalidOperationException $"Fiber succeeded with value: {value}")
        | Failed error ->
            error
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Blocks the calling thread until this fiber completes and prints its result.
    member this.UnsafePrintResult () =
        printfn "%A" (this.UnsafeResult())

    member internal _.Context =
        fiberContext

    override this.ToString () =
        this.Id.ToString()

    interface IDisposable with

        member _.Dispose () =
            (fiberContext :> IDisposable).Dispose()

/// A typed, asynchronous channel for passing messages between fibers.
and [<Sealed; AllowNullLiteral>] Channel<'A> private
    (id: Guid,
    valueQueue: MailboxQueue<obj>,
    blockingSlot: BlockingWorkItemSlot,
    mode: ChannelMode) =
    [<VolatileField>]
    let mutable upcastChannel: Channel<obj> = null

    static let bounded (capacity: int) (fullMode: BoundedChannelFullMode) =
        if capacity < 1 then
            raise (ArgumentOutOfRangeException(nameof capacity, capacity, "A channel's capacity must be at least 1."))

        MailboxQueue<obj>.Bounded(capacity, fullMode)

    /// Creates a new, empty channel with no capacity limit.
    new() = Channel(Guid.NewGuid(), MailboxQueue<obj>(), BlockingWorkItemSlot(), Unbounded)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel suspends until a message is read.
    static member Bounded (capacity: int) =
        Channel(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.Wait, BlockingWorkItemSlot(), Bounded)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel drops the new message.
    static member Dropping (capacity: int) =
        Channel(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.Wait, BlockingWorkItemSlot(), Dropping)

    /// Creates a new, empty channel holding at most the given number of messages; a write to a full channel drops the oldest message.
    static member Sliding (capacity: int) =
        Channel(Guid.NewGuid(), bounded capacity BoundedChannelFullMode.DropOldest, BlockingWorkItemSlot(), Sliding)

    /// This channel's unique identifier.
    member _.Id =
        id

    /// The number of messages currently buffered in this channel.
    member _.Count =
        valueQueue.Count

    /// Returns an effect that writes a message to this channel, yielding the written message; a write to a full bounded channel suspends until a message is read.
    member this.Write<'E> (message: 'A) : FIO<'A, 'E> =
        WriteChan(box message, this.Upcast(), false)

    /// Returns an effect that writes a message to this channel only if it has room now, yielding whether it did; it never suspends.
    member this.TryWrite<'E> (message: 'A) : FIO<bool, 'E> =
        WriteChan(box message, this.Upcast(), true)

    /// Returns an effect that reads the next message from this channel, suspending the fiber until one is available.
    member this.Read<'E> () : FIO<'A, 'E> =
        ReadChan this

    member internal _.ReadAsync () =
        let valueTask = valueQueue.ReadAsync()
        if valueTask.IsCompletedSuccessfully then
            ValueTask<'A>(valueTask.Result :?> 'A)
        else
            ValueTask<'A>(task {
                let! value = valueTask
                return value :?> 'A
            })

    member internal _.BlockingWorkItemCount =
        blockingSlot.Count

    member internal _.AddBlockingWorkItem (waiter: BlockingWaiter) =
        let queue = blockingSlot.GetOrCreate()
        queue.WriteAsync waiter

    member internal _.TryDequeueBlockingWorkItem (workItem: byref<WorkItem>) : bool =
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

/// A lazy, type-safe description of an effect that, when run, either succeeds with a value or fails with a typed error.
and FIO<'A, 'E> =
    internal
    | Success of value: 'A
    | Failure of error: 'E
    | Interrupt of cause: InterruptionCause * message: string
    | Action of func: (unit -> 'A) * onError: (exn -> 'E)
    | WriteChan of message: obj * channel: Channel<obj> * reportAccepted: bool
    | ReadChan of channel: Channel<'A>
    | ForkEffect of effect: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext * daemon: bool
    | JoinFiber of fiberContext: FiberContext
    | JoinFirst of fiberContexts: FiberContext list
    | JoinAllFailFast of fiberContexts: FiberContext[]
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of effect: FIO<obj, 'E> * cont: (obj -> FIO<'A, 'E>)
    | ChainError of effect: FIO<'A, obj> * cont: (obj -> FIO<'A, 'E>)
    | ChainBoth of effect: FIO<obj, obj> * successCont: (obj -> FIO<'A, 'E>) * errorCont: (obj -> FIO<'A, 'E>)
    | OnFinalize of effect: FIO<'A, 'E> * finalizer: FIO<obj, obj>
    | FiberCancellationToken
    | Suspend of thunk: (unit -> FIO<'A, 'E>)
    | WithSuppression of update: (int -> int) * body: (int -> FIO<'A, 'E>)
    | AcquireRelease of acquire: FIO<obj, 'E> * onAcquired: (obj -> FIO<'A, 'E>)

    /// Returns an effect that passes this effect's success value into the given function.
    member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        ChainSuccess(this.UpcastResult(), fun value -> cont (value :?> 'A))

    /// Returns an effect that recovers from this effect's error with the given handler.
    member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        ChainError(this.UpcastError(), fun error -> onError (error :?> 'E))

    /// Returns an effect that runs the given finalizer after this effect on success, failure, and interruption alike.
    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// Returns an effect that runs this effect on a new fiber, yielding the fiber immediately.
    /// The forked fiber is scoped to this one: it is interrupted when this fiber is interrupted or
    /// finishes (only when it finishes, if forked while this fiber was uninterruptible, e.g. in a
    /// finalizer), and this fiber does not finish until it has unwound, so every finalizer in the
    /// subtree has run before a completed fiber's result becomes observable. Await the child here if
    /// you need its result. Use ForkDaemon for a fiber that should outlive its parent.
    member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        Suspend(fun () ->
            let fiber = new Fiber<'A, 'E>()
            ForkEffect(this.UpcastBoth(), fiber, fiber.Context, false))

    /// Returns an effect that runs this effect on a new unscoped fiber, yielding the fiber immediately.
    /// Unlike Fork, the forked fiber is independent of this one: it is neither interrupted nor awaited
    /// when this fiber finishes, so its lifetime — and its finalizers — become the caller's to manage.
    member this.ForkDaemon<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        Suspend(fun () ->
            let fiber = new Fiber<'A, 'E>()
            ForkEffect(this.UpcastBoth(), fiber, fiber.Context, true))

    /// Returns an effect that applies the given function to this effect's success value.
    member this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> =
        this.FlatMap <| fun value -> Success (mapper value)

    /// Returns an effect that applies the given function to this effect's error.
    member this.MapError<'E1> (mapper: 'E -> 'E1) : FIO<'A, 'E1> =
        this.CatchAll <| fun error -> Failure (mapper error)

    /// Returns an effect that maps this effect's success value and error with the two given functions.
    member this.MapBoth<'A1, 'E1> (successMapper: 'A -> 'A1) (errorMapper: 'E -> 'E1) : FIO<'A1, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (successMapper (value :?> 'A))),
            (fun error -> Failure (errorMapper (error :?> 'E))))

    /// Returns an effect that always succeeds, capturing this effect's outcome as a Result.
    member this.Result<'E1> () : FIO<Result<'A, 'E>, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Ok (value :?> 'A))),
            (fun error -> Success (Error (error :?> 'E))))

    /// Returns an effect that always succeeds, yielding Some on success and None on failure.
    member this.Option<'E1> () : FIO<'A option, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Some (value :?> 'A))),
            (fun _ -> Success (None : 'A option)))

    /// Returns an effect that always succeeds, capturing this effect's outcome as a Choice.
    member this.Choice<'E1> () : FIO<Choice<'A, 'E>, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Choice1Of2 (value :?> 'A))),
            (fun error -> Success (Choice2Of2 (error :?> 'E))))

    static member inline private flattenOnFinalize
        (leafUpcast: FIO<'A, 'E> -> FIO<'OR, 'OE>)
        (effect: FIO<'A, 'E>)
        (outerFinalizer: FIO<obj, obj>)
        : FIO<'OR, 'OE> =
        match effect with
        | OnFinalize _ ->
            let finalizers = ResizeArray<FIO<obj, obj>>()
            finalizers.Add outerFinalizer
            let mutable current = effect
            let mutable stopped = false

            while not stopped do
                match current with
                | OnFinalize(innerEff, innerFin) ->
                    finalizers.Add innerFin
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = leafUpcast current
            for i = finalizers.Count - 1 downto 0 do
                rebuilt <- OnFinalize(rebuilt, finalizers[i])

            rebuilt
        | _ ->
            OnFinalize(leafUpcast effect, outerFinalizer)

    static member inline private flattenChainSuccess
        (leafUpcast: FIO<obj, 'E> -> FIO<obj, 'OE>)
        (innerContWrap: (obj -> FIO<obj, 'E>) -> (obj -> FIO<obj, 'OE>))
        (outerContWrap: (obj -> FIO<'A, 'E>) -> (obj -> FIO<'OR, 'OE>))
        (effect: FIO<obj, 'E>)
        (outerCont: obj -> FIO<'A, 'E>)
        : FIO<'OR, 'OE> =
        match effect with
        | ChainSuccess _ ->
            let innerConts = ResizeArray<obj -> FIO<obj, 'E>>()
            let mutable current = effect
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainSuccess(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = leafUpcast current
            for i = innerConts.Count - 1 downto 0 do
                rebuilt <- ChainSuccess(rebuilt, innerContWrap innerConts[i])

            ChainSuccess(rebuilt, outerContWrap outerCont)
        | _ ->
            ChainSuccess(leafUpcast effect, outerContWrap outerCont)

    static member inline private flattenChainError
        (leafUpcast: FIO<'A, obj> -> FIO<'OR, obj>)
        (innerContWrap: (obj -> FIO<'A, obj>) -> (obj -> FIO<'OR, obj>))
        (outerContWrap: (obj -> FIO<'A, 'E>) -> (obj -> FIO<'OR, 'OE>))
        (effect: FIO<'A, obj>)
        (outerCont: obj -> FIO<'A, 'E>)
        : FIO<'OR, 'OE> =
        match effect with
        | ChainError _ ->
            let innerConts = ResizeArray<obj -> FIO<'A, obj>>()
            let mutable current = effect
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainError(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = leafUpcast current
            for i = innerConts.Count - 1 downto 0 do
                rebuilt <- ChainError(rebuilt, innerContWrap innerConts[i])

            ChainError(rebuilt, outerContWrap outerCont)
        | _ ->
            ChainError(leafUpcast effect, outerContWrap outerCont)

    member internal this.UpcastResult () : FIO<obj, 'E> =
        match this with
        | Success value ->
            Success(value :> obj)
        | Failure error ->
            Failure error
        | Interrupt(cause, message) ->
            Interrupt(cause, message)
        | Action(func, onError) ->
            Action(boxFunc func, onError)
        | WriteChan(message, channel, reportAccepted) ->
            WriteChan(message, channel, reportAccepted)
        | ReadChan channel ->
            ReadChan(channel.Upcast())
        | ForkEffect(effect, fiber, fiberContext, daemon) ->
            ForkEffect(effect, fiber, fiberContext, daemon)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | JoinFirst fiberContexts ->
            JoinFirst fiberContexts
        | JoinAllFailFast fiberContexts ->
            JoinAllFailFast fiberContexts
        | AwaitTask(task, onError) ->
            AwaitTask(task, onError)
        | ChainSuccess(effect, cont) ->
            ChainSuccess(effect, fun value -> cont(value).UpcastResult())
        | ChainError(effect, cont) ->
            FIO.flattenChainError
                (fun effect' -> effect'.UpcastResult())
                (fun innerConts -> fun error -> (innerConts error).UpcastResult())
                (fun outerConts -> fun error -> (outerConts error).UpcastResult())
                effect cont
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastResult()),
                (fun error -> (errorCont error).UpcastResult()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize
                (fun effect' -> effect'.UpcastResult())
                effect
                finalizer
        | FiberCancellationToken ->
            FiberCancellationToken
        | Suspend thunk ->
            Suspend(fun () -> (thunk()).UpcastResult())
        | WithSuppression(update, body) ->
            WithSuppression(update, fun level -> (body level).UpcastResult())
        | AcquireRelease(acquire, onAcquired) ->
            AcquireRelease(acquire, fun resource -> (onAcquired resource).UpcastResult())

    member internal this.UpcastError () : FIO<'A, obj> =
        match this with
        | Success value ->
            Success value
        | Failure error ->
            Failure(error :> obj)
        | Interrupt(cause, message) ->
            Interrupt(cause, message)
        | Action(func, onError) ->
            Action(func, boxOnError onError)
        | WriteChan(message, channel, reportAccepted) ->
            WriteChan(message, channel, reportAccepted)
        | ReadChan channel ->
            ReadChan channel
        | ForkEffect(effect, fiber, fiberContext, daemon) ->
            ForkEffect(effect, fiber, fiberContext, daemon)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | JoinFirst fiberContexts ->
            JoinFirst fiberContexts
        | JoinAllFailFast fiberContexts ->
            JoinAllFailFast fiberContexts
        | AwaitTask(task, onError) ->
            AwaitTask(task, boxOnError onError)
        | ChainSuccess(effect, cont) ->
            FIO.flattenChainSuccess
                (fun effect' -> effect'.UpcastError())
                (fun innerConts -> fun value -> (innerConts value).UpcastError())
                (fun outerConts -> fun value -> (outerConts value).UpcastError())
                effect cont
        | ChainError(effect, cont) ->
            ChainError(effect, fun error -> cont(error).UpcastError())
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastError()),
                (fun error -> (errorCont error).UpcastError()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize
                (fun effect' -> effect'.UpcastError())
                effect
                finalizer
        | FiberCancellationToken ->
            FiberCancellationToken
        | Suspend thunk ->
            Suspend(fun () -> (thunk()).UpcastError())
        | WithSuppression(update, body) ->
            WithSuppression(update, fun level -> (body level).UpcastError())
        | AcquireRelease(acquire, onAcquired) ->
            AcquireRelease(acquire.UpcastError(), fun resource -> (onAcquired resource).UpcastError())

    member internal this.UpcastBoth () : FIO<obj, obj> =
        match this with
        | Success value ->
            Success(value :> obj)
        | Failure error ->
            Failure(error :> obj)
        | Interrupt(cause, message) ->
            Interrupt(cause, message)
        | Action(func, onError) ->
            Action(boxFunc func, boxOnError onError)
        | WriteChan(message, channel, reportAccepted) ->
            WriteChan(message, channel, reportAccepted)
        | ReadChan channel ->
            ReadChan(channel.Upcast())
        | ForkEffect(effect, fiber, fiberContext, daemon) ->
            ForkEffect(effect, fiber, fiberContext, daemon)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | JoinFirst fiberContexts ->
            JoinFirst fiberContexts
        | JoinAllFailFast fiberContexts ->
            JoinAllFailFast fiberContexts
        | AwaitTask(task, onError) ->
            AwaitTask(task, boxOnError onError)
        | ChainSuccess(effect, cont) ->
            FIO.flattenChainSuccess
                (fun effect' -> effect'.UpcastError())
                (fun innerConts -> fun value -> (innerConts value).UpcastError())
                (fun outerConts -> fun value -> (outerConts value).UpcastBoth())
                effect cont
        | ChainError(effect, cont) ->
            FIO.flattenChainError
                (fun effect' -> effect'.UpcastResult())
                (fun innerConts -> fun error -> (innerConts error).UpcastResult())
                (fun outerConts -> fun error -> (outerConts error).UpcastBoth())
                effect cont
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastBoth()),
                (fun error -> (errorCont error).UpcastBoth()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize
                (fun effect' -> effect'.UpcastBoth())
                effect
                finalizer
        | FiberCancellationToken ->
            FiberCancellationToken
        | Suspend thunk ->
            Suspend(fun () -> (thunk()).UpcastBoth())
        | WithSuppression(update, body) ->
            WithSuppression(update, fun level -> (body level).UpcastBoth())
        | AcquireRelease(acquire, onAcquired) ->
            AcquireRelease(acquire.UpcastError(), fun resource -> (onAcquired resource).UpcastBoth())
