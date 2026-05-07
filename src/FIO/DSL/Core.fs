/// <summary>Provides the core types and primitives of the FIO effect system.</summary>
[<AutoOpen>]
module FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

/// <summary>Represents a continuation used to chain effect evaluations.</summary>
type internal Cont =
    /// <summary>Represents a success continuation that transforms the current result into the next effect.</summary>
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    /// <summary>Represents a failure continuation that recovers from a typed error by producing the next effect.</summary>
    | FailureCont of cont: (obj -> FIO<obj, obj>)
    /// <summary>Represents a finalizer continuation whose effect runs after the guarded effect completes, fails, or is interrupted.</summary>
    | FinalizerCont of finalizer: FIO<obj, obj>

/// <summary>Represents a single stack entry wrapping a <c>Cont</c> value.</summary>
and [<Struct>] internal ContStackFrame =
    /// <summary>Returns the continuation held by this frame.</summary>
    val Cont: Cont

    /// <summary>Creates a frame wrapping the specified continuation.</summary>
    /// <param name="cont">The continuation to store.</param>
    new cont = { Cont = cont }

/// <summary>Represents the stack of continuations for an in-progress effect evaluation.</summary>
and internal ContStack = Stack<ContStackFrame>

/// <summary>Represents a mutable work unit carrying an effect, its fiber context, continuation stack, and interruption state.</summary>
and internal WorkItem =
    {
        /// <summary>Represents the current effect to evaluate in this work item.</summary>
        mutable Eff: FIO<obj, obj>
        /// <summary>Represents the fiber context that owns this work item's execution.</summary>
        mutable FiberContext: FiberContext
        /// <summary>Represents the continuation stack of pending steps for this work item.</summary>
        mutable Stack: ContStack
        /// <summary>Represents the nesting depth of interruption suppression regions; interruption is suppressed when greater than zero.</summary>
        mutable InterruptionSuppressed: int
    }

/// <summary>Represents a work item that is blocked waiting on a channel message or a fiber's completion.</summary>
and internal BlockingItem =
    /// <summary>Represents a work item blocked on a channel receive, waiting for a message to arrive.</summary>
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    /// <summary>Represents a work item blocked on a fiber join, waiting for the fiber to reach a terminal state.</summary>
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

/// <summary>Represents the terminal state of a fiber as one of three mutually exclusive outcomes.</summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The typed error type.</typeparam>
and FiberResult<'R, 'E> =
    /// <summary>Represents successful completion carrying a result value.</summary>
    | Succeeded of result: 'R
    /// <summary>Represents failure carrying a typed error value.</summary>
    | Failed of error: 'E
    /// <summary>Represents interruption before completion, carrying the originating exception.</summary>
    | Interrupted of ex: FiberInterruptedException

/// <summary>Represents an unbounded message queue for passing values between fibers.</summary>
/// <typeparam name="'R">The element type carried by this channel.</typeparam>
/// <param name="id">The unique identifier for this channel instance.</param>
and [<Sealed; AllowNullLiteral>] internal UnboundedChannel<'R>(id: Guid) =
    let chan = Channel.CreateUnbounded<'R>()

    /// <summary>Creates an unbounded channel with a freshly generated unique identifier.</summary>
    new() = UnboundedChannel(Guid.NewGuid())

    /// <summary>Returns the unique identifier assigned to this channel.</summary>
    /// <returns>A <c>Guid</c> distinguishing this channel from all others.</returns>
    member internal _.Id = id

    /// <summary>Returns the number of items currently buffered in this channel.</summary>
    /// <returns>The count of items awaiting consumption.</returns>
    member internal _.Count = chan.Reader.Count

    /// <summary>Transforms the channel state by asynchronously adding a value to the buffer.</summary>
    /// <param name="msg">The message to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the message has been enqueued.</returns>
    member internal _.AddAsync msg = chan.Writer.WriteAsync msg

    /// <summary>Returns a value from the channel buffer, waiting asynchronously if empty.</summary>
    /// <returns>A <c>ValueTask</c> that completes with the next message.</returns>
    member internal _.TakeAsync() = chan.Reader.ReadAsync()

    /// <summary>Returns whether a message was successfully dequeued without waiting.</summary>
    /// <param name="res">When this method returns <c>true</c>, contains the dequeued message.</param>
    /// <returns><c>true</c> when a message was available and dequeued; <c>false</c> otherwise.</returns>
    member internal _.TryTake(res: byref<'R>) = chan.Reader.TryRead &res

    /// <summary>Returns a <c>ValueTask</c> that completes when data is available to read or the token is cancelled.</summary>
    /// <param name="ct">The cancellation token to observe.</param>
    /// <returns>A <c>ValueTask&lt;bool&gt;</c> that completes with <c>true</c> when data is available.</returns>
    member internal _.WaitToTakeAsync(ct: CancellationToken) = chan.Reader.WaitToReadAsync ct

    /// <summary>Transforms the channel by removing all buffered values.</summary>
    member internal _.Clear() =
        let mutable item = Unchecked.defaultof<'R>

        while chan.Reader.TryRead &item do
            ()

/// <summary>Represents a lazily-initialized slot holding an <c>UnboundedChannel&lt;WorkItem&gt;</c> for blocked work items.</summary>
and [<Sealed>] internal BlockingWorkItemSlot() =

    [<VolatileField>]
    let mutable chan: UnboundedChannel<WorkItem> = null

    /// <summary>Returns the existing channel or creates one if none exists yet.</summary>
    /// <returns>The <c>UnboundedChannel&lt;WorkItem&gt;</c> associated with this slot.</returns>
    member _.GetOrCreate() =
        let existing = Volatile.Read &chan

        if not (isNull existing) then
            existing
        else
            let created = UnboundedChannel<WorkItem>()
            let prev = Interlocked.CompareExchange(&chan, created, null)
            if isNull prev then created else prev

    /// <summary>Returns the channel if it has been created, or null if not yet initialized.</summary>
    /// <returns>The existing <c>UnboundedChannel&lt;WorkItem&gt;</c>, or null.</returns>
    member _.TryGet() = Volatile.Read &chan

    /// <summary>Returns the number of work items buffered in the channel, or zero if the channel has not been created.</summary>
    /// <returns>The count of buffered work items.</returns>
    member _.Count =
        let c = Volatile.Read &chan
        if isNull c then 0 else c.Count

/// <summary>Represents the lifecycle state of a fiber context as a tri-state enum.</summary>
and private FiberContextState =
    /// <summary>Represents a fiber that is still executing.</summary>
    | Running = 0
    /// <summary>Represents a fiber that finished with a success or typed error.</summary>
    | Completed = 1
    /// <summary>Represents a fiber that was interrupted before completion.</summary>
    | Interrupted = 2

/// <summary>Represents the internal execution state of a fiber, tracking completion, interruption, cancellation, and blocked work items.</summary>
and [<Sealed>] internal FiberContext() =
    let id = Guid.NewGuid()
    let mutable state = int FiberContextState.Running

    [<VolatileField>]
    let mutable blockingWorkItemChan: UnboundedChannel<WorkItem> = null

    let resTcs =
        TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously

    let cts = new CancellationTokenSource()

    [<VolatileField>]
    let mutable registrations: ConcurrentBag<IDisposable> = null

    [<VolatileField>]
    let mutable disposed = false

    [<VolatileField>]
    let mutable onTerminalCallback: (unit -> unit) voption = ValueNone

    /// <summary>Returns the unique identifier assigned to this fiber context.</summary>
    /// <returns>A <c>Guid</c> distinguishing this context from all others.</returns>
    member internal _.Id = id

    /// <summary>Returns the task that completes with the fiber's terminal <c>Result&lt;obj, obj&gt;</c>.</summary>
    /// <returns>A task carrying <c>Ok</c> for success or <c>Error</c> for failure or interruption.</returns>
    member internal _.Task = resTcs.Task

    /// <summary>Returns the cancellation token that is cancelled when this fiber is interrupted.</summary>
    /// <returns>A <c>CancellationToken</c> linked to this fiber's lifetime.</returns>
    member internal _.CancellationToken = cts.Token

    /// <summary>Transforms the fiber state by invoking the registered terminal callback with the given result.</summary>
    member private _.InvokeOnTerminal() =
        match onTerminalCallback with
        | ValueSome cb ->
            try
                cb ()
            with _ ->
                ()
        | ValueNone -> ()

    /// <summary>Transforms the fiber context by setting the callback to invoke when the fiber reaches a terminal state.</summary>
    /// <param name="callback">The callback to run on completion or interruption.</param>
    member internal this.SetOnTerminal(callback: unit -> unit) =
        onTerminalCallback <- ValueSome callback

        if this.IsTerminal() then
            try
                callback ()
            with _ ->
                ()

    /// <summary>Transforms the fiber into the completed state with a successful result.</summary>
    /// <param name="res">The <c>Result&lt;obj, obj&gt;</c> to record as the fiber's outcome.</param>
    member internal this.Complete res =
        if
            Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int
                    FiberContextState.Running
        then
            this.DisposeRegistrations()
            resTcs.TrySetResult res |> ignore
            this.InvokeOnTerminal()

    /// <summary>Transforms the fiber into the completed state and reschedules any blocked work items.</summary>
    /// <param name="res">The <c>Result&lt;obj, obj&gt;</c> to record as the fiber's outcome.</param>
    /// <param name="activeWorkItemChan">The channel to receive rescheduled work items.</param>
    /// <returns>A task that completes after the result is set and blocked work items are rescheduled.</returns>
    member internal this.CompleteAndReschedule(res, activeWorkItemChan) =
        task {
            let oldState =
                Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running)

            if oldState = int FiberContextState.Running then
                this.DisposeRegistrations()
                resTcs.TrySetResult res |> ignore
                this.InvokeOnTerminal()

                let chan = Volatile.Read &blockingWorkItemChan

                if not (isNull chan) && chan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
            elif oldState = int FiberContextState.Interrupted then
                this.DisposeRegistrations()
                resTcs.TrySetResult res |> ignore

                let chan = Volatile.Read &blockingWorkItemChan

                if not (isNull chan) && chan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    /// <summary>Transforms the fiber into the interrupted state with the given cause.</summary>
    /// <param name="cause">The reason for the interruption; defaults to <c>ExplicitInterrupt</c>.</param>
    /// <param name="msg">A human-readable description of the interruption.</param>
    member internal this.Interrupt(?cause, ?msg) =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."

        if
            Interlocked.CompareExchange(&state, int FiberContextState.Interrupted, int FiberContextState.Running) = int
                    FiberContextState.Running
        then
            cts.Cancel(throwOnFirstException = false)
            this.DisposeRegistrations()
            let interruptError = Error(FiberInterruptedException(id, cause, msg) :> obj)
            resTcs.TrySetResult interruptError |> ignore
            this.InvokeOnTerminal()

    /// <summary>Transforms the fiber context by adding a work item to the blocking queue.</summary>
    /// <param name="blockingWorkItem">The work item to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the work item has been enqueued.</returns>
    member internal this.AddBlockingWorkItem blockingWorkItem =
        let chan = this.GetOrCreateBlockingChan()
        chan.AddAsync blockingWorkItem

    /// <summary>Transforms the fiber context by moving all blocking work items to the active evaluation channel.</summary>
    /// <param name="activeWorkItemChan">The channel to receive the rescheduled work items.</param>
    /// <returns>A task that completes after all blocked work items have been moved.</returns>
    member internal _.RescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let chan = Volatile.Read &blockingWorkItemChan

            if not (isNull chan) then
                let mutable workItem = Unchecked.defaultof<_>

                while chan.TryTake &workItem do
                    do! activeWorkItemChan.AddAsync workItem
        }

    /// <summary>Returns the existing blocking channel or creates one if none exists yet.</summary>
    /// <returns>The <c>UnboundedChannel&lt;WorkItem&gt;</c> for this fiber's blocked work items.</returns>
    member private _.GetOrCreateBlockingChan() : UnboundedChannel<WorkItem> =
        let existing = Volatile.Read &blockingWorkItemChan

        if not (isNull existing) then
            existing
        else
            let created = UnboundedChannel<WorkItem>()
            let prev = Interlocked.CompareExchange(&blockingWorkItemChan, created, null)
            if isNull prev then created else prev

    /// <summary>Transforms the fiber context by attempting to move blocking work items to the evaluation channel.</summary>
    /// <param name="activeWorkItemChan">The channel to receive rescheduled work items.</param>
    /// <returns>A task completing with <c>true</c> when work items were rescheduled, or <c>false</c> when the fiber is still running.</returns>
    member internal this.TryRescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.IsTerminal() then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

    /// <summary>Transforms the fiber context by adding a cancellation registration for cleanup on interruption.</summary>
    /// <param name="registration">The disposable to track and dispose when the fiber terminates.</param>
    member internal this.AddRegistration(registration: IDisposable) =
        let mutable bag = Volatile.Read &registrations

        if isNull bag then
            let created = ConcurrentBag<IDisposable>()
            let existing = Interlocked.CompareExchange(&registrations, created, null)
            bag <- if isNull existing then created else existing

        bag.Add registration

        if this.IsTerminal() then
            let mutable victim = Unchecked.defaultof<_>

            while bag.TryTake &victim do
                try
                    victim.Dispose()
                with :? ObjectDisposedException ->
                    ()

    /// <summary>Returns whether the fiber has reached the completed state.</summary>
    /// <returns><c>true</c> when the fiber finished with a success or typed error.</returns>
    member internal _.IsCompleted() =
        Volatile.Read &state = int FiberContextState.Completed

    /// <summary>Returns whether the fiber has been interrupted.</summary>
    /// <returns><c>true</c> when the fiber's terminal state is interruption.</returns>
    member internal _.IsInterrupted() =
        Volatile.Read &state = int FiberContextState.Interrupted

    /// <summary>Returns whether the fiber has reached any terminal state.</summary>
    /// <returns><c>true</c> when the fiber has completed or been interrupted.</returns>
    member internal _.IsTerminal() =
        Volatile.Read &state <> int FiberContextState.Running

    /// <summary>Transforms the fiber context by cancelling the associated cancellation token source.</summary>
    member internal _.CancelToken() =
        try
            cts.Cancel(throwOnFirstException = false)
        with :? ObjectDisposedException ->
            ()

    /// <summary>Transforms the fiber context by disposing all registered cancellation registrations.</summary>
    member private _.DisposeRegistrations() =
        let bag = Volatile.Read &registrations

        if not (isNull bag) then
            let mutable registration = Unchecked.defaultof<_>

            while bag.TryTake &registration do
                try
                    registration.Dispose()
                with :? ObjectDisposedException ->
                    ()

    /// <summary>Transforms the fiber context by releasing all managed and unmanaged resources.</summary>
    /// <param name="disposing"><c>true</c> when called from <c>Dispose</c>; <c>false</c> from the finalizer.</param>
    member private _.Dispose disposing =
        if not disposed then
            disposed <- true

            if disposing then
                cts.Dispose()

    interface IDisposable with
        /// <summary>Returns unit, releasing the managed resources held by this fiber context.</summary>
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    /// <summary>Transforms the fiber context by releasing unmanaged resources during garbage collection.</summary>
    override this.Finalize() = this.Dispose false

/// <summary>Represents a lightweight, cooperative thread of execution that can be awaited for its result.</summary>
/// <typeparam name="'R">The success result type produced by the fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fiber may fail with.</typeparam>
and [<Sealed>] Fiber<'R, 'E> internal () =
    let fiberContext = new FiberContext()

    /// <summary>Returns the unique identifier assigned to this fiber.</summary>
    /// <returns>A <c>Guid</c> distinguishing this fiber from all others created in this process.</returns>
    member _.Id = fiberContext.Id

    /// <summary>Returns the cancellation token associated with this fiber's cooperative cancellation.</summary>
    /// <returns>A <c>CancellationToken</c> that is cancelled when this fiber is interrupted.</returns>
    member _.CancellationToken = fiberContext.CancellationToken

    /// <summary>Returns a .NET task that completes with this fiber's <c>FiberResult</c>.</summary>
    /// <returns>A task that completes with <c>Succeeded</c>, <c>Failed</c>, or <c>Interrupted</c> depending on the fiber's terminal state.</returns>
    member _.Task() =
        task {
            match! fiberContext.Task with
            | Ok res -> return Succeeded(res :?> 'R)
            | Error err ->
                match err with
                | :? FiberInterruptedException as ex -> return Interrupted ex
                | _ -> return Failed(err :?> 'E)
        }

    /// <summary>Builds an effect that awaits this fiber's completion and propagates its success or failure to the calling fiber.</summary>
    /// <returns>An effect that completes with the fiber's success value, fails with its typed error, or interrupts when the fiber is interrupted.</returns>
    member _.Join() : FIO<'R, 'E> = JoinFiber fiberContext

    /// <summary>Creates an effect that requests interruption of this fiber.</summary>
    /// <param name="cause">The reason for the interruption; defaults to <c>ExplicitInterrupt</c>.</param>
    /// <param name="msg">A human-readable description of the interruption; defaults to a generic message.</param>
    /// <returns>An effect that completes with unit once interruption has been requested; the fiber may not yet have terminated when the effect completes.</returns>
    member _.Interrupt(?cause: InterruptionCause, ?msg: string) : FIO<unit, 'E> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        InterruptFiber(cause, msg, fiberContext)

    /// <summary>Builds an effect that awaits this fiber and returns its <c>FiberResult</c> as a value, without re-raising errors or propagating interruptions.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because the await never fails.</typeparam>
    /// <returns>An effect that completes with the fiber's terminal state as a value.</returns>
    member this.Await<'E2>() : FIO<FiberResult<'R, 'E>, 'E2> =
        AwaitTask(upcastTask (this.Task()), fun ex -> raise ex)

    /// <summary>Builds an effect that interrupts this fiber and awaits its terminal result.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because the await never fails.</typeparam>
    /// <param name="cause">The reason for the interruption; defaults to <c>ExplicitInterrupt</c>.</param>
    /// <param name="msg">A human-readable description of the interruption; defaults to a generic message.</param>
    /// <returns>An effect that requests interruption and then completes with the fiber's terminal <c>FiberResult</c>.</returns>
    member this.InterruptAwait<'E2>(?cause: InterruptionCause, ?msg: string) : FIO<FiberResult<'R, 'E>, 'E2> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        Action((fun () -> fiberContext.Interrupt(cause, msg)), fun ex -> raise ex).FlatMap(fun () -> this.Await())

    /// <summary>Creates an effect that produces <c>Some result</c> when the fiber has terminated, or <c>None</c> when it is still running.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because polling never fails.</typeparam>
    /// <returns>An effect that completes with <c>Some</c> wrapping the terminal <c>FiberResult</c>, or <c>None</c> when the fiber is not yet terminal.</returns>
    member this.Poll<'E2>() : FIO<FiberResult<'R, 'E> option, 'E2> =
        Action(
            (fun () ->
                if not (fiberContext.IsTerminal()) then
                    None
                else
                    let t = fiberContext.Task

                    match t.Result with
                    | Ok res -> Some(Succeeded(res :?> 'R))
                    | Error err ->
                        match err with
                        | :? FiberInterruptedException as ex -> Some(Interrupted ex)
                        | _ -> Some(Failed(err :?> 'E))),
            fun ex -> raise ex
        )

    /// <summary>Combines this fiber's terminal outcome with three branch handlers, producing the effect from whichever branch matches.</summary>
    /// <typeparam name="'R1">The result type produced by every branch handler.</typeparam>
    /// <typeparam name="'E1">The error type produced by every branch handler.</typeparam>
    /// <param name="onSucceeded">A function from the fiber's success value to the effect to run when the fiber completed successfully.</param>
    /// <param name="onFailed">A function from the fiber's typed error to the effect to run when the fiber failed.</param>
    /// <param name="onInterrupted">A function from the interruption exception to the effect to run when the fiber was interrupted.</param>
    /// <returns>An effect that awaits this fiber and runs the handler matching its terminal state.</returns>
    member this.JoinWith<'R1, 'E1>
        (
            onSucceeded: 'R -> FIO<'R1, 'E1>,
            onFailed: 'E -> FIO<'R1, 'E1>,
            onInterrupted: FiberInterruptedException -> FIO<'R1, 'E1>
        ) : FIO<'R1, 'E1> =
        this
            .Await()
            .FlatMap(fun result ->
                match result with
                | Succeeded r -> onSucceeded r
                | Failed e -> onFailed e
                | Interrupted ex -> onInterrupted ex)

    /// <summary>Returns whether the fiber has reached the completed state.</summary>
    /// <returns><c>true</c> when the fiber finished with a success or a typed error; <c>false</c> while it is still running or has been interrupted.</returns>
    member _.IsCompleted() = fiberContext.IsCompleted()

    /// <summary>Returns whether the fiber has been interrupted.</summary>
    /// <returns><c>true</c> when the fiber's terminal state is interruption; <c>false</c> while running or after normal completion.</returns>
    member _.IsInterrupted() = fiberContext.IsInterrupted()

    /// <summary>Returns whether the fiber has reached any terminal state.</summary>
    /// <returns><c>true</c> when the fiber has completed or been interrupted; <c>false</c> while it is still running.</returns>
    member _.IsTerminal() = fiberContext.IsTerminal()

    /// <summary>Returns the fiber's <c>FiberResult</c> by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The terminal state of the fiber.</returns>
    /// <remarks>Prefer <c>Join</c> or <c>Task</c> in effectful code; this method exists for examples and tests.</remarks>
    member this.UnsafeResult() =
        this.Task() |> Async.AwaitTask |> Async.RunSynchronously

    /// <summary>Returns the fiber's success value by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The success value when the fiber completed successfully.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the fiber failed or was interrupted.</exception>
    member this.UnsafeSuccess() =
        match this.UnsafeResult() with
        | Succeeded res -> res
        | Failed err -> raise (InvalidOperationException $"Fiber failed with error: {err}")
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>Returns the fiber's typed error by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The typed error value when the fiber failed.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the fiber succeeded or was interrupted.</exception>
    member this.UnsafeError() =
        match this.UnsafeResult() with
        | Succeeded res -> raise (InvalidOperationException $"Fiber succeeded with result: {res}")
        | Failed err -> err
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>Returns unit after synchronously printing the fiber's terminal result to standard output.</summary>
    /// <returns>Unit, after the fiber's <c>FiberResult</c> has been printed.</returns>
    member this.UnsafePrintResult() = printfn "%A" (this.UnsafeResult())

    /// <summary>Returns the fiber's identifier as a string.</summary>
    /// <returns>The string form of <c>Id</c>.</returns>
    override this.ToString() = this.Id.ToString()

    /// <summary>Returns the internal <c>FiberContext</c> backing this fiber's execution state.</summary>
    /// <returns>The <c>FiberContext</c> instance associated with this fiber.</returns>
    member internal _.Context = fiberContext

    interface IDisposable with

        /// <summary>Returns unit, releasing the resources held by this fiber.</summary>
        member _.Dispose() = (fiberContext :> IDisposable).Dispose()

/// <summary>Represents a typed communication queue for sending and receiving messages between fibers.</summary>
/// <typeparam name="'R">The element type carried by this channel.</typeparam>
/// <param name="id">The unique identifier for this channel instance.</param>
/// <param name="resChan">The underlying unbounded channel used for value buffering.</param>
/// <param name="blockingSlot">The slot managing blocked work items awaiting values.</param>
and [<Sealed>] Channel<'R>
    private (id: Guid, resChan: UnboundedChannel<obj>, blockingSlot: BlockingWorkItemSlot) =
    let upcastLock = obj ()

    [<VolatileField>]
    let mutable upcastChan = None

    let mutable signalProcessing = 0

    /// <summary>Creates a new channel with a freshly generated unique identifier.</summary>
    new() = Channel(Guid.NewGuid(), UnboundedChannel<obj>(), BlockingWorkItemSlot())

    /// <summary>Returns the unique identifier assigned to this channel.</summary>
    /// <returns>A <c>Guid</c> distinguishing this channel from all others created in this process.</returns>
    member _.Id = id

    /// <summary>Returns the current number of messages buffered in this channel.</summary>
    /// <returns>The count of messages awaiting receivers.</returns>
    member _.Count = resChan.Count

    /// <summary>Builds an effect that sends a message into this channel.</summary>
    /// <param name="msg">The message to enqueue.</param>
    /// <returns>An effect that completes once the message has been enqueued.</returns>
    member this.Send<'E> msg : FIO<'R, 'E> = SendChan(msg, this)

    /// <summary>Builds an effect that receives the next message from this channel, blocking until one is available.</summary>
    /// <returns>An effect that completes with the next message, suspending the fiber while the channel is empty.</returns>
    member this.Receive<'E>() : FIO<'R, 'E> = ReceiveChan this

    /// <summary>Transforms the channel state by asynchronously adding a value to the underlying buffer.</summary>
    /// <param name="msg">The message to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the message has been enqueued.</returns>
    member internal _.SendAsync(msg: 'R) = resChan.AddAsync msg

    /// <summary>Returns the next value from the channel buffer, waiting asynchronously if empty.</summary>
    /// <returns>A task that completes with the next message, cast to the channel's element type.</returns>
    member internal _.ReceiveAsync() =
        task {
            let! res = resChan.TakeAsync()
            return res :?> 'R
        }

    /// <summary>Transforms the channel by adding a work item to the blocking slot for rescheduling on the next send.</summary>
    /// <param name="blockingItem">The work item to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the work item has been enqueued.</returns>
    member internal _.AddBlockingWorkItem blockingItem =
        let chan = blockingSlot.GetOrCreate()
        chan.AddAsync blockingItem

    /// <summary>Returns whether a blocked work item was successfully rescheduled from this channel's blocking slot.</summary>
    /// <param name="activeWorkItemChan">The channel to receive the rescheduled work item.</param>
    /// <returns>A task completing with <c>true</c> when a work item was rescheduled, or <c>false</c> when no blocked work items exist.</returns>
    member internal _.TryRescheduleNextBlockingWorkItem(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let chan = blockingSlot.TryGet()

            if isNull chan then
                return false
            else
                let mutable workItem = Unchecked.defaultof<_>

                if chan.TryTake &workItem then
                    do! activeWorkItemChan.AddAsync workItem
                    return true
                else
                    return false
        }

    /// <summary>Returns whether signal processing was successfully claimed for this channel.</summary>
    /// <returns><c>true</c> when this caller acquired the signal processing lock; <c>false</c> when another caller already holds it.</returns>
    member internal _.TryBeginSignalProcessing() =
        Interlocked.CompareExchange(&signalProcessing, 1, 0) = 0

    /// <summary>Transforms the channel's signal-processing state by marking it as available.</summary>
    member internal _.EndSignalProcessing() = Volatile.Write(&signalProcessing, 0)

    /// <summary>Returns the number of work items blocked waiting on this channel.</summary>
    /// <returns>The count of blocked work items in the blocking slot.</returns>
    member internal _.BlockingWorkItemCount = blockingSlot.Count

    /// <summary>Returns the underlying <c>UnboundedChannel&lt;obj&gt;</c> that stores messages.</summary>
    /// <returns>The internal message channel.</returns>
    member internal _.UnboundedChannel = resChan

    /// <summary>Returns a <c>Channel&lt;obj&gt;</c> sharing the same underlying storage and blocking slot.</summary>
    /// <returns>A cached or newly created <c>Channel&lt;obj&gt;</c> sharing the same underlying message queue.</returns>
    member internal _.Upcast() =
        match upcastChan with
        | Some cached -> cached
        | None ->
            lock upcastLock (fun () ->
                match upcastChan with
                | Some cached -> cached
                | None ->
                    let created = Channel<obj>(id, resChan, blockingSlot)
                    upcastChan <- Some created
                    created)

/// <summary>Represents a lazy, composable description of an effectful computation that may succeed, fail, or be interrupted.</summary>
/// <typeparam name="'R">The success result type produced when the effect completes.</typeparam>
/// <typeparam name="'E">The typed error type the effect may fail with.</typeparam>
and FIO<'R, 'E> =
    internal
    /// <summary>Represents an effect that completes immediately with a success value.</summary>
    | Success of res: 'R
    /// <summary>Represents an effect that fails immediately with a typed error value.</summary>
    | Failure of err: 'E
    /// <summary>Represents an effect that requests interruption of a specific fiber identified by its context.</summary>
    | InterruptFiber of cause: InterruptionCause * msg: string * fiberContext: FiberContext
    /// <summary>Represents an effect that interrupts the currently executing fiber.</summary>
    | InterruptSelf of cause: InterruptionCause * msg: string
    /// <summary>Represents an effect that invokes a side-effecting thunk when interpreted.</summary>
    | Action of func: (unit -> 'R) * onError: (exn -> 'E)
    /// <summary>Represents an effect that sends a message into a typed channel.</summary>
    | SendChan of msg: 'R * chan: Channel<'R>
    /// <summary>Represents an effect that receives the next message from a typed channel, blocking until one is available.</summary>
    | ReceiveChan of chan: Channel<'R>
    /// <summary>Represents an effect that forks a child effect onto a new fiber.</summary>
    | ForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    /// <summary>Represents an effect that forks a .NET task factory onto a new fiber.</summary>
    | ForkTask of taskFactory: (unit -> Task<obj>) * onError: (exn -> 'E) * fiber: obj * fiberContext: FiberContext
    /// <summary>Represents an effect that waits for a fiber to reach a terminal state and propagates its outcome.</summary>
    | JoinFiber of fiberContext: FiberContext
    /// <summary>Represents an effect that awaits a .NET <c>Task</c> and produces its result or mapped error.</summary>
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    /// <summary>Represents sequential composition that runs a continuation on the success result of a preceding effect.</summary>
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'R, 'E>)
    /// <summary>Represents sequential composition that runs a recovery continuation on the typed error of a preceding effect.</summary>
    | ChainError of eff: FIO<'R, obj> * cont: (obj -> FIO<'R, 'E>)
    /// <summary>Represents an effect guarded by a finalizer that runs on success, failure, and interruption.</summary>
    | OnFinalize of eff: FIO<'R, 'E> * finalizer: FIO<obj, obj>
    /// <summary>Represents a deferred re-raise of an interruption error after a finalizer has completed.</summary>
    | ResumeInterrupt of err: obj
    /// <summary>Represents the terminal result of a finalizer, carrying the guarded effect's outcome for propagation.</summary>
    | FinalizerResult of result: Result<obj, obj>

    /// <summary>Creates a new fiber that runs this effect concurrently with the caller.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because forking does not fail.</typeparam>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the newly running fiber.</returns>
    member this.Fork<'E1>() : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Context)

    /// <summary>Combines this effect with a continuation that runs on its successful result.</summary>
    /// <typeparam name="'R1">The success result type produced by the continuation.</typeparam>
    /// <param name="cont">A function from this effect's success value to the next effect to run.</param>
    /// <returns>An effect that runs this effect and, on success, runs the effect produced by <paramref name="cont"/>.</returns>
    member this.FlatMap<'R1>(cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess(this.UpcastResult(), fun res -> cont (res :?> 'R))

    /// <summary>Combines this effect with a recovery function that runs when it fails.</summary>
    /// <typeparam name="'E1">The error type of the recovered effect.</typeparam>
    /// <param name="onError">A function from this effect's typed error to the recovery effect.</param>
    /// <returns>An effect that completes with this effect's success value, or with the recovery effect's outcome on failure.</returns>
    member this.CatchAll<'E1>(onError: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError(this.UpcastError(), fun err -> onError (err :?> 'E))

    /// <summary>Builds an effect that runs a finalizer after this effect regardless of its outcome.</summary>
    /// <param name="finalizer">A cleanup effect that runs after this effect completes, fails, or is interrupted.</param>
    /// <returns>An effect that completes with this effect's outcome and always runs <paramref name="finalizer"/> afterwards.</returns>
    /// <remarks>The main effect's error takes precedence; a failure raised by <paramref name="finalizer"/> only surfaces when the main effect succeeded.</remarks>
    member this.Ensuring(finalizer: FIO<unit, 'E>) : FIO<'R, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// <summary>Transforms this effect's success type parameter to <c>obj</c> while preserving the error type.</summary>
    /// <returns>An equivalent effect with the success channel erased to <c>obj</c>.</returns>
    member internal this.UpcastResult() : FIO<obj, 'E> =
        match this with
        | Success res -> Success(res :> obj)
        | Failure err -> Failure err
        | InterruptFiber(cause, msg, fiberContext) -> InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) -> InterruptSelf(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, onError)
        | SendChan(msg, chan) -> SendChan(msg :> obj, chan.Upcast())
        | ReceiveChan chan -> ReceiveChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | ForkTask(taskFactory, onError, fiber, fiberContext) -> ForkTask(taskFactory, onError, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, onError)
        | ChainSuccess(eff, cont) -> ChainSuccess(eff, fun res -> cont(res).UpcastResult())
        | ChainError(eff, cont) ->
            let innerConts = ResizeArray()
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainError(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastResult()

            for i = innerConts.Count - 1 downto 0 do
                let ic = innerConts[i]
                rebuilt <- ChainError(rebuilt, fun err -> ic(err).UpcastResult())

            ChainError(rebuilt, fun err -> cont(err).UpcastResult())
        | OnFinalize(eff, finalizer) ->
            let finalizers = ResizeArray()
            finalizers.Add finalizer
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | OnFinalize(innerEff, innerFin) ->
                    finalizers.Add innerFin
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastResult()

            for i = finalizers.Count - 1 downto 0 do
                rebuilt <- OnFinalize(rebuilt, finalizers[i])

            rebuilt
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res

    /// <summary>Transforms this effect's error type parameter to <c>obj</c> while preserving the success type.</summary>
    /// <returns>An equivalent effect with the error channel erased to <c>obj</c>.</returns>
    member internal this.UpcastError() : FIO<'R, obj> =
        match this with
        | Success res -> Success res
        | Failure err -> Failure(err :> obj)
        | InterruptFiber(cause, msg, fiberContext) -> InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) -> InterruptSelf(cause, msg)
        | Action(func, onError) -> Action(func, upcastOnError onError)
        | SendChan(msg, chan) -> SendChan(msg, chan)
        | ReceiveChan chan -> ReceiveChan chan
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | ForkTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            let innerConts = ResizeArray()
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainSuccess(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastError()

            for i = innerConts.Count - 1 downto 0 do
                let ic = innerConts[i]
                rebuilt <- ChainSuccess(rebuilt, fun res -> ic(res).UpcastError())

            ChainSuccess(rebuilt, fun res -> cont(res).UpcastError())
        | ChainError(eff, cont) -> ChainError(eff, fun err -> cont(err).UpcastError())
        | OnFinalize(eff, finalizer) ->
            let finalizers = ResizeArray()
            finalizers.Add finalizer
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | OnFinalize(innerEff, innerFin) ->
                    finalizers.Add innerFin
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastError()

            for i = finalizers.Count - 1 downto 0 do
                rebuilt <- OnFinalize(rebuilt, finalizers[i])

            rebuilt
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res

    /// <summary>Transforms both the success and error type parameters to <c>obj</c>.</summary>
    /// <returns>An equivalent effect with both channels erased to <c>obj</c>.</returns>
    member internal this.UpcastBoth() : FIO<obj, obj> =
        match this with
        | Success res -> Success(res :> obj)
        | Failure err -> Failure(err :> obj)
        | InterruptFiber(cause, msg, fiberContext) -> InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) -> InterruptSelf(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, upcastOnError onError)
        | SendChan(msg, chan) -> SendChan(msg :> obj, chan.Upcast())
        | ReceiveChan chan -> ReceiveChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | ForkTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            let innerConts = ResizeArray()
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainSuccess(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastError()

            for i = innerConts.Count - 1 downto 0 do
                let ic = innerConts[i]
                rebuilt <- ChainSuccess(rebuilt, fun res -> ic(res).UpcastError())

            ChainSuccess(rebuilt, fun res -> cont(res).UpcastBoth())
        | ChainError(eff, cont) ->
            let innerConts = ResizeArray()
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | ChainError(innerEff, innerCont) ->
                    innerConts.Add innerCont
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastResult()

            for i = innerConts.Count - 1 downto 0 do
                let ic = innerConts[i]
                rebuilt <- ChainError(rebuilt, fun err -> ic(err).UpcastResult())

            ChainError(rebuilt, fun err -> cont(err).UpcastBoth())
        | OnFinalize(eff, finalizer) ->
            let finalizers = ResizeArray()
            finalizers.Add finalizer
            let mutable current = eff
            let mutable stopped = false

            while not stopped do
                match current with
                | OnFinalize(innerEff, innerFin) ->
                    finalizers.Add innerFin
                    current <- innerEff
                | _ -> stopped <- true

            let mutable rebuilt = current.UpcastBoth()

            for i = finalizers.Count - 1 downto 0 do
                rebuilt <- OnFinalize(rebuilt, finalizers[i])

            rebuilt
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res
