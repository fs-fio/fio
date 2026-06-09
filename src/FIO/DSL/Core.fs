/// <summary>Provides the core types and primitives of the FIO effect system.</summary>
[<AutoOpen>]
module FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

/// <summary>Represents the guarded outcome saved on the continuation stack while a finalizer runs.</summary>
type internal PostFinalizerSaved =
    | PostFinalizerSuccess of value: obj
    | PostFinalizerError of error: obj
    | PostFinalizerInterrupt of error: obj

/// <summary>Represents a continuation used to chain effect evaluations.</summary>
and internal Cont =
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    | FailureCont of cont: (obj -> FIO<obj, obj>)
    | FinalizerCont of finalizer: FIO<obj, obj>
    | PostFinalizerCont of saved: PostFinalizerSaved

/// <summary>Represents a single stack entry wrapping a <c>Cont</c> value.</summary>
and [<Struct>] internal ContStackFrame =
    val Cont: Cont
    new cont = { Cont = cont }

/// <summary>Represents the stack of continuations for an in-progress effect evaluation.</summary>
and internal ContStack = Stack<ContStackFrame>

/// <summary>Represents a mutable work unit carrying an effect, its fiber context, continuation stack, and interruption state.</summary>
and internal WorkItem =
    {
        mutable Eff: FIO<obj, obj>
        mutable FiberContext: FiberContext
        mutable Stack: ContStack
        mutable InterruptionSuppressed: int
    }

/// <summary>Represents a work item that is blocked waiting on a channel message or a fiber's completion.</summary>
and internal BlockingItem =
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

/// <summary>Represents the terminal state of a fiber as one of three mutually exclusive outcomes.</summary>
/// <typeparam name="'A">The success result type.</typeparam>
/// <typeparam name="'E">The typed error type.</typeparam>
and FiberResult<'A, 'E> =
    | Succeeded of value: 'A
    | Failed of error: 'E
    | Interrupted of ex: FiberInterruptedException

/// <summary>Represents an unbounded message queue for passing values between fibers.</summary>
/// <typeparam name="'A">The element type carried by this channel.</typeparam>
/// <param name="id">The unique identifier for this channel instance.</param>
and [<Sealed; AllowNullLiteral>] internal UnboundedChannel<'A>(id: Guid) =
    let chan = Channel.CreateUnbounded<'A>()

    new() = UnboundedChannel(Guid.NewGuid())

    member internal _.Id =
        id

    member internal _.Count =
        chan.Reader.Count

    member internal _.WriteAsync value =
        chan.Writer.WriteAsync value

    member internal _.ReadAsync() =
        chan.Reader.ReadAsync()

    member internal _.TryRead (value: byref<'A>) =
        chan.Reader.TryRead &value

    member internal _.WaitToReadAsync (ct: CancellationToken) =
        chan.Reader.WaitToReadAsync ct

    member internal _.Clear() =
        let mutable value = Unchecked.defaultof<'A>
        while chan.Reader.TryRead &value do
            ()

/// <summary>Represents a lazily-initialized slot holding an <c>UnboundedChannel&lt;WorkItem&gt;</c> for blocked work items.</summary>
and [<Sealed>] internal BlockingWorkItemSlot() =

    [<VolatileField>]
    let mutable chan: UnboundedChannel<WorkItem> = null

    member _.Count =
        let chan' = Volatile.Read &chan
        if isNull chan' then 0 else chan'.Count

    member _.TryGet() =
        Volatile.Read &chan

    member _.GetOrCreate() =
        initIfNull &chan (fun () -> UnboundedChannel<WorkItem>())

/// <summary>Represents the lifecycle state of a fiber context as a tri-state enum.</summary>
and private FiberContextState =
    | Running = 0
    | Completed = 1
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
    let mutable disposed = 0

    [<VolatileField>]
    let mutable onTerminalCallback: (unit -> unit) voption = ValueNone

    [<VolatileField>]
    let mutable onTerminalFired = 0

    member internal _.Id =
        id

    member internal _.Task =
        resTcs.Task

    member internal _.CancellationToken =
        cts.Token

    member internal this.SetOnTerminal(callback: unit -> unit) =
        onTerminalCallback <- ValueSome callback
        if this.IsTerminal() then
            this.InvokeOnTerminal()

    member internal this.AddBlockingWorkItem blockingWorkItem =
        let chan = this.GetOrCreateBlockingChan()
        chan.WriteAsync blockingWorkItem

    member internal _.RescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let chan = Volatile.Read &blockingWorkItemChan
            if not (isNull chan) then
                let mutable workItem = Unchecked.defaultof<_>
                while chan.TryRead &workItem do
                    do! activeWorkItemChan.WriteAsync workItem
        }

    member internal this.TryRescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.IsTerminal() then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

    member internal this.AddRegistration(registration: IDisposable) =
        let bag = initIfNull &registrations (fun () -> ConcurrentBag<IDisposable>())
        bag.Add registration

        if this.IsTerminal() then
            let mutable victim = Unchecked.defaultof<_>
            while bag.TryTake &victim do
                try
                    victim.Dispose()
                with :? ObjectDisposedException ->
                    ()

    member internal _.IsCompleted() =
        Volatile.Read &state = int FiberContextState.Completed

    member internal _.IsInterrupted() =
        Volatile.Read &state = int FiberContextState.Interrupted

    member internal _.IsTerminal() =
        Volatile.Read &state <> int FiberContextState.Running

    member internal this.Complete value =
        if tryTransition &state (int FiberContextState.Running) (int FiberContextState.Completed) then
            this.DisposeRegistrations()
            this.InvokeOnTerminal()
            resTcs.TrySetResult value |> ignore

    member internal this.CompleteAndReschedule(value, activeWorkItemChan) =
        task {
            let oldState =
                transitionFrom &state (int FiberContextState.Running) (int FiberContextState.Completed)
            if oldState = int FiberContextState.Running then
                this.DisposeRegistrations()
                this.InvokeOnTerminal()
                resTcs.TrySetResult value |> ignore

                let chan = Volatile.Read &blockingWorkItemChan
                if not (isNull chan) && chan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
            elif oldState = int FiberContextState.Interrupted then
                this.DisposeRegistrations()
                this.InvokeOnTerminal()
                resTcs.TrySetResult value |> ignore

                let chan = Volatile.Read &blockingWorkItemChan
                if not (isNull chan) && chan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    member internal this.Interrupt (?cause, ?message) =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg message "Fiber was interrupted."

        if tryTransition &state (int FiberContextState.Running) (int FiberContextState.Interrupted) then
            cts.Cancel(throwOnFirstException = false)
            this.DisposeRegistrations()
            let interruptError = Error(FiberInterruptedException(id, cause, msg) :> obj)
            this.InvokeOnTerminal()
            resTcs.TrySetResult interruptError |> ignore

    member internal _.Cancel() =
        try
            cts.Cancel(throwOnFirstException = false)
        with :? ObjectDisposedException ->
            ()

    member private _.InvokeOnTerminal() =
        match onTerminalCallback with
        | ValueSome cb when tryClaim &onTerminalFired ->
            try
                cb ()
            with _ ->
                ()
        | _ ->
            ()

    member private _.GetOrCreateBlockingChan() : UnboundedChannel<WorkItem> =
        initIfNull &blockingWorkItemChan (fun () -> UnboundedChannel<WorkItem>())

    member private _.DisposeRegistrations() =
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
                cts.Dispose()

    override this.Finalize() =
        this.Dispose false

    interface IDisposable with
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

/// <summary>Represents a lightweight, cooperative thread of execution that can be awaited for its result.</summary>
/// <typeparam name="'A">The success result type produced by the fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fiber may fail with.</typeparam>
and [<Sealed>] Fiber<'A, 'E> internal () =
    let fiberContext = new FiberContext()

    /// <summary>Returns the unique identifier assigned to this fiber.</summary>
    /// <returns>A <c>Guid</c> distinguishing this fiber from all others created in this process.</returns>
    member _.Id =
        fiberContext.Id

    /// <summary>Returns the cancellation token associated with this fiber's cancellation.</summary>
    /// <returns>A <c>CancellationToken</c> that is cancelled when this fiber is interrupted.</returns>
    member _.CancellationToken =
        fiberContext.CancellationToken

    /// <summary>Returns a .NET task that completes with this fiber's <c>FiberResult</c>.</summary>
    /// <returns>A task that completes with <c>Succeeded</c>, <c>Failed</c>, or <c>Interrupted</c> depending on the fiber's terminal state.</returns>
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

    /// <summary>Builds an effect that awaits this fiber's completion and propagates its success or failure to the calling fiber.</summary>
    /// <returns>An effect that completes with the fiber's success value, fails with its typed error, or interrupts when the fiber is interrupted.</returns>
    member _.Join () : FIO<'A, 'E> =
        JoinFiber fiberContext

    /// <summary>Creates an effect that requests interruption of this fiber.</summary>
    /// <param name="cause">The reason for the interruption; defaults to <c>ExplicitInterrupt</c>.</param>
    /// <param name="message">A human-readable description of the interruption; defaults to a generic message.</param>
    /// <returns>An effect that completes with unit once interruption has been requested; the fiber may not yet have terminated when the effect completes.</returns>
    member _.Interrupt (cause: InterruptionCause) (message: string) : FIO<unit, 'E> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), fun ex -> raise ex)

    /// <summary>Creates an effect that requests interruption of this fiber using a default cause and message.</summary>
    /// <returns>An effect that completes with unit once interruption has been requested with cause <c>ExplicitInterrupt</c> and message <c>"Fiber interrupted"</c>.</returns>
    /// <remarks>Convenience shorthand for <c>fiber.Interrupt ExplicitInterrupt "Fiber interrupted"</c>; use <c>Interrupt</c> when a specific cause or message is required.</remarks>
    member this.InterruptNow () : FIO<unit, 'E> =
        this.Interrupt ExplicitInterrupt "Fiber interrupted"

    /// <summary>Builds an effect that awaits this fiber and returns its <c>FiberResult</c> as a value, without re-raising errors or propagating interruptions.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because the await never fails.</typeparam>
    /// <returns>An effect that completes with the fiber's terminal state as a value.</returns>
    member this.Await<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        AwaitTask(upcastTask (this.Task()), fun ex -> raise ex)

    /// <summary>Builds an effect that interrupts this fiber and awaits its terminal result.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because the await never fails.</typeparam>
    /// <param name="cause">The reason for the interruption; defaults to <c>ExplicitInterrupt</c>.</param>
    /// <param name="message">A human-readable description of the interruption; defaults to a generic message.</param>
    /// <returns>An effect that requests interruption and then completes with the fiber's terminal <c>FiberResult</c>.</returns>
    member this.InterruptAwait<'E2> (cause: InterruptionCause) (message: string) : FIO<FiberResult<'A, 'E>, 'E2> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), fun ex -> raise ex)
            .FlatMap <| fun () -> this.Await()

    /// <summary>Builds an effect that interrupts this fiber using a default cause and message, then awaits its terminal result.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because the await never fails.</typeparam>
    /// <returns>An effect that requests interruption with cause <c>ExplicitInterrupt</c> and message <c>"Fiber interrupted"</c>, then completes with the fiber's terminal <c>FiberResult</c>.</returns>
    /// <remarks>Convenience shorthand for <c>fiber.InterruptAwait ExplicitInterrupt "Fiber interrupted"</c>; use <c>InterruptAwait</c> when a specific cause or message is required.</remarks>
    member this.InterruptAwaitNow<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        this.InterruptAwait ExplicitInterrupt "Fiber interrupted"

    /// <summary>Creates an effect that produces <c>Some result</c> when the fiber has terminated, or <c>None</c> when it is still running.</summary>
    /// <typeparam name="'E2">The error type of the resulting effect; never produced because polling never fails.</typeparam>
    /// <returns>An effect that completes with <c>Some</c> wrapping the terminal <c>FiberResult</c>, or <c>None</c> when the fiber is not yet terminal.</returns>
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
            fun ex -> raise ex)

    /// <summary>Combines this fiber's terminal outcome with three branch handlers, producing the effect from whichever branch matches.</summary>
    /// <typeparam name="'A1">The result type produced by every branch handler.</typeparam>
    /// <typeparam name="'E1">The error type produced by every branch handler.</typeparam>
    /// <param name="onSucceeded">A function from the fiber's success value to the effect to run when the fiber completed successfully.</param>
    /// <param name="onFailed">A function from the fiber's typed error to the effect to run when the fiber failed.</param>
    /// <param name="onInterrupted">A function from the interruption exception to the effect to run when the fiber was interrupted.</param>
    /// <returns>An effect that awaits this fiber and runs the handler matching its terminal state.</returns>
    member this.JoinWith<'A1, 'E1>
        (onSucceeded: 'A -> FIO<'A1, 'E1>)
        (onFailed: 'E -> FIO<'A1, 'E1>)
        (onInterrupted: FiberInterruptedException -> FIO<'A1, 'E1>)
        : FIO<'A1, 'E1> =
        this.Await().FlatMap(fun result ->
            match result with
            | Succeeded value ->
                onSucceeded value
            | Failed error ->
                onFailed error
            | Interrupted ex ->
                onInterrupted ex)

    /// <summary>Returns whether the fiber has reached the completed state.</summary>
    /// <returns><c>true</c> when the fiber finished with a success or a typed error; <c>false</c> while it is still running or has been interrupted.</returns>
    member _.IsCompleted () =
        fiberContext.IsCompleted()

    /// <summary>Returns whether the fiber has been interrupted.</summary>
    /// <returns><c>true</c> when the fiber's terminal state is interruption; <c>false</c> while running or after normal completion.</returns>
    member _.IsInterrupted () =
        fiberContext.IsInterrupted()

    /// <summary>Returns whether the fiber has reached any terminal state.</summary>
    /// <returns><c>true</c> when the fiber has completed or been interrupted; <c>false</c> while it is still running.</returns>
    member _.IsTerminal () =
        fiberContext.IsTerminal()

    /// <summary>Returns the fiber's <c>FiberResult</c> by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The terminal state of the fiber.</returns>
    /// <remarks>Prefer <c>Join</c> or <c>Task</c> in effectful code; this method exists for examples and tests.</remarks>
    member this.UnsafeResult () =
        this.Task() |> Async.AwaitTask |> Async.RunSynchronously

    /// <summary>Returns the fiber's success value by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The success value when the fiber completed successfully.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the fiber failed or was interrupted.</exception>
    member this.UnsafeSuccess () =
        match this.UnsafeResult() with
        | Succeeded value ->
            value
        | Failed error ->
            raise (InvalidOperationException $"Fiber failed with error: {error}")
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>Returns the fiber's typed error by synchronously blocking the calling thread until it terminates.</summary>
    /// <returns>The typed error value when the fiber failed.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the fiber succeeded or was interrupted.</exception>
    member this.UnsafeError() =
        match this.UnsafeResult() with
        | Succeeded value ->
            raise (InvalidOperationException $"Fiber succeeded with value: {value}")
        | Failed error ->
            error
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>Returns unit after synchronously printing the fiber's terminal result to standard output.</summary>
    /// <returns>Unit, after the fiber's <c>FiberResult</c> has been printed.</returns>
    member this.UnsafePrintResult() =
        printfn "%A" (this.UnsafeResult())

    /// <summary>Returns the internal <c>FiberContext</c> backing this fiber's execution state.</summary>
    /// <returns>The <c>FiberContext</c> instance associated with this fiber.</returns>
    member internal _.Context =
        fiberContext

    /// <summary>Returns the fiber's identifier as a string.</summary>
    /// <returns>The string form of <c>Id</c>.</returns>
    override this.ToString () =
        this.Id.ToString()

    interface IDisposable with
        member _.Dispose() =
            (fiberContext :> IDisposable).Dispose()

/// <summary>Represents a typed communication queue for sending and receiving messages between fibers.</summary>
/// <typeparam name="'A">The element type carried by this channel.</typeparam>
/// <param name="id">The unique identifier for this channel instance.</param>
/// <param name="valueChan">The underlying unbounded channel used for value buffering.</param>
/// <param name="blockingSlot">The slot managing blocked work items awaiting values.</param>
and [<Sealed>] Channel<'A> private (id: Guid, valueChan: UnboundedChannel<obj>, blockingSlot: BlockingWorkItemSlot) =
    let upcastLock = obj ()

    [<VolatileField>]
    let mutable upcastChan = None

    let mutable signalProcessing = 0

    /// <summary>Creates a new channel with a freshly generated unique identifier.</summary>
    new() = Channel(Guid.NewGuid(), UnboundedChannel<obj>(), BlockingWorkItemSlot())

    /// <summary>Returns the unique identifier assigned to this channel.</summary>
    /// <returns>A <c>Guid</c> distinguishing this channel from all others created in this process.</returns>
    member _.Id =
        id

    /// <summary>Returns the current number of messages buffered in this channel.</summary>
    /// <returns>The count of messages awaiting receivers.</returns>
    member _.Count =
        valueChan.Count

    /// <summary>Builds an effect that sends a message into this channel.</summary>
    /// <param name="message">The message to enqueue.</param>
    /// <returns>An effect that completes once the message has been enqueued.</returns>
    member this.Write<'E> message : FIO<'A, 'E> =
        WriteChan(message, this)

    /// <summary>Builds an effect that receives the next message from this channel, blocking until one is available.</summary>
    /// <returns>An effect that completes with the next message, suspending the fiber while the channel is empty.</returns>
    member this.Read<'E> () : FIO<'A, 'E> =
        ReadChan this

    /// <summary>Transforms the channel state by asynchronously adding a value to the underlying buffer.</summary>
    /// <param name="value">The value to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the value has been enqueued.</returns>
    member internal _.WriteAsync (value: 'A) =
        valueChan.WriteAsync value

    /// <summary>Returns the next value from the channel buffer, waiting asynchronously if empty.</summary>
    /// <returns>A task that completes with the next message, cast to the channel's element type.</returns>
    member internal _.ReadAsync () =
        task {
            let! value = valueChan.ReadAsync()
            return value :?> 'A
        }

    /// <summary>Returns the number of work items blocked waiting on this channel.</summary>
    /// <returns>The count of blocked work items in the blocking slot.</returns>
    member internal _.BlockingWorkItemCount =
        blockingSlot.Count

    /// <summary>Transforms the channel by adding a work item to the blocking slot for rescheduling on the next send.</summary>
    /// <param name="blockingItem">The work item to enqueue.</param>
    /// <returns>A <c>ValueTask</c> that completes when the work item has been enqueued.</returns>
    member internal _.AddBlockingWorkItem blockingItem =
        let chan = blockingSlot.GetOrCreate()
        chan.WriteAsync blockingItem

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
                if chan.TryRead &workItem then
                    do! activeWorkItemChan.WriteAsync workItem
                    return true
                else
                    return false
        }

    /// <summary>Returns whether signal processing was successfully claimed for this channel.</summary>
    /// <returns><c>true</c> when this caller acquired the signal processing lock; <c>false</c> when another caller already holds it.</returns>
    member internal _.TryBeginSignalProcessing() =
        tryClaim &signalProcessing

    /// <summary>Transforms the channel's signal-processing state by marking it as available.</summary>
    member internal _.EndSignalProcessing() =
        Volatile.Write(&signalProcessing, 0)

    /// <summary>Returns the underlying <c>UnboundedChannel&lt;obj&gt;</c> that stores messages.</summary>
    /// <returns>The internal message channel.</returns>
    member internal _.UnboundedChannel =
        valueChan

    /// <summary>Returns a <c>Channel&lt;obj&gt;</c> sharing the same underlying storage and blocking slot.</summary>
    /// <returns>A cached or newly created <c>Channel&lt;obj&gt;</c> sharing the same underlying message queue.</returns>
    member internal _.Upcast() =
        match upcastChan with
        | Some cached ->
            cached
        | None ->
            lock upcastLock (fun () ->
                match upcastChan with
                | Some cached ->
                    cached
                | None ->
                    let created = Channel<obj>(id, valueChan, blockingSlot)
                    upcastChan <- Some created
                    created)

/// <summary>Represents a lazy, composable description of an effectful computation that may succeed, fail, or be interrupted.</summary>
/// <typeparam name="'A">The success value type produced when the effect completes.</typeparam>
/// <typeparam name="'E">The error type the effect may fail with.</typeparam>
and FIO<'A, 'E> =
    internal
    | Success of value: 'A
    | Failure of error: 'E
    | Interrupt of cause: InterruptionCause * msg: string
    | Action of func: (unit -> 'A) * onError: (exn -> 'E)
    | WriteChan of value: 'A * chan: Channel<'A>
    | ReadChan of chan: Channel<'A>
    | ForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | JoinFiber of fiberContext: FiberContext
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'A, 'E>)
    | ChainError of eff: FIO<'A, obj> * cont: (obj -> FIO<'A, 'E>)
    | OnFinalize of eff: FIO<'A, 'E> * finalizer: FIO<obj, obj>
    | FiberCancellationToken

    /// <summary>Combines this effect with a continuation that runs on its successful result.</summary>
    /// <typeparam name="'A1">The success result type produced by the continuation.</typeparam>
    /// <param name="cont">A function from this effect's success value to the next effect to run.</param>
    /// <returns>An effect that runs this effect and, on success, runs the effect produced by <paramref name="cont"/>.</returns>
    member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        ChainSuccess(this.UpcastResult(), fun value -> cont (value :?> 'A))

    /// <summary>Combines this effect with a recovery function that runs when it fails.</summary>
    /// <typeparam name="'E1">The error type of the recovered effect.</typeparam>
    /// <param name="onError">A function from this effect's typed error to the recovery effect.</param>
    /// <returns>An effect that completes with this effect's success value, or with the recovery effect's outcome on failure.</returns>
    member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        ChainError(this.UpcastError(), fun error -> onError (error :?> 'E))

    /// <summary>Builds an effect that runs a finalizer after this effect regardless of its outcome.</summary>
    /// <param name="finalizer">A cleanup effect that runs after this effect completes, fails, or is interrupted.</param>
    /// <returns>An effect that completes with this effect's outcome and always runs <paramref name="finalizer"/> afterwards.</returns>
    /// <remarks>The main effect's error takes precedence; a failure raised by <paramref name="finalizer"/> only surfaces when the main effect succeeded.</remarks>
    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// <summary>Creates a new fiber that runs this effect concurrently with the caller.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because forking does not fail.</typeparam>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the newly running fiber.</returns>
    member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        let fiber = new Fiber<'A, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Context)

    /// <summary>Transforms the success value of this effect with a pure function.</summary>
    /// <typeparam name="'A1">The success result type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's success value to the new success value.</param>
    /// <returns>An effect that completes with <paramref name="mapper"/> applied to the original success value.</returns>
    member this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> =
        this.FlatMap <| fun value -> Success (mapper value)

    /// <summary>Transforms the typed error of this effect with a pure function.</summary>
    /// <typeparam name="'E1">The error type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's typed error to the new typed error.</param>
    /// <returns>An effect that propagates the original success value, or fails with <paramref name="mapper"/> applied to the original error.</returns>
    member this.MapError<'E1> (mapper: 'E -> 'E1) : FIO<'A, 'E1> =
        this.CatchAll <| fun error -> Failure (mapper error)

    /// <summary>Transforms both the success and error channels of this effect with pure functions.</summary>
    /// <typeparam name="'A1">The success result type produced by <paramref name="successMapper"/>.</typeparam>
    /// <typeparam name="'E1">The error type produced by <paramref name="errorMapper"/>.</typeparam>
    /// <param name="successMapper">A pure function from the original success value to the new success value.</param>
    /// <param name="errorMapper">A pure function from the original typed error to the new typed error.</param>
    /// <returns>An effect with both channels transformed by their respective mappers.</returns>
    member this.MapBoth<'A1, 'E1> (successMapper: 'A -> 'A1) (errorMapper: 'E -> 'E1) : FIO<'A1, 'E1> =
        this.Map(successMapper).MapError errorMapper

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Result</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because the original error is moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Ok</c> on success or <c>Error</c> on failure.</returns>
    member this.Result<'E1> () : FIO<Result<'A, 'E>, 'E1> =
        this.Map(Ok).CatchAll <| fun error -> Success (Error error)

    /// <summary>Transforms this effect into an infallible effect whose success channel carries an <c>Option</c>, discarding the error.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are mapped to <c>None</c>.</typeparam>
    /// <returns>An effect that completes with <c>Some</c> on success or <c>None</c> on failure.</returns>
    member this.Option<'E1> () : FIO<'A option, 'E1> =
        this.Map(Some).CatchAll <| fun _ -> Success None

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Choice</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Choice1Of2</c> on success or <c>Choice2Of2</c> on failure.</returns>
    member this.Choice<'E1> () : FIO<Choice<'A, 'E>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun error ->
            Success (Choice2Of2 error))

    static member inline private flattenOnFinalize
        (leafUpcast: FIO<'A, 'E> -> FIO<'OR, 'OE>)
        (eff: FIO<'A, 'E>)
        (outerFinalizer: FIO<obj, obj>)
        : FIO<'OR, 'OE> =
        let finalizers = ResizeArray<FIO<obj, obj>>()
        finalizers.Add outerFinalizer
        let mutable current = eff
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

    static member inline private flattenChainSuccess
        (leafUpcast: FIO<obj, 'E> -> FIO<obj, 'OE>)
        (innerContWrap: (obj -> FIO<obj, 'E>) -> (obj -> FIO<obj, 'OE>))
        (outerContWrap: (obj -> FIO<'A, 'E>) -> (obj -> FIO<'OR, 'OE>))
        (eff: FIO<obj, 'E>)
        (outerCont: obj -> FIO<'A, 'E>)
        : FIO<'OR, 'OE> =
        let inners = ResizeArray<obj -> FIO<obj, 'E>>()
        let mutable current = eff
        let mutable stopped = false

        while not stopped do
            match current with
            | ChainSuccess(innerEff, innerCont) ->
                inners.Add innerCont
                current <- innerEff
            | _ -> stopped <- true

        let mutable rebuilt = leafUpcast current
        for i = inners.Count - 1 downto 0 do
            rebuilt <- ChainSuccess(rebuilt, innerContWrap inners[i])

        ChainSuccess(rebuilt, outerContWrap outerCont)

    static member inline private flattenChainError
        (leafUpcast: FIO<'A, obj> -> FIO<'OR, obj>)
        (innerContWrap: (obj -> FIO<'A, obj>) -> (obj -> FIO<'OR, obj>))
        (outerContWrap: (obj -> FIO<'A, 'E>) -> (obj -> FIO<'OR, 'OE>))
        (eff: FIO<'A, obj>)
        (outerCont: obj -> FIO<'A, 'E>)
        : FIO<'OR, 'OE> =
        let inners = ResizeArray<obj -> FIO<'A, obj>>()
        let mutable current = eff
        let mutable stopped = false

        while not stopped do
            match current with
            | ChainError(innerEff, innerCont) ->
                inners.Add innerCont
                current <- innerEff
            | _ -> stopped <- true

        let mutable rebuilt = leafUpcast current
        for i = inners.Count - 1 downto 0 do
            rebuilt <- ChainError(rebuilt, innerContWrap inners[i])

        ChainError(rebuilt, outerContWrap outerCont)

    member internal this.UpcastResult() : FIO<obj, 'E> =
        match this with
        | Success value -> Success(value :> obj)
        | Failure error -> Failure error
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, onError)
        | WriteChan(msg, chan) -> WriteChan(msg :> obj, chan.Upcast())
        | ReadChan chan -> ReadChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, onError)
        | ChainSuccess(eff, cont) -> ChainSuccess(eff, fun value -> cont(value).UpcastResult())
        | ChainError(eff, cont) ->
            FIO.flattenChainError
                (fun e -> e.UpcastResult())
                (fun ic -> fun error -> (ic error).UpcastResult())
                (fun oc -> fun error -> (oc error).UpcastResult())
                eff cont
        | OnFinalize(eff, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastResult()) eff finalizer
        | FiberCancellationToken -> FiberCancellationToken

    member internal this.UpcastError() : FIO<'A, obj> =
        match this with
        | Success value -> Success value
        | Failure error -> Failure(error :> obj)
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(func, upcastOnError onError)
        | WriteChan(msg, chan) -> WriteChan(msg, chan)
        | ReadChan chan -> ReadChan chan
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            FIO.flattenChainSuccess
                (fun e -> e.UpcastError())
                (fun ic -> fun value -> (ic value).UpcastError())
                (fun oc -> fun value -> (oc value).UpcastError())
                eff cont
        | ChainError(eff, cont) -> ChainError(eff, fun error -> cont(error).UpcastError())
        | OnFinalize(eff, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastError()) eff finalizer
        | FiberCancellationToken -> FiberCancellationToken

    member internal this.UpcastBoth() : FIO<obj, obj> =
        match this with
        | Success value -> Success(value :> obj)
        | Failure error -> Failure(error :> obj)
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, upcastOnError onError)
        | WriteChan(msg, chan) -> WriteChan(msg :> obj, chan.Upcast())
        | ReadChan chan -> ReadChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) -> ForkEffect(eff, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            FIO.flattenChainSuccess
                (fun e -> e.UpcastError())
                (fun ic -> fun value -> (ic value).UpcastError())
                (fun oc -> fun value -> (oc value).UpcastBoth())
                eff cont
        | ChainError(eff, cont) ->
            FIO.flattenChainError
                (fun e -> e.UpcastResult())
                (fun ic -> fun error -> (ic error).UpcastResult())
                (fun oc -> fun error -> (oc error).UpcastBoth())
                eff cont
        | OnFinalize(eff, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastBoth()) eff finalizer
        | FiberCancellationToken -> FiberCancellationToken
