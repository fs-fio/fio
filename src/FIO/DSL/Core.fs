/// <summary>
/// Core types and primitives for the FIO effect system.
/// </summary>
[<AutoOpen>]
module FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

/// <summary>
/// Internal continuation representation for effect chaining.
/// </summary>
type internal Cont =
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    | FailureCont of cont: (obj -> FIO<obj, obj>)
    | FinalizerCont of finalizer: FIO<obj, obj>

/// <summary>
/// Internal stack frame for continuation tracking during effect execution.
/// </summary>
and [<Struct>] internal ContStackFrame =
    val Cont: Cont
    new cont = { Cont = cont }

/// <summary>
/// Internal stack of continuations for effect execution.
/// </summary>
and internal ContStack =
    Stack<ContStackFrame>

/// <summary>
/// Internal work item representing an effect to be executed along with its context and continuation stack.
/// </summary>
and internal WorkItem =
    { mutable Eff: FIO<obj, obj>
      mutable FiberContext: FiberContext
      mutable Stack: ContStack
      mutable InterruptionSuppressed: int }

/// <summary>
/// Discriminated union representing effects that are blocked waiting for resources.
/// Used by CooperativeRuntime's blocking worker for linear-time polling.
/// </summary>
and internal BlockingItem =
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

/// <summary>
/// Result of a fiber execution, distinguishing between success, failure, and interruption.
/// </summary>
and FiberResult<'R, 'E> =
    /// <summary>
    /// The fiber completed successfully with a result.
    /// </summary>
    | Succeeded of result: 'R
    /// <summary>
    /// The fiber failed with an error.
    /// </summary>
    | Failed of error: 'E
    /// <summary>
    /// The fiber was interrupted before completion.
    /// </summary>
    | Interrupted of ex: FiberInterruptedException

/// <summary>
/// Thread-safe unbounded channel for message passing.
/// </summary>
and [<Sealed>] internal UnboundedChannel<'R> (id: Guid) =
    let chan = Channel.CreateUnbounded<'R>()

    new() = UnboundedChannel (Guid.NewGuid())

    /// <summary>
    /// Gets the unique identifier.
    /// </summary>
    member internal _.Id =
        id

    /// <summary>
    /// Gets the current message count.
    /// </summary>
    member internal _.Count =
        chan.Reader.Count

    /// <summary>
    /// Adds a message asynchronously.
    /// </summary>
    member internal _.AddAsync (msg: 'R) : ValueTask =
        chan.Writer.WriteAsync msg

    /// <summary>
    /// Takes a message asynchronously.
    /// </summary>
    member internal _.TakeAsync () : ValueTask<'R> =
        chan.Reader.ReadAsync()

    /// <summary>
    /// Tries to take a message without blocking.
    /// </summary>
    member internal _.TryTake (res: byref<'R>) =
        chan.Reader.TryRead &res

    /// <summary>
    /// Waits asynchronously until a message is available.
    /// </summary>
    member internal _.WaitToTakeAsync () : ValueTask<bool> =
        chan.Reader.WaitToReadAsync()

    /// <summary>
    /// Clears all messages from the channel.
    /// </summary>
    member internal _.Clear () =
        let mutable item = Unchecked.defaultof<'R>
        while chan.Reader.TryRead &item do
            ()

/// <summary>
/// Internal state of a fiber's execution context lifecycle.
/// </summary>
and private FiberContextState =
    | Running = 0
    | Completed = 1
    | Interrupted = 2

/// <summary>
/// Internal fiber execution context managing state, result, cancellation, and blocking work items.
/// </summary>
and [<Sealed>] internal FiberContext () =
    let id = Guid.NewGuid()
    let mutable state = int FiberContextState.Running
    let blockingWorkItemChan = UnboundedChannel<WorkItem>()
    let resTcs = TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously
    let cts = new CancellationTokenSource()
    let registrations = ConcurrentBag<IDisposable>()
    let mutable disposed = false

    /// <summary>
    /// Gets the unique identifier.
    /// </summary>
    member internal _.Id =
        id

    /// <summary>
    /// Gets the task representing the fiber's result.
    /// </summary>
    member internal _.Task =
        resTcs.Task

    /// <summary>
    /// Gets the cancellation token for cooperative cancellation.
    /// </summary>
    member internal _.CancellationToken =
        cts.Token

    /// <summary>
    /// Adds a disposable registration to be cleaned up when the fiber completes.
    /// </summary>
    member internal _.AddRegistration (registration: IDisposable) =
        registrations.Add registration

    /// <summary>
    /// Disposes all registrations.
    /// </summary>
    member private _.DisposeRegistrations () =
        let mutable registration = Unchecked.defaultof<_>
        while registrations.TryTake(&registration) do
            try
                registration.Dispose()
            with :? ObjectDisposedException ->
                ()

    /// <summary>
    /// Completes the fiber with the given result.
    /// </summary>
    member internal this.Complete res =
        let oldState = Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running)
        if oldState = int FiberContextState.Running then
            resTcs.TrySetResult res |> ignore
            this.DisposeRegistrations()
        elif oldState = int FiberContextState.Interrupted then
            resTcs.TrySetResult res |> ignore
            this.DisposeRegistrations()

    /// <summary>
    /// Completes the fiber and reschedules blocking work items.
    /// </summary>
    member internal this.CompleteAndReschedule (res, activeWorkItemChan) =
        task {
            let oldState = Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running)
            if oldState = int FiberContextState.Running then
                resTcs.TrySetResult res |> ignore
                this.DisposeRegistrations()
                if blockingWorkItemChan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
            elif oldState = int FiberContextState.Interrupted then
                resTcs.TrySetResult res |> ignore
                this.DisposeRegistrations()
                // Reschedule blocking work items so they can observe the interruption
                if blockingWorkItemChan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    /// <summary>
    /// Gets whether the fiber has completed.
    /// </summary>
    member internal _.Completed () =
        Volatile.Read &state = int FiberContextState.Completed

    /// <summary>
    /// Gets whether the fiber has reached a terminal state (completed or interrupted).
    /// </summary>
    member internal _.IsTerminal () =
        let currentState = Volatile.Read &state
        currentState = int FiberContextState.Completed ||
        currentState = int FiberContextState.Interrupted

    /// <summary>
    /// Interrupts the fiber with the specified cause and message.
    /// </summary>
    member internal this.Interrupt (?cause, ?msg) =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        if Interlocked.CompareExchange(&state, int FiberContextState.Interrupted, int FiberContextState.Running) = int FiberContextState.Running then
            cts.Cancel()
            let interruptError = Error (FiberInterruptedException(id, cause, msg) :> obj)
            resTcs.TrySetResult interruptError |> ignore
            this.DisposeRegistrations()

    /// <summary>
    /// Gets whether the fiber has been interrupted.
    /// </summary>
    member internal _.Interrupted () =
        Volatile.Read &state = int FiberContextState.Interrupted

    /// <summary>
    /// Adds a blocking work item to the queue.
    /// </summary>
    member internal _.AddBlockingWorkItem (blockingWorkItem: WorkItem) =
        blockingWorkItemChan.AddAsync blockingWorkItem

    /// <summary>
    /// Reschedules all blocking work items to the active queue.
    /// </summary>
    member internal _.RescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>
            while blockingWorkItemChan.TryTake &workItem do
                do! activeWorkItemChan.AddAsync workItem
        }

    /// <summary>
    /// Tries to reschedule blocking work items if fiber is in a terminal state.
    /// </summary>
    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.IsTerminal() then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

    member private _.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then
                cts.Dispose()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize () =
        this.Dispose false

/// <summary>
/// A lightweight, cooperative thread of execution that can be awaited for its result.
/// </summary>
and [<Sealed>] Fiber<'R, 'E> internal () =
    let fiberContext = new FiberContext()
    let mutable disposed = false

    /// <summary>
    /// Gets the unique identifier.
    /// </summary>
    member _.Id =
        fiberContext.Id

    /// <summary>
    /// Gets the cancellation token for cooperative cancellation.
    /// </summary>
    member _.CancellationToken =
        fiberContext.CancellationToken

    /// <summary>
    /// Gets whether the fiber has completed.
    /// </summary>
    member _.Completed () =
        fiberContext.Completed()

    /// <summary>
    /// Gets whether the fiber has been interrupted.
    /// </summary>
    member _.Interrupted =
        fiberContext.Interrupted()

    /// <summary>
    /// Awaits the fiber's completion and returns its result.
    /// </summary>
    member _.Join<'R, 'E> () : FIO<'R, 'E> =
        JoinFiber fiberContext

    /// <summary>
    /// Returns the fiber's result as a Task&lt;FiberResult&lt;'R, 'E&gt;&gt;.
    /// </summary>
    member _.Task<'R, 'E> () =
        task {
            match! fiberContext.Task with
            | Ok res -> return Succeeded(res :?> 'R)
            | Error err ->
                match err with
                | :? FiberInterruptedException as ex -> return Interrupted ex
                | _ -> return Failed(err :?> 'E)
        }

    /// <summary>
    /// Synchronously blocks and returns the fiber's FiberResult.
    /// Prefer Join() within effects or Task() for async interop.
    /// </summary>
    /// <returns>FiberResult&lt;'R, 'E&gt; with Succeeded, Failed, or Interrupted.</returns>
    member this.UnsafeResult<'R, 'E> () =
        this.Task()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    /// <summary>
    /// Synchronously blocks and returns the fiber's success value, or throws if the fiber failed.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Prefer using Join() within an effect context or Task() for async/await interop.
    /// Use this when you need to synchronously extract a successful result and want to fail fast on errors (e.g., in simple CLI tools or test assertions).
    /// </summary>
    /// <returns>The success value of type 'R.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the fiber completed with an error.</exception>
    member this.UnsafeSuccess<'R, 'E> () =
        match
            this.Task()
            |> Async.AwaitTask
            |> Async.RunSynchronously with
        | Succeeded res -> res
        | Failed err -> raise (InvalidOperationException $"Fiber failed with error: {err}")
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>
    /// Synchronously blocks and returns the fiber's error value, or throws if the fiber succeeded.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Prefer using Join() within an effect context or Task() for async/await interop.
    /// Use this when you need to synchronously extract an error for testing or debugging purposes.
    /// </summary>
    /// <returns>The error value of type 'E.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the fiber completed successfully.</exception>
    member this.UnsafeError<'R, 'E> () =
        match
            this.Task()
            |> Async.AwaitTask
            |> Async.RunSynchronously with
        | Succeeded res -> raise (InvalidOperationException $"Fiber succeeded with result: {res}")
        | Failed err -> err
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// <summary>
    /// Synchronously blocks and prints the fiber's result to the console.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Use this for quick debugging or in simple CLI applications where blocking is acceptable.
    /// For production code, prefer using Join() with FConsole.printLine within the effect system.
    /// </summary>
    member this.UnsafePrintResult<'R, 'E> () =
        printfn "%A" (this.UnsafeResult<'R, 'E>())

    /// <summary>
    /// Synchronously completes the fiber with the specified result.
    /// This is an unsafe side-effecting operation that bypasses the FIO effect system.
    /// Use this when you need to complete a fiber from outside an effect context (e.g., callbacks, interop).
    /// </summary>
    /// <param name="res">The result to complete the fiber with (Ok for success, Error for failure).</param>
    member _.UnsafeComplete (res: Result<'R, 'E>) =
        match res with
        | Ok res -> fiberContext.Complete(Ok(res :> obj))
        | Error err -> fiberContext.Complete(Error(err :> obj))

    /// <summary>
    /// Synchronously interrupts the fiber with the specified cause and message.
    /// Unlike Interrupt(), this is an unsafe side-effecting operation that bypasses the FIO effect system.
    /// Use this when you need to interrupt a fiber from outside an effect context (e.g., event handlers, cancellation callbacks).
    /// This will cancel the fiber's CancellationToken and complete it with FiberInterruptedException.
    /// </summary>
    /// <param name="cause">The interruption cause. Defaults to ExplicitInterrupt.</param>
    /// <param name="msg">The interruption message. Defaults to "Fiber was interrupted."</param>
    member _.UnsafeInterrupt (?cause: InterruptionCause, ?msg: string) =
        fiberContext.Interrupt(?cause = cause, ?msg = msg)

    /// <summary>
    /// Interrupts the fiber with the specified cause and message.
    /// </summary>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    member _.Interrupt<'E> (?cause: InterruptionCause, ?msg: string) : FIO<unit, 'E> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        InterruptFiber(cause, msg, fiberContext)

    /// <summary>
    /// Gets the internal fiber context (for runtime use only).
    /// </summary>
    member internal _.Internal =
        fiberContext

    override this.ToString () = 
        this.Id.ToString()

    member private _.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then
                (fiberContext :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize () =
        this.Dispose false

/// <summary>
/// A typed communication queue for sending and receiving messages.
/// </summary>
and [<Sealed>] Channel<'R> private (id: Guid, resChan: UnboundedChannel<obj>, blockingWorkItemChan: UnboundedChannel<WorkItem>) =
    let upcastLock = obj()
    let mutable upcastChan: Channel<obj> option = None
    let mutable signalProcessing = 0

    /// <summary>
    /// Creates a new channel with a unique identifier.
    /// </summary>
    new() = Channel(Guid.NewGuid(), UnboundedChannel<obj>(), UnboundedChannel<WorkItem>())

    /// <summary>
    /// Gets the unique identifier.
    /// </summary>
    member _.Id =
        id

    /// <summary>
    /// Gets the current message count.
    /// </summary>
    member _.Count =
        resChan.Count

    /// <summary>
    /// Sends a message to the channel.
    /// </summary>
    /// <param name="msg">The message to send.</param>
    member this.Send<'R, 'E> (msg: 'R) : FIO<'R, 'E> =
        SendChan(msg, this)

    /// <summary>
    /// Receives a message from the channel (blocking if empty).
    /// </summary>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        ReceiveChan this

    /// <summary>
    /// Sends a message to the channel asynchronously (for runtime use only).
    /// </summary>
    member internal _.SendAsync (msg: 'R) =
        resChan.AddAsync msg

    /// <summary>
    /// Receives a message from the channel asynchronously (for runtime use only).
    /// </summary>
    member internal _.ReceiveAsync () =
        task {
            let! res = resChan.TakeAsync()
            return res :?> 'R
        }

    /// <summary>
    /// Adds a blocking work item to the channel (for runtime use only).
    /// </summary>
    member internal _.AddBlockingWorkItem blockingItem =
        blockingWorkItemChan.AddAsync blockingItem

    /// <summary>
    /// Reschedules the next blocking work item to the active work item channel (for runtime use only).
    /// </summary>
    member internal _.RescheduleNextBlockingWorkItem (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        let mutable workItem = Unchecked.defaultof<_>
        if blockingWorkItemChan.TryTake &workItem then
            activeWorkItemChan.AddAsync workItem
        else
            ValueTask()

    /// <summary>
    /// Tries to reschedule next blocking work item if available.
    /// Returns true if rescheduled, false otherwise.
    /// </summary>
    member internal _.TryRescheduleNextBlockingWorkItem (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>
            if blockingWorkItemChan.TryTake &workItem then
                do! activeWorkItemChan.AddAsync workItem
                return true
            else
                return false
        }

    /// <summary>
    /// Attempts to transition the channel to "signal processing" state.
    /// Returns true if the caller owns signal processing.
    /// </summary>
    member internal _.TryBeginSignalProcessing () =
        Interlocked.CompareExchange(&signalProcessing, 1, 0) = 0

    /// <summary>
    /// Ends signal processing for the channel.
    /// </summary>
    member internal _.EndSignalProcessing () =
        Volatile.Write(&signalProcessing, 0)

    /// <summary>
    /// Gets the count of blocking work items (for runtime use only).
    /// </summary>
    member internal _.BlockingWorkItemCount =
        blockingWorkItemChan.Count

    /// <summary>
    /// Gets the unbounded channel (for runtime use only).
    /// </summary>
    member internal _.UnboundedChannel =
        resChan

    /// <summary>
    /// Upcasts the channel to Channel (for runtime use only).
    /// </summary>
    member internal _.Upcast () =
        match upcastChan with
        | Some cached ->
            cached
        | None ->
            lock upcastLock (fun () ->
                match upcastChan with
                | Some cached ->
                    cached
                | None ->
                    let created = Channel<obj>(id, resChan, blockingWorkItemChan)
                    upcastChan <- Some created
                    created)


/// <summary>
/// A functional effect that succeeds with a result or fails with an error when interpreted.
/// </summary>
and FIO<'R, 'E> =
    internal
    | Success of res: 'R
    | Failure of err: 'E
    | InterruptFiber of cause: InterruptionCause * msg: string * fiberContext: FiberContext
    | InterruptSelf of cause: InterruptionCause * msg: string
    | Action of func: (unit -> 'R) * onError: (exn -> 'E)
    | SendChan of msg: 'R * chan: Channel<'R>
    | ReceiveChan of chan: Channel<'R>
    | ForkEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | ForkTPLTask of taskFactory: (unit -> Task) * onError: (exn -> 'E) * fiber: obj * fiberContext: FiberContext
    | ForkGenericTPLTask of taskFactory: (unit -> Task<obj>) * onError: (exn -> 'E) * fiber: obj * fiberContext: FiberContext
    | JoinFiber of fiberContext: FiberContext
    | AwaitTPLTask of task: Task * onError: (exn -> 'E)
    | AwaitGenericTPLTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'R, 'E>)
    | ChainError of eff: FIO<'R, obj> * cont: (obj -> FIO<'R, 'E>)
    | OnFinalize of eff: FIO<'R, 'E> * finalizer: FIO<obj, obj>
    | ResumeInterrupt of err: obj
    | FinalizerResult of result: Result<obj, obj>

    /// <summary>
    /// Executes this effect concurrently in a new Fiber.
    /// </summary>
    member this.Fork<'E1> () : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Internal)

    /// <summary>
    /// Chains this effect with a continuation function (monadic bind).
    /// </summary>
    /// <param name="cont">The continuation function.</param>
    member this.FlatMap<'R1> (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess(this.UpcastResult(), fun res -> cont(res :?> 'R))

    /// <summary>
    /// Handles errors with a recovery function.
    /// </summary>
    /// <param name="onError">The error handler function.</param>
    member this.CatchAll<'E1> (onError: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError(this.UpcastError(), fun err -> onError(err :?> 'E))

    /// <summary>
    /// Runs a finalizer after this effect, preserving the original outcome.
    /// If the main effect succeeds but the finalizer fails, the finalizer error is returned.
    /// If the main effect fails, the main error is preserved (finalizer errors are suppressed).
    /// </summary>
    /// <param name="finalizer">The finalizer effect to run.</param>
    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'R, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// <summary>
    /// Upcasts the result type to obj (for runtime use only).
    /// </summary>
    member internal this.UpcastResult () : FIO<obj, 'E> =
        match this with
        | Success res ->
            Success (res :> obj)
        | Failure err ->
            Failure err
        | InterruptFiber(cause, msg, fiberContext) ->
            InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) ->
            InterruptSelf(cause, msg)
        | Action(func, onError) ->
            Action(upcastFunc func, onError)
        | SendChan(msg, chan) ->
            SendChan(msg :> obj, chan.Upcast())
        | ReceiveChan chan ->
            ReceiveChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) ->
            ForkEffect(eff, fiber, fiberContext)
        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTPLTask(taskFactory, onError, fiber, fiberContext)
        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | AwaitTPLTask(task, onError) ->
            AwaitTPLTask(task, onError)
        | AwaitGenericTPLTask(task, onError) ->
            AwaitGenericTPLTask(task, onError)
        | ChainSuccess(eff, cont) ->
            ChainSuccess(eff, fun res -> cont(res).UpcastResult())
        | ChainError(eff, cont) ->
            ChainError(eff.UpcastResult(), fun err -> cont(err).UpcastResult())
        | OnFinalize(eff, finalizer) ->
            OnFinalize(eff.UpcastResult(), finalizer)
        | ResumeInterrupt err ->
            ResumeInterrupt err
        | FinalizerResult res ->
            FinalizerResult res

    /// <summary>
    /// Upcasts the error type to obj (for runtime use only).
    /// </summary>
    member internal this.UpcastError () : FIO<'R, obj> =
        match this with
        | Success res ->
            Success res
        | Failure err ->
            Failure(err :> obj)
        | InterruptFiber(cause, msg, fiberContext) ->
            InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) ->
            InterruptSelf(cause, msg)
        | Action(func, onError) ->
            Action(func, upcastOnError onError)
        | SendChan(msg, chan) ->
            SendChan(msg, chan)
        | ReceiveChan chan ->
            ReceiveChan chan
        | ForkEffect(eff, fiber, fiberContext) ->
            ForkEffect(eff, fiber, fiberContext)
        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkGenericTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | AwaitTPLTask(task, onError) ->
            AwaitTPLTask(task, upcastOnError onError)
        | AwaitGenericTPLTask(task, onError) ->
            AwaitGenericTPLTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            ChainSuccess(eff.UpcastError(), fun res -> cont(res).UpcastError())
        | ChainError(eff, cont) ->
            ChainError(eff, fun err -> cont(err).UpcastError())
        | OnFinalize(eff, finalizer) ->
            OnFinalize(eff.UpcastError(), finalizer)
        | ResumeInterrupt err ->
            ResumeInterrupt err
        | FinalizerResult res ->
            FinalizerResult res

    /// <summary>
    /// Upcasts both the result and error types to obj (for runtime use only).
    /// </summary>
    member internal this.UpcastBoth () : FIO<obj, obj> =
        match this with
        | Success res ->
            Success (res :> obj)
        | Failure err ->
            Failure (err :> obj)
        | InterruptFiber(cause, msg, fiberContext) ->
            InterruptFiber(cause, msg, fiberContext)
        | InterruptSelf(cause, msg) ->
            InterruptSelf(cause, msg)
        | Action(func, onError) ->
            Action(upcastFunc func, upcastOnError onError)
        | SendChan(msg, chan) ->
            SendChan(msg :> obj, chan.Upcast())
        | ReceiveChan chan ->
            ReceiveChan(chan.Upcast())
        | ForkEffect(eff, fiber, fiberContext) ->
            ForkEffect(eff, fiber, fiberContext)
        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ForkGenericTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
        | AwaitTPLTask(task, onError) ->
            AwaitTPLTask(task, upcastOnError onError)
        | AwaitGenericTPLTask(task, onError) ->
            AwaitGenericTPLTask(task, upcastOnError onError)
        | ChainSuccess(eff, cont) ->
            ChainSuccess(eff.UpcastError(), fun res -> cont(res).UpcastResult().UpcastError())
        | ChainError(eff, cont) ->
            ChainError(eff.UpcastResult(), fun err -> cont(err).UpcastResult().UpcastError())
        | OnFinalize(eff, finalizer) ->
            OnFinalize(eff.UpcastResult().UpcastError(), finalizer)
        | ResumeInterrupt err ->
            ResumeInterrupt err
        | FinalizerResult res ->
            FinalizerResult res
