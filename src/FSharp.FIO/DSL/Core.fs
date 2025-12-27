/// <summary>
/// Core types and primitives for the FIO effect system.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Concurrent

/// <summary>
/// Internal continuation representation for effect chaining.
/// </summary>
type internal Cont =
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    | FailureCont of cont: (obj -> FIO<obj, obj>)

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
    ResizeArray<ContStackFrame>

/// <summary>
/// Internal work item representing an effect to be executed along with its context and continuation stack.
/// </summary>
and internal WorkItem =
    { mutable Eff: FIO<obj, obj>
      mutable FiberContext: FiberContext
      mutable Stack: ContStack }

/// <summary>
/// Discriminated union representing effects that are blocked waiting for resources.
/// Used by CooperativeRuntime's blocking worker for linear-time polling.
/// </summary>
and internal BlockingItem =
    | BlockingChannel of channel: obj channel * waitingWorkItem: WorkItem
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

/// <summary>
/// Thread-safe unbounded channel for message passing.
/// </summary>
and [<Sealed>] internal UnboundedChannel<'R> (id: Guid) =
    let mutable count = 0L
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
        Volatile.Read &count

    /// <summary>
    /// Adds a message asynchronously.
    /// </summary>
    member internal _.AddAsync (msg: 'R) =
        task {
            do! chan.Writer.WriteAsync msg
            Interlocked.Increment &count |> ignore
        }

    /// <summary>
    /// Takes a message asynchronously.
    /// </summary>
    member internal _.TakeAsync () =
        task {
            let! res = chan.Reader.ReadAsync()
            Interlocked.Decrement &count |> ignore
            return res
        }

    /// <summary>
    /// Tries to take a message without blocking.
    /// </summary>
    member internal _.TryTake (res: byref<'R>) =
        let success = chan.Reader.TryRead &res
        if success then
            Interlocked.Decrement &count |> ignore
        success

    /// <summary>
    /// Waits asynchronously until a message is available.
    /// </summary>
    member internal _.WaitToTakeAsync () =
        chan.Reader.WaitToReadAsync().AsTask()

    /// <summary>
    /// Clears all messages from the channel.
    /// </summary>
    member internal _.Clear () =
        let mutable item = Unchecked.defaultof<'R>
        while chan.Reader.TryRead &item do
            Interlocked.Decrement &count |> ignore

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
            registration.Dispose()

    /// <summary>
    /// Completes the fiber with the given result.
    /// </summary>
    member internal this.Complete res =
        if Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int FiberContextState.Running then
            resTcs.TrySetResult res |> ignore
            this.DisposeRegistrations()

    /// <summary>
    /// Completes the fiber and reschedules blocking work items.
    /// </summary>
    member internal this.CompleteAndReschedule (res, activeWorkItemChan) =
        task {
            if Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int FiberContextState.Running then
                if resTcs.TrySetResult res then
                    this.DisposeRegistrations()
                    if blockingWorkItemChan.Count > 0 then
                        do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    /// <summary>
    /// Gets whether the fiber has completed.
    /// </summary>
    member internal _.Completed () =
        Volatile.Read &state = int FiberContextState.Completed

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
    /// Tries to reschedule blocking work items if fiber is completed.
    /// </summary>
    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.Completed() then
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
and Fiber<'R, 'E> internal () =
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
        fiberContext.Interrupted

    /// <summary>
    /// Awaits the fiber's completion and returns its result.
    /// </summary>
    member _.Join<'R, 'E> () : FIO<'R, 'E> =
        JoinFiber fiberContext

    /// <summary>
    /// Returns the fiber's result as a Task.
    /// </summary>
    member _.Task<'R, 'E> () =
        task {
            match! fiberContext.Task with
            | Ok res -> return Ok(res :?> 'R)
            | Error err -> return Error(err :?> 'E)
        }

    /// <summary>
    /// Synchronously blocks and returns the fiber's result as a Result type.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Prefer using Join() within an effect context or Task() for async/await interop.
    /// Use this when you need to synchronously extract a fiber's result from outside the effect system (e.g., in Main or test assertions).
    /// </summary>
    /// <returns>Result&lt;'R, 'E&gt; where Ok contains the success value or Error contains the failure value.</returns>
    member _.UnsafeResult<'R, 'E> () =
        match fiberContext.Task.Result with
        | Ok res -> Ok(res :?> 'R)
        | Error err -> Error(err :?> 'E)

    /// <summary>
    /// Synchronously blocks and returns the fiber's success value, or throws if the fiber failed.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Prefer using Join() within an effect context or Task() for async/await interop.
    /// Use this when you need to synchronously extract a successful result and want to fail fast on errors (e.g., in simple CLI tools or test assertions).
    /// </summary>
    /// <returns>The success value of type 'R.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the fiber completed with an error.</exception>
    member _.UnsafeSuccess<'R, 'E> () =
        match fiberContext.Task.Result with
        | Ok res -> res :?> 'R
        | Error err -> raise (InvalidOperationException(sprintf "Fiber failed with error: %A" (err :?> 'E)))

    /// <summary>
    /// Synchronously blocks and returns the fiber's error value, or throws if the fiber succeeded.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Prefer using Join() within an effect context or Task() for async/await interop.
    /// Use this when you need to synchronously extract an error for testing or debugging purposes.
    /// </summary>
    /// <returns>The error value of type 'E.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the fiber completed successfully.</exception>
    member _.UnsafeError<'R, 'E> () =
        match fiberContext.Task.Result with
        | Ok res -> raise (InvalidOperationException(sprintf "Fiber succeeded with result: %A" (res :?> 'R)))
        | Error err -> err :?> 'E

    /// <summary>
    /// Synchronously blocks and prints the fiber's result to the console.
    /// This is an unsafe blocking operation that should be used with caution.
    /// Use this for quick debugging or in simple CLI applications where blocking is acceptable.
    /// For production code, prefer using Join() with FConsole.PrintLine within the effect system.
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
    member _.UnsafeInterrupt<'R, 'E> (?cause: InterruptionCause, ?msg: string) =
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
and Channel<'R> private (id: Guid, resChan: UnboundedChannel<obj>, blockingWorkItemChan: UnboundedChannel<WorkItem>) =

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
            Task.FromResult()

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
    /// Upcasts the channel to Channel&lt;obj&gt; (for runtime use only).
    /// </summary>
    member internal _.Upcast () =
        Channel<obj>(id, resChan, blockingWorkItemChan)

/// <summary>
/// Type alias for Channel with lowercase naming convention.
/// </summary>
and channel<'R> = Channel<'R>

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

    /// <summary>
    /// Executes this effect concurrently in a new Fiber.
    /// </summary>
    member this.Fork<'R, 'E, 'E1> () : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ForkEffect(this.Upcast(), fiber, fiber.Internal)

    /// <summary>
    /// Chains this effect with a continuation function (monadic bind).
    /// </summary>
    /// <param name="cont">The continuation function.</param>
    member this.FlatMap<'R, 'R1, 'E> (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess(this.UpcastResult(), fun res -> cont(res :?> 'R))

    /// <summary>
    /// Handles errors with a recovery function.
    /// </summary>
    /// <param name="onError">The error handler function.</param>
    member this.CatchAll<'R, 'E, 'E1> (onError: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError(this.UpcastError(), fun err -> onError(err :?> 'E))

    /// <summary>
    /// Runs a finalizer after this effect, preserving the original outcome.
    /// </summary>
    /// <param name="finalizer">The finalizer effect to run.</param>
    member this.Ensuring<'R, 'E> (finalizer: FIO<unit, 'E>) : FIO<'R, 'E> =
        let runFinalizer outcome = finalizer.CatchAll(fun _ -> Success()).FlatMap(fun _ -> outcome)
        this.FlatMap(fun res -> runFinalizer(Success res))
            .CatchAll(fun err -> runFinalizer(Failure err))

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
            ChainSuccess(eff, fun res -> (cont res).UpcastResult())
        | ChainError(eff, cont) ->
            ChainError(eff.UpcastResult(), fun err -> (cont err).UpcastResult())

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
            ChainSuccess(eff.UpcastError(), fun res -> (cont res).UpcastError())
        | ChainError(eff, cont) ->
            ChainError(eff, fun err -> (cont err).UpcastError())

    /// <summary>
    /// Upcasts both the result and error types to obj (for runtime use only).
    /// </summary>
    member internal this.Upcast () : FIO<obj, obj> =
        this.UpcastResult().UpcastError()
