(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Core types and primitives for the FIO effect system.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels

/// <summary>
/// Represents the cause or reason for fiber interruption.
/// </summary>
type InterruptionCause =
    /// Fiber was interrupted due to a timeout.
    | Timeout of durationMs: float
    /// Fiber was interrupted because its parent fiber was interrupted.
    | ParentInterrupted of parentFiberId: Guid
    /// Fiber was explicitly interrupted via Fiber.Interrupt().
    | ExplicitInterrupt
    /// Fiber was interrupted due to resource exhaustion or system limits.
    | ResourceExhaustion of reason: string

    override this.ToString() =
        match this with
        | Timeout ms -> $"Timeout({ms}ms)"
        | ParentInterrupted id -> $"ParentInterrupted({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | ResourceExhaustion r -> $"ResourceExhaustion({r})"

/// <summary>
/// Exception thrown when a fiber is interrupted during execution.
/// </summary>
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    override this.Message =
        $"Fiber {this.fiberId} interrupted: {this.cause} - {this.message}"

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
/// Internal runtime action representing the outcome of effect evaluation.
/// </summary>
and internal RuntimeAction =
    | Skipped
    | Evaluated
    | RescheduleForRunning
    | RescheduleForBlocking of BlockingItem

/// <summary>
/// Internal representation of items that can block execution.
/// </summary>
and internal BlockingItem =
    | BlockingChannel of Channel<obj>
    | BlockingFiberContext of FiberContext

/// <summary>
/// Internal data structure tracking a blocked work item and its blocking resource.
/// </summary>
and internal BlockingData =
    { BlockingItem: BlockingItem
      WaitingWorkItem: WorkItem }

/// <summary>
/// Internal work item representing an effect to be executed along with its context and continuation stack.
/// </summary>
and internal WorkItem =
    { Eff: FIO<obj, obj>
      FiberContext: FiberContext
      Stack: ContStack
      PrevAction: RuntimeAction }

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
    /// Completes the fiber with the given result.
    /// </summary>
    member internal _.Complete res =
        if Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int FiberContextState.Running then
            resTcs.TrySetResult res |> ignore

    /// <summary>
    /// Completes the fiber and reschedules blocking work items.
    /// </summary>
    member internal this.CompleteAndReschedule (res, activeWorkItemChan) =
        task {
            if Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int FiberContextState.Running then
                if resTcs.TrySetResult res then
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
    member internal _.Interrupt (cause, msg) =
        if Interlocked.CompareExchange(&state, int FiberContextState.Interrupted, int FiberContextState.Running) = int FiberContextState.Running then
            cts.Cancel()
            let interruptError = Error (FiberInterruptedException(id, cause, msg) :> obj)
            resTcs.TrySetResult interruptError |> ignore

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
        AwaitFiberContext fiberContext

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
    /// Interrupts the fiber with the specified cause and message.
    /// </summary>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    member _.Interrupt<'E> (cause: InterruptionCause, msg: string) : FIO<unit, 'E> =
        Interruption(cause, msg)

    /// <summary>
    /// Gets the internal fiber context (for runtime use only).
    /// </summary>
    member internal _.Internal =
        fiberContext

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
    | Interruption of cause: InterruptionCause * msg: string
    | Action of func: (unit -> 'R) * onError: (exn -> 'E)
    | SendChan of msg: 'R * chan: Channel<'R>
    | ReceiveChan of chan: Channel<'R>
    | ConcurrentEffect of eff: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | ConcurrentTPLTask of taskFactory: (unit -> Task) * onError: (exn -> 'E) * fiber: obj * fiberContext: FiberContext
    | ConcurrentGenericTPLTask of taskFactory: (unit -> Task<obj>) * onError: (exn -> 'E) * fiber: obj * fiberContext: FiberContext
    | AwaitFiberContext of fiberContext: FiberContext
    | AwaitTPLTask of task: Task * onError: (exn -> 'E)
    | AwaitGenericTPLTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'R, 'E>)
    | ChainError of eff: FIO<'R, obj> * cont: (obj -> FIO<'R, 'E>)

    /// <summary>
    /// Executes this effect concurrently in a new Fiber.
    /// </summary>
    member this.Fork<'R, 'E, 'E1> () : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ConcurrentEffect (this.Upcast(), fiber, fiber.Internal)

    /// <summary>
    /// Chains this effect with a continuation function (monadic bind).
    /// </summary>
    /// <param name="cont">The continuation function.</param>
    member this.FlatMap<'R, 'R1, 'E> (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess (this.UpcastResult(), fun res -> cont(res :?> 'R))

    /// <summary>
    /// Handles errors with a recovery function.
    /// </summary>
    /// <param name="onError">The error handler function.</param>
    member this.CatchAll<'R, 'E, 'E1> (onError: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError (this.UpcastError(), fun err -> onError(err :?> 'E))

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
        | Interruption(cause, msg) ->
            Interruption(cause, msg)
        | Action(func, onError) ->
            Action(upcastFunc func, onError)
        | SendChan(msg, chan) ->
            SendChan(msg :> obj, chan.Upcast())
        | ReceiveChan chan ->
            ReceiveChan(chan.Upcast())
        | ConcurrentEffect(eff, fiber, fiberContext) ->
            ConcurrentEffect(eff, fiber, fiberContext)
        | ConcurrentTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ConcurrentTPLTask(taskFactory, onError, fiber, fiberContext)
        | ConcurrentGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ConcurrentGenericTPLTask(taskFactory, onError, fiber, fiberContext)
        | AwaitFiberContext fiberContext ->
            AwaitFiberContext fiberContext
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
        | Interruption(cause, msg) ->
            Interruption(cause, msg)
        | Action(func, onError) ->
            Action(func, upcastOnError onError)
        | SendChan(msg, chan) ->
            SendChan(msg, chan)
        | ReceiveChan chan ->
            ReceiveChan chan
        | ConcurrentEffect(eff, fiber, fiberContext) ->
            ConcurrentEffect(eff, fiber, fiberContext)
        | ConcurrentTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ConcurrentTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | ConcurrentGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
            ConcurrentGenericTPLTask(taskFactory, upcastOnError onError, fiber, fiberContext)
        | AwaitFiberContext fiberContext ->
            AwaitFiberContext fiberContext
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
