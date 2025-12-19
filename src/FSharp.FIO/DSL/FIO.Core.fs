(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides the core types, primitives, and internal machinery for the FIO effect system, including FIO, Fiber, Channel, and supporting types for effectful, concurrent, and asynchronous programming in F#.
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
/// Unbounded channel implementation using System.Threading.Channels for efficient message passing.
/// Thread-safety: All operations are thread-safe. Count uses Volatile.Read for non-blocking
/// queries (returning potentially stale values), while Interlocked ensures atomic
/// updates during Add/Take operations.
/// </summary>
and internal UnboundedChannel<'R> (id: Guid) =
    let chan = Channel.CreateUnbounded<'R>()
    let mutable count = 0L

    new() = UnboundedChannel (Guid.NewGuid ())

    member internal _.AddAsync (msg: 'R) =
        task {
            do! chan.Writer.WriteAsync msg
            Interlocked.Increment &count
            |> ignore
        }
    
    member internal _.TakeAsync () =
        task {
            let! res = chan.Reader.ReadAsync()
            Interlocked.Decrement &count
            |> ignore
            return res
        }
        
    member internal _.TryTake (res: byref<'R>) =
        let success = chan.Reader.TryRead &res
        if success then
            Interlocked.Decrement &count
            |> ignore
        success
    
    member internal _.WaitToTakeAsync () =
        chan.Reader.WaitToReadAsync().AsTask()
        
    member internal _.Clear () =
        let mutable item = Unchecked.defaultof<'R>
        while chan.Reader.TryRead &item do
            Interlocked.Decrement &count
            |> ignore

    member internal _.Count =
        Volatile.Read &count

    member internal _.Id =
        id

/// <summary>
/// Internal state of a fiber's execution lifecycle.
/// </summary>
and private FiberState =
    | Running = 0
    | Completed = 1
    | Interrupted = 2

/// <summary>
/// Internal fiber execution context managing state, result, cancellation, and blocking work items.
/// </summary>
and internal FiberContext () =
    let id = Guid.NewGuid ()
    let resTcs = TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously
    let blockingWorkItemChan = UnboundedChannel<WorkItem> ()
    let mutable state = int FiberState.Running
    let cts = new CancellationTokenSource()
    let mutable disposed = false

    let tryTransitionState fromState toState =
        Interlocked.CompareExchange(&state, int toState, int fromState) = int fromState

    member internal _.Complete res =
        task {
            if tryTransitionState FiberState.Running FiberState.Completed then
                resTcs.TrySetResult res |> ignore
        }

    member internal this.CompleteAndReschedule res activeWorkItemChan =
        task {
            if tryTransitionState FiberState.Running FiberState.Completed then
                if resTcs.TrySetResult res then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    member internal _.Interrupt cause msg =
        if tryTransitionState FiberState.Running FiberState.Interrupted then
            cts.Cancel()
            let interruptError = Error (FiberInterruptedException(id, cause, msg) :> obj)
            resTcs.TrySetResult interruptError |> ignore

    member internal _.IsInterrupted () =
        let currentState = enum<FiberState>(Volatile.Read &state)
        currentState = FiberState.Interrupted

    member internal _.CancellationToken =
        cts.Token
    
    member internal _.Task =
        resTcs.Task
    
    member internal _.AddBlockingWorkItem (blockingWorkItem: WorkItem) =
        task {
            // if completed then
            //     printfn "WARNING: FiberContext: Adding a blocking item on a fiber that is already completed!"
            do! blockingWorkItemChan.AddAsync blockingWorkItem
        }
        
    member internal _.BlockingWorkItemCount =
        blockingWorkItemChan.Count

    member internal _.Completed () =
        let currentState = enum<FiberState>(Volatile.Read &state)
        currentState = FiberState.Completed

    member internal _.Id =
        id

    member internal _.RescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>
            while blockingWorkItemChan.TryTake &workItem do
                do! activeWorkItemChan.AddAsync workItem
        }

    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.Completed () then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

    member private _.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then
                cts.Dispose ()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize () =
        this.Dispose false

/// <summary>
/// A Fiber represents a lightweight, cooperative thread of execution.
/// Fibers are used to execute effects concurrently and can be awaited to retrieve the result.
/// </summary>
/// <typeparam name="R">The result type of the fiber.</typeparam>
/// <typeparam name="E">The error type of the fiber.</typeparam>
and Fiber<'R, 'E> internal () =
    let fiberContext = new FiberContext ()
    let mutable disposed = false

    /// <summary>
    /// Creates an effect that awaits the fiber and succeeds with its result.
    /// </summary>
    /// <returns>An FIO effect that awaits the fiber and returns its result or error.</returns>
    member _.Await<'R, 'E> () : FIO<'R, 'E> =
        AwaitFiberContext fiberContext

    /// <summary>
    /// Awaits the fiber and returns its result as a Task.
    /// </summary>
    /// <returns>A Task that completes with the result or error of the fiber.</returns>
    member _.Task<'R, 'E> () =
        task {
            match! fiberContext.Task with
            | Ok res -> return Ok (res :?> 'R)
            | Error err -> return Error (err :?> 'E)
        }

    /// <summary>
    /// Interrupts the fiber with the specified cause and message.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    /// <returns>An FIO effect that performs the interruption.</returns>
    member _.Interrupt<'E> (cause: InterruptionCause) (msg: string) : FIO<unit, 'E> =
        Interruption (cause, msg)

    /// <summary>
    /// Gets a value indicating whether the fiber has been interrupted.
    /// </summary>
    /// <returns>True if the fiber is interrupted, otherwise false.</returns>
    member _.IsInterrupted =
        fiberContext.IsInterrupted

    /// <summary>
    /// Gets the unique identifier of the fiber.
    /// </summary>
    /// <returns>The unique Guid of the fiber.</returns>
    member _.Id =
        fiberContext.Id

    /// <summary>
    /// Gets the internal fiber context (for runtime use only).
    /// </summary>
    member internal _.Internal =
        fiberContext

    member private _.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then
                (fiberContext :> IDisposable).Dispose ()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize () =
        this.Dispose false

/// <summary>
/// A Channel represents a communication queue that holds data of type 'R.
/// Data can be sent to and received (blocking) from a channel.
/// </summary>
/// <typeparam name="R">The type of data held by the channel.</typeparam>
and Channel<'R> private (id: Guid, resChan: UnboundedChannel<obj>, blockingWorkItemChan: UnboundedChannel<WorkItem>) =

    /// <summary>
    /// Creates a new channel with a unique identifier.
    /// </summary>
    new() = Channel(Guid.NewGuid (), UnboundedChannel<obj> (), UnboundedChannel<WorkItem> ())

    /// <summary>
    /// Sends a message to the channel and succeeds with the message.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The message to send.</param>
    /// <returns>An FIO effect that sends the message and returns it.</returns>
    member this.Send<'R, 'E> (msg: 'R) : FIO<'R, 'E> =
        SendChan (msg, this)

    /// <summary>
    /// Receives a message from the channel and succeeds with it.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that receives a message from the channel.</returns>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        ReceiveChan this

    /// <summary>
    /// Gets the number of messages currently in the channel.
    /// </summary>
    /// <returns>The number of messages in the channel.</returns>
    member _.Count =
        resChan.Count
        
    /// <summary>
    /// Gets the unique identifier of the channel.
    /// </summary>
    /// <returns>The unique Guid of the channel.</returns>
    member _.Id =
        id
        
    /// <summary>
    /// Gets the unbounded channel (for runtime use only).
    /// </summary>
    member internal _.UnboundedChannel =
        resChan

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
        task {
            let mutable workItem = Unchecked.defaultof<_>
            if blockingWorkItemChan.TryTake &workItem then
                do! activeWorkItemChan.AddAsync workItem
        }

    member internal _.BlockingWorkItemCount =
        blockingWorkItemChan.Count

    /// <summary>
    /// Upcasts the channel to Channel&lt;obj&gt; (for runtime use only).
    /// </summary>
    member internal _.Upcast () =
        Channel<obj> (id, resChan, blockingWorkItemChan)

/// <summary>
/// Type alias for Channel with lowercase naming convention.
/// </summary>
and channel<'R> = Channel<'R>

/// <summary>
/// Builds a functional effect that can either succeed with a result or fail with an error when interpreted by a runtime.
/// This is the core type of the FIO effect system.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
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
    /// Executes an effect concurrently in a new Fiber and immediately returns the Fiber handle.
    /// The fiber can be awaited for the result of the effect.
    /// </summary>
    /// <typeparam name="R">The result type of the fiber.</typeparam>
    /// <typeparam name="E">The error type of the fiber.</typeparam>
    /// <typeparam name="E1">The error type of the returned effect.</typeparam>
    /// <returns>An FIO effect that starts concurrent execution and returns the Fiber handle.</returns>
    member this.Fork<'R, 'E, 'E1> () : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ConcurrentEffect (this.Upcast (), fiber, fiber.Internal)

    /// <summary>
    /// Binds a continuation to the result of this effect. Errors are propagated immediately if the effect fails.
    /// </summary>
    /// <typeparam name="R">The original result type.</typeparam>
    /// <typeparam name="R1">The result type of the continuation.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cont">The continuation function to apply to the result.</param>
    /// <returns>An FIO effect that applies the continuation to the result, or propagates an error.</returns>
    member this.Bind<'R, 'R1, 'E> (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess (this.UpcastResult (), fun res -> cont (res :?> 'R))

    /// <summary>
    /// Binds a continuation to the error of this effect. Results are propagated immediately if the effect succeeds.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The original error type.</typeparam>
    /// <typeparam name="E1">The error type of the continuation.</typeparam>
    /// <param name="cont">The continuation function to apply to the error.</param>
    /// <returns>An FIO effect that applies the continuation to the error, or propagates the result.</returns>
    member this.BindError<'R, 'E, 'E1> (cont: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError (this.UpcastError (), fun err -> cont (err :?> 'E))

    /// <summary>
    /// Upcasts the result type to obj (for runtime use only).
    /// </summary>
    member internal this.UpcastResult () : FIO<obj, 'E> =
        match this with
        | Success res ->
            Success (res :> obj)
        | Failure err ->
            Failure err
        | Interruption (cause, msg) ->
            Interruption (cause, msg)
        | Action (func, onError) ->
            Action (upcastFunc func, onError)
        | SendChan (msg, chan) ->
            SendChan (msg :> obj, chan.Upcast ())
        | ReceiveChan chan ->
            ReceiveChan <| chan.Upcast ()
        | ConcurrentEffect (eff, fiber, fiberContext) ->
            ConcurrentEffect (eff, fiber, fiberContext)
        | ConcurrentTPLTask (taskFactory, onError, fiber, fiberContext) ->
            ConcurrentTPLTask (taskFactory, onError, fiber, fiberContext)
        | ConcurrentGenericTPLTask (taskFactory, onError, fiber, fiberContext) ->
            ConcurrentGenericTPLTask (taskFactory, onError, fiber, fiberContext)
        | AwaitFiberContext fiberContext ->
            AwaitFiberContext fiberContext
        | AwaitTPLTask (task, onError) ->
            AwaitTPLTask (task, onError)
        | AwaitGenericTPLTask (task, onError) ->
            AwaitGenericTPLTask (task, onError)
        | ChainSuccess (eff, cont) ->
            ChainSuccess (eff, fun res -> (cont res).UpcastResult ())
        | ChainError (eff, cont) ->
            ChainError (eff.UpcastResult (), fun err -> (cont err).UpcastResult ())

    /// <summary>
    /// Upcasts the error type to obj (for runtime use only).
    /// </summary>
    member internal this.UpcastError () : FIO<'R, obj> =
        match this with
        | Success res ->
            Success res
        | Failure err ->
            Failure (err :> obj)
        | Interruption (cause, msg) ->
            Interruption (cause, msg)
        | Action (func, onError) ->
            Action (func, upcastOnError onError)
        | SendChan (msg, chan) ->
            SendChan (msg, chan)
        | ReceiveChan chan ->
            ReceiveChan chan
        | ConcurrentEffect (eff, fiber, fiberContext) ->
            ConcurrentEffect (eff, fiber, fiberContext)
        | ConcurrentTPLTask (taskFactory, onError, fiber, fiberContext) ->
            ConcurrentTPLTask (taskFactory, upcastOnError onError, fiber, fiberContext)
        | ConcurrentGenericTPLTask (taskFactory, onError, fiber, fiberContext) ->
            ConcurrentGenericTPLTask (taskFactory, upcastOnError onError, fiber, fiberContext)
        | AwaitFiberContext fiberContext ->
            AwaitFiberContext fiberContext
        | AwaitTPLTask (task, onError) ->
            AwaitTPLTask (task, upcastOnError onError)
        | AwaitGenericTPLTask (task, onError) ->
            AwaitGenericTPLTask (task, upcastOnError onError)
        | ChainSuccess (eff, cont) ->
            ChainSuccess (eff.UpcastError (), fun res -> (cont res).UpcastError ())
        | ChainError (eff, cont) ->
            ChainError (eff, fun err -> (cont err).UpcastError ())

    /// <summary>
    /// Upcasts both the result and error types to obj (for runtime use only).
    /// </summary>
    member internal this.Upcast () : FIO<obj, obj> =
        this.UpcastResult().UpcastError()
