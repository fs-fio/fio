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

[<AutoOpen>]
module private Utils =

    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) =
        fun (exn: exn) -> onError exn :> obj

    let inline upcastFunc (func: unit -> 'R) : unit -> obj =
        fun () -> func () :> obj
        
    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        task {
            let! res = genericTask
            return box res
        }

// In Core.fs or a new Exceptions.fs

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

type internal Cont =
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    | FailureCont of cont: (obj -> FIO<obj, obj>)

and [<Struct>] internal ContStackFrame =
    val Cont: Cont
    new cont = { Cont = cont }

and internal ContStack =
    ResizeArray<ContStackFrame>

and internal RuntimeAction =
    | Skipped
    | Evaluated
    | RescheduleForRunning
    | RescheduleForBlocking of BlockingItem

and internal BlockingItem =
    | BlockingChannel of Channel<obj>
    | BlockingIFiber of InternalFiber
    
and internal BlockingData =
    { BlockingItem: BlockingItem
      WaitingWorkItem: WorkItem }

and internal WorkItem =
    { Eff: FIO<obj, obj>
      IFiber: InternalFiber
      Stack: ContStack
      PrevAction: RuntimeAction }

/// <summary>
/// Internal channel implementation using System.Threading.Channels for efficient message passing.
/// Thread-safety: All operations are thread-safe. Count uses Volatile.Read for non-blocking
/// queries (acceptable to return slightly stale values), while Interlocked ensures atomic
/// updates during Add/Take operations.
/// </summary>
and internal InternalChannel<'R> (id: Guid) =
    let chan = Channel.CreateUnbounded<'R>()
    let mutable count = 0L

    new() = InternalChannel (Guid.NewGuid ())

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

and private FiberState =
    | Running = 0
    | Completed = 1
    | Interrupted = 2

and internal InternalFiber () =
    let id = Guid.NewGuid ()
    let resTcs = TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously
    let blockingWorkItemChan = InternalChannel<WorkItem> ()
    let mutable state = int FiberState.Running
    let cts = new CancellationTokenSource()
    let mutable disposed = false

    // Helper to try transition state atomically
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
            //     printfn "WARNING: InternalFiber: Adding a blocking item on a fiber that is already completed!"
            do! blockingWorkItemChan.AddAsync blockingWorkItem
        }
        
    member internal _.BlockingWorkItemCount =
        blockingWorkItemChan.Count

    member internal _.Completed () =
        let currentState = enum<FiberState>(Volatile.Read &state)
        currentState = FiberState.Completed

    member internal _.Id =
        id

    member internal _.RescheduleBlockingWorkItems (activeWorkItemChan: InternalChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>
            while blockingWorkItemChan.TryTake &workItem do
                do! activeWorkItemChan.AddAsync workItem
        }

    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemChan: InternalChannel<WorkItem>) =
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
/// Fibers are used to interpret multiple effects in parallel and can be awaited to retrieve the result of the effect.
/// </summary>
/// <typeparam name="R">The result type of the fiber.</typeparam>
/// <typeparam name="E">The error type of the fiber.</typeparam>
and Fiber<'R, 'E> internal () =
    let ifiber = new InternalFiber ()
    let mutable disposed = false

    /// <summary>
    /// Creates an effect that waits for the fiber and succeeds with its result.
    /// </summary>
    /// <returns>An FIO effect that waits for the fiber and returns its result or error.</returns>
    member _.Await<'R, 'E> () : FIO<'R, 'E> =
        AwaitFiber ifiber

    /// <summary>
    /// Waits for the fiber and succeeds with its result, returning a Task.
    /// </summary>
    /// <returns>A Task that completes with the result or error of the fiber.</returns>
    member _.Task<'R, 'E> () =
        task {
            match! ifiber.Task with
            | Ok res -> return Ok (res :?> 'R)
            | Error err -> return Error (err :?> 'E)
        }

    member _.Interrupt<'E> (cause: InterruptionCause) (msg: string) : FIO<unit, 'E> =
        Interruption (cause, msg)

    member _.IsInterrupted =
        ifiber.IsInterrupted

    /// <summary>
    /// Gets the unique identifier of the fiber.
    /// </summary>
    /// <returns>The unique Guid of the fiber.</returns>
    member _.Id =
        ifiber.Id

    member internal _.Internal =
        ifiber

    member private _.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then
                (ifiber :> IDisposable).Dispose ()

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
and Channel<'R> private (id: Guid, resChan: InternalChannel<obj>, blockingWorkItemChan: InternalChannel<WorkItem>) =

    /// <summary>
    /// Creates a new channel with a unique identifier.
    /// </summary>
    new() = Channel(Guid.NewGuid (), InternalChannel<obj> (), InternalChannel<WorkItem> ())

    /// <summary>
    /// Sends a message to the channel and succeeds with the message.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The message to send.</param>
    /// <returns>An FIO effect that sends the message and returns it, or an error.</returns>
    member this.Send<'R, 'E> (msg: 'R) : FIO<'R, 'E> =
        SendChan (msg, this)

    /// <summary>
    /// Receives a message from the channel and succeeds with it.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that receives a message from the channel or returns an error.</returns>
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
        
    member internal _.Internal =
        resChan

    member internal _.SendAsync (msg: 'R) =
        resChan.AddAsync msg

    member internal _.ReceiveAsync () =
        task {
            let! res = resChan.TakeAsync()
            return res :?> 'R
        }

    member internal _.AddBlockingWorkItem blockingItem =
        blockingWorkItemChan.AddAsync blockingItem

    member internal _.RescheduleNextBlockingWorkItem (activeWorkItemChan: InternalChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>
            if blockingWorkItemChan.TryTake &workItem then
                do! activeWorkItemChan.AddAsync workItem
        }

    member internal _.BlockingWorkItemCount =
        blockingWorkItemChan.Count

    member internal _.Upcast () =
        Channel<obj> (id, resChan, blockingWorkItemChan)

and channel<'R> = Channel<'R>

/// <summary>
/// Builds a functional effect that can either succeed with a result or fail with an error when interpreted by a runtime.
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
    | ConcurrentEffect of eff: FIO<obj, obj> * fiber: obj * ifiber: InternalFiber
    | ConcurrentTPLTask of lazyTask: (unit -> Task) * onError: (exn -> 'E) * fiber: obj * ifiber: InternalFiber
    | ConcurrentGenericTPLTask of lazyTask: (unit -> Task<obj>) * onError: (exn -> 'E) * fiber: obj * ifiber: InternalFiber
    | AwaitFiber of ifiber: InternalFiber
    | AwaitTPLTask of task: Task * onError: (exn -> 'E)
    | AwaitGenericTPLTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'R, 'E>)
    | ChainError of eff: FIO<'R, obj> * cont: (obj -> FIO<'R, 'E>)

    /// <summary>
    /// Succeeds immediately with the provided result value.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="res">The result value to succeed with.</param>
    /// <returns>An FIO effect that succeeds with the given result.</returns>
    static member Succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        Success res

    /// <summary>
    /// Fails immediately with the provided error value.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="err">The error value to fail with.</param>
    /// <returns>An FIO effect that fails with the given error.</returns>
    static member Fail<'R, 'E> (err: 'E) : FIO<'R, 'E> =
        Failure err

    /// <summary>
    /// Interrupts the effect with the provided message.
    /// </summary>
    /// typeparam name="R">The result type.</typeparam>
    /// typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The interruption message.</param>
    /// <returns>An FIO effect that is interrupted with the given message.</returns>
    static member Interrupt<'R, 'E> (?cause: InterruptionCause, ?msg: string) : FIO<'R, 'E> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted"
        Interruption (cause, msg)

    /// <summary>
    /// Converts a function into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="func">The function to execute.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that executes the function and returns its result or an error.</returns>
    static member FromFunc<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        Action (func, onError)

    /// <summary>
    /// Converts a function into an effect with a default onError handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="func">The function to execute.</param>
    /// <returns>An FIO effect that executes the function and returns its result or an exception.</returns>
    static member inline FromFunc<'R, 'E> (func: unit -> 'R) : FIO<'R, exn> =
        FIO.FromFunc<'R, exn> (func, id)

    /// <summary>
    /// Converts a Result value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="res">The Result value to convert.</param>
    /// <returns>An FIO effect that succeeds with the Ok value or fails with the Error value.</returns>
    static member inline FromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> FIO.Succeed res
        | Error err -> FIO.Fail err
    
    /// <summary>
    /// Converts an Option value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="opt">The Option value to convert.</param>
    /// <param name="onNone">A function to produce an error if the option is None.</param>
    /// <returns>An FIO effect that succeeds with the Some value or fails with the error from onNone.</returns>
    static member inline FromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> FIO.Succeed res
        | None -> FIO.Fail <| onNone ()

    /// <summary>
    /// Converts a Choice value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="choice">The Choice value to convert.</param>
    /// <returns>An FIO effect that succeeds with the Choice1Of2 value or fails with the Choice2Of2 value.</returns>
    static member inline FromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> FIO.Succeed res
        | Choice2Of2 err -> FIO.Fail err

    /// <summary>
    /// Awaits a Task and turns it into an effect.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes when the Task completes or fails with an error.</returns>
    static member AwaitTask<'R, 'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTPLTask (task, onError)

    /// <summary>
    /// Awaits a Task and turns it into an effect with a default onError handler.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <returns>An FIO effect that completes when the Task completes or fails with an exception.</returns>
    static member inline AwaitTask<'R, 'E> (task: Task) : FIO<unit, exn> =
        FIO.AwaitTask<unit, exn> (task, id)

    /// <summary>
    /// Awaits a generic Task and turns it into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes with the Task result or fails with an error.</returns>
    static member AwaitGenericTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitGenericTPLTask (upcastTask task, onError)

    /// <summary>
    /// Awaits a generic Task and turns it into an effect with a default onError handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <returns>An FIO effect that completes with the Task result or fails with an exception.</returns>
    static member inline AwaitGenericTask<'R, 'E> (task: Task<'R>) : FIO<'R, exn> =
        FIO.AwaitGenericTask<'R, exn> (task, id)

    /// <summary>
    /// Awaits an Async computation and turns it into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes with the Async result or fails with an error.</returns>
    static member inline AwaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E>  =
        FIO.AwaitGenericTask<'R, 'E> (Async.StartAsTask async, onError)

    /// <summary>
    /// Awaits an Async computation and turns it into an effect with a default onError handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="async">The Async computation to await.</param>
    /// <returns>An FIO effect that completes with the Async result or fails with an exception.</returns>
    static member inline AwaitAsync<'R, 'E> (async: Async<'R>) : FIO<'R, exn> =
        FIO.AwaitAsync<'R, exn> (async, id)

    /// <summary>
    /// Converts a Task into a Fiber.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="lazyTask">A function that produces the Task to run.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that starts the Task in a Fiber and returns the Fiber.</returns>
    static member FromTask<'R, 'E> (lazyTask: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E> ()
        ConcurrentTPLTask ((fun () -> lazyTask ()), onError, fiber, fiber.Internal)

    /// <summary>
    /// Converts a Task into a Fiber with a default onError handler.
    /// </summary>
    /// <param name="lazyTask">A function that produces the Task to run.</param>
    /// <returns>An FIO effect that starts the Task in a Fiber and returns the Fiber.</returns>
    static member inline FromTask<'R, 'E> (lazyTask: unit -> Task) : FIO<Fiber<unit, exn>, exn> =
        FIO.FromTask<unit, exn> (lazyTask, id)
    
    /// <summary>
    /// Converts a generic Task into a Fiber.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="lazyTask">A function that produces the Task to run.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that starts the Task in a Fiber and returns the Fiber.</returns>
    static member FromGenericTask<'R, 'E> (lazyTask: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E> ()
        ConcurrentGenericTPLTask ((fun () -> upcastTask (lazyTask ())), onError, fiber, fiber.Internal)

    /// <summary>
    /// Converts a generic Task into a Fiber with a default onError handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="lazyTask">A function that produces the Task to run.</param>
    /// <returns>An FIO effect that starts the Task in a Fiber and returns the Fiber.</returns>
    static member inline FromGenericTask<'R> (lazyTask: unit -> Task<'R>) : FIO<Fiber<'R, exn>, exn> =
        FIO.FromGenericTask<'R, exn> (lazyTask, id)

    /// <summary>
    /// Acquire-release pattern for resource management with custom error types.
    /// Ensures the release effect runs after the use effect, even if an error occurs.
    /// This is the general-purpose resource management primitive that works with arbitrary error types.
    /// </summary>
    /// <typeparam name="'A">The resource type.</typeparam>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">The effect that releases the resource.</param>
    /// <param name="use">The effect that uses the resource.</param>
    /// <returns>An FIO effect that acquires, uses, and releases the resource.</returns>
    static member AcquireRelease<'A, 'R, 'E> (acquire: FIO<'A, 'E>) (release: 'A -> FIO<unit, 'E>) (useResource: 'A -> FIO<'R, 'E>) : FIO<'R, 'E> =
        acquire.Bind(fun resource ->
            (useResource resource)
                .Bind(fun res -> (release resource).Bind(fun _ -> FIO.Succeed res))
                .BindError(fun err -> (release resource).Bind(fun _ -> FIO.Fail err)))

    /// <summary>
    /// Interprets an effect concurrently and returns the fiber interpreting it. The fiber can be awaited for the result of the effect.
    /// </summary>
    /// <typeparam name="R">The result type of the fiber.</typeparam>
    /// <typeparam name="E">The error type of the fiber.</typeparam>
    /// <typeparam name="E1">The error type.</typeparam>
    /// <returns>An FIO effect that starts the effect in a new fiber and returns the fiber.</returns>
    member this.Fork<'R, 'E, 'E1> () : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ConcurrentEffect (this.Upcast (), fiber, fiber.Internal)

    /// <summary>
    /// Binds a continuation to the result of an effect. If the effect fails, the error is immediately returned.
    /// </summary>
    /// <typeparam name="R">The result type of the applied effect.</typeparam>
    /// <typeparam name="R1">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cont">The continuation function to apply to the result.</param>
    /// <returns>An FIO effect that applies the continuation to the result or returns an error.</returns>
    member this.Bind<'R, 'R1, 'E> (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess (this.UpcastResult (), fun res -> cont (res :?> 'R))

    /// <summary>
    /// Binds a continuation to the error of an effect. If the effect succeeds, the result is immediately returned.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the applied effect.</typeparam>
    /// <typeparam name="E1">The error type.</typeparam>
    /// <param name="cont">The continuation function to apply to the error.</param>
    /// <returns>An FIO effect that applies the continuation to the error or returns the result.</returns>
    member this.BindError<'R, 'E, 'E1> (cont: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError (this.UpcastError (), fun err -> cont (err :?> 'E))

    /// <summary>
    /// Maps a function over the result of an effect.
    /// </summary>
    /// <typeparam name="R">The result type of the applied effect.</typeparam>
    /// <typeparam name="R1">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cont">The function to apply to the result.</param>
    /// <returns>An FIO effect that applies the function to the result or returns an error.</returns>
    member inline this.Map<'R, 'R1, 'E> (cont: 'R -> 'R1) : FIO<'R1, 'E> =
        this.Bind <| fun res ->
            FIO.Succeed <| cont res

    /// <summary>
    /// Maps a function over the error of an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the applied effect.</typeparam>
    /// <typeparam name="E1">The error type.</typeparam>
    /// <param name="cont">The function to apply to the error.</param>
    /// <returns>An FIO effect that applies the function to the error or returns the result.</returns>
    member inline this.MapError<'R, 'E, 'E1> (cont: 'E -> 'E1) : FIO<'R, 'E1> =
        this.BindError <| fun err ->
            FIO.Fail <| cont err

    /// <summary>
    /// Sequences two effects, ignoring the result of the first effect. If the first effect fails, the error is immediately returned.
    /// </summary>
    /// <typeparam name="R">The result type of the applied effect.</typeparam>
    /// <typeparam name="R1">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to sequence after the first.</param>
    /// <returns>An FIO effect that sequences the two effects, returning the result of the second or an error.</returns>
    member inline this.Then<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.Bind <| fun _ -> eff

    /// <summary>
    /// Sequences two effects, ignoring the error of the first effect. If the first effect succeeds, the result is immediately returned.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the applied effect.</typeparam>
    /// <typeparam name="E1">The error type.</typeparam>
    /// <param name="eff">The effect to sequence after the first error.</param>
    /// <returns>An FIO effect that sequences the two effects, returning the result or the error of the second effect.</returns>
    member inline this.ThenError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E1> =
        this.BindError <| fun _ -> eff

    /// <summary>
    /// Combines two effects: one produces a result function and the other produces a result value. The function is applied to the value, and the result is returned. Errors are immediately returned if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the applied effect.</typeparam>
    /// <typeparam name="R1">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply.</param>
    /// <returns>An FIO effect that applies the function to the result or returns an error.</returns>
    member inline this.Apply<'R, 'R1, 'E> (eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
        eff.Bind <| this.Map

    /// <summary>
    /// Combines two effects: one produces an error function and the other produces an error value. The function is applied to the value, and the error is returned.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the applied effect.</typeparam>
    /// <typeparam name="E1">The error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply to the error.</param>
    /// <returns>An FIO effect that applies the function to the error or returns the result.</returns>
    member inline this.ApplyError<'R, 'E, 'E1> (eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
        eff.BindError <| this.MapError

    /// <summary>
    /// Combines two effects and succeeds with a tuple of their results when both complete. Errors are immediately returned if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to zip with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of the results or an error.</returns>
    member inline this.Zip<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.Bind <| fun res ->
            eff.Bind <| fun res' ->
                FIO.Succeed (res, res')

    /// <summary>
    /// Combines two effects and succeeds with a tuple of their errors when both complete.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to zip errors with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of the errors or the result.</returns>
    member inline this.ZipError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.BindError <| fun err ->
            eff.BindError <| fun err' ->
                FIO.Fail (err, err')

    /// <summary>
    /// Interprets two effects concurrently and succeeds with a tuple of their results when both complete. If either effect fails, the error is immediately returned.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to run in parallel with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of the results or an error.</returns>
    member inline this.Parallel<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().Bind <| fun fiber ->
            this.Bind <| fun res ->
                fiber.Await().Bind <| fun res' ->
                     FIO.Succeed (res, res')

    /// <summary>
    /// Interprets two effects concurrently and succeeds with a tuple of their errors when both complete.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to run in parallel with this effect for errors.</param>
    /// <returns>An FIO effect that returns a tuple of the errors or the result.</returns>
    member inline this.ParallelError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        eff.Fork().Bind <| fun fiber ->
            this.BindError <| fun err ->
                fiber.Await().BindError <| fun err' ->
                    FIO.Fail (err, err')
                     
    /// <summary>
    /// Retries this effect up to maxRetries times with exponential backoff on failure.
    /// Executes onEachRetry callback before each retry attempt (not before the initial attempt).
    /// The delay doubles after each failed attempt, starting from the initial delay.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="delayBetweenRetriesMillis">Initial delay in milliseconds before the first retry.</param>
    /// <param name="maxRetries">Maximum number of retry attempts.</param>
    /// <param name="onEachRetry">Callback executed before each retry with current retry number (0-indexed) and maxRetries.</param>
    /// <returns>An FIO effect that retries on failure with exponential backoff, or returns the final error if all retries are exhausted.</returns>
    member this.Retry (delayBetweenRetriesMillis: float) (maxRetries: int) (onEachRetry: int -> int -> FIO<unit, 'E>): FIO<'R, 'E> =
        let rec loop retry (delayBetweenRetriesMillis: float) =
            this.BindError <| fun err ->
                if retry >= maxRetries then
                    FIO<'R, 'E>.Fail err
                else
                    let nextDelay = delayBetweenRetriesMillis * 2.0
                    let delayEff = FIO<unit, 'E>.AwaitTask(Task.Delay(TimeSpan.FromMilliseconds delayBetweenRetriesMillis), fun _ -> err)
                    (onEachRetry retry maxRetries)
                        .Then(delayEff
                            .Then (loop (retry + 1) nextDelay))
        loop 0 delayBetweenRetriesMillis

    /// <summary>
    /// Times out this effect after the specified duration, failing with the onTimeout error if the duration is exceeded.
    /// Uses Task.WhenAny to race the original effect against a timeout timer - whichever completes first wins.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="durationMillis">Maximum duration in milliseconds to wait before timing out.</param>
    /// <param name="onTimeout">Function that produces the error to fail with if timeout occurs.</param>
    /// <returns>An FIO effect that returns the result if completed within duration, or fails with timeout error.</returns>
    member this.Timeout (durationMillis: float) (onTimeout: unit -> 'E) : FIO<'R, 'E> =
        this.Fork().Bind <| fun originalFiber ->
            // Race the original effect against a timeout using Task.WhenAny
            FIO.AwaitGenericTask<'R, 'E>(
                task {
                    let originalTask = originalFiber.Task()
                    let timeoutTask = task {
                        do! Task.Delay(TimeSpan.FromMilliseconds durationMillis)
                        return Error (onTimeout ())
                    }
                    let! completedTask = Task.WhenAny(originalTask, timeoutTask)
                    return! completedTask
                }
            ).MapError(fun _ -> onTimeout())
             .Bind FIO.FromResult

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
        | ConcurrentEffect (eff, fiber, ifiber) ->
            ConcurrentEffect (eff, fiber, ifiber)
        | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
            ConcurrentTPLTask (lazyTask, onError, fiber, ifiber)
        | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
            ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber)
        | AwaitFiber ifiber ->
            AwaitFiber ifiber
        | AwaitTPLTask (task, onError) ->
            AwaitTPLTask (task, onError)
        | AwaitGenericTPLTask (task, onError) ->
            AwaitGenericTPLTask (task, onError)
        | ChainSuccess (eff, cont) ->
            ChainSuccess (eff, fun res -> (cont res).UpcastResult ())
        | ChainError (eff, cont) ->
            ChainError (eff.UpcastResult (), fun err -> (cont err).UpcastResult ())

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
        | ConcurrentEffect (eff, fiber, ifiber) ->
            ConcurrentEffect (eff, fiber, ifiber)
        | ConcurrentTPLTask (lazyTask, onError, fiber, ifiber) ->
            ConcurrentTPLTask (lazyTask, upcastOnError onError, fiber, ifiber)
        | ConcurrentGenericTPLTask (lazyTask, onError, fiber, ifiber) ->
            ConcurrentGenericTPLTask (lazyTask, upcastOnError onError, fiber, ifiber)
        | AwaitFiber ifiber ->
            AwaitFiber ifiber
        | AwaitTPLTask (task, onError) ->
            AwaitTPLTask (task, upcastOnError onError)
        | AwaitGenericTPLTask (task, onError) ->
            AwaitGenericTPLTask (task, upcastOnError onError)
        | ChainSuccess (eff, cont) ->
            ChainSuccess (eff.UpcastError (), fun res -> (cont res).UpcastError ())
        | ChainError (eff, cont) ->
            ChainError (eff, fun err -> (cont err).UpcastError ())

    member internal this.Upcast () : FIO<obj, obj> =
        this.UpcastResult().UpcastError()
