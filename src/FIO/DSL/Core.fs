/// Core types and primitives for the FIO effect system.
[<AutoOpen>]
module FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

/// Internal continuation representation for effect chaining.
type internal Cont =
    /// Continuation invoked on successful result.
    | SuccessCont of cont: (obj -> FIO<obj, obj>)
    /// Continuation invoked on error result.
    | FailureCont of cont: (obj -> FIO<obj, obj>)
    /// Finalizer effect that runs regardless of outcome.
    | FinalizerCont of finalizer: FIO<obj, obj>

/// Internal stack frame for continuation tracking during effect execution.
and [<Struct>] internal ContStackFrame =
    /// Gets the continuation stored in this frame.
    val Cont: Cont

    /// Creates a new continuation stack frame.
    new cont = { Cont = cont }

/// Internal stack of continuations for effect execution.
and internal ContStack = Stack<ContStackFrame>

/// Internal work item representing an effect to be executed along with its context and continuation stack.
and internal WorkItem =
    {
        /// The current effect to be evaluated.
        mutable Eff: FIO<obj, obj>
        /// The fiber context associated with this work item.
        mutable FiberContext: FiberContext
        /// The continuation stack for chained effects.
        mutable Stack: ContStack
        /// Counter tracking nested interruption suppression depth.
        mutable InterruptionSuppressed: int
    }

/// Discriminated union representing effects that are blocked waiting for resources.
/// Used by CooperativeRuntime's blocking worker for linear-time polling.
and internal BlockingItem =
    /// A work item blocked waiting for a channel message.
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    /// A work item blocked waiting for a fiber to complete.
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

/// Result of a fiber execution, distinguishing between success, failure, and interruption.
and FiberResult<'R, 'E> =
    /// The fiber completed successfully with a result.
    | Succeeded of result: 'R
    /// The fiber failed with an error.
    | Failed of error: 'E
    /// The fiber was interrupted before completion.
    | Interrupted of ex: FiberInterruptedException

/// Thread-safe unbounded channel for message passing.
and [<Sealed>] internal UnboundedChannel<'R>(id: Guid) =
    let chan = Channel.CreateUnbounded<'R>()

    /// Creates a new unbounded channel with a generated unique identifier.
    new() = UnboundedChannel(Guid.NewGuid())

    /// Gets the unique identifier.
    member internal _.Id = id

    /// Gets the current message count.
    member internal _.Count = chan.Reader.Count

    /// Adds a message asynchronously.
    member internal _.AddAsync msg = chan.Writer.WriteAsync msg

    /// Takes a message asynchronously.
    member internal _.TakeAsync() = chan.Reader.ReadAsync()

    /// Tries to take a message without blocking.
    member internal _.TryTake(res: byref<'R>) = chan.Reader.TryRead &res

    /// Waits asynchronously until a message is available, observing the given cancellation token.
    member internal _.WaitToTakeAsync(ct: CancellationToken) = chan.Reader.WaitToReadAsync ct

    /// Clears all messages from the channel.
    member internal _.Clear() =
        let mutable item = Unchecked.defaultof<'R>

        while chan.Reader.TryRead &item do
            ()

/// Internal state of a fiber's execution context lifecycle.
and private FiberContextState =
    /// The fiber is currently executing.
    | Running = 0
    /// The fiber has completed successfully or with an error.
    | Completed = 1
    /// The fiber has been interrupted.
    | Interrupted = 2

/// Internal fiber execution context managing state, result, cancellation, and blocking work items.
and [<Sealed>] internal FiberContext() =
    let id = Guid.NewGuid()
    let mutable state = int FiberContextState.Running
    let blockingWorkItemChan = UnboundedChannel<WorkItem>()

    let resTcs =
        TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously

    let cts = new CancellationTokenSource()

    [<VolatileField>]
    let mutable registrations: ConcurrentBag<IDisposable> = null

    [<VolatileField>]
    let mutable disposed = false

    /// Gets the unique identifier.
    member internal _.Id = id

    /// Gets the task representing the fiber's result.
    member internal _.Task = resTcs.Task

    /// Gets the cancellation token for cooperative cancellation.
    member internal _.CancellationToken = cts.Token

    /// Completes the fiber with the given result.
    member internal this.Complete res =
        if
            Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running) = int
                FiberContextState.Running
        then
            this.DisposeRegistrations()
            resTcs.TrySetResult res |> ignore

    /// Completes the fiber and reschedules blocking work items.
    member internal this.CompleteAndReschedule(res, activeWorkItemChan) =
        task {
            let oldState =
                Interlocked.CompareExchange(&state, int FiberContextState.Completed, int FiberContextState.Running)

            if oldState = int FiberContextState.Running then
                this.DisposeRegistrations()
                resTcs.TrySetResult res |> ignore

                if blockingWorkItemChan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
            elif oldState = int FiberContextState.Interrupted then
                this.DisposeRegistrations()
                resTcs.TrySetResult res |> ignore

                if blockingWorkItemChan.Count > 0 then
                    do! this.RescheduleBlockingWorkItems activeWorkItemChan
        }

    /// Interrupts the fiber with the specified cause and message.
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

    /// Adds a blocking work item to the queue.
    member internal _.AddBlockingWorkItem blockingWorkItem =
        blockingWorkItemChan.AddAsync blockingWorkItem

    /// Reschedules all blocking work items to the active queue.
    member internal _.RescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>

            while blockingWorkItemChan.TryTake &workItem do
                do! activeWorkItemChan.AddAsync workItem
        }

    /// Tries to reschedule blocking work items if fiber is in a terminal state.
    member internal this.TryRescheduleBlockingWorkItems(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.IsTerminal() then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

    /// Adds a disposable registration to be cleaned up when the fiber completes.
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

    /// Gets whether the fiber has completed.
    member internal _.IsCompleted() =
        Volatile.Read &state = int FiberContextState.Completed

    /// Gets whether the fiber has been interrupted.
    member internal _.IsInterrupted() =
        Volatile.Read &state = int FiberContextState.Interrupted

    /// Gets whether the fiber has reached a terminal state (completed or interrupted).
    member internal _.IsTerminal() =
        Volatile.Read &state <> int FiberContextState.Running

    /// Disposes all tracked registrations.
    member private _.DisposeRegistrations() =
        let bag = Volatile.Read &registrations

        if not (isNull bag) then
            let mutable registration = Unchecked.defaultof<_>

            while bag.TryTake &registration do
                try
                    registration.Dispose()
                with :? ObjectDisposedException ->
                    ()

    /// Releases managed and unmanaged resources.
    member private _.Dispose disposing =
        if not disposed then
            disposed <- true

            if disposing then
                cts.Dispose()

    interface IDisposable with
        /// Disposes the fiber context and its cancellation token source.
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    /// Releases unmanaged resources during garbage collection.
    override this.Finalize() = this.Dispose false

/// A lightweight, cooperative thread of execution that can be awaited for its result.
and [<Sealed>] Fiber<'R, 'E> internal () =
    let fiberContext = new FiberContext()

    /// Gets the unique identifier.
    member _.Id = fiberContext.Id

    /// Gets the cancellation token for cooperative cancellation.
    member _.CancellationToken = fiberContext.CancellationToken

    /// Returns the fiber's result as a Task&lt;FiberResult&lt;'R, 'E&gt;&gt;.
    member _.Task() =
        task {
            match! fiberContext.Task with
            | Ok res -> return Succeeded(res :?> 'R)
            | Error err ->
                match err with
                | :? FiberInterruptedException as ex -> return Interrupted ex
                | _ -> return Failed(err :?> 'E)
        }

    /// Awaits the fiber's completion and returns its result.
    member _.Join() : FIO<'R, 'E> = JoinFiber fiberContext

    /// Interrupts the fiber with the specified cause and message.
    member _.Interrupt(?cause: InterruptionCause, ?msg: string) : FIO<unit, 'E> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        InterruptFiber(cause, msg, fiberContext)

    /// Awaits the fiber's completion and returns its FiberResult without re-raising errors or propagating interruptions.
    member this.Await<'E2>() : FIO<FiberResult<'R, 'E>, 'E2> =
        AwaitTask(upcastTask (this.Task()), fun ex -> raise ex)

    /// Interrupts the fiber and awaits its terminal result, unlike the fire-and-forget Interrupt().
    member this.InterruptAwait<'E2>(?cause: InterruptionCause, ?msg: string) : FIO<FiberResult<'R, 'E>, 'E2> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        Action((fun () -> fiberContext.Interrupt(cause, msg)), fun ex -> raise ex).FlatMap(fun () -> this.Await())

    /// Non-blocking check returning Some(result) if terminal, None if still running.
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

    /// Awaits completion and handles all three outcome cases.
    /// <param name="onSucceeded">Handler for successful completion.</param>
    /// <param name="onFailed">Handler for failure.</param>
    /// <param name="onInterrupted">Handler for interruption.</param>
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

    /// Gets whether the fiber has completed.
    member _.IsCompleted() = fiberContext.IsCompleted()

    /// Gets whether the fiber has been interrupted.
    member _.IsInterrupted() = fiberContext.IsInterrupted()

    /// Gets whether the fiber has reached a terminal state (completed or interrupted).
    member _.IsTerminal() = fiberContext.IsTerminal()

    /// Synchronously blocks and returns the FiberResult. Prefer Join() or Task() instead.
    member this.UnsafeResult() =
        this.Task() |> Async.AwaitTask |> Async.RunSynchronously

    /// Synchronously blocks and returns the success value, or throws on failure/interruption.
    /// <exception cref="InvalidOperationException">Thrown if the fiber did not succeed.</exception>
    member this.UnsafeSuccess() =
        match this.UnsafeResult() with
        | Succeeded res -> res
        | Failed err -> raise (InvalidOperationException $"Fiber failed with error: {err}")
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Synchronously blocks and returns the error value, or throws on success/interruption.
    /// <exception cref="InvalidOperationException">Thrown if the fiber did not fail.</exception>
    member this.UnsafeError() =
        match this.UnsafeResult() with
        | Succeeded res -> raise (InvalidOperationException $"Fiber succeeded with result: {res}")
        | Failed err -> err
        | Interrupted ex -> raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    /// Synchronously blocks and prints the fiber's result to the console.
    member this.UnsafePrintResult() = printfn "%A" (this.UnsafeResult())

    /// Returns the fiber's unique identifier as a string.
    override this.ToString() = this.Id.ToString()

    /// Gets the fiber context.
    member internal _.Context = fiberContext

    interface IDisposable with

        /// Disposes the fiber and its underlying context.
        member _.Dispose() = (fiberContext :> IDisposable).Dispose()

/// A typed communication queue for sending and receiving messages.
and [<Sealed>] Channel<'R>
    private (id: Guid, resChan: UnboundedChannel<obj>, blockingWorkItemChan: UnboundedChannel<WorkItem>) =
    let upcastLock = obj ()

    [<VolatileField>]
    let mutable upcastChan = None

    let mutable signalProcessing = 0

    /// Creates a new channel with a generated unique identifier.
    new() = Channel(Guid.NewGuid(), UnboundedChannel<obj>(), UnboundedChannel<WorkItem>())

    /// Gets the unique identifier.
    member _.Id = id

    /// Gets the current message count.
    member _.Count = resChan.Count

    /// Sends a message to the channel.
    member this.Send<'E> msg : FIO<'R, 'E> = SendChan(msg, this)

    /// Receives a message from the channel (blocking if empty).
    member this.Receive<'E>() : FIO<'R, 'E> = ReceiveChan this

    /// Sends a message asynchronously.
    member internal _.SendAsync(msg: 'R) = resChan.AddAsync msg

    /// Receives a message asynchronously.
    member internal _.ReceiveAsync() =
        task {
            let! res = resChan.TakeAsync()
            return res :?> 'R
        }

    /// Adds a blocking work item.
    member internal _.AddBlockingWorkItem blockingItem =
        blockingWorkItemChan.AddAsync blockingItem

    /// Tries to reschedule next blocking work item if available.
    member internal _.TryRescheduleNextBlockingWorkItem(activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let mutable workItem = Unchecked.defaultof<_>

            if blockingWorkItemChan.TryTake &workItem then
                do! activeWorkItemChan.AddAsync workItem
                return true
            else
                return false
        }

    /// Attempts to transition the channel to "signal processing" state.
    member internal _.TryBeginSignalProcessing() =
        Interlocked.CompareExchange(&signalProcessing, 1, 0) = 0

    /// Ends signal processing for the channel.
    member internal _.EndSignalProcessing() = Volatile.Write(&signalProcessing, 0)

    /// Gets the count of blocking work items.
    member internal _.BlockingWorkItemCount = blockingWorkItemChan.Count

    /// Gets the underlying unbounded channel.
    member internal _.UnboundedChannel = resChan

    /// Upcasts the channel to Channel&lt;obj&gt;.
    member internal _.Upcast() =
        match upcastChan with
        | Some cached -> cached
        | None ->
            lock upcastLock (fun () ->
                match upcastChan with
                | Some cached -> cached
                | None ->
                    let created = Channel<obj>(id, resChan, blockingWorkItemChan)
                    upcastChan <- Some created
                    created)

/// A functional effect that succeeds with a result or fails with an error when interpreted.
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
    | ForkTask of
        taskFactory: (unit -> Task<obj>) *
        onError: (exn -> 'E) *
        fiber: obj *
        fiberContext: FiberContext
    | JoinFiber of fiberContext: FiberContext
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of eff: FIO<obj, 'E> * cont: (obj -> FIO<'R, 'E>)
    | ChainError of eff: FIO<'R, obj> * cont: (obj -> FIO<'R, 'E>)
    | OnFinalize of eff: FIO<'R, 'E> * finalizer: FIO<obj, obj>
    | ResumeInterrupt of err: obj
    | FinalizerResult of result: Result<obj, obj>

    /// Iteratively flattens nested OnFinalize nodes to preserve stack safety.
    static member inline private FlattenFinalizers
        (eff: FIO<'A, 'B>, outerFin: FIO<obj, obj>, upcastInner: FIO<'A, 'B> -> FIO<'C, 'D>)
        : FIO<'C, 'D> =
        let finalizers = ResizeArray<FIO<obj, obj>>()
        finalizers.Add outerFin
        let mutable current = eff
        let mutable stopped = false

        while not stopped do
            match current with
            | OnFinalize(innerEff, innerFin) ->
                finalizers.Add innerFin
                current <- innerEff
            | _ -> stopped <- true

        let mutable rebuilt = upcastInner current

        for i = finalizers.Count - 1 downto 0 do
            rebuilt <- OnFinalize(rebuilt, finalizers[i])

        rebuilt

    /// Iteratively flattens nested ChainSuccess nodes to preserve stack safety.
    static member inline private FlattenChainSuccess
        (
            eff: FIO<obj, 'B>,
            upcastInner: FIO<obj, 'B> -> FIO<obj, obj>,
            wrapCont: (obj -> FIO<obj, 'B>) -> (obj -> FIO<obj, obj>)
        ) : FIO<obj, obj> =
        let innerConts = ResizeArray<obj -> FIO<obj, 'B>>()
        let mutable current = eff
        let mutable stopped = false

        while not stopped do
            match current with
            | ChainSuccess(innerEff, innerCont) ->
                innerConts.Add innerCont
                current <- innerEff
            | _ -> stopped <- true

        let mutable rebuilt = upcastInner current

        for i = innerConts.Count - 1 downto 0 do
            rebuilt <- ChainSuccess(rebuilt, wrapCont innerConts[i])

        rebuilt

    /// Iteratively flattens nested ChainError nodes to preserve stack safety.
    static member inline private FlattenChainError
        (
            eff: FIO<'A, obj>,
            upcastInner: FIO<'A, obj> -> FIO<obj, obj>,
            wrapCont: (obj -> FIO<'A, obj>) -> (obj -> FIO<obj, obj>)
        ) : FIO<obj, obj> =
        let innerConts = ResizeArray<obj -> FIO<'A, obj>>()
        let mutable current = eff
        let mutable stopped = false

        while not stopped do
            match current with
            | ChainError(innerEff, innerCont) ->
                innerConts.Add innerCont
                current <- innerEff
            | _ -> stopped <- true

        let mutable rebuilt = upcastInner current

        for i = innerConts.Count - 1 downto 0 do
            rebuilt <- ChainError(rebuilt, wrapCont innerConts[i])

        rebuilt

    /// Executes this effect concurrently in a new Fiber.
    member this.Fork<'E1>() : FIO<Fiber<'R, 'E>, 'E1> =
        let fiber = new Fiber<'R, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Context)

    /// Chains this effect with a continuation function (monadic bind).
    member this.FlatMap<'R1>(cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        ChainSuccess(this.UpcastResult(), fun res -> cont (res :?> 'R))

    /// Handles errors with a recovery function.
    member this.CatchAll<'E1>(onError: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        ChainError(this.UpcastError(), fun err -> onError (err :?> 'E))

    /// Runs a finalizer after this effect regardless of outcome.
    /// Main effect errors take precedence; finalizer errors only surface if the main effect succeeded.
    member this.Ensuring(finalizer: FIO<unit, 'E>) : FIO<'R, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// Upcasts the result type to obj (for runtime use only).
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
        | ForkTask(taskFactory, onError, fiber, fiberContext) ->
            ForkTask(taskFactory, onError, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, onError)
        | ChainSuccess(eff, cont) -> ChainSuccess(eff, fun res -> cont(res).UpcastResult())
        | ChainError(eff, cont) ->
            let rebuilt =
                FIO<obj, 'E>.FlattenChainError(
                    eff,
                    (fun e -> e.UpcastResult()),
                    (fun ic -> fun err -> ic(err).UpcastResult())
                )

            ChainError(rebuilt, fun err -> cont(err).UpcastResult())
        | OnFinalize(eff, finalizer) ->
            FIO<obj, 'E>.FlattenFinalizers(eff, finalizer, fun e -> e.UpcastResult())
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res

    /// Upcasts the error type to obj (for runtime use only).
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
            let rebuilt =
                FIO<'R, obj>.FlattenChainSuccess(
                    eff,
                    (fun e -> e.UpcastError()),
                    (fun ic -> fun res -> ic(res).UpcastError())
                )

            ChainSuccess(rebuilt, fun res -> cont(res).UpcastError())
        | ChainError(eff, cont) -> ChainError(eff, fun err -> cont(err).UpcastError())
        | OnFinalize(eff, finalizer) ->
            FIO<'R, obj>.FlattenFinalizers(eff, finalizer, fun e -> e.UpcastError())
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res

    /// Upcasts both the result and error types to obj (for runtime use only).
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
            let rebuilt =
                FIO<obj, obj>.FlattenChainSuccess(
                    eff,
                    (fun e -> e.UpcastError()),
                    (fun ic -> fun res -> ic(res).UpcastError())
                )

            ChainSuccess(rebuilt, fun res -> cont(res).UpcastBoth())
        | ChainError(eff, cont) ->
            let rebuilt =
                FIO<obj, obj>.FlattenChainError(
                    eff,
                    (fun e -> e.UpcastResult()),
                    (fun ic -> fun err -> ic(err).UpcastResult())
                )

            ChainError(rebuilt, fun err -> cont(err).UpcastBoth())
        | OnFinalize(eff, finalizer) ->
            FIO<obj, obj>.FlattenFinalizers(eff, finalizer, fun e -> e.UpcastBoth())
        | ResumeInterrupt err -> ResumeInterrupt err
        | FinalizerResult res -> FinalizerResult res
