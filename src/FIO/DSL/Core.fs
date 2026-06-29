namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

type internal Rethrow<'A>() =
    static let instance: exn -> 'A = fun ex -> raise ex
    static member Instance = instance

[<Struct>]
type internal PostFinalizerSaved =
    | PostFinalizerSucceeded of value: obj
    | PostFinalizerFailed of error: obj
    | PostFinalizerInterrupted of error: obj

and [<Struct>] internal Cont =
    | SuccessCont of successCont: (obj -> FIO<obj, obj>)
    | FailureCont of failureCont: (obj -> FIO<obj, obj>)
    | FinalizerCont of finalizer: FIO<obj, obj>
    | PostFinalizerCont of saved: PostFinalizerSaved

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

and [<Sealed; AllowNullLiteral>] internal MailboxQueue<'A>() =
    let channel = Channel.CreateUnbounded<'A>()
    let reader = channel.Reader
    let writer = channel.Writer

    member internal _.Count =
        reader.Count

    member internal _.WriteAsync value =
        writer.WriteAsync value

    member internal _.ReadAsync () =
        reader.ReadAsync()

    member internal _.TryRead (value: byref<'A>) =
        reader.TryRead &value

    member internal _.WaitToReadAsync (cancelToken: CancellationToken) =
        reader.WaitToReadAsync cancelToken

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

and [<Sealed>] internal FiberContext() =
    let id = Guid.NewGuid()
    let mutable state = int FiberContextState.Running

    [<VolatileField>]
    let mutable blockingWorkItemQueue: MailboxQueue<BlockingWaiter> = null

    let resultSource =
        TaskCompletionSource<Result<obj, obj>> TaskCreationOptions.RunContinuationsAsynchronously

    let cancelSource = new CancellationTokenSource()

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
        resultSource.Task

    member internal _.CancellationToken =
        cancelSource.Token

    member internal this.SetOnTerminal (callback: unit -> unit) =
        onTerminalCallback <- ValueSome callback
        if this.IsTerminal() then
            this.InvokeOnTerminal()

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

    member internal this.Complete value =
        if tryTransition &state (int FiberContextState.Running) (int FiberContextState.Completed) then
            this.DisposeRegistrations()
            this.InvokeOnTerminal()
            resultSource.TrySetResult value |> ignore

    member internal this.CompleteAndReschedule (value, activeWorkItemQueue) =
        let oldState =
            transitionFrom &state (int FiberContextState.Running) (int FiberContextState.Completed)
        if oldState = int FiberContextState.Running ||
            oldState = int FiberContextState.Interrupted then
            this.DisposeRegistrations()
            this.InvokeOnTerminal()
            resultSource.TrySetResult value |> ignore

            let queue = Volatile.Read &blockingWorkItemQueue
            if not (isNull queue) && queue.Count > 0 then
                ValueTask(this.RescheduleBlockingWorkItems activeWorkItemQueue)
            else
                ValueTask.CompletedTask
        else
            ValueTask.CompletedTask

    member internal this.Interrupt (?cause, ?message) =
        let cause = defaultArg cause ExplicitInterrupt
        let message = defaultArg message "Fiber was interrupted."
        if tryTransition &state (int FiberContextState.Running) (int FiberContextState.Interrupted) then
            cancelSource.Cancel(throwOnFirstException = false)
            this.DisposeRegistrations()
            let interruptError = Error(FiberInterruptedException(id, cause, message) :> obj)
            this.InvokeOnTerminal()
            resultSource.TrySetResult interruptError |> ignore

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

    override this.Finalize () =
        this.Dispose false

    interface IDisposable with

        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

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
    blockingSlot: BlockingWorkItemSlot) =
    [<VolatileField>]
    let mutable upcastChannel: Channel<obj> = null

    /// Creates a new, empty channel.
    new() = Channel(Guid.NewGuid(), MailboxQueue<obj>(), BlockingWorkItemSlot())

    /// This channel's unique identifier.
    member _.Id =
        id

    /// The number of messages currently buffered in this channel.
    member _.Count =
        valueQueue.Count

    /// Returns an effect that writes a message to this channel, yielding the written message.
    member this.Write<'E> message : FIO<'A, 'E> =
        WriteChan(message, this)

    /// Returns an effect that reads the next message from this channel, suspending the fiber until one is available.
    member this.Read<'E> () : FIO<'A, 'E> =
        ReadChan this

    member internal _.WriteAsync (value: 'A) =
        valueQueue.WriteAsync value

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

    member internal _.Upcast () =
        initIfNull &upcastChannel (fun () -> Channel<obj>(id, valueQueue, blockingSlot))

/// A lazy, type-safe description of an effect that, when run, either succeeds with a value or fails with a typed error.
and FIO<'A, 'E> =
    internal
    | Success of value: 'A
    | Failure of error: 'E
    | Interrupt of cause: InterruptionCause * message: string
    | Action of func: (unit -> 'A) * onError: (exn -> 'E)
    | WriteChan of value: 'A * channel: Channel<'A>
    | ReadChan of channel: Channel<'A>
    | ForkEffect of effect: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | JoinFiber of fiberContext: FiberContext
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of effect: FIO<obj, 'E> * cont: (obj -> FIO<'A, 'E>)
    | ChainError of effect: FIO<'A, obj> * cont: (obj -> FIO<'A, 'E>)
    | ChainBoth of effect: FIO<obj, obj> * successCont: (obj -> FIO<'A, 'E>) * errorCont: (obj -> FIO<'A, 'E>)
    | OnFinalize of effect: FIO<'A, 'E> * finalizer: FIO<obj, obj>
    | FiberCancellationToken
    | Suspend of thunk: (unit -> FIO<'A, 'E>)

    /// Returns an effect that passes this effect's success value into the given function.
    member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        ChainSuccess(this.UpcastResult(), fun value -> cont (value :?> 'A))

    /// Returns an effect that recovers from this effect's error with the given handler.
    member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        ChainError(this.UpcastError(), fun error -> onError (error :?> 'E))

    /// Returns an effect that runs the given finalizer after this effect on success, failure, and interruption alike.
    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    /// Returns an effect that runs this effect on a new fiber, yielding the fiber's handle.
    member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        let fiber = new Fiber<'A, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Context)

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
        | WriteChan(value, channel) ->
            WriteChan(value :> obj, channel.Upcast())
        | ReadChan channel ->
            ReadChan(channel.Upcast())
        | ForkEffect(effect, fiber, fiberContext) ->
            ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
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
        | WriteChan(value, channel) ->
            WriteChan(value, channel)
        | ReadChan channel ->
            ReadChan channel
        | ForkEffect(effect, fiber, fiberContext) ->
            ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
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
        | WriteChan(value, channel) ->
            WriteChan(value :> obj, channel.Upcast())
        | ReadChan channel ->
            ReadChan(channel.Upcast())
        | ForkEffect(effect, fiber, fiberContext) ->
            ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext ->
            JoinFiber fiberContext
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
