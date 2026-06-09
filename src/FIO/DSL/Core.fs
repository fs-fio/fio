[<AutoOpen>]
module FIO.DSL.Core

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System.Collections.Generic
open System.Collections.Concurrent

type internal Rethrow<'a>() =
    static let instance: exn -> 'a = fun ex -> raise ex
    static member Instance = instance

[<Struct>]
type internal PostFinalizerSaved =
    | PostFinalizerSuccess of value: obj
    | PostFinalizerError of error: obj
    | PostFinalizerInterrupt of error: obj

and [<Struct>] internal Cont =
    | SuccessCont of successCont: (obj -> FIO<obj, obj>)
    | FailureCont of failureCont: (obj -> FIO<obj, obj>)
    | FinalizerCont of finalizer: FIO<obj, obj>
    | PostFinalizerCont of saved: PostFinalizerSaved

and internal ContStack = Stack<Cont>

and internal WorkItem =
    {
        mutable Eff: FIO<obj, obj>
        mutable FiberContext: FiberContext
        mutable Stack: ContStack
        mutable InterruptionSuppressed: int
    }

and internal BlockingItem =
    | BlockingChannel of channel: Channel<obj> * waitingWorkItem: WorkItem
    | BlockingFiber of fiberContext: FiberContext * waitingWorkItem: WorkItem

and FiberResult<'A, 'E> =
    | Succeeded of value: 'A
    | Failed of error: 'E
    | Interrupted of ex: FiberInterruptedException

and [<Sealed; AllowNullLiteral>] internal UnboundedChannel<'A>() =
    let chan = Channel.CreateUnbounded<'A>()

    member internal _.Count =
        chan.Reader.Count

    member internal _.WriteAsync value =
        chan.Writer.WriteAsync value

    member internal _.ReadAsync () =
        chan.Reader.ReadAsync()

    member internal _.TryRead (value: byref<'A>) =
        chan.Reader.TryRead &value

    member internal _.WaitToReadAsync (ct: CancellationToken) =
        chan.Reader.WaitToReadAsync ct

    member internal _.Clear () =
        let mutable value = Unchecked.defaultof<'A>
        while chan.Reader.TryRead &value do
            ()

and [<Sealed>] internal BlockingWorkItemSlot() =

    [<VolatileField>]
    let mutable chan: UnboundedChannel<WorkItem> = null

    member _.Count =
        let chan' = Volatile.Read &chan
        if isNull chan' then 0 else chan'.Count

    member _.TryGet () =
        Volatile.Read &chan

    member _.GetOrCreate () =
        initIfNull &chan (fun () -> UnboundedChannel<WorkItem>())

and private FiberContextState =
    | Running = 0
    | Completed = 1
    | Interrupted = 2

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

    member internal this.SetOnTerminal (callback: unit -> unit) =
        onTerminalCallback <- ValueSome callback
        if this.IsTerminal() then
            this.InvokeOnTerminal()

    member internal this.AddBlockingWorkItem blockingWorkItem =
        let chan = this.GetOrCreateBlockingChan()
        chan.WriteAsync blockingWorkItem

    member internal _.RescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            let chan = Volatile.Read &blockingWorkItemChan
            if not (isNull chan) then
                let mutable workItem = Unchecked.defaultof<_>
                while chan.TryRead &workItem do
                    do! activeWorkItemChan.WriteAsync workItem
        }

    member internal this.TryRescheduleBlockingWorkItems (activeWorkItemChan: UnboundedChannel<WorkItem>) =
        task {
            if not <| this.IsTerminal() then
                return false
            else
                do! this.RescheduleBlockingWorkItems activeWorkItemChan
                return true
        }

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

    member internal _.Cancel () =
        try
            cts.Cancel(throwOnFirstException = false)
        with :? ObjectDisposedException ->
            ()

    member private _.InvokeOnTerminal() =
        match onTerminalCallback with
        | ValueSome cb when tryClaim &onTerminalFired ->
            try cb ()
            with _ -> ()
        | _ -> ()

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

and [<Sealed>] Fiber<'A, 'E> internal () =
    let fiberContext = new FiberContext()

    member _.Id =
        fiberContext.Id
    
    member _.CancellationToken =
        fiberContext.CancellationToken

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

    member _.Join () : FIO<'A, 'E> =
        JoinFiber fiberContext

    member _.Interrupt (cause: InterruptionCause) (message: string) : FIO<unit, 'E> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)

    member this.InterruptNow () : FIO<unit, 'E> =
        this.Interrupt ExplicitInterrupt "Fiber interrupted"

    member this.Await<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        AwaitTask(upcastTask (this.Task()), Rethrow<_>.Instance)

    member this.InterruptAwait<'E2> (cause: InterruptionCause) (message: string) : FIO<FiberResult<'A, 'E>, 'E2> =
        Action((fun () -> fiberContext.Interrupt(cause, message)), Rethrow<_>.Instance)
            .FlatMap <| fun () -> this.Await()

    member this.InterruptAwaitNow<'E2> () : FIO<FiberResult<'A, 'E>, 'E2> =
        this.InterruptAwait ExplicitInterrupt "Fiber interrupted"

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

    member this.JoinWith<'A1, 'E1>
        (onSucceeded: 'A -> FIO<'A1, 'E1>)
        (onFailed: 'E -> FIO<'A1, 'E1>)
        (onInterrupted: FiberInterruptedException -> FIO<'A1, 'E1>)
        : FIO<'A1, 'E1> =
        this.Await().FlatMap(fun result ->
            match result with
            | Succeeded value -> onSucceeded value
            | Failed error -> onFailed error
            | Interrupted ex -> onInterrupted ex)

    member _.IsCompleted () =
        fiberContext.IsCompleted()

    member _.IsInterrupted () =
        fiberContext.IsInterrupted()

    member _.IsTerminal () =
        fiberContext.IsTerminal()

    member this.UnsafeResult () =
        this.Task() |> Async.AwaitTask |> Async.RunSynchronously

    member this.UnsafeSuccess () =
        match this.UnsafeResult() with
        | Succeeded value ->
            value
        | Failed error ->
            raise (InvalidOperationException $"Fiber failed with error: {error}")
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    member this.UnsafeError() =
        match this.UnsafeResult() with
        | Succeeded value ->
            raise (InvalidOperationException $"Fiber succeeded with value: {value}")
        | Failed error ->
            error
        | Interrupted ex ->
            raise (InvalidOperationException $"Fiber was interrupted: {ex.Message}")

    member this.UnsafePrintResult() =
        printfn "%A" (this.UnsafeResult())

    member internal _.Context =
        fiberContext

    override this.ToString () =
        this.Id.ToString()

    interface IDisposable with
        member _.Dispose() =
            (fiberContext :> IDisposable).Dispose()

and [<Sealed; AllowNullLiteral>] Channel<'A> private (id: Guid, valueChan: UnboundedChannel<obj>, blockingSlot: BlockingWorkItemSlot) =
    [<VolatileField>]
    let mutable upcastChan: Channel<obj> = null

    let mutable signalProcessing = 0

    new() = Channel(Guid.NewGuid(), UnboundedChannel<obj>(), BlockingWorkItemSlot())

    member _.Id =
        id

    member _.Count =
        valueChan.Count

    member this.Write<'E> message : FIO<'A, 'E> =
        WriteChan(message, this)

    member this.Read<'E> () : FIO<'A, 'E> =
        ReadChan this

    member internal _.WriteAsync (value: 'A) =
        valueChan.WriteAsync value

    member internal _.ReadAsync () =
        let vt = valueChan.ReadAsync()
        if vt.IsCompletedSuccessfully then
            ValueTask<'A>(vt.Result :?> 'A)
        else
            ValueTask<'A>(task {
                let! value = vt
                return value :?> 'A
            })

    member internal _.BlockingWorkItemCount =
        blockingSlot.Count

    member internal _.AddBlockingWorkItem blockingItem =
        let chan = blockingSlot.GetOrCreate()
        chan.WriteAsync blockingItem

    member internal _.TryRescheduleNextBlockingWorkItem (activeWorkItemChan: UnboundedChannel<WorkItem>) =
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

    member internal _.TryBeginSignalProcessing () =
        tryClaim &signalProcessing

    member internal _.EndSignalProcessing () =
        Volatile.Write(&signalProcessing, 0)

    member internal _.UnboundedChannel =
        valueChan

    member internal _.Upcast () =
        initIfNull &upcastChan (fun () -> Channel<obj>(id, valueChan, blockingSlot))

and FIO<'A, 'E> =
    internal
    | Success of value: 'A
    | Failure of error: 'E
    | Interrupt of cause: InterruptionCause * msg: string
    | Action of func: (unit -> 'A) * onError: (exn -> 'E)
    | WriteChan of value: 'A * chan: Channel<'A>
    | ReadChan of chan: Channel<'A>
    | ForkEffect of effect: FIO<obj, obj> * fiber: obj * fiberContext: FiberContext
    | JoinFiber of fiberContext: FiberContext
    | AwaitTask of task: Task<obj> * onError: (exn -> 'E)
    | ChainSuccess of effect: FIO<obj, 'E> * cont: (obj -> FIO<'A, 'E>)
    | ChainError of effect: FIO<'A, obj> * cont: (obj -> FIO<'A, 'E>)
    | ChainBoth of effect: FIO<obj, obj> * successCont: (obj -> FIO<'A, 'E>) * errorCont: (obj -> FIO<'A, 'E>)
    | OnFinalize of effect: FIO<'A, 'E> * finalizer: FIO<obj, obj>
    | FiberCancellationToken
    | Suspend of thunk: (unit -> FIO<'A, 'E>)

    member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        ChainSuccess(this.UpcastResult(), fun value -> cont (value :?> 'A))

    member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        ChainError(this.UpcastError(), fun error -> onError (error :?> 'E))

    member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> =
        OnFinalize(this, finalizer.UpcastBoth())

    member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> =
        let fiber = new Fiber<'A, 'E>()
        ForkEffect(this.UpcastBoth(), fiber, fiber.Context)

    member this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> =
        this.FlatMap <| fun value -> Success (mapper value)

    member this.MapError<'E1> (mapper: 'E -> 'E1) : FIO<'A, 'E1> =
        this.CatchAll <| fun error -> Failure (mapper error)

    member this.MapBoth<'A1, 'E1> (successMapper: 'A -> 'A1) (errorMapper: 'E -> 'E1) : FIO<'A1, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (successMapper (value :?> 'A))),
            (fun error -> Failure (errorMapper (error :?> 'E))))

    member this.Result<'E1> () : FIO<Result<'A, 'E>, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Ok (value :?> 'A))),
            (fun error -> Success (Error (error :?> 'E))))

    member this.Option<'E1> () : FIO<'A option, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Some (value :?> 'A))),
            (fun _ -> Success (None : 'A option)))

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
            let inners = ResizeArray<obj -> FIO<obj, 'E>>()
            let mutable current = effect
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
            let inners = ResizeArray<obj -> FIO<'A, obj>>()
            let mutable current = effect
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
        | _ ->
            ChainError(leafUpcast effect, outerContWrap outerCont)

    member internal this.UpcastResult() : FIO<obj, 'E> =
        match this with
        | Success value -> Success(value :> obj)
        | Failure error -> Failure error
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, onError)
        | WriteChan(msg, chan) -> WriteChan(msg :> obj, chan.Upcast())
        | ReadChan chan -> ReadChan(chan.Upcast())
        | ForkEffect(effect, fiber, fiberContext) -> ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, onError)
        | ChainSuccess(effect, cont) -> ChainSuccess(effect, fun value -> cont(value).UpcastResult())
        | ChainError(effect, cont) ->
            FIO.flattenChainError
                (fun e -> e.UpcastResult())
                (fun ic -> fun error -> (ic error).UpcastResult())
                (fun oc -> fun error -> (oc error).UpcastResult())
                effect cont
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastResult()),
                (fun error -> (errorCont error).UpcastResult()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastResult()) effect finalizer
        | FiberCancellationToken -> FiberCancellationToken
        | Suspend thunk -> Suspend(fun () -> (thunk()).UpcastResult())

    member internal this.UpcastError() : FIO<'A, obj> =
        match this with
        | Success value -> Success value
        | Failure error -> Failure(error :> obj)
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(func, upcastOnError onError)
        | WriteChan(msg, chan) -> WriteChan(msg, chan)
        | ReadChan chan -> ReadChan chan
        | ForkEffect(effect, fiber, fiberContext) -> ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(effect, cont) ->
            FIO.flattenChainSuccess
                (fun e -> e.UpcastError())
                (fun ic -> fun value -> (ic value).UpcastError())
                (fun oc -> fun value -> (oc value).UpcastError())
                effect cont
        | ChainError(effect, cont) -> ChainError(effect, fun error -> cont(error).UpcastError())
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastError()),
                (fun error -> (errorCont error).UpcastError()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastError()) effect finalizer
        | FiberCancellationToken -> FiberCancellationToken
        | Suspend thunk -> Suspend(fun () -> (thunk()).UpcastError())

    member internal this.UpcastBoth() : FIO<obj, obj> =
        match this with
        | Success value -> Success(value :> obj)
        | Failure error -> Failure(error :> obj)
        | Interrupt(cause, msg) -> Interrupt(cause, msg)
        | Action(func, onError) -> Action(upcastFunc func, upcastOnError onError)
        | WriteChan(msg, chan) -> WriteChan(msg :> obj, chan.Upcast())
        | ReadChan chan -> ReadChan(chan.Upcast())
        | ForkEffect(effect, fiber, fiberContext) -> ForkEffect(effect, fiber, fiberContext)
        | JoinFiber fiberContext -> JoinFiber fiberContext
        | AwaitTask(task, onError) -> AwaitTask(task, upcastOnError onError)
        | ChainSuccess(effect, cont) ->
            FIO.flattenChainSuccess
                (fun e -> e.UpcastError())
                (fun ic -> fun value -> (ic value).UpcastError())
                (fun oc -> fun value -> (oc value).UpcastBoth())
                effect cont
        | ChainError(effect, cont) ->
            FIO.flattenChainError
                (fun e -> e.UpcastResult())
                (fun ic -> fun error -> (ic error).UpcastResult())
                (fun oc -> fun error -> (oc error).UpcastBoth())
                effect cont
        | ChainBoth(effect, successCont, errorCont) ->
            ChainBoth(effect,
                (fun value -> (successCont value).UpcastBoth()),
                (fun error -> (errorCont error).UpcastBoth()))
        | OnFinalize(effect, finalizer) ->
            FIO.flattenOnFinalize (fun e -> e.UpcastBoth()) effect finalizer
        | FiberCancellationToken -> FiberCancellationToken
        | Suspend thunk -> Suspend(fun () -> (thunk()).UpcastBoth())
