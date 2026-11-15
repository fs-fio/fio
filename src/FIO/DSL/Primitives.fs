namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

/// <summary>A mutable reference to an immutable value, updated atomically without locks; a transition may run more than
/// once under contention, so keep it pure. With this namespace open, <c>Ref&lt;'A&gt;</c> means this type.</summary>
[<Sealed>]
type Ref<'A>(initial: 'A) =

    let mutable cell = box initial

    [<TailCall>]
    let rec cas (transition: 'A -> 'B * 'A) =
        let current = Volatile.Read &cell
        let result, next = transition (unbox<'A> current)

        if obj.ReferenceEquals(Interlocked.CompareExchange(&cell, box next, current), current) then
            result
        else
            cas transition

    /// Reads the current value outside an effect.
    member _.UnsafeGet() : 'A = unbox<'A>(Volatile.Read &cell)

    /// Applies a transition outside an effect and returns its result.
    member _.UnsafeModify(transition: 'A -> 'B * 'A) : 'B = cas transition

    /// Applies a function to the value outside an effect.
    member _.UnsafeUpdate(func: 'A -> 'A) : unit = cas (fun value -> (), func value)

    /// Returns an effect that yields the current value.
    member this.Get<'E>() : FIO<'A, 'E> = FIO.succeedWith this.UnsafeGet

    /// Returns an effect that replaces the value.
    member _.Set<'E>(value: 'A) : FIO<unit, 'E> =
        FIO.succeedWith (fun () -> Volatile.Write(&cell, box value))

    /// Returns an effect that replaces the value and yields the previous one.
    member _.GetAndSet<'E>(value: 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () -> unbox<'A>(Interlocked.Exchange(&cell, box value)))

    /// Returns an effect that applies a function to the value atomically.
    member _.Update<'E>(func: 'A -> 'A) : FIO<unit, 'E> =
        FIO.succeedWith (fun () -> cas (fun value -> (), func value))

    /// Returns an effect that applies a function to the value atomically and yields the new value.
    member _.UpdateAndGet<'E>(func: 'A -> 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () ->
            cas (fun value ->
                let next = func value
                next, next))

    /// Returns an effect that applies a function to the value atomically and yields the previous value.
    member _.GetAndUpdate<'E>(func: 'A -> 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () -> cas (fun value -> value, func value))

    /// Returns an effect that applies a transition atomically, storing its new value and yielding its result.
    member _.Modify<'B, 'E>(transition: 'A -> 'B * 'A) : FIO<'B, 'E> =
        FIO.succeedWith (fun () -> cas transition)

/// A variable that is completed once, with a value or an error, and that any number of fibers can await.
[<Sealed>]
type Promise<'A, 'E>() =

    // Never faulted or cancelled: a typed error is stored as a value, so an unawaited one raises no task exception.
    let completion =
        TaskCompletionSource<obj> TaskCreationOptions.RunContinuationsAsynchronously

    /// Returns an effect that completes this promise with the given value, yielding whether this call completed it.
    member _.Succeed<'E1>(value: 'A) : FIO<bool, 'E1> =
        FIO.succeedWith (fun () -> completion.TrySetResult(box (Ok value: Result<'A, 'E>)))

    /// Returns an effect that completes this promise with the given error, yielding whether this call completed it.
    member _.Fail<'E1>(error: 'E) : FIO<bool, 'E1> =
        FIO.succeedWith (fun () -> completion.TrySetResult(box (Error error: Result<'A, 'E>)))

    /// Returns an effect that waits for this promise, succeeding with its value or failing with its error.
    member _.Await() : FIO<'A, 'E> =
        AwaitTaskNode<obj, 'E>(completion.Task, Rethrow<_>.Instance)
            .FlatMap(fun outcome -> FIO.fromResult (outcome :?> Result<'A, 'E>))

    /// Returns an effect that yields this promise's outcome if it has been completed, or None if it has not.
    member _.Poll<'E1>() : FIO<Result<'A, 'E> option, 'E1> =
        FIO.succeedWith (fun () ->
            if completion.Task.IsCompleted then
                Some(completion.Task.Result :?> Result<'A, 'E>)
            else
                None)

/// <summary>A fixed number of permits that bounds how many effects run at once; fewer than 1 permit throws. With
/// <c>System.Threading</c> opened after <c>FIO.DSL</c>, the name <c>Semaphore</c> means that namespace's type.</summary>
[<Sealed>]
type Semaphore(permits: int) =

    do
        if permits < 1 then
            raise (ArgumentOutOfRangeException(nameof permits, permits, "A semaphore needs at least 1 permit."))

    let gate = new SemaphoreSlim(permits, permits)

    // A wait the fiber gave up on can still be granted, so the permit goes back exactly when the wait succeeds. The
    // continuation is attached whatever the wait's state: one that completes during the check still decides itself.
    let returnWhenGranted (wait: Task) =
        if wait.IsCompletedSuccessfully then
            gate.Release() |> ignore
        else
            wait.ContinueWith(
                (fun (granted: Task) ->
                    if granted.IsCompletedSuccessfully then
                        gate.Release() |> ignore),
                CancellationToken.None,
                TaskContinuationOptions.ExecuteSynchronously,
                TaskScheduler.Default
            )
            |> ignore

    /// The number of permits no fiber holds.
    member _.Available: int = gate.CurrentCount

    /// Returns an effect that waits for a permit, interruptibly unless the caller is uninterruptible, runs the given
    /// effect holding it, and returns the permit however the effect ends.
    member _.WithPermit<'A, 'E>(effect: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.cancellationToken().FlatMap
        <| fun cancellationToken ->
            FIO.acquireReleaseWith
                (FIO.succeedWith (fun () -> gate.WaitAsync cancellationToken))
                (fun wait -> FIO.succeedWith (fun () -> returnWhenGranted wait))
                (fun wait -> (FIO.awaitUnitTask wait Rethrow<_>.Instance).FlatMap(fun () -> effect))
