namespace FIO.DSL

open System.Threading

/// A mutable reference to an immutable value, read and updated atomically without locks. A transition may
/// run more than once under contention, so keep it pure. With this namespace open, an explicit
/// <c>Ref&lt;'A&gt;</c> annotation names this type rather than F#'s reference cell.
[<Sealed>]
type Ref<'A>(initial: 'A) =

    let mutable cell: obj = box initial

    let rec cas (transition: 'A -> 'B * 'A) : 'B =
        let current = Volatile.Read &cell
        let result, next = transition (unbox<'A> current)

        if obj.ReferenceEquals(Interlocked.CompareExchange(&cell, box next, current), current) then result
        else cas transition

    /// Reads the current value outside an effect.
    member _.UnsafeGet () : 'A =
        unbox<'A> (Volatile.Read &cell)

    /// Applies a transition outside an effect and returns its result.
    member _.UnsafeModify (transition: 'A -> 'B * 'A) : 'B =
        cas transition

    /// Applies a function to the value outside an effect.
    member _.UnsafeUpdate (func: 'A -> 'A) : unit =
        cas (fun value -> (), func value)

    /// Returns an effect that yields the current value.
    member this.Get<'E> () : FIO<'A, 'E> =
        FIO.succeedWith this.UnsafeGet

    /// Returns an effect that replaces the value.
    member _.Set<'E> (value: 'A) : FIO<unit, 'E> =
        FIO.succeedWith (fun () -> Volatile.Write(&cell, box value))

    /// Returns an effect that replaces the value and yields the previous one.
    member _.GetAndSet<'E> (value: 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () -> unbox<'A> (Interlocked.Exchange(&cell, box value)))

    /// Returns an effect that applies a function to the value atomically.
    member _.Update<'E> (func: 'A -> 'A) : FIO<unit, 'E> =
        FIO.succeedWith (fun () -> cas (fun value -> (), func value))

    /// Returns an effect that applies a function to the value atomically and yields the new value.
    member _.UpdateAndGet<'E> (func: 'A -> 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () ->
            cas (fun value ->
                let next = func value
                next, next))

    /// Returns an effect that applies a function to the value atomically and yields the previous value.
    member _.GetAndUpdate<'E> (func: 'A -> 'A) : FIO<'A, 'E> =
        FIO.succeedWith (fun () -> cas (fun value -> value, func value))

    /// Returns an effect that applies a transition atomically, storing its new value and yielding its result.
    member _.Modify<'B, 'E> (transition: 'A -> 'B * 'A) : FIO<'B, 'E> =
        FIO.succeedWith (fun () -> cas transition)
