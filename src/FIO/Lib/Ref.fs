/// Atomic references for thread-safe mutable state.
[<AutoOpen>]
module FIO.Ref

open System.Threading

open FIO.DSL

/// Atomic reference for reference types using lock-free CAS operations.
[<Sealed>]
type Ref<'T when 'T: not struct> private (initial: 'T) =
    let mutable value: 'T = initial

    /// CAS loop: applies transform, retries on contention, returns selected value.
    member private _.CasLoop(f: 'T -> 'T, select: 'T -> 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt (
            (fun () ->
                let mutable current = Volatile.Read(&value)
                let mutable updated = f current

                while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current)
                      |> not do
                    current <- Volatile.Read(&value)
                    updated <- f current

                select current updated),
            onError
        )

    /// CAS loop returning a computed result alongside the updated value.
    member private _.CasModify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.attempt (
            (fun () ->
                let mutable current = Volatile.Read(&value)
                let mutable updated, result = f current

                while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current)
                      |> not do
                    current <- Volatile.Read(&value)
                    let (u, r) = f current
                    updated <- u
                    result <- r

                result),
            onError
        )

    /// Gets the current value.
    /// <returns>An effect that produces the current value.</returns>
    member _.Get<'E>() : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Volatile.Read(&value)), fun ex -> raise ex)

    /// Sets a new value.
    /// <param name="newValue">The value to set.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that sets the value.</returns>
    member _.Set(newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> Volatile.Write(&value, newValue)), onError)

    /// Atomically exchanges the value and returns the old one.
    /// <param name="newValue">The value to set.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the previous value.</returns>
    member _.GetAndSet(newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Interlocked.Exchange(&value, newValue)), onError)

    /// Atomically updates the value and returns the old one.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the previous value.</returns>
    member this.GetAndUpdate(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun current _ -> current), onError)

    /// Atomically updates the value and returns the new one.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the updated value.</returns>
    member this.UpdateAndGet(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun _ updated -> updated), onError)

    /// Atomically updates the value.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that updates the value.</returns>
    member this.Update(f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// Sets the value only if the current value is reference-equal to expected.
    /// <param name="expected">The expected current value.</param>
    /// <param name="newValue">The value to set on match.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing true if the swap succeeded.</returns>
    member _.CompareAndSet(expected: 'T, newValue: 'T, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt (
            (fun () -> obj.ReferenceEquals(Interlocked.CompareExchange(&value, newValue, expected), expected)),
            onError
        )

    /// Atomically modifies the value and returns a computed result.
    /// <param name="f">Function taking the current value and returning (newValue, result).</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the computed result.</returns>
    member this.Modify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> = this.CasModify(f, onError)

    /// Creates a new Ref with the given initial value.
    /// <param name="initial">The initial value.</param>
    /// <returns>A new Ref instance.</returns>
    static member internal Create(initial: 'T) = Ref<'T> initial

/// Atomic reference for value types using boxing for lock-free CAS operations.
[<Sealed>]
type RefValue<'T when 'T: struct> private (initial: 'T) =
    let mutable value: obj = box initial

    /// CAS loop: applies transform, retries on contention, returns selected value.
    member private _.CasLoop(f: 'T -> 'T, select: 'T -> 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt (
            (fun () ->
                let mutable current = Volatile.Read(&value)
                let mutable updated = box (f (current :?> 'T))

                while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current)
                      |> not do
                    current <- Volatile.Read(&value)
                    updated <- box (f (current :?> 'T))

                select (current :?> 'T) (updated :?> 'T)),
            onError
        )

    /// CAS loop returning a computed result alongside the updated value.
    member private _.CasModify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.attempt (
            (fun () ->
                let mutable current = Volatile.Read(&value)
                let mutable updated, result = f (current :?> 'T)

                while obj.ReferenceEquals(Interlocked.CompareExchange(&value, box updated, current), current)
                      |> not do
                    current <- Volatile.Read(&value)
                    let u, r = f (current :?> 'T)
                    updated <- u
                    result <- r

                result),
            onError
        )

    /// Gets the current value.
    /// <returns>An effect that produces the current value.</returns>
    member _.Get<'E>() : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Volatile.Read(&value) :?> 'T), fun ex -> raise ex)

    /// Sets a new value.
    /// <param name="newValue">The value to set.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that sets the value.</returns>
    member _.Set(newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> Volatile.Write(&value, box newValue)), onError)

    /// Atomically exchanges the value and returns the old one.
    /// <param name="newValue">The value to set.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the previous value.</returns>
    member _.GetAndSet(newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Interlocked.Exchange(&value, box newValue) :?> 'T), onError)

    /// Atomically updates the value and returns the old one.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the previous value.</returns>
    member this.GetAndUpdate(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun current _ -> current), onError)

    /// Atomically updates the value and returns the new one.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the updated value.</returns>
    member this.UpdateAndGet(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun _ updated -> updated), onError)

    /// Atomically updates the value.
    /// <param name="f">Update function applied to the current value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that updates the value.</returns>
    member this.Update(f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// Sets the value only if the current value equals expected.
    /// <param name="expected">The expected current value.</param>
    /// <param name="newValue">The value to set on match.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing true if the swap succeeded.</returns>
    member _.CompareAndSet(expected: 'T, newValue: 'T, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt (
            (fun () ->
                let boxedExpected = box expected
                let boxedNewValue = box newValue
                let mutable finished = false
                let mutable swapped = false

                while not finished do
                    let current = Volatile.Read(&value)

                    if not <| obj.Equals(current, boxedExpected) then
                        finished <- true
                    elif obj.ReferenceEquals(Interlocked.CompareExchange(&value, boxedNewValue, current), current) then
                        swapped <- true
                        finished <- true

                swapped),
            onError
        )

    /// Atomically modifies the value and returns a computed result.
    /// <param name="f">Function taking the current value and returning (newValue, result).</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the computed result.</returns>
    member this.Modify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> = this.CasModify(f, onError)

    /// Creates a new RefValue with the given initial value.
    /// <param name="initial">The initial value.</param>
    /// <returns>A new RefValue instance.</returns>
    static member internal Create(initial: 'T) = RefValue<'T> initial

/// Factory functions for creating Ref instances.
module Ref =
    /// Creates a new atomic reference for a reference type.
    /// <param name="initial">The initial value.</param>
    /// <returns>An effect that produces a new Ref instance.</returns>
    let make<'T, 'E when 'T: not struct> (initial: 'T) : FIO<Ref<'T>, 'E> =
        FIO.attempt ((fun () -> Ref<'T>.Create initial), fun ex -> raise ex)

    /// Creates a new atomic reference for a value type.
    /// <param name="initial">The initial value.</param>
    /// <returns>An effect that produces a new RefValue instance.</returns>
    let makeValue<'T, 'E when 'T: struct> (initial: 'T) : FIO<RefValue<'T>, 'E> =
        FIO.attempt ((fun () -> RefValue<'T>.Create initial), fun ex -> raise ex)
