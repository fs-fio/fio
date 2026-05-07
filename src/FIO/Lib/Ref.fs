/// <summary>Provides atomic references for thread-safe mutable state in FIO programs.</summary>
[<AutoOpen>]
module FIO.Ref

open System.Threading

open FIO.DSL

/// <summary>Represents an atomic reference holding a value of a reference type.</summary>
/// <typeparam name="'T">The element type stored in the reference; must be a reference type.</typeparam>
[<Sealed>]
type Ref<'T when 'T: not struct> private (initial: 'T) =
    /// <summary>Represents the current value held by this reference.</summary>
    let mutable value: 'T = initial

    /// <summary>Builds an atomic update loop that retries until the modification succeeds.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="select">A selector that receives the pre-swap and post-swap values and produces the result.</param>
    /// <param name="onError">A function that maps an exception thrown during the loop to the typed error.</param>
    /// <returns>An effect that completes with the value chosen by <paramref name="select"/>.</returns>
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

    /// <summary>Builds an atomic update loop that retries until the modification succeeds and returns both old and new values.</summary>
    /// <param name="f">A function from the current value to the new value paired with a derived result; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the loop to the typed error.</param>
    /// <returns>An effect that completes with the derived result returned by <paramref name="f"/>.</returns>
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

    /// <summary>Returns the current value of this reference.</summary>
    /// <returns>An effect that completes with the value most recently published to this reference.</returns>
    member _.Get<'E>() : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Volatile.Read(&value)), fun ex -> raise ex)

    /// <summary>Creates an effect that overwrites this reference with a new value.</summary>
    /// <param name="newValue">The value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during the write to the typed error.</param>
    /// <returns>An effect that completes with unit once the new value has been published.</returns>
    member _.Set(newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> Volatile.Write(&value, newValue)), onError)

    /// <summary>Combines a write of a new value with a read of the previous value, exchanging them atomically.</summary>
    /// <param name="newValue">The value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during the exchange to the typed error.</param>
    /// <returns>An effect that completes with the value held before this call.</returns>
    member _.GetAndSet(newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Interlocked.Exchange(&value, newValue)), onError)

    /// <summary>Transforms this reference's value using a pure function and returns the previous value.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the value held before the update.</returns>
    member this.GetAndUpdate(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun current _ -> current), onError)

    /// <summary>Transforms this reference's value using a pure function and returns the new value.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the value published by this update.</returns>
    member this.UpdateAndGet(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun _ updated -> updated), onError)

    /// <summary>Transforms this reference's value using a pure function, discarding both old and new values.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with unit once the update has been published.</returns>
    member this.Update(f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// <summary>Creates an effect that publishes a new value only when the current value is reference-equal to an expected one.</summary>
    /// <param name="expected">The expected current value to match by reference equality.</param>
    /// <param name="newValue">The value to publish if the comparison succeeds.</param>
    /// <param name="onError">A function that maps an exception thrown during the swap to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when the swap succeeded; <c>false</c> when the current value did not match.</returns>
    member _.CompareAndSet(expected: 'T, newValue: 'T, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt (
            (fun () -> obj.ReferenceEquals(Interlocked.CompareExchange(&value, newValue, expected), expected)),
            onError
        )

    /// <summary>Transforms this reference's value using a function that also produces a derived result.</summary>
    /// <typeparam name="'R">The derived result type produced by <paramref name="f"/>.</typeparam>
    /// <param name="f">A function from the current value to the new value paired with a derived result; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the derived result returned by <paramref name="f"/>.</returns>
    member this.Modify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> = this.CasModify(f, onError)

    /// <summary>Creates a new <c>Ref</c> initialized with the given value.</summary>
    /// <param name="initial">The initial value to publish.</param>
    /// <returns>A freshly initialized <c>Ref</c> instance.</returns>
    static member internal Create(initial: 'T) = Ref<'T> initial

/// <summary>Represents an atomic reference holding a value of a value type.</summary>
/// <typeparam name="'T">The element type stored in the reference; must be a struct.</typeparam>
[<Sealed>]
type RefValue<'T when 'T: struct> private (initial: 'T) =
    /// <summary>Represents the current stored value.</summary>
    let mutable value: obj = box initial

    /// <summary>Builds an atomic update loop that retries until the modification succeeds.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="select">A selector that receives the pre-swap and post-swap values and produces the result.</param>
    /// <param name="onError">A function that maps an exception thrown during the loop to the typed error.</param>
    /// <returns>An effect that completes with the value chosen by <paramref name="select"/>.</returns>
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

    /// <summary>Builds an atomic update loop that retries until the modification succeeds and returns both old and new values.</summary>
    /// <param name="f">A function from the current value to the new value paired with a derived result; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the loop to the typed error.</param>
    /// <returns>An effect that completes with the derived result returned by <paramref name="f"/>.</returns>
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

    /// <summary>Returns the current value of this reference.</summary>
    /// <returns>An effect that completes with the value most recently published to this reference.</returns>
    member _.Get<'E>() : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Volatile.Read(&value) :?> 'T), fun ex -> raise ex)

    /// <summary>Creates an effect that overwrites this reference with a new value.</summary>
    /// <param name="newValue">The value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during the write to the typed error.</param>
    /// <returns>An effect that completes with unit once the new value has been published.</returns>
    member _.Set(newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> Volatile.Write(&value, box newValue)), onError)

    /// <summary>Combines a write of a new value with a read of the previous value, exchanging them atomically.</summary>
    /// <param name="newValue">The value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during the exchange to the typed error.</param>
    /// <returns>An effect that completes with the value held before this call.</returns>
    member _.GetAndSet(newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt ((fun () -> Interlocked.Exchange(&value, box newValue) :?> 'T), onError)

    /// <summary>Transforms this reference's value using a pure function and returns the previous value.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the value held before the update.</returns>
    member this.GetAndUpdate(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun current _ -> current), onError)

    /// <summary>Transforms this reference's value using a pure function and returns the new value.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the value published by this update.</returns>
    member this.UpdateAndGet(f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        this.CasLoop(f, (fun _ updated -> updated), onError)

    /// <summary>Transforms this reference's value using a pure function, discarding both old and new values.</summary>
    /// <param name="f">A pure function from the current value to the new value; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with unit once the update has been published.</returns>
    member this.Update(f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// <summary>Creates an effect that publishes a new value only when the current value equals an expected one by structural equality.</summary>
    /// <param name="expected">The expected current value.</param>
    /// <param name="newValue">The value to publish if the comparison succeeds.</param>
    /// <param name="onError">A function that maps an exception thrown during the swap to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when the swap succeeded; <c>false</c> when the current value did not match.</returns>
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

    /// <summary>Transforms this reference's value using a function that also produces a derived result.</summary>
    /// <typeparam name="'R">The derived result type produced by <paramref name="f"/>.</typeparam>
    /// <param name="f">A function from the current value to the new value paired with a derived result; may be retried on contention.</param>
    /// <param name="onError">A function that maps an exception thrown during the update to the typed error.</param>
    /// <returns>An effect that completes with the derived result returned by <paramref name="f"/>.</returns>
    member this.Modify(f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> = this.CasModify(f, onError)

    /// <summary>Creates a new <c>RefValue</c> initialized with the given value.</summary>
    /// <param name="initial">The initial value to publish.</param>
    /// <returns>A freshly initialized <c>RefValue</c> instance.</returns>
    static member internal Create(initial: 'T) = RefValue<'T> initial

/// <summary>Provides factory functions for creating atomic references.</summary>
module Ref =
    /// <summary>Creates a new atomic reference holding a reference-typed value.</summary>
    /// <typeparam name="'T">The element type; must be a reference type.</typeparam>
    /// <param name="initial">The initial value to publish.</param>
    /// <returns>An effect that completes with a freshly initialized <c>Ref</c>.</returns>
    let make<'T, 'E when 'T: not struct> (initial: 'T) : FIO<Ref<'T>, 'E> =
        FIO.attempt ((fun () -> Ref<'T>.Create initial), fun ex -> raise ex)

    /// <summary>Creates a new atomic reference holding a value-typed value.</summary>
    /// <typeparam name="'T">The element type; must be a struct.</typeparam>
    /// <param name="initial">The initial value to publish.</param>
    /// <returns>An effect that completes with a freshly initialized <c>RefValue</c>.</returns>
    let makeValue<'T, 'E when 'T: struct> (initial: 'T) : FIO<RefValue<'T>, 'E> =
        FIO.attempt ((fun () -> RefValue<'T>.Create initial), fun ex -> raise ex)
