/// <summary>
/// Atomic references for thread-safe mutable state.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.Ref

open System.Threading

open FSharp.FIO.DSL

/// <summary>
/// Atomic reference for reference types using lock-free operations.
/// </summary>
[<Sealed>]
type Ref<'T when 'T : not struct> private (initial: 'T) =
    let mutable value: 'T = initial

    /// <summary>Gets the current value.</summary>
    member _.Get<'E> () : FIO<'T, 'E> =
        FIO.succeed(Volatile.Read(&value))

    /// <summary>Sets a new value.</summary>
    /// <param name="newValue">The new value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Set (newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt((fun () -> Volatile.Write(&value, newValue)), onError)

    /// <summary>Sets a new value.</summary>
    /// <param name="newValue">The new value.</param>
    member _.SetExn (newValue: 'T) : FIO<unit, exn> =
        FIO.attemptExn(fun () -> Volatile.Write(&value, newValue))

    /// <summary>Atomically exchanges the current value with a new value and returns the old value.</summary>
    /// <param name="newValue">The new value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.GetAndSet (newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () -> Interlocked.Exchange(&value, newValue)), onError)

    /// <summary>Atomically exchanges the current value with a new value and returns the old value.</summary>
    /// <param name="newValue">The new value.</param>
    member _.GetAndSetExn (newValue: 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () -> Interlocked.Exchange(&value, newValue))

    /// <summary>Atomically updates the value using a function and returns the old value.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.GetAndUpdate (f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- f current
            current), onError)

    /// <summary>Atomically updates the value using a function and returns the old value.</summary>
    /// <param name="f">Transform function.</param>
    member _.GetAndUpdateExn (f: 'T -> 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- f current
            current)

    /// <summary>Atomically updates the value using a function and returns the new value.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.UpdateAndGet (f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- f current
            updated), onError)

    /// <summary>Atomically updates the value using a function and returns the new value.</summary>
    /// <param name="f">Transform function.</param>
    member _.UpdateAndGetExn (f: 'T -> 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- f current
            updated)

    /// <summary>Atomically updates the value using a function.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member this.Update (f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// <summary>Atomically updates the value using a function.</summary>
    /// <param name="f">Transform function.</param>
    member this.UpdateExn (f: 'T -> 'T) : FIO<unit, exn> =
        this.UpdateAndGetExn(f).Map(fun _ -> ())

    /// <summary>Atomically compares and sets if current value equals expected, returning true if successful.</summary>
    /// <param name="expected">Expected current value.</param>
    /// <param name="newValue">New value if comparison succeeds.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.CompareAndSet (expected: 'T, newValue: 'T, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt((fun () ->
            obj.ReferenceEquals(Interlocked.CompareExchange(&value, newValue, expected), expected)), onError)

    /// <summary>Atomically compares and sets if current value equals expected, returning true if successful.</summary>
    /// <param name="expected">Expected current value.</param>
    /// <param name="newValue">New value if comparison succeeds.</param>
    member _.CompareAndSetExn (expected: 'T, newValue: 'T) : FIO<bool, exn> =
        FIO.attemptExn(fun () ->
            obj.ReferenceEquals(Interlocked.CompareExchange(&value, newValue, expected), expected))

    /// <summary>Atomically modifies the value and returns a computed result.</summary>
    /// <param name="f">Function returning (new value, result) tuple.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Modify (f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated, result = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                let (u, r) = f current
                updated <- u
                result <- r
            result), onError)

    /// <summary>Atomically modifies the value and returns a computed result.</summary>
    /// <param name="f">Function returning (new value, result) tuple.</param>
    member _.ModifyExn (f: 'T -> 'T * 'R) : FIO<'R, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated, result = f current
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                let (u, r) = f current
                updated <- u
                result <- r
            result)

    static member internal Create (initial: 'T) =
        Ref<'T> initial

/// <summary>
/// Atomic reference for value types using boxing for lock-free operations.
/// </summary>
[<Sealed>]
type RefValue<'T when 'T : struct> private (initial: 'T) =
    let mutable value: obj = box initial

    /// <summary>Gets the current value.</summary>
    member _.Get<'E> () : FIO<'T, 'E> =
        FIO.succeed(Volatile.Read(&value) :?> 'T)

    /// <summary>Sets a new value.</summary>
    /// <param name="newValue">The new value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Set (newValue: 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt((fun () -> Volatile.Write(&value, box newValue)), onError)

    /// <summary>Sets a new value.</summary>
    /// <param name="newValue">The new value.</param>
    member _.SetExn (newValue: 'T) : FIO<unit, exn> =
        FIO.attemptExn(fun () -> Volatile.Write(&value, box newValue))

    /// <summary>Atomically exchanges the current value with a new value and returns the old value.</summary>
    /// <param name="newValue">The new value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.GetAndSet (newValue: 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () -> Interlocked.Exchange(&value, box newValue) :?> 'T), onError)

    /// <summary>Atomically exchanges the current value with a new value and returns the old value.</summary>
    /// <param name="newValue">The new value.</param>
    member _.GetAndSetExn (newValue: 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () -> Interlocked.Exchange(&value, box newValue) :?> 'T)

    /// <summary>Atomically updates the value using a function and returns the old value.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.GetAndUpdate (f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = box (f (current :?> 'T))
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- box (f (current :?> 'T))
            current :?> 'T), onError)

    /// <summary>Atomically updates the value using a function and returns the old value.</summary>
    /// <param name="f">Transform function.</param>
    member _.GetAndUpdateExn (f: 'T -> 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = box (f (current :?> 'T))
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- box (f (current :?> 'T))
            current :?> 'T)

    /// <summary>Atomically updates the value using a function and returns the new value.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.UpdateAndGet (f: 'T -> 'T, onError: exn -> 'E) : FIO<'T, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = box (f (current :?> 'T))
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- box (f (current :?> 'T))
            updated :?> 'T), onError)

    /// <summary>Atomically updates the value using a function and returns the new value.</summary>
    /// <param name="f">Transform function.</param>
    member _.UpdateAndGetExn (f: 'T -> 'T) : FIO<'T, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated = box (f (current :?> 'T))
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, updated, current), current) |> not do
                current <- Volatile.Read(&value)
                updated <- box (f (current :?> 'T))
            updated :?> 'T)

    /// <summary>Atomically updates the value using a function.</summary>
    /// <param name="f">Transform function.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member this.Update (f: 'T -> 'T, onError: exn -> 'E) : FIO<unit, 'E> =
        this.UpdateAndGet(f, onError).Map(fun _ -> ())

    /// <summary>Atomically updates the value using a function.</summary>
    /// <param name="f">Transform function.</param>
    member this.UpdateExn (f: 'T -> 'T) : FIO<unit, exn> =
        this.UpdateAndGetExn(f).Map(fun _ -> ())

    /// <summary>Atomically compares and sets if current value equals expected, returning true if successful.</summary>
    /// <param name="expected">Expected current value.</param>
    /// <param name="newValue">New value if comparison succeeds.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.CompareAndSet (expected: 'T, newValue: 'T, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt((fun () ->
            let boxedExpected = box expected
            let result = Interlocked.CompareExchange(&value, box newValue, boxedExpected)
            obj.Equals(result, boxedExpected)), onError)

    /// <summary>Atomically compares and sets if current value equals expected, returning true if successful.</summary>
    /// <param name="expected">Expected current value.</param>
    /// <param name="newValue">New value if comparison succeeds.</param>
    member _.CompareAndSetExn (expected: 'T, newValue: 'T) : FIO<bool, exn> =
        FIO.attemptExn(fun () ->
            let boxedExpected = box expected
            let result = Interlocked.CompareExchange(&value, box newValue, boxedExpected)
            obj.Equals(result, boxedExpected))

    /// <summary>Atomically modifies the value and returns a computed result.</summary>
    /// <param name="f">Function returning (new value, result) tuple.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Modify (f: 'T -> 'T * 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.attempt((fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated, result = f (current :?> 'T)
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, box updated, current), current) |> not do
                current <- Volatile.Read(&value)
                let u, r = f (current :?> 'T)
                updated <- u
                result <- r
            result), onError)

    /// <summary>Atomically modifies the value and returns a computed result.</summary>
    /// <param name="f">Function returning (new value, result) tuple.</param>
    member _.ModifyExn (f: 'T -> 'T * 'R) : FIO<'R, exn> =
        FIO.attemptExn(fun () ->
            let mutable current = Volatile.Read(&value)
            let mutable updated, result = f (current :?> 'T)
            while obj.ReferenceEquals(Interlocked.CompareExchange(&value, box updated, current), current) |> not do
                current <- Volatile.Read(&value)
                let u, r = f (current :?> 'T)
                updated <- u
                result <- r
            result)

    static member internal Create (initial: 'T) =
        RefValue<'T> initial

/// <summary>Factory functions for creating Ref instances.</summary>
module Ref =
    /// <summary>Creates a new atomic reference for a reference type.</summary>
    /// <param name="initial">Initial value.</param>
    let make<'T, 'E when 'T : not struct> (initial: 'T) : FIO<Ref<'T>, 'E> =
        FIO.succeed(Ref<'T>.Create initial)

    /// <summary>Creates a new atomic reference for a value type.</summary>
    /// <param name="initial">Initial value.</param>
    let makeValue<'T, 'E when 'T : struct> (initial: 'T) : FIO<RefValue<'T>, 'E> =
        FIO.succeed(RefValue<'T>.Create initial)
