(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides the computation expression builder for FIO, enabling idiomatic F# computation expression syntax (fio { ... }) for composing functional effects.
/// Includes the FIOBuilder type and the fio instance for effectful workflows.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.CE

open System

/// <summary>
/// The computation expression builder for FIO effects, enabling F# computation expression syntax (fio { ... }) for composing functional effects.
/// </summary>
type FIOBuilder internal () =

    /// <summary>
    /// Binds the result of an FIO effect to a continuation, enabling sequential composition in computation expressions.
    /// </summary>
    /// <typeparam name="'R">The result type of the first effect.</typeparam>
    /// <typeparam name="'R1">The result type of the continuation effect.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to bind.</param>
    /// <param name="cont">The continuation function to apply to the result.</param>
    /// <returns>An FIO effect representing the composed computation.</returns>
    member inline _.Bind<'R, 'R1, 'E> (eff: FIO<'R, 'E>, cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        eff.Bind cont
    
    /// <summary>
    /// Binds the result of an FIO effect to a function, mapping the result in computation expressions.
    /// Maps a function over the result of the effect, or propagates an error.
    /// </summary>
    /// <typeparam name="'R">The result type of the first effect.</typeparam>
    /// <typeparam name="'R1">The mapped result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to bind.</param>
    /// <param name="cont">The function to apply to the result.</param>
    /// <returns>An FIO effect representing the mapped computation.</returns>
    member inline _.BindReturn<'R, 'R1, 'E> (eff: FIO<'R, 'E>, cont: 'R -> 'R1) : FIO<'R1, 'E> =
        eff.Map cont

    /// <summary>
    /// Combines two FIO effects, sequencing them in computation expressions.
    /// Ignores the result of the first effect. If the first effect fails, the error is propagated immediately.
    /// </summary>
    /// <typeparam name="'R">The result type of the first effect.</typeparam>
    /// <typeparam name="'R1">The result type of the second effect.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect to run after the first succeeds.</param>
    /// <returns>An FIO effect representing the sequential composition.</returns>
    member inline _.Combine<'R, 'R1, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R1, 'E> =
        eff.Then eff'

    /// <summary>
    /// Runs the given FIO effect. Used to mark the end of a computation expression.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to run.</param>
    /// <returns>The same FIO effect.</returns>
    member inline _.Run<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Returns an FIO effect that succeeds with unit. Used for empty computation expressions.
    /// </summary>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <returns>An FIO effect that succeeds with unit.</returns>
    member inline _.Zero<'E> () : FIO<unit, 'E> =
        FIO.Succeed ()

    /// <summary>
    /// Returns an FIO effect that succeeds with the given value.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="res">The value to return.</param>
    /// <returns>An FIO effect that succeeds with the given value.</returns>
    member inline _.Return<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        FIO.Succeed res

    /// <summary>
    /// Returns the given FIO effect. Used to return an effect from a computation expression.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to return.</param>
    /// <returns>The same FIO effect.</returns>
    member inline _.ReturnFrom<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Returns the given FIO effect. Used to return an effect from a computation expression as the final operation.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to return.</param>
    /// <returns>The same FIO effect.</returns>
    member inline _.ReturnFromFinal<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Returns an FIO effect that succeeds with the given value. Used for yield expressions.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="res">The value to yield.</param>
    /// <returns>An FIO effect that succeeds with the given value.</returns>
    member inline _.Yield<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        FIO.Succeed res

    /// <summary>
    /// Returns the given FIO effect. Used for yield! expressions.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to yield from.</param>
    /// <returns>The same FIO effect.</returns>
    member inline _.YieldFrom<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Returns the given FIO effect. Used for yield! expressions as the final operation.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to yield from.</param>
    /// <returns>The same FIO effect.</returns>
    member inline _.YieldFromFinal<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Handles exceptions in an FIO effect by binding the error to a handler.
    /// If the effect succeeds, the result is propagated immediately. If it fails, the handler is invoked with the error.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <typeparam name="'E1">The new error type after handling.</typeparam>
    /// <param name="eff">The FIO effect to try.</param>
    /// <param name="cont">The error handler function that receives the error and returns a recovery effect.</param>
    /// <returns>An FIO effect that handles errors using the given handler.</returns>
    member inline _.TryWith<'R, 'E, 'E1> (eff: FIO<'R, 'E>, cont: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        eff.BindError cont

    /// <summary>
    /// Ensures a finalizer effect is run after an FIO effect, even if an error occurs.
    /// The finalizer runs on both success and error paths, consistent with try-finally semantics.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to try.</param>
    /// <param name="finalizer">The finalizer effect to run after the effect.</param>
    /// <returns>An FIO effect that runs the finalizer after the effect.</returns>
    member inline _.TryFinally<'R, 'E> (eff: FIO<'R, 'E>, finalizer: FIO<unit, 'E>) : FIO<'R, 'E> =
        eff.Bind(fun res ->
            finalizer.Bind(fun _ -> FIO.Succeed res)
                     .BindError(fun _ -> FIO.Succeed res))
           .BindError(fun err ->
            finalizer.Bind(fun _ -> FIO.Fail err)
                     .BindError(fun _ -> FIO.Fail err))

    /// <summary>
    /// Delays the execution of a computation until it is needed.
    /// </summary>
    /// <typeparam name="'R1">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="cont">A function that produces the FIO effect to run.</param>
    /// <returns>An FIO effect that delays execution.</returns>
    member inline this.Delay<'R1, 'E> (cont: unit -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
       this.Zero().Bind cont

    /// <summary>
    /// Iterates over a sequence, running the body for each element in order.
    /// The enumerator is always disposed, even if the body fails or throws an exception.
    /// </summary>
    /// <typeparam name="'T">The element type of the sequence.</typeparam>
    /// <typeparam name="'E">The error type (must be an exception type).</typeparam>
    /// <param name="sequence">The sequence to iterate over.</param>
    /// <param name="body">The function to run for each element.</param>
    /// <returns>An FIO effect that runs the body for each element in the sequence.</returns>
    member inline this.For<'T, 'E when 'E :> exn> (sequence: seq<'T>, body: 'T -> FIO<unit, 'E>) : FIO<unit, 'E> =
        let enumerator = sequence.GetEnumerator()

        let rec loop () =
            if enumerator.MoveNext () then
                this.Delay <| fun () ->
                    body(enumerator.Current).Bind(fun _ -> loop ())
            else
                this.Zero ()

        let errorHandler (exn: exn) : 'E =
            match exn with
            | :? 'E as e -> e
            | e -> unbox e
        let disposeEffect = FIO.FromFunc<unit, 'E>((fun () -> enumerator.Dispose ()), errorHandler)
        this.TryFinally(loop (), disposeEffect)

    /// <summary>
    /// Repeatedly runs the body while the guard returns true.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="guard">A function that returns true to continue looping.</param>
    /// <param name="body">The FIO effect to run each iteration.</param>
    /// <returns>An FIO effect that loops while the guard is true.</returns>
    member inline this.While<'R, 'E> (guard: unit -> bool, body: FIO<'R, 'E>) : FIO<unit, 'E> =
        let rec loop () =
            if guard () then
                this.Delay <| fun () -> body.Bind <| fun _ -> loop ()
            else
                this.Zero ()
        loop ()

    /// <summary>
    /// Uses a disposable resource in a computation expression, ensuring it is disposed after use.
    /// This method supports use! syntax but requires the error type to be an exception.
    /// The resource is always disposed, even if the body fails. Disposal errors are silently ignored
    /// to preserve the original result/error from the body.
    /// </summary>
    /// <typeparam name="'T">The type of the resource.</typeparam>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type (must be an exception type).</typeparam>
    /// <param name="resource">The disposable resource to use.</param>
    /// <param name="body">The function to run with the resource.</param>
    /// <returns>An FIO effect that uses and disposes the resource.</returns>
    member inline this.Using<'T, 'R, 'E when 'T :> IDisposable and 'E :> exn> (resource: 'T, body: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        let errorHandler (exn: exn) : 'E =
            match exn with
            | :? 'E as e -> e
            | e -> unbox e
        let disposeEffect = FIO.FromFunc<unit, 'E>((fun () -> resource.Dispose ()), errorHandler)
        this.TryFinally (body resource, disposeEffect)

    /// <summary>
    /// Pattern matches on a value and runs the corresponding FIO effect.
    /// </summary>
    /// <typeparam name="'R">The result type.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <typeparam name="'T">The type of the value to match on.</typeparam>
    /// <param name="value">The value to match on.</param>
    /// <param name="cases">A function that returns the FIO effect for each case.</param>
    /// <returns>An FIO effect representing the matched case.</returns>
    member inline _.Match<'R, 'E, 'T> (value: 'T, cases: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        cases value

    /// <summary>
    /// Merges two FIO effects into one that returns a tuple of their results.
    /// Sequences both effects and succeeds with a tuple when both complete. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="'R">The result type of the first effect.</typeparam>
    /// <typeparam name="'R1">The result type of the second effect.</typeparam>
    /// <typeparam name="'E">The error type.</typeparam>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <returns>An FIO effect that returns a tuple of both results, or propagates an error.</returns>
    member inline _.MergeSources<'R, 'R1, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Zip eff'

/// <summary>
/// The FIO computation expression builder instance. Enables the use of `fio { ... }` syntax for composing FIO effects.
/// </summary>
let fio = FIOBuilder()
