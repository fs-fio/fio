(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides the computation expression builder for FIO, enabling idiomatic F# computation expression syntax (fio { ... }) for composing functional effects.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.CE

open System

/// <summary>
/// The computation expression builder for FIO effects, enabling F# computation expression syntax (fio { ... }) for composing functional effects.
/// </summary>
[<Sealed>]
type FIOBuilder internal () =

    /// <summary>
    /// Enables <c>let!</c> bindings in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to bind.</param>
    /// <param name="cont">The continuation function.</param>
    member inline _.Bind<'R, 'R1, 'E> (eff: FIO<'R, 'E>, cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        eff.FlatMap cont

    /// <summary>
    /// Enables <c>let! ... return</c> patterns in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to bind.</param>
    /// <param name="cont">The mapping function.</param>
    member inline _.BindReturn<'R, 'R1, 'E> (eff: FIO<'R, 'E>, cont: 'R -> 'R1) : FIO<'R1, 'E> =
        eff.Map cont

    /// <summary>
    /// Enables <c>return</c> expressions in computation expressions.
    /// </summary>
    /// <param name="res">The value to return.</param>
    member inline _.Return<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        FIO.Succeed res

    /// <summary>
    /// Enables <c>return!</c> expressions in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to return.</param>
    member inline _.ReturnFrom<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Enables <c>return!</c> as the final expression in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to return.</param>
    member inline _.ReturnFromFinal<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Enables <c>yield</c> expressions in computation expressions.
    /// </summary>
    /// <param name="res">The value to yield.</param>
    member inline _.Yield<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        FIO.Succeed res

    /// <summary>
    /// Enables <c>yield!</c> expressions in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to yield.</param>
    member inline _.YieldFrom<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Enables <c>yield!</c> as the final expression in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to yield.</param>
    member inline _.YieldFromFinal<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Returns a unit effect for empty computation expressions.
    /// </summary>
    member inline _.Zero<'E> () : FIO<unit, 'E> =
        FIO.Succeed ()

    /// <summary>
    /// Sequences two effects, returning the result of the second.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    member inline _.Combine<'R, 'R1, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R1, 'E> =
        eff.ZipRight eff'

    /// <summary>
    /// Finalizes the computation expression.
    /// </summary>
    /// <param name="eff">The FIO effect.</param>
    member inline _.Run<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        eff

    /// <summary>
    /// Delays execution until the effect is run.
    /// </summary>
    /// <param name="cont">A function that produces the FIO effect.</param>
    member inline _.Delay<'R1, 'E> (cont: unit -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        FIO.Unit().FlatMap cont

    /// <summary>
    /// Enables <c>try...with</c> expressions in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to try.</param>
    /// <param name="handler">The error handler function.</param>
    member inline _.TryWith<'R, 'E, 'E1> (eff: FIO<'R, 'E>, handler: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll handler

    /// <summary>
    /// Enables <c>try...finally</c> expressions in computation expressions.
    /// </summary>
    /// <param name="eff">The FIO effect to try.</param>
    /// <param name="finalizer">The finalizer effect to run after the main effect.</param>
    member inline _.TryFinally<'R, 'E> (eff: FIO<'R, 'E>, finalizer: unit -> FIO<unit, 'E>) : FIO<'R, 'E> =
        eff.Ensuring <| finalizer ()

    /// <summary>
    /// Enables <c>for...do</c> expressions in computation expressions.
    /// </summary>
    /// <param name="sequence">The sequence to iterate over.</param>
    /// <param name="body">The function to run for each element.</param>
    member inline _.For<'T, 'E> (sequence: seq<'T>, body: 'T -> FIO<unit, 'E>) : FIO<unit, 'E> =
        FIO.Unit().FlatMap(fun () ->
            let enumerator = sequence.GetEnumerator()

            let rec loop () =
                if enumerator.MoveNext() then
                    body(enumerator.Current).FlatMap(fun _ -> loop())
                else
                    FIO.Unit()

            let dispose = FIO.Unit().FlatMap(fun () -> enumerator.Dispose(); FIO.Unit())
            loop().Ensuring dispose)

    /// <summary>
    /// Enables <c>while...do</c> expressions in computation expressions.
    /// </summary>
    /// <param name="guard">A function that returns true to continue looping.</param>
    /// <param name="body">The FIO effect to run each iteration.</param>
    member inline _.While<'R, 'E> (guard: unit -> bool, body: FIO<'R, 'E>) : FIO<unit, 'E> =
        let rec loop () =
            if guard () then
                body.FlatMap(fun _ -> loop ())
            else
                FIO.Succeed ()
        loop ()

    /// <summary>
    /// Enables <c>use</c> bindings in computation expressions.
    /// </summary>
    /// <param name="resource">The disposable resource.</param>
    /// <param name="body">The function to run with the resource.</param>
    member inline _.Using<'T, 'R, 'E when 'T :> IDisposable> (resource: 'T, body: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        (body resource).Ensuring(FIO.Unit().FlatMap(
            fun () -> resource.Dispose(); FIO.Unit()))

    /// <summary>
    /// Enables pattern matching in computation expressions.
    /// </summary>
    /// <param name="value">The value to match on.</param>
    /// <param name="cases">The function mapping values to effects.</param>
    member inline _.Match<'R, 'E, 'T> (value: 'T, cases: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        cases value

    /// <summary>
    /// Enables <c>let! ... and! ...</c> patterns in computation expressions.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    member inline _.MergeSources<'R, 'R1, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Zip eff'

    /// <summary>
    /// Enables <c>let! ... and! ... and! ...</c> patterns in computation expressions.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    member inline _.MergeSources3<'R, 'R1, 'R2, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>) : FIO<'R * 'R1 * 'R2, 'E> =
        eff.Zip(eff').FlatMap(fun (r, r1) ->
            eff''.Map(fun r2 -> r, r1, r2))

    /// <summary>
    /// Enables <c>let! ... and! ... and! ... and! ...</c> patterns in computation expressions.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    member inline _.MergeSources4<'R, 'R1, 'R2, 'R3, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>) : FIO<'R * 'R1 * 'R2 * 'R3, 'E> =
        eff.Zip(eff').FlatMap(fun (r, r1) ->
            eff''.Zip(eff''').Map(fun (r2, r3) -> r, r1, r2, r3))

    /// <summary>
    /// Enables <c>let! ... and! ... and! ... and! ... and! ...</c> patterns in computation expressions.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    /// <param name="eff''''">The fifth FIO effect.</param>
    member inline _.MergeSources5<'R, 'R1, 'R2, 'R3, 'R4, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, eff'''': FIO<'R4, 'E>) : FIO<'R * 'R1 * 'R2 * 'R3 * 'R4, 'E> =
        eff.Zip(eff').FlatMap(fun (r, r1) ->
            eff''.Zip(eff''').FlatMap(fun (r2, r3) ->
                eff''''.Map(fun r4 -> r, r1, r2, r3, r4)))

    /// <summary>
    /// Optimized binding for three effects in <c>let! ... and! ... and! ...</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="cont">The continuation function.</param>
    member inline this.Bind3<'R, 'R1, 'R2, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, cont: 'R * 'R1 * 'R2 -> FIO<'ROut, 'E>) : FIO<'ROut, 'E> =
        this.MergeSources3(eff, eff', eff'').FlatMap cont

    /// <summary>
    /// Optimized binding for four effects in <c>let! ... and! ...</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    /// <param name="cont">The continuation function.</param>
    member inline this.Bind4<'R, 'R1, 'R2, 'R3, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, cont: 'R * 'R1 * 'R2 * 'R3 -> FIO<'ROut, 'E>) : FIO<'ROut, 'E> =
        this.MergeSources4(eff, eff', eff'', eff''').FlatMap cont

    /// <summary>
    /// Optimized binding for five effects in <c>let! ... and! ...</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    /// <param name="eff''''">The fifth FIO effect.</param>
    /// <param name="cont">The continuation function.</param>
    member inline this.Bind5<'R, 'R1, 'R2, 'R3, 'R4, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, eff'''': FIO<'R4, 'E>, cont: 'R * 'R1 * 'R2 * 'R3 * 'R4 -> FIO<'ROut, 'E>) : FIO<'ROut, 'E> =
        this.MergeSources5(eff, eff', eff'', eff''', eff'''').FlatMap cont

    /// <summary>
    /// Optimized bind-and-return for three effects in <c>let! ... and! ... return</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="cont">The mapping function.</param>
    member inline this.Bind3Return<'R, 'R1, 'R2, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, cont: 'R * 'R1 * 'R2 -> 'ROut) : FIO<'ROut, 'E> =
        this.MergeSources3(eff, eff', eff'').Map cont

    /// <summary>
    /// Optimized bind-and-return for four effects in <c>let! ... and! ... return</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    /// <param name="cont">The mapping function.</param>
    member inline this.Bind4Return<'R, 'R1, 'R2, 'R3, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, cont: 'R * 'R1 * 'R2 * 'R3 -> 'ROut) : FIO<'ROut, 'E> =
        this.MergeSources4(eff, eff', eff'', eff''').Map cont

    /// <summary>
    /// Optimized bind-and-return for five effects in <c>let! ... and! ... return</c> patterns.
    /// </summary>
    /// <param name="eff">The first FIO effect.</param>
    /// <param name="eff'">The second FIO effect.</param>
    /// <param name="eff''">The third FIO effect.</param>
    /// <param name="eff'''">The fourth FIO effect.</param>
    /// <param name="eff''''">The fifth FIO effect.</param>
    /// <param name="cont">The mapping function.</param>
    member inline this.Bind5Return<'R, 'R1, 'R2, 'R3, 'R4, 'ROut, 'E> (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, eff'''': FIO<'R4, 'E>, cont: 'R * 'R1 * 'R2 * 'R3 * 'R4 -> 'ROut) : FIO<'ROut, 'E> =
        this.MergeSources5(eff, eff', eff'', eff''', eff'''').Map cont

/// <summary>
/// The FIO computation expression builder instance. Enables idiomatic F# computation expression syntax (fio { ... }) for composing functional effects.
/// </summary>
let fio = FIOBuilder()
