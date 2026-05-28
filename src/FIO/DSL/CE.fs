/// <summary>Provides the computation expression builder for FIO effects.</summary>
[<AutoOpen>]
module FIO.DSL.CE

open System

/// <summary>Represents the computation expression builder backing the <c>fio { ... }</c> syntax for composing FIO effects.</summary>
[<Sealed>]
type FIOBuilder internal () =

    /// <summary>Combines an effect with a continuation invoked on its successful result, supporting <c>let!</c> bindings.</summary>
    /// <typeparam name="'A1">The success result type produced by the continuation.</typeparam>
    /// <param name="eff">The effect whose success value is bound.</param>
    /// <param name="cont">A function from the success value of <paramref name="eff"/> to the next effect to run.</param>
    /// <returns>An effect that runs <paramref name="eff"/> and then runs the effect produced by <paramref name="cont"/>.</returns>
    member inline _.Bind<'A, 'A1, 'E> (eff: FIO<'A, 'E>, cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        eff.FlatMap cont

    /// <summary>Transforms the success value of an effect with a pure function, supporting <c>let! ... return</c> patterns.</summary>
    /// <typeparam name="'A1">The success result type produced by the mapper.</typeparam>
    /// <param name="eff">The effect whose success value is transformed.</param>
    /// <param name="cont">A pure function from the success value of <paramref name="eff"/> to the new success value.</param>
    /// <returns>An effect that completes with <paramref name="cont"/> applied to <paramref name="eff"/>'s success value.</returns>
    member inline _.BindReturn<'A, 'A1, 'E> (eff: FIO<'A, 'E>, cont: 'A -> 'A1) : FIO<'A1, 'E> =
        eff.Map cont

    /// <summary>Lifts a value into a successful effect, supporting <c>return</c> expressions.</summary>
    /// <param name="value">The value to wrap as a successful result.</param>
    /// <returns>An effect that completes immediately with <paramref name="value"/>.</returns>
    member inline _.Return<'A, 'E> (value: 'A) : FIO<'A, 'E> =
        FIO.succeed value

    /// <summary>Returns the given effect unchanged, supporting <c>return!</c> expressions.</summary>
    /// <param name="eff">The effect to return as the result of the computation expression.</param>
    /// <returns>The effect <paramref name="eff"/>.</returns>
    member inline _.ReturnFrom<'A, 'E> (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        eff

    /// <summary>Returns the given effect unchanged, supporting <c>return!</c> as the final expression of a computation.</summary>
    /// <param name="eff">The effect to return as the final result.</param>
    /// <returns>The effect <paramref name="eff"/>.</returns>
    member inline _.ReturnFromFinal<'A, 'E> (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        eff

    /// <summary>Lifts a value into a successful effect, supporting <c>yield</c> expressions.</summary>
    /// <param name="value">The value to wrap as a successful result.</param>
    /// <returns>An effect that completes immediately with <paramref name="value"/>.</returns>
    member inline _.Yield<'A, 'E> (value: 'A) : FIO<'A, 'E> =
        FIO.succeed value

    /// <summary>Returns the given effect unchanged, supporting <c>yield!</c> expressions.</summary>
    /// <param name="eff">The effect to yield as the result of the computation expression.</param>
    /// <returns>The effect <paramref name="eff"/>.</returns>
    member inline _.YieldFrom<'A, 'E> (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        eff

    /// <summary>Returns the given effect unchanged, supporting <c>yield!</c> as the final expression of a computation.</summary>
    /// <param name="eff">The effect to yield as the final result.</param>
    /// <returns>The effect <paramref name="eff"/>.</returns>
    member inline _.YieldFromFinal<'A, 'E> (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        eff

    /// <summary>Lifts unit into a successful effect, supplying the default value for empty computation expressions.</summary>
    /// <returns>An effect that completes successfully with unit.</returns>
    member inline _.Zero<'E>() : FIO<unit, 'E> =
        FIO.succeed ()

    /// <summary>Combines two effects sequentially and returns the second effect's result, supporting statement sequencing inside the builder.</summary>
    /// <typeparam name="'A1">The success result type of the second effect; propagated.</typeparam>
    /// <param name="eff">The effect to evaluate first; its result is discarded.</param>
    /// <param name="eff'">The effect to evaluate second; its result is propagated.</param>
    /// <returns>An effect that runs both in order and completes with the second result.</returns>
    member inline _.Combine<'A, 'A1, 'E> (eff: FIO<'A, 'E>, eff': FIO<'A1, 'E>) : FIO<'A1, 'E> =
        eff.ZipRight eff'

    /// <summary>Returns the assembled effect produced by the computation expression unchanged.</summary>
    /// <param name="eff">The effect to return as the final result of the builder.</param>
    /// <returns>The effect <paramref name="eff"/>.</returns>
    member inline _.Run<'A, 'E> (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        eff

    /// <summary>Builds an effect whose construction is deferred until the resulting effect is interpreted.</summary>
    /// <typeparam name="'A1">The success result type of the deferred effect.</typeparam>
    /// <param name="cont">A factory function producing the deferred effect at evaluation time.</param>
    /// <returns>An effect that calls <paramref name="cont"/> at evaluation time and runs the effect it produces.</returns>
    member inline _.Delay<'A1, 'E> (cont: unit -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        FIO.unit().FlatMap cont

    /// <summary>Combines an effect with an error handler, supporting <c>try ... with</c> blocks.</summary>
    /// <typeparam name="'E1">The error type of the recovered effect.</typeparam>
    /// <param name="eff">The effect to attempt.</param>
    /// <param name="handler">A function from the typed error to the recovery effect.</param>
    /// <returns>An effect that completes with <paramref name="eff"/>'s success value, or with the recovery effect's outcome on failure.</returns>
    member inline _.TryWith<'A, 'E, 'E1> (eff: FIO<'A, 'E>, handler: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
        eff.CatchAll handler

    /// <summary>Combines an effect with a finalizer that runs on every outcome, supporting <c>try ... finally</c> blocks.</summary>
    /// <param name="eff">The effect to evaluate.</param>
    /// <param name="finalizer">A factory producing the cleanup effect that runs after <paramref name="eff"/> on success, failure, and interruption.</param>
    /// <returns>An effect that completes with <paramref name="eff"/>'s outcome and always runs the cleanup effect afterwards.</returns>
    member inline _.TryFinally<'A, 'E> (eff: FIO<'A, 'E>, finalizer: unit -> FIO<unit, 'E>) : FIO<'A, 'E> =
        eff.Ensuring <| FIO.suspend finalizer

    /// <summary>Combines a sequence with a per-element body, supporting <c>for ... do</c> iteration.</summary>
    /// <typeparam name="'T">The element type of the sequence.</typeparam>
    /// <param name="sequence">The sequence to iterate.</param>
    /// <param name="body">A function from each element to the effect that processes it.</param>
    /// <returns>An effect that runs <paramref name="body"/> for each element in order and completes with unit.</returns>
    member inline _.For<'T, 'E> (sequence: seq<'T>, body: 'T -> FIO<unit, 'E>) : FIO<unit, 'E> =
        FIO.suspend <|fun () ->
            let enumerator = sequence.GetEnumerator()
            try
                let rec loop () =
                    if enumerator.MoveNext() then
                        body(enumerator.Current).FlatMap(fun _ -> loop ())
                    else
                        FIO.unit ()

                let dispose =
                    FIO.suspend (fun () ->
                        enumerator.Dispose()
                        FIO.unit ())

                loop().Ensuring dispose
            with
            | :? ObjectDisposedException ->
                enumerator.Dispose()
                reraise ()
            | :? InvalidOperationException ->
                enumerator.Dispose()
                reraise ()

    /// <summary>Combines a guard predicate with a body effect, supporting <c>while ... do</c> loops.</summary>
    /// <param name="guard">A predicate evaluated before each iteration; the loop continues while it returns <c>true</c>.</param>
    /// <param name="body">The effect to evaluate on each iteration.</param>
    /// <returns>An effect that repeatedly runs <paramref name="body"/> until <paramref name="guard"/> returns <c>false</c>.</returns>
    member inline _.While<'A, 'E> (guard: unit -> bool, body: FIO<'A, 'E>) : FIO<unit, 'E> =
        let rec loop () =
            if guard () then
                body.FlatMap(fun _ -> loop ())
            else
                FIO.succeed ()
        FIO.suspend loop

    /// <summary>Builds a resource-managed effect that disposes the resource after use, supporting <c>use</c> bindings.</summary>
    /// <typeparam name="'T">The disposable resource type.</typeparam>
    /// <typeparam name="'A">The success result type of the body effect.</typeparam>
    /// <param name="resource">The disposable resource to manage; <c>null</c> is treated as already disposed.</param>
    /// <param name="body">A function from the resource to the effect that uses it.</param>
    /// <returns>An effect that runs <paramref name="body"/> and disposes <paramref name="resource"/> on every outcome.</returns>
    member inline _.Using<'T, 'A, 'E when 'T :> IDisposable> (resource: 'T, body: 'T -> FIO<'A, 'E>) : FIO<'A, 'E> =
        let dispose =
            FIO.suspend <| fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()
                FIO.unit ()
        (body resource).Ensuring dispose

    /// <summary>Combines a value with a pattern-matching function that produces an effect, supporting pattern matching inside the builder.</summary>
    /// <typeparam name="'T">The type of the matched value.</typeparam>
    /// <param name="value">The value to match against.</param>
    /// <param name="cases">A function from the value to the effect produced by the matched arm.</param>
    /// <returns>The effect produced by applying <paramref name="cases"/> to <paramref name="value"/>.</returns>
    member inline _.Match<'A, 'E, 'T> (value: 'T, cases: 'T -> FIO<'A, 'E>) : FIO<'A, 'E> =
        cases value

    /// <summary>Combines two effects in parallel and returns both results as a tuple, supporting <c>and!</c> bindings with two sources.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <param name="eff">The first effect to fork.</param>
    /// <param name="eff'">The second effect to fork.</param>
    /// <returns>An effect that runs both concurrently and completes with a pair of their results.</returns>
    /// <remarks>Both effects run on separate fibers in fiber runtimes.</remarks>
    member inline _.MergeSources<'A, 'A1, 'E> (eff: FIO<'A, 'E>, eff': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        eff.ZipPar eff'

    /// <summary>Combines three effects in parallel and returns all three results as a tuple, supporting <c>and!</c> bindings with three sources.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <typeparam name="'A2">The success result type of the third effect.</typeparam>
    /// <param name="eff">The first effect to fork.</param>
    /// <param name="eff'">The second effect to fork.</param>
    /// <param name="eff''">The third effect to fork.</param>
    /// <returns>An effect that runs all three concurrently and completes with a triple of their results.</returns>
    /// <remarks>All effects run on separate fibers in fiber runtimes.</remarks>
    member inline _.MergeSources3<'A, 'A1, 'A2, 'E> (eff: FIO<'A, 'E>, eff': FIO<'A1, 'E>, eff'': FIO<'A2, 'E>) : FIO<'A * 'A1 * 'A2, 'E> =
        eff.ZipPar(eff').ZipPar(eff'').Map(fun ((r, r1), r2) -> r, r1, r2)

    /// <summary>Combines four effects in parallel and returns all four results as a tuple, supporting <c>and!</c> bindings with four sources.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <typeparam name="'A2">The success result type of the third effect.</typeparam>
    /// <typeparam name="'A3">The success result type of the fourth effect.</typeparam>
    /// <param name="eff">The first effect to fork.</param>
    /// <param name="eff'">The second effect to fork.</param>
    /// <param name="eff''">The third effect to fork.</param>
    /// <param name="eff'''">The fourth effect to fork.</param>
    /// <returns>An effect that runs all four concurrently and completes with a quadruple of their results.</returns>
    /// <remarks>All effects run on separate fibers in fiber runtimes.</remarks>
    member inline _.MergeSources4<'A, 'A1, 'A2, 'A3, 'E> (eff: FIO<'A, 'E>, eff': FIO<'A1, 'E>, eff'': FIO<'A2, 'E>, eff''': FIO<'A3, 'E>) : FIO<'A * 'A1 * 'A2 * 'A3, 'E> =
        eff.ZipPar(eff').ZipPar(eff'').ZipPar(eff''').Map(fun (((r, r1), r2), r3) -> r, r1, r2, r3)

    /// <summary>Combines five effects in parallel and returns all five results as a tuple, supporting <c>and!</c> bindings with five sources.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <typeparam name="'A2">The success result type of the third effect.</typeparam>
    /// <typeparam name="'A3">The success result type of the fourth effect.</typeparam>
    /// <typeparam name="'A4">The success result type of the fifth effect.</typeparam>
    /// <param name="eff">The first effect to fork.</param>
    /// <param name="eff'">The second effect to fork.</param>
    /// <param name="eff''">The third effect to fork.</param>
    /// <param name="eff'''">The fourth effect to fork.</param>
    /// <param name="eff''''">The fifth effect to fork.</param>
    /// <returns>An effect that runs all five concurrently and completes with a quintuple of their results.</returns>
    /// <remarks>All effects run on separate fibers in fiber runtimes.</remarks>
    member inline _.MergeSources5<'A, 'A1, 'A2, 'A3, 'A4, 'E> (eff: FIO<'A, 'E>, eff': FIO<'A1, 'E>, eff'': FIO<'A2, 'E>, eff''': FIO<'A3, 'E>, eff'''': FIO<'A4, 'E>) : FIO<'A * 'A1 * 'A2 * 'A3 * 'A4, 'E> =
        eff
            .ZipPar(eff')
            .ZipPar(eff'')
            .ZipPar(eff''')
            .ZipPar(eff'''')
            .Map(fun ((((r, r1), r2), r3), r4) ->
                r, r1, r2, r3, r4)

/// <summary>Returns the shared builder instance for the <c>fio { ... }</c> computation expression.</summary>
let fio = FIOBuilder()
