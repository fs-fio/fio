/// Computation expression builder for FIO effects.
[<AutoOpen>]
module FIO.DSL.CE

open System

/// The computation expression builder for FIO effects, enabling fio { ... } syntax.
[<Sealed>]
type FIOBuilder internal () =

    /// Enables let! bindings.
    /// <param name="eff">The effect to bind.</param>
    /// <param name="cont">Continuation to apply to the result.</param>
    /// <returns>An effect chaining the binding and continuation.</returns>
    member inline _.Bind<'R, 'R1, 'E>(eff: FIO<'R, 'E>, cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap cont

    /// Enables let! ... return patterns.
    /// <param name="eff">The effect to bind.</param>
    /// <param name="cont">Mapping function to apply to the result.</param>
    /// <returns>An effect whose result is the mapped value.</returns>
    member inline _.BindReturn<'R, 'R1, 'E>(eff: FIO<'R, 'E>, cont: 'R -> 'R1) : FIO<'R1, 'E> = eff.Map cont

    /// Enables return expressions.
    /// <param name="res">The value to wrap in a successful effect.</param>
    /// <returns>An effect that succeeds with the given value.</returns>
    member inline _.Return<'R, 'E>(res: 'R) : FIO<'R, 'E> = FIO.succeed res

    /// Enables return! expressions.
    /// <param name="eff">The effect to return directly.</param>
    /// <returns>The given effect unchanged.</returns>
    member inline _.ReturnFrom<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables return! as the final expression.
    /// <param name="eff">The effect to return as the final result.</param>
    /// <returns>The given effect unchanged.</returns>
    member inline _.ReturnFromFinal<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables yield expressions.
    /// <param name="res">The value to wrap in a successful effect.</param>
    /// <returns>An effect that succeeds with the given value.</returns>
    member inline _.Yield<'R, 'E>(res: 'R) : FIO<'R, 'E> = FIO.succeed res

    /// Enables yield! expressions.
    /// <param name="eff">The effect to yield directly.</param>
    /// <returns>The given effect unchanged.</returns>
    member inline _.YieldFrom<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables yield! as the final expression.
    /// <param name="eff">The effect to yield as the final result.</param>
    /// <returns>The given effect unchanged.</returns>
    member inline _.YieldFromFinal<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Returns a unit effect for empty computation expressions.
    /// <returns>An effect that succeeds with unit.</returns>
    member inline _.Zero<'E>() : FIO<unit, 'E> = FIO.succeed ()

    /// Sequences two effects, returning the second result.
    /// <param name="eff">The left effect to evaluate first.</param>
    /// <param name="eff'">The right effect to evaluate second.</param>
    /// <returns>An effect that evaluates both sequentially and returns the second result.</returns>
    member inline _.Combine<'R, 'R1, 'E>(eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipRight eff'

    /// Finalizes the computation expression.
    /// <param name="eff">The effect to finalize.</param>
    /// <returns>The given effect unchanged.</returns>
    member inline _.Run<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Defers effect construction until execution.
    /// <param name="cont">The function that produces the deferred effect.</param>
    /// <returns>An effect that suspends the given function until evaluation.</returns>
    member inline _.Delay<'R1, 'E>(cont: unit -> FIO<'R1, 'E>) : FIO<'R1, 'E> = FIO.unit().FlatMap cont

    /// Enables try...with error handling.
    /// <param name="eff">The effect to attempt.</param>
    /// <param name="handler">Error handler applied when the effect fails.</param>
    /// <returns>An effect that catches errors from the first effect using the handler.</returns>
    member inline _.TryWith<'R, 'E, 'E1>(eff: FIO<'R, 'E>, handler: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll handler

    /// Enables try...finally guarantees.
    /// <param name="eff">The effect to evaluate.</param>
    /// <param name="finalizer">Cleanup function that runs on success, failure, and interruption.</param>
    /// <returns>An effect that ensures the finalizer runs after the main effect completes.</returns>
    member inline _.TryFinally<'R, 'E>(eff: FIO<'R, 'E>, finalizer: unit -> FIO<unit, 'E>) : FIO<'R, 'E> =
        eff.Ensuring(FIO.suspend finalizer)

    /// Enables for...do iteration over sequences.
    /// <param name="sequence">The sequence to iterate over.</param>
    /// <param name="body">The effect-producing function applied to each element.</param>
    /// <returns>An effect that evaluates the body for each element sequentially.</returns>
    member inline _.For<'T, 'E>(sequence: seq<'T>, body: 'T -> FIO<unit, 'E>) : FIO<unit, 'E> =
        FIO.suspend (fun () ->
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
                reraise ())

    /// Enables while...do loops.
    /// <param name="guard">Condition function evaluated before each iteration.</param>
    /// <param name="body">The effect to execute on each iteration.</param>
    /// <returns>An effect that loops while the guard returns true.</returns>
    member inline _.While<'R, 'E>(guard: unit -> bool, body: FIO<'R, 'E>) : FIO<unit, 'E> =
        let rec loop () =
            if guard () then
                body.FlatMap(fun _ -> loop ())
            else
                FIO.succeed ()

        FIO.suspend loop

    /// Enables use bindings with IDisposable resources.
    /// <param name="resource">The disposable resource to manage.</param>
    /// <param name="body">The effect-producing function that uses the resource.</param>
    /// <returns>An effect that uses the resource and guarantees disposal.</returns>
    member inline _.Using<'T, 'R, 'E when 'T :> IDisposable>(resource: 'T, body: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        let dispose =
            FIO.suspend (fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()

                FIO.unit ())

        (body resource).Ensuring dispose

    /// Enables pattern matching in computation expressions.
    /// <param name="value">The value to match against.</param>
    /// <param name="cases">The pattern matching function producing an effect.</param>
    /// <returns>An effect produced by applying the value to the cases function.</returns>
    member inline _.Match<'R, 'E, 'T>(value: 'T, cases: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> = cases value

    /// Enables and! patterns with 2 sources.
    /// <param name="eff">The first effect.</param>
    /// <param name="eff'">The second effect.</param>
    /// <returns>An effect that executes both in parallel and returns a tuple of their results.</returns>
    /// <remarks>Both effects run concurrently in fiber runtimes.</remarks>
    member inline _.MergeSources<'R, 'R1, 'E>(eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.ZipPar eff'

    /// Enables and! patterns with 3 sources.
    /// <param name="eff">The first effect.</param>
    /// <param name="eff'">The second effect.</param>
    /// <param name="eff''">The third effect.</param>
    /// <returns>An effect that executes all three in parallel and returns a triple of their results.</returns>
    /// <remarks>All effects run concurrently in fiber runtimes.</remarks>
    member inline _.MergeSources3<'R, 'R1, 'R2, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>)
        : FIO<'R * 'R1 * 'R2, 'E> =
        eff.ZipPar(eff').ZipPar(eff'').Map(fun ((r, r1), r2) -> r, r1, r2)

    /// Enables and! patterns with 4 sources.
    /// <param name="eff">The first effect.</param>
    /// <param name="eff'">The second effect.</param>
    /// <param name="eff''">The third effect.</param>
    /// <param name="eff'''">The fourth effect.</param>
    /// <returns>An effect that executes all four in parallel and returns a quadruple of their results.</returns>
    /// <remarks>All effects run concurrently in fiber runtimes.</remarks>
    member inline _.MergeSources4<'R, 'R1, 'R2, 'R3, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>)
        : FIO<'R * 'R1 * 'R2 * 'R3, 'E> =
        eff
            .ZipPar(eff')
            .ZipPar(eff'')
            .ZipPar(eff''')
            .Map(fun (((r, r1), r2), r3) -> r, r1, r2, r3)

    /// Enables and! patterns with 5 sources.
    /// <param name="eff">The first effect.</param>
    /// <param name="eff'">The second effect.</param>
    /// <param name="eff''">The third effect.</param>
    /// <param name="eff'''">The fourth effect.</param>
    /// <param name="eff''''">The fifth effect.</param>
    /// <returns>An effect that executes all five in parallel and returns a quintuple of their results.</returns>
    /// <remarks>All effects run concurrently in fiber runtimes.</remarks>
    member inline _.MergeSources5<'R, 'R1, 'R2, 'R3, 'R4, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, eff'''': FIO<'R4, 'E>)
        : FIO<'R * 'R1 * 'R2 * 'R3 * 'R4, 'E> =
        eff
            .ZipPar(eff')
            .ZipPar(eff'')
            .ZipPar(eff''')
            .ZipPar(eff'''')
            .Map(fun ((((r, r1), r2), r3), r4) -> r, r1, r2, r3, r4)

/// The FIO computation expression builder instance.
let fio = FIOBuilder()
