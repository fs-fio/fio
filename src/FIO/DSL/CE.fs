/// Computation expression builder for FIO effects.
[<AutoOpen>]
module FIO.DSL.CE

open System

/// The computation expression builder for FIO effects, enabling fio { ... } syntax.
[<Sealed>]
type FIOBuilder internal () =

    /// Enables let! bindings.
    member inline _.Bind<'R, 'R1, 'E>(eff: FIO<'R, 'E>, cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap cont

    /// Enables let! ... return patterns.
    member inline _.BindReturn<'R, 'R1, 'E>(eff: FIO<'R, 'E>, cont: 'R -> 'R1) : FIO<'R1, 'E> = eff.Map cont

    /// Enables return expressions.
    member inline _.Return<'R, 'E>(res: 'R) : FIO<'R, 'E> = FIO.succeed res

    /// Enables return! expressions.
    member inline _.ReturnFrom<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables return! as the final expression.
    member inline _.ReturnFromFinal<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables yield expressions.
    member inline _.Yield<'R, 'E>(res: 'R) : FIO<'R, 'E> = FIO.succeed res

    /// Enables yield! expressions.
    member inline _.YieldFrom<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Enables yield! as the final expression.
    member inline _.YieldFromFinal<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Returns a unit effect for empty computation expressions.
    member inline _.Zero<'E>() : FIO<unit, 'E> = FIO.succeed ()

    /// Sequences two effects, returning the second result.
    member inline _.Combine<'R, 'R1, 'E>(eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipRight eff'

    /// Finalizes the computation expression.
    member inline _.Run<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> = eff

    /// Defers effect construction until execution.
    member inline _.Delay<'R1, 'E>(cont: unit -> FIO<'R1, 'E>) : FIO<'R1, 'E> = FIO.unit().FlatMap cont

    /// Enables try...with error handling.
    member inline _.TryWith<'R, 'E, 'E1>(eff: FIO<'R, 'E>, handler: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll handler

    /// Enables try...finally guarantees.
    member inline _.TryFinally<'R, 'E>(eff: FIO<'R, 'E>, finalizer: unit -> FIO<unit, 'E>) : FIO<'R, 'E> =
        eff.Ensuring(FIO.suspend finalizer)

    /// Enables for...do iteration over sequences.
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
    member inline _.While<'R, 'E>(guard: unit -> bool, body: FIO<'R, 'E>) : FIO<unit, 'E> =
        let rec loop () =
            if guard () then
                body.FlatMap(fun _ -> loop ())
            else
                FIO.succeed ()

        FIO.suspend loop

    /// Enables use bindings with IDisposable resources.
    member inline _.Using<'T, 'R, 'E when 'T :> IDisposable>(resource: 'T, body: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> =
        let dispose =
            FIO.suspend (fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()

                FIO.unit ())

        (body resource).Ensuring dispose

    /// Enables pattern matching in computation expressions.
    member inline _.Match<'R, 'E, 'T>(value: 'T, cases: 'T -> FIO<'R, 'E>) : FIO<'R, 'E> = cases value

    /// Enables and! patterns with 2 sources.
    member inline _.MergeSources<'R, 'R1, 'E>(eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.Zip eff'

    /// Enables and! patterns with 3 sources.
    member inline _.MergeSources3<'R, 'R1, 'R2, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>)
        : FIO<'R * 'R1 * 'R2, 'E> =
        eff.Zip(eff').FlatMap(fun (r, r1) -> eff''.Map(fun r2 -> r, r1, r2))

    /// Enables and! patterns with 4 sources.
    member inline _.MergeSources4<'R, 'R1, 'R2, 'R3, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>)
        : FIO<'R * 'R1 * 'R2 * 'R3, 'E> =
        eff.Zip(eff').FlatMap(fun (r, r1) -> eff''.Zip(eff''').Map(fun (r2, r3) -> r, r1, r2, r3))

    /// Enables and! patterns with 5 sources.
    member inline _.MergeSources5<'R, 'R1, 'R2, 'R3, 'R4, 'E>
        (eff: FIO<'R, 'E>, eff': FIO<'R1, 'E>, eff'': FIO<'R2, 'E>, eff''': FIO<'R3, 'E>, eff'''': FIO<'R4, 'E>)
        : FIO<'R * 'R1 * 'R2 * 'R3 * 'R4, 'E> =
        eff
            .Zip(eff')
            .FlatMap(fun (r, r1) -> eff''.Zip(eff''').FlatMap(fun (r2, r3) -> eff''''.Map(fun r4 -> r, r1, r2, r3, r4)))

/// The FIO computation expression builder instance.
let fio = FIOBuilder()
