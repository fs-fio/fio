namespace FIO.DSL

open System

[<AutoOpen>]
module CE =

    [<Sealed>]
    type FIOBuilder internal () =

        member inline _.Bind<'A, 'A1, 'E> (effect: FIO<'A, 'E>, cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
            effect.FlatMap cont

        member inline _.BindReturn<'A, 'A1, 'E> (effect: FIO<'A, 'E>, cont: 'A -> 'A1) : FIO<'A1, 'E> =
            effect.Map cont

        member inline _.Return<'A, 'E> (value: 'A) : FIO<'A, 'E> =
            FIO.succeed value

        member inline _.ReturnFrom<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            effect

        member inline _.ReturnFromFinal<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            effect

        member inline _.Yield<'A, 'E> (value: 'A) : FIO<'A, 'E> =
            FIO.succeed value

        member inline _.YieldFrom<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            effect

        member inline _.YieldFromFinal<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            effect

        member inline _.Zero<'E> () : FIO<unit, 'E> =
            FIO.succeed ()

        member inline _.Combine<'A, 'A1, 'E> (effect: FIO<'A, 'E>, effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
            effect.ZipRight effect'

        member inline _.Run<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            effect

        member inline _.Delay<'A1, 'E> (cont: unit -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
            FIO.suspend cont

        member inline _.TryWith<'A, 'E, 'E1> (effect: FIO<'A, 'E>, handler: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> =
            effect.CatchAll handler

        member inline _.TryFinally<'A, 'E> (effect: FIO<'A, 'E>, finalizer: unit -> FIO<unit, 'E>) : FIO<'A, 'E> =
            effect.Ensuring <| FIO.suspend finalizer

        member inline _.For<'T, 'E> (sequence: seq<'T>, body: 'T -> FIO<unit, 'E>) : FIO<unit, 'E> =
            FIO.suspend <| fun () ->
                let enumerator = sequence.GetEnumerator()

                let dispose =
                    FIO.suspend <| fun () ->
                        enumerator.Dispose()
                        FIO.unit ()

                let rec loop () =
                    if enumerator.MoveNext() then
                        body(enumerator.Current).FlatMap <| fun _ ->
                            loop ()
                    else
                        FIO.unit ()

                (FIO.suspend loop).Ensuring dispose

        member inline _.While<'A, 'E> (guard: unit -> bool, body: FIO<'A, 'E>) : FIO<unit, 'E> =
            let rec loop () =
                if guard () then
                    body.FlatMap <| fun _ ->
                        loop ()
                else
                    FIO.succeed ()
            FIO.suspend loop

        member inline _.Using<'R, 'A, 'E when 'R :> IDisposable> (resource: 'R, body: 'R -> FIO<'A, 'E>) : FIO<'A, 'E> =
            let dispose =
                FIO.suspend <| fun () ->
                    if not (obj.ReferenceEquals(resource, null)) then
                        resource.Dispose()
                    FIO.unit ()
            (body resource).Ensuring dispose

        member inline _.MergeSources<'A, 'A1, 'E> (effect: FIO<'A, 'E>, effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
            effect.ZipPar effect'

    /// <summary>Builds effects with <c>let!</c>, <c>do!</c>, <c>return</c>, <c>for</c>, <c>while</c>, <c>use</c>,
    /// <c>try/with</c> and <c>try/finally</c>, and <c>and!</c>, which runs its effects concurrently.</summary>
    let fio : FIOBuilder = FIOBuilder()
