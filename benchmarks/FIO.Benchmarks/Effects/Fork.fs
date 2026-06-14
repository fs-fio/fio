[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fork

open FIO.DSL

let effect actorCount : FIO<unit, exn> =
    FIO.forEachParDiscard (seq { 1..actorCount }) (fun _ -> FIO.unit ())
