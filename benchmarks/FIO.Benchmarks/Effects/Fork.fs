[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fork

open FIO.DSL

// Builds the Fork workload: forks the given number of no-op fibers in parallel.
let effect actorCount : FIO<unit, exn> =
    FIO.forEachParDiscard (seq { 1..actorCount }) (fun _ -> FIO.unit ())
