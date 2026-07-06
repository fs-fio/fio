[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.ZipRace

open FIO.DSL

// Builds the ZipRace workload: sequential rounds of ZipPar and RaceFirst over trivial
// effects, stressing the parallel combinators' per-invocation fork/settle/join cost.
let effect roundCount : FIO<unit, exn> =
    FIO.forEachDiscard (seq { 1..roundCount }) (fun round ->
        ((FIO.succeed round).ZipPar(FIO.succeed (round + 1))).FlatMap <| fun _ ->
            (FIO.succeed round).RaceFirst(FIO.succeed (round + 1)))
