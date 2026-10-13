namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module internal ZipRace =

    // Builds the ZipRace workload: rounds of ZipPar and RaceFirst over trivial effects, stressing fork/settle/join cost.
    let effect roundCount : FIO<unit, exn> =
        FIO.forEachDiscard (seq { 1..roundCount }) (fun round ->
            ((FIO.succeed round).ZipPar(FIO.succeed (round + 1))).FlatMap <| fun _ ->
                (FIO.succeed round).RaceFirst(FIO.succeed (round + 1)))

// Measures the per-invocation cost of the parallel combinators (ZipPar + RaceFirst).
[<MemoryDiagnoser>]
[<RankColumn>]
type ZipRaceBenchmark() =
    inherit FIOBenchmark()

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_ZIPRACE_ROUNDS" [| 1_000; 10_000 |]

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    override this.CreateEffect () =
        ZipRace.effect this.RoundCount
