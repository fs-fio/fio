namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures the per-invocation cost of the parallel combinators (ZipPar + RaceFirst).
[<MemoryDiagnoser>]
[<RankColumn>]
type ZipRaceBenchmark() =
    let mutable runtime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_ZIPRACE_ROUNDS" [| 1_000; 10_000 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- ZipRace.effect this.RoundCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
