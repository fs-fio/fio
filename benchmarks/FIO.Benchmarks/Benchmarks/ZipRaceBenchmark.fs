namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures the per-invocation cost of the parallel combinators (ZipPar + RaceFirst).
[<MemoryDiagnoser>]
[<RankColumn>]
type ZipRaceBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Round counts to sweep, overridable via FIO_BENCH_ZIPRACE_ROUNDS.
    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_ZIPRACE_ROUNDS" [| 1_000; 10_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The round count for the current run.
    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- ZipRace.effect this.RoundCount

    // Disposes the runtime if it owns unmanaged resources.
    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    // Runs the workload effect to completion on the runtime under test.
    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
