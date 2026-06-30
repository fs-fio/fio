namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures many sending actors flooding a single receiver over one channel.
[<MemoryDiagnoser>]
[<RankColumn>]
type BangBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Sending-actor counts to sweep, overridable via FIO_BENCH_BANG_ACTORS.
    member _.ActorCounts =
        RuntimeParam.intParams "FIO_BENCH_BANG_ACTORS" [| 50; 200 |]

    // Messages-per-actor counts to sweep, overridable via FIO_BENCH_BANG_ROUNDS.
    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_BANG_ROUNDS" [| 1_000; 5_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The sending-actor count for the current run.
    [<ParamsSource("ActorCounts")>]
    member val ActorCount = 0 with get, set

    // The messages-per-actor count for the current run.
    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Bang.effect this.ActorCount this.RoundCount

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
