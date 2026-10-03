namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures all-to-all ping/pong messaging across a fully-connected mesh of actors.
[<MemoryDiagnoser>]
[<RankColumn>]
type BigBenchmark() =
    let mutable runtime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.ActorCounts =
        RuntimeParam.intParams "FIO_BENCH_BIG_ACTORS" [| 10; 25 |]

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_BIG_ROUNDS" [| 100; 500 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("ActorCounts")>]
    member val ActorCount = 0 with get, set

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Big.effect this.ActorCount this.RoundCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
