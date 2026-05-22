namespace FIO.Benchmarks.Benchmarks

open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type BigBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>

    member _.ActorCounts = RuntimeParam.intParams "FIO_BENCH_BIG_ACTORS" [| 10; 25 |]
    member _.RoundCounts = RuntimeParam.intParams "FIO_BENCH_BIG_ROUNDS" [| 100; 500 |]
    member _.Runtimes = RuntimeParam.runtimes ()

    [<ParamsSource("ActorCounts")>]
    member val ActorCount = 0 with get, set

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        runtime <- RuntimeParam.create this.Runtime

    [<GlobalCleanup>]
    member _.Cleanup() =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member this.Run() =
        runtime.Run(Big.effect (this.ActorCount, this.RoundCount)).Task()
