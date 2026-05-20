namespace FIO.Benchmarks.Benchmarks

open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type PingpongBenchmarks() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_PINGPONG_ROUNDS" [| 10_000; 50_000; 150_000 |]

    member _.Runtimes = RuntimeParam.runtimes ()

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
        runtime.Run(Pingpong.effect this.RoundCount).Task()
