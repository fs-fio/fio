namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type PingpongBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_PINGPONG_ROUNDS" [| 10_000; 50_000; 150_000 |]

    member _.Runtimes = RuntimeParam.runtimes ()

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Pingpong.effect this.RoundCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
