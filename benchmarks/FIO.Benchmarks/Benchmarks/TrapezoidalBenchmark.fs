namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type TrapezoidalBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.WorkerCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_WORKERS" [| 8; 16 |]

    member _.PointCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_POINTS" [| 1_000_000 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("WorkerCounts")>]
    member val WorkerCount = 0 with get, set

    [<ParamsSource("PointCounts")>]
    member val PointCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Trapezoidal.effect this.WorkerCount this.PointCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
