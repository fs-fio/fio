namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures parallel trapezoidal integration approximating pi across worker fibers.
[<MemoryDiagnoser>]
[<RankColumn>]
type TrapezoidalBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Worker-fiber counts to sweep, overridable via FIO_BENCH_TRAPEZOIDAL_WORKERS.
    member _.WorkerCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_WORKERS" [| 8; 16 |]

    // Integration-point counts to sweep, overridable via FIO_BENCH_TRAPEZOIDAL_POINTS.
    member _.PointCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_POINTS" [| 1_000_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The worker-fiber count for the current run.
    [<ParamsSource("WorkerCounts")>]
    member val WorkerCount = 0 with get, set

    // The integration-point count for the current run.
    [<ParamsSource("PointCounts")>]
    member val PointCount = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Trapezoidal.effect this.WorkerCount this.PointCount

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
