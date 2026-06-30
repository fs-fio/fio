namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures recursive parallel Fibonacci, forking a fiber per branch above a threshold.
[<MemoryDiagnoser>]
[<RankColumn>]
type FibonacciBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Fibonacci indices to sweep, overridable via FIO_BENCH_FIBONACCI_N.
    member _.Ns =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_N" [| 25; 30 |]

    // Sequential-cutoff thresholds to sweep, overridable via FIO_BENCH_FIBONACCI_THRESHOLD.
    member _.Thresholds =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_THRESHOLD" [| 12 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The Fibonacci index for the current run.
    [<ParamsSource("Ns")>]
    member val N = 0 with get, set

    // The sequential-cutoff threshold for the current run.
    [<ParamsSource("Thresholds")>]
    member val Threshold = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Fibonacci.effect this.N this.Threshold

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
