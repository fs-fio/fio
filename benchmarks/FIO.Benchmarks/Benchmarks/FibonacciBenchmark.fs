namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type FibonacciBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.Ns =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_N" [| 25; 30 |]

    member _.Thresholds =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_THRESHOLD" [| 12 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("Ns")>]
    member val N = 0 with get, set

    [<ParamsSource("Thresholds")>]
    member val Threshold = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Fibonacci.effect this.N this.Threshold

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
