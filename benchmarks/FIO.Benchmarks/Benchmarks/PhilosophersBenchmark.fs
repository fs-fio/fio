namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures the dining-philosophers problem coordinated through an arbitrator actor.
[<MemoryDiagnoser>]
[<RankColumn>]
type PhilosophersBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Philosopher counts to sweep, overridable via FIO_BENCH_PHILOSOPHERS_COUNT.
    member _.PhilosopherCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_COUNT" [| 20 |]

    // Rounds-per-philosopher counts to sweep, overridable via FIO_BENCH_PHILOSOPHERS_ROUNDS.
    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_ROUNDS" [| 10_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The philosopher count for the current run.
    [<ParamsSource("PhilosopherCounts")>]
    member val PhilosopherCount = 0 with get, set

    // The rounds-per-philosopher count for the current run.
    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Philosophers.effect this.PhilosopherCount this.RoundCount

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
