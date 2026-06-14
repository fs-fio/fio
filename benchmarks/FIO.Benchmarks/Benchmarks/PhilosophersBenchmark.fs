namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type PhilosophersBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.PhilosopherCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_COUNT" [| 20 |]

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_ROUNDS" [| 10_000 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("PhilosopherCounts")>]
    member val PhilosopherCount = 0 with get, set

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Philosophers.effect this.PhilosopherCount this.RoundCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
