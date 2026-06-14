namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type BoundedBufferBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.ProducerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_PRODUCERS" [| 4 |]

    member _.ConsumerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_CONSUMERS" [| 4 |]

    member _.Capacities =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_CAPACITY" [| 10 |]

    member _.ItemsPerProducerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_ITEMS" [| 100_000 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("ProducerCounts")>]
    member val ProducerCount = 0 with get, set

    [<ParamsSource("ConsumerCounts")>]
    member val ConsumerCount = 0 with get, set

    [<ParamsSource("Capacities")>]
    member val Capacity = 0 with get, set

    [<ParamsSource("ItemsPerProducerCounts")>]
    member val ItemsPerProducer = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- BoundedBuffer.effect this.ProducerCount this.ConsumerCount this.Capacity this.ItemsPerProducer

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
