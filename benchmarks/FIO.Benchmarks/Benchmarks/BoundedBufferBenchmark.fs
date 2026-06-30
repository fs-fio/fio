namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures producer/consumer throughput through a bounded buffer.
[<MemoryDiagnoser>]
[<RankColumn>]
type BoundedBufferBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Producer counts to sweep, overridable via FIO_BENCH_BOUNDEDBUFFER_PRODUCERS.
    member _.ProducerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_PRODUCERS" [| 4 |]

    // Consumer counts to sweep, overridable via FIO_BENCH_BOUNDEDBUFFER_CONSUMERS.
    member _.ConsumerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_CONSUMERS" [| 4 |]

    // Buffer capacities to sweep, overridable via FIO_BENCH_BOUNDEDBUFFER_CAPACITY.
    member _.Capacities =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_CAPACITY" [| 10 |]

    // Items-per-producer counts to sweep, overridable via FIO_BENCH_BOUNDEDBUFFER_ITEMS.
    member _.ItemsPerProducerCounts =
        RuntimeParam.intParams "FIO_BENCH_BOUNDEDBUFFER_ITEMS" [| 100_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The producer count for the current run.
    [<ParamsSource("ProducerCounts")>]
    member val ProducerCount = 0 with get, set

    // The consumer count for the current run.
    [<ParamsSource("ConsumerCounts")>]
    member val ConsumerCount = 0 with get, set

    // The buffer capacity for the current run.
    [<ParamsSource("Capacities")>]
    member val Capacity = 0 with get, set

    // The items-per-producer count for the current run.
    [<ParamsSource("ItemsPerProducerCounts")>]
    member val ItemsPerProducer = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- BoundedBuffer.effect this.ProducerCount this.ConsumerCount this.Capacity this.ItemsPerProducer

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
