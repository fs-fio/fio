namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

// Measures the Chameneos-Redux symmetric rendezvous of color-changing creatures.
[<MemoryDiagnoser>]
[<RankColumn>]
type ChameneosBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    // Creature counts to sweep, overridable via FIO_BENCH_CHAMENEOS_CREATURES.
    member _.CreatureCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_CREATURES" [| 100 |]

    // Meeting counts to sweep, overridable via FIO_BENCH_CHAMENEOS_MEETINGS.
    member _.MeetingCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_MEETINGS" [| 100_000 |]

    // Runtime specs to sweep, overridable via FIO_BENCH_RUNTIMES.
    member _.Runtimes =
        RuntimeParam.runtimes ()

    // The creature count for the current run.
    [<ParamsSource("CreatureCounts")>]
    member val CreatureCount = 0 with get, set

    // The meeting count for the current run.
    [<ParamsSource("MeetingCounts")>]
    member val MeetingCount = 0 with get, set

    // The runtime spec for the current run.
    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    // Builds the runtime and the workload effect for the current parameters.
    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Chameneos.effect this.CreatureCount this.MeetingCount

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
