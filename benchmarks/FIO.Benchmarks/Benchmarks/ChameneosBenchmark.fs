namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks
open FIO.Benchmarks.Effects

open BenchmarkDotNet.Attributes

open System

[<MemoryDiagnoser>]
[<RankColumn>]
type ChameneosBenchmark() =
    let mutable runtime: FIORuntime = Unchecked.defaultof<_>
    let mutable effect = Unchecked.defaultof<_>

    member _.CreatureCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_CREATURES" [| 100 |]

    member _.MeetingCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_MEETINGS" [| 100_000 |]

    member _.Runtimes =
        RuntimeParam.runtimes ()

    [<ParamsSource("CreatureCounts")>]
    member val CreatureCount = 0 with get, set

    [<ParamsSource("MeetingCounts")>]
    member val MeetingCount = 0 with get, set

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- Chameneos.effect this.CreatureCount this.MeetingCount

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
