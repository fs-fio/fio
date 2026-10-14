namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module internal Fork =

    // Builds the Fork workload: forks the given number of no-op fibers in parallel.
    let effect actorCount : FIO<unit, exn> =
        FIO.forEachParDiscard (seq { 1..actorCount }) (fun _ -> FIO.unit ())

// Measures raw fiber fork/join throughput with no work per fiber.
[<MemoryDiagnoser>]
[<RankColumn>]
type ForkBenchmark() =
    inherit FIOBenchmark()

    member _.ActorCounts =
        RuntimeParam.intParams "FIO_BENCH_FORK_ACTORS" [| 1_000; 10_000; 50_000 |]

    [<ParamsSource("ActorCounts")>]
    member val ActorCount = 0 with get, set

    override this.CreateEffect () =
        Fork.effect this.ActorCount
