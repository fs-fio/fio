module FIO.Benchmarks.Program

open FIO.Benchmarks.Benchmarks

open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs

open System

let private defaultWarmupCount = 3
let private defaultIterationCount = 30

let private buildConfig () =
    let warmupCount =
        match Environment.GetEnvironmentVariable "FIO_BENCH_WARMUP" with
        | null
        | "" -> defaultWarmupCount
        | value -> int value

    let iterationCount =
        match Environment.GetEnvironmentVariable "FIO_BENCH_ITERATIONS" with
        | null
        | "" -> defaultIterationCount
        | value -> int value

    let job = Job.Default.WithWarmupCount(warmupCount).WithIterationCount iterationCount

    ManualConfig.Create(DefaultConfig.Instance).AddJob job

[<EntryPoint>]
let main args =
    let config = buildConfig ()

    BenchmarkSwitcher
        .FromTypes(
            [|
                typeof<PingpongBenchmark>
                typeof<ThreadringBenchmark>
                typeof<BigBenchmark>
                typeof<BangBenchmark>
                typeof<ForkBenchmark>
            |]
        )
        .Run(args, config)
    |> ignore

    0
