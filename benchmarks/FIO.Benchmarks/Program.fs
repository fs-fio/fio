module FIO.Benchmarks.Program

open FIO.Benchmarks.Benchmarks

open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs

open System

let private defaultWarmupCount = 3
let private defaultIterationCount = 30

let private parseCount (name: string) (minValue: int) (defaultValue: int) =
    match Environment.GetEnvironmentVariable name with
    | null
    | "" -> defaultValue
    | value ->
        match Int32.TryParse value with
        | true, n when n >= minValue -> n
        | _ -> raise (ArgumentException $"Invalid {name}: '{value}' (expected an integer >= {minValue})")

let private buildConfig () =
    let warmupCount = parseCount "FIO_BENCH_WARMUP" 0 defaultWarmupCount
    let iterationCount = parseCount "FIO_BENCH_ITERATIONS" 1 defaultIterationCount

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
