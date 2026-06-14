module FIO.Benchmarks.Program

open FIO.Benchmarks.Benchmarks

open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Engines

open Perfolizer.Horology

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

    let job =
        Job.Default
            .WithWarmupCount(warmupCount)
            .WithIterationCount(iterationCount)
            .WithMinIterationTime(TimeInterval.FromMilliseconds 100.0)
            .WithStrategy(RunStrategy.Throughput)
            .WithGcServer(false)
            .WithGcConcurrent true

    ManualConfig.Create(DefaultConfig.Instance).AddJob job

[<EntryPoint>]
let main args =
    let hasCliJob =
        args |> Array.exists (fun arg -> arg = "--job")

    let config =
        if hasCliJob then ManualConfig.Create DefaultConfig.Instance
        else buildConfig ()

    BenchmarkSwitcher
        .FromTypes(
            [|
                typeof<BangBenchmark>
                typeof<BigBenchmark>
                typeof<BoundedBufferBenchmark>
                typeof<ChameneosBenchmark>
                typeof<CountingBenchmark>
                typeof<FibonacciBenchmark>
                typeof<ForkBenchmark>
                typeof<PhilosophersBenchmark>
                typeof<PingpongBenchmark>
                typeof<ThreadringBenchmark>
                typeof<TrapezoidalBenchmark>
            |]).Run(args, config)
            |> ignore
    0
