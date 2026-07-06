module FIO.Benchmarks.Program

open FIO.Benchmarks.Benchmarks

open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Engines
open BenchmarkDotNet.Reports

open Perfolizer.Horology

open System

let private defaultWarmupCount = 3
let private defaultIterationCount = 30

let private summaryStyle =
    SummaryStyle.Default.WithMaxParameterColumnWidth 40

// Parses an integer env var with a minimum, falling back to a default.
let private parseCount (name: string) (minValue: int) (defaultValue: int) =
    match Environment.GetEnvironmentVariable name with
    | null
    | "" -> defaultValue
    | value ->
        match Int32.TryParse value with
        | true, n when n >= minValue -> n
        | _ -> raise (ArgumentException $"Invalid {name}: '{value}' (expected an integer >= {minValue})")

// Builds the BenchmarkDotNet config, honoring warmup and iteration env overrides.
let private buildConfig () =
    let warmupCount = parseCount "FIO_BENCH_WARMUP" 0 defaultWarmupCount
    let iterationCount = parseCount "FIO_BENCH_ITERATIONS" 1 defaultIterationCount

    let job =
        Job.Default
            .WithWarmupCount(warmupCount)
            .WithIterationCount(iterationCount)
            .WithMinIterationTime(TimeInterval.FromMilliseconds 100.0)
            .WithStrategy(RunStrategy.Throughput)
            .WithGcServer(true)
            .WithGcConcurrent true

    ManualConfig.Create(DefaultConfig.Instance).AddJob(job).WithSummaryStyle summaryStyle

// Runs the benchmark switcher, using a CLI --job config when one is supplied.
[<EntryPoint>]
let main args =
    let hasCliJob =
        args |> Array.exists (fun arg -> arg = "--job")

    let config =
        if hasCliJob then (ManualConfig.Create DefaultConfig.Instance).WithSummaryStyle summaryStyle
        else buildConfig ()

    let summaries =
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
                    typeof<ZipRaceBenchmark>
                |]).Run(args, config)

    if summaries
       |> Seq.exists (fun summary ->
           summary.HasCriticalValidationErrors
           || summary.Reports |> Seq.exists (fun report -> not report.Success))
    then 1
    else 0
