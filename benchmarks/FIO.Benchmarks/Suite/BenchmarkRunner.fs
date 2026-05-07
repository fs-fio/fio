/// <summary>Provides benchmark execution orchestration with timing and statistics collection.</summary>
module internal FIO.Benchmarks.Suite.BenchmarkRunner

open FIO.DSL
open FIO.Runtime

open System
open System.Diagnostics

/// <summary>Represents the verbosity level of benchmark output during execution.</summary>
type private OutputMode =
    /// <summary>Represents ultra-minimal output, suppressing progress updates.</summary>
    | Quiet
    /// <summary>Represents milestone-based progress output with summary statistics.</summary>
    | Concise
    /// <summary>Represents per-run output with full timing details.</summary>
    | Detailed

/// <summary>Returns progress milestone checkpoints as a map from run number to completion percentage.</summary>
/// <param name="totalRuns">Total number of benchmark runs.</param>
/// <returns>Map from run number to percentage milestone.</returns>
let private progressMilestones (totalRuns: int) =
    [ 10; 25; 50; 75; 100 ]
    |> List.map (fun pct ->
        let run = Math.Ceiling(float pct / 100.0 * float totalRuns) |> int |> max 1
        run, pct)
    |> List.fold (fun milestones (run, pct) -> Map.add run pct milestones) Map.empty

/// <summary>Creates a benchmark execution task for the specified number of runs.</summary>
/// <param name="runtime">Runtime instance to use.</param>
/// <param name="totalRuns">Number of runs to execute.</param>
/// <param name="warmupRuns">Number of warmup runs before measured runs.</param>
/// <param name="outputMode">Verbosity mode for benchmark output.</param>
/// <param name="config">Benchmark configuration.</param>
/// <returns>Benchmark result with timing statistics.</returns>
let private runBenchmark
    (runtime: FIORuntime, totalRuns: int, warmupRuns: int, outputMode: OutputMode, config: BenchmarkConfig)
    =
    let runtimeName = runtime.ToString()
    let configText = BenchmarkConfig.toString config
    let benchmarkName = BenchmarkConfig.name config

    let isDetailed =
        match outputMode with
        | Detailed -> true
        | _ -> false

    let isConcise =
        match outputMode with
        | Concise -> true
        | _ -> false

    let isQuiet =
        match outputMode with
        | Quiet -> true
        | _ -> false

    /// <summary>Returns the arithmetic mean of a list of values.</summary>
    /// <param name="onlyTimes">List of values to average.</param>
    /// <returns>Arithmetic mean.</returns>
    let average onlyTimes = onlyTimes |> List.averageBy float

    /// <summary>Returns the population standard deviation of a list of values.</summary>
    /// <param name="onlyTimes">List of values.</param>
    /// <returns>Population standard deviation.</returns>
    let standardDeviation (onlyTimes: int64 list) =
        if onlyTimes.IsEmpty then
            0.0
        else
            let mean = onlyTimes |> List.averageBy float

            let variance =
                onlyTimes |> List.map (fun x -> pown (float x - mean) 2) |> List.average

            sqrt variance

    /// <summary>Returns the median of a list of values.</summary>
    /// <param name="values">List of values to compute the median of.</param>
    /// <returns>Median value.</returns>
    let median (values: int64 list) =
        if values.IsEmpty then
            0.0
        else
            let sorted = values |> List.sort
            let mid = sorted.Length / 2

            if sorted.Length % 2 = 0 then
                (float sorted.[mid - 1] + float sorted.[mid]) / 2.0
            else
                float sorted.[mid]

    /// <summary>Returns the percentile value using nearest-rank method.</summary>
    /// <param name="p">Percentile to compute (0-100).</param>
    /// <param name="values">List of values to compute the percentile of.</param>
    /// <returns>Percentile value.</returns>
    let percentile (p: float) (values: int64 list) =
        if values.IsEmpty then
            0.0
        else
            let sorted = values |> List.sort
            let rank = int (ceil (p / 100.0 * float sorted.Length)) - 1
            let index = rank |> max 0 |> min (sorted.Length - 1)
            float sorted.[index]

    /// <summary>Creates a benchmark execution task that repeats the effect and collects timing metrics.</summary>
    /// <param name="eff">Benchmark effect to execute.</param>
    /// <param name="proc">Process handle for refreshing system metrics between runs.</param>
    /// <returns>Tuple of runs and execution times.</returns>
    let rec executeBenchmark (eff: FIO<int64, exn>, proc: Process) =
        task {
            let runs = ResizeArray<int64> totalRuns
            let executionTimes = ResizeArray<int64> totalRuns
            let checkpoints = progressMilestones totalRuns
            let mutable executionTimeSum = 0L

            for currentRun in 1..totalRuns do
                if isDetailed then
                    printfn
                        $"[bench/run] %s{benchmarkName} | runtime=%s{runtimeName} | run=%i{currentRun}/%i{totalRuns}"

                let! benchRes = runtime.Run(eff).Task()
                proc.Refresh()

                let executionTime =
                    match benchRes with
                    | Succeeded time -> time
                    | Failed err -> invalidOp $"BenchmarkRunner: Failed executing benchmark with error: %A{err}"
                    | Interrupted ex -> invalidOp $"BenchmarkRunner: Benchmark interrupted: %s{ex.Message}"

                executionTimeSum <- executionTimeSum + executionTime

                if isDetailed then
                    printfn
                        $"[bench/run-done] %s{benchmarkName} | runtime=%s{runtimeName} | run=%i{currentRun}/%i{totalRuns} | time=%i{executionTime}ms"
                elif isConcise then
                    match checkpoints |> Map.tryFind currentRun with
                    | Some pct ->
                        let avgExecutionTime = float executionTimeSum / float currentRun

                        printfn
                            $"[bench/progress] %s{benchmarkName} | runtime=%s{runtimeName} | %i{pct}%% (%i{currentRun}/%i{totalRuns}) | last=%i{executionTime}ms | avg=%.2f{avgExecutionTime}ms"
                    | None -> ()

                runs.Add(int64 currentRun)
                executionTimes.Add executionTime

            return Seq.toList runs, Seq.toList executionTimes
        }

    task {
        printfn $"[bench/start] %s{configText} | runtime=%s{runtimeName} | runs=%i{totalRuns} | warmup=%i{warmupRuns}"

        let eff =
            match config with
            | PingpongConfig rc -> Pingpong.benchmark (PingpongConfig rc)
            | ThreadringConfig(ac, rc) -> Threadring.benchmark (ThreadringConfig(ac, rc))
            | BigConfig(ac, rc) -> Big.benchmark (BigConfig(ac, rc))
            | BangConfig(ac, rc) -> Bang.benchmark (BangConfig(ac, rc))
            | ForkConfig ac -> Fork.benchmark (ForkConfig ac)

        use proc = Process.GetCurrentProcess()

        if warmupRuns > 0 then
            if not isQuiet then
                printfn $"[bench/warmup] %s{configText} | runtime=%s{runtimeName} | runs=%i{warmupRuns}"

            for warmupRun in 1..warmupRuns do
                if isDetailed then
                    printfn
                        $"[bench/warmup-run] %s{benchmarkName} | runtime=%s{runtimeName} | run=%i{warmupRun}/%i{warmupRuns}"

                let! warmupRes = runtime.Run(eff).Task()

                match warmupRes with
                | Succeeded _ -> ()
                | Failed err -> invalidOp $"BenchmarkRunner: Warmup failed with error: %A{err}"
                | Interrupted ex -> invalidOp $"BenchmarkRunner: Warmup interrupted: %s{ex.Message}"

            if not isQuiet then
                printfn $"[bench/warmup-done] %s{configText} | runtime=%s{runtimeName}"

        let! runs, executionTimes = executeBenchmark (eff, proc)

        let avgExecution = average executionTimes
        let medianExecution = median executionTimes
        let p95Execution = percentile 95.0 executionTimes
        let stdExecution = standardDeviation executionTimes

        printfn
            $"[bench/done] %s{configText} | runtime=%s{runtimeName} | median=%.2f{medianExecution}ms | p95=%.2f{p95Execution}ms"

        return
            {
                Config = config
                RuntimeName = runtimeName
                RuntimeFileName = runtime.ToFileString()
                Runs = runs
                ExecutionTimes = executionTimes
                AvgExecutionTime = avgExecution
                MedianExecutionTime = medianExecution
                P95ExecutionTime = p95Execution
                StdExecutionTime = stdExecution
            }
    }

/// <summary>Creates a benchmark execution task for a specific runtime selection.</summary>
/// <param name="runtimeSelection">Runtime to create and use.</param>
/// <param name="totalRuns">Number of measured runs to execute.</param>
/// <param name="warmupRuns">Number of warmup runs before measured runs.</param>
/// <param name="outputMode">Verbosity mode for benchmark output.</param>
/// <param name="config">Benchmark configuration to run.</param>
/// <returns>Benchmark result with timing statistics.</returns>
let private runBenchmarkForSelection
    (
        runtimeSelection: RuntimeSelection,
        totalRuns: int,
        warmupRuns: int,
        outputMode: OutputMode,
        config: BenchmarkConfig
    ) =
    task {
        let runtime = RuntimeSelection.createRuntime runtimeSelection

        try
            let! result = runBenchmark (runtime, totalRuns, warmupRuns, outputMode, config)
            return result
        finally
            match box runtime with
            | :? IDisposable as disposable -> disposable.Dispose()
            | _ -> ()
    }

/// <summary>Creates a benchmark execution task for all configured benchmarks with optional increments.</summary>
/// <param name="args">Benchmark arguments including runtime and configurations.</param>
/// <returns>A task that completes when all benchmarks have finished.</returns>
let internal run args =
    task {
        let actorInc, actorIncTimes = args.ActorIncrement
        let roundInc, roundIncTimes = args.RoundIncrement

        if args.Runs < 1 then
            invalidArg (nameof args.Runs) $"BenchmarkRunner: Runs argument must at least be 1. Runs = %i{args.Runs}"

        if args.WarmupRuns < 0 then
            invalidArg
                (nameof args.WarmupRuns)
                $"BenchmarkRunner: WarmupRuns must be >= 0. WarmupRuns = %i{args.WarmupRuns}"

        let outputMode =
            if args.Quiet then Quiet
            elif args.Detailed then Detailed
            else Concise

        let includeRunRowsInResultTable =
            match outputMode with
            | Detailed -> true
            | _ -> false

        let configs =
            args.BenchmarkConfigs
            |> List.collect (fun config ->
                [
                    for actorIncTime in 0..actorIncTimes do
                        for roundIncTime in 0..roundIncTimes do
                            match config with
                            | PingpongConfig rc -> yield PingpongConfig(rc + roundInc * roundIncTime)
                            | ThreadringConfig(ac, rc) ->
                                yield ThreadringConfig(ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                            | BigConfig(ac, rc) ->
                                yield BigConfig(ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                            | BangConfig(ac, rc) ->
                                yield BangConfig(ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                            | ForkConfig ac -> yield ForkConfig(ac + actorInc * actorIncTime)
                ])

        let completedComparisons =
            ResizeArray<BenchmarkConfig * BenchmarkResult list> configs.Length

        let printDeferredComparisons () =
            let hasComparisonToPrint =
                completedComparisons |> Seq.exists (fun (_, results) -> results.Length > 1)

            if hasComparisonToPrint then
                printfn "\nComparison summaries:"

                for config, runtimeResults in completedComparisons do
                    BenchmarkResult.printComparisonSummary config runtimeResults

        try
            for config in configs do
                let runtimeResults = ResizeArray<BenchmarkResult> args.Runtimes.Length

                for runtimeSelection in args.Runtimes do
                    let! result =
                        runBenchmarkForSelection (runtimeSelection, args.Runs, args.WarmupRuns, outputMode, config)

                    BenchmarkResult.printWithMode includeRunRowsInResultTable result

                    if args.SaveToCsv then
                        BenchmarkResult.writeToCsv (result, args.SavePath)

                    runtimeResults.Add result

                completedComparisons.Add(config, List.ofSeq runtimeResults)
        finally
            printDeferredComparisons ()
    }
