/// <summary>
/// Orchestrates benchmark execution, collecting timing and memory statistics.
/// </summary>
module internal FSharp.FIO.Benchmarks.Suite.BenchmarkRunner

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open System.Diagnostics

/// <summary>
/// Executes a single benchmark configuration for the specified number of runs.
/// </summary>
/// <param name="runtime">Runtime instance to use.</param>
/// <param name="totalRuns">Number of runs to execute.</param>
/// <param name="config">Benchmark configuration.</param>
/// <returns>Benchmark result with timing and memory statistics.</returns>
let private runBenchmark (runtime: FIORuntime, totalRuns, config: BenchmarkConfig) =
    
    /// <summary>
    /// Calculates the arithmetic mean of a list of values.
    /// </summary>
    /// <param name="onlyTimes">List of values to average.</param>
    /// <returns>Arithmetic mean.</returns>
    let average onlyTimes =
        onlyTimes |> List.averageBy float
    
    /// <summary>
    /// Calculates the population standard deviation of a list of values.
    /// </summary>
    /// <param name="onlyTimes">List of values.</param>
    /// <returns>Population standard deviation.</returns>
    let standardDeviation (onlyTimes: int64 list) =
        if onlyTimes.IsEmpty then
            0.0
        else
            let mean = onlyTimes |> List.averageBy float
            let variance = 
                onlyTimes 
                |> List.map (fun x -> pown (float x - mean) 2)
                |> List.average
            sqrt variance
    
    /// <summary>
    /// Executes the benchmark effect repeatedly and collects metrics.
    /// </summary>
    /// <param name="eff">Benchmark effect to execute.</param>
    /// <returns>Tuple of runs, execution times, and memory usages.</returns>
    let rec executeBenchmark (eff: FIO<int64, exn>) =
        task {
            let runs = ResizeArray<int64>(int totalRuns)
            let executionTimes = ResizeArray<int64>(int totalRuns)
            let memoryUsages = ResizeArray<int64>(int totalRuns)

            for currentRun in 1L..totalRuns do
                printfn $"[FIO.Benchmarks]: Executing: %s{BenchmarkConfig.toString config}, Runtime: %s{runtime.ToString()}, Run (%i{currentRun}/%i{totalRuns})"
                let! benchRes = runtime.Run(eff).Task()
                use proc = Process.GetCurrentProcess()
                let memoryUsage = proc.WorkingSet64 / (1024L * 1024L)
                let executionTime =
                    match benchRes with
                    | Succeeded time -> time
                    | Failed err -> invalidOp $"BenchmarkRunner: Failed executing benchmark with error: %A{err}"
                    | Interrupted exn -> invalidOp $"BenchmarkRunner: Benchmark interrupted: %s{exn.Message}"

                printfn $"[FIO.Benchmarks]: Execution completed: Execution Time: %i{executionTime}ms, Memory Usage: %i{memoryUsage}MB, Run (%i{currentRun}/%i{totalRuns})"
                runs.Add currentRun
                executionTimes.Add executionTime
                memoryUsages.Add memoryUsage

            return Seq.toList runs, Seq.toList executionTimes, Seq.toList memoryUsages
        }
    
    task {
        printfn $"[FIO.Benchmarks]: Starting session: %s{BenchmarkConfig.toString config}, Runtime: %s{runtime.ToString()}, Runs: %i{totalRuns}"
        
        let eff =
            match config with
            | PingpongConfig rc ->
                Pingpong.benchmark(PingpongConfig rc)
            | ThreadringConfig(ac, rc) ->
                Threadring.benchmark(ThreadringConfig (ac, rc))
            | BigConfig(ac, rc) ->
                Big.benchmark(BigConfig (ac, rc))
            | BangConfig(ac, rc) ->
                Bang.benchmark(BangConfig (ac, rc))
            | ForkConfig ac ->
                Fork.benchmark(ForkConfig ac)
            
        let! runs, executionTimes, memoryUsages = executeBenchmark eff
        
        printfn $"[FIO.Benchmarks]: Completed session: %s{BenchmarkConfig.toString config}, Runtime: %s{runtime.ToString()}, Runs: %i{totalRuns}"
        
        return
            { Config = config
              RuntimeName = runtime.ToString()
              RuntimeFileName = runtime.ToFileString()
              Runs = runs
              ExecutionTimes = executionTimes
              AvgExecutionTime = average executionTimes
              StdExecutionTime = standardDeviation executionTimes
              MemoryUsages = memoryUsages
              AvgMemoryUsage = average memoryUsages
              StdMemoryUsage = standardDeviation memoryUsages }
    }

/// <summary>
/// Runs all configured benchmarks with optional actor/round increments.
/// </summary>
/// <param name="args">Benchmark arguments including runtime and configurations.</param>
let internal run args =
    task {
        let actorInc, actorIncTimes = args.ActorIncrement
        let roundInc, roundIncTimes = args.RoundIncrement

        if args.Runs < 1 then
            invalidArg (nameof args.Runs) $"BenchmarkRunner: Runs argument must at least be 1. Runs = %i{args.Runs}"

        let configs =
            args.BenchmarkConfigs
            |> List.collect (fun config ->
                [ for actorIncTime in 0..actorIncTimes do
                    for roundIncTime in 0..roundIncTimes do
                        match config with
                        | PingpongConfig rc ->
                            yield PingpongConfig (rc + roundInc * roundIncTime)
                        | ThreadringConfig(ac, rc) ->
                            yield ThreadringConfig (ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                        | BigConfig(ac, rc) ->
                            yield BigConfig (ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                        | BangConfig(ac, rc) ->
                            yield BangConfig (ac + actorInc * actorIncTime, rc + roundInc * roundIncTime)
                        | ForkConfig ac ->
                            yield ForkConfig (ac + actorInc * actorIncTime) ])

        for config in configs do
            let! result = runBenchmark(args.Runtime, args.Runs, config)
            BenchmarkResult.print result
            if args.SaveToCsv then
                BenchmarkResult.writeToCsv(result, args.SavePath)
    }
