namespace FSharp.FIO.Benchmarks.Suite

open FSharp.FIO.Runtime

open System
open System.IO
open System.Globalization

/// <summary>
/// Configuration for a specific benchmark, specifying actor and round counts.
/// </summary>
type internal BenchmarkConfig =
    /// <summary>Configuration for Pingpong benchmark.</summary>
    | PingpongConfig of roundCount: int
    /// <summary>Configuration for Threadring benchmark.</summary>
    | ThreadringConfig of actorCount: int * roundCount: int
    /// <summary>Configuration for Big benchmark.</summary>
    | BigConfig of actorCount: int * roundCount: int
    /// <summary>Configuration for Bang benchmark.</summary>
    | BangConfig of actorCount: int * roundCount: int
    /// <summary>Configuration for Fork benchmark.</summary>
    | ForkConfig of actorCount: int

/// <summary>
/// Helper functions for BenchmarkConfig formatting and conversion.
/// </summary>
module internal BenchmarkConfig =

    /// <summary>
    /// Gets the display name for the benchmark.
    /// </summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Display name string.</returns>
    let name config =
        match config with
        | PingpongConfig _ -> "Pingpong"
        | ThreadringConfig _ -> "Threadring"
        | BigConfig _ -> "Big"
        | BangConfig _ -> "Bang"
        | ForkConfig _ -> "Fork"

    /// <summary>
    /// Formats an integer with thousand separators.
    /// </summary>
    /// <param name="n">Integer to format.</param>
    /// <returns>Formatted string with thousand separators.</returns>
    let private formatCount (n: int) =
        n.ToString("N0", CultureInfo.InvariantCulture)

    /// <summary>
    /// Gets a human-readable configuration string showing actor/round counts.
    /// </summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Formatted configuration string.</returns>
    let configString config =
        match config with
        | PingpongConfig rc ->
            $"Actor Count: 2 Round Count: {formatCount rc}"
        | ThreadringConfig (ac, rc) | BigConfig (ac, rc) | BangConfig (ac, rc) ->
            $"Actor Count: {formatCount ac} Round Count: {formatCount rc}"
        | ForkConfig ac ->
            $"Actor Count: {formatCount ac} Round Count: 1"

    /// <summary>
    /// Gets a full display string including benchmark name and configuration.
    /// </summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Full display string.</returns>
    let toString config =
        $"{name config} ({configString config})"

    /// <summary>
    /// Gets a sanitized filename-safe string representation.
    /// </summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Filename-safe string.</returns>
    let toFileString config =
        toString config
        |> String.filter (fun c -> not ("():,. ".Contains c))
        |> _.ToLowerInvariant()
        
/// <summary>
/// Results from a benchmark run including execution times and memory usage statistics.
/// </summary>
type internal BenchmarkResult =
    { /// <summary>Benchmark configuration used for this run.</summary>
      Config: BenchmarkConfig
      /// <summary>Display name of the runtime.</summary>
      RuntimeName: string
      /// <summary>Filename-safe runtime name.</summary>
      RuntimeFileName: string
      /// <summary>List of run numbers.</summary>
      Runs: int64 list
      /// <summary>Execution times in milliseconds for each run.</summary>
      ExecutionTimes: int64 list
      /// <summary>Average execution time in milliseconds.</summary>
      AvgExecutionTime: float
      /// <summary>Standard deviation of execution times.</summary>
      StdExecutionTime: float
      /// <summary>Memory usage in MB for each run.</summary>
      MemoryUsages: int64 list
      /// <summary>Average memory usage in MB.</summary>
      AvgMemoryUsage: float
      /// <summary>Standard deviation of memory usage.</summary>
      StdMemoryUsage: float }

/// <summary>
/// Helper functions for printing and persisting benchmark results.
/// </summary>
module internal BenchmarkResult =

    /// <summary>
    /// Prints benchmark results to the console in a formatted table.
    /// </summary>
    /// <param name="result">Benchmark result to print.</param>
    let print result =
        let header =
            $"
┌────────────────────────────────────────────────────────────────────────────────────────────┐
│  Benchmark:  %-50s{BenchmarkConfig.toString result.Config}                            │
│  Runtime:    %-50s{result.RuntimeName}                            │
├────────────────────────────────────────────────────────────────────────────────────────────┤
│  Run                           Execution Time (ms)           Memory Usage (MB)             │
│  ────────────────────────────  ────────────────────────────  ────────────────────────────  │\n"

        let allData =
            List.map3 (fun a b c -> a, b, c)
                result.Runs result.ExecutionTimes result.MemoryUsages

        let dataRows =
            let rec dataRows curDataRows acc =
                match curDataRows with
                | [] ->
                    acc
                        + "│                                                                                            │\n"
                        + "│                                Avg. Execution Time (ms)      Avg. Memory Usage (MB)        │\n"
                        + "│                                ────────────────────────────  ────────────────────────────  │\n"
                       + $"│                                %-28f{result.AvgExecutionTime}  %-28f{result.AvgMemoryUsage}  │\n"
                        + "│                                                                                            │\n"
                        + "│                                Std. Execution Time (ms)      Std. Memory Usage (MB)        │\n"
                        + "│                                ────────────────────────────  ────────────────────────────  │\n"
                       + $"│                                %-28f{result.StdExecutionTime}  %-28f{result.StdMemoryUsage}  │\n"
                        + "└────────────────────────────────────────────────────────────────────────────────────────────┘"
                | (run, executionTime, memoryUsage) :: ts ->
                    let str = $"│  #%-27i{run}  %-28i{executionTime}  %-28i{memoryUsage}  │\n"
                    dataRows ts (acc + str)
            dataRows allData ""

        printfn $"%s{header + dataRows}"

    /// <summary>
    /// Writes benchmark results to a CSV file for analysis.
    /// </summary>
    /// <param name="result">Benchmark result to save.</param>
    /// <param name="savePath">Directory path for saving the CSV file.</param>
    let writeToCsv (result, savePath) =
        let rec csvContent (result: BenchmarkResult, allData, acc) =
            match allData with
            | [] -> acc
            | (run, executionTime, memoryUsage) :: ts ->
                let runStr = string run + ","
                let executionTimeStr = string executionTime + ","
                let memoryUsageStr = string memoryUsage + ","
                let avgExecutionTimeStr = string result.AvgExecutionTime + ","
                let stdExecutionTimeStr = string result.StdExecutionTime + ","
                let avgMemoryUsageTimeStr = string result.AvgMemoryUsage + ","
                let stdMemoryUsageStr = string result.StdMemoryUsage
                let rowStr = $"%-28s{runStr} %-28s{executionTimeStr} %-28s{memoryUsageStr} %-28s{avgExecutionTimeStr} %-28s{stdExecutionTimeStr} %-28s{avgMemoryUsageTimeStr} %s{stdMemoryUsageStr}\n"
                csvContent(result, ts, acc + rowStr)

        let benchName = BenchmarkConfig.toFileString result.Config
        let folderName = $"%s{benchName}-runs-%s{result.Runs.Length.ToString()}"
        let dirPath = savePath + folderName + @"\" + result.RuntimeFileName.ToLower()

        if not (Directory.Exists dirPath) then
            Directory.CreateDirectory dirPath |> ignore

        let fileName = $"""{folderName}-{result.RuntimeFileName.ToLower()}-{DateTime.Now.ToString
            "dd_MM_yyyy-HH-mm-ss"}.csv"""
        let filePath = dirPath + @"\" + fileName

        let runHeader = "Run,"
        let executionTimeHeader = "Execution Time (ms),"
        let memoryUsageHeader = "Memory Usage (MB),"
        let avgExecutionTimeHeader = "Avg. Execution Time (ms),"
        let stdExecutionTimeHeader = "Std. Execution Time (ms),"
        let avgMemoryUsageHeader = "Avg. Memory Usage (MB),"
        let stdMemoryUsageHeader = "Std. Memory Usage (MB)"
        let csvHeader = $"%-28s{runHeader} %-28s{executionTimeHeader} %-28s{memoryUsageHeader} %-28s{avgExecutionTimeHeader} %-28s{stdExecutionTimeHeader} %-28s{avgMemoryUsageHeader} %s{stdMemoryUsageHeader}\n"

        let allData =
            List.map3 (fun a b c -> a, b, c)
                result.Runs result.ExecutionTimes result.MemoryUsages

        File.WriteAllText(filePath, csvHeader + csvContent(result, allData, ""))
        printfn $"\nSaved benchmark result to file: '%s{filePath}'"

/// <summary>
/// Arguments controlling benchmark execution including runtime, runs, and configurations.
/// </summary>
type internal BenchmarkArgs =
    { /// <summary>Runtime instance to use for benchmark execution.</summary>
      Runtime: FIORuntime
      /// <summary>Number of runs per benchmark configuration.</summary>
      Runs: int
      /// <summary>Actor increment value and number of times to apply.</summary>
      ActorIncrement: int * int
      /// <summary>Round increment value and number of times to apply.</summary>
      RoundIncrement: int * int
      /// <summary>List of benchmark configurations to run.</summary>
      BenchmarkConfigs: BenchmarkConfig list
      /// <summary>Whether to save results to CSV files.</summary>
      SaveToCsv: bool
      /// <summary>Directory path for saving CSV files.</summary>
      SavePath: string }
