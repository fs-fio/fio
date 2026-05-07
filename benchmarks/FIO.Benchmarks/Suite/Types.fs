namespace FIO.Benchmarks.Suite

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Globalization

/// <summary>Represents a benchmark configuration specifying actor and round counts.</summary>
type internal BenchmarkConfig =
    /// <summary>Represents the Pingpong benchmark configuration.</summary>
    | PingpongConfig of roundCount: int
    /// <summary>Represents the Threadring benchmark configuration.</summary>
    | ThreadringConfig of actorCount: int * roundCount: int
    /// <summary>Represents the Big benchmark configuration.</summary>
    | BigConfig of actorCount: int * roundCount: int
    /// <summary>Represents the Bang benchmark configuration.</summary>
    | BangConfig of actorCount: int * roundCount: int
    /// <summary>Represents the Fork benchmark configuration.</summary>
    | ForkConfig of actorCount: int

/// <summary>Represents a runtime selection supported by the benchmark CLI.</summary>
type internal RuntimeSelection =
    /// <summary>Represents the Direct runtime selection.</summary>
    | Direct
    /// <summary>Represents the Cooperative runtime selection with the given worker configuration.</summary>
    | Cooperative of WorkerConfig
    /// <summary>Represents the Concurrent runtime selection with the given worker configuration.</summary>
    | Concurrent of WorkerConfig

/// <summary>Provides helper functions for runtime selection and runtime instantiation.</summary>
module internal RuntimeSelection =

    /// <summary>Creates an FIORuntime instance from the given runtime selection.</summary>
    /// <param name="selection">The runtime selection to instantiate.</param>
    /// <returns>The created FIORuntime.</returns>
    let createRuntime selection : FIORuntime =
        match selection with
        | Direct -> new DirectRuntime() :> FIORuntime
        | Cooperative config -> new CooperativeRuntime(config) :> FIORuntime
        | Concurrent config -> new ConcurrentRuntime(config) :> FIORuntime

/// <summary>Provides helper functions for BenchmarkConfig formatting and conversion.</summary>
module internal BenchmarkConfig =

    /// <summary>Returns the display name for the benchmark.</summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Display name string.</returns>
    let name config =
        match config with
        | PingpongConfig _ -> "Pingpong"
        | ThreadringConfig _ -> "Threadring"
        | BigConfig _ -> "Big"
        | BangConfig _ -> "Bang"
        | ForkConfig _ -> "Fork"

    /// <summary>Returns a formatted integer with thousand separators.</summary>
    /// <param name="n">Integer to format.</param>
    /// <returns>Formatted string with thousand separators.</returns>
    let private formatCount (n: int) =
        n.ToString("N0", CultureInfo.InvariantCulture)

    /// <summary>Returns a human-readable configuration string showing actor and round counts.</summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Formatted configuration string.</returns>
    let configString config =
        match config with
        | PingpongConfig rc -> $"Actor Count: 2 Round Count: {formatCount rc}"
        | ThreadringConfig(ac, rc)
        | BigConfig(ac, rc)
        | BangConfig(ac, rc) -> $"Actor Count: {formatCount ac} Round Count: {formatCount rc}"
        | ForkConfig ac -> $"Actor Count: {formatCount ac} Round Count: 1"

    /// <summary>Returns a full display string including benchmark name and configuration.</summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Full display string.</returns>
    let toString config =
        $"{name config} ({configString config})"

    /// <summary>Returns a sanitized filename-safe string representation.</summary>
    /// <param name="config">Benchmark configuration.</param>
    /// <returns>Filename-safe string.</returns>
    let toFileString config =
        toString config
        |> String.filter (fun c -> not ("():,. ".Contains c))
        |> (fun s -> s.ToLowerInvariant())

/// <summary>Represents benchmark run results including execution times and statistics.</summary>
type internal BenchmarkResult =
    {
        /// <summary>Represents the benchmark configuration used for this run.</summary>
        Config: BenchmarkConfig
        /// <summary>Represents the display name of the runtime.</summary>
        RuntimeName: string
        /// <summary>Represents the filename-safe runtime name.</summary>
        RuntimeFileName: string
        /// <summary>Represents the list of run numbers.</summary>
        Runs: int64 list
        /// <summary>Represents the execution times in milliseconds for each run.</summary>
        ExecutionTimes: int64 list
        /// <summary>Represents the average execution time in milliseconds.</summary>
        AvgExecutionTime: float
        /// <summary>Represents the median execution time in milliseconds.</summary>
        MedianExecutionTime: float
        /// <summary>Represents the 95th percentile execution time in milliseconds.</summary>
        P95ExecutionTime: float
        /// <summary>Represents the standard deviation of execution times.</summary>
        StdExecutionTime: float
    }

/// <summary>Provides helper functions for printing and persisting benchmark results.</summary>
module internal BenchmarkResult =
    /// <summary>Returns the megabyte equivalent of a byte count.</summary>
    /// <param name="bytes">Byte count to convert.</param>
    /// <returns>The value in megabytes.</returns>
    let private bytesToMb (bytes: int64) = float bytes / 1024.0 / 1024.0

    /// <summary>Returns a formatted megabyte string with three decimal places.</summary>
    /// <param name="value">Megabyte value to format.</param>
    /// <returns>A formatted string with three decimal places.</returns>
    let private formatMb (value: float) =
        value.ToString("0.000", CultureInfo.InvariantCulture)

    /// <summary>Transforms benchmark results into a formatted console output table.</summary>
    /// <param name="includeRunRows">When true, includes per-run rows; otherwise summary-only output is shown.</param>
    /// <param name="result">Benchmark result to print.</param>
    let printWithMode includeRunRows result =
        let innerWidth = 92
        let horizontal = String.replicate innerWidth "─"

        let boxed (text: string) =
            let safeText = if isNull text then String.Empty else text

            let normalized =
                if safeText.Length > innerWidth then
                    safeText.Substring(0, innerWidth)
                else
                    safeText.PadRight innerWidth

            $"│%s{normalized}│"

        let allData =
            List.zip result.Runs result.ExecutionTimes
            |> List.map (fun (run, executionTime) -> run, executionTime)

        let avgExecutionStr =
            result.AvgExecutionTime.ToString("F6", CultureInfo.InvariantCulture)

        let medianExecutionStr =
            result.MedianExecutionTime.ToString("F6", CultureInfo.InvariantCulture)

        let p95ExecutionStr =
            result.P95ExecutionTime.ToString("F6", CultureInfo.InvariantCulture)

        let stdExecutionStr =
            result.StdExecutionTime.ToString("F6", CultureInfo.InvariantCulture)

        let summaryUnderline = String.replicate 28 "─"

        let lines = ResizeArray<string>()

        lines.Add $"┌%s{horizontal}┐"
        lines.Add(boxed (sprintf "  Benchmark:  %-50s" (BenchmarkConfig.toString result.Config)))
        lines.Add(boxed (sprintf "  Runtime:    %-50s" result.RuntimeName))
        lines.Add $"├%s{horizontal}┤"

        if includeRunRows then
            lines.Add(boxed "  Run                 Execution Time (ms)")
            lines.Add(boxed "  ──────────────────  ───────────────────  ───────────────────  ───────────────────")

            for run, executionTime in allData do
                lines.Add(boxed (sprintf "  #%-17i  %-19i" run executionTime))
        else
            lines.Add(boxed "  Summary mode (use --detailed to print all per-run rows).")

        lines.Add(boxed "")
        lines.Add(boxed (sprintf "                                %-28s" "Avg. Execution Time (ms)"))
        lines.Add(boxed (sprintf "                                %-28s  %-28s" summaryUnderline summaryUnderline))
        lines.Add(boxed (sprintf "                                %-28s " avgExecutionStr))

        lines.Add(boxed "")

        lines.Add(
            boxed (sprintf "                                %-28s  %-28s" "Median Exec Time (ms)" "P95 Exec Time (ms)")
        )

        lines.Add(boxed (sprintf "                                %-28s  %-28s" summaryUnderline summaryUnderline))
        lines.Add(boxed (sprintf "                                %-28s  %-28s" medianExecutionStr p95ExecutionStr))

        lines.Add(boxed "")
        lines.Add(boxed (sprintf "                                %-28s" "Std. Execution Time (ms)"))
        lines.Add(boxed (sprintf "                                %-28s  %-28s" summaryUnderline summaryUnderline))
        lines.Add(boxed (sprintf "                                %-28s" stdExecutionStr))

        lines.Add $"└%s{horizontal}┘"

        printfn "%s" (String.concat "\n" lines)

    /// <summary>Transforms benchmark results into a formatted console output with all per-run rows.</summary>
    /// <param name="result">Benchmark result to print.</param>
    let print result = printWithMode true result

    /// <summary>Transforms the console output by printing a compact comparison summary across runtimes for one benchmark config.</summary>
    /// <param name="config">Benchmark configuration compared.</param>
    /// <param name="results">Runtime-specific benchmark results.</param>
    let printComparisonSummary (config: BenchmarkConfig) (results: BenchmarkResult list) =
        let fitLeft (width: int) (value: string) =
            let safeValue = if isNull value then String.Empty else value

            if safeValue.Length > width then
                safeValue.Substring(0, width)
            else
                safeValue.PadRight width

        let fitRight (width: int) (value: string) =
            let safeValue = if isNull value then String.Empty else value

            if safeValue.Length > width then
                safeValue.Substring(0, width)
            else
                safeValue.PadLeft width

        let formatFloatCell (width: int) (decimals: int) (value: float) =
            let fixedPoint = value.ToString($"F{decimals}", CultureInfo.InvariantCulture)

            let rendered =
                if fixedPoint.Length <= width then
                    fixedPoint
                else
                    let scientific = value.ToString("0.###E+0", CultureInfo.InvariantCulture)

                    if scientific.Length <= width then
                        scientific
                    else
                        scientific.Substring(0, width)

            fitRight width rendered

        match results with
        | []
        | [ _ ] -> ()
        | _ ->
            let runtimeWidth = 34
            let avgWidth = 10
            let medianWidth = 10
            let p95Width = 10
            let avgProcDeltaWidth = 16
            let deltaWidth = 14

            let bestMedian = results |> List.map (fun r -> r.MedianExecutionTime) |> List.min
            printfn $"\nComparison Summary: %s{BenchmarkConfig.toString config}"

            let headerLine =
                String.concat
                    " "
                    [
                        fitLeft runtimeWidth "Runtime"
                        fitRight avgWidth "Avg (ms)"
                        fitRight medianWidth "Median"
                        fitRight p95Width "P95"
                        fitRight avgProcDeltaWidth "Avg. Proc Δ (MB)"
                        fitRight deltaWidth "Delta Median %"
                    ]

            printfn "  %s" headerLine
            printfn "  %s" (String.replicate headerLine.Length "-")

            for result in results do
                let deltaMedianPct =
                    if bestMedian <= 0.0 then
                        0.0
                    else
                        (result.MedianExecutionTime - bestMedian) / bestMedian * 100.0

                let rowLine =
                    String.concat
                        " "
                        [
                            fitLeft runtimeWidth result.RuntimeName
                            formatFloatCell avgWidth 2 result.AvgExecutionTime
                            formatFloatCell medianWidth 2 result.MedianExecutionTime
                            formatFloatCell p95Width 2 result.P95ExecutionTime
                            formatFloatCell deltaWidth 2 deltaMedianPct
                        ]

                printfn "  %s" rowLine

    /// <summary>Transforms benchmark results into a CSV file for analysis.</summary>
    /// <param name="result">Benchmark result to save.</param>
    /// <param name="savePath">Directory path for saving the CSV file.</param>
    let writeToCsv (result, savePath) =
        let csvContent (result: BenchmarkResult) (allData: (int64 * int64) list) =
            let avgExecutionTimeStr =
                result.AvgExecutionTime.ToString("R", CultureInfo.InvariantCulture)

            let medianExecutionTimeStr =
                result.MedianExecutionTime.ToString("R", CultureInfo.InvariantCulture)

            let p95ExecutionTimeStr =
                result.P95ExecutionTime.ToString("R", CultureInfo.InvariantCulture)

            let stdExecutionTimeStr =
                result.StdExecutionTime.ToString("R", CultureInfo.InvariantCulture)

            allData
            |> List.map (fun (run, executionTime) ->
                String.concat
                    ","
                    [
                        string run
                        string executionTime
                        avgExecutionTimeStr
                        medianExecutionTimeStr
                        p95ExecutionTimeStr
                        stdExecutionTimeStr
                    ])
            |> String.concat "\n"

        let benchName = BenchmarkConfig.toFileString result.Config
        let folderName = $"%s{benchName}-runs-%s{result.Runs.Length.ToString()}"

        let dirPath =
            Path.Combine(savePath, folderName, result.RuntimeFileName.ToLowerInvariant())

        if not (Directory.Exists dirPath) then
            Directory.CreateDirectory dirPath |> ignore

        let fileName =
            $"""{folderName}-{result.RuntimeFileName.ToLowerInvariant()}-{DateTime.Now.ToString "dd_MM_yyyy-HH-mm-ss"}.csv"""

        let filePath = Path.Combine(dirPath, fileName)

        let csvHeader =
            String.concat
                ","
                [
                    "Run"
                    "Execution Time (ms)"
                    "Managed Heap Delta (MB)"
                    "Avg. Execution Time (ms)"
                    "Median Execution Time (ms)"
                    "P95 Execution Time (ms)"
                    "Std. Execution Time (ms)"
                ]

        let allData =
            List.zip result.Runs result.ExecutionTimes
            |> List.map (fun (run, executionTime) -> run, executionTime)

        let csvRows = csvContent result allData

        let csvText =
            if String.IsNullOrWhiteSpace csvRows then
                csvHeader + "\n"
            else
                csvHeader + "\n" + csvRows + "\n"

        File.WriteAllText(filePath, csvText)
        printfn $"\nSaved benchmark result to file: '%s{filePath}'"

/// <summary>Represents the arguments controlling benchmark execution including runtime, runs, and configurations.</summary>
type internal BenchmarkArgs =
    {
        /// <summary>Represents the runtime selections to benchmark in canonical order.</summary>
        Runtimes: RuntimeSelection list
        /// <summary>Represents the number of runs per benchmark configuration.</summary>
        Runs: int
        /// <summary>Represents the number of warmup runs executed before measured runs.</summary>
        WarmupRuns: int
        /// <summary>Represents whether to suppress per-run logs.</summary>
        Quiet: bool
        /// <summary>Represents whether to print detailed per-run logs and per-run result rows.</summary>
        Detailed: bool
        /// <summary>Represents the actor increment value and number of times to apply.</summary>
        ActorIncrement: int * int
        /// <summary>Represents the round increment value and number of times to apply.</summary>
        RoundIncrement: int * int
        /// <summary>Represents the list of benchmark configurations to run.</summary>
        BenchmarkConfigs: BenchmarkConfig list
        /// <summary>Represents whether to save results to CSV files.</summary>
        SaveToCsv: bool
        /// <summary>Represents the directory path for saving CSV files.</summary>
        SavePath: string
    }
