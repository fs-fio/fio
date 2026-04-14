namespace FIO.Benchmarks.Plots

open FSharp.Data

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions

/// <summary>
/// Runtime worker configuration metadata (EWC, EWS, BWC parameters).
/// </summary>
type WorkerMetadata =
    {
        /// <summary>Effect worker count.</summary>
        EWC: int
        /// <summary>Effect worker sleep duration.</summary>
        EWS: int
        /// <summary>Blocked worker count.</summary>
        BWC: int
    }

    override this.ToString() =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{this.EWC.ToString("N0", ci)} EWS: %s{this.EWS.ToString("N0", ci)} BWC: %s{this.BWC.ToString("N0", ci)}"""

/// <summary>
/// Metadata extracted from benchmark CSV filenames.
/// </summary>
type FileMetadata =
    {
        /// <summary>Name of the benchmark.</summary>
        BenchmarkName: string
        /// <summary>Number of actors in the benchmark.</summary>
        ActorCount: int
        /// <summary>Number of rounds in the benchmark.</summary>
        RoundCount: int
        /// <summary>Number of benchmark runs.</summary>
        Runs: int
        /// <summary>Name of the runtime used.</summary>
        RuntimeName: string
        /// <summary>Optional worker configuration metadata.</summary>
        WorkerMetadata: WorkerMetadata option
    }

    override this.ToString() =
        match this.WorkerMetadata with
        | Some metadata -> $"%s{this.RuntimeName} (%s{metadata.ToString()})"
        | None -> this.RuntimeName

/// <summary>
/// Helper functions for FileMetadata parsing and formatting.
/// </summary>
module internal FileMetadata =

    /// <summary>
    /// Generates a chart title from file metadata.
    /// </summary>
    /// <param name="fm">File metadata to generate title from.</param>
    /// <returns>Formatted chart title string.</returns>
    let title (fm: FileMetadata) =
        let ci = CultureInfo "en-US"
        $"""%s{fm.BenchmarkName} (Actor Count: %s{fm.ActorCount.ToString("N0", ci)} Round Count: %s{fm.RoundCount.ToString("N0", ci)})"""

    /// <summary>
    /// Capitalizes the first character of a string.
    /// </summary>
    /// <param name="s">String to capitalize.</param>
    /// <returns>String with the first character uppercased.</returns>
    let private capitalizeFirst s =
        match s with
        | "" -> s
        | _ -> s[0] |> Char.ToUpper |> string |> (fun first -> first + s[1..])

    /// <summary>
    /// Parses file metadata from a CSV file path.
    /// </summary>
    /// <param name="path">File path to parse metadata from.</param>
    /// <returns>Parsed file metadata.</returns>
    let parse (path: string) =
        let fullPath = Path.GetFullPath path
        let runtimeDirectory = DirectoryInfo(Path.GetDirectoryName fullPath)
        let benchmarkDirectory = runtimeDirectory.Parent

        if isNull benchmarkDirectory then
            invalidOp $"Unable to parse benchmark metadata from path '%s{path}'."

        let benchmarkDirectoryName = benchmarkDirectory.Name.ToLowerInvariant()

        let benchmarkMatch =
            Regex.Match(
                benchmarkDirectoryName,
                @"^(?<bench>[a-z]+)actorcount(?<actors>\d+)roundcount(?<rounds>\d+)-runs-(?<runs>\d+)$"
            )

        if not benchmarkMatch.Success then
            invalidOp $"Unsupported benchmark folder format in path '%s{path}'."

        let runtimeToken = runtimeDirectory.Name.ToLowerInvariant()

        let runtimeName, workerMetadata =
            match runtimeToken with
            | "directruntime" -> "DirectRuntime", None
            | _ when runtimeToken.StartsWith "cooperativeruntime" ->
                let workerMatch =
                    Regex.Match(runtimeToken, @"^cooperativeruntime-ewc-(\d+)-ews-(\d+)-bwc-(\d+)$")

                if not workerMatch.Success then
                    invalidOp $"Unsupported cooperative runtime folder format in path '%s{path}'."

                "CooperativeRuntime",
                Some
                    {
                        EWC = int workerMatch.Groups[1].Value
                        EWS = int workerMatch.Groups[2].Value
                        BWC = int workerMatch.Groups[3].Value
                    }
            | _ when runtimeToken.StartsWith "concurrentruntime" ->
                let workerMatch =
                    Regex.Match(runtimeToken, @"^concurrentruntime-ewc-(\d+)-ews-(\d+)-bwc-(\d+)$")

                if not workerMatch.Success then
                    invalidOp $"Unsupported concurrent runtime folder format in path '%s{path}'."

                "ConcurrentRuntime",
                Some
                    {
                        EWC = int workerMatch.Groups[1].Value
                        EWS = int workerMatch.Groups[2].Value
                        BWC = int workerMatch.Groups[3].Value
                    }
            | _ -> invalidOp $"Unsupported runtime folder format in path '%s{path}'."

        {
            BenchmarkName = capitalizeFirst benchmarkMatch.Groups["bench"].Value
            ActorCount = int benchmarkMatch.Groups["actors"].Value
            RoundCount = int benchmarkMatch.Groups["rounds"].Value
            Runs = int benchmarkMatch.Groups["runs"].Value
            RuntimeName = runtimeName
            WorkerMetadata = workerMetadata
        }

/// <summary>
/// Parsed benchmark data from a CSV file including execution times and memory usage.
/// </summary>
type BenchmarkData =
    {
        /// <summary>CSV column headers.</summary>
        Headers: string list
        /// <summary>List of run numbers.</summary>
        Runs: int64 list
        /// <summary>Execution times in milliseconds.</summary>
        ExecutionTimes: int64 list
        /// <summary>Average execution time in milliseconds.</summary>
        AvgExecutionTime: float
        /// <summary>Median execution time in milliseconds.</summary>
        MedianExecutionTime: float
        /// <summary>95th percentile execution time in milliseconds.</summary>
        P95ExecutionTime: float
        /// <summary>Standard deviation of execution times.</summary>
        StdExecutionTime: float
    }

/// <summary>
/// Helper functions for parsing BenchmarkData from CSV files.
/// </summary>
module internal BenchmarkData =
    /// <summary>
    /// Expected CSV column headers for benchmark data files.
    /// </summary>
    let private expectedHeaders =
        [
            "Run"
            "Execution Time (ms)"
            "Process WS Delta (MB)"
            "Managed Heap Delta (MB)"
            "Avg. Execution Time (ms)"
            "Median Execution Time (ms)"
            "P95 Execution Time (ms)"
            "Std. Execution Time (ms)"
        ]

    /// <summary>
    /// Parses a string as a 64-bit integer using invariant culture.
    /// </summary>
    /// <param name="raw">String to parse.</param>
    /// <returns>Parsed 64-bit integer value.</returns>
    let private parseInt64 (raw: string) =
        Int64.Parse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture)

    /// <summary>
    /// Parses a string as a floating-point number using invariant culture.
    /// </summary>
    /// <param name="raw">String to parse.</param>
    /// <returns>Parsed floating-point value.</returns>
    let private parseFloat (raw: string) =
        Double.Parse(raw, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture)

    /// <summary>
    /// Parses benchmark data from a CSV file.
    /// </summary>
    /// <param name="path">Path to the CSV file.</param>
    /// <returns>Parsed benchmark data.</returns>
    let parse (path: string) =
        let csvFile = CsvFile.Load path

        let headers =
            match csvFile.Headers with
            | Some headers -> Array.toList headers
            | None -> failwith "No headers found in CSV file!"

        if headers <> expectedHeaders then
            invalidOp
                $"Unsupported benchmark CSV schema in '%s{path}'. Regenerate benchmark CSV files with the current FIO.Benchmarks version."

        let runs = ResizeArray<int64>()
        let executionTimes = ResizeArray<int64>()
        let avgExecutionTimes = ResizeArray<float>()
        let medianExecutionTimes = ResizeArray<float>()
        let p95ExecutionTimes = ResizeArray<float>()
        let stdExecutionTimes = ResizeArray<float>()

        for row in csvFile.Rows do
            runs.Add(parseInt64 row[0])
            executionTimes.Add(parseInt64 row[1])
            avgExecutionTimes.Add(parseFloat row[2])
            medianExecutionTimes.Add(parseFloat row[3])
            p95ExecutionTimes.Add(parseFloat row[4])
            stdExecutionTimes.Add(parseFloat row[5])

        if runs.Count = 0 then
            failwith $"CSV file contains no benchmark rows: '%s{path}'"

        {
            Headers = headers
            Runs = List.ofSeq runs
            ExecutionTimes = List.ofSeq executionTimes
            AvgExecutionTime = avgExecutionTimes[0]
            MedianExecutionTime = medianExecutionTimes[0]
            P95ExecutionTime = p95ExecutionTimes[0]
            StdExecutionTime = stdExecutionTimes[0]
        }

/// <summary>
/// Utilities for loading all CSV benchmark results from a directory.
/// </summary>
module internal CsvResults =

    /// <summary>
    /// Gets all benchmark results from subdirectories, grouped by benchmark type.
    /// </summary>
    /// <param name="rootDir">Root directory containing benchmark subdirectories.</param>
    /// <returns>List of benchmark data grouped by benchmark type.</returns>
    let getAll rootDir =
        if not <| Directory.Exists rootDir then
            invalidArg rootDir "Directory does not exist!"

        Directory.GetDirectories rootDir
        |> Array.toList
        |> List.map (fun benchDir ->
            Directory.GetFiles(benchDir, "*.csv", SearchOption.AllDirectories)
            |> Array.toList
            |> List.rev
            |> List.map (fun path -> FileMetadata.parse path, BenchmarkData.parse path))
