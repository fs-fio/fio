namespace FIO.Benchmarks.Plots

open FSharp.Data

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions

/// <summary>Represents runtime worker configuration metadata for benchmark runtimes.</summary>
type WorkerMetadata =
    {
        /// <summary>Represents the evaluation worker count.</summary>
        EWC: int
        /// <summary>Represents the evaluation worker sleep duration.</summary>
        EWS: int
        /// <summary>Represents the blocking worker count.</summary>
        BWC: int
    }

    /// <summary>Returns a formatted string displaying the worker configuration parameters.</summary>
    /// <returns>A string containing the EWC, EWS, and BWC values.</returns>
    override this.ToString() =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{this.EWC.ToString("N0", ci)} EWS: %s{this.EWS.ToString("N0", ci)} BWC: %s{this.BWC.ToString("N0", ci)}"""

/// <summary>Represents metadata extracted from benchmark CSV filenames.</summary>
type FileMetadata =
    {
        /// <summary>Represents the benchmark name.</summary>
        BenchmarkName: string
        /// <summary>Represents the number of actors in the benchmark.</summary>
        ActorCount: int
        /// <summary>Represents the number of rounds in the benchmark.</summary>
        RoundCount: int
        /// <summary>Represents the number of benchmark runs.</summary>
        Runs: int
        /// <summary>Represents the runtime name.</summary>
        RuntimeName: string
        /// <summary>Represents the optional worker configuration metadata.</summary>
        WorkerMetadata: WorkerMetadata option
    }

    /// <summary>Returns a display string containing the runtime name and optional worker configuration.</summary>
    /// <returns>A string with the runtime name, including worker metadata when present.</returns>
    override this.ToString() =
        match this.WorkerMetadata with
        | Some metadata -> $"%s{this.RuntimeName} (%s{metadata.ToString()})"
        | None -> this.RuntimeName

/// <summary>Provides parsing and formatting functions for file metadata.</summary>
module internal FileMetadata =

    /// <summary>Creates a chart title from file metadata.</summary>
    /// <param name="fm">The file metadata to format as a title.</param>
    /// <returns>A formatted chart title string including benchmark name, actor count, and round count.</returns>
    let title (fm: FileMetadata) =
        let ci = CultureInfo "en-US"
        $"""%s{fm.BenchmarkName} (Actor Count: %s{fm.ActorCount.ToString("N0", ci)} Round Count: %s{fm.RoundCount.ToString("N0", ci)})"""

    /// <summary>Transforms a string by capitalizing its first character.</summary>
    /// <param name="s">The string to capitalize.</param>
    /// <returns>The string with its first character uppercased, or the original string if empty.</returns>
    let private capitalizeFirst s =
        match s with
        | "" -> s
        | _ -> s[0] |> Char.ToUpper |> string |> (fun first -> first + s[1..])

    /// <summary>Returns file metadata extracted from a CSV file path by parsing the directory structure.</summary>
    /// <param name="path">The file path to extract metadata from.</param>
    /// <returns>The file metadata containing benchmark name, actor count, round count, runtime name, and optional worker configuration.</returns>
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

/// <summary>Represents benchmark data from a CSV file including execution times and statistics.</summary>
type BenchmarkData =
    {
        /// <summary>Represents the CSV column headers.</summary>
        Headers: string list
        /// <summary>Represents the list of run numbers.</summary>
        Runs: int64 list
        /// <summary>Represents the execution times in milliseconds.</summary>
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

/// <summary>Provides parsing functions for benchmark data from CSV files.</summary>
module internal BenchmarkData =
    /// <summary>Returns the expected CSV column headers for benchmark data files.</summary>
    /// <returns>The list of expected header strings.</returns>
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

    /// <summary>Returns a 64-bit integer from a string using invariant culture.</summary>
    /// <param name="raw">The string to convert.</param>
    /// <returns>The parsed 64-bit integer value.</returns>
    let private parseInt64 (raw: string) =
        Int64.Parse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture)

    /// <summary>Returns a floating-point number from a string using invariant culture.</summary>
    /// <param name="raw">The string to convert.</param>
    /// <returns>The parsed floating-point value.</returns>
    let private parseFloat (raw: string) =
        Double.Parse(raw, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture)

    /// <summary>Returns benchmark data extracted from a CSV file.</summary>
    /// <param name="path">The path to the CSV file to load.</param>
    /// <returns>The benchmark data containing execution times and statistics.</returns>
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

/// <summary>Provides functions for loading all CSV benchmark results from a directory.</summary>
module internal CsvResults =

    /// <summary>Returns all benchmark results from subdirectories, grouped by benchmark type.</summary>
    /// <param name="rootDir">The root directory containing benchmark subdirectories.</param>
    /// <returns>A list of benchmark data pairs grouped by benchmark type.</returns>
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
