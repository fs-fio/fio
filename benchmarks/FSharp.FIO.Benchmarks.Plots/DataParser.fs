namespace FSharp.FIO.Benchmarks.Plots

open FSharp.Data

open System
open System.IO
open System.Globalization

/// <summary>
/// Runtime worker configuration metadata (EWC, EWS, BWC parameters).
/// </summary>
type WorkerMetadata =
    { /// <summary>Effect worker count.</summary>
      EWC: int
      /// <summary>Effect worker sleep duration.</summary>
      EWS: int
      /// <summary>Blocked worker count.</summary>
      BWC: int }

    override this.ToString () =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{this.EWC.ToString("N0", ci)} EWS: %s{this.EWS.ToString("N0", ci)} BWC: %s{this.BWC.ToString("N0", ci)}"""

/// <summary>
/// Metadata extracted from benchmark CSV filenames.
/// </summary>
type FileMetadata =
    { /// <summary>Name of the benchmark.</summary>
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
      WorkerMetadata: WorkerMetadata option }

    override this.ToString () =
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

    let private capitalizeFirst s =
        match s with
        | "" -> s
        | _  -> s[0] |> Char.ToUpper |> string |> fun first -> first + s[1..]

    /// <summary>
    /// Parses file metadata from a CSV file path.
    /// </summary>
    /// <param name="path">File path to parse metadata from.</param>
    /// <returns>Parsed file metadata.</returns>
    let parse (path: string) =
        let fileName = path.ToLowerInvariant().Split Path.DirectorySeparatorChar |> Array.last
        let split = fileName.Split('_').[0].Split '-' |> fun s -> s[..s.Length - 2]

        let bench = split[0].Trim()
        let runtime = split[9].Trim()

        let isWorkerRuntime =
            match runtime with
            | "direct" -> false
            | "cooperative" | "concurrent" -> true
            | _ -> invalidOp "Unknown runtime!"

        { BenchmarkName = capitalizeFirst bench
          ActorCount = int split[3]
          RoundCount = int split[6]
          Runs = int split[8]
          RuntimeName = capitalizeFirst runtime
          WorkerMetadata =
              if isWorkerRuntime then
                  Some { EWC = int split[11]
                         EWS = int split[13]
                         BWC = int split[15] }
              else None }

/// <summary>
/// Parsed benchmark data from a CSV file including execution times and memory usage.
/// </summary>
type BenchmarkData =
    { /// <summary>CSV column headers.</summary>
      Headers: string list
      /// <summary>List of run numbers.</summary>
      Runs: int64 list
      /// <summary>Execution times in milliseconds.</summary>
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
/// Helper functions for parsing BenchmarkData from CSV files.
/// </summary>
module internal BenchmarkData =

    /// <summary>
    /// Unzips a list of 7-tuples into 7 separate lists.
    /// </summary>
    /// <param name="list">List of 7-tuples to unzip.</param>
    /// <returns>7 separate lists containing the tuple elements.</returns>
    let private unzip7 list =
        let rec loop acc1 acc2 acc3 acc4 acc5 acc6 acc7 = function
            | [] -> List.rev acc1, List.rev acc2, List.rev acc3, List.rev acc4, List.rev acc5, List.rev acc6, List.rev acc7
            | (a, b, c, d, e, f, g)::rest ->
                loop (a::acc1) (b::acc2) (c::acc3) (d::acc4) (e::acc5) (f::acc6) (g::acc7) rest
        loop [] [] [] [] [] [] [] list

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

        let runs, executionTimes, memoryUsages, avgExecutionTimes,
            stdExecutionTimes, avgMemoryUsages, stdMemoryUsage =
                csvFile.Rows |> Seq.map (fun row ->
                int64 row[0], int64 row[1], int64 row[2],
                float row[3], float row[4], float row[5],
                float row[6])
                    |> List.ofSeq
                    |> unzip7

        { Headers = headers
          Runs = runs
          ExecutionTimes = executionTimes
          AvgExecutionTime = List.head avgExecutionTimes
          StdExecutionTime = List.head stdExecutionTimes
          MemoryUsages = memoryUsages
          AvgMemoryUsage = List.head avgMemoryUsages
          StdMemoryUsage = List.head stdMemoryUsage }

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
            Directory.GetFiles (benchDir, "*.csv", SearchOption.AllDirectories)
            |> Array.toList
            |> List.rev
            |> List.map (fun path -> FileMetadata.parse path, BenchmarkData.parse path))
