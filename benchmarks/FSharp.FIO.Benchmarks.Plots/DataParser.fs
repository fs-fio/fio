module internal FSharp.FIO.Benchmarks.Plots.DataParser

open FSharp.Data

open System
open System.IO
open System.Globalization

type FileMetadata =
    { BenchmarkName: string
      ActorCount: int
      RoundCount: int
      Runs: int
      RuntimeName: string
      WorkerMetaData: WorkerMetaData option }

    member this.Title () =
        let ci = CultureInfo "en-US"
        $"""%s{this.BenchmarkName} (Actor Count: %s{this.ActorCount.ToString("N0", ci)} Round Count: %s{this.RoundCount.ToString("N0", ci)})"""

    override this.ToString () = 
        match this.WorkerMetaData with
        | Some metadata -> $"%s{this.RuntimeName} (%s{metadata.ToString()})"
        | None -> this.RuntimeName

and WorkerMetaData =
    { EWC: int
      EWS: int
      BWC: int }

    override this.ToString () = 
        let ci = CultureInfo "en-US"
        $"""EWC: %s{this.EWC.ToString("N0", ci)} EWS: %s{this.EWS.ToString("N0", ci)} BWC: %s{this.BWC.ToString("N0", ci)}"""

and BenchmarkData =
    { Headers: string list
      Runs: int64 list
      ExecutionTimes: int64 list
      AvgExecutionTime: float
      StdExecutionTime: float
      MemoryUsages: int64 list
      AvgMemoryUsage: float
      StdMemoryUsage: float }

let private parseBenchmarkData (path: string) =
    let csvFile = CsvFile.Load path
    let headers =
        match csvFile.Headers with
        | Some headers -> Array.toList headers
        | None -> failwith "No headers found in CSV file!"
        
    let unzip7 list =
        let rec loop acc1 acc2 acc3 acc4 acc5 acc6 acc7 = function
            | [] -> List.rev acc1, List.rev acc2, List.rev acc3, List.rev acc4, List.rev acc5, List.rev acc6, List.rev acc7
            | (a, b, c, d, e, f, g)::rest ->
                loop (a::acc1) (b::acc2) (c::acc3) (d::acc4) (e::acc5) (f::acc6) (g::acc7) rest
        loop [] [] [] [] [] [] [] list
    
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

let private parseFileMetadata (path: string) =
    let fileName = path.ToLowerInvariant().Split Path.DirectorySeparatorChar |> Array.last
    let split = fileName.Split('_').[0].Split '-' |> fun s -> s[..s.Length - 2]

    let bench = split[0].Trim()
    let runtime = split[9].Trim()

    let isWorkerRuntime =
        match runtime with
        | "direct" -> false
        | "cooperative" | "concurrent" -> true
        | _ -> invalidOp "Unknown runtime!"

    let capitalizeFirst s =
        match s with
        | "" -> s
        | _  -> s[0] |> Char.ToUpper |> string |> fun first -> first + s[1..]

    { BenchmarkName = capitalizeFirst bench
      ActorCount = int split[3]
      RoundCount = int split[6]
      Runs = int split[8]
      RuntimeName = capitalizeFirst runtime
      WorkerMetaData =
          if isWorkerRuntime then
              Some { EWC = int split[11]
                     EWS = int split[13]
                     BWC = int split[15] }
          else None }

let getAllCsvResults rootDir =
    if not <| Directory.Exists rootDir then
        invalidArg rootDir "Directory does not exist!"
    Directory.GetDirectories rootDir
    |> Array.toList
    |> List.map (fun benchDir ->
        Directory.GetFiles (benchDir, "*.csv", SearchOption.AllDirectories)
        |> Array.toList
        |> List.rev
        |> List.map (fun path -> parseFileMetadata path, parseBenchmarkData path))
