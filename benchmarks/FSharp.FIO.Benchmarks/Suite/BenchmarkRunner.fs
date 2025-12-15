(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.BenchmarkRunner

open FSharp.FIO.DSL
open FSharp.FIO.Runtime
open FSharp.FIO.Benchmarks.Suite.Big
open FSharp.FIO.Benchmarks.Suite.Bang
open FSharp.FIO.Benchmarks.Suite.Fork
open FSharp.FIO.Benchmarks.Suite.Pingpong
open FSharp.FIO.Benchmarks.Suite.Threadring

open System
open System.IO
open System.Diagnostics

let private writeResultToCsv result savePath =

    let rec csvContent (result: BenchmarkResult) allData acc =
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
            csvContent result ts (acc + rowStr)

    let benchName = result.Config.ToFileString()
    let folderName = $"%s{benchName}-runs-%s{result.Runs.Length.ToString()}"
    let dirPath = savePath + folderName + @"\" + result.RuntimeFileName.ToLower()

    if not <| Directory.Exists dirPath then
        Directory.CreateDirectory dirPath |> ignore

    let fileName = $"""{folderName}-{result.RuntimeFileName.ToLower()}-{DateTime.Now.ToString("dd_MM_yyyy-HH-mm-ss")}.csv"""
    let filePath = dirPath + @"\" + fileName

    let runHeader = "Run,"
    let executionTimeHeader = "Execution Time (ms),"
    let memoryUsageHeader = "Memory Usage (MB),"
    let avgExecutionTimeHeader = "Avg. Execution Time (ms),"
    let stdExecutionTimeHeader = "Std. Execution Time (ms),"
    let avgMemoryUsageHeader = "Avg. Memory Usage (MB),"
    let stdMemoryUsageHeader = "Std. Memory Usage (MB)"
    let csvHeader = $"%-28s{runHeader} %-28s{executionTimeHeader} %-28s{memoryUsageHeader} %-28s{avgExecutionTimeHeader} %-28s{stdExecutionTimeHeader} %-28s{avgMemoryUsageHeader} %s{stdMemoryUsageHeader}\n"
    
    let allData = List.map3 (fun a b c -> (a, b, c))
                    result.Runs result.ExecutionTimes result.MemoryUsages
    
    File.WriteAllText(filePath, csvHeader + csvContent result allData "")
    printfn $"\nSaved benchmark result to file: '%s{filePath}'"

let private printResult result =
    
    let header =
        $"
┌────────────────────────────────────────────────────────────────────────────────────────────┐
│  Benchmark:  %-50s{result.Config.ToString()}                            │
│  Runtime:    %-50s{result.RuntimeName}                            │
├────────────────────────────────────────────────────────────────────────────────────────────┤
│  Run                           Execution Time (ms)           Memory Usage (MB)             │
│  ────────────────────────────  ────────────────────────────  ────────────────────────────  │\n"
        
    let allData = List.map3 (fun a b c -> (a, b, c))
                    result.Runs result.ExecutionTimes result.MemoryUsages
    
    let dataRows =
        let rec dataRows curDataRows acc =
            match curDataRows with
            | [] ->
                (acc
                    + "│                                                                                            │\n"
                    + "│                                Avg. Execution Time (ms)      Avg. Memory Usage (MB)        │\n"
                    + "│                                ────────────────────────────  ────────────────────────────  │\n"
                   + $"│                                %-28f{result.AvgExecutionTime}  %-28f{result.AvgMemoryUsage}  │\n"
                    + "│                                                                                            │\n"                   
                    + "│                                Std. Execution Time (ms)      Std. Memory Usage (MB)        │\n"
                    + "│                                ────────────────────────────  ────────────────────────────  │\n"
                   + $"│                                %-28f{result.StdExecutionTime}  %-28f{result.StdMemoryUsage}  │\n"
                    + "└────────────────────────────────────────────────────────────────────────────────────────────┘")
            | (run, executionTime, memoryUsage) :: ts ->
                let str = $"│  #%-27i{run}  %-28i{executionTime}  %-28i{memoryUsage}  │\n"
                dataRows ts (acc + str)
        dataRows allData ""

    printfn $"%s{header + dataRows}"

let private runBenchmark (runtime: FRuntime) totalRuns (config: BenchmarkConfig) =
    
    let average onlyTimes =
        onlyTimes |> List.averageBy float
    
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
    
    let rec executeBenchmark (eff: FIO<int64, exn>) =
        task {
            let mutable runs = []
            let mutable executionTimes = []
            let mutable memoryUsages = []
            
            for currentRun in 1L..totalRuns do
                printfn $"[FIO.Benchmarks]: Executing: %s{config.ToString()}, Runtime: %s{runtime.ToString()}, Run (%i{currentRun}/%i{totalRuns})"
                let! benchRes = runtime.Run(eff).Task()
                use proc = Process.GetCurrentProcess()
                let memoryUsage = proc.WorkingSet64 / (1024L * 1024L)
                let executionTime =
                    match benchRes with
                    | Ok time -> time
                    | Error err -> invalidOp $"BenchmarkRunner: Failed executing benchmark with error: %A{err}"
                        
                printfn $"[FIO.Benchmarks]: Execution completed: Execution Time: %i{executionTime}ms, Memory Usage: %i{memoryUsage}MB, Run (%i{currentRun}/%i{totalRuns})"
                runs <- List.append runs [currentRun]
                executionTimes <- List.append executionTimes [executionTime]
                memoryUsages <- List.append memoryUsages [memoryUsage]
                
            return runs, executionTimes, memoryUsages
        }
    
    task {
        printfn $"[FIO.Benchmarks]: Starting session: %s{config.ToString()}, Runtime: %s{runtime.ToString()}, Runs: %i{totalRuns}"
        
        let eff =
            match config with
            | PingpongConfig roundCount ->
                createPingpongBenchmark <| PingpongConfig roundCount
            | ThreadringConfig (actorCount, roundCount) ->
                createThreadringBenchmark <| ThreadringConfig (actorCount, roundCount)
            | BigConfig (actorCount, roundCount) ->
                createBigBenchmark <| BigConfig (actorCount, roundCount)
            | BangConfig (actorCount, roundCount) ->
                createBangBenchmark <| BangConfig (actorCount, roundCount)
            | ForkConfig actorCount ->
                createForkBenchmark <| ForkConfig actorCount
            
        let! runs, executionTimes, memoryUsages = executeBenchmark eff
        
        printfn $"[FIO.Benchmarks]: Completed session: %s{config.ToString()}, Runtime: %s{runtime.ToString()}, Runs: %i{totalRuns}"
        
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

let internal runBenchmarks args =
    task {
        let actorInc, actorIncTimes = args.ActorIncrement
        let roundInc, roundIncTimes = args.RoundIncrement
        
        if args.Runs < 1 then
            invalidArg (nameof(args.Runs)) $"BenchmarkRunner: Runs argument must at least be 1. Runs = %i{args.Runs}"
        
        let configs =
            args.BenchmarkConfigs
            |> List.collect (fun config ->
                [ for actorIncTime in 0..actorIncTimes do
                    for roundIncTime in 0..roundIncTimes do
                        match config with
                        | PingpongConfig rounds -> 
                            yield PingpongConfig (rounds + (roundInc * roundIncTime))
                        | ThreadringConfig (actors, rounds) -> 
                            yield ThreadringConfig (actors + (actorInc * actorIncTime), rounds + (roundInc * roundIncTime))
                        | BigConfig (actors, rounds) -> 
                            yield BigConfig (actors + (actorInc * actorIncTime), rounds + (roundInc * roundIncTime))
                        | BangConfig (actors, rounds) -> 
                            yield BangConfig (actors + (actorInc * actorIncTime), rounds + (roundInc * roundIncTime))
                        | ForkConfig actors -> 
                            yield ForkConfig (actors + (actorInc * actorIncTime)) ])
        
        let results = List.map (runBenchmark args.Runtime args.Runs) configs
        
        for task in results do
            let! result = task
            printResult result
            if args.SaveToCsv then
                writeResultToCsv result args.SavePath
    }
