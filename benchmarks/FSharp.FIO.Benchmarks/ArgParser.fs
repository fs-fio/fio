(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.ArgParser

open FSharp.FIO.Runtime
open FSharp.FIO.Benchmarks.Suite

open Argu
open System.IO

type private Arguments =
    | Direct_Runtime
    | Cooperative_Runtime of ewc: int * ews: int * bwc: int
    | Concurrent_Runtime of ewc: int * ews: int * bwc: int
    | Runs of runs: int
    | Actor_Increment of actorInc: int * times: int
    | Round_Increment of roundInc: int * times: int
    | Pingpong of roundCount: int
    | Threadring of actorCount: int * roundCount: int
    | Big of actorCount: int * roundCount: int
    | Bang of actorCount: int * roundCount: int
    | Fork of actorCount: int
    | Save of saveToCsv: bool
    | SavePath of absolutePath: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Direct_Runtime -> 
                "specify Direct runtime"
            | Cooperative_Runtime _ -> 
                "specify Cooperative runtime with ewc, ews and bwc"
            | Concurrent_Runtime _ -> 
                "specify Concurrent runtime with ewc, ews and bwc"
            | Runs _ -> 
                "specify number of runs for each benchmark"
            | Actor_Increment _ -> 
                "specify the value of actor increment and the number of times"
            | Round_Increment _ -> 
                "specify the value of round increment and the number of times"
            | Pingpong _ -> 
                "specify number of rounds for Pingpong benchmark"
            | Threadring _ -> 
                "specify number of actors and rounds for Threadring benchmark"
            | Big _ -> 
                "specify number of actors and rounds for Big benchmark"
            | Bang _ -> 
                "specify number of actors and rounds for Bang benchmark"
            | Fork _ -> 
                "specify number of actors for Fork benchmark"
            | Save _ -> 
                "should save benchmark results to csv file"
            | SavePath _ ->
                "specify absolute path to save the benchmark results csv file"

let private parser =
    ArgumentParser.Create<Arguments> (programName = "FSharp.FIO.Benchmarks")

let printUsage () =
    parser.PrintUsage ()
    |> printfn "%s"

let printArgs args =
    printfn "%s arguments: %s" parser.ProgramName (String.concat " " args)

let parseArgs args =
    let results = parser.Parse args

    let runtime: FRuntime =
        if results.Contains Direct_Runtime then
            new Direct.Runtime ()
        elif results.Contains Cooperative_Runtime then
            let ewc, ews, bwc = results.GetResult Cooperative_Runtime
            new Cooperative.Runtime { EWC = ewc; EWS = ews; BWC = bwc }
        elif results.Contains Concurrent_Runtime then
            let ewc, ews, bwc = results.GetResult Concurrent_Runtime
            new Concurrent.Runtime { EWC = ewc; EWS = ews; BWC = bwc }
        else
            invalidArg "args" "Runtime should be specified!"

    let runs = results.TryGetResult Runs|> Option.defaultValue 1

    let actorInc = results.TryGetResult Actor_Increment |> Option.defaultValue (0, 0)

    let roundInc = results.TryGetResult Round_Increment |> Option.defaultValue (0, 0)

    let configs =
        [ results.TryGetResult Pingpong |> Option.map PingpongConfig
          results.TryGetResult Threadring |> Option.map ThreadringConfig
          results.TryGetResult Big |> Option.map BigConfig
          results.TryGetResult Bang |> Option.map BangConfig
          results.TryGetResult Fork |> Option.map ForkConfig ]
        |> List.choose id

    if configs.IsEmpty then
        invalidArg "args" "At least one benchmark should be specified!"

    let saveToCsv =
        results.TryGetResult Save
        |> Option.defaultValue false

    let projectDirPath =
        Directory.GetCurrentDirectory ()
        |> Directory.GetParent
        |> _.Parent
        |> _.Parent
        |> function
            | null -> failwith "Unexpected directory structure!"
            | di -> di.FullName

    let savePath =
       results.TryGetResult SavePath
       |> Option.defaultValue (Path.Combine(projectDirPath, @"results\"))

    { Runtime = runtime
      Runs = runs
      ActorIncrement = actorInc
      RoundIncrement = roundInc
      BenchmarkConfigs = configs
      SaveToCsv = saveToCsv
      SavePath = savePath }
