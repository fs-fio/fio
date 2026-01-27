/// <summary>
/// Command-line argument parsing for benchmark configuration.
/// </summary>
module internal FSharp.FIO.Benchmarks.Args

open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Direct
open FSharp.FIO.Benchmarks.Suite
open FSharp.FIO.Runtime.Concurrent
open FSharp.FIO.Runtime.Cooperative

open Argu

open System.IO

/// <summary>
/// Discriminated union representing all supported command-line arguments.
/// </summary>
type private Arguments =
    /// <summary>Use Direct runtime.</summary>
    | Direct_Runtime
    /// <summary>Use Cooperative runtime with worker configuration.</summary>
    | Cooperative_Runtime of ewc: int * ews: int * bwc: int
    /// <summary>Use Concurrent runtime with worker configuration.</summary>
    | Concurrent_Runtime of ewc: int * ews: int * bwc: int
    /// <summary>Number of benchmark runs.</summary>
    | Runs of runs: int
    /// <summary>Actor count increment value and number of times to apply.</summary>
    | Actor_Increment of actorInc: int * times: int
    /// <summary>Round count increment value and number of times to apply.</summary>
    | Round_Increment of roundInc: int * times: int
    /// <summary>Run Pingpong benchmark with specified round count.</summary>
    | Pingpong of roundCount: int
    /// <summary>Run Threadring benchmark with specified actor and round counts.</summary>
    | Threadring of actorCount: int * roundCount: int
    /// <summary>Run Big benchmark with specified actor and round counts.</summary>
    | Big of actorCount: int * roundCount: int
    /// <summary>Run Bang benchmark with specified actor and round counts.</summary>
    | Bang of actorCount: int * roundCount: int
    /// <summary>Run Fork benchmark with specified actor count.</summary>
    | Fork of actorCount: int
    /// <summary>Enable saving results to CSV file.</summary>
    | Save of saveToCsv: bool
    /// <summary>Absolute path for saving CSV results.</summary>
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

/// <summary>
/// Prints the argument parser usage information.
/// </summary>
let printUsage () =
   printfn "%s" (parser.PrintUsage())

/// <summary>
/// Prints the program name and provided arguments.
/// </summary>
/// <param name="args">Command-line arguments to print.</param>
let print args =
    printfn "%s arguments: %s" parser.ProgramName (String.concat " " args)

/// <summary>
/// Parses command-line arguments into a BenchmarkArgs configuration.
/// </summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Parsed benchmark arguments configuration.</returns>
let parse args =
    let results = parser.Parse args

    let runtime: FIORuntime =
        if results.Contains Direct_Runtime then
            new DirectRuntime()
        elif results.Contains Cooperative_Runtime then
            let ewc, ews, bwc = results.GetResult Cooperative_Runtime
            new CooperativeRuntime { EWC = ewc; EWS = ews; BWC = bwc }
        elif results.Contains Concurrent_Runtime then
            let ewc, ews, bwc = results.GetResult Concurrent_Runtime
            new ConcurrentRuntime { EWC = ewc; EWS = ews; BWC = bwc }
        else
            invalidArg "args" "Runtime should be specified!"

    let runs = results.TryGetResult Runs |> Option.defaultValue 1
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
        Directory.GetCurrentDirectory()
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
