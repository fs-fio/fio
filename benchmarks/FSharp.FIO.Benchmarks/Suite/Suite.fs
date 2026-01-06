namespace FSharp.FIO.Benchmarks.Suite

open FSharp.FIO.Runtime

open System.Globalization

type internal BenchmarkConfig =
    | PingpongConfig of roundCount: int
    | ThreadringConfig of actorCount: int * roundCount: int
    | BigConfig of actorCount: int * roundCount: int
    | BangConfig of actorCount: int * roundCount: int
    | ForkConfig of actorCount: int

    member this.Name =
        match this with
        | PingpongConfig _ -> "Pingpong"
        | ThreadringConfig _ -> "Threadring"
        | BigConfig _ -> "Big"
        | BangConfig _ -> "Bang"
        | ForkConfig _ -> "Fork"

    member this.ConfigString =
        let ci = CultureInfo("en-US")
        match this with
        | PingpongConfig roundCount ->
            $"""Actor Count: 2 Round Count: %s{roundCount.ToString("N0", ci)}"""
        | ThreadringConfig (actorCount, roundCount) ->
            $"""Actor Count: %s{actorCount.ToString("N0", ci)} Round Count: %s{roundCount.ToString("N0", ci)}"""
        | BigConfig (actorCount, roundCount) ->
            $"""Actor Count: %s{actorCount.ToString("N0", ci)} Round Count: %s{roundCount.ToString("N0", ci)}"""
        | BangConfig (actorCount, roundCount) ->
            $"""Actor Count: %s{actorCount.ToString("N0", ci)} Round Count: %s{roundCount.ToString("N0", ci)}"""
        | ForkConfig actorCount ->
            $"""Actor Count: %s{actorCount.ToString("N0", ci)} Round Count: 1"""

    member this.ToFileString () =
        this.ToString()
            .ToLowerInvariant()
            .Replace("(", "") 
            .Replace(")", "")
            .Replace(":", "")
            .Replace(" ", "-")
            .Replace(",", "")
            .Replace(".", "")
    
    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"
        
type internal BenchmarkResult = 
    { Config: BenchmarkConfig
      RuntimeName: string
      RuntimeFileName: string
      Runs: int64 list
      ExecutionTimes: int64 list
      AvgExecutionTime: float
      StdExecutionTime: float
      MemoryUsages: int64 list
      AvgMemoryUsage: float
      StdMemoryUsage: float }

type internal BenchmarkArgs = 
    { Runtime: FRuntime
      Runs: int
      ActorIncrement: int * int
      RoundIncrement: int * int
      BenchmarkConfigs: BenchmarkConfig list
      SaveToCsv: bool 
      SavePath: string }
