/// <summary>
/// Entry point for the FIO benchmarks application.
/// </summary>
module private FSharp.FIO.Benchmarks.Program

open System
open System.Threading

/// <summary>
/// Configures .NET ThreadPool settings for optimal benchmark performance.
/// </summary>
module private ThreadPoolConfig =

    /// <summary>
    /// Sets min/max worker and I/O threads based on processor count.
    /// </summary>
    let configure () =
        let cores = Environment.ProcessorCount
        let minWorkerThreads = cores * 2
        let maxWorkerThreads = cores * 50
        let minIOThreads = cores
        let maxIOThreads = cores * 10

        ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
        ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

do ThreadPoolConfig.configure()

/// <summary>
/// Application entry point. Parses arguments and runs the benchmark suite.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args =
    Args.print args
    let task =
        Suite.BenchmarkRunner.run
        <| Args.parse args
    task.GetAwaiter().GetResult()
    0
