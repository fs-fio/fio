/// <summary>Provides the entry point for the FIO benchmarks application.</summary>
module private FIO.Benchmarks.Program

open System
open System.Threading

/// <summary>Provides .NET ThreadPool configuration for optimal benchmark performance.</summary>
module private ThreadPoolConfig =

    /// <summary>Transforms ThreadPool settings by configuring min/max worker and I/O threads based on processor count.</summary>
    let configure () =
        let cores = Environment.ProcessorCount
        let minWorkerThreads = cores * 2
        let maxWorkerThreads = cores * 50
        let minIOThreads = cores
        let maxIOThreads = cores * 10

        ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
        ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

do ThreadPoolConfig.configure ()

/// <summary>Returns the exit code for successful execution.</summary>
/// <returns>The success exit code (0).</returns>
[<Literal>]
let private SuccessExitCode = 0

/// <summary>Returns the exit code for a runtime error during benchmark execution.</summary>
/// <returns>The runtime error exit code (1).</returns>
[<Literal>]
let private RuntimeErrorExitCode = 1

/// <summary>Returns the exit code for invalid command-line arguments.</summary>
/// <returns>The invalid arguments exit code (2).</returns>
[<Literal>]
let private InvalidArgsExitCode = 2

/// <summary>Transforms benchmark arguments by executing the benchmark suite synchronously.</summary>
/// <param name="benchmarkArgs">Parsed benchmark arguments.</param>
let private executeBenchmarkSuite benchmarkArgs =
    (Suite.BenchmarkRunner.run benchmarkArgs).GetAwaiter().GetResult()

/// <summary>Returns an exit code after parsing command-line arguments and executing via a custom executor function.</summary>
/// <param name="execute">Function to execute with parsed benchmark arguments.</param>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Exit code indicating success or failure.</returns>
let internal runWithArgsUsing execute args =
    match Args.parse args with
    | Args.HelpRequested usage ->
        printfn "%s" usage
        SuccessExitCode
    | Args.InvalidArgs(errorText, usage) ->
        eprintfn "%s" errorText
        eprintfn "%s" usage
        InvalidArgsExitCode
    | Args.Parsed benchmarkArgs ->
        try
            execute benchmarkArgs
            SuccessExitCode
        with ex ->
            eprintfn "%s" (ex.ToString())
            RuntimeErrorExitCode

/// <summary>Returns an exit code after parsing command-line arguments and executing the benchmark suite.</summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Exit code indicating success or failure.</returns>
let internal runWithArgs args =
    runWithArgsUsing executeBenchmarkSuite args

/// <summary>Returns an exit code after parsing arguments and executing the benchmark suite.</summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args = runWithArgs args
