/// <summary>
/// Entry point for the FIO benchmarks application.
/// </summary>
module private FIO.Benchmarks.Program

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

do ThreadPoolConfig.configure ()

/// <summary>Exit code returned on successful execution.</summary>
[<Literal>]
let private SuccessExitCode = 0

/// <summary>Exit code returned when a runtime error occurs during benchmark execution.</summary>
[<Literal>]
let private RuntimeErrorExitCode = 1

/// <summary>Exit code returned when command-line arguments are invalid.</summary>
[<Literal>]
let private InvalidArgsExitCode = 2

/// <summary>
/// Executes the benchmark suite synchronously using the provided arguments.
/// </summary>
/// <param name="benchmarkArgs">Parsed benchmark arguments.</param>
let private executeBenchmarkSuite benchmarkArgs =
    (Suite.BenchmarkRunner.run benchmarkArgs).GetAwaiter().GetResult()

/// <summary>
/// Parses command-line arguments and runs the benchmark using a custom executor function.
/// </summary>
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

/// <summary>
/// Parses command-line arguments and runs the benchmark suite.
/// </summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Exit code indicating success or failure.</returns>
let internal runWithArgs args =
    runWithArgsUsing executeBenchmarkSuite args

/// <summary>
/// Application entry point. Parses arguments and runs the benchmark suite.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args = runWithArgs args
