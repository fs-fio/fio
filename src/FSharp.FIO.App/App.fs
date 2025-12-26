/// <summary>
/// FIOApp base class for running FIO effects.
/// </summary>
module FSharp.FIO.App

open FSharp.FIO.DSL
open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Thread pool configuration utilities.
/// </summary>
module internal ThreadPoolConfig =

    /// <summary>
    /// Configures the .NET ThreadPool for FIO workloads.
    /// </summary>
    let configure () =
        let cores = Environment.ProcessorCount
        let minWorkerThreads = cores * 2
        let maxWorkerThreads = cores * 50
        let minIOThreads = cores
        let maxIOThreads = cores * 10

        ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
        ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

/// <summary>
/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// </summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E> () =

    let mutable _runtime: FRuntime option = None
    let mutable _cts: CancellationTokenSource option = None

    /// <summary>
    /// The main effect to execute. Must be overridden.
    /// </summary>
    abstract member effect: FIO<'R, 'E>

    /// <summary>
    /// Creates the runtime for executing effects. Default: DefaultRuntime.
    /// </summary>
    /// <returns>The runtime instance.</returns>
    abstract member runtime: FRuntime
    default _.runtime =
        new DefaultRuntime()

    /// <summary>
    /// Configures the .NET ThreadPool before runtime creation.
    /// </summary>
    abstract member configureThreadPool: unit -> unit
    default _.configureThreadPool() =
        ThreadPoolConfig.configure()

    /// <summary>
    /// Maps a successful result to an exit code. Default: 0.
    /// </summary>
    abstract member exitCodeSuccess: 'R -> int
    default _.exitCodeSuccess _ = 0

    /// <summary>
    /// Maps an error to an exit code. Default: 1.
    /// </summary>
    abstract member exitCodeError: 'E -> int
    default _.exitCodeError _ = 1

    /// <summary>
    /// Handler invoked on successful completion. Default: prints result in green.
    /// </summary>
    abstract member onSuccess: 'R -> Task<unit>
    default _.onSuccess res =
        task {
            Console.ForegroundColor <- ConsoleColor.DarkGreen
            Console.WriteLine $"%A{Ok res}"
            Console.ResetColor()
        }

    /// <summary>
    /// Handler invoked on failure. Default: prints error in red.
    /// </summary>
    abstract member onError: 'E -> Task<unit>
    default _.onError err =
        task {
            Console.ForegroundColor <- ConsoleColor.DarkRed
            Console.WriteLine $"%A{Error err}"
            Console.ResetColor()
        }

    /// <summary>
    /// Cleanup effect run on interruption (Ctrl+C). Default: no-op.
    /// </summary>
    abstract member beforeShutdown: unit -> FIO<unit, 'E>
    default _.beforeShutdown() =
        FIO.Unit()

    /// <summary>
    /// Maximum time to wait for shutdown hooks. Default: 10 seconds.
    /// </summary>
    abstract member shutdownTimeout: TimeSpan
    default _.shutdownTimeout =
        TimeSpan.FromSeconds 10.0

    /// <summary>
    /// Command-line arguments passed to the application.
    /// </summary>
    member val Args: string array =
        Array.empty with get, set

    /// <summary>
    /// Gets or creates the runtime instance (lazy initialization).
    /// </summary>
    member private this.GetOrCreateRuntime () =
        match _runtime with
        | Some rt -> rt
        | None ->
            this.configureThreadPool()
            let rt = this.runtime
            _runtime <- Some rt
            rt

    /// <summary>
    /// Executes the effect with lifecycle management and shutdown hooks.
    /// </summary>
    member private this.RunEffect (args: string array) =
        task {
            try
                this.Args <- args
                let runtime = this.GetOrCreateRuntime()
                let cts = new CancellationTokenSource()
                _cts <- Some cts

                let mutable shutdownRequested = false
                let shutdownHandler = ConsoleCancelEventHandler(fun _ eventArgs ->
                    if not shutdownRequested then
                        shutdownRequested <- true
                        eventArgs.Cancel <- true
                        printfn "Shutdown requested, cleaning up..."
                        cts.Cancel()
                )

                Console.CancelKeyPress.AddHandler shutdownHandler
                let mutable exitCode = 1

                try
                    match! runtime.Run(this.effect).Task() with
                    | Ok res ->
                        do! this.onSuccess res
                        exitCode <- this.exitCodeSuccess res
                    | Error err ->
                        do! this.onError err
                        exitCode <- this.exitCodeError err
                finally
                    Console.CancelKeyPress.RemoveHandler shutdownHandler

                if shutdownRequested then
                    try
                        let shutdownFiber = runtime.Run(this.beforeShutdown())
                        let shutdownTask = shutdownFiber.Task()
                        let! shutdownResult = shutdownTask.WaitAsync this.shutdownTimeout
                        match shutdownResult with
                        | Ok _ -> printfn "Shutdown completed successfully"
                        | Error err -> printfn $"Shutdown error: %A{err}"
                    with
                    | :? TimeoutException ->
                        printfn "Shutdown timeout exceeded"
                    | ex ->
                        printfn $"Shutdown exception: %s{ex.Message}"

                cts.Dispose()
                return exitCode

            with ex ->
                printfn $"Fatal error: %s{ex.Message}"
                return 1
        }

    /// <summary>
    /// Runs the application synchronously.
    /// </summary>
    member this.Run () =
        this.RunAsync(Array.empty).GetAwaiter().GetResult()

    /// <summary>
    /// Runs the application synchronously with command-line arguments.
    /// </summary>
    member this.Run (args: string array) =
        this.RunAsync(args).GetAwaiter().GetResult()

    /// <summary>
    /// Runs the application asynchronously.
    /// </summary>
    member this.RunAsync () =
        this.RunEffect Array.empty

    /// <summary>
    /// Runs the application asynchronously with command-line arguments.
    /// </summary>
    member this.RunAsync (args: string array) : Task<int> =
        this.RunEffect args

/// <summary>
/// FIOApp variant using 'exn' as the error type.
/// </summary>
/// <typeparam name="'R">The success result type.</typeparam>
[<AbstractClass>]
type FIOAppDefault<'R> () =
    inherit FIOApp<'R, exn>()

/// <summary>
/// FIOApp variant using 'unit' result and 'exn' error types.
/// </summary>
[<AbstractClass>]
type SimpleFIOApp () =
    inherit FIOApp<unit, exn>()
