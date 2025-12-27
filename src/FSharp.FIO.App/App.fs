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
module private ThreadPoolConfig =

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
/// Console output utilities.
/// </summary>
module private Console =

    /// <summary>
    /// Prints a message to the console with the specified color.
    /// </summary>
    let print (verbose: bool) (color, message: string) =
        if verbose then
            Console.ForegroundColor <- color
            Console.WriteLine message
            Console.ResetColor()
        

/// <summary>
/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// </summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E> () =

    let id = Guid.NewGuid()
    let syncRoot = obj()
    let mutable _runtime : FRuntime option = None

    member _.Id = id

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
    /// Whether to print verbose lifecycle messages. Default: false.
    /// </summary>
    abstract member verbose: bool
    default _.verbose = false

    /// <summary>
    /// Handler invoked on successful completion. Default: prints result in green.
    /// </summary>
    abstract member onSuccess: 'R -> unit
    default this.onSuccess res =
        Console.print this.verbose (ConsoleColor.DarkGreen, $"%A{Ok res}")

    /// <summary>
    /// Handler invoked on failure. Default: prints error in red.
    /// </summary>
    abstract member onError: 'E -> unit
    default this.onError err =
        Console.print this.verbose (ConsoleColor.DarkRed, $"%A{Error err}")

    /// <summary>
    /// Handler invoked on interruption. Default: prints interruption in yellow.
    /// </summary>
    abstract member onInterrupted: FiberInterruptedException -> unit
    default this.onInterrupted exn =
        Console.print this.verbose (ConsoleColor.DarkYellow, $"[FIOApp]: FiberInterruptedException message: {exn.Message}")

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
    /// Maps an interruption to an exit code. Default: 130 (SIGINT).
    /// </summary>
    abstract member exitCodeInterrupted: FiberInterruptedException -> int
    default _.exitCodeInterrupted _ = 130

    /// <summary>
    /// Cleanup effect run on interruption (Ctrl+C). Default: no-op.
    /// </summary>
    abstract member shutdownHook: unit -> FIO<unit, 'E>
    default _.shutdownHook() =
        FIO.Unit()

    /// <summary>
    /// Maximum time to wait for shutdown hooks. Default: 10 seconds.
    /// </summary>
    abstract member shutdownHookTimeout: TimeSpan
    default _.shutdownHookTimeout =
        TimeSpan.FromSeconds 10.0

    /// <summary>
    /// Gets or creates the runtime instance (thread-safe lazy initialization).
    /// </summary>
    member private this.GetOrCreateRuntime () =
        lock syncRoot (fun () ->
            match _runtime with
            | Some runtime -> runtime
            | None ->
                this.configureThreadPool()
                let runtime = this.runtime
                _runtime <- Some runtime
                runtime
        )

    /// <summary>
    /// Runs shutdown cleanup hooks.
    /// </summary>
    member private this.RunShutdownHook(runtime: FRuntime) =
        task {
            try
                let shutdownFiber = runtime.Run(this.shutdownHook())
                let shutdownTask = shutdownFiber.Task()
                let! shutdownResult = shutdownTask.WaitAsync this.shutdownHookTimeout
                match shutdownResult with
                | Ok _ ->
                    Console.print this.verbose (ConsoleColor.DarkYellow, "[FIOApp]: Shutdown hook completed successfully")
                | Error err ->
                    Console.print this.verbose (ConsoleColor.DarkRed, $"[FIOApp]: Shutdown hook completed with error: %A{err}")
            with
            | :? TimeoutException ->
                Console.print this.verbose (ConsoleColor.DarkYellow, $"[FIOApp]: Shutdown hook timed out {this.shutdownHookTimeout.TotalSeconds} seconds")
            | exn ->
                Console.print this.verbose (ConsoleColor.DarkRed, $"[FIOApp]: Shutdown hook failed with exception: %s{exn.Message}")
        }

    /// <summary>
    /// Executes the effect with lifecycle management and shutdown hooks.
    /// </summary>
    member private this.RunEffect () =
        task {
            try
                Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: Starting FIO application: {this.Id}")

                let runtime = this.GetOrCreateRuntime()
                Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: Runtime initialized: {runtime}")

                let fiber = runtime.Run this.effect
                Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: Executing fiber: {fiber}")

                let mutable shutdownRequested = false
                let shutdownHandler = ConsoleCancelEventHandler(fun _ eventArgs ->
                    if not shutdownRequested then
                        shutdownRequested <- true
                        eventArgs.Cancel <- true
                        Console.print this.verbose (ConsoleColor.DarkYellow, $"[FIOApp]: FIO application shutdown requested. Interrupting Fiber: {fiber}")
                        fiber.UnsafeInterrupt(ExplicitInterrupt, "Application shutdown requested")
                )
                Console.CancelKeyPress.AddHandler shutdownHandler
                
                let! exitCode =
                    task {
                        try
                            match! fiber.Task() with
                            | Ok res ->
                                this.onSuccess res
                                Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: FIO application completed with success: {this.Id}")
                                return this.exitCodeSuccess res
                            | Error err ->
                                match box err with
                                | :? FiberInterruptedException as exn ->
                                    this.onInterrupted exn
                                    Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: FIO application was interrupted: {this.Id}")
                                    return this.exitCodeInterrupted exn
                                | _ ->
                                    this.onError err
                                    Console.print this.verbose (ConsoleColor.DarkMagenta, $"[FIOApp]: FIO application completed with error: {this.Id}")
                                    return this.exitCodeError err
                        finally
                            Console.CancelKeyPress.RemoveHandler shutdownHandler
                    }

                if shutdownRequested then
                    do! this.RunShutdownHook runtime

                return exitCode

            with ex ->
                printfn $"Fatal error: %s{ex.Message}"
                return 1
        }

    /// <summary>
    /// Runs the application synchronously.
    /// </summary>
    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()

    /// <summary>
    /// Runs the application asynchronously.
    /// </summary>
    member this.RunAsync () : Task<int> =
        this.RunEffect()

    override this.ToString() =
        $"FIOApp(Id={this.Id})"

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
