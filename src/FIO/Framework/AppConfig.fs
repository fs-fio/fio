/// Record-of-functions configuration for running FIO applications.
module FIO.AppConfig

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

/// Avoids conflicts with FIO.Console.
type private SysConsole = System.Console

let internal printColored color (msg: string) =
    SysConsole.ForegroundColor <- color
    SysConsole.WriteLine msg
    SysConsole.ResetColor()

/// Configuration for running FIO applications with lifecycle management and shutdown hooks.
/// Use `FIOAppConfig.create` or `FIOAppConfig.createVerbose` to construct with sensible defaults,
/// then override individual fields using record update syntax.
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
type FIOAppConfig<'R, 'E> =
    {
        /// The main effect to execute.
        Effect: FIO<'R, 'E>

        /// Factory function for the runtime. Called lazily once; the result is cached.
        /// Default: creates a DefaultRuntime.
        Runtime: unit -> FIORuntime

        /// Application name. Default: "FIOApp".
        Name: string

        /// Application version string. Default: "0.0.0".
        Version: string

        /// Whether to display a startup banner. Default: false.
        ShowBanner: bool

        /// Banner text factory. Receives name and version, returns banner text.
        /// Default: auto-generated box.
        Banner: string -> string -> string

        /// Called when the application starts.
        OnStart: unit -> unit

        /// Called after the runtime is initialized.
        /// <param name="runtime">The initialized runtime instance.</param>
        OnRuntimeInitialized: FIORuntime -> unit

        /// Called when the main fiber starts running.
        OnFiberRunning: unit -> unit

        /// Called when the main effect succeeds.
        /// <param name="res">The success result.</param>
        OnSuccess: 'R -> unit

        /// Called when the main effect fails.
        /// <param name="err">The error value.</param>
        OnError: 'E -> unit

        /// Called when the main fiber is interrupted.
        /// <param name="ex">The interruption exception.</param>
        OnInterrupted: FiberInterruptedException -> unit

        /// Called on unhandled exceptions.
        /// <param name="ex">The unhandled exception.</param>
        OnFatalError: exn -> unit

        /// Called when Ctrl+C or programmatic shutdown is requested.
        OnShutdownRequested: unit -> unit

        /// Called when the shutdown hook fiber completes.
        /// <param name="result">The fiber result from the shutdown hook.</param>
        OnShutdownComplete: FiberResult<unit, 'E> -> unit

        /// Called when the shutdown hook throws or times out.
        /// <param name="ex">The exception from the shutdown hook.</param>
        OnShutdownFailed: exn -> unit

        /// Cleanup effect run after the main effect completes. Default: no-op.
        OnShutdown: unit -> FIO<unit, 'E>

        /// Maximum time to wait for the shutdown hook. Default: 10 seconds.
        OnShutdownTimeout: TimeSpan

        /// Exit code on success. Default: always 0.
        /// <param name="res">The success result.</param>
        ExitCodeSuccess: 'R -> int

        /// Exit code on error. Default: always 1.
        /// <param name="err">The error value.</param>
        ExitCodeError: 'E -> int

        /// Exit code on fatal error. Default: always 2.
        /// <param name="ex">The fatal exception.</param>
        ExitCodeFatalError: exn -> int

        /// Exit code on interruption. Default: always 130.
        /// <param name="ex">The interruption exception.</param>
        ExitCodeInterrupted: FiberInterruptedException -> int
    }

/// Handle for a running FIO application, supporting programmatic shutdown.
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
type FIOAppHandle<'R, 'E> =
    {
        /// Task that completes with the exit code when the application finishes.
        ExitCode: Task<int>

        /// Requests graceful shutdown. Safe to call from any thread. No-op if already stopping.
        Stop: unit -> unit

        /// Whether the application is currently running.
        IsRunning: unit -> bool
    }

/// Functions for creating FIOAppConfig values with sensible defaults.
module FIOAppConfig =

    let private defaultBanner (name: string) (version: string) =
        let separator = String.replicate (name.Length + version.Length + 6) "─"
        $"┌{separator}┐\n│  {name} v{version}  │\n└{separator}┘"

    /// Creates a config with the given effect and silent defaults.
    /// All lifecycle callbacks are no-ops. Override individual fields with record update syntax.
    /// <param name="effect">The main FIO effect to execute.</param>
    /// <returns>A config with default lifecycle handlers.</returns>
    let create (effect: FIO<'R, 'E>) : FIOAppConfig<'R, 'E> =
        {
            Effect = effect
            Runtime = fun () -> new DefaultRuntime()
            Name = "FIOApp"
            Version = "0.0.0"
            ShowBanner = false
            Banner = defaultBanner
            OnStart = ignore
            OnRuntimeInitialized = ignore
            OnFiberRunning = ignore
            OnSuccess = ignore
            OnError = ignore
            OnInterrupted = ignore
            OnFatalError = ignore
            OnShutdownRequested = ignore
            OnShutdownComplete = ignore
            OnShutdownFailed = ignore
            OnShutdown = fun () -> FIO.unit ()
            OnShutdownTimeout = TimeSpan.FromSeconds 10.0
            ExitCodeSuccess = fun _ -> 0
            ExitCodeError = fun _ -> 1
            ExitCodeFatalError = fun _ -> 2
            ExitCodeInterrupted = fun _ -> 130
        }

    /// Creates a config with verbose defaults that print lifecycle events to the console.
    /// <param name="effect">The main FIO effect to execute.</param>
    /// <returns>A config with verbose lifecycle handlers.</returns>
    let createVerbose (effect: FIO<'R, 'E>) : FIOAppConfig<'R, 'E> =
        { create effect with
            OnStart = fun () -> printColored ConsoleColor.DarkMagenta "starting"
            OnRuntimeInitialized = fun rt -> printColored ConsoleColor.DarkMagenta $"runtime: {rt.Name}"
            OnFiberRunning = fun () -> printColored ConsoleColor.DarkMagenta "running"
            OnSuccess = fun res -> printColored ConsoleColor.DarkGreen $"success: %A{res}"
            OnError = fun err -> printColored ConsoleColor.DarkRed $"error: %A{err}"
            OnInterrupted = fun ex -> printColored ConsoleColor.DarkYellow $"interrupted: %s{ex.Message}"
            OnFatalError = fun ex -> printColored ConsoleColor.Red $"fatal: %s{ex.Message}"
            OnShutdownRequested = fun () -> printColored ConsoleColor.DarkCyan "shutdown"
            OnShutdownComplete =
                fun result ->
                    match result with
                    | Succeeded _ -> printColored ConsoleColor.DarkCyan "cleanup done"
                    | Failed err -> printColored ConsoleColor.DarkRed $"cleanup error: %A{err}"
                    | Interrupted ex -> printColored ConsoleColor.DarkYellow $"cleanup interrupted: %s{ex.Message}"
            OnShutdownFailed =
                fun ex ->
                    match ex with
                    | :? TimeoutException -> printColored ConsoleColor.DarkCyan "cleanup timeout"
                    | _ -> printColored ConsoleColor.Red $"cleanup failed: %s{ex.Message}"
        }

/// Functions for running FIO applications from a config.
module FIOApp =

    let private runShutdown (config: FIOAppConfig<'R, 'E>) (runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run(config.OnShutdown())
                shutdownFiberOpt <- Some fiber
                let shutdownTask = fiber.Task()
                let! shutdownResult = shutdownTask.WaitAsync config.OnShutdownTimeout

                config.OnShutdownComplete shutdownResult
            with
            | :? TimeoutException as ex ->
                timedOut <- true
                config.OnShutdownFailed ex
            | ex -> config.OnShutdownFailed ex

            if timedOut then
                match shutdownFiberOpt with
                | Some fiber ->
                    fiber.Context.Interrupt(ExplicitInterrupt, "Shutdown hook exceeded timeout.")

                    try
                        let! _ = fiber.Task().WaitAsync(TimeSpan.FromSeconds 2.0)
                        ()
                    with :? TimeoutException ->
                        ()
                | None -> ()

            match shutdownFiberOpt with
            | Some fiber -> (fiber :> IDisposable).Dispose()
            | None -> ()
        }

    let private runEffect (config: FIOAppConfig<'R, 'E>) =
        let mutable shutdownRequested = 0
        let mutable runningFiber: Fiber<'R, 'E> option = None
        let lazyRuntime = lazy config.Runtime()

        let stop () =
            match Volatile.Read(&runningFiber) with
            | Some fiber when Interlocked.CompareExchange(&shutdownRequested, 1, 0) = 0 ->
                config.OnShutdownRequested()
                fiber.Context.Interrupt(ExplicitInterrupt, "Application shutdown requested programmatically.")
            | _ -> ()

        let isRunning () =
            Option.isSome (Volatile.Read(&runningFiber))

        let exitCodeTask =
            task {
                let mutable registeredHandler: ConsoleCancelEventHandler option = None

                try
                    try
                        if config.ShowBanner then
                            SysConsole.WriteLine(config.Banner config.Name config.Version)

                        config.OnStart()

                        let runtime = lazyRuntime.Value
                        config.OnRuntimeInitialized runtime

                        let fiber = runtime.Run config.Effect
                        Volatile.Write(&runningFiber, Some fiber)

                        try
                            config.OnFiberRunning()

                            let handler =
                                ConsoleCancelEventHandler(fun _ eventArgs ->
                                    if Interlocked.CompareExchange(&shutdownRequested, 1, 0) = 0 then
                                        eventArgs.Cancel <- true
                                        config.OnShutdownRequested()

                                        fiber.Context.Interrupt(
                                            ExplicitInterrupt,
                                            "Application shutdown requested from console."
                                        ))

                            SysConsole.CancelKeyPress.AddHandler handler
                            registeredHandler <- Some handler

                            let! exitCode =
                                task {
                                    match! fiber.Task() with
                                    | Succeeded res ->
                                        config.OnSuccess res
                                        return config.ExitCodeSuccess res
                                    | Failed err ->
                                        config.OnError err
                                        return config.ExitCodeError err
                                    | Interrupted ex ->
                                        config.OnInterrupted ex
                                        return config.ExitCodeInterrupted ex
                                }

                            do! runShutdown config runtime
                            return exitCode
                        finally
                            (fiber :> IDisposable).Dispose()
                    with ex ->
                        config.OnFatalError ex

                        if lazyRuntime.IsValueCreated then
                            try
                                do! runShutdown config lazyRuntime.Value
                            with _ ->
                                ()

                        return config.ExitCodeFatalError ex
                finally
                    Volatile.Write(&runningFiber, None)
                    shutdownRequested <- 0

                    match registeredHandler with
                    | Some handler -> SysConsole.CancelKeyPress.RemoveHandler handler
                    | None -> ()

                    if lazyRuntime.IsValueCreated then
                        match box lazyRuntime.Value with
                        | :? IDisposable as d -> d.Dispose()
                        | _ -> ()
            }

        struct {|
            ExitCode = exitCodeTask
            Stop = stop
            IsRunning = isRunning
        |}

    /// Starts the application and returns a handle for programmatic control.
    /// <param name="config">The application configuration.</param>
    /// <returns>A handle with the exit code task, stop function, and running status.</returns>
    let start (config: FIOAppConfig<'R, 'E>) : FIOAppHandle<'R, 'E> =
        let result = runEffect config

        {
            ExitCode = result.ExitCode
            Stop = result.Stop
            IsRunning = result.IsRunning
        }

    /// Runs the application asynchronously and returns the exit code.
    /// <param name="config">The application configuration.</param>
    /// <returns>A Task that completes with the exit code.</returns>
    let runAsync (config: FIOAppConfig<'R, 'E>) : Task<int> =
        let result = runEffect config
        result.ExitCode

    /// Runs the application synchronously and returns the exit code.
    /// <param name="config">The application configuration.</param>
    /// <returns>The application exit code.</returns>
    let run (config: FIOAppConfig<'R, 'E>) : int =
        (runAsync config).GetAwaiter().GetResult()
