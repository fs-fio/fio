/// <summary>Provides a record-of-functions configuration model for running FIO applications.</summary>
module FIO.AppConfig

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

type private SysConsole = System.Console

/// <summary>Builds a side effect that prints a colored message to the console and resets the color.</summary>
/// <param name="color">The foreground color to apply to the message.</param>
/// <param name="msg">The message to print.</param>
let internal printColored color (msg: string) =
    SysConsole.ForegroundColor <- color
    SysConsole.WriteLine msg
    SysConsole.ResetColor()

/// <summary>Represents the configuration for running an FIO application, including the main effect, lifecycle callbacks, and exit-code mappings.</summary>
/// <typeparam name="'R">The success result type produced by the main effect.</typeparam>
/// <typeparam name="'E">The typed error type the main effect may fail with.</typeparam>
type FIOAppConfig<'R, 'E> =
    {
        /// <summary>Represents the main effect to evaluate when the application runs.</summary>
        Effect: FIO<'R, 'E>

        /// <summary>Represents the factory that produces the runtime for effect evaluation.</summary>
        Runtime: unit -> FIORuntime

        /// <summary>Represents the application name displayed in banners and diagnostics.</summary>
        Name: string

        /// <summary>Represents the application version displayed in banners and diagnostics.</summary>
        Version: string

        /// <summary>Represents whether to print a startup banner before evaluating the effect.</summary>
        ShowBanner: bool

        /// <summary>Represents the function that builds the banner text from the application name and version.</summary>
        Banner: string -> string -> string

        /// <summary>Represents the callback invoked once the application begins starting up.</summary>
        OnStart: unit -> unit

        /// <summary>Represents the callback invoked after the runtime has been constructed.</summary>
        OnRuntimeInitialized: FIORuntime -> unit

        /// <summary>Represents the callback invoked once the main fiber has been scheduled.</summary>
        OnFiberRunning: unit -> unit

        /// <summary>Represents the callback invoked when the main effect completes successfully.</summary>
        OnSuccess: 'R -> unit

        /// <summary>Represents the callback invoked when the main effect fails with a typed error.</summary>
        OnError: 'E -> unit

        /// <summary>Represents the callback invoked when the main fiber is interrupted before completion.</summary>
        OnInterrupted: FiberInterruptedException -> unit

        /// <summary>Represents the callback invoked when an unhandled exception occurs outside the managed effect.</summary>
        OnFatalError: exn -> unit

        /// <summary>Represents the callback invoked when shutdown is requested via Ctrl+C or programmatic stop.</summary>
        OnShutdownRequested: unit -> unit

        /// <summary>Represents the callback invoked once the shutdown hook fiber reaches a terminal state.</summary>
        OnShutdownComplete: FiberResult<unit, 'E> -> unit

        /// <summary>Represents the callback invoked when the shutdown hook throws or exceeds its timeout.</summary>
        OnShutdownFailed: exn -> unit

        /// <summary>Represents the cleanup effect to evaluate after the main effect terminates.</summary>
        OnShutdown: unit -> FIO<unit, 'E>

        /// <summary>Represents the maximum time the shutdown hook is allowed to run before being interrupted.</summary>
        OnShutdownTimeout: TimeSpan

        /// <summary>Represents the function that maps a successful result to the process exit code.</summary>
        ExitCodeSuccess: 'R -> int

        /// <summary>Represents the function that maps a typed error to the process exit code.</summary>
        ExitCodeError: 'E -> int

        /// <summary>Represents the function that maps a fatal exception to the process exit code.</summary>
        ExitCodeFatalError: exn -> int

        /// <summary>Represents the function that maps an interruption exception to the process exit code.</summary>
        ExitCodeInterrupted: FiberInterruptedException -> int
    }

/// <summary>Represents a handle to a running FIO application that supports observing completion and requesting shutdown.</summary>
/// <typeparam name="'R">The success result type produced by the application's main effect.</typeparam>
/// <typeparam name="'E">The typed error type the application's main effect may fail with.</typeparam>
type FIOAppHandle<'R, 'E> =
    {
        /// <summary>Represents the task that completes with the application's exit code once it terminates.</summary>
        ExitCode: Task<int>

        /// <summary>Represents the function that requests graceful shutdown; safe to call from any thread and idempotent.</summary>
        Stop: unit -> unit

        /// <summary>Represents the predicate that returns whether the application is still running.</summary>
        IsRunning: unit -> bool
    }

/// <summary>Provides factory functions for creating <c>FIOAppConfig</c> values with sensible defaults.</summary>
module FIOAppConfig =

    /// <summary>Builds the default banner text from an application name and version.</summary>
    /// <param name="name">The application name to display.</param>
    /// <param name="version">The application version to display.</param>
    /// <returns>A box-drawing banner string containing the name and version.</returns>
    let private defaultBanner (name: string) (version: string) =
        let separator = String.replicate (name.Length + version.Length + 6) "─"
        $"┌{separator}┐\n│  {name} v{version}  │\n└{separator}┘"

    /// <summary>Creates an application configuration whose lifecycle callbacks are no-ops, suitable as a starting point for record-update overrides.</summary>
    /// <param name="effect">The main effect the application will evaluate.</param>
    /// <returns>A configuration carrying <paramref name="effect"/> and silent default callbacks.</returns>
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

    /// <summary>Creates an application configuration whose lifecycle callbacks emit colored progress messages to standard output.</summary>
    /// <param name="effect">The main effect the application will evaluate.</param>
    /// <returns>A configuration carrying <paramref name="effect"/> and verbose default callbacks.</returns>
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

/// <summary>Provides functions for running FIO applications from an <c>FIOAppConfig</c>.</summary>
module FIOApp =

    /// <summary>Builds a task that evaluates the shutdown hook effect with timeout enforcement and cleanup.</summary>
    /// <param name="config">The application configuration containing the shutdown hook and callbacks.</param>
    /// <param name="runtime">The runtime used to evaluate the shutdown hook effect.</param>
    /// <returns>A task that completes when the shutdown hook has finished or been interrupted after a timeout.</returns>
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

    /// <summary>Builds the main application execution pipeline including runtime setup, fiber evaluation, signal handling, and shutdown.</summary>
    /// <param name="config">The application configuration containing the main effect and lifecycle callbacks.</param>
    /// <returns>A struct containing the exit-code task, the shutdown trigger, and the running predicate.</returns>
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

    /// <summary>Creates a running application from the given configuration and returns a handle for programmatic control.</summary>
    /// <param name="config">The application configuration to run.</param>
    /// <returns>A handle exposing the exit-code task, the shutdown trigger, and the running predicate.</returns>
    let start (config: FIOAppConfig<'R, 'E>) : FIOAppHandle<'R, 'E> =
        let result = runEffect config

        {
            ExitCode = result.ExitCode
            Stop = result.Stop
            IsRunning = result.IsRunning
        }

    /// <summary>Builds a task that asynchronously runs the application and completes with its exit code.</summary>
    /// <param name="config">The application configuration to run.</param>
    /// <returns>A task that completes with the exit code reported by the configured handlers.</returns>
    let runAsync (config: FIOAppConfig<'R, 'E>) : Task<int> =
        let result = runEffect config
        result.ExitCode

    /// <summary>Returns the exit code produced by synchronously evaluating the application.</summary>
    /// <param name="config">The application configuration to run.</param>
    /// <returns>The exit code reported by the configured handlers.</returns>
    let run (config: FIOAppConfig<'R, 'E>) : int =
        (runAsync config).GetAwaiter().GetResult()
