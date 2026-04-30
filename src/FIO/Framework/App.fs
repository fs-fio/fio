/// FIOApp base class for running FIO effects with overridable members.
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.AppConfig
open FIO.Runtime.Default

open System
open System.Threading.Tasks

/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// Override `effect` to define the main application logic.
/// All other members have sensible defaults and can be selectively overridden.
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E>() as this =

    let id = Guid.NewGuid()
    let mutable handle: FIOAppHandle<'R, 'E> option = None
    let lazyRuntime = lazy this.runtime

    /// Application name. Default: the concrete type name.
    /// <returns>The application name.</returns>
    abstract member name: string
    default this.name = this.GetType().Name

    /// Application version string. Default: the assembly version.
    /// <returns>The version string.</returns>
    abstract member version: string

    default this.version =
        let asm = this.GetType().Assembly
        let version = asm.GetName().Version
        if isNull version then "0.0.0" else version.ToString()

    /// Whether to display a startup banner. Default: false.
    /// <returns>True if the banner should be displayed.</returns>
    abstract member showBanner: bool
    default _.showBanner = false

    /// Banner text displayed on startup when `showBanner` is true.
    /// Default: auto-generated box from `name` and `version`.
    /// <returns>The banner text.</returns>
    abstract member banner: string

    default this.banner =
        let separator = String.replicate (this.name.Length + this.version.Length + 6) "─"
        $"┌{separator}┐\n│  {this.name} v{this.version}  │\n└{separator}┘"

    /// Whether lifecycle handlers print verbose messages. Default: false.
    /// <returns>True if verbose output is enabled.</returns>
    abstract member verbose: bool
    default _.verbose = false

    /// The main effect to execute. Must be overridden.
    /// <returns>The main FIO effect to execute.</returns>
    abstract member effect: FIO<'R, 'E>

    /// The runtime for executing effects. Default: DefaultRuntime.
    /// <returns>The runtime instance for executing effects.</returns>
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// Cleanup effect run after the main effect completes. Default: no-op.
    /// <returns>The cleanup effect to run on shutdown.</returns>
    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown() = FIO.unit ()

    /// Maximum time to wait for the shutdown hook. Default: 10 seconds.
    /// <returns>The shutdown timeout duration.</returns>
    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout = TimeSpan.FromSeconds 10.0

    /// Called when the application starts. Default: prints "starting" when verbose.
    abstract member onStart: unit -> unit

    default this.onStart() =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta "starting"

    /// Called after the runtime is initialized. Default: prints runtime name when verbose.
    /// <param name="runtime">The initialized runtime instance.</param>
    abstract member onRuntimeInitialized: FIORuntime -> unit

    default this.onRuntimeInitialized runtime =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta $"runtime: {runtime.Name}"

    /// Called when the main fiber starts running. Default: prints "running" when verbose.
    abstract member onFiberRunning: unit -> unit

    default this.onFiberRunning() =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta "running"

    /// Called when the main effect succeeds. Default: prints result when verbose.
    /// <param name="res">The success result.</param>
    abstract member onSuccess: 'R -> unit

    default this.onSuccess res =
        if this.verbose then
            printColored ConsoleColor.DarkGreen $"success: %A{res}"

    /// Called when the main effect fails. Default: prints error when verbose.
    /// <param name="err">The error value.</param>
    abstract member onError: 'E -> unit

    default this.onError err =
        if this.verbose then
            printColored ConsoleColor.DarkRed $"error: %A{err}"

    /// Called when the main fiber is interrupted. Default: prints message when verbose.
    /// <param name="ex">The interruption exception.</param>
    abstract member onInterrupted: FiberInterruptedException -> unit

    default this.onInterrupted(ex: FiberInterruptedException) =
        if this.verbose then
            printColored ConsoleColor.DarkYellow $"interrupted: %s{ex.Message}"

    /// Called on unhandled exceptions. Default: prints message when verbose.
    /// <param name="ex">The unhandled exception.</param>
    abstract member onFatalError: exn -> unit

    default this.onFatalError ex =
        if this.verbose then
            printColored ConsoleColor.Red $"fatal: %s{ex.Message}"

    /// Called when Ctrl+C is pressed. Default: prints "shutdown" when verbose.
    abstract member onShutdownRequested: unit -> unit

    default this.onShutdownRequested() =
        if this.verbose then
            printColored ConsoleColor.DarkCyan "shutdown"

    /// Called when the shutdown hook fiber completes. Default: prints result when verbose.
    /// <param name="result">The fiber result from the shutdown hook.</param>
    abstract member onShutdownComplete: FiberResult<unit, 'E> -> unit

    default this.onShutdownComplete result =
        if this.verbose then
            match result with
            | Succeeded _ -> printColored ConsoleColor.DarkCyan "cleanup done"
            | Failed err -> printColored ConsoleColor.DarkRed $"cleanup error: %A{err}"
            | Interrupted ex -> printColored ConsoleColor.DarkYellow $"cleanup interrupted: %s{ex.Message}"

    /// Called when the shutdown hook throws or times out. Default: prints message when verbose.
    /// <param name="ex">The exception from the shutdown hook.</param>
    abstract member onShutdownFailed: exn -> unit

    default this.onShutdownFailed ex =
        if this.verbose then
            match ex with
            | :? TimeoutException ->
                printColored ConsoleColor.DarkCyan $"cleanup timeout ({this.onShutdownTimeout.TotalSeconds}s)"
            | _ -> printColored ConsoleColor.Red $"cleanup failed: %s{ex.Message}"

    /// Exit code on success. Default: 0.
    /// <param name="res">The success result.</param>
    /// <returns>The exit code for a successful execution.</returns>
    abstract member exitCodeSuccess: 'R -> int
    default _.exitCodeSuccess _ = 0

    /// Exit code on error. Default: 1.
    /// <param name="err">The error value.</param>
    /// <returns>The exit code for a failed execution.</returns>
    abstract member exitCodeError: 'E -> int
    default _.exitCodeError _ = 1

    /// Exit code on fatal error. Default: 2.
    /// <param name="ex">The fatal exception.</param>
    /// <returns>The exit code for a fatal error.</returns>
    abstract member exitCodeFatalError: exn -> int
    default _.exitCodeFatalError _ = 2

    /// Exit code on interruption. Default: 130.
    /// <param name="ex">The interruption exception.</param>
    /// <returns>The exit code for an interrupted execution.</returns>
    abstract member exitCodeInterrupted: FiberInterruptedException -> int
    default _.exitCodeInterrupted _ = 130

    /// Gets the unique identifier for this application instance.
    /// <returns>The unique identifier for this application instance.</returns>
    member _.Id = id

    /// Gets the runtime instance used by this application.
    /// Unlike the overridable `runtime` member, this always returns the same cached instance.
    /// <returns>The cached runtime instance.</returns>
    member _.Runtime = lazyRuntime.Value

    /// Gets whether the application is currently running.
    /// <returns>True if the application is currently executing.</returns>
    member _.IsRunning =
        match handle with
        | Some h -> h.IsRunning()
        | None -> false

    /// Requests graceful shutdown of the application.
    /// Safe to call from any thread. No-op if not running or already shutting down.
    member _.Stop() =
        match handle with
        | Some h -> h.Stop()
        | None -> ()

    /// Builds a FIOAppConfig from the overridable members.
    member internal this.ToConfig() : FIOAppConfig<'R, 'E> =
        { FIOAppConfig.create this.effect with
            Runtime = fun () -> this.runtime
            Name = this.name
            Version = this.version
            ShowBanner = this.showBanner
            Banner = fun _ _ -> this.banner
            OnStart = this.onStart
            OnRuntimeInitialized = this.onRuntimeInitialized
            OnFiberRunning = this.onFiberRunning
            OnSuccess = this.onSuccess
            OnError = this.onError
            OnInterrupted = this.onInterrupted
            OnFatalError = this.onFatalError
            OnShutdownRequested = this.onShutdownRequested
            OnShutdownComplete = this.onShutdownComplete
            OnShutdownFailed = this.onShutdownFailed
            OnShutdown = this.onShutdown
            OnShutdownTimeout = this.onShutdownTimeout
            ExitCodeSuccess = this.exitCodeSuccess
            ExitCodeError = this.exitCodeError
            ExitCodeFatalError = this.exitCodeFatalError
            ExitCodeInterrupted = this.exitCodeInterrupted
        }

    /// Runs the application synchronously and returns the exit code.
    /// <returns>The application exit code.</returns>
    member this.Run() =
        this.RunAsync().GetAwaiter().GetResult()

    /// Runs the application asynchronously and returns the exit code.
    /// <returns>A Task that completes with the exit code.</returns>
    member this.RunAsync() : Task<int> =
        let h = FIOApp.start (this.ToConfig())
        handle <- Some h
        h.ExitCode

    override this.ToString() = $"{this.name} ({id})"
