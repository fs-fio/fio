/// <summary>Provides the abstract <c>FIOApp</c> base class for running FIO effects with overridable lifecycle members.</summary>
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.AppConfig
open FIO.Runtime.Default

open System
open System.Threading.Tasks

/// <summary>Represents the abstract base for FIO applications, exposing overridable members for the main effect, runtime selection, lifecycle callbacks, and exit-code mappings.</summary>
/// <typeparam name="'R">The success result type produced by the application's main effect.</typeparam>
/// <typeparam name="'E">The typed error type the application's main effect may fail with.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E>() as this =

    /// <summary>Represents the unique identifier for this application instance.</summary>
    let id = Guid.NewGuid()
    /// <summary>Represents the mutable handle to the currently running application, if any.</summary>
    let mutable handle: FIOAppHandle<'R, 'E> option = None
    /// <summary>Represents the lazily initialized runtime instance for this application.</summary>
    let lazyRuntime = lazy this.runtime

    /// <summary>Returns the application name displayed in banners and diagnostics.</summary>
    /// <returns>The application name; the default is the concrete type name.</returns>
    abstract member name: string
    default this.name = this.GetType().Name

    /// <summary>Returns the application version string displayed in banners and diagnostics.</summary>
    /// <returns>The version string; the default is the assembly version or <c>"0.0.0"</c> when unavailable.</returns>
    abstract member version: string

    default this.version =
        let asm = this.GetType().Assembly
        let version = asm.GetName().Version
        if isNull version then "0.0.0" else version.ToString()

    /// <summary>Returns whether the startup banner is printed before evaluating the effect.</summary>
    /// <returns><c>true</c> when the banner should be displayed; the default is <c>false</c>.</returns>
    abstract member showBanner: bool
    default _.showBanner = false

    /// <summary>Returns the banner text printed at startup when <c>showBanner</c> is <c>true</c>.</summary>
    /// <returns>The banner text; the default is an auto-generated box built from <c>name</c> and <c>version</c>.</returns>
    abstract member banner: string

    default this.banner =
        let separator = String.replicate (this.name.Length + this.version.Length + 6) "─"
        $"┌{separator}┐\n│  {this.name} v{this.version}  │\n└{separator}┘"

    /// <summary>Returns whether lifecycle callbacks emit verbose progress messages.</summary>
    /// <returns><c>true</c> when verbose output is enabled; the default is <c>false</c>.</returns>
    abstract member verbose: bool
    default _.verbose = false

    /// <summary>Returns the main effect this application evaluates.</summary>
    /// <returns>The effect to run; this member must be overridden by concrete subclasses.</returns>
    abstract member effect: FIO<'R, 'E>

    /// <summary>Returns the runtime used to evaluate effects.</summary>
    /// <returns>The runtime instance; the default is a fresh <c>DefaultRuntime</c>.</returns>
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// <summary>Returns the cleanup effect that runs after the main effect terminates.</summary>
    /// <returns>The cleanup effect; the default is a no-op.</returns>
    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown() = FIO.unit ()

    /// <summary>Returns the maximum time the shutdown hook is allowed to run before it is interrupted.</summary>
    /// <returns>The shutdown timeout; the default is 10 seconds.</returns>
    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout = TimeSpan.FromSeconds 10.0

    /// <summary>Builds the side effect performed when the application begins starting up.</summary>
    abstract member onStart: unit -> unit

    default this.onStart() =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta "starting"

    /// <summary>Builds the side effect performed once the runtime has been constructed.</summary>
    /// <param name="runtime">The runtime that was just initialized.</param>
    abstract member onRuntimeInitialized: FIORuntime -> unit

    default this.onRuntimeInitialized runtime =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta $"runtime: {runtime.Name}"

    /// <summary>Builds the side effect performed once the main fiber has been scheduled.</summary>
    abstract member onFiberRunning: unit -> unit

    default this.onFiberRunning() =
        if this.verbose then
            printColored ConsoleColor.DarkMagenta "running"

    /// <summary>Builds the side effect performed when the main effect completes successfully.</summary>
    /// <param name="res">The success value produced by the main effect.</param>
    abstract member onSuccess: 'R -> unit

    default this.onSuccess res =
        if this.verbose then
            printColored ConsoleColor.DarkGreen $"success: %A{res}"

    /// <summary>Builds the side effect performed when the main effect fails with a typed error.</summary>
    /// <param name="err">The typed error produced by the main effect.</param>
    abstract member onError: 'E -> unit

    default this.onError err =
        if this.verbose then
            printColored ConsoleColor.DarkRed $"error: %A{err}"

    /// <summary>Builds the side effect performed when the main fiber is interrupted before completion.</summary>
    /// <param name="ex">The interruption exception describing the cause.</param>
    abstract member onInterrupted: FiberInterruptedException -> unit

    default this.onInterrupted(ex: FiberInterruptedException) =
        if this.verbose then
            printColored ConsoleColor.DarkYellow $"interrupted: %s{ex.Message}"

    /// <summary>Builds the side effect performed when an unhandled exception occurs outside the managed effect.</summary>
    /// <param name="ex">The unhandled exception.</param>
    abstract member onFatalError: exn -> unit

    default this.onFatalError ex =
        if this.verbose then
            printColored ConsoleColor.Red $"fatal: %s{ex.Message}"

    /// <summary>Builds the side effect performed when shutdown is requested via Ctrl+C or programmatic stop.</summary>
    abstract member onShutdownRequested: unit -> unit

    default this.onShutdownRequested() =
        if this.verbose then
            printColored ConsoleColor.DarkCyan "shutdown"

    /// <summary>Builds the side effect performed once the shutdown hook fiber reaches a terminal state.</summary>
    /// <param name="result">The terminal state of the shutdown hook fiber.</param>
    abstract member onShutdownComplete: FiberResult<unit, 'E> -> unit

    default this.onShutdownComplete result =
        if this.verbose then
            match result with
            | Succeeded _ -> printColored ConsoleColor.DarkCyan "cleanup done"
            | Failed err -> printColored ConsoleColor.DarkRed $"cleanup error: %A{err}"
            | Interrupted ex -> printColored ConsoleColor.DarkYellow $"cleanup interrupted: %s{ex.Message}"

    /// <summary>Builds the side effect performed when the shutdown hook throws or exceeds its timeout.</summary>
    /// <param name="ex">The exception observed during shutdown.</param>
    abstract member onShutdownFailed: exn -> unit

    default this.onShutdownFailed ex =
        if this.verbose then
            match ex with
            | :? TimeoutException ->
                printColored ConsoleColor.DarkCyan $"cleanup timeout ({this.onShutdownTimeout.TotalSeconds}s)"
            | _ -> printColored ConsoleColor.Red $"cleanup failed: %s{ex.Message}"

    /// <summary>Transforms a successful result into the process exit code reported on success.</summary>
    /// <param name="res">The success value produced by the main effect.</param>
    /// <returns>The exit code; the default is <c>0</c>.</returns>
    abstract member exitCodeSuccess: 'R -> int
    default _.exitCodeSuccess _ = 0

    /// <summary>Transforms a typed error into the process exit code reported on failure.</summary>
    /// <param name="err">The typed error produced by the main effect.</param>
    /// <returns>The exit code; the default is <c>1</c>.</returns>
    abstract member exitCodeError: 'E -> int
    default _.exitCodeError _ = 1

    /// <summary>Transforms a fatal exception into the process exit code reported on unhandled errors.</summary>
    /// <param name="ex">The unhandled exception.</param>
    /// <returns>The exit code; the default is <c>2</c>.</returns>
    abstract member exitCodeFatalError: exn -> int
    default _.exitCodeFatalError _ = 2

    /// <summary>Transforms an interruption exception into the process exit code reported on interruption.</summary>
    /// <param name="ex">The interruption exception.</param>
    /// <returns>The exit code; the default is <c>130</c>, matching the conventional SIGINT exit status.</returns>
    abstract member exitCodeInterrupted: FiberInterruptedException -> int
    default _.exitCodeInterrupted _ = 130

    /// <summary>Returns the unique identifier assigned to this application instance.</summary>
    /// <returns>A <c>Guid</c> distinguishing this application from other instances created in this process.</returns>
    member _.Id = id

    /// <summary>Returns the cached runtime instance used by this application.</summary>
    /// <returns>The runtime that was constructed on the first access; the same instance is returned thereafter.</returns>
    member _.Runtime = lazyRuntime.Value

    /// <summary>Returns whether this application is currently running.</summary>
    /// <returns><c>true</c> while the application's main fiber has not yet terminated; <c>false</c> before <c>Run</c> has been invoked or after the run completes.</returns>
    member _.IsRunning =
        match handle with
        | Some h -> h.IsRunning()
        | None -> false

    /// <summary>Creates a graceful shutdown request for this application; safe to call from any thread and idempotent when not running or already stopping.</summary>
    member _.Stop() =
        match handle with
        | Some h -> h.Stop()
        | None -> ()

    /// <summary>Creates an <c>FIOAppConfig</c> record from this application's overridable members.</summary>
    /// <returns>A configuration record suitable for passing to <c>FIOApp.start</c> or <c>FIOApp.run</c>.</returns>
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

    /// <summary>Returns the exit code produced by synchronously evaluating the application.</summary>
    /// <returns>The exit code reported by the configured handlers.</returns>
    member this.Run() =
        this.RunAsync().GetAwaiter().GetResult()

    /// <summary>Builds a task that runs the application asynchronously and completes with its exit code.</summary>
    /// <returns>A task that completes with the exit code reported by the configured handlers.</returns>
    member this.RunAsync() : Task<int> =
        let h = FIOApp.start (this.ToConfig())
        handle <- Some h
        h.ExitCode

    /// <summary>Returns a string containing the application name and unique identifier.</summary>
    override this.ToString() = $"{this.name} ({id})"
