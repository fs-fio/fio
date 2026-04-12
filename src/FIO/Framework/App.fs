/// <summary>
/// FIOApp base class for running FIO effects.
/// </summary>
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

type private SysConsole = System.Console

/// <summary>
/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// </summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E> () as this =

    static let mutable defaultThreadPoolConfigured = 0

    let id = Guid.NewGuid()

    let lazyRuntime = lazy (
        this.configureThreadPool()
        this.runtime
    )

    /// <summary>Gets the unique identifier for this application instance.</summary>
    /// <returns>The application GUID.</returns>
    member _.Id = id

    /// <summary>
    /// Human-readable name of the application.
    /// Default: The type name of the application class.
    /// </summary>
    /// <returns>The application name.</returns>
    abstract member name: string
    default _.name = this.GetType().Name

    /// <summary>
    /// Version string of the application.
    /// Default: Assembly version or "0.0.0" if not available.
    /// </summary>
    /// <returns>The version string.</returns>
    abstract member version: string
    default _.version =
        let asm = this.GetType().Assembly
        let version = asm.GetName().Version
        if isNull version then "0.0.0" else version.ToString()

    /// <summary>
    /// Description of the application.
    /// Default: Empty string.
    /// </summary>
    /// <returns>The application description.</returns>
    abstract member description: string
    default _.description = ""

    /// <summary>
    /// Whether to display a startup banner when the application starts.
    /// Default: false.
    /// </summary>
    /// <returns>True if banner should be displayed.</returns>
    abstract member showBanner: bool
    default _.showBanner = false

    /// <summary>
    /// The startup banner text displayed when showBanner is true.
    /// Default: Auto-generated banner with name, version, and runtime info.
    /// Override to provide a custom banner.
    /// </summary>
    /// <returns>The banner text.</returns>
    abstract member banner: string
    default _.banner =
        let separator = String.replicate (this.name.Length + this.version.Length + 6) "─"
        $"┌{separator}┐\n│  {this.name} v{this.version}  │\n└{separator}┘"

    /// <summary>
    /// The main effect to execute. Must be overridden.
    /// </summary>
    /// <returns>The main FIO effect.</returns>
    abstract member effect: FIO<'R, 'E>

    /// <summary>Creates the runtime for executing effects. Default: DefaultRuntime.</summary>
    /// <returns>The runtime instance.</returns>
    abstract member runtime: FIORuntime
    default _.runtime =
        new DefaultRuntime()

    /// <summary>
    /// Configures the .NET ThreadPool before runtime creation.
    /// </summary>
    abstract member configureThreadPool: unit -> unit
    default _.configureThreadPool() =
        if Interlocked.CompareExchange(&defaultThreadPoolConfigured, 1, 0) = 0 then
            let cores = Environment.ProcessorCount
            let minWorkerThreads = cores * 2
            let maxWorkerThreads = cores * 50
            let minIOThreads = cores
            let maxIOThreads = cores * 10
            ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
            ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

    /// <summary>Whether to print verbose lifecycle messages.</summary>
    /// <returns>True if verbose mode is enabled.</returns>
    abstract member verbose: bool
    default _.verbose = false

    /// <summary>Handler invoked when the application starts.</summary>
    abstract member onStart: unit -> unit
    default this.onStart () =
        if this.showBanner then
            SysConsole.WriteLine this.banner
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkMagenta
            SysConsole.WriteLine "starting"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when the runtime is initialized.</summary>
    /// <param name="runtime">The runtime instance.</param>
    abstract member onRuntimeInitialized: FIORuntime -> unit
    default this.onRuntimeInitialized runtime =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkMagenta
            SysConsole.WriteLine $"runtime: {runtime.Name}"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when the fiber starts executing.</summary>
    abstract member onFiberRunning: unit -> unit
    default this.onFiberRunning () =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkMagenta
            SysConsole.WriteLine "running"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when the application completes successfully.</summary>
    /// <param name="res">The success result.</param>
    abstract member onSuccess: 'R -> unit
    default _.onSuccess res =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkGreen
            SysConsole.WriteLine $"success: %A{res}"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when the application completes with an error.</summary>
    /// <param name="err">The error.</param>
    abstract member onError: 'E -> unit
    default _.onError err =
        SysConsole.ForegroundColor <- ConsoleColor.DarkRed
        SysConsole.WriteLine $"error: %A{err}"
        SysConsole.ResetColor()

    /// <summary>Handler invoked when the application is interrupted.</summary>
    /// <param name="ex">The interruption exception.</param>
    abstract member onInterrupted: exn -> unit
    default this.onInterrupted ex =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkYellow
            SysConsole.WriteLine $"interrupted: %s{ex.Message}"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when a fatal error occurs.</summary>
    /// <param name="ex">The fatal exception.</param>
    abstract member onFatalError: exn -> unit
    default _.onFatalError ex =
        SysConsole.ForegroundColor <- ConsoleColor.Red
        SysConsole.WriteLine $"fatal: %s{ex.Message}"
        SysConsole.ResetColor()

    /// <summary>Handler invoked when shutdown is requested.</summary>
    abstract member onShutdownRequested: unit -> unit
    default this.onShutdownRequested () =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkCyan
            SysConsole.WriteLine "shutdown"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when shutdown hook completes successfully.</summary>
    abstract member onShutdownHookSuccess: unit -> unit
    default this.onShutdownHookSuccess () =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkCyan
            SysConsole.WriteLine "cleanup done"
            SysConsole.ResetColor()

    /// <summary>Handler invoked when shutdown hook completes with an error.</summary>
    /// <param name="err">The error.</param>
    abstract member onShutdownHookError: 'E -> unit
    default _.onShutdownHookError err =
        SysConsole.ForegroundColor <- ConsoleColor.DarkRed
        SysConsole.WriteLine $"cleanup error: %A{err}"
        SysConsole.ResetColor()

    /// <summary>Handler invoked when shutdown hook times out.</summary>
    /// <param name="timeout">The timeout duration.</param>
    abstract member onShutdownHookTimeout: TimeSpan -> unit
    default _.onShutdownHookTimeout timeout =
        SysConsole.ForegroundColor <- ConsoleColor.DarkCyan
        SysConsole.WriteLine $"cleanup timeout ({timeout.TotalSeconds}s)"
        SysConsole.ResetColor()

    /// <summary>
    /// Handler invoked when shutdown hook fails with an exception.
    /// Default: Always prints exception (ignores verbose flag) - exceptions indicate failure.
    /// Override this to integrate with logging frameworks or custom lifecycle management.
    /// </summary>
    /// <param name="ex">The exception.</param>
    abstract member onShutdownHookException: exn -> unit
    default _.onShutdownHookException ex =
        SysConsole.ForegroundColor <- ConsoleColor.Red
        SysConsole.WriteLine $"cleanup failed: %s{ex.Message}"
        SysConsole.ResetColor()

    /// <summary>Handler invoked when shutdown hook is interrupted.</summary>
    /// <param name="ex">The interruption exception.</param>
    abstract member onShutdownHookInterrupted: FiberInterruptedException -> unit
    default this.onShutdownHookInterrupted ex =
        if this.verbose then
            SysConsole.ForegroundColor <- ConsoleColor.DarkYellow
            SysConsole.WriteLine $"cleanup interrupted: %s{ex.Message}"
            SysConsole.ResetColor()

    /// <summary>Maps a successful result to an exit code.</summary>
    /// <param name="res">The success result.</param>
    /// <returns>The exit code.</returns>
    abstract member exitCodeSuccess: 'R -> int
    default _.exitCodeSuccess _ = 0

    /// <summary>Maps an error to an exit code.</summary>
    /// <param name="err">The error.</param>
    /// <returns>The exit code.</returns>
    abstract member exitCodeError: 'E -> int
    default _.exitCodeError _ = 1

    /// <summary>Maps an exception to an exit code.</summary>
    /// <param name="ex">The exception.</param>
    /// <returns>The exit code.</returns>
    abstract member exitCodeFatalError: exn -> int
    default _.exitCodeFatalError _ = 2

    /// <summary>Maps an interruption to an exit code.</summary>
    /// <param name="ex">The interruption exception.</param>
    /// <returns>The exit code.</returns>
    abstract member exitCodeInterrupted: FiberInterruptedException -> int
    default _.exitCodeInterrupted _ = 130

    /// <summary>Cleanup effect run on interruption.</summary>
    /// <returns>The shutdown effect.</returns>
    abstract member shutdownHook: unit -> FIO<unit, 'E>
    default _.shutdownHook() =
        FIO.unit()

    /// <summary>Maximum time to wait for shutdown hooks.</summary>
    /// <returns>The timeout duration.</returns>
    abstract member shutdownHookTimeout: TimeSpan
    default _.shutdownHookTimeout =
        TimeSpan.FromSeconds 10.0

    /// <summary>Runs shutdown cleanup hooks.</summary>
    /// <param name="runtime">The runtime instance.</param>
    member private this.RunShutdownHook(runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false
            try
                let fiber = runtime.Run(this.shutdownHook())
                shutdownFiberOpt <- Some fiber
                let shutdownTask = fiber.Task()
                let! shutdownResult = shutdownTask.WaitAsync this.shutdownHookTimeout
                match shutdownResult with
                | Succeeded _ -> this.onShutdownHookSuccess()
                | Failed err -> this.onShutdownHookError err
                | Interrupted ex -> this.onShutdownHookInterrupted ex
            with
            | :? TimeoutException ->
                timedOut <- true
                this.onShutdownHookTimeout this.shutdownHookTimeout
            | ex ->
                this.onShutdownHookException ex
            // On timeout, interrupt the still-running shutdown fiber and wait briefly for it to settle.
            // Without this, the fiber would keep running in the background after the app reports "cleanup timeout".
            if timedOut then
                match shutdownFiberOpt with
                | Some fiber ->
                    fiber.Context.Interrupt(ExplicitInterrupt, "Shutdown hook exceeded timeout.")
                    try
                        let! _ = fiber.Task().WaitAsync(TimeSpan.FromSeconds 2.0)
                        ()
                    with
                    | :? TimeoutException -> ()
                | None -> ()
        }

    /// <summary>Executes the effect with lifecycle management and shutdown hooks.</summary>
    member private this.RunEffect () =
        task {
            let mutable shutdownHandler = Unchecked.defaultof<ConsoleCancelEventHandler>
            let mutable handlerRegistered = false
            try
                try
                    this.onStart()

                    let runtime = lazyRuntime.Value
                    this.onRuntimeInitialized runtime

                    let fiber = runtime.Run this.effect
                    this.onFiberRunning()

                    let mutable shutdownRequested = false
                    shutdownHandler <- ConsoleCancelEventHandler(fun _ eventArgs ->
                        if not shutdownRequested then
                            shutdownRequested <- true
                            eventArgs.Cancel <- true
                            this.onShutdownRequested()
                            fiber.Context.Interrupt(ExplicitInterrupt, "Application shutdown requested from console.")
                    )
                    SysConsole.CancelKeyPress.AddHandler shutdownHandler
                    handlerRegistered <- true

                    let! exitCode =
                        task {
                            match! fiber.Task() with
                            | Succeeded res ->
                                this.onSuccess res
                                return this.exitCodeSuccess res
                            | Failed err ->
                                this.onError err
                                return this.exitCodeError err
                            | Interrupted ex ->
                                this.onInterrupted ex
                                return this.exitCodeInterrupted ex
                        }

                    do! this.RunShutdownHook runtime
                    return exitCode
                with ex ->
                    this.onFatalError ex
                    return this.exitCodeFatalError ex
            finally
                if handlerRegistered then
                    SysConsole.CancelKeyPress.RemoveHandler shutdownHandler
        }

    /// <summary>Runs the application synchronously.</summary>
    /// <returns>The exit code.</returns>
    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()

    /// <summary>Runs the application asynchronously.</summary>
    /// <returns>A task that resolves to the exit code.</returns>
    member this.RunAsync () : Task<int> =
        this.RunEffect()

    override this.ToString() =
        $"{this.name} ({id})"

/// <summary>
/// FIOApp variant using 'exn' as the error type.
/// </summary>
/// <typeparam name="'R">The success result type.</typeparam>
[<AbstractClass>]
type DefaultFIOApp<'R> () =
    inherit FIOApp<'R, exn>()

/// <summary>
/// FIOApp variant using 'unit' result and 'exn' error types.
/// </summary>
[<AbstractClass>]
type SimpleFIOApp () =
    inherit FIOApp<unit, exn>()
