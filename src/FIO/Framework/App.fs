/// FIOApp base class for running FIO effects with overridable members.
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

/// Avoids conflicts with FIO.Console.
type private SysConsole = System.Console

let private printColored color (msg: string) =
    SysConsole.ForegroundColor <- color
    SysConsole.WriteLine msg
    SysConsole.ResetColor()

/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// Override `effect` to define the main application logic.
/// All other members have sensible defaults and can be selectively overridden.
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E>() as this =

    let id = Guid.NewGuid()

    /// Lazily initialized runtime.
    let lazyRuntime = lazy this.runtime

    let mutable shutdownRequested = 0
    let mutable runningFiber: Fiber<'R, 'E> option = None

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
    member _.IsRunning = Option.isSome (Volatile.Read(&runningFiber))

    /// Requests graceful shutdown of the application.
    /// Safe to call from any thread. No-op if not running or already shutting down.
    member this.Stop() =
        match Volatile.Read(&runningFiber) with
        | Some fiber when Interlocked.CompareExchange(&shutdownRequested, 1, 0) = 0 ->
            this.onShutdownRequested ()
            fiber.Context.Interrupt(ExplicitInterrupt, "Application shutdown requested programmatically.")
        | _ -> ()

    /// Runs shutdown cleanup hooks.
    member private this.RunShutdown(runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run(this.onShutdown ())
                shutdownFiberOpt <- Some fiber
                let shutdownTask = fiber.Task()
                let! shutdownResult = shutdownTask.WaitAsync this.onShutdownTimeout

                this.onShutdownComplete shutdownResult
            with
            | :? TimeoutException as ex ->
                timedOut <- true
                this.onShutdownFailed ex
            | ex -> this.onShutdownFailed ex

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

    /// Executes the effect with lifecycle management and shutdown hooks.
    member private this.RunEffect() =
        task {
            let mutable registeredHandler: ConsoleCancelEventHandler option = None

            try
                try
                    if this.showBanner then
                        SysConsole.WriteLine this.banner

                    this.onStart ()

                    let runtime = lazyRuntime.Value
                    this.onRuntimeInitialized runtime

                    let fiber = runtime.Run this.effect
                    Volatile.Write(&runningFiber, Some fiber)

                    try
                        this.onFiberRunning ()

                        let handler =
                            ConsoleCancelEventHandler(fun _ eventArgs ->
                                // Second Ctrl+C: eventArgs.Cancel is not set,
                                // allowing hard process termination.
                                if Interlocked.CompareExchange(&shutdownRequested, 1, 0) = 0 then
                                    eventArgs.Cancel <- true
                                    this.onShutdownRequested ()

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
                                    this.onSuccess res
                                    return this.exitCodeSuccess res
                                | Failed err ->
                                    this.onError err
                                    return this.exitCodeError err
                                | Interrupted ex ->
                                    this.onInterrupted ex
                                    return this.exitCodeInterrupted ex
                            }

                        do! this.RunShutdown runtime
                        return exitCode
                    finally
                        (fiber :> IDisposable).Dispose()
                with ex ->
                    this.onFatalError ex

                    if lazyRuntime.IsValueCreated then
                        try
                            do! this.RunShutdown lazyRuntime.Value
                        with _ ->
                            ()

                    return this.exitCodeFatalError ex
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

    /// Runs the application synchronously and returns the exit code.
    /// <returns>The application exit code.</returns>
    member this.Run() =
        this.RunAsync().GetAwaiter().GetResult()

    /// Runs the application asynchronously and returns the exit code.
    /// <returns>A Task that completes with the exit code.</returns>
    member this.RunAsync() : Task<int> = this.RunEffect()

    override this.ToString() = $"{this.name} ({id})"
