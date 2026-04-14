/// FIOApp base class for running FIO effects with config records.
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

/// Avoids conflicts with FIO.Console.
type private SysConsole = System.Console

/// Static configuration for FIO applications.
type FIOAppSettings =
    {
        /// Application name. None uses the type name.
        Name: string option
        /// Version string. None uses the assembly version.
        Version: string option
        /// Application description.
        Description: string
        /// Whether to display a startup banner.
        ShowBanner: bool
        /// Custom banner text. None auto-generates from name and version.
        Banner: string option
        /// Whether to print verbose lifecycle messages.
        Verbose: bool
        /// Maximum time to wait for shutdown hooks.
        ShutdownHookTimeout: TimeSpan
    }

    static member Default =
        {
            Name = None
            Version = None
            Description = ""
            ShowBanner = false
            Banner = None
            Verbose = false
            ShutdownHookTimeout = TimeSpan.FromSeconds 10.0
        }

/// Lifecycle handlers for FIO applications.
/// Use `FIOAppHandlers.Default(settings)` to create defaults, then override individual fields.
type FIOAppHandlers<'R, 'E> =
    {
        OnStart: unit -> unit
        OnRuntimeInitialized: FIORuntime -> unit
        OnFiberRunning: unit -> unit
        OnSuccess: 'R -> unit
        OnError: 'E -> unit
        OnInterrupted: exn -> unit
        OnFatalError: exn -> unit
        OnShutdownRequested: unit -> unit
        OnShutdownHookSuccess: unit -> unit
        OnShutdownHookError: 'E -> unit
        OnShutdownHookTimeout: TimeSpan -> unit
        OnShutdownHookException: exn -> unit
        OnShutdownHookInterrupted: FiberInterruptedException -> unit
        ExitCodeSuccess: 'R -> int
        ExitCodeError: 'E -> int
        ExitCodeFatalError: exn -> int
        ExitCodeInterrupted: FiberInterruptedException -> int
        ConfigureThreadPool: unit -> unit
    }

    /// Creates default handlers based on settings.
    static member Default(settings: FIOAppSettings) =
        let printColored color (msg: string) =
            SysConsole.ForegroundColor <- color
            SysConsole.WriteLine msg
            SysConsole.ResetColor()

        let printVerbose color msg =
            if settings.Verbose then
                printColored color msg

        {
            OnStart = fun () -> printVerbose ConsoleColor.DarkMagenta "starting"
            OnRuntimeInitialized = fun runtime -> printVerbose ConsoleColor.DarkMagenta $"runtime: {runtime.Name}"
            OnFiberRunning = fun () -> printVerbose ConsoleColor.DarkMagenta "running"
            OnSuccess = fun res -> printVerbose ConsoleColor.DarkGreen $"success: %A{res}"
            OnError = fun err -> printColored ConsoleColor.DarkRed $"error: %A{err}"
            OnInterrupted = fun ex -> printVerbose ConsoleColor.DarkYellow $"interrupted: %s{ex.Message}"
            OnFatalError = fun ex -> printColored ConsoleColor.Red $"fatal: %s{ex.Message}"
            OnShutdownRequested = fun () -> printVerbose ConsoleColor.DarkCyan "shutdown"
            OnShutdownHookSuccess = fun () -> printVerbose ConsoleColor.DarkCyan "cleanup done"
            OnShutdownHookError = fun err -> printColored ConsoleColor.DarkRed $"cleanup error: %A{err}"
            OnShutdownHookTimeout =
                fun timeout -> printColored ConsoleColor.DarkCyan $"cleanup timeout ({timeout.TotalSeconds}s)"
            OnShutdownHookException = fun ex -> printColored ConsoleColor.Red $"cleanup failed: %s{ex.Message}"
            OnShutdownHookInterrupted =
                fun ex -> printVerbose ConsoleColor.DarkYellow $"cleanup interrupted: %s{ex.Message}"
            ExitCodeSuccess = fun _ -> 0
            ExitCodeError = fun _ -> 1
            ExitCodeFatalError = fun _ -> 2
            ExitCodeInterrupted = fun _ -> 130
            ConfigureThreadPool =
                fun () ->
                    let cores = Environment.ProcessorCount
                    let minWorkerThreads = cores * 2
                    let maxWorkerThreads = cores * 50
                    let minIOThreads = cores
                    let maxIOThreads = cores * 10
                    ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
                    ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore
        }

    /// Creates default handlers with no-op defaults (no console output).
    static member Silent() =
        {
            OnStart = ignore
            OnRuntimeInitialized = ignore
            OnFiberRunning = ignore
            OnSuccess = ignore
            OnError = ignore
            OnInterrupted = ignore
            OnFatalError = ignore
            OnShutdownRequested = ignore
            OnShutdownHookSuccess = ignore
            OnShutdownHookError = ignore
            OnShutdownHookTimeout = ignore
            OnShutdownHookException = ignore
            OnShutdownHookInterrupted = ignore
            ExitCodeSuccess = fun _ -> 0
            ExitCodeError = fun _ -> 1
            ExitCodeFatalError = fun _ -> 2
            ExitCodeInterrupted = fun _ -> 130
            ConfigureThreadPool = ignore
        }

/// Base class for FIO applications with lifecycle management and shutdown hooks.
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The error type.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E>(?settings: FIOAppSettings, ?handlers: FIOAppHandlers<'R, 'E>) as this =

    let settings = defaultArg settings FIOAppSettings.Default
    let handlers = defaultArg handlers (FIOAppHandlers<'R, 'E>.Default settings)

    let id = Guid.NewGuid()

    let name = defaultArg settings.Name (this.GetType().Name)

    let version =
        defaultArg
            settings.Version
            (let asm = this.GetType().Assembly
             let version = asm.GetName().Version
             if isNull version then "0.0.0" else version.ToString())

    let banner =
        defaultArg
            settings.Banner
            (let separator = String.replicate (name.Length + version.Length + 6) "─"
             $"┌{separator}┐\n│  {name} v{version}  │\n└{separator}┘")

    /// Lazily initialized runtime with ThreadPool configuration.
    let lazyRuntime =
        lazy
            (handlers.ConfigureThreadPool()
             this.runtime)

    /// Gets the unique identifier for this application instance.
    member _.Id = id

    /// Resolved application name.
    member _.Name = name

    /// Resolved version string.
    member _.Version = version

    /// Resolved banner text.
    member _.Banner = banner

    /// Gets the application settings.
    member _.Settings = settings

    /// Gets the lifecycle handlers.
    member _.Handlers = handlers

    /// The main effect to execute.
    abstract member effect: FIO<'R, 'E>

    /// Creates the runtime for executing effects. Default: DefaultRuntime.
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// Cleanup effect run after main effect completes. Default: no-op.
    abstract member shutdownHook: unit -> FIO<unit, 'E>
    default _.shutdownHook() = FIO.unit ()

    /// Runs shutdown cleanup hooks.
    member private this.RunShutdownHook(runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run(this.shutdownHook ())
                shutdownFiberOpt <- Some fiber
                let shutdownTask = fiber.Task()
                let! shutdownResult = shutdownTask.WaitAsync settings.ShutdownHookTimeout

                match shutdownResult with
                | Succeeded _ -> handlers.OnShutdownHookSuccess()
                | Failed err -> handlers.OnShutdownHookError err
                | Interrupted ex -> handlers.OnShutdownHookInterrupted ex
            with
            | :? TimeoutException ->
                timedOut <- true
                handlers.OnShutdownHookTimeout settings.ShutdownHookTimeout
            | ex -> handlers.OnShutdownHookException ex

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
        }

    /// Executes the effect with lifecycle management and shutdown hooks.
    member private this.RunEffect() =
        task {
            let mutable shutdownHandler = Unchecked.defaultof<ConsoleCancelEventHandler>
            let mutable handlerRegistered = false

            try
                try
                    if settings.ShowBanner then
                        SysConsole.WriteLine banner

                    handlers.OnStart()

                    let runtime = lazyRuntime.Value
                    handlers.OnRuntimeInitialized runtime

                    let fiber = runtime.Run this.effect
                    handlers.OnFiberRunning()

                    let mutable shutdownRequested = false

                    shutdownHandler <-
                        ConsoleCancelEventHandler(fun _ eventArgs ->
                            if not shutdownRequested then
                                shutdownRequested <- true
                                eventArgs.Cancel <- true
                                handlers.OnShutdownRequested()

                                fiber.Context.Interrupt(
                                    ExplicitInterrupt,
                                    "Application shutdown requested from console."
                                ))

                    SysConsole.CancelKeyPress.AddHandler shutdownHandler
                    handlerRegistered <- true

                    let! exitCode =
                        task {
                            match! fiber.Task() with
                            | Succeeded res ->
                                handlers.OnSuccess res
                                return handlers.ExitCodeSuccess res
                            | Failed err ->
                                handlers.OnError err
                                return handlers.ExitCodeError err
                            | Interrupted ex ->
                                handlers.OnInterrupted ex
                                return handlers.ExitCodeInterrupted ex
                        }

                    do! this.RunShutdownHook runtime
                    return exitCode
                with ex ->
                    handlers.OnFatalError ex
                    return handlers.ExitCodeFatalError ex
            finally
                if handlerRegistered then
                    SysConsole.CancelKeyPress.RemoveHandler shutdownHandler
        }

    /// Runs the application synchronously and returns the exit code.
    member this.Run() =
        this.RunAsync().GetAwaiter().GetResult()

    /// Runs the application asynchronously and returns the exit code.
    member this.RunAsync() : Task<int> = this.RunEffect()

    override this.ToString() = $"{name} ({id})"
