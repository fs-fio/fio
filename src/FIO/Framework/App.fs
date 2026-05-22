/// <summary>Provides the abstract <c>FIOApp</c> base class for running FIO effects with a minimal lifecycle surface.</summary>
module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

type private SysConsole = System.Console

/// <summary>Represents the terminal outcome of an <c>FIOApp</c> run, including the fatal-error case where the runtime itself failed to construct or schedule the effect.</summary>
/// <typeparam name="'R">The success result type produced by the application's main effect.</typeparam>
/// <typeparam name="'E">The typed error type the application's main effect may fail with.</typeparam>
type AppOutcome<'R, 'E> =
    /// <summary>Represents a run that completed successfully with the given value.</summary>
    | AppSucceeded of 'R
    /// <summary>Represents a run whose main effect failed with a typed error.</summary>
    | AppFailed of 'E
    /// <summary>Represents a run whose main fiber was interrupted before completion.</summary>
    | AppInterrupted of FiberInterruptedException
    /// <summary>Represents a run that aborted due to an unhandled exception outside the managed effect, such as a runtime construction failure.</summary>
    | AppFatalError of exn

/// <summary>Represents the abstract base for FIO applications, exposing overridable members for the main effect, runtime, shutdown hook, shutdown timeout, and exit-code mapping.</summary>
/// <typeparam name="'R">The success result type produced by the application's main effect.</typeparam>
/// <typeparam name="'E">The typed error type the application's main effect may fail with.</typeparam>
[<AbstractClass>]
type FIOApp<'R, 'E>() as this =

    let lazyRuntime = lazy this.runtime

    [<VolatileField>]
    let mutable runningFiber: Fiber<'R, 'E> option = None

    [<VolatileField>]
    let mutable shutdownRequested = 0

    [<VolatileField>]
    let mutable runStarted = 0

    /// <summary>Returns the main effect this application evaluates.</summary>
    /// <returns>The effect to run; this member must be overridden by concrete subclasses.</returns>
    abstract member effect: FIO<'R, 'E>

    /// <summary>Returns the runtime used to evaluate effects.</summary>
    /// <returns>The runtime instance; the default is a fresh <c>DefaultRuntime</c>.</returns>
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// <summary>Returns the cleanup effect that runs after the main effect terminates, whether by success, failure, or interruption.</summary>
    /// <returns>The cleanup effect; the default is a no-op.</returns>
    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown() = FIO.unit ()

    /// <summary>Returns the maximum time the shutdown hook is allowed to run before it is interrupted.</summary>
    /// <returns>The shutdown timeout; the default is 10 seconds.</returns>
    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout = TimeSpan.FromSeconds 10.0

    /// <summary>Transforms the terminal outcome of a run into the process exit code.</summary>
    /// <param name="outcome">The terminal outcome of the application's run.</param>
    /// <returns>The exit code; defaults map to <c>0</c> for success, <c>1</c> for typed error, <c>130</c> for interruption (matching the conventional SIGINT status), and <c>2</c> for fatal error.</returns>
    abstract member mapExitCode: AppOutcome<'R, 'E> -> int
    default _.mapExitCode outcome =
        match outcome with
        | AppSucceeded _ -> 0
        | AppFailed _ -> 1
        | AppInterrupted _ -> 130
        | AppFatalError _ -> 2

    /// <summary>Returns whether this application is currently running.</summary>
    /// <returns><c>true</c> while the application's main fiber has not yet terminated; <c>false</c> before <c>Run</c> has been invoked or after the run completes. Safe to call from any thread.</returns>
    member _.IsRunning = Option.isSome (Volatile.Read &runningFiber)

    /// <summary>Creates a graceful shutdown request for this application; safe to call from any thread and idempotent when not running or already stopping.</summary>
    member _.Stop() =
        match Volatile.Read &runningFiber with
        | Some fiber when tryClaim &shutdownRequested ->
            fiber.Context.Interrupt(ExplicitInterrupt, "Application shutdown requested programmatically.")
        | _ -> ()

    member private this.RunShutdownAsync(runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run(this.onShutdown ())
                shutdownFiberOpt <- Some fiber
                let! _ = (fiber.Task()).WaitAsync this.onShutdownTimeout
                ()
            with
            | :? TimeoutException ->
                timedOut <- true
                eprintfn "FIOApp shutdown hook exceeded timeout (%O)" this.onShutdownTimeout
            | ex ->
                eprintfn "FIOApp shutdown hook threw: %s" ex.Message

            if timedOut then
                match shutdownFiberOpt with
                | Some fiber ->
                    fiber.Context.Interrupt(ExplicitInterrupt, "Shutdown hook exceeded timeout.")

                    try
                        let! _ = (fiber.Task()).WaitAsync(TimeSpan.FromSeconds 2.0)
                        ()
                    with :? TimeoutException ->
                        ()
                | None -> ()

            match shutdownFiberOpt with
            | Some fiber -> (fiber :> IDisposable).Dispose()
            | None -> ()
        }

    /// <summary>Builds a task that runs the application asynchronously and completes with its exit code.</summary>
    /// <returns>A task that completes with the exit code reported by <c>mapExitCode</c>.</returns>
    /// <exception cref="System.InvalidOperationException">Raised when a prior <c>Run</c> or <c>RunAsync</c> has already started on this instance; only one live run per <c>FIOApp</c> instance is supported.</exception>
    member this.RunAsync() : Task<int> =
        if not (tryClaim &runStarted) then
            invalidOp "FIOApp is already running; concurrent Run on a single instance is not supported."

        task {
            let mutable registeredHandler: ConsoleCancelEventHandler option = None

            try
                try
                    let runtime = lazyRuntime.Value
                    let fiber = runtime.Run this.effect
                    Volatile.Write(&runningFiber, Some fiber)

                    try
                        let handler =
                            ConsoleCancelEventHandler(fun _ eventArgs ->
                                if tryClaim &shutdownRequested then
                                    eventArgs.Cancel <- true

                                    fiber.Context.Interrupt(
                                        ExplicitInterrupt,
                                        "Application shutdown requested from console."
                                    ))

                        SysConsole.CancelKeyPress.AddHandler handler
                        registeredHandler <- Some handler

                        let! outcome =
                            task {
                                match! fiber.Task() with
                                | Succeeded res -> return AppSucceeded res
                                | Failed err ->
                                    eprintfn "FIOApp effect failed: %A" err
                                    return AppFailed err
                                | Interrupted ex ->
                                    eprintfn "FIOApp effect interrupted: %s" ex.Message
                                    return AppInterrupted ex
                            }

                        do! this.RunShutdownAsync runtime
                        return this.mapExitCode outcome
                    finally
                        (fiber :> IDisposable).Dispose()
                with ex ->
                    eprintfn "FIOApp fatal error: %s" ex.Message

                    if lazyRuntime.IsValueCreated then
                        try
                            do! this.RunShutdownAsync lazyRuntime.Value
                        with _ ->
                            ()

                    return this.mapExitCode (AppFatalError ex)
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

    /// <summary>Returns the exit code produced by synchronously evaluating the application.</summary>
    /// <returns>The exit code reported by <c>mapExitCode</c>.</returns>
    member this.Run() =
        this.RunAsync().GetAwaiter().GetResult()
