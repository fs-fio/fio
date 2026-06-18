module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading

type private SysConsole = System.Console

/// The outcome of running a FIOApp.
type AppResult<'A, 'E> =
    /// The application's effect succeeded with a value.
    | AppSucceeded of value: 'A
    /// The application's effect failed with a typed error.
    | AppFailed of error: 'E
    /// The application's effect was interrupted.
    | AppInterrupted of ex: FiberInterruptedException
    /// The application crashed with an unexpected exception.
    | AppFatalError of ex: exn

/// Base class for a FIO application. Override `effect`; optionally override the rest.
[<AbstractClass>]
type FIOApp<'A, 'E>() as this =

    let lazyRuntime = lazy this.runtime

    [<VolatileField>]
    let mutable runningFiber: Fiber<'A, 'E> option = None

    [<VolatileField>]
    let mutable shutdownRequested = 0

    [<VolatileField>]
    let mutable runStarted = 0

    /// The effect this application runs. Override this.
    abstract member effect: FIO<'A, 'E>

    /// The runtime used to run the effect. Defaults to the recommended runtime.
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// An effect run on shutdown, before the process exits. Defaults to no-op.
    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown () = FIO.unit ()

    /// How long to wait for the shutdown effect before forcing exit. Defaults to 10 seconds.
    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout = TimeSpan.FromSeconds 10.0

    /// Maps the application's outcome to a process exit code.
    abstract member mapExitCode: AppResult<'A, 'E> -> int
    default _.mapExitCode outcome =
        match outcome with
        | AppSucceeded _ -> 0
        | AppFailed _ -> 1
        | AppInterrupted _ -> 130
        | AppFatalError _ -> 2

    /// Returns true while the application's effect is running.
    member _.IsRunning =
        Option.isSome (Volatile.Read &runningFiber)

    /// Requests shutdown, interrupting the running effect.
    member _.Stop () =
        match Volatile.Read &runningFiber with
        | Some fiber when tryClaim &shutdownRequested ->
            fiber.Context.Interrupt(
                ExplicitInterrupt,
                "Application shutdown requested programmatically.")
        | _ -> ()

    member private this.RunShutdownAsync (runtime: FIORuntime) =
        task {
            let mutable shutdownFiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run <| this.onShutdown ()
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
                    fiber.Context.Interrupt(
                        ExplicitInterrupt,
                        "Shutdown hook exceeded timeout.")

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

    /// Runs the application asynchronously and returns its process exit code.
    member this.RunAsync () =
        if not <| tryClaim &runStarted then
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
                                | Succeeded value ->
                                    return AppSucceeded value
                                | Failed error ->
                                    eprintfn "FIOApp effect failed: %A" error
                                    return AppFailed error
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
                | Some handler ->
                    SysConsole.CancelKeyPress.RemoveHandler handler
                | None ->
                    ()

                if lazyRuntime.IsValueCreated then
                    match box lazyRuntime.Value with
                    | :? IDisposable as d -> d.Dispose()
                    | _ -> ()
        }

    /// Runs the application and returns its process exit code.
    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()
