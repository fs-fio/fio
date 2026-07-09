module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Runtime.InteropServices

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

    /// An effect run with the application's outcome after it settles, before shutdown. Defaults to no-op.
    abstract member onOutcome: AppResult<'A, 'E> -> FIO<unit, 'E>
    default _.onOutcome _ = FIO.unit ()

    /// How long to wait for the outcome effect before continuing to shutdown. Defaults to 10 seconds.
    abstract member onOutcomeTimeout: TimeSpan
    default _.onOutcomeTimeout = TimeSpan.FromSeconds 10.0

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

    member private _.RunHookAsync (runtime: FIORuntime) (label: string) (timeout: TimeSpan) (effect: FIO<unit, 'E>) =
        task {
            let mutable fiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run effect
                fiberOpt <- Some fiber
                let! _ = (fiber.Task()).WaitAsync timeout
                ()
            with
            | :? TimeoutException ->
                timedOut <- true
                eprintfn "FIOApp %s hook exceeded timeout (%O)" label timeout
            | ex ->
                eprintfn "FIOApp %s hook threw: %s" label ex.Message

            if timedOut then
                match fiberOpt with
                | Some fiber ->
                    fiber.Context.Interrupt(
                        ExplicitInterrupt,
                        sprintf "%s hook exceeded timeout." label)

                    try
                        let! _ = (fiber.Task()).WaitAsync(TimeSpan.FromSeconds 2.0)
                        ()
                    with :? TimeoutException ->
                        ()
                | None -> ()

            match fiberOpt with
            | Some fiber -> (fiber :> IDisposable).Dispose()
            | None -> ()
        }

    member private this.RunOutcomeAsync (runtime: FIORuntime) (outcome: AppResult<'A, 'E>) =
        this.RunHookAsync runtime "outcome" this.onOutcomeTimeout (this.onOutcome outcome)

    member private this.RunShutdownAsync (runtime: FIORuntime) =
        this.RunHookAsync runtime "shutdown" this.onShutdownTimeout (this.onShutdown ())

    /// Runs the application asynchronously and returns its process exit code.
    member this.RunAsync () =
        if not <| tryClaim &runStarted then
            invalidOp "FIOApp can only be run once per instance; create a new instance to run again."

        task {
            let mutable signalRegistrations: PosixSignalRegistration list = []
            let mutable cancelKeyHandler: ConsoleCancelEventHandler option = None

            try
                try
                    let runtime = lazyRuntime.Value
                    let fiber = runtime.Run this.effect
                    Volatile.Write(&runningFiber, Some fiber)

                    try
                        let requestShutdown (source: string) =
                            if tryClaim &shutdownRequested then
                                try
                                    fiber.Context.Interrupt(
                                        ExplicitInterrupt,
                                        sprintf "Application shutdown requested (%s)." source)
                                with ex ->
                                    eprintfn "FIOApp failed to interrupt from %s handler: %s" source ex.Message

                                true
                            else
                                false

                        signalRegistrations <-
                            [ PosixSignal.SIGTERM ]
                            |> List.choose (fun signal ->
                                try
                                    Some(
                                        PosixSignalRegistration.Create(
                                            signal,
                                            fun context ->
                                                let claimed = requestShutdown (string context.Signal)
                                                context.Cancel <- claimed))
                                with ex ->
                                    eprintfn "FIOApp failed to register %O handler: %s" signal ex.Message
                                    None)

                        let handler =
                            ConsoleCancelEventHandler(fun _ args ->
                                let source = string args.SpecialKey
                                let claimed = requestShutdown source
                                args.Cancel <- claimed)

                        try
                            Console.CancelKeyPress.AddHandler handler
                            cancelKeyHandler <- Some handler
                        with ex ->
                            eprintfn "FIOApp failed to register CancelKeyPress handler: %s" ex.Message

                        let! outcome =
                            task {
                                match! fiber.Task() with
                                | Succeeded value ->
                                    return AppSucceeded value
                                | Failed error ->
                                    return AppFailed error
                                | Interrupted ex ->
                                    return AppInterrupted ex
                            }

                        do! this.RunOutcomeAsync runtime outcome
                        do! this.RunShutdownAsync runtime
                        return this.mapExitCode outcome
                    finally
                        (fiber :> IDisposable).Dispose()
                with ex ->
                    eprintfn "FIOApp fatal error: %s" ex.Message

                    if lazyRuntime.IsValueCreated then
                        try
                            do! this.RunOutcomeAsync lazyRuntime.Value (AppFatalError ex)
                            do! this.RunShutdownAsync lazyRuntime.Value
                        with cleanupEx ->
                            eprintfn "FIOApp fatal cleanup failed: %s" cleanupEx.Message

                    return this.mapExitCode (AppFatalError ex)
            finally
                Volatile.Write(&runningFiber, None)
                shutdownRequested <- 0

                for registration in signalRegistrations do
                    try
                        registration.Dispose()
                    with ex ->
                        eprintfn "FIOApp failed to remove signal handler: %s" ex.Message

                signalRegistrations <- []

                match cancelKeyHandler with
                | Some handler ->
                    try
                        Console.CancelKeyPress.RemoveHandler handler
                    with ex ->
                        eprintfn "FIOApp failed to remove CancelKeyPress handler: %s" ex.Message

                    cancelKeyHandler <- None
                | None -> ()

                if lazyRuntime.IsValueCreated then
                    match box lazyRuntime.Value with
                    | :? IDisposable as d ->
                        try
                            d.Dispose()
                        with ex ->
                            eprintfn "FIOApp failed to dispose runtime: %s" ex.Message
                    | _ -> ()
        }

    /// Runs the application and returns its process exit code.
    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()
