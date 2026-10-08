module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices

/// The outcome of running a FIOApp.
type AppResult<'A, 'E> =
    /// The application's effect succeeded with a value.
    | AppSucceeded of value: 'A
    /// The application's effect failed with a typed error.
    | AppFailed of error: 'E
    /// The application's effect was asked to stop: Ctrl+C, SIGTERM, SIGQUIT, Stop(), or an explicit interrupt.
    | AppInterrupted of ex: FiberInterruptedException
    /// The application crashed: an unexpected exception, a defect in the effect, an invalid argument, or resource exhaustion.
    | AppFatalError of ex: exn

/// Base class for a FIO application. Override `effect`; optionally override the rest.
[<AbstractClass>]
type FIOApp<'A, 'E>() as this =

    let lazyRuntime = lazy this.runtime

    [<VolatileField>]
    let mutable running = 0

    [<VolatileField>]
    let mutable effectContext = None

    [<VolatileField>]
    let mutable shutdownRequested = 0

    [<VolatileField>]
    let mutable runStarted = 0

    let interruptEffect (context: FiberContext) (source: string) =
        context.Interrupt(ExplicitInterrupt, $"Application shutdown requested ({source}).")

    // Interruption is the outcome only when someone asked for it. A defect or a rejected argument is a
    // crash, and for a defect the thrown exception is what onOutcome should see.
    let outcomeOfInterruption (ex: FiberInterruptedException) : AppResult<'A, 'E> =
        match ex.cause with
        | ExplicitInterrupt
        | ParentInterrupted _ -> AppInterrupted ex
        | Defect inner -> AppFatalError inner
        | InvalidArgument _
        | ResourceExhaustion _ -> AppFatalError(ex :> exn)

    // Applied by scoped when the effect is forked, so a Stop() racing startup is not lost.
    let requestShutdown (source: string) =
        if Volatile.Read &running = 1 && tryClaim &shutdownRequested then
            match Volatile.Read &effectContext with
            | Some context -> interruptEffect context source
            | None -> ()

            true
        else
            false

    // An interrupted fiber publishes before its finalizers run; a completing one waits for its children
    // to unwind. Awaiting the effect as a child of the root puts every finalizer before the hooks.
    let scoped (effect: FIO<'A, 'E>) : FIO<FiberResult<'A, 'E>, 'E> =
        effect.Fork().FlatMap(fun child ->
            Interlocked.Exchange(&effectContext, Some child.Context) |> ignore

            if Volatile.Read &shutdownRequested = 1 then
                interruptEffect child.Context "before the effect started"

            child.Await())

    // The exit code is computed after the hooks, outside anything that would run them again.
    let exitCodeOf (outcome: AppResult<'A, 'E>) =
        try
            this.mapExitCode outcome
        with ex ->
            eprintfn "FIOApp mapExitCode threw: %s" ex.Message
            1

    /// The effect this application runs. Override this.
    abstract member effect: FIO<'A, 'E>

    /// The runtime used to run the effect, disposed when the application ends. Defaults to the recommended runtime.
    abstract member runtime: FIORuntime
    default _.runtime : FIORuntime = new DefaultRuntime()

    /// An effect run with the application's settled outcome, after the effect's finalizers and before onShutdown;
    /// no-op by default.
    abstract member onOutcome: AppResult<'A, 'E> -> FIO<unit, 'E>
    default _.onOutcome _ : FIO<unit, 'E> = FIO.unit ()

    /// How long to wait for the outcome effect, 10 seconds by default: 0 to Int32.MaxValue milliseconds or
    /// Timeout.InfiniteTimeSpan; any other value makes Run throw before the effect starts.
    abstract member onOutcomeTimeout: TimeSpan
    default _.onOutcomeTimeout : TimeSpan = TimeSpan.FromSeconds 10.0

    /// An effect run after onOutcome, before the process exits; the effect's finalizers have already run, so this is
    /// for process-level work. No-op by default.
    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown () : FIO<unit, 'E> = FIO.unit ()

    /// How long to wait for the shutdown effect, 10 seconds by default: 0 to Int32.MaxValue milliseconds or
    /// Timeout.InfiniteTimeSpan; any other value makes Run throw before the effect starts.
    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout : TimeSpan = TimeSpan.FromSeconds 10.0

    /// Maps the application's outcome to a process exit code: 0 on success, 1 on a failure or a fatal error (as in
    /// ZIO), 130 when interrupted. If it throws, the exit code is 1.
    abstract member mapExitCode: AppResult<'A, 'E> -> int
    default _.mapExitCode outcome : int =
        match outcome with
        | AppSucceeded _ -> 0
        | AppFailed _ -> 1
        | AppInterrupted _ -> 130
        | AppFatalError _ -> 1

    /// Returns true from Run or RunAsync until onShutdown has finished.
    member _.IsRunning : bool =
        Volatile.Read &running = 1

    /// Requests shutdown, interrupting the running effect; before Run it does nothing. Once shutdown was requested,
    /// by this or by a signal, a further Ctrl+C or SIGTERM is no longer handled and ends the process.
    member _.Stop () : unit =
        requestShutdown "programmatically" |> ignore

    member private _.RunHookAsync (runtime: FIORuntime) (label: string) (timeout: TimeSpan) (hook: unit -> FIO<unit, 'E>) =
        task {
            let mutable fiberOpt = None
            let mutable timedOut = false

            try
                let fiber = runtime.Run(hook ())
                fiberOpt <- Some fiber
                let! _ = (fiber.Task()).WaitAsync timeout
                ()
            with
            | :? TimeoutException ->
                timedOut <- true
                eprintfn "FIOApp %s hook exceeded timeout (%O)" label timeout
            | ex ->
                eprintfn "FIOApp %s hook threw: %s" label ex.Message

            match fiberOpt with
            | Some fiber when timedOut ->
                fiber.Context.Interrupt(ExplicitInterrupt, sprintf "%s hook exceeded timeout." label)
            | Some fiber -> (fiber :> IDisposable).Dispose()
            | None -> ()
        }

    member private this.RunHooksAsync (runtime: FIORuntime) (outcomeTimeout: TimeSpan) (shutdownTimeout: TimeSpan) (outcome: AppResult<'A, 'E>) =
        task {
            do! this.RunHookAsync runtime "outcome" outcomeTimeout (fun () -> this.onOutcome outcome)
            do! this.RunHookAsync runtime "shutdown" shutdownTimeout (fun () -> this.onShutdown ())
        }

    /// Runs the application asynchronously and returns its process exit code.
    member this.RunAsync () : Task<int> =
        let outcomeTimeout = this.onOutcomeTimeout
        let shutdownTimeout = this.onShutdownTimeout
        validateTimeout (nameof this.onOutcomeTimeout) outcomeTimeout
        validateTimeout (nameof this.onShutdownTimeout) shutdownTimeout

        if not <| tryClaim &runStarted then
            invalidOp "FIOApp can only be run once per instance; create a new instance to run again."

        Volatile.Write(&running, 1)

        task {
            let mutable signalRegistrations = []
            let mutable cancelKeyHandler = None
            let mutable outcome = Unchecked.defaultof<AppResult<'A, 'E>>

            try
                try
                    let requestShutdownFrom (source: string) =
                        try
                            requestShutdown source
                        with ex ->
                            eprintfn "FIOApp failed to interrupt from %s handler: %s" source ex.Message
                            true

                    signalRegistrations <-
                        [ PosixSignal.SIGTERM ]
                        |> List.choose (fun signal ->
                            try
                                Some(
                                    PosixSignalRegistration.Create(
                                        signal,
                                        fun context ->
                                            if requestShutdownFrom (string context.Signal) then
                                                context.Cancel <- true))
                            with ex ->
                                eprintfn "FIOApp failed to register %O handler: %s" signal ex.Message
                                None)

                    let handler =
                        ConsoleCancelEventHandler(fun _ args ->
                            let source = string args.SpecialKey

                            if requestShutdownFrom source then
                                args.Cancel <- true)

                    try
                        Console.CancelKeyPress.AddHandler handler
                        cancelKeyHandler <- Some handler
                    with ex ->
                        eprintfn "FIOApp failed to register CancelKeyPress handler: %s" ex.Message

                    let runtime = lazyRuntime.Value
                    let fiber = runtime.Run (scoped this.effect)

                    try
                        let! settled =
                            task {
                                match! fiber.Task() with
                                | Succeeded(Succeeded value) ->
                                    return AppSucceeded value
                                | Succeeded(Failed error) ->
                                    return AppFailed error
                                | Succeeded(Interrupted ex) ->
                                    return outcomeOfInterruption ex
                                | Failed error ->
                                    return AppFailed error
                                | Interrupted ex ->
                                    return outcomeOfInterruption ex
                            }

                        do! this.RunHooksAsync runtime outcomeTimeout shutdownTimeout settled
                        outcome <- settled
                    finally
                        (fiber :> IDisposable).Dispose()
                with ex ->
                    eprintfn "FIOApp fatal error: %s" ex.Message
                    outcome <- AppFatalError ex

                    if lazyRuntime.IsValueCreated then
                        try
                            do! this.RunHooksAsync lazyRuntime.Value outcomeTimeout shutdownTimeout outcome
                        with cleanupEx ->
                            eprintfn "FIOApp fatal cleanup failed: %s" cleanupEx.Message

                return exitCodeOf outcome
            finally
                Volatile.Write(&effectContext, None)
                Volatile.Write(&running, 0)
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
    member this.Run () : int =
        this.RunAsync().GetAwaiter().GetResult()
