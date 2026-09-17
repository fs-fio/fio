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
    /// The application's effect was asked to stop: Ctrl+C, SIGTERM, Stop(), or an explicit interrupt.
    | AppInterrupted of ex: FiberInterruptedException
    /// The application crashed: an unexpected exception, a defect in the effect, or an invalid argument.
    | AppFatalError of ex: exn

/// Base class for a FIO application. Override `effect`; optionally override the rest.
[<AbstractClass>]
type FIOApp<'A, 'E>() as this =

    let lazyRuntime = lazy this.runtime

    [<VolatileField>]
    let mutable running = 0

    [<VolatileField>]
    let mutable effectContext: FiberContext option = None

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

    // A request that arrives before the effect is forked is applied by scoped, so a Stop() racing
    // startup is not lost.
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

    /// The effect this application runs. Override this.
    abstract member effect: FIO<'A, 'E>

    /// The runtime used to run the effect. Defaults to the recommended runtime.
    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    /// An effect run with the application's outcome once the effect has settled and its finalizers have run,
    /// before onShutdown. Defaults to no-op.
    abstract member onOutcome: AppResult<'A, 'E> -> FIO<unit, 'E>
    default _.onOutcome _ = FIO.unit ()

    /// How long to wait for the outcome effect before continuing to shutdown. Defaults to 10 seconds.
    abstract member onOutcomeTimeout: TimeSpan
    default _.onOutcomeTimeout = TimeSpan.FromSeconds 10.0

    /// An effect run after onOutcome, before the process exits. The effect's finalizers have already run, so
    /// use this for process-level work and put cleanup in a finalizer. Defaults to no-op.
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

    /// Returns true from Run or RunAsync until onShutdown has finished.
    member _.IsRunning =
        Volatile.Read &running = 1

    /// Requests shutdown, interrupting the running effect.
    member _.Stop () =
        requestShutdown "programmatically" |> ignore

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

        Volatile.Write(&running, 1)

        task {
            let mutable signalRegistrations: PosixSignalRegistration list = []
            let mutable cancelKeyHandler: ConsoleCancelEventHandler option = None

            try
                try
                    let runtime = lazyRuntime.Value
                    let fiber = runtime.Run (scoped this.effect)

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
                                                let claimed = requestShutdownFrom (string context.Signal)
                                                context.Cancel <- claimed))
                                with ex ->
                                    eprintfn "FIOApp failed to register %O handler: %s" signal ex.Message
                                    None)

                        let handler =
                            ConsoleCancelEventHandler(fun _ args ->
                                let source = string args.SpecialKey
                                let claimed = requestShutdownFrom source
                                args.Cancel <- claimed)

                        try
                            Console.CancelKeyPress.AddHandler handler
                            cancelKeyHandler <- Some handler
                        with ex ->
                            eprintfn "FIOApp failed to register CancelKeyPress handler: %s" ex.Message

                        let! outcome =
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
    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()
