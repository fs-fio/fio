module FIO.App

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Default

open System
open System.Threading
open System.Threading.Tasks

type private SysConsole = System.Console

type AppResult<'A, 'E> =
    | AppSucceeded of value: 'A
    | AppFailed of error: 'E
    | AppInterrupted of ex: FiberInterruptedException
    | AppFatalError of ex: exn

[<AbstractClass>]
type FIOApp<'A, 'E>() as this =

    let lazyRuntime = lazy this.runtime

    [<VolatileField>]
    let mutable runningFiber: Fiber<'A, 'E> option = None

    [<VolatileField>]
    let mutable shutdownRequested = 0

    [<VolatileField>]
    let mutable runStarted = 0

    abstract member effect: FIO<'A, 'E>

    abstract member runtime: FIORuntime
    default _.runtime = new DefaultRuntime()

    abstract member onShutdown: unit -> FIO<unit, 'E>
    default _.onShutdown() = FIO.unit ()

    abstract member onShutdownTimeout: TimeSpan
    default _.onShutdownTimeout = TimeSpan.FromSeconds 10.0

    abstract member mapExitCode: AppResult<'A, 'E> -> int
    default _.mapExitCode outcome =
        match outcome with
        | AppSucceeded _ -> 0
        | AppFailed _ -> 1
        | AppInterrupted _ -> 130
        | AppFatalError _ -> 2

    member _.IsRunning =
        Option.isSome (Volatile.Read &runningFiber)

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

    member this.RunAsync () =
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

    member this.Run () =
        this.RunAsync().GetAwaiter().GetResult()
