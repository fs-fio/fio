module FIO.Tests.ChildProcess

open FIO.Tests.Utilities

open FIO.App
open FIO.DSL
open FIO.Runtime
open FIO.Runtime.WorkStealing

open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices

let private print (line: string) =
    FIO.attempt
        (fun () ->
            Console.Out.WriteLine line
            Console.Out.Flush())
        id

type private SignalApp(hangInFinalizer: bool) =
    inherit FIOApp<unit, exn>()

    override _.effect =
        let finalizer =
            if hangInFinalizer then (print "finalizing").FlatMap(fun () -> FIO.never ())
            else print "finalized"

        FIO.never<unit, exn>().Ensuring finalizer

    override _.onOutcome outcome =
        match outcome with
        | AppSucceeded _ -> print "outcome:Succeeded"
        | AppFailed _ -> print "outcome:Failed"
        | AppInterrupted _ -> print "outcome:Interrupted"
        | AppFatalError _ -> print "outcome:FatalError"

    override _.onShutdown () =
        print "shutdown"

type private SlowRuntimeApp() =
    inherit SignalApp false

    override _.runtime =
        Console.Out.WriteLine "creating-runtime"
        Console.Out.Flush()
        Thread.Sleep 2000
        new WorkStealingRuntime() :> FIORuntime

// RunAsync registers its signal handlers before its first await, so `ready` means a signal is handled by the app.
let private runApp (app: FIOApp<unit, exn>) =
    let running = app.RunAsync()
    Console.Out.WriteLine "ready"
    Console.Out.Flush()
    running.GetAwaiter().GetResult()

let private disposeParkedOnTask () =
    use runtime = new WorkStealingRuntime(testConfig)
    let source = TaskCompletionSource<int>()
    let fiber = runtime.Run(FIO.awaitTask source.Task id)
    Thread.Sleep 100
    (fiber :> IDisposable).Dispose()
    source.SetCanceled()

    match fiber.Task().Wait(TimeSpan.FromSeconds 5.0), fiber.Task() with
    | true, ended when (match ended.Result with Failed _ -> true | _ -> false) ->
        Console.Out.WriteLine $"survived {runtime.GetType().Name}"
        0
    | _ -> 1

let private disposeParkedWriter () =
    use runtime = new WorkStealingRuntime(testConfig)
    let channel = Channel<int>.Bounded 1
    runtime.Run(channel.Write 1).UnsafeResult() |> ignore
    let fiber = runtime.Run(channel.Write 2)
    Thread.Sleep 100
    (fiber :> IDisposable).Dispose()
    runtime.Run(channel.Read()).UnsafeResult() |> ignore

    match fiber.Task().Wait(TimeSpan.FromSeconds 5.0), fiber.Task() with
    | true, ended when (match ended.Result with Succeeded 2 -> true | _ -> false) ->
        Console.Out.WriteLine $"survived {runtime.GetType().Name}"
        0
    | _ -> 1

let private runAppBesideCancelingHandler () =
    Console.CancelKeyPress.Add(fun args -> args.Cancel <- true)
    runApp (SignalApp true)

let private runAppBesideCancelingSigtermHandler () =
    let running = (SignalApp true).RunAsync()
    use _registration = PosixSignalRegistration.Create(PosixSignal.SIGTERM, fun context -> context.Cancel <- true)
    Console.Out.WriteLine "ready"
    Console.Out.Flush()
    running.GetAwaiter().GetResult()

let run (scenario: string) : int =
    match scenario with
    | "app" -> runApp (SignalApp false)
    | "app-hanging-finalizer" -> runApp (SignalApp true)
    | "app-beside-canceling-handler" -> runAppBesideCancelingHandler ()
    | "app-beside-canceling-sigterm-handler" -> runAppBesideCancelingSigtermHandler ()
    | "app-slow-runtime" -> runApp (SlowRuntimeApp())
    | "dispose-parked-on-task" -> disposeParkedOnTask ()
    | "dispose-parked-writer" -> disposeParkedWriter ()
    | other ->
        eprintfn $"Unknown child scenario: {other}"
        2

type ChildProcess = FIO.Tests.Shared.ChildProcessRunner.ChildProcess
