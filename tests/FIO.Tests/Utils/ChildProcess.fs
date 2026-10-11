module FIO.Tests.ChildProcess

open FIO.Tests.Utilities

open FIO.App
open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks
open System.Collections.Concurrent
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

let private workerRuntimes : (string * (unit -> FIORuntime)) list =
    [
        "PollingRuntime", (fun () -> new PollingRuntime(testConfig) :> FIORuntime)
        "SignalingRuntime", (fun () -> new SignalingRuntime(testConfig) :> FIORuntime)
        "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(testConfig) :> FIORuntime)
    ]

let private disposeParkedOnTask () =
    let mutable failed = false

    for name, make in workerRuntimes do
        use runtime = make ()
        let source = TaskCompletionSource<int>()
        let fiber = runtime.Run(FIO.awaitTask source.Task id)
        Thread.Sleep 100
        (fiber :> IDisposable).Dispose()
        source.SetCanceled()

        match fiber.Task().Wait(TimeSpan.FromSeconds 5.0), fiber.Task() with
        | true, ended when (match ended.Result with Failed _ -> true | _ -> false) -> Console.Out.WriteLine $"survived {name}"
        | _ -> failed <- true

    if failed then 1 else 0

let private disposeParkedWriter () =
    let mutable failed = false

    for name, make in workerRuntimes do
        use runtime = make ()
        let channel = Channel<int>.Bounded 1
        runtime.Run(channel.Write 1).UnsafeResult() |> ignore
        let fiber = runtime.Run(channel.Write 2)
        Thread.Sleep 100
        (fiber :> IDisposable).Dispose()
        runtime.Run(channel.Read()).UnsafeResult() |> ignore

        match fiber.Task().Wait(TimeSpan.FromSeconds 5.0), fiber.Task() with
        | true, ended when (match ended.Result with Succeeded 2 -> true | _ -> false) -> Console.Out.WriteLine $"survived {name}"
        | _ -> failed <- true

    if failed then 1 else 0

let private runAppBesideCancelingHandler () =
    Console.CancelKeyPress.Add(fun args -> args.Cancel <- true)
    runApp (SignalApp true)

// .NET runs SIGTERM handlers newest first, so registering after RunAsync makes the app's handler run last and decide.
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

type private Marker = class end

// Re-runs this test assembly as a child process, for behaviour that needs a process of its own.
type ChildProcess(scenario: string) =
    let lines = ConcurrentQueue<string>()

    let host =
        match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
        | null
        | "" -> "dotnet"
        | path -> path

    let info =
        ProcessStartInfo(host, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)

    do
        info.ArgumentList.Add "exec"
        info.ArgumentList.Add typeof<Marker>.Assembly.Location
        info.ArgumentList.Add "--child"
        info.ArgumentList.Add scenario

    let proc = Process.Start info

    do
        proc.OutputDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue args.Data)
        proc.ErrorDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue $"stderr: {args.Data}")
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

    member _.Output : string list =
        List.ofSeq lines

    member _.WaitForLine (expected: string) (timeout: TimeSpan) : bool =
        let stopwatch = Stopwatch.StartNew()

        while not (lines |> Seq.contains expected) && not proc.HasExited && stopwatch.Elapsed < timeout do
            Thread.Sleep 20

        lines |> Seq.contains expected

    member _.Signal (name: string) : unit =
        use kill = Process.Start("kill", $"-{name} {proc.Id}")
        kill.WaitForExit()

    member _.WaitForExit (timeout: TimeSpan) : int option =
        if proc.WaitForExit(int timeout.TotalMilliseconds) then
            proc.WaitForExit()
            Some proc.ExitCode
        else
            None

    interface IDisposable with
        member _.Dispose () =
            if not proc.HasExited then
                try proc.Kill true with _ -> ()

            proc.Dispose()
