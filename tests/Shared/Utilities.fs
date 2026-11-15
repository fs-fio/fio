module FIO.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.IO
open System.Net
open System.Threading
open System.Diagnostics

let testConfig =
    { WorkerConfig.Default with
        EvaluationWorkers = 2
    }

let runtimeFactories: (string * (unit -> FIORuntime)) list =
    [
        "DirectRuntime", (fun () -> new DirectRuntime() :> FIORuntime)
        "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(testConfig) :> FIORuntime)
    ]

let allRuntimes () =
    [ for _, make in runtimeFactories -> make () ]

let resultWithin (timeout: TimeSpan) (fiber: Fiber<'A, 'E>) =
    let task = fiber.Task()
    if task.Wait timeout then Some task.Result else None

let waitForFlag (flag: bool ref) =
    let deadline = DateTime.UtcNow.AddSeconds 5.0
    while not flag.Value && DateTime.UtcNow < deadline do
        Thread.Sleep 1
    flag.Value

let waitUntil (condition: unit -> bool) =
    let deadline = DateTime.UtcNow.AddSeconds 5.0
    while not (condition ()) && DateTime.UtcNow < deadline do
        Thread.Sleep 1
    condition ()

let private disposeRuntime (runtime: FIORuntime) =
    match box runtime with
    | :? IDisposable as disposable -> disposable.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
    testList
        name
        [
            for runtime in allRuntimes () ->
                testCase (runtime.GetType().Name) (fun () ->
                    try
                        f runtime
                    finally
                        disposeRuntime runtime)
        ]

let testAllRuntimesSequenced name (f: FIORuntime -> unit) = testSequenced <| testAllRuntimes name f

let stressEnabled = Environment.GetEnvironmentVariable "FIO_RUN_STRESS" = "1"

let stressTestCase name f =
    if stressEnabled then testCase name f else ptestCase name f

let stressTestAllRuntimes name (f: FIORuntime -> unit) =
    if stressEnabled then
        testAllRuntimes name f
    else
        ptestCase name ignore

let testUnix name (body: unit -> unit) =
    testCase name (fun () ->
        if OperatingSystem.IsWindows() then
            skiptest "A POSIX signal cannot be sent to a single process on Windows"

        body ())

// Async.RunSynchronously's timeout waits for the computation to stop, which a hung fiber never does, so it never fires.
let runWithin (seconds: float) (runtime: FIORuntime) (effect: FIO<'A, 'E>) =
    let fiber = runtime.Run effect
    let result = fiber.Task()

    if not (result.Wait(TimeSpan.FromSeconds seconds)) then
        runtime.Run(fiber.InterruptNow()) |> ignore
        failtest $"The effect did not finish within {seconds} s on {runtime.Name}"

    match result.Result with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let freePort () =
    let probe = new Sockets.TcpListener(IPAddress.Loopback, 0)
    probe.Start()
    let port = (probe.LocalEndpoint :?> IPEndPoint).Port
    probe.Stop()
    port

let sleepMs (ms: float) =
    FIO.sleep (TimeSpan.FromMilliseconds ms)

let waitForTerminal (fiber: Fiber<'A, 'E>) (budgetMs: int) =
    fio {
        let stopwatch = Stopwatch.StartNew()
        let mutable terminal = fiber.IsCompleted() || fiber.IsInterrupted()

        while not terminal && stopwatch.ElapsedMilliseconds < int64 budgetMs do
            do! sleepMs 20.0
            terminal <- fiber.IsCompleted() || fiber.IsInterrupted()

        return terminal, stopwatch.ElapsedMilliseconds
    }

let withCapturedError (body: unit -> 'T) : 'T * string =
    let original = Console.Error
    use captured = new StringWriter()
    Console.SetError captured

    try
        let result = body ()
        result, captured.ToString()
    finally
        Console.SetError original

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }
