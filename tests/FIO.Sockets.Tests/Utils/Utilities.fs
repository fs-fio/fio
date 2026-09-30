module FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Net
open System.Diagnostics

open Expecto
open FsCheck.FSharp

let testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

module FsCheckProperties =

    type Generators =
        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new PollingRuntime(testConfig) :> FIORuntime)
                    Gen.constant (new SignalingRuntime(testConfig) :> FIORuntime)
                    Gen.constant (new WorkStealingRuntime(testConfig) :> FIORuntime)
                ]
            |> Arb.fromGen

    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime(testConfig) :> FIORuntime
        new SignalingRuntime(testConfig) :> FIORuntime
        new WorkStealingRuntime(testConfig) :> FIORuntime
    ]

let private disposeRuntime (runtime: FIORuntime) =
    match box runtime with
    | :? IDisposable as d -> d.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
    testSequenced (
        testList
            name
            [
                for rt in runtimes () ->
                    testCase (rt.GetType().Name) (fun () ->
                        try
                            f rt
                        finally
                            disposeRuntime rt)
            ]
    )

let noopHandler (_socket: Socket) =
    FIO.unit ()

let echoHandler (socket: Socket) =
    fio {
        let! data, _ = socket.ReceiveBytes 8192
        do! socket.SendBytes data
    }

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, SocketError>) =
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 10_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let sleepMs (ms: float) =
    FIO.sleep (TimeSpan.FromMilliseconds ms)

let freePort () =
    let probe = new Sockets.TcpListener(IPAddress.Loopback, 0)
    probe.Start()
    let port = (probe.LocalEndpoint :?> IPEndPoint).Port
    probe.Stop()
    port

let waitForTerminal (fiber: Fiber<'A, 'E>) (budgetMs: int) =
    fio {
        let stopwatch = Stopwatch.StartNew()
        let mutable terminal = fiber.IsCompleted() || fiber.IsInterrupted()

        while not terminal && stopwatch.ElapsedMilliseconds < int64 budgetMs do
            do! sleepMs 20.0
            terminal <- fiber.IsCompleted() || fiber.IsInterrupted()

        return terminal, stopwatch.ElapsedMilliseconds
    }

let connectWhenListening (host: string) (port: int) =
    (SocketClient.connectWith host port)
        .Retry 60 (fun (_, _, _) -> FIO.sleep (TimeSpan.FromMilliseconds 50.0))

let withTestServer
    (handler: Socket -> FIO<unit, SocketError>)
    (action: int -> FIO<'A, SocketError>)
    (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port

            let! serverFiber =
                (fio {
                    let! socket = ServerSocket.accept server
                    do! handler socket
                    do! socket.Close()
                }).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, SocketError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = (ServerSocket.acceptLoop echoHandler server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect
