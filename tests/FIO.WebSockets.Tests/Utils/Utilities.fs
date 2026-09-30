module FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.WebSockets
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Net
open System.Net.Sockets
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

let private disposeRuntime (rt: FIORuntime) =
    match box rt with
    | :? IDisposable as d -> d.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
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

let findAvailablePort () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
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

let noopHandler (ws: WebSocket) =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame _ -> return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

let echoHandler (ws: WebSocket) =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame frame ->
                do! ws.SendFrame frame
                return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, WsError>) =
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 30_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let startTestListenerOn (host: string) =
    let rec attempt remaining =
        fio {
            let port = findAvailablePort ()
            let url = $"http://{host}:{port}/"

            let! outcome =
                (WebSocketServer.start url)
                    .Map(fun listener -> Ok(port, listener))
                    .CatchAll(fun error -> FIO.succeed (Error error))

            match outcome with
            | Ok result -> return result
            | Error error ->
                if remaining > 0 then
                    do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                    return! attempt (remaining - 1)
                else
                    return! FIO.fail error
        }

    attempt 10

let startTestListener () =
    startTestListenerOn "localhost"

let private portConflict (error: WsError) =
    match error with
    | ConnectionFailed message -> message.Contains "conflicts with an existing registration"
    | _ -> false

let withServedUrl (startServer: string -> FIO<unit, WsError>) (action: string -> FIO<'A, WsError>) =
    let rec attempt remaining =
        fio {
            let port = findAvailablePort ()
            let! serverFiber = (startServer $"http://localhost:{port}/").Fork()

            let serverDied: FIO<'A, WsError> =
                (serverFiber.Join())
                    .FlatMap(fun _ -> FIO.fail (ConnectionFailed "server exited before the client connected"))

            let! outcome =
                ((action $"ws://localhost:{port}/").RaceFirst serverDied)
                    .Map(fun value -> Ok value)
                    .CatchAll(fun error -> FIO.succeed (Error error))

            do! (serverFiber.InterruptNow()).CatchAll(fun _ -> FIO.unit ())

            match outcome with
            | Ok value -> return value
            | Error error when remaining > 0 && portConflict error -> return! attempt (remaining - 1)
            | Error error -> return! FIO.fail error
        }

    attempt 5

let connectWhenListening (url: string) =
    (WebSocketClient.connectDefault url)
        .Retry 60 (fun (_, _, _) -> FIO.sleep (TimeSpan.FromMilliseconds 50.0))

let withTestServer (handler: WebSocket -> FIO<unit, WsError>) (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! port, listener = startTestListener ()

            let! serverFiber =
                (fio {
                    let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                    do! handler ws
                    do! ws.Close()
                }).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServerOn (host: string) (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let closingEchoHandler (ws: WebSocket) =
        fio {
            do! echoHandler ws
            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
        }

    let effect =
        fio {
            let! port, listener = startTestListenerOn host

            let! serverFiber =
                (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig closingEchoHandler).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    withTestEchoServerOn "localhost" action runtime
