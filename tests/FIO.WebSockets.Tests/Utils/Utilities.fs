module FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.WebSockets
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling

open System
open System.Net

open Expecto
open FsCheck

module FsCheckProperties =

    type Generators =

        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new PollingRuntime() :> FIORuntime)
                    Gen.constant (new SignalingRuntime() :> FIORuntime)
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
        new PollingRuntime() :> FIORuntime
        new SignalingRuntime() :> FIORuntime
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
    let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

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

let private runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, WsError>) =
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 10_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let withTestServer (handler: WebSocket -> FIO<unit, WsError>) (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let effect =
        fio {
            let! listener = WebSocketServer.start url

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

let withTestEchoServer (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let closingEchoHandler (ws: WebSocket) =
        fio {
            do! echoHandler ws
            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
        }

    let effect =
        fio {
            let! listener = WebSocketServer.start url

            let! serverFiber =
                (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig closingEchoHandler).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime effect
