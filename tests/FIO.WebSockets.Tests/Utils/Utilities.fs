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

open Expecto
open FsCheck.FSharp

module FsCheckProperties =

    type Generators =

        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new PollingRuntime() :> FIORuntime)
                    Gen.constant (new SignalingRuntime() :> FIORuntime)
                    Gen.constant (new WorkStealingRuntime() :> FIORuntime)
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
        new WorkStealingRuntime() :> FIORuntime
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
        |> fun async -> Async.RunSynchronously(async, timeout = 30_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let private startTestListener () =
    let rec attempt remaining =
        fio {
            let port = findAvailablePort ()
            let url = $"http://localhost:{port}/"

            let! outcome =
                (WebSocketServer.start url)
                    .Map(fun listener -> Ok(port, listener))
                    .CatchAll(fun error -> FIO.succeed (Error error))

            match outcome with
            | Ok result -> return result
            | Error error ->
                if remaining > 0 then
                    do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) WsError.fromException
                    return! attempt (remaining - 1)
                else
                    return! FIO.fail error
        }

    attempt 10

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

let withTestEchoServer (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let closingEchoHandler (ws: WebSocket) =
        fio {
            do! echoHandler ws
            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
        }

    let effect =
        fio {
            let! port, listener = startTestListener ()

            let! serverFiber =
                (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig closingEchoHandler).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime effect
