module FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

open System
open System.Net

open Expecto
open FsCheck

open FIO.WebSockets

module FsCheckProperties =

    type Generators =
        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new CooperativeRuntime() :> FIORuntime)
                    Gen.constant (new ConcurrentRuntime() :> FIORuntime)
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
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
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

let noopHandler (ws: WebSocket) : FIO<unit, WsError> =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame _ -> return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

let echoHandler (ws: WebSocket) : FIO<unit, WsError> =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame frame ->
                do! ws.SendFrame frame
                return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

let private runWithTimeout (runtime: FIORuntime) (eff: FIO<'R, WsError>) : 'R =
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let withTestServer (handler: WebSocket -> FIO<unit, WsError>) (action: int -> FIO<'R, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let eff =
        fio {
            let! listener = WebSocketServer.start url

            let! serverFiber =
                (fio {
                    let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                    do! handler ws
                    do! ws.Close()
                })
                    .Fork()

            let! result = action port
            do! serverFiber.Interrupt()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime eff

let withTestEchoServer (action: int -> FIO<'R, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let closingEchoHandler (ws: WebSocket) =
        fio {
            do! echoHandler ws
            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
        }

    let eff =
        fio {
            let! listener = WebSocketServer.start url

            let! serverFiber =
                (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig closingEchoHandler).Fork()

            let! result = action port
            do! serverFiber.Interrupt()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime eff
