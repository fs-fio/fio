module FIO.WebSockets.Tests.Utilities

open FIO.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.WebSockets

open Expecto

open System
open System.Net
open System.Threading.Tasks

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

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, WsError>) = runWithin 30.0 runtime effect

let stressTestAllRuntimes name (f: FIORuntime -> unit) =
    testSequenced (FIO.Tests.Utilities.stressTestAllRuntimes name f)

let startListenerWith (start: string -> FIO<HttpListener, WsError>) (host: string) =
    let rec attempt remaining =
        fio {
            let port = freePort ()
            let url = $"http://{host}:{port}/"

            let! outcome =
                (start url).Map(fun listener -> Ok(port, listener)).CatchAll(fun error -> FIO.succeed (Error error))

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

let startTestListenerOn (host: string) =
    startListenerWith WebSocketServer.start host

let startTestListener () = startTestListenerOn "localhost"

// A client connecting while the managed HttpListener starts can fail Start and hold the port (dotnet/runtime#28658).
let private retryOnNewPort (error: WsError) =
    match error with
    | ConnectionFailed "Value cannot be null." -> true
    | ConnectionFailed message ->
        [
            "conflicts with an existing registration"
            "already in use"
            "being used by another process"
        ]
        |> List.exists message.Contains
    | _ -> false

type ServedServer =
    {
        Server: Fiber<unit, WsError>
        Url: string
        Unwound: Task
    }

// An interrupted fiber publishes before its finalizers run, so only Unwound tells that the server has shut down.
let withServerOn (host: string) (startServer: string -> FIO<unit, WsError>) (action: ServedServer -> FIO<'A, WsError>) =
    let rec attempt remaining =
        fio {
            let port = freePort ()
            let unwound = TaskCompletionSource()

            let! server =
                (startServer $"http://{host}:{port}/")
                    .Ensuring(FIO.succeedWith (fun () -> unwound.TrySetResult() |> ignore))
                    .Fork()

            let serverDied =
                server
                    .Await()
                    .FlatMap(fun result ->
                        match result with
                        | Succeeded() -> FIO.fail (ConnectionFailed "server exited before the test was done with it")
                        | Failed error -> FIO.fail error
                        | Interrupted _ -> FIO.never ())

            let! outcome =
                ((action
                    {
                        Server = server
                        Url = $"ws://{host}:{port}/"
                        Unwound = unwound.Task
                    })
                    .RaceFirst
                    serverDied)
                    .Map(fun value -> Ok value)
                    .CatchAll(fun error -> FIO.succeed (Error error))

            do! (server.InterruptNow()).CatchAll(fun _ -> FIO.unit ())

            match outcome with
            | Ok value -> return value
            | Error error when remaining > 0 && retryOnNewPort error -> return! attempt (remaining - 1)
            | Error error -> return! FIO.fail error
        }

    attempt 5

let withServedUrl (startServer: string -> FIO<unit, WsError>) (action: string -> FIO<'A, WsError>) =
    withServerOn "localhost" startServer (fun served -> action served.Url)

let connectWhenListening (url: string) =
    (WebSocketClient.connectDefault url).Retry 60 (fun (_, _, _) -> FIO.sleep (TimeSpan.FromMilliseconds 50.0))

let withTestServer (handler: WebSocket -> FIO<unit, WsError>) (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! port, listener = startTestListener ()

            let! serverFiber =
                (fio {
                    let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                    do! handler ws
                    do! ws.Close()
                })
                    .Fork()

            let! result = action port
            do! serverFiber.InterruptNow()
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
            do! serverFiber.InterruptNow()
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    withTestEchoServerOn "localhost" action runtime
