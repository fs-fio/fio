module FIO.WebSockets.Tests.WebSocketTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Net.WebSockets

open Expecto

[<Tests>]
let webSocketTests =
    testList
        "WebSocket"
        [
            testList
                "Send / Receive"
                [
                    testAllRuntimes "SendText/ReceiveMessage text roundtrip" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.SendText "hello text"
                                    let! msg = ws.ReceiveMessage()

                                    match msg with
                                    | Frame(Text s) -> Expect.equal s "hello text" "Text roundtrip"
                                    | other -> failtest $"Expected text frame but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "SendBinary/ReceiveMessage binary roundtrip" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let data = [| 10uy; 20uy; 30uy; 40uy |]
                                    do! ws.SendBinary data
                                    let! msg = ws.ReceiveMessage()

                                    match msg with
                                    | Frame(Binary b) -> Expect.equal b data "Binary roundtrip"
                                    | other -> failtest $"Expected binary frame but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "Send/Receive with text codec roundtrip" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Send(Codec.text, "codec text")
                                    let! result = ws.Receive Codec.text

                                    Expect.equal result "codec text" "Text codec roundtrip"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "Send/Receive with json codec roundtrip" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let msg = { Id = 42; Text = "json ws" }
                                    do! ws.Send(Codec.json, msg)
                                    let! result = ws.Receive Codec.json

                                    Expect.equal result.Id 42 "Id should match"
                                    Expect.equal result.Text "json ws" "Text should match"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "SendFrame with Text frame" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.SendFrame(Text "frame send")
                                    let! msg = ws.ReceiveMessage()

                                    match msg with
                                    | Frame(Text s) -> Expect.equal s "frame send" "SendFrame text"
                                    | other -> failtest $"Expected text frame but got {other}"

                                    do! ws.Close()
                                })
                            runtime)
                ]

            testList
                "Close / Abort"
                [
                    testAllRuntimes "Close transitions state" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    match! ws.ReceiveMessage() with
                                    | ConnectionClosed _ -> ()
                                    | _ -> ()
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Close()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Closed "Should be Closed after Close()"
                                })
                            runtime)

                    testAllRuntimes "Abort terminates connection" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Abort()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Aborted "Should be Aborted"
                                })
                            runtime)
                ]

            testList
                "CloseOutput and Abort"
                [
                    testAllRuntimes "CloseOutput half-closes without waiting for the peer" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.CloseOutput()
                                    let! state = ws.State()

                                    Expect.notEqual state WebSocketState.Open "CloseOutput must leave the socket no longer open"
                                })
                            runtime)

                    testAllRuntimes "CloseOutput accepts an explicit status and description" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.CloseOutput(WebSocketCloseStatus.NormalClosure, "done here")
                                    let! state = ws.State()

                                    Expect.notEqual state WebSocketState.Open "An explicit CloseOutput must also close the output"
                                })
                            runtime)

                    testAllRuntimes "Abort tears the connection down immediately" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Abort()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Aborted "Abort must leave the socket in the Aborted state"
                                })
                            runtime)
                ]

            testList
                "Connection state"
                [
                    testAllRuntimes "State returns Open for connected socket" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Open "Should be Open"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "CloseStatus returns None for open socket" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! status = ws.CloseStatus()

                                    Expect.isNone status "CloseStatus should be None for open socket"

                                    do! ws.Close()
                                })
                            runtime)
                ]

            testList
                "Connection state accessors"
                [
                    testAllRuntimes "State reports an open connection, then a closed one" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! openState = ws.State()
                                    do! ws.Close()
                                    let! closedState = ws.State()

                                    Expect.equal openState WebSocketState.Open "A fresh connection must report Open"
                                    Expect.notEqual closedState WebSocketState.Open "After Close it must not report Open"
                                })
                            runtime)

                    testAllRuntimes "CloseStatus is absent while open and present after closing" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! whileOpen = ws.CloseStatus()
                                    Expect.isNone whileOpen "An open connection has no close status"

                                    do! ws.Close()
                                    let! afterClose = ws.CloseStatus()
                                    Expect.isSome afterClose "A closed connection must report a close status"
                                })
                            runtime)

                    testAllRuntimes "CloseStatusDescription and Subprotocol are readable" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! subprotocol = ws.Subprotocol()

                                    Expect.isTrue
                                        (String.IsNullOrEmpty subprotocol)
                                        $"No subprotocol was negotiated, so it must be empty but was '{subprotocol}'"

                                    do! ws.Close()
                                    let! description = ws.CloseStatusDescription()
                                    Expect.equal
                                        description
                                        "Normal closure"
                                        "Close sends its own status description, which must be readable afterwards"
                                })
                            runtime)
                ]

            testList
                "Error classification"
                [
                    testAllRuntimes "Receive with a codec fails with Closed when the peer closes" (fun runtime ->
                        withTestServer
                            (fun ws -> ws.Close())
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! outcome = (ws.Receive Codec.text).Result()

                                    match outcome with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected Closed but got {other}"

                                    do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                })
                            runtime)

                    testAllRuntimes "ReceiveMessage on a closed socket fails with Closed" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    match! ws.ReceiveMessage() with
                                    | ConnectionClosed _ -> ()
                                    | _ -> ()
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Close()
                                    let! outcome = (ws.ReceiveMessage()).Result()

                                    match outcome with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected Closed but got {other}"
                                })
                            runtime)

                    testAllRuntimes "SendText on a closed socket fails with Closed" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    match! ws.ReceiveMessage() with
                                    | ConnectionClosed _ -> ()
                                    | _ -> ()
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Close()
                                    let! outcome = (ws.SendText "too late").Result()

                                    match outcome with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected Closed but got {other}"
                                })
                            runtime)

                    testAllRuntimes "SendText after the peer's close frame fails with Closed" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    do! ws.CloseOutput()
                                    do! ws.ReceiveMessage().Unit().CatchAll(fun _ -> FIO.unit ())
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"

                                    match! ws.ReceiveMessage() with
                                    | ConnectionClosed _ -> ()
                                    | other -> failtest $"Expected the peer's close frame but got {other}"

                                    let! outcome = (ws.SendText "reply after the peer closed").Result()

                                    match outcome with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected Closed but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "Close against a peer that never reads times out and leaves the socket aborted" (fun runtime ->
                        withTestServer
                            (fun _ -> FIO.never ())
                            (fun port ->
                                fio {
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 500
                                    let! cancelToken = FIO.cancellationToken ()
                                    let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config cancelToken
                                    let! outcome = ws.Close().Result()

                                    match outcome with
                                    | Error(TimeoutError _) -> ()
                                    | other -> failtest $"Expected TimeoutError but got {other}"

                                    let! state = ws.State()

                                    Expect.equal
                                        state
                                        WebSocketState.Aborted
                                        "A timed-out close must leave the socket aborted; if this fails, CloseIfOpen needs an Abort fallback"
                                })
                            runtime)

                    testAllRuntimes "ReceiveMessage on an aborted socket fails with Closed" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Abort()
                                    let! outcome = (ws.ReceiveMessage()).Result()

                                    match outcome with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected Closed but got {other}"
                                })
                            runtime)
                ]
        ]
