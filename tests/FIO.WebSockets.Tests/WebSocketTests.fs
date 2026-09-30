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
                    testAllRuntimes "SendText - roundtrips a text message through ReceiveMessage" (fun runtime ->
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

                    testAllRuntimes "SendBinary - roundtrips a binary message through ReceiveMessage" (fun runtime ->
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

                    testAllRuntimes "ReceiveMessage - reassembles messages larger than the receive buffer" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"

                                    for size in [ 4_095; 4_096; 4_097; 16_384; 100_000 ] do
                                        let text = String(Array.init size (fun i -> char (int 'a' + i % 26)))
                                        do! ws.SendText text
                                        let! textMsg = ws.ReceiveMessage()

                                        match textMsg with
                                        | Frame(Text s) -> Expect.equal s text $"Text of {size} bytes"
                                        | other -> failtest $"Expected text frame but got {other}"

                                        let data = Array.init size (fun i -> byte (i % 251))
                                        do! ws.SendBinary data
                                        let! binaryMsg = ws.ReceiveMessage()

                                        match binaryMsg with
                                        | Frame(Binary b) -> Expect.equal b data $"Binary of {size} bytes"
                                        | other -> failtest $"Expected binary frame but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "ReceiveMessage - fails with MessageTooLarge past MaxMessageSize" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                    let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                    do! ws.SendText(String('x', 16_384))
                                    let! result = ws.ReceiveMessage().Result()

                                    match result with
                                    | Error(MessageTooLarge(actual, max)) ->
                                        Expect.equal max 10_000L "The configured limit"
                                        Expect.isGreaterThan actual 10_000L "The size at which the limit was crossed"
                                    | other -> failtest $"Expected MessageTooLarge but got {other}"

                                    do! ws.Abort().Ignore()
                                })
                            runtime)

                    testAllRuntimes "ReceiveMessage - past MaxMessageSize, aborts the connection rather than returning the rest as a message" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                    let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                    do! ws.SendText(String('x', 16_384))
                                    let! first = ws.ReceiveMessage().Result()
                                    let! second = ws.ReceiveMessage().Result()

                                    match first with
                                    | Error(MessageTooLarge _) -> ()
                                    | other -> failtest $"Expected MessageTooLarge but got {other}"

                                    match second with
                                    | Error(Closed _) -> ()
                                    | other -> failtest $"Expected a closed connection after an oversized message, but the next receive gave {other}"
                                })
                            runtime)

                    testAllRuntimes "Send - roundtrips through Receive with the text codec" (fun runtime ->
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

                    testAllRuntimes "Send - roundtrips through Receive with the json codec" (fun runtime ->
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

                    testAllRuntimes "SendFrame - sends a Text frame" (fun runtime ->
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
                    testAllRuntimes "Close - transitions the state to closed" (fun runtime ->
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

                    testAllRuntimes "Abort - terminates the connection" (fun runtime ->
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
                    testAllRuntimes "CloseOutput - half-closes without waiting for the peer" (fun runtime ->
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

                    testAllRuntimes "CloseOutput - accepts an explicit status and description" (fun runtime ->
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

                    testAllRuntimes "Abort - tears the connection down immediately" (fun runtime ->
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
                    testAllRuntimes "State - returns Open for a connected socket" (fun runtime ->
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

                    testAllRuntimes "CloseStatus - returns None for an open socket" (fun runtime ->
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
                    testAllRuntimes "State - reports an open connection, then a closed one" (fun runtime ->
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

                    testAllRuntimes "CloseStatus - is absent while open and present after closing" (fun runtime ->
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

                    testAllRuntimes "CloseStatusDescription/Subprotocol - are readable" (fun runtime ->
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
                    testAllRuntimes "Receive - with a codec fails with Closed when the peer closes" (fun runtime ->
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

                    testAllRuntimes "ReceiveMessage - on a closed socket fails with Closed" (fun runtime ->
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

                    testAllRuntimes "SendText - on a closed socket fails with Closed" (fun runtime ->
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

                    testAllRuntimes "SendText - after the peer's close frame fails with Closed" (fun runtime ->
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

                    testAllRuntimes "Close - against a peer that never reads, times out and leaves the socket aborted" (fun runtime ->
                        withTestServer
                            (fun _ -> FIO.never ())
                            (fun port ->
                                fio {
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 500
                                    let! cancellationToken = FIO.cancellationToken ()
                                    let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config cancellationToken
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

                    testAllRuntimes "ReceiveMessage - on an aborted socket fails with Closed" (fun runtime ->
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

            testList
                "TryReceive"
                [
                    testAllRuntimes "TryReceive - yields Received for a decodable message" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.SendText "hello"
                                    let! outcome = ws.TryReceive Codec.text

                                    Expect.equal outcome (Received "hello") "A decodable message must be Received"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "TryReceive - yields Undecodable for a frame the codec rejects" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.SendText "not json"
                                    let! outcome = ws.TryReceive Codec.json<TestMessage>

                                    match outcome with
                                    | Undecodable _ -> ()
                                    | other -> failtest $"Expected Undecodable but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "TryReceive - yields PeerClosed when the peer closes" (fun runtime ->
                        withTestServer
                            (fun ws -> ws.Close())
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! outcome = ws.TryReceive Codec.text

                                    match outcome with
                                    | PeerClosed _ -> ()
                                    | other -> failtest $"Expected PeerClosed but got {other}"

                                    do! ws.CloseIfOpen()
                                })
                            runtime)

                    testAllRuntimes "TryReceive - still fails with any other error" (fun runtime ->
                        withTestServer
                            echoHandler
                            (fun port ->
                                fio {
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                    let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                    do! ws.SendText(String('x', 16_384))
                                    let! result = (ws.TryReceive Codec.text).Result()

                                    match result with
                                    | Error(MessageTooLarge _) -> ()
                                    | other -> failtest $"Expected MessageTooLarge but got {other}"

                                    do! ws.Abort().Ignore()
                                })
                            runtime)
                ]

            testList
                "CloseIfOpen"
                [
                    testAllRuntimes "CloseIfOpen - closes an open connection" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.CloseIfOpen()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Closed "An open connection must end Closed"
                                })
                            runtime)

                    testAllRuntimes "CloseIfOpen - answers the peer's close" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    do! ws.CloseOutput()
                                    do! ws.ReceiveMessage().Unit().CatchAll(fun _ -> FIO.unit ())
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! _ = ws.ReceiveMessage()
                                    do! ws.CloseIfOpen()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Closed "A received close must be answered"
                                })
                            runtime)

                    testAllRuntimes "CloseIfOpen - succeeds on a connection already closed" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Close()
                                    do! ws.CloseIfOpen()
                                })
                            runtime)

                    testAllRuntimes "CloseIfOpen - leaves an aborted connection aborted" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.Abort()
                                    do! ws.CloseIfOpen()
                                    let! state = ws.State()

                                    Expect.equal state WebSocketState.Aborted "An aborted connection cannot be closed"
                                })
                            runtime)
                ]
        ]
