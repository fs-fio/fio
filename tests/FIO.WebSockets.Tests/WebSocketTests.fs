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
                        let msg =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendText "hello text"
                                        let! msg = ws.ReceiveMessage()
                                        do! ws.Close()
                                        return msg
                                    })
                                runtime

                        match msg with
                        | Frame(Text s) -> Expect.equal s "hello text" "Text roundtrip"
                        | other -> failtest $"Expected text frame but got {other}")

                    testAllRuntimes "SendBinary - roundtrips a binary message through ReceiveMessage" (fun runtime ->
                        let data = [| 10uy; 20uy; 30uy; 40uy |]

                        let msg =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendBinary data
                                        let! msg = ws.ReceiveMessage()
                                        do! ws.Close()
                                        return msg
                                    })
                                runtime

                        match msg with
                        | Frame(Binary b) -> Expect.equal b data "Binary roundtrip"
                        | other -> failtest $"Expected binary frame but got {other}")

                    testAllRuntimes "ReceiveMessage - reassembles messages larger than the receive buffer" (fun runtime ->
                        let results =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! results =
                                            FIO.forEach [ 4_095; 4_096; 4_097; 16_384; 100_000 ] (fun size ->
                                                fio {
                                                    let text = String(Array.init size (fun i -> char (int 'a' + i % 26)))
                                                    do! ws.SendText text
                                                    let! textMsg = ws.ReceiveMessage()
                                                    let data = Array.init size (fun i -> byte (i % 251))
                                                    do! ws.SendBinary data
                                                    let! binaryMsg = ws.ReceiveMessage()
                                                    return size, text, textMsg, data, binaryMsg
                                                })
                                        do! ws.Close()
                                        return results
                                    })
                                runtime

                        for size, text, textMsg, data, binaryMsg in results do
                            match textMsg with
                            | Frame(Text s) -> Expect.equal s text $"Text of {size} bytes"
                            | other -> failtest $"Expected text frame but got {other}"
                            match binaryMsg with
                            | Frame(Binary b) -> Expect.equal b data $"Binary of {size} bytes"
                            | other -> failtest $"Expected binary frame but got {other}")

                    testAllRuntimes "ReceiveMessage - fails with MessageTooLarge past MaxMessageSize" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                        let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                        do! ws.SendText(String('x', 16_384))
                                        let! result = ws.ReceiveMessage().Result()
                                        do! ws.Abort().Ignore()
                                        return result
                                    })
                                runtime

                        match result with
                        | Error(MessageTooLarge(actual, max)) ->
                            Expect.equal max 10_000L "The configured limit"
                            Expect.isGreaterThan actual 10_000L "The size at which the limit was crossed"
                        | other -> failtest $"Expected MessageTooLarge but got {other}")

                    testAllRuntimes "ReceiveMessage - past MaxMessageSize, aborts the connection rather than returning the rest as a message" (fun runtime ->
                        let first, second =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                        let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                        do! ws.SendText(String('x', 16_384))
                                        let! first = ws.ReceiveMessage().Result()
                                        let! second = ws.ReceiveMessage().Result()
                                        return first, second
                                    })
                                runtime

                        match first with
                        | Error(MessageTooLarge _) -> ()
                        | other -> failtest $"Expected MessageTooLarge but got {other}"
                        match second with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected a closed connection after an oversized message, but the next receive gave {other}")

                    testAllRuntimes "Send - roundtrips through Receive with the text codec" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Send(Codec.text, "codec text")
                                        let! result = ws.Receive Codec.text
                                        do! ws.Close()
                                        return result
                                    })
                                runtime

                        Expect.equal result "codec text" "Text codec roundtrip")

                    testAllRuntimes "Send - roundtrips through Receive with the json codec" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let msg = { Id = 42; Text = "json ws" }
                                        do! ws.Send(Codec.json, msg)
                                        let! result = ws.Receive Codec.json
                                        do! ws.Close()
                                        return result
                                    })
                                runtime

                        Expect.equal result.Id 42 "Id should match"
                        Expect.equal result.Text "json ws" "Text should match")

                    testAllRuntimes "SendFrame - sends a Text frame" (fun runtime ->
                        let msg =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendFrame(Text "frame send")
                                        let! msg = ws.ReceiveMessage()
                                        do! ws.Close()
                                        return msg
                                    })
                                runtime

                        match msg with
                        | Frame(Text s) -> Expect.equal s "frame send" "SendFrame text"
                        | other -> failtest $"Expected text frame but got {other}")
                ]

            testList
                "Close / Abort"
                [
                    testAllRuntimes "Close - transitions the state to closed" (fun runtime ->
                        let state =
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
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Closed "Should be Closed after Close()")

                    testAllRuntimes "SendFrame - a Close frame answers the peer's close" (fun runtime ->
                        let first, state =
                            withTestServer
                                (fun ws ->
                                    fio {
                                        do! ws.CloseOutput()
                                        let! _ = ws.ReceiveMessage()
                                        return ()
                                    })
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! first = ws.ReceiveMessage()
                                        do! ws.SendFrame(Close(WebSocketCloseStatus.NormalClosure, "bye"))
                                        let! state = ws.State()
                                        return first, state
                                    })
                                runtime

                        match first with
                        | ConnectionClosed _ -> ()
                        | other -> failtest $"Expected the peer's close first, got {other}"
                        Expect.equal state WebSocketState.Closed "Answering the peer's close should complete the handshake")

                    testAllRuntimes "Abort - terminates the connection" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Abort()
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Aborted "Should be Aborted")

                    testAllRuntimes "Dispose - releases the connection, after which a send fails with Closed" (fun runtime ->
                        let outcome =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Dispose()
                                        let! outcome =
                                            ws.SendText("after dispose").Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Some(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")
                ]

            testList
                "CloseOutput and Abort"
                [
                    testAllRuntimes "CloseOutput - half-closes without waiting for the peer" (fun runtime ->
                        let state =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.CloseOutput()
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.notEqual state WebSocketState.Open "CloseOutput must leave the socket no longer open")

                    testAllRuntimes "CloseOutput - accepts an explicit status and description" (fun runtime ->
                        let state =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.CloseOutput(WebSocketCloseStatus.NormalClosure, "done here")
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.notEqual state WebSocketState.Open "An explicit CloseOutput must also close the output")

                    testAllRuntimes "Abort - tears the connection down immediately" (fun runtime ->
                        let state =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Abort()
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Aborted "Abort must leave the socket in the Aborted state")
                ]

            testList
                "Connection state"
                [
                    testAllRuntimes "State - returns Open for a connected socket" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! state = ws.State()
                                        do! ws.Close()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Open "Should be Open")

                    testAllRuntimes "CloseStatus - returns None for an open socket" (fun runtime ->
                        let status =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! status = ws.CloseStatus()
                                        do! ws.Close()
                                        return status
                                    })
                                runtime

                        Expect.isNone status "CloseStatus should be None for open socket")
                ]

            testList
                "Connection state accessors"
                [
                    testAllRuntimes "State - reports an open connection, then a closed one" (fun runtime ->
                        let openState, closedState =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! openState = ws.State()
                                        do! ws.Close()
                                        let! closedState = ws.State()
                                        return openState, closedState
                                    })
                                runtime

                        Expect.equal openState WebSocketState.Open "A fresh connection must report Open"
                        Expect.notEqual closedState WebSocketState.Open "After Close it must not report Open")

                    testAllRuntimes "CloseStatus - is absent while open and present after closing" (fun runtime ->
                        let whileOpen, afterClose =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! whileOpen = ws.CloseStatus()
                                        do! ws.Close()
                                        let! afterClose = ws.CloseStatus()
                                        return whileOpen, afterClose
                                    })
                                runtime

                        Expect.isNone whileOpen "An open connection has no close status"
                        Expect.isSome afterClose "A closed connection must report a close status")

                    testAllRuntimes "CloseStatusDescription/Subprotocol - are readable" (fun runtime ->
                        let subprotocol, description =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! subprotocol = ws.Subprotocol()
                                        do! ws.Close()
                                        let! description = ws.CloseStatusDescription()
                                        return subprotocol, description
                                    })
                                runtime

                        Expect.isTrue
                            (String.IsNullOrEmpty subprotocol)
                            $"No subprotocol was negotiated, so it must be empty but was '{subprotocol}'"
                        Expect.equal
                            description
                            "Normal closure"
                            "Close sends its own status description, which must be readable afterwards")
                ]

            testList
                "Error classification"
                [
                    testAllRuntimes "Receive - with a codec fails with Closed when the peer closes" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun ws -> ws.Close())
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! outcome = (ws.Receive Codec.text).Result()
                                        do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "ReceiveMessage - on a closed socket fails with Closed" (fun runtime ->
                        let outcome =
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
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "SendText - on a closed socket fails with Closed" (fun runtime ->
                        let outcome =
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
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "SendText - after the peer's close frame fails with Closed" (fun runtime ->
                        let first, outcome =
                            withTestServer
                                (fun ws ->
                                    fio {
                                        do! ws.CloseOutput()
                                        do! ws.ReceiveMessage().Unit().CatchAll(fun _ -> FIO.unit ())
                                    })
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! first = ws.ReceiveMessage()
                                        let! outcome = (ws.SendText "reply after the peer closed").Result()
                                        do! ws.Close()
                                        return first, outcome
                                    })
                                runtime

                        match first with
                        | ConnectionClosed _ -> ()
                        | other -> failtest $"Expected the peer's close frame but got {other}"
                        match outcome with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "Close - against a peer that never reads, times out and leaves the socket aborted" (fun runtime ->
                        let outcome, state =
                            withTestServer
                                (fun _ -> FIO.never ())
                                (fun port ->
                                    fio {
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 500
                                        let! cancellationToken = FIO.cancellationToken ()
                                        let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config cancellationToken
                                        let! outcome = ws.Close().Result()
                                        let! state = ws.State()
                                        return outcome, state
                                    })
                                runtime

                        match outcome with
                        | Error(TimeoutError _) -> ()
                        | other -> failtest $"Expected TimeoutError but got {other}"
                        Expect.equal
                            state
                            WebSocketState.Aborted
                            "A timed-out close must leave the socket aborted; if this fails, CloseIfOpen needs an Abort fallback")

                    testAllRuntimes "SendBinary - times out with TimeoutError against a peer that never reads" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun _ -> FIO.never ())
                                (fun port ->
                                    fio {
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 200
                                        let! ws =
                                            WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                        let chunk = Array.zeroCreate<byte> (1024 * 1024)
                                        let rec flood (sent: int) =
                                            if sent >= 64 then
                                                FIO.succeed None
                                            else
                                                (ws.SendBinary chunk)
                                                    .Map(fun () -> Ok())
                                                    .CatchAll(fun error -> FIO.succeed (Error error))
                                                    .FlatMap(function
                                                        | Ok() -> flood (sent + 1)
                                                        | Error error -> FIO.succeed (Some error))
                                        let! outcome = flood 0
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Some(TimeoutError _) -> ()
                        | other -> failtest $"Expected TimeoutError within 64 one-megabyte messages to a peer that never reads (loopback buffers absorb a few megabytes), got %A{other}")

                    testAllRuntimes "ReceiveMessage - on an aborted socket fails with Closed" (fun runtime ->
                        let outcome =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Abort()
                                        let! outcome = (ws.ReceiveMessage()).Result()
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed _) -> ()
                        | other -> failtest $"Expected Closed but got {other}")
                ]

            testList
                "TryReceive"
                [
                    testAllRuntimes "TryReceive - yields Received for a decodable message" (fun runtime ->
                        let outcome =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendText "hello"
                                        let! outcome = ws.TryReceive Codec.text
                                        do! ws.Close()
                                        return outcome
                                    })
                                runtime

                        Expect.equal outcome (Received "hello") "A decodable message must be Received")

                    testAllRuntimes "TryReceive - yields Undecodable for a frame the codec rejects" (fun runtime ->
                        let outcome =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendText "not json"
                                        let! outcome = ws.TryReceive Codec.json<TestMessage>
                                        do! ws.Close()
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Undecodable _ -> ()
                        | other -> failtest $"Expected Undecodable but got {other}")

                    testAllRuntimes "TryReceive - yields PeerClosed when the peer closes" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun ws -> ws.Close())
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! outcome = ws.TryReceive Codec.text
                                        do! ws.CloseIfOpen()
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | PeerClosed _ -> ()
                        | other -> failtest $"Expected PeerClosed but got {other}")

                    testAllRuntimes "TryReceive - still fails with any other error" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxMessageSize 10_000L
                                        let! ws = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config Threading.CancellationToken.None
                                        do! ws.SendText(String('x', 16_384))
                                        let! result = (ws.TryReceive Codec.text).Result()
                                        do! ws.Abort().Ignore()
                                        return result
                                    })
                                runtime

                        match result with
                        | Error(MessageTooLarge _) -> ()
                        | other -> failtest $"Expected MessageTooLarge but got {other}")
                ]

            testList
                "CloseIfOpen"
                [
                    testAllRuntimes "CloseIfOpen - closes an open connection" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.CloseIfOpen()
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Closed "An open connection must end Closed")

                    testAllRuntimes "CloseIfOpen - answers the peer's close" (fun runtime ->
                        let state =
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
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Closed "A received close must be answered")

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
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.Abort()
                                        do! ws.CloseIfOpen()
                                        let! state = ws.State()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Aborted "An aborted connection cannot be closed")

                    testSequenced (
                        testAllRuntimes "CloseIfOpen - stays quiet when the peer drops the connection instead of answering the close" (fun runtime ->
                            let captured = new IO.StringWriter()
                            let original = Console.Error
                            Console.SetError captured
                            let effect =
                                fio {
                                    let! port, listener = startTestListener ()
                                    let! server =
                                        (fio {
                                            let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                                            let! _ = ws.ReceiveMessage()
                                            do! WebSocketServer.abort listener
                                        }).Fork()
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    do! ws.CloseOutput()
                                    do! ws.CloseIfOpen()
                                    do! server.Await().Unit()
                                }

                            try
                                runWithTimeout runtime effect
                            finally
                                Console.SetError original

                            Expect.equal
                                (captured.ToString())
                                ""
                                "A peer that dropped the connection is not an error worth reporting")
                    )
                ]
        ]
