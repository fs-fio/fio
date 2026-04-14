module FIO.WebSockets.Tests.WebSocketTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL

open System
open System.Net.WebSockets
open System.Text

open Expecto

open FIO.WebSockets

[<Tests>]
let webSocketTests =
    testList
        "WebSocket"
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
