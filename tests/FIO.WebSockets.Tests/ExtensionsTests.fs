module FIO.WebSockets.Tests.ExtensionsTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets
open FIO.WebSockets.WebSocketExtensions

open System.Text.Json

open Expecto

[<Tests>]
let extensionsTests =
    testList
        "Extensions"
        [
            testAllRuntimes "SendJson - roundtrips through ReceiveJson" (fun runtime ->
                let received: TestMessage =
                    withTestServer
                        echoHandler
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                let msg = { Id = 1; Text = "json ext" }
                                do! ws.SendJson msg
                                let! received = ws.ReceiveJson()
                                do! ws.Close()
                                return received
                            })
                        runtime

                Expect.equal received.Id 1 "Id should match"
                Expect.equal received.Text "json ext" "Text should match")

            testAllRuntimes "SendJson - roundtrips through ReceiveJson with custom options" (fun runtime ->
                let received: TestMessage =
                    withTestServer
                        echoHandler
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                                let msg = { Id = 2; Text = "custom opts" }
                                do! ws.SendJson(msg, options)
                                let! received = ws.ReceiveJson options
                                do! ws.Close()
                                return received
                            })
                        runtime

                Expect.equal received.Id 2 "Id should match"
                Expect.equal received.Text "custom opts" "Text should match")

            testAllRuntimes "SendString - roundtrips through ReceiveString" (fun runtime ->
                let received =
                    withTestServer
                        echoHandler
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! ws.SendString "hello string ext"
                                let! received = ws.ReceiveString()
                                do! ws.Close()
                                return received
                            })
                        runtime

                Expect.equal received "hello string ext" "String roundtrip")

            testAllRuntimes "SendBytes - roundtrips through ReceiveBytes" (fun runtime ->
                let data = [| 5uy; 10uy; 15uy; 20uy |]

                let received =
                    withTestServer
                        echoHandler
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! ws.SendBytes data
                                let! received = ws.ReceiveBytes()
                                do! ws.Close()
                                return received
                            })
                        runtime

                Expect.equal received data "Bytes roundtrip")

            testList
                "Frame-type mismatches"
                [
                    testAllRuntimes "ReceiveString - rejects a binary frame" (fun runtime ->
                        let outcome =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendBytes [| 1uy; 2uy; 3uy |]
                                        let! outcome =
                                            (ws.ReceiveString())
                                                .Map(fun text -> Ok text)
                                                .CatchAll(fun error -> FIO.succeed (Error error))
                                        do! (ws.Close()).CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error error ->
                            Expect.stringContains
                                (error.ToString())
                                "got binary"
                                "ReceiveString must say the frame was binary"
                        | Ok text -> failtest $"Expected a typed failure but received {text}")

                    testAllRuntimes "ReceiveString - fails with Closed when the peer closes" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun ws -> ws.Close())
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! outcome =
                                            (ws.ReceiveString())
                                                .Map(fun text -> Ok text)
                                                .CatchAll(fun error -> FIO.succeed (Error error))
                                        do! (ws.Close()).CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed message) ->
                            Expect.stringStarts message "Peer closed the connection" "ReceiveString must report the peer's close"
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "ReceiveBytes - rejects a text frame" (fun runtime ->
                        let outcome =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendString "not bytes"
                                        let! outcome =
                                            (ws.ReceiveBytes())
                                                .Map(fun data -> Ok data)
                                                .CatchAll(fun error -> FIO.succeed (Error error))
                                        do! (ws.Close()).CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error error ->
                            Expect.stringContains
                                (error.ToString())
                                "got text"
                                "ReceiveBytes must say the frame was text"
                        | Ok data -> failtest $"Expected a typed failure but received {data.Length} bytes")

                    testAllRuntimes "ReceiveBytes - fails with Closed when the peer closes" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun ws -> ws.Close())
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! outcome =
                                            (ws.ReceiveBytes())
                                                .Map(fun data -> Ok data)
                                                .CatchAll(fun error -> FIO.succeed (Error error))
                                        do! (ws.Close()).CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error(Closed message) ->
                            Expect.stringStarts message "Peer closed the connection" "ReceiveBytes must report the peer's close"
                        | other -> failtest $"Expected Closed but got {other}")

                    testAllRuntimes "ReceiveJson - fails when the connection closes first" (fun runtime ->
                        let outcome =
                            withTestServer
                                (fun ws -> ws.Close())
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! outcome =
                                            (ws.ReceiveJson<TestMessage>())
                                                .Map(fun value -> Ok value)
                                                .CatchAll(fun error -> FIO.succeed (Error error))
                                        do! (ws.Close()).CatchAll(fun _ -> FIO.unit ())
                                        return outcome
                                    })
                                runtime

                        match outcome with
                        | Error _ -> ()
                        | Ok value -> failtest $"Expected a failure on a closed connection, got {value}")
                ]
        ]
