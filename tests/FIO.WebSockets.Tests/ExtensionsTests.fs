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
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                            let msg = { Id = 1; Text = "json ext" }
                            do! ws.SendJson msg
                            let! received = ws.ReceiveJson()

                            Expect.equal received.Id 1 "Id should match"
                            Expect.equal received.Text "json ext" "Text should match"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "SendJson - roundtrips through ReceiveJson with custom options" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                            let msg = { Id = 2; Text = "custom opts" }
                            do! ws.SendJson(msg, options)
                            let! received = ws.ReceiveJson options

                            Expect.equal received.Id 2 "Id should match"
                            Expect.equal received.Text "custom opts" "Text should match"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "SendString - roundtrips through ReceiveString" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                            do! ws.SendString "hello string ext"
                            let! received = ws.ReceiveString()

                            Expect.equal received "hello string ext" "String roundtrip"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "SendBytes - roundtrips through ReceiveBytes" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                            let data = [| 5uy; 10uy; 15uy; 20uy |]
                            do! ws.SendBytes data
                            let! received = ws.ReceiveBytes()

                            Expect.equal received data "Bytes roundtrip"

                            do! ws.Close()
                        })
                    runtime)

            testList
                "Frame-type mismatches"
                [
                    testAllRuntimes "ReceiveString - rejects a binary frame" (fun runtime ->
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

                                    match outcome with
                                    | Error error ->
                                        Expect.stringContains
                                            (error.ToString())
                                            "got binary"
                                            "ReceiveString must say the frame was binary"
                                    | Ok text -> failtest $"Expected a typed failure but received {text}"
                                })
                            runtime)

                    testAllRuntimes "ReceiveBytes - rejects a text frame" (fun runtime ->
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

                                    match outcome with
                                    | Error error ->
                                        Expect.stringContains
                                            (error.ToString())
                                            "got text"
                                            "ReceiveBytes must say the frame was text"
                                    | Ok data -> failtest $"Expected a typed failure but received {data.Length} bytes"
                                })
                            runtime)

                    testAllRuntimes "ReceiveJson - fails when the connection closes first" (fun runtime ->
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

                                    match outcome with
                                    | Error _ -> ()
                                    | Ok value -> failtest $"Expected a failure on a closed connection, got {value}"
                                })
                            runtime)
                ]
        ]
