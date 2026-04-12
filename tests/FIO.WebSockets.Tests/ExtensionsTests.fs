module FIO.WebSockets.Tests.ExtensionsTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System.Text.Json

open Expecto

[<Tests>]
let extensionsTests =
    testList "Extensions" [

        testAllRuntimes "SendJson/ReceiveJson roundtrip" (fun runtime ->
            withTestServer echoHandler (fun port ->
                fio {
                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                    let msg =
                        { Id = 1;
                          Text = "json ext" }
                    do! ws.SendJson msg
                    let! received = ws.ReceiveJson()

                    Expect.equal received.Id 1 "Id should match"
                    Expect.equal received.Text "json ext" "Text should match"

                    do! ws.Close()
                }) runtime)

        testAllRuntimes "SendJson/ReceiveJson with custom options" (fun runtime ->
            withTestServer echoHandler (fun port ->
                fio {
                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                    let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    let msg =
                        { Id = 2;
                          Text = "custom opts" }
                    do! ws.SendJson(msg, options)
                    let! received = ws.ReceiveJson options

                    Expect.equal received.Id 2 "Id should match"
                    Expect.equal received.Text "custom opts" "Text should match"

                    do! ws.Close()
                }) runtime)

        testAllRuntimes "SendString/ReceiveString roundtrip" (fun runtime ->
            withTestServer echoHandler (fun port ->
                fio {
                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                    do! ws.SendString "hello string ext"
                    let! received = ws.ReceiveString()

                    Expect.equal received "hello string ext" "String roundtrip"

                    do! ws.Close()
                }) runtime)

        testAllRuntimes "SendBytes/ReceiveBytes roundtrip" (fun runtime ->
            withTestServer echoHandler (fun port ->
                fio {
                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                    let data = [| 5uy; 10uy; 15uy; 20uy |]
                    do! ws.SendBytes data
                    let! received = ws.ReceiveBytes()

                    Expect.equal received data "Bytes roundtrip"

                    do! ws.Close()
                }) runtime)
    ]
