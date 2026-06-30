module FIO.Sockets.Tests.ExtensionsTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets
open FIO.Sockets.SocketExtensions

open System.Text.Json

open Expecto

[<Tests>]
let extensionsTests =
    testList
        "Extensions"
        [

            testAllRuntimes "SendJson/ReceiveJson roundtrip with custom options" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create "127.0.0.1" port
                            let! socket = SocketClient.connect config
                            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                            let msg = { Id = 1; Text = "custom json" }
                            do! socket.SendJson(msg, options)
                            let! received = socket.ReceiveJson(8192, options)

                            Expect.equal received.Id 1 "Id should match"
                            Expect.equal received.Text "custom json" "Text should match"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendJsonLine/ReceiveJsonLine roundtrip with custom options" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create "127.0.0.1" port
                            let! socket = SocketClient.connect config
                            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                            let msg = { Id = 2; Text = "custom json line" }
                            do! socket.SendJsonLine(msg, options)
                            let! received = socket.ReceiveJsonLine(8192, options)

                            Expect.equal received.Id 2 "Id should match"
                            Expect.equal received.Text "custom json line" "Text should match"

                            do! socket.Close()
                        })
                    runtime)
        ]
