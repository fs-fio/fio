module FIO.WebSockets.Tests.WebSocketClientTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL

open System
open System.Net.WebSockets
open System.Threading

open Expecto

open FIO.WebSockets

[<Tests>]
let webSocketClientTests =
    testList
        "WebSocketClient"
        [

            testAllRuntimes "connect with URI and config succeeds" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let uri = Uri $"ws://localhost:{port}/"

                            let! ws =
                                WebSocketClient.connect uri WebSocketConfig.defaultConfig CancellationToken.None

                            let! state = ws.State()

                            Expect.equal state WebSocketState.Open "Should be connected"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "connectWith convenience works" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let uri = Uri $"ws://localhost:{port}/"
                            let! ws = WebSocketClient.connectWith uri
                            let! state = ws.State()

                            Expect.equal state WebSocketState.Open "Should be connected"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "connectString/connectStringWith/connectDefault work" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let url = $"ws://localhost:{port}/"
                            let! ws = WebSocketClient.connectDefault url
                            let! state = ws.State()

                            Expect.equal state WebSocketState.Open "Should be connected"

                            do! ws.Close()
                        })
                    runtime)

            testAllRuntimes "connect fails for unreachable host" (fun runtime ->
                let eff =
                    fio {
                        return!
                            (WebSocketClient.connectDefault "ws://localhost:1/")
                                .Map(fun _ -> None)
                                .CatchAll(fun err -> FIO.succeed (Some err))
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(GeneralError _) -> ()
                | other -> failtest $"Expected GeneralError but got {other}")

            testAllRuntimes "withConnection auto-closes" (fun runtime ->
                withTestServer
                    (fun ws ->
                        fio {
                            match! ws.ReceiveMessage() with
                            | ConnectionClosed _ -> ()
                            | _ -> ()
                        })
                    (fun port ->
                        fio {
                            let uri = Uri $"ws://localhost:{port}/"

                            let! wasOpen =
                                WebSocketClient.withConnection uri WebSocketConfig.defaultConfig (fun ws ->
                                    fio {
                                        let! state = ws.State()
                                        return state = WebSocketState.Open
                                    })

                            Expect.isTrue wasOpen "Should have been open during action"
                        })
                    runtime)

            testAllRuntimes "withConnectionString auto-closes" (fun runtime ->
                withTestServer
                    (fun ws ->
                        fio {
                            match! ws.ReceiveMessage() with
                            | ConnectionClosed _ -> ()
                            | _ -> ()
                        })
                    (fun port ->
                        fio {
                            let! wasOpen =
                                WebSocketClient.withConnectionString $"ws://localhost:{port}/" (fun ws ->
                                    fio {
                                        let! state = ws.State()
                                        return state = WebSocketState.Open
                                    })

                            Expect.isTrue wasOpen "Should have been open during action"
                        })
                    runtime)
        ]
