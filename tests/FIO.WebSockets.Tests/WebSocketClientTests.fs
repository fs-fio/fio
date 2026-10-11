module FIO.WebSockets.Tests.WebSocketClientTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Threading
open System.Net.WebSockets

open Expecto

[<Tests>]
let webSocketClientTests =
    testList
        "WebSocketClient"
        [
            testList
                "Connect"
                [
                    testAllRuntimes "connect - succeeds with a URI and a configuration" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let uri = Uri $"ws://localhost:{port}/"
                                        let! ws =
                                            WebSocketClient.connect uri WebSocketConfig.defaultConfig CancellationToken.None
                                        let! state = ws.State()
                                        do! ws.Close()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Open "Should be connected")

                    testAllRuntimes "connectWith - connects to a listening server" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let uri = Uri $"ws://localhost:{port}/"
                                        let! ws = WebSocketClient.connectWith uri
                                        let! state = ws.State()
                                        do! ws.Close()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Open "Should be connected")

                    testAllRuntimes "connectString/connectStringWith/connectDefault - connect to a listening server" (fun runtime ->
                        let state =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let url = $"ws://localhost:{port}/"
                                        let! ws = WebSocketClient.connectDefault url
                                        let! state = ws.State()
                                        do! ws.Close()
                                        return state
                                    })
                                runtime

                        Expect.equal state WebSocketState.Open "Should be connected")

                    testAllRuntimes "connect - fails for an unreachable host" (fun runtime ->
                        let effect =
                            fio {
                                return!
                                    (WebSocketClient.connectDefault "ws://localhost:1/")
                                        .Map(fun _ -> None)
                                        .CatchAll(fun error -> FIO.succeed (Some error))
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Some(ConnectionFailed _) -> ()
                        | other -> failtest $"Expected ConnectionFailed but got {other}")

                    testAllRuntimes "connect - yields a socket without endpoints" (fun runtime ->
                        let remote, local =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let remote, local = ws.RemoteEndPoint, ws.LocalEndPoint
                                        do! ws.Close()
                                        return remote, local
                                    })
                                runtime

                        Expect.isNone remote "A client socket does not know a resolved remote endpoint"
                        Expect.isNone local "A client socket does not know its local endpoint")
                ]

            testList
                "Scoped lifetime"
                [
                    testAllRuntimes "withConnection - closes the connection when the scope ends" (fun runtime ->
                        let wasOpen =
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
                                        return wasOpen
                                    })
                                runtime

                        Expect.isTrue wasOpen "Should have been open during action")

                    testSequenced (
                        testAllRuntimes "withConnection - stays quiet when an interrupted receive aborted the socket" (fun runtime ->
                            let originalErr = Console.Error
                            use captured = new IO.StringWriter()
                            Console.SetError captured

                            let winner =
                                try
                                    withTestServer
                                        noopHandler
                                        (fun port ->
                                            fio {
                                                let uri = Uri $"ws://localhost:{port}/"
                                                let! winner =
                                                    WebSocketClient.withConnection uri WebSocketConfig.defaultConfig (fun ws ->
                                                        (ws.ReceiveMessage().Map(fun _ -> "receiver"))
                                                            .RaceFirst(FIO.succeed "quit"))
                                                return winner
                                            })
                                        runtime
                                finally
                                    Console.SetError originalErr

                            Expect.equal winner "quit" "The immediate effect should win the race"
                            Expect.equal (captured.ToString()) "" "Releasing an aborted socket must not log to stderr"))

                    testAllRuntimes "withConnection - its release is bounded by SendTimeout when the peer never answers the close" (fun runtime ->
                        let elapsedSeconds =
                            withTestServer
                                (fun _ -> FIO.never ())
                                (fun port ->
                                    fio {
                                        let uri = Uri $"ws://localhost:{port}/"
                                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 500
                                        let clock = Diagnostics.Stopwatch.StartNew()
                                        do! WebSocketClient.withConnection uri config (fun _ -> FIO.unit ())
                                        return clock.Elapsed.TotalSeconds
                                    })
                                runtime

                        Expect.isLessThan
                            elapsedSeconds
                            10.0
                            "A peer that never reads must not hold the release beyond the send timeout")

                    testAllRuntimes "withConnectionString - closes the connection when the scope ends" (fun runtime ->
                        let wasOpen =
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
                                        return wasOpen
                                    })
                                runtime

                        Expect.isTrue wasOpen "Should have been open during action")
                ]
        ]
