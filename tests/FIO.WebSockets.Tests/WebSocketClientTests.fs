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

                    testAllRuntimes "connect yields a socket without endpoints" (fun runtime ->
                        withTestServer
                            noopHandler
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"

                                    Expect.isNone ws.RemoteEndPoint "A client socket does not know a resolved remote endpoint"
                                    Expect.isNone ws.LocalEndPoint "A client socket does not know its local endpoint"

                                    do! ws.Close()
                                })
                            runtime)
                ]

            testList
                "Scoped lifetime"
                [
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

                    // Sequenced: it captures the process-global stderr, which a parallel test's log line could pollute.
                    testSequenced (
                        testAllRuntimes "withConnection stays quiet when an interrupted receive aborted the socket" (fun runtime ->
                            let originalErr = Console.Error
                            use captured = new IO.StringWriter()
                            Console.SetError captured

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

                                            Expect.equal winner "quit" "The immediate effect should win the race"
                                        })
                                    runtime
                            finally
                                Console.SetError originalErr

                            Expect.equal (captured.ToString()) "" "Releasing an aborted socket must not log to stderr"))

                    testAllRuntimes "withConnection release is bounded by SendTimeout when the peer never answers the close" (fun runtime ->
                        withTestServer
                            (fun _ -> FIO.never ())
                            (fun port ->
                                fio {
                                    let uri = Uri $"ws://localhost:{port}/"
                                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 500
                                    let clock = Diagnostics.Stopwatch.StartNew()

                                    do! WebSocketClient.withConnection uri config (fun _ -> FIO.unit ())

                                    Expect.isLessThan
                                        clock.Elapsed.TotalSeconds
                                        10.0
                                        "A peer that never reads must not hold the release beyond the send timeout"
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
        ]
