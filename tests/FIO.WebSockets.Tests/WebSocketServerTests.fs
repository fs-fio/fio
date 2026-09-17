module FIO.WebSockets.Tests.WebSocketServerTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Net.Http

open Expecto

[<Tests>]
let webSocketServerTests =
    testList
        "WebSocketServer"
        [
            testList
                "Lifecycle"
                [
                    testAllRuntimes "start creates listening server" (fun runtime ->
                        let port = findAvailablePort ()
                        let url = $"http://localhost:{port}/"

                        let effect =
                            fio {
                                let! listener = WebSocketServer.start url
                                do! WebSocketServer.close listener
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "close stops the listener" (fun runtime ->
                        let port = findAvailablePort ()
                        let url = $"http://localhost:{port}/"

                        let effect =
                            fio {
                                let! listener = WebSocketServer.start url
                                do! WebSocketServer.close listener
                                let! listener2 = WebSocketServer.start url
                                do! WebSocketServer.close listener2
                            }

                        runtime.Run(effect).UnsafeSuccess())
                ]

            testList
                "Accept"
                [
                    testAllRuntimes "accept yields the peer's endpoints" (fun runtime ->
                        withTestServer
                            (fun ws ->
                                fio {
                                    match ws.RemoteEndPoint, ws.LocalEndPoint with
                                    | Some(:? Net.IPEndPoint as remote), Some(:? Net.IPEndPoint as local) ->
                                        Expect.isTrue (Net.IPAddress.IsLoopback remote.Address) "The peer should be loopback"
                                        Expect.isTrue (Net.IPAddress.IsLoopback local.Address) "The local address should be loopback"
                                        Expect.notEqual remote.Port 0 "The peer's port should be known"
                                    | remote, local -> failtest $"Expected IP endpoints but got {remote} and {local}"

                                    do! ws.SendText "seen"
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! _ = ws.ReceiveMessage()
                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "accept receives client connection" (fun runtime ->
                        withTestServer
                            (fun ws -> fio { do! ws.SendText "from server" })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! msg = ws.ReceiveMessage()

                                    match msg with
                                    | Frame(Text s) -> Expect.equal s "from server" "Should receive server message"
                                    | other -> failtest $"Expected text frame but got {other}"

                                    do! ws.Close()
                                })
                            runtime)

                    testAllRuntimes "acceptLoop handles multiple connections" (fun runtime ->
                        withTestEchoServer
                            (fun port ->
                                FIO.forEachDiscard [ 1..3 ] (fun i ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let msg = $"msg{i}"
                                        do! ws.SendText msg
                                        let! received = ws.ReceiveMessage()

                                        match received with
                                        | Frame(Text s) -> Expect.equal s msg $"Echo {i} should match"
                                        | other -> failtest $"Expected text frame but got {other}"

                                        do! ws.Close()
                                    }))
                            runtime)

                    testAllRuntimes "startDefault is alias for start" (fun runtime ->
                        let port = findAvailablePort ()
                        let url = $"http://localhost:{port}/"

                        let effect =
                            fio {
                                let! listener = WebSocketServer.startDefault url
                                do! WebSocketServer.close listener
                            }

                        runtime.Run(effect).UnsafeSuccess())
                ]

            testList
                "Handshake rejection"
                [
                    testAllRuntimes "accept rejects a plain HTTP request with 400" (fun runtime ->
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()

                                let! acceptFiber =
                                    (WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig)
                                        .Map(fun _ -> "upgraded")
                                        .CatchAll(fun _ -> FIO.succeed "rejected")
                                        .Fork()

                                let! status =
                                    FIO.attempt
                                        (fun () ->
                                            use client = new HttpClient()
                                            let response = client.GetAsync($"http://localhost:{port}/").Result
                                            int response.StatusCode)
                                        WsError.fromException

                                let! outcome = acceptFiber.Join()
                                do! WebSocketServer.close listener
                                return status, outcome
                            }

                        let status, outcome = runWithTimeout runtime effect

                        Expect.equal status 400 "A non-WebSocket request must be answered with 400"
                        Expect.equal outcome "rejected" "accept must fail rather than yield a socket")
                ]

            testList
                "abort"
                [
                    testAllRuntimes "abort stops the listener immediately" (fun runtime ->
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()
                                do! WebSocketServer.abort listener

                                let! refused =
                                    FIO.attempt
                                        (fun () ->
                                            try
                                                use client = new HttpClient()
                                                client.Timeout <- TimeSpan.FromSeconds 2.0
                                                client.GetAsync($"http://localhost:{port}/").Result |> ignore
                                                false
                                            with _ -> true)
                                        WsError.fromException

                                return refused
                            }

                        Expect.isTrue (runWithTimeout runtime effect) "An aborted listener must stop serving")
                ]

            testList
                "serve / serveWith"
                [
                    testAllRuntimes "serve accepts a connection and runs the handler" (fun runtime ->
                        let received = ResizeArray<string>()
                        let handlerDone = Channel<unit>()

                        let handler (ws: WebSocket) =
                            fio {
                                let! message = ws.Receive Codec.text
                                do! FIO.attempt (fun () -> received.Add message) WsError.fromException
                                do! (handlerDone.Write ()).Unit()
                            }

                        let effect =
                            withServedUrl
                                (fun url -> WebSocketServer.serve url WebSocketConfig.defaultConfig handler)
                                (fun wsUrl ->
                                    fio {
                                        let! client = connectWhenListening wsUrl
                                        do! client.Send(Codec.text, "hello serve")
                                        do! (handlerDone.Read ()).Unit()
                                        do! client.Close()
                                        return ()
                                    })

                        runWithTimeout runtime effect |> ignore

                        Expect.sequenceEqual received [ "hello serve" ] "serve must deliver the message to its handler")

                    testAllRuntimes "serveWith runs a request/response protocol" (fun runtime ->
                        let respond (request: string) = FIO.succeed (request.ToUpperInvariant())

                        let effect =
                            withServedUrl
                                (fun url ->
                                    WebSocketServer.serveWith url WebSocketConfig.defaultConfig Codec.text Codec.text respond)
                                (fun wsUrl ->
                                    fio {
                                        let! client = connectWhenListening wsUrl
                                        do! client.Send(Codec.text, "shout")
                                        let! reply = client.Receive Codec.text
                                        do! client.Close()
                                        return reply
                                    })

                        Expect.equal
                            (runWithTimeout runtime effect)
                            "SHOUT"
                            "serveWith must decode the request and encode the handler's reply")
                ]
        ]
