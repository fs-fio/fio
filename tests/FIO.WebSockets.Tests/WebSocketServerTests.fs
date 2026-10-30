module FIO.WebSockets.Tests.WebSocketServerTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Net.Http
open System.Threading

open Expecto

let private malformedUpgrade (port: int) =
    String.concat "\r\n" [
        "GET / HTTP/1.1"
        $"Host: localhost:{port}"
        "Connection: Upgrade"
        "Upgrade: websocket"
        ""
        "" ]

let private clientStateThrough (start: string -> FIO<Net.HttpListener, WsError>) =
    fio {
        let! port, listener = startListenerWith start "localhost"
        let! accepting = (WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig).Fork()
        let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
        let! state = client.State()
        let! server = accepting.Join()
        do! client.Abort()
        do! server.Dispose()
        do! WebSocketServer.close listener
        return state
    }

[<Tests>]
let webSocketServerTests =
    testList
        "WebSocketServer"
        [
            testList
                "Lifecycle"
                [
                    testAllRuntimes "start - creates a listening server" (fun runtime ->
                        let state = runWithTimeout runtime (clientStateThrough WebSocketServer.start)

                        Expect.equal state Net.WebSockets.WebSocketState.Open "A client should connect to the started listener")

                    testAllRuntimes "close - stops the listener" (fun runtime ->
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()
                                do! WebSocketServer.close listener
                                let! listener2 = WebSocketServer.start $"http://localhost:{port}/"
                                do! WebSocketServer.close listener2
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "close - disposes the listener" (fun runtime ->
                        let effect =
                            fio {
                                let! _, listener = startTestListener ()
                                do! WebSocketServer.close listener
                                return listener
                            }

                        let listener = runtime.Run(effect).UnsafeSuccess()

                        Expect.throwsT<ObjectDisposedException> (fun () -> listener.Prefixes |> ignore) "A closed listener should be disposed")

                    testSequenced (
                        testAllRuntimes "close - closing twice stays quiet" (fun runtime ->
                            let originalErr = Console.Error
                            use captured = new IO.StringWriter()
                            Console.SetError captured
                            let effect =
                                fio {
                                    let! _, listener = startTestListener ()
                                    do! WebSocketServer.close listener
                                    do! WebSocketServer.close listener
                                }

                            try
                                runtime.Run(effect).UnsafeSuccess()
                            finally
                                Console.SetError originalErr

                            Expect.equal (captured.ToString()) "" "Closing a closed listener must not log to stderr"))
                ]

            testList
                "Wildcard hosts"
                [
                    for host in [ "0.0.0.0"; "[::]" ] do
                        testAllRuntimes $"start - on {host} serves both 127.0.0.1 and localhost" (fun runtime ->
                            if OperatingSystem.IsWindows() then
                                skiptest "http.sys needs a URL reservation to listen on every interface"

                            let results =
                                withTestEchoServerOn
                                    host
                                    (fun port ->
                                        FIO.forEach [ "127.0.0.1"; "localhost" ] (fun target ->
                                            fio {
                                                let! ws = WebSocketClient.connectDefault $"ws://{target}:{port}/"
                                                do! ws.SendText target
                                                let! echoed = ws.Receive Codec.text
                                                do! ws.Close()
                                                return target, echoed
                                            }))
                                    runtime

                            for target, echoed in results do
                                Expect.equal echoed target $"The echo through {target}")
                ]

            testList
                "Accept"
                [
                    testAllRuntimes "accept - yields the peer's endpoints" (fun runtime ->
                        let endpoints = ref (None, None)

                        withTestServer
                            (fun ws ->
                                fio {
                                    endpoints.Value <- (ws.RemoteEndPoint, ws.LocalEndPoint)
                                    do! ws.SendText "seen"
                                })
                            (fun port ->
                                fio {
                                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                    let! _ = ws.ReceiveMessage()
                                    do! ws.Close()
                                })
                            runtime

                        match endpoints.Value with
                        | Some(:? Net.IPEndPoint as remote), Some(:? Net.IPEndPoint as local) ->
                            Expect.isTrue (Net.IPAddress.IsLoopback remote.Address) "The peer should be loopback"
                            Expect.isTrue (Net.IPAddress.IsLoopback local.Address) "The local address should be loopback"
                            Expect.notEqual remote.Port 0 "The peer's port should be known"
                        | remote, local -> failtest $"Expected IP endpoints but got {remote} and {local}")

                    testAllRuntimes "accept - receives a client connection" (fun runtime ->
                        let msg =
                            withTestServer
                                (fun ws -> fio { do! ws.SendText "from server" })
                                (fun port ->
                                    fio {
                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        let! msg = ws.ReceiveMessage()
                                        do! ws.Close()
                                        return msg
                                    })
                                runtime

                        match msg with
                        | Frame(Text s) -> Expect.equal s "from server" "Should receive server message"
                        | other -> failtest $"Expected text frame but got {other}")

                    testAllRuntimes "acceptLoop - handles multiple connections" (fun runtime ->
                        let results =
                            withTestEchoServer
                                (fun port ->
                                    FIO.forEach [ 1..3 ] (fun i ->
                                        fio {
                                            let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                            let msg = $"msg{i}"
                                            do! ws.SendText msg
                                            let! received = ws.ReceiveMessage()
                                            do! ws.Close()
                                            return i, msg, received
                                        }))
                                runtime

                        for i, msg, received in results do
                            match received with
                            | Frame(Text s) -> Expect.equal s msg $"Echo {i} should match"
                            | other -> failtest $"Expected text frame but got {other}")

                    testAllRuntimes "acceptLoop - keeps serving after a handler fails with a typed error" (fun runtime ->
                        let attempts = ref 0
                        let handler (ws: WebSocket) =
                            if Interlocked.Increment attempts = 1 then
                                FIO.fail (GeneralError "The handler failed.")
                            else
                                echoHandler ws
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()
                                let! loop = (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig handler).Fork()

                                let! first = connectWhenListening $"ws://localhost:{port}/"
                                let! firstOutcome = first.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)

                                let! second = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! second.SendText "still serving"
                                let! echoed = second.Receive Codec.text
                                do! second.Close()

                                do! loop.InterruptNow()
                                do! WebSocketServer.close listener
                                return firstOutcome, echoed
                            }

                        let firstOutcome, echoed = runWithTimeout runtime effect

                        match firstOutcome with
                        | Some(ConnectionClosed _) -> ()
                        | other -> failtest $"The failed handler's connection should be closed, got {other}"
                        Expect.equal echoed "still serving" "A handler that fails must not stop the loop")

                    testAllRuntimes "startDefault - is an alias for start" (fun runtime ->
                        let state = runWithTimeout runtime (clientStateThrough WebSocketServer.startDefault)

                        Expect.equal state Net.WebSockets.WebSocketState.Open "A client should connect to the listener startDefault started")
                ]

            testList
                "Handshake rejection"
                [
                    testAllRuntimes "accept - rejects a plain HTTP request with 400" (fun runtime ->
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

                    testAllRuntimes "acceptLoop - answers a plain HTTP request with 400 and keeps serving" (fun runtime ->
                        let status, echoed =
                            withTestEchoServer
                                (fun port ->
                                    fio {
                                        let! status =
                                            FIO.attempt
                                                (fun () ->
                                                    use client = new HttpClient()
                                                    int (client.GetAsync($"http://localhost:{port}/").Result.StatusCode))
                                                WsError.fromException

                                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                        do! ws.SendText "after the plain request"
                                        let! echoed = ws.Receive Codec.text
                                        do! ws.Close()
                                        return status, echoed
                                    })
                                runtime

                        Expect.equal status 400 "A plain HTTP request must be answered with 400"
                        Expect.equal echoed "after the plain request" "The loop must keep serving")

                    testAllRuntimes "acceptLoop - keeps serving after a malformed upgrade request" (fun runtime ->
                        let echoed =
                            withTestEchoServer
                                (fun port ->
                                    fio {
                                        let! malformed =
                                            FIO.attempt
                                                (fun () ->
                                                    let client = new Net.Sockets.TcpClient("localhost", port)
                                                    let bytes = Text.Encoding.ASCII.GetBytes(malformedUpgrade port)
                                                    client.GetStream().Write(bytes, 0, bytes.Length)
                                                    client)
                                                WsError.fromException

                                        do! sleepMs 200.0

                                        return!
                                            (fio {
                                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                                do! ws.SendText "after the malformed upgrade"
                                                let! echoed = ws.Receive Codec.text
                                                do! ws.Close()
                                                return echoed
                                            })
                                                .Ensuring(FIO.succeedWith (fun () -> malformed.Dispose()))
                                    })
                                runtime

                        Expect.equal echoed "after the malformed upgrade" "The loop must keep serving after a failed handshake")

                    testAllRuntimes "acceptLoop - answers a malformed upgrade request with 400" (fun runtime ->
                        let statusLine =
                            withTestEchoServer
                                (fun port ->
                                    FIO.attempt
                                        (fun () ->
                                            use client = new Net.Sockets.TcpClient("localhost", port)
                                            let stream = client.GetStream()
                                            let bytes = Text.Encoding.ASCII.GetBytes(malformedUpgrade port)
                                            stream.Write(bytes, 0, bytes.Length)
                                            stream.ReadTimeout <- 10000
                                            use reader = new IO.StreamReader(stream, Text.Encoding.ASCII)
                                            try reader.ReadLine() with _ -> null)
                                        WsError.fromException)
                                runtime

                        Expect.isNotNull statusLine "A malformed upgrade request must be answered"
                        Expect.stringContains statusLine "400" "A malformed upgrade request must be answered with 400")

                    testAllRuntimes "acceptLoop - malformed upgrade requests do not hold up the next client" (fun runtime ->
                        let waited =
                            withTestEchoServer
                                (fun port ->
                                    fio {
                                        let! malformed =
                                            FIO.attempt
                                                (fun () ->
                                                    [
                                                        for _ in 1..60 ->
                                                            let client = new Net.Sockets.TcpClient("localhost", port)
                                                            let bytes = Text.Encoding.ASCII.GetBytes(malformedUpgrade port)
                                                            client.GetStream().Write(bytes, 0, bytes.Length)
                                                            client
                                                    ])
                                                WsError.fromException

                                        let clock = Diagnostics.Stopwatch.StartNew()

                                        return!
                                            (fio {
                                                let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                                do! ws.SendText "next"
                                                let! _ = ws.Receive Codec.text
                                                let waited = clock.ElapsedMilliseconds
                                                do! ws.Close()
                                                return waited
                                            })
                                                .Ensuring(FIO.succeedWith (fun () -> for client in malformed do client.Dispose()))
                                    })
                                runtime

                        Expect.isLessThan waited 1_000L "A failed handshake must not pause the accept loop")

                    testAllRuntimes "acceptLoop - ends with an error when its listener is stopped under it" (fun runtime ->
                        let effect =
                            fio {
                                let! _, listener = startTestListener ()
                                let! loop = (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig noopHandler).Fork()
                                do! sleepMs 150.0
                                do! WebSocketServer.close listener
                                let! ended, _ = waitForTerminal loop 2_000

                                if ended then
                                    let! outcome = loop.Await()
                                    return Some outcome
                                else
                                    do! loop.InterruptNow()
                                    return None
                            }

                        let outcome = runWithTimeout runtime effect

                        match outcome with
                        | Some(Failed(ConnectionFailed _)) -> ()
                        | other -> failtest $"The loop must stop once its listener is stopped, got %A{other}")

                    testAllRuntimes "acceptLoop - answers 503 to a connection beyond MaxConnections" (fun runtime ->
                        let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withMaxConnections 2
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()
                                let! loop = (WebSocketServer.acceptLoop listener config noopHandler).Fork()
                                let! first = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                let! second = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! sleepMs 100.0
                                let! third = (WebSocketClient.connectDefault $"ws://localhost:{port}/").Result()
                                do! first.Close()
                                do! second.Close()
                                do! loop.InterruptNow()
                                return third
                            }

                        let third = runWithTimeout runtime effect

                        match third with
                        | Error(ConnectionFailed message) -> Expect.stringContains message "503" "The connection over the limit must be refused with 503"
                        | other -> failtest $"Expected ConnectionFailed with a 503 but got {other}")

                    testAllRuntimes "accept - fails with ConnectionFailed when the client did not offer the subprotocol" (fun runtime ->
                        let effect =
                            fio {
                                let! port, listener = startTestListener ()

                                let! acceptFiber =
                                    (WebSocketServer.accept listener WebSocketConfig.defaultConfig (Some "chat"))
                                        .Map(fun _ -> None)
                                        .CatchAll(fun error -> FIO.succeed (Some error))
                                        .Fork()

                                let! _ =
                                    (WebSocketClient.connectDefault $"ws://localhost:{port}/")
                                        .Map(fun ws -> Ok ws)
                                        .CatchAll(fun error -> FIO.succeed (Error error))
                                        .Timeout(TimeSpan.FromSeconds 5.0)

                                let! acceptOutcome = acceptFiber.Join()
                                do! WebSocketServer.close listener
                                return acceptOutcome
                            }

                        let acceptOutcome = runWithTimeout runtime effect

                        match acceptOutcome with
                        | Some(ConnectionFailed _) -> ()
                        | other -> failtest $"accept must fail with ConnectionFailed, got {other}")
                ]

            testList
                "abort"
                [
                    testAllRuntimes "abort - stops the listener immediately" (fun runtime ->
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

                        let refused = runWithTimeout runtime effect

                        Expect.isTrue refused "An aborted listener must stop serving")
                ]

            testList
                "serve / serveWith"
                [
                    testAllRuntimes "serve - accepts a connection and runs the handler" (fun runtime ->
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

                    testAllRuntimes "serveWith - runs a request/response protocol" (fun runtime ->
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

                        let reply = runWithTimeout runtime effect

                        Expect.equal
                            reply
                            "SHOUT"
                            "serveWith must decode the request and encode the handler's reply")
                ]
        ]
