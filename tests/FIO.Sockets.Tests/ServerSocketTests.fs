module FIO.Sockets.Tests.ServerSocketTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System.Net
open System.Text
open System.Threading

open Expecto

[<Tests>]
let serverSocketTests =
    testList
        "ServerSocket"
        [
            testList
                "Bind / Accept"
                [
                    testAllRuntimes "bind - succeeds on port 0" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                do! ServerSocket.close server
                                return port
                            }

                        let port = runtime.Run(effect).UnsafeSuccess()

                        Expect.isGreaterThan port 0 "Port should be assigned")

                    testAllRuntimes "bind - resolves the hostname localhost" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "localhost" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                do! ServerSocket.close server
                                return port
                            }

                        let port = runtime.Run(effect).UnsafeSuccess()

                        Expect.isGreaterThan port 0 "Port should be assigned for a resolved hostname")

                    testAllRuntimes "bind - fails with BindFailed when the port is in use" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! first = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint first
                                let port = (ep :?> IPEndPoint).Port
                                let! taken = ServerSocketConfig.create "127.0.0.1" port

                                let! outcome =
                                    (ServerSocket.bind taken)
                                        .Map(fun second -> Ok second)
                                        .CatchAll(fun error -> FIO.succeed (Error error))

                                do! ServerSocket.close first
                                return outcome, port
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Error(BindFailed(address, boundPort, (:? Sockets.SocketException as ex))), port ->
                            Expect.equal (address, boundPort) ("127.0.0.1", port) "BindFailed should name what it tried to bind"
                            Expect.equal ex.SocketErrorCode Sockets.SocketError.AddressAlreadyInUse "The port should be reported as in use"
                        | Ok second, _ ->
                            runtime.Run(ServerSocket.close second).UnsafeSuccess()
                            failtest "A port in use must not bind a second time"
                        | Error other, _ -> failtest $"Expected BindFailed but got {other}")

                    testAllRuntimes "bind - releases its socket when listening fails" (fun runtime ->
                        let probe =
                            new Sockets.Socket(Sockets.AddressFamily.InterNetwork, Sockets.SocketType.Dgram, Sockets.ProtocolType.Udp)
                        probe.Bind(IPEndPoint(IPAddress.Loopback, 0))
                        let port = (probe.LocalEndPoint :?> IPEndPoint).Port
                        probe.Dispose()
                        let effect =
                            fio {
                                let! tcpConfig = ServerSocketConfig.create "127.0.0.1" port

                                let udpConfig =
                                    { tcpConfig with
                                        SocketType = Sockets.SocketType.Dgram
                                        ProtocolType = Sockets.ProtocolType.Udp
                                    }

                                return!
                                    (ServerSocket.bind udpConfig)
                                        .Map(fun server -> Ok server)
                                        .CatchAll(fun error -> FIO.succeed (Error error))
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Error(BindFailed _) ->
                            let rebound =
                                try
                                    use rebind =
                                        new Sockets.Socket(
                                            Sockets.AddressFamily.InterNetwork,
                                            Sockets.SocketType.Dgram,
                                            Sockets.ProtocolType.Udp
                                        )

                                    rebind.Bind(IPEndPoint(IPAddress.Loopback, port))
                                    true
                                with :? Sockets.SocketException ->
                                    false
                            Expect.isTrue rebound "A failed bind must release its socket, leaving the port free"
                        | other -> failtest $"Expected BindFailed for a socket that cannot listen, got {other}")

                    testAllRuntimes "accept - receives a client connection" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        let data = Encoding.UTF8.GetBytes "from server"
                                        do! socket.SendBytes data
                                    })
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! received, bytesRead = socket.ReceiveBytes 8192
                                        let result = Encoding.UTF8.GetString(received, 0, bytesRead)
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        Expect.equal result "from server" "Should receive server message")

                    testAllRuntimes "accept - fails with AcceptFailed on a closed server socket" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                do! ServerSocket.close server
                                return! (ServerSocket.accept server).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                            }

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded(Some(AcceptFailed _)) -> ()
                        | other -> failtest $"Accepting on a closed server socket must fail with AcceptFailed, got %A{other}")

                    testAllRuntimes "accept - applies the AcceptedSocketConfig's receive timeout to the accepted socket" (fun runtime ->
                        let effect =
                            fio {
                                let! acceptedBase = SocketConfig.create "127.0.0.1" 1
                                let accepted = SocketConfig.withReceiveTimeout 150 acceptedBase
                                let! baseConfig = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind (ServerSocketConfig.withAcceptedConfig accepted baseConfig)
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port

                                let! acceptFiber = (ServerSocket.accept server).Fork()
                                let! client = SocketClient.connectWith "127.0.0.1" port
                                let! serverSide = acceptFiber.Join()

                                let! outcome =
                                    (serverSide.ReceiveBytes 16).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))

                                do! serverSide.Close()
                                do! client.Close()
                                do! ServerSocket.close server
                                return outcome, serverSide.GetConfig()
                            }

                        let result = runWithTimeout runtime effect

                        match result with
                        | Some(TimeoutError _), config ->
                            Expect.equal config.ReceiveTimeout 150 "The accepted socket should carry the configured timeout"
                        | other, _ -> failtest $"An idle accepted socket must time out, got {other}")

                    testAllRuntimes "accept - applies the AcceptedSocketConfig's NoDelay and buffer sizes to the accepted socket" (fun runtime ->
                        let effect =
                            fio {
                                let! acceptedBase = SocketConfig.create "127.0.0.1" 1
                                let accepted =
                                    acceptedBase
                                    |> SocketConfig.withNoDelay true
                                    |> SocketConfig.withSendBufferSize 24576
                                    |> SocketConfig.withReceiveBufferSize 24576
                                let! baseConfig = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind (ServerSocketConfig.withAcceptedConfig accepted baseConfig)
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! acceptFiber = (ServerSocket.accept server).Fork()
                                let! client = SocketClient.connectWith "127.0.0.1" port
                                let! serverSide = acceptFiber.Join()
                                let options =
                                    serverSide.NetSocket.NoDelay, serverSide.NetSocket.SendBufferSize, serverSide.NetSocket.ReceiveBufferSize
                                do! serverSide.Close()
                                do! client.Close()
                                do! ServerSocket.close server
                                return options
                            }

                        let noDelay, sendBuffer, receiveBuffer = runWithTimeout runtime effect

                        Expect.isTrue noDelay "The accepted socket should have Nagle's algorithm disabled"
                        Expect.contains [ 24576; 49152 ] sendBuffer "The accepted socket should get the configured send buffer (Linux reports it doubled)"
                        Expect.contains [ 24576; 49152 ] receiveBuffer "The accepted socket should get the configured receive buffer (Linux reports it doubled)")
                ]

            testList
                "Lifecycle"
                [
                    testAllRuntimes "withServerSocket - acquires and releases the server socket" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0

                                let! result =
                                    ServerSocket.withServerSocket
                                        config
                                        (fun server ->
                                            fio {
                                                let! ep = ServerSocket.getLocalEndPoint server
                                                let port = (ep :?> IPEndPoint).Port
                                                return port > 0
                                            })
                                return result
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue result "Should have gotten a valid port")

                    testAllRuntimes "acceptLoop - handles multiple connections" (fun runtime ->
                        let echoes =
                            withTestEchoServer
                                (fun port ->
                                    FIO.forEach [ 1..3 ] (fun i ->
                                        fio {
                                            let! socket = SocketClient.connectWith "127.0.0.1" port
                                            let msg = $"msg{i}"
                                            do! socket.SendString msg
                                            let! received = socket.ReceiveString 8192
                                            do! socket.Close()
                                            return i, msg, received
                                        }))
                                runtime

                        for i, msg, received in echoes do
                            Expect.equal received msg $"Echo {i} should match")
                ]

            testList
                "Inspection"
                [
                    testAllRuntimes "getConfig - returns the bound configuration" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let retrieved = ServerSocket.getConfig server
                                do! ServerSocket.close server
                                return retrieved
                            }

                        let retrieved = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal retrieved.BindAddress "127.0.0.1" "BindAddress"
                        Expect.equal retrieved.BindPort 0 "BindPort")

                    testAllRuntimes "getLocalEndPoint - returns the bound endpoint" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let ipEp = ep :?> IPEndPoint
                                do! ServerSocket.close server
                                return ipEp
                            }

                        let ipEp = runtime.Run(effect).UnsafeSuccess()

                        Expect.isGreaterThan ipEp.Port 0 "Bound port should be assigned"
                        Expect.equal (ipEp.Address.ToString()) "127.0.0.1" "Bound address should match")
                ]

            testList
                "serve / serveWith (bind + accept + close in one effect)"
                [
                    testAllRuntimes "serve - accepts a connection and closes the server when interrupted" (fun runtime ->
                        let received = ResizeArray<string>()
                        let handlerDone = Channel<unit>()
                        let handler (socket: Socket) =
                            fio {
                                let! message = socket.Receive(Codec.line, 1024)
                                do! FIO.attempt (fun () -> received.Add message) SocketError.fromException
                                do! socket.Close()
                                do! (handlerDone.Write ()).Unit()
                            }
                        let effect =
                            fio {
                                let! probe = ServerSocketConfig.create "127.0.0.1" 0
                                let! probeServer = ServerSocket.bind probe
                                let! ep = ServerSocket.getLocalEndPoint probeServer
                                let port = (ep :?> IPEndPoint).Port
                                do! ServerSocket.close probeServer

                                let! config = ServerSocketConfig.create "127.0.0.1" port
                                let! serveFiber = (ServerSocket.serve config handler).Fork()

                                let! client = connectWhenListening "127.0.0.1" port
                                do! client.Send(Codec.line, "hello serve")
                                do! client.Close()

                                do! (handlerDone.Read ()).Unit()
                                do! serveFiber.InterruptNow()
                                return ()
                            }

                        runWithTimeout runtime effect |> ignore

                        Expect.sequenceEqual received [ "hello serve" ] "serve must deliver the message to its handler")

                    testAllRuntimes "serveWith - runs a request/response protocol" (fun runtime ->
                        let effect =
                            fio {
                                let! probe = ServerSocketConfig.create "127.0.0.1" 0
                                let! probeServer = ServerSocket.bind probe
                                let! ep = ServerSocket.getLocalEndPoint probeServer
                                let port = (ep :?> IPEndPoint).Port
                                do! ServerSocket.close probeServer

                                let! config = ServerSocketConfig.create "127.0.0.1" port

                                let respond (request: string) =
                                    FIO.succeed (request.ToUpperInvariant())

                                let! serveFiber =
                                    (ServerSocket.serveWith Codec.line Codec.line respond config).Fork()

                                let! client = connectWhenListening "127.0.0.1" port
                                do! client.Send(Codec.line, "shout")
                                let! reply = client.Receive(Codec.line, 1024)
                                do! client.Close()

                                do! serveFiber.InterruptNow()
                                return reply
                            }

                        let reply = runWithTimeout runtime effect

                        Expect.equal reply "SHOUT" "serveWith must decode the request and encode the handler's reply")
                ]

            testList
                "Accept-loop resilience"
                [
                    testAllRuntimes "acceptLoopWith - runs at most maxConcurrentHandlers handlers, treating a non-positive limit as one" (fun runtime ->
                        let entered = ref 0
                        let gate = Channel<unit>()
                        let enteredSignal = Channel<unit>()
                        let gatedHandler (socket: Socket) =
                            fio {
                                do! FIO.attempt (fun () -> Interlocked.Increment entered |> ignore) SocketError.fromException
                                do! (enteredSignal.Write ()).Unit()
                                do! (gate.Read ()).Unit()
                                do! socket.Close()
                            }
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoopWith 0 gatedHandler server).Fork()

                                let! first = connectWhenListening "127.0.0.1" port
                                let! second = SocketClient.connectWith "127.0.0.1" port
                                do! (enteredSignal.Read ()).Unit()
                                do! FIO.sleep (System.TimeSpan.FromMilliseconds 200.0)
                                let whileFirstRuns = entered.Value

                                do! (gate.Write ()).Unit()
                                do! (enteredSignal.Read ()).Unit()
                                do! (gate.Write ()).Unit()

                                do! first.Close()
                                do! second.Close()
                                do! loopFiber.InterruptNow()
                                do! ServerSocket.close server
                                return whileFirstRuns, entered.Value
                            }

                        let whileFirstRuns, total = runWithTimeout runtime effect

                        Expect.equal whileFirstRuns 1 "A limit of 0 must still admit exactly one handler at a time"
                        Expect.equal total 2 "The second handler must run once the first one finishes")

                    testAllRuntimes "acceptLoop - survives a failing handler and keeps serving" (fun runtime ->
                        let attempts = ref 0
                        let firstFailed = Channel<unit>()
                        let flakyHandler (socket: Socket) =
                            fio {
                                let! n = FIO.attempt (fun () -> Interlocked.Increment attempts) SocketError.fromException

                                if n = 1 then
                                    do! (firstFailed.Write ()).Unit()
                                    return! FIO.fail (SocketError.GeneralError (exn "handler exploded"))
                                else
                                    do! socket.Send(Codec.line, "still alive")
                                    do! socket.Close()
                            }
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoop flakyHandler server).Fork()

                                do! (fio {
                                        let! first = connectWhenListening "127.0.0.1" port
                                        do! first.Send(Codec.line, "one")
                                        do! first.Close()
                                     }).CatchAll(fun _ -> FIO.unit ())

                                do! (firstFailed.Read ()).Unit()

                                let! second = connectWhenListening "127.0.0.1" port
                                let! reply = second.Receive(Codec.line, 1024)
                                do! second.Close()

                                do! loopFiber.InterruptNow()
                                do! ServerSocket.close server
                                return reply
                            }

                        let reply = runWithTimeout runtime effect

                        Expect.equal reply "still alive" "A failing handler must not stop the accept loop"
                        Expect.isGreaterThanOrEqual attempts.Value 2 "The loop must have accepted a second connection")

                    testAllRuntimes "acceptLoop - survives a handler that throws, closes its connection and keeps serving" (fun runtime ->
                        let attempts = ref 0
                        let throwingHandler (socket: Socket) =
                            if Interlocked.Increment attempts = 1 then
                                failwith "handler threw"
                            else
                                fio {
                                    do! socket.Send(Codec.line, "still alive")
                                    do! socket.Close()
                                }
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoop throwingHandler server).Fork()

                                let! first = connectWhenListening "127.0.0.1" port
                                let! firstOutcome = first.Receive(Codec.line, 1024).Timeout(System.TimeSpan.FromSeconds 5.0).Result()
                                do! first.Close()

                                let! second = connectWhenListening "127.0.0.1" port
                                let! reply = second.Receive(Codec.line, 1024).Timeout(System.TimeSpan.FromSeconds 5.0)
                                do! second.Close()

                                do! loopFiber.InterruptNow()
                                do! ServerSocket.close server
                                return firstOutcome, reply
                            }

                        let firstOutcome, reply = runWithTimeout runtime effect

                        Expect.isError firstOutcome "The connection whose handler threw should be closed, not left open"
                        Expect.equal reply (Some "still alive") "A handler that throws must not stop the accept loop")

                    testAllRuntimes "acceptLoop - stops with AcceptFailed when its server socket is closed under it" (fun runtime ->
                        let served = Channel<unit>()
                        let handler (socket: Socket) =
                            fio {
                                do! socket.Close()
                                do! (served.Write ()).Unit()
                            }
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoop handler server).Fork()
                                let! client = connectWhenListening "127.0.0.1" port
                                do! (served.Read ()).Unit()
                                do! client.Close()
                                do! FIO.sleep (System.TimeSpan.FromMilliseconds 150.0)
                                do! ServerSocket.close server
                                let! ended, _ = waitForTerminal loopFiber 2_000
                                if ended then
                                    let! outcome = loopFiber.Await()
                                    return Some outcome
                                else
                                    do! loopFiber.InterruptNow()
                                    return None
                            }

                        let outcome = runWithTimeout runtime effect

                        match outcome with
                        | Some(Failed(AcceptFailed _)) -> ()
                        | other -> failtest $"The loop must stop promptly with AcceptFailed once its server socket is closed, got %A{other}")
                ]
        ]
