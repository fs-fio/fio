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

                                Expect.isGreaterThan port 0 "Port should be assigned"

                                do! ServerSocket.close server
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "bind - resolves the hostname localhost" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "localhost" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port

                                Expect.isGreaterThan port 0 "Port should be assigned for a resolved hostname"

                                do! ServerSocket.close server
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "accept - receives a client connection" (fun runtime ->
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

                                    Expect.equal result "from server" "Should receive server message"

                                    do! socket.Close()
                                })
                            runtime)
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

                                Expect.isTrue result "Should have gotten a valid port"
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "acceptLoop - handles multiple connections" (fun runtime ->
                        withTestEchoServer
                            (fun port ->
                                FIO.forEachDiscard [ 1..3 ] (fun i ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let msg = $"msg{i}"
                                        do! socket.SendString msg
                                        let! received = socket.ReceiveString 8192

                                        Expect.equal received msg $"Echo {i} should match"

                                        do! socket.Close()
                                    }))
                            runtime)
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

                                Expect.equal retrieved.BindAddress "127.0.0.1" "BindAddress"
                                Expect.equal retrieved.BindPort 0 "BindPort"

                                do! ServerSocket.close server
                            }

                        runtime.Run(effect).UnsafeSuccess())

                    testAllRuntimes "getLocalEndPoint - returns the bound endpoint" (fun runtime ->
                        let effect =
                            fio {
                                let! config = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind config
                                let! ep = ServerSocket.getLocalEndPoint server
                                let ipEp = ep :?> IPEndPoint

                                Expect.isGreaterThan ipEp.Port 0 "Bound port should be assigned"
                                Expect.equal (ipEp.Address.ToString()) "127.0.0.1" "Bound address should match"

                                do! ServerSocket.close server
                            }

                        runtime.Run(effect).UnsafeSuccess())
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

                        let throwingHandler (socket: Socket) : FIO<unit, SocketError> =
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
                ]
        ]
