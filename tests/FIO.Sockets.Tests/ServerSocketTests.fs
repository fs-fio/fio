module FIO.Sockets.Tests.ServerSocketTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System.Net
open System.Text

open Expecto

[<Tests>]
let serverSocketTests =
    testList
        "ServerSocket"
        [

            testAllRuntimes "bind succeeds on port 0" (fun runtime ->
                let eff =
                    fio {
                        let! config = ServerSocketConfig.create ("127.0.0.1", 0)
                        let! server = ServerSocket.bind config
                        let! ep = ServerSocket.getLocalEndPoint server
                        let port = (ep :?> IPEndPoint).Port

                        Expect.isGreaterThan port 0 "Port should be assigned"

                        do! ServerSocket.close server
                    }

                runtime.Run(eff).UnsafeSuccess())

            testAllRuntimes "accept receives client connection" (fun runtime ->
                withTestServer
                    (fun socket ->
                        fio {
                            let data = Encoding.UTF8.GetBytes "from server"
                            do! socket.SendBytes data
                        })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let! received, bytesRead = socket.ReceiveBytes 8192
                            let result = Encoding.UTF8.GetString(received, 0, bytesRead)

                            Expect.equal result "from server" "Should receive server message"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "withServerSocket provides acquire/release" (fun runtime ->
                let eff =
                    fio {
                        let! config = ServerSocketConfig.create ("127.0.0.1", 0)

                        let! result =
                            ServerSocket.withServerSocket (
                                config,
                                fun server ->
                                    fio {
                                        let! ep = ServerSocket.getLocalEndPoint server
                                        let port = (ep :?> IPEndPoint).Port
                                        return port > 0
                                    }
                            )

                        Expect.isTrue result "Should have gotten a valid port"
                    }

                runtime.Run(eff).UnsafeSuccess())

            testAllRuntimes "acceptLoop handles multiple connections" (fun runtime ->
                withTestEchoServer
                    (fun port ->
                        fio {
                            for i in 1..3 do
                                let! socket = SocketClient.connectWith "127.0.0.1" port
                                let msg = $"msg{i}"
                                do! socket.SendString msg
                                let! received = socket.ReceiveString 8192

                                Expect.equal received msg $"Echo {i} should match"

                                do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "getConfig returns bound configuration" (fun runtime ->
                let eff =
                    fio {
                        let! config = ServerSocketConfig.create ("127.0.0.1", 0)
                        let! server = ServerSocket.bind config
                        let retrieved = ServerSocket.getConfig server

                        Expect.equal retrieved.BindAddress "127.0.0.1" "BindAddress"
                        Expect.equal retrieved.BindPort 0 "BindPort"

                        do! ServerSocket.close server
                    }

                runtime.Run(eff).UnsafeSuccess())

            testAllRuntimes "getLocalEndPoint returns bound endpoint" (fun runtime ->
                let eff =
                    fio {
                        let! config = ServerSocketConfig.create ("127.0.0.1", 0)
                        let! server = ServerSocket.bind config
                        let! ep = ServerSocket.getLocalEndPoint server
                        let ipEp = ep :?> IPEndPoint

                        Expect.isGreaterThan ipEp.Port 0 "Bound port should be assigned"
                        Expect.equal (ipEp.Address.ToString()) "127.0.0.1" "Bound address should match"

                        do! ServerSocket.close server
                    }

                runtime.Run(eff).UnsafeSuccess())
        ]
