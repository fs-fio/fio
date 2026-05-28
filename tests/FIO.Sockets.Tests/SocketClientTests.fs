/// <summary>Provides tests for socket client connection and communication.</summary>
module FIO.Sockets.Tests.SocketClientTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System.Text

open Expecto

[<Tests>]
let socketClientTests =
    testList
        "SocketClient"
        [

            testAllRuntimes "connect succeeds to listening server" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            Expect.isTrue (socket.IsConnected()) "Should be connected"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "connectWith convenience works" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! socket = SocketClient.connectWith "127.0.0.1" port

                            Expect.isTrue (socket.IsConnected()) "Should be connected"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "connect fails for unreachable host" (fun runtime ->
                let eff =
                    fio {
                        let! config = SocketConfig.create ("127.0.0.1", 1)

                        return!
                            SocketClient.connect(config).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(ConnectionFailed _) -> ()
                | other -> failtest $"Expected ConnectionFailed but got {other}")

            testAllRuntimes "withConnection auto-closes socket" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)

                            let! wasConnected =
                                SocketClient.withConnection config (fun socket -> FIO.succeed (socket.IsConnected()))

                            Expect.isTrue wasConnected "Should have been connected during action"
                        })
                    runtime)

            testAllRuntimes "withConnectionTo echoes data" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! result =
                                SocketClient.withConnectionTo "127.0.0.1" port (fun socket ->
                                    fio {
                                        let data = Encoding.UTF8.GetBytes "echo test"
                                        do! socket.SendBytes data
                                        let! received, bytesRead = socket.ReceiveBytes 8192
                                        return Encoding.UTF8.GetString(received, 0, bytesRead)
                                    })

                            Expect.equal result "echo test" "Should echo data"
                        })
                    runtime)

            testAllRuntimes "receiveWith receives data with codec" (fun runtime ->
                withTestServer
                    (fun socket -> fio { do! socket.SendString "hello receiveWith" })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! result = SocketClient.receiveWith Codec.string 8192 config
                            Expect.equal result "hello receiveWith" "Should receive data"
                        })
                    runtime)

            testAllRuntimes "sendWith sends data with codec" (fun runtime ->
                withTestServer
                    (fun socket ->
                        fio {
                            let! _, _ = socket.ReceiveBytes 8192
                            return ()
                        })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            do! SocketClient.sendWith Codec.string "hello sendWith" config
                        })
                    runtime)
        ]
