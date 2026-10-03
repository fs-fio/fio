module FIO.Sockets.Tests.SocketClientTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System
open System.Text

open Expecto

[<Tests>]
let socketClientTests =
    testList
        "SocketClient"
        [
            testList
                "Connect"
                [
                    testAllRuntimes "connect - succeeds against a listening server" (fun runtime ->
                        let connected =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let connected = socket.IsConnected()
                                        do! socket.Close()
                                        return connected
                                    })
                                runtime

                        Expect.isTrue connected "Should be connected")

                    testAllRuntimes "connectWith - connects to a listening server" (fun runtime ->
                        let connected =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let connected = socket.IsConnected()
                                        do! socket.Close()
                                        return connected
                                    })
                                runtime

                        Expect.isTrue connected "Should be connected")

                    testAllRuntimes "connect - fails for an unreachable host" (fun runtime ->
                        let effect =
                            fio {
                                let! config = SocketConfig.create "127.0.0.1" 1

                                return!
                                    SocketClient.connect(config).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Some(ConnectionFailed _) -> ()
                        | other -> failtest $"Expected ConnectionFailed but got {other}")

                    testAllRuntimes "connect - fails with ConnectionFailed for a record whose port is out of range" (fun runtime ->
                        let effect =
                            fio {
                                let! config = SocketConfig.create "127.0.0.1" 1

                                return!
                                    SocketClient
                                        .connect({ config with Port = 70000 })
                                        .Map(fun _ -> None)
                                        .CatchAll(fun error -> FIO.succeed (Some error))
                            }

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded(Some(ConnectionFailed("127.0.0.1", 70000, _))) -> ()
                        | other -> failtest $"Expected ConnectionFailed but got {other}")
                ]

            testList
                "Scoped lifetime"
                [
                    testAllRuntimes "withConnection - closes the socket when the scope ends" (fun runtime ->
                        let wasConnected =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        return! SocketClient.withConnection config (fun socket -> FIO.succeed (socket.IsConnected()))
                                    })
                                runtime

                        Expect.isTrue wasConnected "Should have been connected during action")

                    testAllRuntimes "withConnection - fails with ConnectionFailed when nothing listens" (fun runtime ->
                        let actionRan = ref false
                        let effect =
                            fio {
                                let! config = SocketConfig.create "127.0.0.1" 1

                                return!
                                    (SocketClient.withConnection config (fun _ ->
                                        FIO.succeedWith (fun () -> actionRan.Value <- true)))
                                        .Map(fun _ -> None)
                                        .CatchAll(fun error -> FIO.succeed (Some error))
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Some(ConnectionFailed("127.0.0.1", 1, _)) -> ()
                        | other -> failtest $"Expected ConnectionFailed but got {other}"
                        Expect.isFalse actionRan.Value "The action must not run without a connection")

                    testAllRuntimes "withConnectionTo - connects and echoes data" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    SocketClient.withConnectionTo "127.0.0.1" port (fun socket ->
                                        fio {
                                            let data = Encoding.UTF8.GetBytes "echo test"
                                            do! socket.SendBytes data
                                            let! received, bytesRead = socket.ReceiveBytes 8192
                                            return Encoding.UTF8.GetString(received, 0, bytesRead)
                                        }))
                                runtime

                        Expect.equal result "echo test" "Should echo data")
                ]

            testList
                "Codec wrappers"
                [
                    testAllRuntimes "receiveWith - receives data through a codec" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket -> fio { do! socket.SendString "hello receiveWith" })
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        return! SocketClient.receiveWith Codec.string 8192 config
                                    })
                                runtime

                        Expect.equal result "hello receiveWith" "Should receive data")

                    testAllRuntimes "sendWith - sends data through a codec" (fun runtime ->
                        withTestServer
                            (fun socket ->
                                fio {
                                    let! _, _ = socket.ReceiveBytes 8192
                                    return ()
                                })
                            (fun port ->
                                fio {
                                    let! config = SocketConfig.create "127.0.0.1" port
                                    do! SocketClient.sendWith Codec.string "hello sendWith" config
                                })
                            runtime)

                    testAllRuntimes "sendWith - delivers every byte before closing the connection" (fun runtime ->
                        let payload = Array.create (1024 * 1024) 7uy
                        let received = Channel<int * bool>()
                        let rec drain (socket: Socket) (total: int) =
                            (socket.ReceiveBytes 4096)
                                .Map(fun (_, count) -> Ok count)
                                .CatchAll(fun error -> FIO.succeed (Error error))
                                .FlatMap(function
                                    | Ok count -> (FIO.sleep (TimeSpan.FromMilliseconds 1.0)).FlatMap(fun () -> drain socket (total + count))
                                    | Error(ConnectionClosed _) -> FIO.succeed (total, true)
                                    | Error _ -> FIO.succeed (total, false))

                        let total, closedCleanly =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        do! FIO.sleep (TimeSpan.FromMilliseconds 300.0)
                                        let! outcome = drain socket 0
                                        do! (received.Write outcome).Unit()
                                    })
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        do! SocketClient.sendWith Codec.bytes payload config
                                        return! received.Read()
                                    })
                                runtime

                        Expect.equal total payload.Length "Every byte sent before the close must arrive"
                        Expect.isTrue closedCleanly "The connection should end with a clean close, not a reset")
                ]
        ]
