module FIO.Sockets.Tests.SocketTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System.Net
open System.Text

open Expecto

[<Tests>]
let socketTests =
    testList
        "Socket"
        [
            testList
                "Byte and text I/O"
                [
                    testAllRuntimes "SendBytes - roundtrips through ReceiveBytes against an echo server" (fun runtime ->
                        let result =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let data = Encoding.UTF8.GetBytes "hello echo"
                                        do! socket.SendBytes data
                                        let! received, bytesRead = socket.ReceiveBytes 8192
                                        let result = Encoding.UTF8.GetString(received, 0, bytesRead)
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        Expect.equal result "hello echo" "Echo roundtrip")

                    testAllRuntimes "ReceiveBytes - fails for a non-positive maxBytes" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! result =
                                            socket.ReceiveBytes(0).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(InvalidState _) -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testAllRuntimes "ReceiveBytes - returns the number of bytes received" (fun runtime ->
                        let bytesRead, text =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        let data = Encoding.UTF8.GetBytes "knowndata!"
                                        do! socket.SendBytes data
                                    })
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! received, bytesRead = socket.ReceiveBytes 8192
                                        let text = Encoding.UTF8.GetString(received, 0, bytesRead)
                                        do! socket.Close()
                                        return bytesRead, text
                                    })
                                runtime

                        Expect.equal bytesRead 10 "Should receive 10 bytes"
                        Expect.equal text "knowndata!" "Content should match")

                    testAllRuntimes "SendString - roundtrips through ReceiveString" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        do! socket.SendString "hello string"
                                        let! received = socket.ReceiveString 8192
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal received "hello string" "String roundtrip")

                    testAllRuntimes "SendLine - roundtrips through ReceiveLine" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        do! socket.SendLine "hello line"
                                        let! received = socket.ReceiveLine 8192
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal received "hello line" "Line roundtrip")
                ]

            testList
                "ReceiveExactly"
                [
                    testAllRuntimes "ReceiveExactly - receives exactly the bytes sent" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let data = Encoding.UTF8.GetBytes "exactdata!"
                                        do! socket.SendBytes data
                                        let! received = socket.ReceiveExactly data.Length
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal
                            (Encoding.UTF8.GetString received)
                            "exactdata!"
                            "ReceiveExactly should receive exact bytes")

                    testAllRuntimes "ReceiveExactly - fails for a non-positive numBytes" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! result =
                                            socket
                                                .ReceiveExactly(0)
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(InvalidState _) -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testAllRuntimes "ReceiveExactly - fails with ConnectionClosed when the peer closes mid-read" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket -> fio { do! socket.SendBytes [| 1uy; 2uy |] })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket
                                                .ReceiveExactly(4)
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(ConnectionClosed message) ->
                            Expect.stringContains message "after 2 of 4 bytes" "The failure should say how much arrived"
                        | other -> failtest $"Expected ConnectionClosed but got {other}")

                    testAllRuntimes "ReceiveExactly - fails with ConnectionClosed once the socket is closed" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        do! socket.Close()
                                        let! result =
                                            socket
                                                .ReceiveExactly(4)
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(ConnectionClosed "Socket is not connected") -> ()
                        | other -> failtest $"Expected ConnectionClosed but got {other}")
                ]

            testList
                "Codec / JSON"
                [
                    testAllRuntimes "Send - roundtrips through Receive with a codec" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        do! socket.Send(Codec.string, "codec roundtrip")
                                        let! received = socket.Receive(Codec.string, 8192)
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal received "codec roundtrip" "Codec send/receive roundtrip")

                    testAllRuntimes "SendJson - roundtrips through ReceiveJson" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let msg = { Id = 42; Text = "json test" }
                                        do! socket.SendJson msg
                                        let! received = socket.ReceiveJson<TestMessage> 8192
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal received.Id 42 "Id should match"
                        Expect.equal received.Text "json test" "Text should match")

                    testAllRuntimes "SendJsonLine - roundtrips through ReceiveJsonLine" (fun runtime ->
                        let received =
                            withTestServer
                                echoHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let msg = { Id = 7; Text = "json line" }
                                        do! socket.SendJsonLine msg
                                        let! received = socket.ReceiveJsonLine<TestMessage> 8192
                                        do! socket.Close()
                                        return received
                                    })
                                runtime

                        Expect.equal received.Id 7 "Id should match"
                        Expect.equal received.Text "json line" "Text should match")
                ]

            testList
                "Message framing"
                [
                    testAllRuntimes "ReceiveFramed - assembles a frame split across writes" (fun runtime ->
                        let msg =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        let! frame = (Codec.lengthPrefixed Codec.string).Encode "hello world"
                                        do! socket.SendBytes frame.[0..2]
                                        do! FIO.sleep (System.TimeSpan.FromMilliseconds 50.0)
                                        do! socket.SendBytes frame.[3..]
                                    })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! msg = socket.ReceiveFramed(Codec.string)
                                        do! socket.Close()
                                        return msg
                                    })
                                runtime

                        Expect.equal msg "hello world" "Frame should be assembled across segments")

                    testAllRuntimes "ReceiveFramed - reads one frame per call when frames are coalesced" (fun runtime ->
                        let a, b =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        let! f1 = (Codec.lengthPrefixed Codec.string).Encode "first"
                                        let! f2 = (Codec.lengthPrefixed Codec.string).Encode "second"
                                        do! socket.SendBytes(Array.append f1 f2)
                                    })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! a = socket.ReceiveFramed(Codec.string)
                                        let! b = socket.ReceiveFramed(Codec.string)
                                        do! socket.Close()
                                        return a, b
                                    })
                                runtime

                        Expect.equal a "first" "First frame"
                        Expect.equal b "second" "Second frame")

                    testAllRuntimes "ReceiveFramed - fails with CodecError for a negative length prefix" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket -> fio { do! socket.SendBytes [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket
                                                .ReceiveFramed(Codec.string)
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(CodecError(message, _)) ->
                            Expect.stringContains message "Negative frame length: -1" "The failure should name the length"
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimes "ReceiveFramed - fails with BufferOverflow for a frame over maxFrameSize" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket -> fio { do! socket.SendBytes [| 0uy; 0uy; 0uy; 100uy |] })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket
                                                .ReceiveFramed(Codec.string, 10)
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(BufferOverflow(requested, available)) ->
                            Expect.equal (requested, available) (100, 10) "The failure should name both sizes"
                        | other -> failtest $"Expected BufferOverflow but got {other}")

                    testAllRuntimes "SendFramed - roundtrips an empty payload through ReceiveFramed" (fun runtime ->
                        let empty, after =
                            withTestServer
                                (fun socket ->
                                    fio {
                                        do! socket.SendFramed(Codec.string, "")
                                        do! socket.SendFramed(Codec.string, "after")
                                    })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! empty = socket.ReceiveFramed(Codec.string)
                                        let! after = socket.ReceiveFramed(Codec.string)
                                        do! socket.Close()
                                        return empty, after
                                    })
                                runtime

                        Expect.equal empty "" "An empty frame should decode to an empty payload"
                        Expect.equal after "after" "The frame after an empty one should arrive intact")

                    testAllRuntimes "ReceiveLine - reads one line per call when lines are coalesced" (fun runtime ->
                        let a, b =
                            withTestServer
                                (fun socket -> fio { do! socket.SendBytes(Encoding.UTF8.GetBytes "alpha\nbeta\n") })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! a = socket.ReceiveLine 1024
                                        let! b = socket.ReceiveLine 1024
                                        do! socket.Close()
                                        return a, b
                                    })
                                runtime

                        Expect.equal a "alpha" "First line"
                        Expect.equal b "beta" "Second line")

                    testAllRuntimes "ReceiveLine - fails with InvalidState for a non-positive maxBytes" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket.ReceiveLine(0).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(InvalidState _) -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testAllRuntimes "ReceiveLine - fails with BufferOverflow for a line longer than maxBytes" (fun runtime ->
                        let result =
                            withTestServer
                                (fun socket -> fio { do! socket.SendBytes(Encoding.UTF8.GetBytes "abcdef\n") })
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket.ReceiveLine(3).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(BufferOverflow(requested, available)) ->
                            Expect.equal (requested, available) (4, 3) "The failure should name both sizes"
                        | other -> failtest $"Expected BufferOverflow but got {other}")
                ]

            testList
                "Connection state"
                [
                    testAllRuntimes "IsConnected - is true after connect" (fun runtime ->
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

                    testAllRuntimes "Close - IsConnected is false afterwards" (fun runtime ->
                        let connected =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        do! socket.Close()
                                        let connected = socket.IsConnected()
                                        return connected
                                    })
                                runtime

                        Expect.isFalse connected "Should not be connected after close")

                    testAllRuntimes "Dispose - IsConnected is false afterwards" (fun runtime ->
                        let viaEffectConnected, viaInterfaceConnected =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! viaEffect = SocketClient.connectWith "127.0.0.1" port
                                        do! viaEffect.Dispose()
                                        let viaEffectConnected = viaEffect.IsConnected()
                                        let! viaInterface = SocketClient.connectWith "127.0.0.1" port
                                        (viaInterface :> System.IDisposable).Dispose()
                                        let viaInterfaceConnected = viaInterface.IsConnected()
                                        return viaEffectConnected, viaInterfaceConnected
                                    })
                                runtime

                        Expect.isFalse viaEffectConnected "Dispose() should release the socket"
                        Expect.isFalse viaInterfaceConnected "IDisposable.Dispose should release the socket")
                ]

            testList
                "Inspection & failure modes"
                [
                    testAllRuntimes "GetRemoteEndPoint - returns a valid endpoint" (fun runtime ->
                        let ipEp, port =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! ep = socket.GetRemoteEndPoint()
                                        let ipEp = ep :?> IPEndPoint
                                        do! socket.Close()
                                        return ipEp, port
                                    })
                                runtime

                        Expect.equal ipEp.Port port "Remote port should match server port")

                    testAllRuntimes "GetLocalEndPoint - returns a valid endpoint" (fun runtime ->
                        let ipEp =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let! ep = socket.GetLocalEndPoint()
                                        let ipEp = ep :?> IPEndPoint
                                        do! socket.Close()
                                        return ipEp
                                    })
                                runtime

                        Expect.isGreaterThan ipEp.Port 0 "Local port should be assigned")

                    testAllRuntimes "GetConfig - returns the socket configuration" (fun runtime ->
                        let cfg, port =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        let cfg = socket.GetConfig()
                                        do! socket.Close()
                                        return cfg, port
                                    })
                                runtime

                        Expect.equal cfg.Host "127.0.0.1" "Host should match"
                        Expect.equal cfg.Port port "Port should match")

                    testAllRuntimes "SendBytes - fails with InvalidState for a null buffer" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        let! result =
                                            socket.SendBytes(null).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(InvalidState("non-null buffer", "null")) -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testAllRuntimes "SendBytes - fails on a closed socket" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! config = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect config
                                        do! socket.Close()
                                        let! result =
                                            (socket.SendBytes [| 1uy |])
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(ConnectionClosed "Socket is not connected") -> ()
                        | other -> failtest $"Expected ConnectionClosed but got {other}")

                    testAllRuntimes "ReceiveBytes - fails with ConnectionClosed on a closed socket" (fun runtime ->
                        let result =
                            withTestServer
                                noopHandler
                                (fun port ->
                                    fio {
                                        let! socket = SocketClient.connectWith "127.0.0.1" port
                                        do! socket.Close()
                                        let! result =
                                            socket.ReceiveBytes(16).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(ConnectionClosed "Socket is not connected") -> ()
                        | other -> failtest $"Expected ConnectionClosed but got {other}")
                ]

            testList
                "Timeouts"
                [
                    testAllRuntimes "ReceiveBytes - times out with TimeoutError" (fun runtime ->
                        let result =
                            withTestServer
                                (fun _socket ->
                                    fio { do! FIO.sleep (System.TimeSpan.FromMilliseconds 3000.0) })
                                (fun port ->
                                    fio {
                                        let! baseConfig = SocketConfig.create "127.0.0.1" port
                                        let config = SocketConfig.withReceiveTimeout 300 baseConfig
                                        let! socket = SocketClient.connect config
                                        let! result =
                                            (socket.ReceiveBytes 1024).Map(fun _ -> None).CatchAll(fun e -> FIO.succeed (Some e))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(TimeoutError _) -> ()
                        | other -> failtest $"Expected TimeoutError but got {other}")

                    testAllRuntimes "SendBytes - times out with TimeoutError when the peer stops reading" (fun runtime ->
                        let result =
                            withTestServer
                                (fun _socket -> FIO.never<unit, SocketError> ())
                                (fun port ->
                                    fio {
                                        let! baseConfig = SocketConfig.create "127.0.0.1" port
                                        let! socket = SocketClient.connect (SocketConfig.withSendTimeout 200 baseConfig)
                                        let! result =
                                            (socket.SendBytes(Array.zeroCreate (64 * 1024 * 1024)))
                                                .Map(fun _ -> None)
                                                .CatchAll(fun error -> FIO.succeed (Some error))
                                        do! socket.Close()
                                        return result
                                    })
                                runtime

                        match result with
                        | Some(TimeoutError _) -> ()
                        | other -> failtest $"Expected TimeoutError but got {other}")

                    testAllRuntimes "Receive - times out with TimeoutError when the peer never sends" (fun runtime ->
                        let silentHandler (_socket: Socket) = FIO.never<unit, SocketError> ()
                        let effect =
                            fio {
                                let! serverConfig = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind serverConfig
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoop silentHandler server).Fork()

                                let! baseConfig = SocketConfig.create "127.0.0.1" port
                                let clientConfig = baseConfig |> SocketConfig.withReceiveTimeout 150

                                let! client = SocketClient.connect clientConfig

                                let! outcome =
                                    (client.Receive(Codec.line, 1024))
                                        .Map(fun value -> Ok value)
                                        .CatchAll(fun error -> FIO.succeed (Error error))

                                do! (client.Close()).CatchAll(fun _ -> FIO.unit ())
                                do! loopFiber.InterruptNow()
                                do! ServerSocket.close server
                                return outcome
                            }

                        let outcome = runWithTimeout runtime effect

                        match outcome with
                        | Error (SocketError.TimeoutError _) -> ()
                        | Error other -> failtest $"Expected TimeoutError but got {other}"
                        | Ok value -> failtest $"Expected a timeout but received {value}")

                    testAllRuntimes "Receive - succeeds within a configured timeout that is not exceeded" (fun runtime ->
                        let effect =
                            fio {
                                let! serverConfig = ServerSocketConfig.create "127.0.0.1" 0
                                let! server = ServerSocket.bind serverConfig
                                let! ep = ServerSocket.getLocalEndPoint server
                                let port = (ep :?> IPEndPoint).Port
                                let! loopFiber = (ServerSocket.acceptLoop echoHandler server).Fork()

                                let! baseConfig = SocketConfig.create "127.0.0.1" port

                                let clientConfig =
                                    baseConfig
                                    |> SocketConfig.withReceiveTimeout 5000
                                    |> SocketConfig.withSendTimeout 5000

                                let! client = SocketClient.connect clientConfig
                                do! client.Send(Codec.line, "ping")
                                let! reply = client.Receive(Codec.line, 1024)
                                do! client.Close()
                                do! loopFiber.InterruptNow()
                                do! ServerSocket.close server
                                return reply
                            }

                        let reply = runWithTimeout runtime effect

                        Expect.equal
                            reply
                            "ping"
                            "A generous timeout must not interfere with a normal exchange")

                    testAllRuntimes "ReceiveExactly - reports a reset connection as an error, not a timeout, when a receive timeout is set" (fun runtime ->
                        let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
                        listener.Start()
                        let port = (listener.LocalEndpoint :?> IPEndPoint).Port
                        try
                            let effect =
                                fio {
                                    let! baseConfig = SocketConfig.create "127.0.0.1" port
                                    let! client = SocketClient.connect (SocketConfig.withReceiveTimeout 5000 baseConfig)

                                    do! FIO.attempt
                                            (fun () ->
                                                use peer = listener.AcceptSocket()
                                                peer.LingerState <- Sockets.LingerOption(true, 0)
                                                peer.Close())
                                            SocketError.fromException

                                    let! result =
                                        client.ReceiveExactly(4).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))

                                    do! client.Close()
                                    return result
                                }

                            let result = runWithTimeout runtime effect

                            match result with
                            | Some(TimeoutError _) -> failtest "A reset connection must not be reported as a timeout"
                            | Some _ -> ()
                            | None -> failtest "Reading from a reset connection must fail"
                        finally
                            listener.Stop())
                ]
        ]
