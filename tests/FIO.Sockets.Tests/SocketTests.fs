/// <summary>Provides tests for core socket read, write, and lifecycle operations.</summary>
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
            // ─── Byte and text I/O ─────────────────────────────────────────

            testAllRuntimes "SendBytes/ReceiveBytes echo roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let data = Encoding.UTF8.GetBytes "hello echo"
                            do! socket.SendBytes data
                            let! received, bytesRead = socket.ReceiveBytes 8192
                            let result = Encoding.UTF8.GetString(received, 0, bytesRead)

                            Expect.equal result "hello echo" "Echo roundtrip"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "ReceiveBytes fails with non-positive maxBytes" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            let! result =
                                socket.ReceiveBytes(0).Map(fun _ -> None).CatchAll(fun error -> FIO.succeed (Some error))

                            match result with
                            | Some(InvalidState _) -> ()
                            | other -> failtest $"Expected InvalidState but got {other}"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "ReceiveBytes returns correct byte count" (fun runtime ->
                withTestServer
                    (fun socket ->
                        fio {
                            let data = Encoding.UTF8.GetBytes "knowndata!"
                            do! socket.SendBytes data
                        })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            let! received, bytesRead = socket.ReceiveBytes 8192
                            Expect.equal bytesRead 10 "Should receive 10 bytes"

                            let text = Encoding.UTF8.GetString(received, 0, bytesRead)
                            Expect.equal text "knowndata!" "Content should match"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendString/ReceiveString roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            do! socket.SendString "hello string"
                            let! received = socket.ReceiveString 8192

                            Expect.equal received "hello string" "String roundtrip"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendLine/ReceiveLine roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            do! socket.SendLine "hello line"
                            let! received = socket.ReceiveLine 8192

                            Expect.equal received "hello line" "Line roundtrip"

                            do! socket.Close()
                        })
                    runtime)

            // ─── Connection state ─────────────────────────────────────────

            testAllRuntimes "IsConnected true after connect" (fun runtime ->
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

            testAllRuntimes "Close then IsConnected false" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            do! socket.Close()

                            Expect.isFalse (socket.IsConnected()) "Should not be connected after close"
                        })
                    runtime)

            // ─── ReceiveExactly ─────────────────────────────────────────

            testAllRuntimes "ReceiveExactly roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let data = Encoding.UTF8.GetBytes "exactdata!"
                            do! socket.SendBytes data
                            let! received = socket.ReceiveExactly data.Length

                            Expect.equal
                                (Encoding.UTF8.GetString received)
                                "exactdata!"
                                "ReceiveExactly should receive exact bytes"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "ReceiveExactly fails with non-positive numBytes" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            let! result =
                                socket
                                    .ReceiveExactly(0)
                                    .Map(fun _ -> None)
                                    .CatchAll(fun error -> FIO.succeed (Some error))

                            match result with
                            | Some(InvalidState _) -> ()
                            | other -> failtest $"Expected InvalidState but got {other}"

                            do! socket.Close()
                        })
                    runtime)

            // ─── Codec / JSON ─────────────────────────────────────────

            testAllRuntimes "Send/Receive with codec roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            do! socket.Send(Codec.string, "codec roundtrip")
                            let! received = socket.Receive(Codec.string, 8192)

                            Expect.equal received "codec roundtrip" "Codec send/receive roundtrip"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendJson/ReceiveJson roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let msg = { Id = 42; Text = "json test" }
                            do! socket.SendJson msg
                            let! received = socket.ReceiveJson<TestMessage> 8192

                            Expect.equal received.Id 42 "Id should match"
                            Expect.equal received.Text "json test" "Text should match"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendJsonLine/ReceiveJsonLine roundtrip" (fun runtime ->
                withTestServer
                    echoHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let msg = { Id = 7; Text = "json line" }
                            do! socket.SendJsonLine msg
                            let! received = socket.ReceiveJsonLine<TestMessage> 8192

                            Expect.equal received.Id 7 "Id should match"
                            Expect.equal received.Text "json line" "Text should match"

                            do! socket.Close()
                        })
                    runtime)

            // ─── Inspection & failure modes ─────────────────────────────────────────

            testAllRuntimes "GetRemoteEndPoint returns valid endpoint" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let! ep = socket.GetRemoteEndPoint()
                            let ipEp = ep :?> IPEndPoint

                            Expect.equal ipEp.Port port "Remote port should match server port"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "GetLocalEndPoint returns valid endpoint" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let! ep = socket.GetLocalEndPoint()
                            let ipEp = ep :?> IPEndPoint

                            Expect.isGreaterThan ipEp.Port 0 "Local port should be assigned"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "GetConfig returns socket configuration" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            let cfg = socket.GetConfig()

                            Expect.equal cfg.Host "127.0.0.1" "Host should match"
                            Expect.equal cfg.Port port "Port should match"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "SendBytes fails on closed socket" (fun runtime ->
                withTestServer
                    noopHandler
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config
                            do! socket.Close()

                            let! result =
                                (socket.SendBytes [| 1uy |])
                                    .Map(fun _ -> None)
                                    .CatchAll(fun error -> FIO.succeed (Some error))

                            match result with
                            | Some(ConnectionClosed _) -> ()
                            | Some(GeneralError _) -> () // disposed socket may throw
                            | other -> failtest $"Expected ConnectionClosed or GeneralError but got {other}"
                        })
                    runtime)

            // ─── Message framing ─────────────────────────────────────────

            testAllRuntimes "ReceiveFramed assembles a frame split across writes" (fun runtime ->
                withTestServer
                    (fun socket ->
                        fio {
                            let! frame = (Codec.lengthPrefixed Codec.string).Encode "hello world"
                            do! socket.SendBytes frame.[0..2]
                            do! FIO.sleep (System.TimeSpan.FromMilliseconds 50.0) SocketError.fromException
                            do! socket.SendBytes frame.[3..]
                        })
                    (fun port ->
                        fio {
                            let! socket = SocketClient.connectWith "127.0.0.1" port
                            let! msg = socket.ReceiveFramed(Codec.string)

                            Expect.equal msg "hello world" "Frame should be assembled across segments"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "ReceiveFramed reads one frame per call when coalesced" (fun runtime ->
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

                            Expect.equal a "first" "First frame"
                            Expect.equal b "second" "Second frame"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "ReceiveLine reads one line per call when coalesced" (fun runtime ->
                withTestServer
                    (fun socket -> fio { do! socket.SendBytes(Encoding.UTF8.GetBytes "alpha\nbeta\n") })
                    (fun port ->
                        fio {
                            let! socket = SocketClient.connectWith "127.0.0.1" port
                            let! a = socket.ReceiveLine 1024
                            let! b = socket.ReceiveLine 1024

                            Expect.equal a "alpha" "First line"
                            Expect.equal b "beta" "Second line"

                            do! socket.Close()
                        })
                    runtime)

            // ─── Timeout ─────────────────────────────────────────

            testAllRuntimes "ReceiveBytes times out as TimeoutError" (fun runtime ->
                withTestServer
                    (fun _socket ->
                        fio { do! FIO.sleep (System.TimeSpan.FromMilliseconds 3000.0) SocketError.fromException })
                    (fun port ->
                        fio {
                            let! baseConfig = SocketConfig.create ("127.0.0.1", port)
                            let config = SocketConfig.withReceiveTimeout (300, baseConfig)
                            let! socket = SocketClient.connect config

                            let! result =
                                (socket.ReceiveBytes 1024).Map(fun _ -> None).CatchAll(fun e -> FIO.succeed (Some e))

                            do! socket.Close()

                            match result with
                            | Some(TimeoutError _) -> ()
                            | other -> failtest $"Expected TimeoutError but got {other}"
                        })
                    runtime)
        ]
