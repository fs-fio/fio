/// <summary>Provides tests for socket domain types and error representations.</summary>
module FIO.Sockets.Tests.TypesTests

open FIO.Sockets.Tests.Utilities
open FIO.Sockets.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Sockets

open System

open Expecto

[<Tests>]
let typesTests =
    testList
        "Types"
        [

            testList
                "SocketError"
                [

                    testPropertyWithConfig fsCheckConfig "fromException wraps in GeneralError"
                    <| fun (_: FIORuntime) ->
                        let exn = Exception "test"
                        let error = SocketError.fromException exn

                        match error with
                        | GeneralError e ->
                            Expect.isTrue (Object.ReferenceEquals(e, exn)) "Should wrap same exception reference"
                        | _ -> failtest "Expected GeneralError"

                    testPropertyWithConfig fsCheckConfig "toException unwraps GeneralError"
                    <| fun (_: FIORuntime) ->
                        let original = Exception "test"
                        let error = GeneralError original
                        let result = SocketError.toException error

                        Expect.isTrue
                            (Object.ReferenceEquals(result, original))
                            "Should return same exception reference"

                    testCase "toException creates Exception for other variants"
                    <| fun () ->
                        let error = ConnectionClosed "peer disconnected"
                        let result = SocketError.toException error

                        Expect.stringContains
                            result.Message
                            "peer disconnected"
                            "Exception message should contain error details"
                ]

            testList
                "SocketConfig"
                [

                    testPropertyWithConfig fsCheckConfig "create succeeds with valid host and port"
                    <| fun (runtime: FIORuntime) ->
                        let port = abs (Random.Shared.Next()) % 65535 + 1
                        let eff = SocketConfig.create ("localhost", port)

                        let config = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal config.Host "localhost" "Host should match"
                        Expect.equal config.Port port "Port should match"
                        Expect.equal config.SendBufferSize 8192 "Default send buffer"
                        Expect.equal config.ReceiveBufferSize 8192 "Default receive buffer"
                        Expect.isTrue config.NoDelay "Default NoDelay should be true"
                        Expect.isTrue config.LingerEnabled "Default LingerEnabled should be true"
                        Expect.equal config.LingerTimeout 0 "Default LingerTimeout should be 0"

                    testAllRuntimes "create fails for empty host" (fun runtime ->
                        let eff = SocketConfig.create ("", 8080)
                        let error = runtime.Run(eff).UnsafeError()

                        match error with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState but got {other}"

                        let eff2 = SocketConfig.create ("   ", 8080)
                        let err2 = runtime.Run(eff2).UnsafeError()

                        match err2 with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testAllRuntimes "create fails for invalid port" (fun runtime ->
                        for port in [ 0; -1; 65536; 100000 ] do
                            let eff = SocketConfig.create ("localhost", port)
                            let error = runtime.Run(eff).UnsafeError()

                            match error with
                            | InvalidState _ -> ()
                            | other -> failtest $"Expected InvalidState for port {port} but got {other}")

                    testCase "builder functions update fields"
                    <| fun () ->
                        let config =
                            {
                                Host = "localhost"
                                Port = 8080
                                AddressFamily = Net.Sockets.AddressFamily.InterNetwork
                                SocketType = Net.Sockets.SocketType.Stream
                                ProtocolType = Net.Sockets.ProtocolType.Tcp
                                SendBufferSize = 8192
                                ReceiveBufferSize = 8192
                                SendTimeout = 30000
                                ReceiveTimeout = 30000
                                NoDelay = true
                                LingerEnabled = true
                                LingerTimeout = 0
                            }

                        let updated =
                            config
                            |> fun c -> SocketConfig.withSendBufferSize (16384, c)
                            |> fun c -> SocketConfig.withReceiveBufferSize (16384, c)
                            |> fun c -> SocketConfig.withSendTimeout (5000, c)
                            |> fun c -> SocketConfig.withReceiveTimeout (5000, c)
                            |> fun c -> SocketConfig.withNoDelay (false, c)
                            |> fun c -> SocketConfig.withAddressFamily (Net.Sockets.AddressFamily.InterNetworkV6, c)

                        Expect.equal updated.SendBufferSize 16384 "SendBufferSize"
                        Expect.equal updated.ReceiveBufferSize 16384 "ReceiveBufferSize"
                        Expect.equal updated.SendTimeout 5000 "SendTimeout"
                        Expect.equal updated.ReceiveTimeout 5000 "ReceiveTimeout"
                        Expect.isFalse updated.NoDelay "NoDelay"
                        Expect.equal updated.AddressFamily Net.Sockets.AddressFamily.InterNetworkV6 "AddressFamily"
                        Expect.equal updated.Host "localhost" "Host should be unchanged"
                ]

            testList
                "ServerSocketConfig"
                [

                    testAllRuntimes "create succeeds with valid inputs" (fun runtime ->
                        let eff = ServerSocketConfig.create ("127.0.0.1", 9090)
                        let config = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal config.BindAddress "127.0.0.1" "BindAddress"
                        Expect.equal config.BindPort 9090 "BindPort"
                        Expect.equal config.Backlog 100 "Default Backlog"
                        Expect.isNone config.AcceptedSocketConfig "Default AcceptedSocketConfig")

                    testAllRuntimes "create allows port 0" (fun runtime ->
                        let eff = ServerSocketConfig.create ("127.0.0.1", 0)
                        let config = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal config.BindPort 0 "Port 0 should be allowed")

                    testAllRuntimes "create fails for invalid inputs" (fun runtime ->
                        let eff1 = ServerSocketConfig.create ("", 8080)

                        match runtime.Run(eff1).UnsafeError() with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState but got {other}"

                        let eff2 = ServerSocketConfig.create ("127.0.0.1", -1)

                        match runtime.Run(eff2).UnsafeError() with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState but got {other}"

                        let eff3 = ServerSocketConfig.create ("127.0.0.1", 65536)

                        match runtime.Run(eff3).UnsafeError() with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState but got {other}")

                    testCase "defaultConfig and builders"
                    <| fun () ->
                        let config = ServerSocketConfig.defaultConfig

                        Expect.equal config.BindAddress "127.0.0.1" "Default BindAddress"
                        Expect.equal config.BindPort 8080 "Default BindPort"
                        Expect.equal config.Backlog 100 "Default Backlog"

                        let updated = ServerSocketConfig.withBacklog (50, config)
                        Expect.equal updated.Backlog 50 "Updated Backlog"

                        let updated2 =
                            ServerSocketConfig.withAddressFamily (Net.Sockets.AddressFamily.InterNetworkV6, config)

                        Expect.equal
                            updated2.AddressFamily
                            Net.Sockets.AddressFamily.InterNetworkV6
                            "Updated AddressFamily"

                    testCase "withAcceptedConfig updates field"
                    <| fun () ->
                        let config = ServerSocketConfig.defaultConfig

                        let socketConfig =
                            {
                                Host = "localhost"
                                Port = 9090
                                AddressFamily = Net.Sockets.AddressFamily.InterNetwork
                                SocketType = Net.Sockets.SocketType.Stream
                                ProtocolType = Net.Sockets.ProtocolType.Tcp
                                SendBufferSize = 8192
                                ReceiveBufferSize = 8192
                                SendTimeout = 30000
                                ReceiveTimeout = 30000
                                NoDelay = true
                                LingerEnabled = true
                                LingerTimeout = 0
                            }

                        let updated = ServerSocketConfig.withAcceptedConfig (socketConfig, config)

                        Expect.isSome updated.AcceptedSocketConfig "Should have AcceptedSocketConfig"

                        match updated.AcceptedSocketConfig with
                        | Some sc ->
                            Expect.equal sc.Host "localhost" "AcceptedSocketConfig host"
                            Expect.equal sc.Port 9090 "AcceptedSocketConfig port"
                        | None -> failtest "Expected Some AcceptedSocketConfig"
                ]

            testList
                "SocketPoolConfig"
                [

                    testAllRuntimes "create returns valid defaults" (fun runtime ->
                        let socketConfig =
                            {
                                Host = "localhost"
                                Port = 8080
                                AddressFamily = Net.Sockets.AddressFamily.InterNetwork
                                SocketType = Net.Sockets.SocketType.Stream
                                ProtocolType = Net.Sockets.ProtocolType.Tcp
                                SendBufferSize = 8192
                                ReceiveBufferSize = 8192
                                SendTimeout = 30000
                                ReceiveTimeout = 30000
                                NoDelay = true
                                LingerEnabled = true
                                LingerTimeout = 0
                            }

                        let eff = SocketPoolConfig.create socketConfig
                        let config = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal config.MinPoolSize 0 "Default MinPoolSize"
                        Expect.equal config.MaxPoolSize 10 "Default MaxPoolSize"
                        Expect.equal config.ConnectionLifetime 300 "Default ConnectionLifetime"
                        Expect.isTrue config.ValidateOnAcquire "Default ValidateOnAcquire")

                    testAllRuntimes "withMinPoolSize/withMaxPoolSize validate constraints" (fun runtime ->
                        let socketConfig =
                            {
                                Host = "localhost"
                                Port = 8080
                                AddressFamily = Net.Sockets.AddressFamily.InterNetwork
                                SocketType = Net.Sockets.SocketType.Stream
                                ProtocolType = Net.Sockets.ProtocolType.Tcp
                                SendBufferSize = 8192
                                ReceiveBufferSize = 8192
                                SendTimeout = 30000
                                ReceiveTimeout = 30000
                                NoDelay = true
                                LingerEnabled = true
                                LingerTimeout = 0
                            }

                        let eff =
                            fio {
                                let! config = SocketPoolConfig.create socketConfig
                                return! SocketPoolConfig.withMinPoolSize (-1, config)
                            }

                        match runtime.Run(eff).UnsafeError() with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState for negative min but got {other}"

                        let eff2 =
                            fio {
                                let! config = SocketPoolConfig.create socketConfig
                                return! SocketPoolConfig.withMaxPoolSize (0, config)
                            }

                        match runtime.Run(eff2).UnsafeError() with
                        | InvalidState _ -> ()
                        | other -> failtest $"Expected InvalidState for zero max but got {other}")

                    testCase "pure builders update fields"
                    <| fun () ->
                        let poolConfig =
                            {
                                SocketConfig =
                                    {
                                        Host = "localhost"
                                        Port = 8080
                                        AddressFamily = Net.Sockets.AddressFamily.InterNetwork
                                        SocketType = Net.Sockets.SocketType.Stream
                                        ProtocolType = Net.Sockets.ProtocolType.Tcp
                                        SendBufferSize = 8192
                                        ReceiveBufferSize = 8192
                                        SendTimeout = 30000
                                        ReceiveTimeout = 30000
                                        NoDelay = true
                                        LingerEnabled = true
                                        LingerTimeout = 0
                                    }
                                MinPoolSize = 0
                                MaxPoolSize = 10
                                ConnectionLifetime = 300
                                ValidateOnAcquire = true
                            }

                        let updated = SocketPoolConfig.withConnectionLifetime (600, poolConfig)
                        Expect.equal updated.ConnectionLifetime 600 "ConnectionLifetime"

                        let updated2 = SocketPoolConfig.withValidateOnAcquire (false, poolConfig)
                        Expect.isFalse updated2.ValidateOnAcquire "ValidateOnAcquire"
                ]
        ]
