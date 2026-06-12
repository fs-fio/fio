/// <summary>Provides tests for socket codec encode and decode operations.</summary>
module FIO.Sockets.Tests.CodecTests

open FIO.Sockets.Tests.Utilities
open FIO.Sockets.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Sockets

open System.Text
open System.Text.Json

open Expecto

[<Tests>]
let codecTests =
    testList
        "Codec"
        [

            testList
                "bytes"
                [

                    testPropertyWithConfig fsCheckConfig "roundtrip preserves data"
                    <| fun (runtime: FIORuntime) ->
                        let data = Encoding.UTF8.GetBytes "hello bytes"

                        let effect =
                            fio {
                                let! encoded = Codec.bytes.Encode data
                                let! decoded = Codec.bytes.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result data "bytes codec roundtrip"
                ]

            testList
                "string"
                [

                    testPropertyWithConfig fsCheckConfig "roundtrip preserves string"
                    <| fun (runtime: FIORuntime) ->
                        let text = "hello world"

                        let effect =
                            fio {
                                let! encoded = Codec.string.Encode text
                                let! decoded = Codec.string.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result text "string codec roundtrip"

                    testAllRuntimes "empty string" (fun runtime ->
                        let effect =
                            fio {
                                let! encoded = Codec.string.Encode ""
                                let! decoded = Codec.string.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "" "empty string roundtrip")

                    testAllRuntimes "unicode string" (fun runtime ->
                        let effect =
                            fio {
                                let! encoded = Codec.string.Encode "héllo wörld 🌍"
                                let! decoded = Codec.string.Decode encoded
                                return decoded
                            }

                        let result2 = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result2 "héllo wörld 🌍" "unicode string roundtrip")
                ]

            testList
                "line"
                [

                    testPropertyWithConfig fsCheckConfig "encode appends newline"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            fio {
                                let! encoded = Codec.line.Encode "hello"
                                return Encoding.UTF8.GetString encoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello\n" "Should append newline"

                    testAllRuntimes "decode trims newline" (fun runtime ->
                        let effect =
                            fio {
                                let bytes = Encoding.UTF8.GetBytes "hello\n"
                                let! decoded = Codec.line.Decode bytes
                                return decoded
                            }

                        let result2 = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result2 "hello" "Should trim \\n")

                    testAllRuntimes "decode trims carriage return" (fun runtime ->
                        let effect =
                            fio {
                                let bytes = Encoding.UTF8.GetBytes "hello\r\n"
                                let! decoded = Codec.line.Decode bytes
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello" "Should trim \\r\\n")

                    testPropertyWithConfig fsCheckConfig "roundtrip preserves line content"
                    <| fun (runtime: FIORuntime) ->
                        let text = "test line"

                        let effect =
                            fio {
                                let! encoded = Codec.line.Encode text
                                let! decoded = Codec.line.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result text "line codec roundtrip"
                ]

            testList
                "json"
                [

                    testAllRuntimes "TestMessage roundtrip" (fun runtime ->
                        let msg = { Id = 42; Text = "hello" }
                        let codec = Codec.json

                        let effect =
                            fio {
                                let! encoded = codec.Encode msg
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result.Id msg.Id "Id should match"
                        Expect.equal result.Text msg.Text "Text should match")

                    testAllRuntimes "invalid bytes produce CodecError" (fun runtime ->
                        let codec = Codec.json
                        let effect = codec.Decode [| 0uy; 1uy; 2uy |]
                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")
                ]

            testList
                "jsonLine"
                [

                    testAllRuntimes "roundtrip with trailing newline" (fun runtime ->
                        let msg = { Id = 1; Text = "jsonline" }
                        let codec = Codec.jsonLine None

                        let effect =
                            fio {
                                let! encoded = codec.Encode msg
                                let encodedStr = Encoding.UTF8.GetString encoded
                                Expect.stringContains encodedStr "\n" "Should contain newline"
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result.Id msg.Id "Id should match"
                        Expect.equal result.Text msg.Text "Text should match")
                ]

            testList
                "map"
                [

                    testPropertyWithConfig fsCheckConfig "bidirectional mapping roundtrip"
                    <| fun (runtime: FIORuntime) ->
                        let intCodec = Codec.string |> Codec.map int string

                        let effect =
                            fio {
                                let! encoded = intCodec.Encode 42
                                let! decoded = intCodec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()
                        Expect.equal result 42 "map codec roundtrip"
                ]

            testList
                "compose"
                [

                    testAllRuntimes "pair roundtrip" (fun runtime ->
                        let pairCodec = Codec.compose Codec.string Codec.string

                        let effect =
                            fio {
                                let! encoded = pairCodec.Encode("hello", "world")
                                let! decoded = pairCodec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result ("hello", "world") "compose codec roundtrip")

                    testAllRuntimes "malformed length prefix produces CodecError" (fun runtime ->
                        let codec = Codec.compose Codec.string Codec.string

                        let error =
                            runtime
                                .Run(codec.Decode [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0uy |])
                                .UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")
                ]

            testList
                "lengthPrefixed"
                [

                    testAllRuntimes "roundtrip" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string

                        let effect =
                            fio {
                                let! encoded = codec.Encode "hello"
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello" "lengthPrefixed roundtrip")

                    testAllRuntimes "insufficient bytes produce CodecError" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string
                        let effect = codec.Decode [| 0uy; 0uy |]
                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimes "negative length prefix produces CodecError" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string

                        let error =
                            runtime.Run(codec.Decode [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 1uy; 2uy |]).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimes "length exceeding buffer produces CodecError" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string
                        // big-endian length 100 but only 3 payload bytes available
                        let error =
                            runtime.Run(codec.Decode [| 0uy; 0uy; 0uy; 100uy; 1uy; 2uy; 3uy |]).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimes "encode uses network byte order" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.bytes
                        let encoded = runtime.Run(codec.Encode [| 0xAAuy |]).UnsafeSuccess()

                        Expect.equal
                            encoded
                            [| 0uy; 0uy; 0uy; 1uy; 0xAAuy |]
                            "Length prefix should be big-endian (network order)")
                ]

            testList
                "jsonWithOptions"
                [

                    testAllRuntimes "custom options roundtrip" (fun runtime ->
                        let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                        let codec = Codec.jsonWithOptions options
                        let msg = { Id = 42; Text = "hello" }

                        let effect =
                            fio {
                                let! encoded = codec.Encode msg
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result.Id msg.Id "Id should match"
                        Expect.equal result.Text msg.Text "Text should match")
                ]

            testList
                "create"
                [

                    testAllRuntimes "effectful encode/decode roundtrip" (fun runtime ->
                        let codec =
                            Codec.create (fun (str: string) -> FIO.succeed (Encoding.UTF8.GetBytes str)) (fun bytes ->
                                FIO.succeed (Encoding.UTF8.GetString bytes))

                        let effect =
                            fio {
                                let! encoded = codec.Encode "hello"
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello" "create codec roundtrip")
                ]

            testList
                "createPure"
                [

                    testAllRuntimes "throwing encoder produces CodecError" (fun runtime ->
                        let codec =
                            Codec.createPure (fun (_: string) -> failwith "boom") (fun bytes ->
                                Encoding.UTF8.GetString bytes)

                        let effect = codec.Encode "test"
                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimes "throwing decoder produces CodecError" (fun runtime ->
                        let codec =
                            Codec.createPure (fun (str: string) -> Encoding.UTF8.GetBytes str) (fun (_: byte[]) ->
                                failwith "boom")

                        let effect = codec.Decode [| 1uy |]
                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")
                ]
        ]
