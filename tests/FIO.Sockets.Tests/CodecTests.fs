module FIO.Sockets.Tests.CodecTests

open FIO.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Sockets

open Expecto

open System.Text
open System.Text.Json

[<Tests>]
let codecTests =
    testList
        "Codec"
        [

            testList
                "bytes"
                [

                    testAllRuntimesSequenced "bytes - roundtrip preserves the data"
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

                    testAllRuntimesSequenced "string - roundtrip preserves the string"
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

                    testAllRuntimesSequenced "string - roundtrips an empty string" (fun runtime ->
                        let effect =
                            fio {
                                let! encoded = Codec.string.Encode ""
                                let! decoded = Codec.string.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "" "empty string roundtrip")

                    testAllRuntimesSequenced "string - roundtrips a unicode string" (fun runtime ->
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

                    testAllRuntimesSequenced "line - encode appends a newline"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            fio {
                                let! encoded = Codec.line.Encode "hello"
                                return Encoding.UTF8.GetString encoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello\n" "Should append newline"

                    testAllRuntimesSequenced "line - decode trims the newline" (fun runtime ->
                        let effect =
                            fio {
                                let bytes = Encoding.UTF8.GetBytes "hello\n"
                                let! decoded = Codec.line.Decode bytes
                                return decoded
                            }

                        let result2 = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result2 "hello" "Should trim \\n")

                    testAllRuntimesSequenced "line - decode trims a carriage return" (fun runtime ->
                        let effect =
                            fio {
                                let bytes = Encoding.UTF8.GetBytes "hello\r\n"
                                let! decoded = Codec.line.Decode bytes
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello" "Should trim \\r\\n")

                    testAllRuntimesSequenced "line - roundtrip preserves the line content"
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

                    testAllRuntimesSequenced "json - roundtrips a TestMessage" (fun runtime ->
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

                    testAllRuntimesSequenced "json - invalid bytes produce CodecError" (fun runtime ->
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

                    testAllRuntimesSequenced "jsonLine - roundtrips with a trailing newline" (fun runtime ->
                        let msg = { Id = 1; Text = "jsonline" }
                        let codec = Codec.jsonLine None
                        let effect =
                            fio {
                                let! encoded = codec.Encode msg
                                let encodedStr = Encoding.UTF8.GetString encoded
                                let! decoded = codec.Decode encoded
                                return encodedStr, decoded
                            }

                        let encodedStr, result = runtime.Run(effect).UnsafeSuccess()

                        Expect.stringContains encodedStr "\n" "Should contain newline"
                        Expect.equal result.Id msg.Id "Id should match"
                        Expect.equal result.Text msg.Text "Text should match")
                ]

            testList
                "map"
                [

                    testAllRuntimesSequenced "map - roundtrips through a bidirectional mapping"
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

                    testAllRuntimesSequenced "compose - roundtrips a pair" (fun runtime ->
                        let pairCodec = Codec.compose Codec.string Codec.string
                        let effect =
                            fio {
                                let! encoded = pairCodec.Encode("hello", "world")
                                let! decoded = pairCodec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result ("hello", "world") "compose codec roundtrip")

                    testAllRuntimesSequenced "compose - a malformed length prefix produces CodecError" (fun runtime ->
                        let codec = Codec.compose Codec.string Codec.string

                        let error =
                            runtime
                                .Run(codec.Decode [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0uy |])
                                .UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced "compose - fewer than 8 bytes produce CodecError" (fun runtime ->
                        let codec = Codec.compose Codec.string Codec.string

                        let error = runtime.Run(codec.Decode [| 0uy; 0uy; 0uy |]).UnsafeError()

                        match error with
                        | CodecError(message, _) ->
                            Expect.stringContains message "Insufficient bytes" "The failure should say why"
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced
                        "compose - a first payload past the end produces CodecError"
                        (fun runtime ->
                            let codec = Codec.compose Codec.string Codec.string

                            let error =
                                runtime.Run(codec.Decode [| 0uy; 0uy; 0uy; 10uy; 0uy; 0uy; 0uy; 0uy |]).UnsafeError()

                            match error with
                            | CodecError(message, _) ->
                                Expect.stringContains message "Incomplete first payload" "The failure should say why"
                            | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced "compose - a negative second length produces CodecError" (fun runtime ->
                        let codec = Codec.compose Codec.string Codec.string

                        let error =
                            runtime
                                .Run(codec.Decode [| 0uy; 0uy; 0uy; 0uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |])
                                .UnsafeError()

                        match error with
                        | CodecError(message, _) ->
                            Expect.stringContains message "Negative second payload length" "The failure should say why"
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced
                        "compose - a second payload past the end produces CodecError"
                        (fun runtime ->
                            let codec = Codec.compose Codec.string Codec.string

                            let error =
                                runtime
                                    .Run(codec.Decode [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 5uy; 1uy; 2uy |])
                                    .UnsafeError()

                            match error with
                            | CodecError(message, _) ->
                                Expect.stringContains message "Incomplete second payload" "The failure should say why"
                            | other -> failtest $"Expected CodecError but got {other}")
                ]

            testList
                "lengthPrefixed"
                [

                    testAllRuntimesSequenced "lengthPrefixed - roundtrip preserves the data" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string
                        let effect =
                            fio {
                                let! encoded = codec.Encode "hello"
                                let! decoded = codec.Decode encoded
                                return decoded
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "hello" "lengthPrefixed roundtrip")

                    testAllRuntimesSequenced "lengthPrefixed - insufficient bytes produce CodecError" (fun runtime ->
                        let codec = Codec.lengthPrefixed Codec.string
                        let effect = codec.Decode [| 0uy; 0uy |]

                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced
                        "lengthPrefixed - a negative length prefix produces CodecError"
                        (fun runtime ->
                            let codec = Codec.lengthPrefixed Codec.string

                            let error =
                                runtime.Run(codec.Decode [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 1uy; 2uy |]).UnsafeError()

                            match error with
                            | CodecError _ -> ()
                            | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced
                        "lengthPrefixed - a length exceeding the buffer produces CodecError"
                        (fun runtime ->
                            let codec = Codec.lengthPrefixed Codec.string

                            let error =
                                runtime.Run(codec.Decode [| 0uy; 0uy; 0uy; 100uy; 1uy; 2uy; 3uy |]).UnsafeError()

                            match error with
                            | CodecError _ -> ()
                            | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced "lengthPrefixed - encode uses network byte order" (fun runtime ->
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

                    testAllRuntimesSequenced "jsonWithOptions - roundtrips with custom options" (fun runtime ->
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

                    testAllRuntimesSequenced
                        "create - roundtrips through an effectful encode and decode"
                        (fun runtime ->
                            let codec =
                                Codec.create
                                    (fun (str: string) -> FIO.succeed (Encoding.UTF8.GetBytes str))
                                    (fun bytes -> FIO.succeed (Encoding.UTF8.GetString bytes))
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

                    testAllRuntimesSequenced "createPure - a throwing encoder produces CodecError" (fun runtime ->
                        let codec =
                            Codec.createPure (fun (_: string) -> failwith "boom") (fun bytes ->
                                Encoding.UTF8.GetString bytes)
                        let effect = codec.Encode "test"

                        let error = runtime.Run(effect).UnsafeError()

                        match error with
                        | CodecError _ -> ()
                        | other -> failtest $"Expected CodecError but got {other}")

                    testAllRuntimesSequenced "createPure - a throwing decoder produces CodecError" (fun runtime ->
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
