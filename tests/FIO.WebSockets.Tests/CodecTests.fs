module FIO.WebSockets.Tests.CodecTests

open FIO.WebSockets.Tests.Utilities
open FIO.WebSockets.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open System.Net.WebSockets
open System.Text.Json

open Expecto

open FIO.WebSockets

[<Tests>]
let codecTests =
    testList "Codec" [

        testList "frame" [

            testPropertyWithConfig fsCheckConfig "Text frame roundtrip"
            <| fun (runtime: FIORuntime) ->
                let frame = Text "hello"
                let eff = fio {
                    let! encoded = Codec.frame.Encode frame
                    let! decoded = Codec.frame.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result frame "frame codec roundtrip"

            testAllRuntimes "Binary frame roundtrip" (fun runtime ->
                let frame = Binary [| 1uy; 2uy; 3uy |]
                let eff = fio {
                    let! encoded = Codec.frame.Encode frame
                    let! decoded = Codec.frame.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result frame "Binary frame roundtrip")
        ]

        testList "binary" [

            testAllRuntimes "encode produces Binary frame" (fun runtime ->
                let data = [| 10uy; 20uy; 30uy |]
                let eff = Codec.binary.Encode data
                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Binary b -> Expect.equal b data "Binary data should match"
                | other -> failtest $"Expected Binary frame but got {other}")

            testPropertyWithConfig fsCheckConfig "roundtrip preserves data"
            <| fun (runtime: FIORuntime) ->
                let data = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
                let eff = fio {
                    let! encoded = Codec.binary.Encode data
                    let! decoded = Codec.binary.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result data "binary codec roundtrip"

            testAllRuntimes "decode fails on Text frame" (fun runtime ->
                let eff = Codec.binary.Decode (Text "hello")
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | CodecError _ -> ()
                | other -> failtest $"Expected CodecError but got {other}")
        ]

        testList "text" [

            testAllRuntimes "encode produces Text frame" (fun runtime ->
                let eff = Codec.text.Encode "hello"
                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Text s -> Expect.equal s "hello" "Text content should match"
                | other -> failtest $"Expected Text frame but got {other}")

            testPropertyWithConfig fsCheckConfig "roundtrip preserves string"
            <| fun (runtime: FIORuntime) ->
                let text = "hello world"
                let eff = fio {
                    let! encoded = Codec.text.Encode text
                    let! decoded = Codec.text.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result text "text codec roundtrip"

            testAllRuntimes "empty string" (fun runtime ->
                let eff = fio {
                    let! encoded = Codec.text.Encode ""
                    let! decoded = Codec.text.Decode encoded
                    return decoded
                }

                Expect.equal (runtime.Run(eff).UnsafeSuccess()) "" "empty string roundtrip")

            testAllRuntimes "unicode string" (fun runtime ->
                let eff = fio {
                    let! encoded = Codec.text.Encode "héllo wörld 🌍"
                    let! decoded = Codec.text.Decode encoded
                    return decoded
                }

                Expect.equal (runtime.Run(eff).UnsafeSuccess()) "héllo wörld 🌍" "unicode roundtrip")

            testAllRuntimes "decode fails on Binary frame" (fun runtime ->
                let eff = Codec.text.Decode (Binary [| 1uy |])
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | CodecError _ -> ()
                | other -> failtest $"Expected CodecError but got {other}")
        ]

        testList "json" [

            testAllRuntimes "TestMessage roundtrip" (fun runtime ->
                let msg =
                    { Id = 42;
                      Text = "hello" }
                let codec = Codec.json
                let eff = fio {
                    let! encoded = codec.Encode msg
                    let! decoded = codec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result.Id msg.Id "Id should match"
                Expect.equal result.Text msg.Text "Text should match")

            testAllRuntimes "invalid JSON produces error" (fun runtime ->
                let codec = Codec.json
                let eff = codec.Decode (Text "not valid json!!!")
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | GeneralError _ -> ()
                | other -> failtest $"Expected GeneralError but got {other}")

            testAllRuntimes "Close frame produces CodecError" (fun runtime ->
                let codec = Codec.json
                let eff = codec.Decode (Close(WebSocketCloseStatus.NormalClosure, "bye"))
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | CodecError _ -> ()
                | other -> failtest $"Expected CodecError but got {other}")
        ]

        testList "jsonWithOptions" [

            testAllRuntimes "custom options roundtrip" (fun runtime ->
                let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                let codec = Codec.jsonWithOptions options
                let msg =
                    { Id = 7;
                      Text = "custom" }
                let eff = fio {
                    let! encoded = codec.Encode msg
                    let! decoded = codec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result.Id msg.Id "Id should match"
                Expect.equal result.Text msg.Text "Text should match")
        ]

        testList "jsonLine" [

            testAllRuntimes "encode appends newline" (fun runtime ->
                let codec = Codec.jsonLine None
                let msg =
                    { Id = 1;
                      Text = "line" }
                let eff = codec.Encode msg

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Text s -> Expect.stringContains s "\n" "Should contain newline"
                | other -> failtest $"Expected Text frame but got {other}")

            testAllRuntimes "roundtrip preserves content" (fun runtime ->
                let codec = Codec.jsonLine None
                let msg =
                    { Id = 3;
                      Text = "jsonline" }
                let eff = fio {
                    let! encoded = codec.Encode msg
                    let! decoded = codec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result.Id msg.Id "Id should match"
                Expect.equal result.Text msg.Text "Text should match")
        ]

        testList "map" [

            testPropertyWithConfig fsCheckConfig "bidirectional mapping roundtrip"
            <| fun (runtime: FIORuntime) ->
                let intCodec =
                    Codec.text
                    |> Codec.map int string
                let eff = fio {
                    let! encoded = intCodec.Encode 42
                    let! decoded = intCodec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 42 "map codec roundtrip"
        ]

        testList "compose" [

            testAllRuntimes "pair roundtrip" (fun runtime ->
                let pairCodec = Codec.compose Codec.text Codec.text
                let eff = fio {
                    let! encoded = pairCodec.Encode ("hello", "world")
                    let! decoded = pairCodec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result ("hello", "world") "compose codec roundtrip")
        ]

        testList "create" [

            testAllRuntimes "effectful encode/decode roundtrip" (fun runtime ->
                let codec =
                    Codec.create
                        (fun (str: string) -> FIO.succeed (Text str))
                        (fun frame ->
                            match frame with
                            | Text s -> FIO.succeed s
                            | _ -> FIO.fail (CodecError "Expected text"))
                let eff = fio {
                    let! encoded = codec.Encode "hello"
                    let! decoded = codec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result "hello" "create codec roundtrip")
        ]

        testList "createPure" [

            testAllRuntimes "throwing encoder produces error" (fun runtime ->
                let codec =
                    Codec.createPure
                        (fun _ -> failwith "boom")
                        (fun frame ->
                            match frame with
                            | Text s -> s
                            | _ -> failwith "unexpected")
                let eff = codec.Encode "test"
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | GeneralError _ -> ()
                | other -> failtest $"Expected GeneralError but got {other}")

            testAllRuntimes "throwing decoder produces error" (fun runtime ->
                let codec =
                    Codec.createPure
                        (fun str -> Text str)
                        (fun (_: WebSocketFrame) -> failwith "boom")
                let eff = codec.Decode (Text "test")
                let err = runtime.Run(eff).UnsafeError()

                match err with
                | GeneralError _ -> ()
                | other -> failtest $"Expected GeneralError but got {other}")

            testAllRuntimes "normal roundtrip" (fun runtime ->
                let codec =
                    Codec.createPure
                        (fun str -> Text str)
                        (fun frame ->
                            match frame with
                            | Text s -> s
                            | _ -> failwith "unexpected")
                let eff = fio {
                    let! encoded = codec.Encode "pure test"
                    let! decoded = codec.Decode encoded
                    return decoded
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result "pure test" "createPure roundtrip")
        ]
    ]
