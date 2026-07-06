module FIO.WebSockets.Tests.TypesTests

open FIO.DSL
open FIO.WebSockets

open System
open System.Net.WebSockets

open Expecto

[<Tests>]
let typesTests =
    testList
        "Types"
        [

            testList
                "WsError"
                [

                    testCase "fromException maps TimeoutException to TimeoutError"
                    <| fun () ->
                        let ex = TimeoutException "timed out"
                        let error = WsError.fromException ex

                        match error with
                        | TimeoutError msg -> Expect.stringContains msg "timed out" "Message should match"
                        | other -> failtest $"Expected TimeoutError but got {other}"

                    testCase "fromException maps WebSocketException to GeneralError"
                    <| fun () ->
                        let ex = WebSocketException "ws error"
                        let error = WsError.fromException ex

                        match error with
                        | GeneralError msg -> Expect.stringContains msg "ws error" "Message should match"
                        | other -> failtest $"Expected GeneralError but got {other}"

                    testCase "fromException maps generic Exception to GeneralError"
                    <| fun () ->
                        let ex = Exception "generic"
                        let error = WsError.fromException ex

                        match error with
                        | GeneralError msg -> Expect.stringContains msg "generic" "Message should match"
                        | other -> failtest $"Expected GeneralError but got {other}"

                    testCase "toException maps TimeoutError to TimeoutException"
                    <| fun () ->
                        let error = TimeoutError "timeout msg"
                        let ex = WsError.toException error

                        Expect.isTrue (ex :? TimeoutException) "Should be TimeoutException"

                    testCase "toException maps GeneralError to WebSocketException"
                    <| fun () ->
                        let error = GeneralError "general msg"
                        let ex = WsError.toException error

                        Expect.isTrue (ex :? WebSocketException) "Should be WebSocketException"

                    testCase "toException maps other variants to Exception with message"
                    <| fun () ->
                        let error = ConnectionFailed "conn fail"
                        let ex = WsError.toException error

                        Expect.stringContains ex.Message "conn fail" "Exception message should contain error details"

                        let err2 = Closed "peer closed"
                        let ex2 = WsError.toException err2

                        Expect.stringContains ex2.Message "peer closed" "Closed message should match"
                ]

            testList
                "WebSocketConfig"
                [

                    testCase "defaultConfig has expected values"
                    <| fun () ->
                        let config = WebSocketConfig.defaultConfig

                        Expect.equal config.ReceiveBufferSize 4096 "Default ReceiveBufferSize"
                        Expect.equal config.SendBufferSize 4096 "Default SendBufferSize"
                        Expect.equal config.MaxMessageSize 1_048_576L "Default MaxMessageSize (1 MB)"
                        Expect.equal config.SendTimeout 30_000 "Default SendTimeout"
                        Expect.equal config.ReceiveTimeout 30_000 "Default ReceiveTimeout"

                    testCase "builder functions update correct fields"
                    <| fun () ->
                        let config = WebSocketConfig.defaultConfig

                        let c1 = WebSocketConfig.withReceiveBufferSize 8192 config
                        Expect.equal c1.ReceiveBufferSize 8192 "ReceiveBufferSize updated"
                        Expect.equal c1.SendBufferSize 4096 "SendBufferSize preserved"

                        let c2 = WebSocketConfig.withSendBufferSize 16384 config
                        Expect.equal c2.SendBufferSize 16384 "SendBufferSize updated"

                        let c3 = WebSocketConfig.withMaxMessageSize 2_097_152L config
                        Expect.equal c3.MaxMessageSize 2_097_152L "MaxMessageSize updated"

                        let c4 = WebSocketConfig.withSendTimeout 5000 config
                        Expect.equal c4.SendTimeout 5000 "SendTimeout updated"

                        let c5 = WebSocketConfig.withReceiveTimeout 10000 config
                        Expect.equal c5.ReceiveTimeout 10000 "ReceiveTimeout updated"
                ]

            testList
                "WebSocketFrame"
                [

                    testCase "Text frame construction"
                    <| fun () ->
                        let frame = Text "hello"

                        match frame with
                        | Text s -> Expect.equal s "hello" "Text content"
                        | _ -> failtest "Expected Text frame"

                    testCase "Binary frame construction"
                    <| fun () ->
                        let data = [| 1uy; 2uy; 3uy |]
                        let frame = Binary data

                        match frame with
                        | Binary b -> Expect.equal b data "Binary content"
                        | _ -> failtest "Expected Binary frame"

                    testCase "Close frame construction"
                    <| fun () ->
                        let frame = Close(WebSocketCloseStatus.NormalClosure, "goodbye")

                        match frame with
                        | Close(status, reason) ->
                            Expect.equal status WebSocketCloseStatus.NormalClosure "Close status"
                            Expect.equal reason "goodbye" "Close reason"
                        | _ -> failtest "Expected Close frame"
                ]

            testList
                "WebSocketMessage"
                [

                    testCase "Frame wraps WebSocketFrame"
                    <| fun () ->
                        let msg = Frame(Text "hello")

                        match msg with
                        | Frame(Text s) -> Expect.equal s "hello" "Frame text content"
                        | _ -> failtest "Expected Frame message"

                    testCase "ConnectionClosed carries optional status"
                    <| fun () ->
                        let msg = ConnectionClosed(Some WebSocketCloseStatus.NormalClosure, "bye")

                        match msg with
                        | ConnectionClosed(status, desc) ->
                            Expect.equal status (Some WebSocketCloseStatus.NormalClosure) "Close status"
                            Expect.equal desc "bye" "Close description"
                        | _ -> failtest "Expected ConnectionClosed"

                        let msg2 = ConnectionClosed(None, "unknown")

                        match msg2 with
                        | ConnectionClosed(status, _) -> Expect.isNone status "Status should be None"
                        | _ -> failtest "Expected ConnectionClosed"
                ]
        ]
