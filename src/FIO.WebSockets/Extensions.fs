namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text.Json
open System.Threading

module WebSocketExtensions =

    type WebSocket with

        /// Sends a value as JSON text, optionally using the given serializer options and cancellation token.
        member this.SendJson<'A> (value: 'A, ?options: JsonSerializerOptions, ?cancelToken: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())

                let! cancelToken =
                    match cancelToken with
                    | Some token -> FIO.succeed token
                    | None -> FIO.cancellationToken ()

                let! jsonString =
                    FIO.attempt
                        (fun () -> JsonSerializer.Serialize(value, opts))
                        WsError.codecError

                do! this.SendText(jsonString, cancelToken)
            }

        /// Receives a JSON value, optionally using the given serializer options and cancellation token.
        member this.ReceiveJson<'A> (?options: JsonSerializerOptions, ?cancelToken: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())

                let! cancelToken =
                    match cancelToken with
                    | Some token -> FIO.succeed token
                    | None -> FIO.cancellationToken ()

                match! this.ReceiveMessage cancelToken with
                | Frame(Text json) ->
                    return! FIO.attempt
                        (fun () -> JsonSerializer.Deserialize<'A>(json, opts))
                        WsError.codecError
                | Frame(Binary data) ->
                    return! FIO.attempt
                        (fun () -> JsonSerializer.Deserialize<'A>(ReadOnlySpan<byte> data, opts))
                        WsError.codecError
                | Frame(Close _) ->
                    return! FIO.fail (Closed "Connection closed while waiting for JSON")
                | ConnectionClosed(status, desc) ->
                    return! FIO.fail (Closed(WsError.describeClose status desc))
            }

        /// Sends a text message.
        member this.SendString (text: string) =
            this.SendText text

        /// Receives a text message, failing if the next frame is not text.
        member this.ReceiveString () =
            fio {
                match! this.ReceiveMessage() with
                | Frame(Text text) ->
                    return text
                | Frame(Binary _) ->
                    return! FIO.fail (CodecError "Expected text frame, got binary")
                | Frame(Close _) ->
                    return! FIO.fail (Closed "Connection closed while waiting for text")
                | ConnectionClosed(status, desc) ->
                    return! FIO.fail (Closed(WsError.describeClose status desc))
            }

        /// Sends a binary message.
        member this.SendBytes (data: byte[]) =
            this.SendBinary data

        /// Receives a binary message, failing if the next frame is not binary.
        member this.ReceiveBytes () =
            fio {
                match! this.ReceiveMessage() with
                | Frame(Binary data) ->
                    return data
                | Frame(Text _) ->
                    return! FIO.fail (CodecError "Expected binary frame, got text")
                | Frame(Close _) ->
                    return! FIO.fail (Closed "Connection closed while waiting for binary")
                | ConnectionClosed(status, desc) ->
                    return! FIO.fail (Closed(WsError.describeClose status desc))
            }
