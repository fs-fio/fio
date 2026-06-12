namespace FIO.WebSockets

open FIO.DSL

open System.Text.Json
open System.Threading

/// <summary>Builds convenience extension methods for JSON and raw-frame WebSocket operations.</summary>
[<AutoOpen>]
module WebSocketExtensions =

    type WebSocket with

        /// <summary>Creates an effect that serializes a value as JSON and sends it as a text frame.</summary>
        /// <param name="value">The value to serialize and send.</param>
        /// <param name="options">Optional JSON serializer options; uses defaults when omitted.</param>
        /// <param name="ct">Optional cancellation token; uses <c>CancellationToken.None</c> when omitted.</param>
        /// <returns>An effect that completes when the JSON text frame has been sent.</returns>
        member this.SendJson<'T>(value: 'T, ?options: JsonSerializerOptions, ?ct: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())

                let! ct =
                    match ct with
                    | Some token -> FIO.succeed token
                    | None -> FIO.cancellationToken ()

                let! jsonString =
                    FIO.attempt
                        (fun () -> JsonSerializer.Serialize(value, opts))
                        WsError.fromException

                do! this.SendText(jsonString, ct)
            }

        /// <summary>Creates an effect that receives a frame and deserializes it as JSON.</summary>
        /// <typeparam name="T">The target type for deserialization.</typeparam>
        /// <param name="options">Optional JSON serializer options; uses defaults when omitted.</param>
        /// <param name="ct">Optional cancellation token; uses <c>CancellationToken.None</c> when omitted.</param>
        /// <returns>An effect that produces the deserialized value, or fails if the connection is closed or the frame cannot be parsed.</returns>
        member this.ReceiveJson<'T>(?options: JsonSerializerOptions, ?ct: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())

                let! ct =
                    match ct with
                    | Some token -> FIO.succeed token
                    | None -> FIO.cancellationToken ()

                match! this.ReceiveMessage ct with
                | Frame(Text json) ->
                    return! FIO.attempt
                        (fun () -> JsonSerializer.Deserialize<'T>(json, opts))
                        WsError.fromException
                | Frame(Binary data) ->
                    return!
                        FIO.attempt
                            (fun () -> JsonSerializer.Deserialize<'T>(System.ReadOnlySpan<byte> data, opts))
                            WsError.fromException
                | Frame(Close _) ->
                    return!
                        FIO.fail (WsError.fromException (System.Exception "Connection closed while waiting for JSON"))
                | ConnectionClosed(status, desc) ->
                    return!
                        FIO.fail (
                            WsError.fromException (
                                System.Exception $"Connection closed. Status: {status}, Description: {desc}"
                            )
                        )
            }

        /// <summary>Creates an effect that sends a string as a text frame.</summary>
        /// <param name="text">The string to send.</param>
        /// <returns>An effect that completes when the text frame has been sent.</returns>
        member this.SendString(text: string) = this.SendText text

        /// <summary>Creates an effect that receives a text frame and returns its string content.</summary>
        /// <returns>An effect that produces the received text string, or fails if a non-text frame or close is received.</returns>
        member this.ReceiveString() =
            fio {
                match! this.ReceiveMessage() with
                | Frame(Text text) -> return text
                | Frame(Binary _) ->
                    return! FIO.fail (WsError.fromException (System.Exception "Expected text frame, got binary"))
                | Frame(Close _) ->
                    return! FIO.fail (WsError.fromException (System.Exception "Expected text frame, got close frame"))
                | ConnectionClosed(status, desc) ->
                    return!
                        FIO.fail (
                            WsError.fromException (
                                System.Exception $"Connection closed. Status: {status}, Description: {desc}"
                            )
                        )
            }

        /// <summary>Creates an effect that sends a byte array as a binary frame.</summary>
        /// <param name="data">The byte array to send.</param>
        /// <returns>An effect that completes when the binary frame has been sent.</returns>
        member this.SendBytes(data: byte[]) = this.SendBinary data

        /// <summary>Creates an effect that receives a binary frame and returns its byte content.</summary>
        /// <returns>An effect that produces the received byte array, or fails if a non-binary frame or close is received.</returns>
        member this.ReceiveBytes() =
            fio {
                match! this.ReceiveMessage() with
                | Frame(Binary data) -> return data
                | Frame(Text _) ->
                    return! FIO.fail (WsError.fromException (System.Exception "Expected binary frame, got text"))
                | Frame(Close _) ->
                    return! FIO.fail (WsError.fromException (System.Exception "Expected binary frame, got close frame"))
                | ConnectionClosed(status, desc) ->
                    return!
                        FIO.fail (
                            WsError.fromException (
                                System.Exception $"Connection closed. Status: {status}, Description: {desc}"
                            )
                        )
            }
