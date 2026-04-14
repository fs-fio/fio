namespace FIO.WebSockets

open FIO.DSL

open System.Text.Json
open System.Threading

/// <summary>
/// Extension methods for JSON serialization and other conveniences over WebSockets.
/// </summary>
[<AutoOpen>]
module WebSocketExtensions =

    type WebSocket with

        /// <summary>
        /// Sends a value as JSON over the WebSocket.
        /// </summary>
        /// <param name="value">The value to send.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <param name="cancellationToken">Optional cancellation token.</param>
        member this.SendJson<'T>(value: 'T, ?options: JsonSerializerOptions, ?ct: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let ct = defaultArg ct CancellationToken.None
                let! jsonString = FIO.attempt ((fun () -> JsonSerializer.Serialize(value, opts)), WsError.fromException)
                do! this.SendText(jsonString, ct)
            }

        /// <summary>
        /// Receives and deserializes a JSON value from the WebSocket.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <param name="cancellationToken">Optional cancellation token.</param>
        /// <returns>The deserialized value.</returns>
        member this.ReceiveJson<'T>(?options: JsonSerializerOptions, ?ct: CancellationToken) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let ct = defaultArg ct CancellationToken.None

                match! this.ReceiveMessage ct with
                | Frame(Text json) ->
                    return! FIO.attempt ((fun () -> JsonSerializer.Deserialize<'T>(json, opts)), WsError.fromException)
                | Frame(Binary data) ->
                    return!
                        FIO.attempt (
                            (fun () -> JsonSerializer.Deserialize<'T>(System.ReadOnlySpan<byte> data, opts)),
                            WsError.fromException
                        )
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

        /// <summary>
        /// Sends a string as a text frame (convenience method).
        /// </summary>
        /// <param name="text">The string to send.</param>
        member this.SendString(text: string) = this.SendText text

        /// <summary>
        /// Receives a text frame as a string (convenience method).
        /// </summary>
        /// <returns>The received text string.</returns>
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

        /// <summary>
        /// Sends binary data (convenience method).
        /// </summary>
        /// <param name="data">The binary data to send.</param>
        member this.SendBytes(data: byte[]) = this.SendBinary data

        /// <summary>
        /// Receives binary data (convenience method).
        /// </summary>
        /// <returns>The received binary data.</returns>
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
