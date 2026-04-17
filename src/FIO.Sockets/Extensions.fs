namespace FIO.Sockets

open FIO.DSL

open System.Text.Json

/// Extension methods for TCP sockets providing additional convenience operations.
[<AutoOpen>]
module SocketExtensions =

    type Socket with

        /// Sends a value as JSON with custom serializer options.
        /// <param name="value">The value to send.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <returns>Effect that sends the JSON value.</returns>
        member this.SendJson<'T>(value: 'T, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'T> opts
                do! this.Send(codec, value)
            }

        /// Receives and deserializes JSON with custom serializer options.
        /// <param name="maxBytes">Maximum number of bytes to receive.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <returns>The deserialized JSON value.</returns>
        member this.ReceiveJson<'T>(maxBytes: int, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'T> opts
                return! this.Receive(codec, maxBytes)
            }

        /// Sends line-delimited JSON with custom serializer options.
        /// <param name="value">The value to send.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <returns>Effect that sends the JSON line value.</returns>
        member this.SendJsonLine<'T>(value: 'T, ?options) =
            fio {
                let codec = Codec.jsonLine<'T> options
                do! this.Send(codec, value)
            }

        /// Receives line-delimited JSON with custom serializer options.
        /// <param name="maxBytes">Maximum number of bytes to receive.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <returns>The deserialized JSON value.</returns>
        member this.ReceiveJsonLine<'T>(maxBytes: int, ?options) =
            fio {
                let codec = Codec.jsonLine<'T> options
                return! this.Receive(codec, maxBytes)
            }
