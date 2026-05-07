namespace FIO.Sockets

open FIO.DSL

open System.Text.Json

/// <summary>Builds Socket extension methods for JSON operations with custom serializer options.</summary>
[<AutoOpen>]
module SocketExtensions =

    type Socket with

        /// <summary>Creates an effect that serializes a value to JSON with optional custom settings and sends it over the connection.</summary>
        /// <param name="value">The value to serialize and send.</param>
        /// <param name="options">Optional JSON serializer options; defaults are used when omitted.</param>
        /// <returns>An effect that completes when the JSON-encoded value has been sent.</returns>
        member this.SendJson<'T>(value: 'T, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'T> opts
                do! this.Send(codec, value)
            }

        /// <summary>Creates an effect that receives bytes and deserializes them from JSON with optional custom settings.</summary>
        /// <param name="maxBytes">The maximum number of bytes to receive.</param>
        /// <param name="options">Optional JSON serializer options; defaults are used when omitted.</param>
        /// <returns>An effect that produces the deserialized value.</returns>
        member this.ReceiveJson<'T>(maxBytes: int, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'T> opts
                return! this.Receive(codec, maxBytes)
            }

        /// <summary>Creates an effect that serializes a value to newline-terminated JSON with optional custom settings and sends it over the connection.</summary>
        /// <param name="value">The value to serialize and send.</param>
        /// <param name="options">Optional JSON serializer options; defaults are used when omitted.</param>
        /// <returns>An effect that completes when the JSON line has been sent.</returns>
        member this.SendJsonLine<'T>(value: 'T, ?options) =
            fio {
                let codec = Codec.jsonLine<'T> options
                do! this.Send(codec, value)
            }

        /// <summary>Creates an effect that receives bytes and deserializes them from newline-terminated JSON with optional custom settings.</summary>
        /// <param name="maxBytes">The maximum number of bytes to receive.</param>
        /// <param name="options">Optional JSON serializer options; defaults are used when omitted.</param>
        /// <returns>An effect that produces the deserialized value.</returns>
        member this.ReceiveJsonLine<'T>(maxBytes: int, ?options) =
            fio {
                let codec = Codec.jsonLine<'T> options
                return! this.Receive(codec, maxBytes)
            }
