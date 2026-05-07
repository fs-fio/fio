namespace FIO.Sockets

open FIO.DSL

open System
open System.Text
open System.Text.Json

/// <summary>Represents a codec for encoding and decoding messages to and from bytes.</summary>
/// <typeparam name="'T">The message type to encode and decode.</typeparam>
type SocketCodec<'T> =
    {
        /// <summary>Represents the encoding function from a value to bytes.</summary>
        Encode: 'T -> FIO<byte[], SocketError>
        /// <summary>Represents the decoding function from bytes to a value.</summary>
        Decode: byte[] -> FIO<'T, SocketError>
    }

/// <summary>Creates codecs for encoding and decoding socket messages.</summary>
module Codec =

    /// <summary>Returns the identity codec for byte arrays that passes data through unchanged.</summary>
    /// <returns>A codec that succeeds immediately with the input bytes on both encode and decode.</returns>
    let bytes: SocketCodec<byte[]> =
        {
            Encode = fun bytes -> FIO.succeed bytes
            Decode = fun bytes -> FIO.succeed bytes
        }

    /// <summary>Returns the UTF-8 string codec.</summary>
    /// <returns>A codec that encodes strings to UTF-8 bytes and decodes UTF-8 bytes to strings.</returns>
    let string: SocketCodec<string> =
        {
            Encode =
                fun str ->
                    FIO.attempt (
                        (fun () -> Encoding.UTF8.GetBytes str),
                        fun exn -> CodecError("UTF-8 encoding failed", exn)
                    )
            Decode =
                fun bytes ->
                    FIO.attempt (
                        (fun () -> Encoding.UTF8.GetString bytes),
                        fun exn -> CodecError("UTF-8 decoding failed", exn)
                    )
        }

    /// <summary>Returns the line-delimited string codec that appends a newline on encode and trims it on decode.</summary>
    /// <returns>A codec that frames strings with newline delimiters using UTF-8 encoding.</returns>
    let line: SocketCodec<string> =
        {
            Encode =
                fun str ->
                    fio {
                        let! encoded = string.Encode(str + "\n")
                        return encoded
                    }
            Decode =
                fun bytes ->
                    fio {
                        let! decoded = string.Decode bytes
                        return decoded.TrimEnd('\n', '\r')
                    }
        }

    /// <summary>Creates a JSON codec using the specified serializer options.</summary>
    /// <param name="options">The JSON serializer options to use for encoding and decoding.</param>
    /// <returns>A codec that serializes values to JSON bytes and deserializes JSON bytes to values.</returns>
    let jsonWithOptions<'T> (options: JsonSerializerOptions) =
        {
            Encode =
                fun value ->
                    FIO.attempt (
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, options)
                            Encoding.UTF8.GetBytes json),
                        fun exn -> CodecError("JSON encoding failed", exn)
                    )
            Decode =
                fun bytes ->
                    FIO.attempt (
                        (fun () ->
                            let json = Encoding.UTF8.GetString bytes
                            JsonSerializer.Deserialize<'T>(json, options)),
                        fun exn -> CodecError("JSON decoding failed", exn)
                    )
        }

    /// <summary>Returns the JSON codec using default serializer options.</summary>
    /// <returns>A codec that serializes values to JSON bytes and deserializes JSON bytes to values.</returns>
    let json<'T> = jsonWithOptions<'T> (JsonSerializerOptions())

    /// <summary>Creates a line-delimited JSON codec that appends a newline after each JSON value.</summary>
    /// <param name="options">Optional JSON serializer options; defaults are used when None.</param>
    /// <returns>A codec that serializes values to newline-terminated JSON bytes and deserializes them back.</returns>
    let jsonLine<'T> options =
        let opts = defaultArg options (JsonSerializerOptions())

        {
            Encode =
                fun value ->
                    FIO.attempt (
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, opts)
                            Encoding.UTF8.GetBytes(json + "\n")),
                        fun exn -> CodecError("JSON line encoding failed", exn)
                    )
            Decode =
                fun bytes ->
                    FIO.attempt (
                        (fun () ->
                            let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                            JsonSerializer.Deserialize<'T>(json, opts)),
                        fun exn -> CodecError("JSON line decoding failed", exn)
                    )
        }

    /// <summary>Transforms a codec to a different type using bidirectional conversion functions.</summary>
    /// <param name="f">A function that converts a decoded value from the source type to the target type.</param>
    /// <param name="g">A function that converts a value from the target type back to the source type for encoding.</param>
    /// <param name="codec">The source codec to transform.</param>
    /// <returns>A codec that encodes via <paramref name="g"/> then the source codec, and decodes via the source codec then <paramref name="f"/>.</returns>
    let map (f: 'A -> 'B) (g: 'B -> 'A) (codec: SocketCodec<'A>) =
        {
            Encode = fun b -> codec.Encode(g b)
            Decode =
                fun bytes ->
                    fio {
                        let! a = codec.Decode bytes
                        return f a
                    }
        }

    /// <summary>Combines two codecs into one that encodes and decodes pairs using length-prefixed framing.</summary>
    /// <param name="codec1">The codec for the first element of the pair.</param>
    /// <param name="codec2">The codec for the second element of the pair.</param>
    /// <returns>A codec that encodes pairs as length-prefixed concatenated payloads and decodes them back into tuples.</returns>
    let compose (codec1: SocketCodec<'A>) (codec2: SocketCodec<'B>) =
        {
            Encode =
                fun (a, b) ->
                    fio {
                        let! bytes1 = codec1.Encode a
                        let! bytes2 = codec2.Encode b

                        let len1Bytes = BitConverter.GetBytes bytes1.Length
                        let len2Bytes = BitConverter.GetBytes bytes2.Length

                        return Array.concat [ len1Bytes; bytes1; len2Bytes; bytes2 ]
                    }
            Decode =
                fun bytes ->
                    fio {
                        if bytes.Length < 8 then
                            return!
                                FIO.fail (
                                    CodecError(
                                        "Insufficient bytes for pair decoding (need at least 8 bytes for length prefixes)",
                                        Exception()
                                    )
                                )

                        let len1 = BitConverter.ToInt32(bytes, 0)

                        if bytes.Length < 4 + len1 + 4 then
                            return!
                                FIO.fail (
                                    CodecError(
                                        $"Incomplete first payload: expected {len1} bytes plus second length prefix",
                                        Exception()
                                    )
                                )

                        let bytes1 = bytes.[4 .. 4 + len1 - 1]

                        let len2 = BitConverter.ToInt32(bytes, 4 + len1)

                        if bytes.Length < 4 + len1 + 4 + len2 then
                            return!
                                FIO.fail (CodecError($"Incomplete second payload: expected {len2} bytes", Exception()))

                        let bytes2 = bytes.[4 + len1 + 4 .. 4 + len1 + 4 + len2 - 1]

                        let! a = codec1.Decode bytes1
                        let! b = codec2.Decode bytes2
                        return a, b
                    }
        }

    /// <summary>Transforms a codec by adding a four-byte length prefix to each encoded payload.</summary>
    /// <param name="innerCodec">The codec to wrap with length-prefixed framing.</param>
    /// <returns>A codec that prepends a four-byte length header on encode and strips it on decode.</returns>
    let lengthPrefixed<'T> (innerCodec: SocketCodec<'T>) =
        {
            Encode =
                fun value ->
                    fio {
                        let! payload = innerCodec.Encode value
                        let lengthBytes = BitConverter.GetBytes payload.Length
                        return Array.append lengthBytes payload
                    }
            Decode =
                fun bytes ->
                    fio {
                        if bytes.Length < 4 then
                            return! FIO.fail (CodecError("Insufficient bytes for length prefix", Exception()))

                        let length = BitConverter.ToInt32(bytes, 0)
                        let payload = bytes.[4..]

                        if payload.Length < length then
                            return!
                                FIO.fail (
                                    CodecError(
                                        $"Incomplete payload: expected {length}, got {payload.Length}",
                                        Exception()
                                    )
                                )

                        return! innerCodec.Decode payload.[0 .. length - 1]
                    }
        }

    /// <summary>Creates a codec from effectful encode and decode functions.</summary>
    /// <param name="encode">A function that encodes a value into a byte array effect.</param>
    /// <param name="decode">A function that decodes a byte array into a value effect.</param>
    /// <returns>A codec wrapping the given encode and decode functions.</returns>
    let create (encode: 'T -> FIO<byte[], SocketError>) (decode: byte[] -> FIO<'T, SocketError>) =
        { Encode = encode; Decode = decode }

    /// <summary>Creates a codec from pure encode and decode functions, wrapping exceptions as CodecError.</summary>
    /// <param name="encode">A pure function that encodes a value into a byte array.</param>
    /// <param name="decode">A pure function that decodes a byte array into a value.</param>
    /// <returns>A codec that lifts the pure functions into effects with automatic error handling.</returns>
    let createPure (encode: 'T -> byte[]) (decode: byte[] -> 'T) =
        {
            Encode = fun value -> FIO.attempt ((fun () -> encode value), fun exn -> CodecError("Encoding failed", exn))
            Decode = fun bytes -> FIO.attempt ((fun () -> decode bytes), fun exn -> CodecError("Decoding failed", exn))
        }
