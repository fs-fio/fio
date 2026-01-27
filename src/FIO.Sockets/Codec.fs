namespace FIO.Sockets

open FIO.DSL

open System
open System.Text
open System.Text.Json

/// <summary>
/// Codec for encoding and decoding messages to/from bytes.
/// </summary>
type SocketCodec<'T> =
    {
        /// <summary>
        /// Encodes a value to bytes.
        /// </summary>
        Encode: 'T -> FIO<byte[], SocketError>
        /// <summary>
        /// Decodes bytes to a value.
        /// </summary>
        Decode: byte[] -> FIO<'T, SocketError>
    }

/// <summary>
/// Codec builders and common codecs.
/// </summary>
module Codec =

    /// <summary>
    /// Identity codec for byte arrays (no encoding/decoding).
    /// </summary>
    let bytes: SocketCodec<byte[]> =
        { Encode = fun bytes -> FIO.succeed bytes
          Decode = fun bytes -> FIO.succeed bytes }

    /// <summary>
    /// UTF-8 string codec.
    /// </summary>
    let string: SocketCodec<string> =
        { Encode = fun str ->
            FIO.attempt(
                (fun () -> Encoding.UTF8.GetBytes str),
                fun exn -> CodecError("UTF-8 encoding failed", exn))
          Decode = fun bytes ->
            FIO.attempt(
                (fun () -> Encoding.UTF8.GetString bytes),
                fun exn -> CodecError("UTF-8 decoding failed", exn)) }

    /// <summary>
    /// Line-delimited string codec (adds/removes newline).
    /// </summary>
    let line: SocketCodec<string> =
        { Encode = fun str ->
            fio {
                let! encoded = string.Encode(str + "\n")
                return encoded
            }
          Decode = fun bytes ->
            fio {
                let! decoded = string.Decode bytes
                return decoded.TrimEnd('\n', '\r')
            } }

    /// <summary>
    /// JSON codec with custom options.
    /// </summary>
    /// <param name="options">JSON serializer options.</param>
    /// <returns>The JSON codec.</returns>
    let jsonWithOptions<'T> (options: JsonSerializerOptions) : SocketCodec<'T> =
        { Encode = fun value ->
            FIO.attempt((fun () ->
                let json = JsonSerializer.Serialize(value, options)
                Encoding.UTF8.GetBytes json),
                fun exn -> CodecError("JSON encoding failed", exn))
          Decode = fun bytes ->
            FIO.attempt((fun () ->
                let json = Encoding.UTF8.GetString bytes
                JsonSerializer.Deserialize<'T>(json, options)),
                fun exn -> CodecError("JSON decoding failed", exn)) }

    /// <summary>
    /// JSON codec with default options.
    /// </summary>
    let json<'T> : SocketCodec<'T> =
        jsonWithOptions<'T> (JsonSerializerOptions())

    /// <summary>
    /// Line-delimited JSON codec.
    /// </summary>
    /// <param name="options">Optional JSON serializer options.</param>
    /// <returns>The line-delimited JSON codec.</returns>
    let jsonLine<'T> (options: JsonSerializerOptions option) : SocketCodec<'T> =
        let opts = defaultArg options (JsonSerializerOptions())
        { Encode = fun value ->
            FIO.attempt((fun () ->
                let json = JsonSerializer.Serialize(value, opts)
                Encoding.UTF8.GetBytes(json + "\n")),
                fun exn -> CodecError("JSON line encoding failed", exn))
          Decode = fun bytes ->
            FIO.attempt((fun () ->
                let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                JsonSerializer.Deserialize<'T>(json, opts)),
                fun exn -> CodecError("JSON line decoding failed", exn)) }

    /// <summary>
    /// Maps a codec to a different type using bidirectional functions.
    /// </summary>
    /// <param name="f">Function to convert from 'A to 'B.</param>
    /// <param name="g">Function to convert from 'B to 'A.</param>
    /// <param name="codec">The source codec.</param>
    /// <returns>The mapped codec.</returns>
    let map (f: 'A -> 'B) (g: 'B -> 'A) (codec: SocketCodec<'A>) : SocketCodec<'B> =
        { Encode = fun b -> codec.Encode(g b)
          Decode = fun bytes ->
            fio {
                let! a = codec.Decode bytes
                return f a
            } }

    /// <summary>
    /// Composes two codecs to handle pairs using length-prefixed framing.
    /// </summary>
    /// <param name="codec1">Codec for the first element.</param>
    /// <param name="codec2">Codec for the second element.</param>
    /// <returns>The composed codec for pairs.</returns>
    let compose (codec1: SocketCodec<'A>) (codec2: SocketCodec<'B>) : SocketCodec<'A * 'B> =
        { Encode = fun (a, b) ->
            fio {
                let! bytes1 = codec1.Encode a
                let! bytes2 = codec2.Encode b

                // Encode with length prefixes: [len1:4bytes][payload1][len2:4bytes][payload2]
                let len1Bytes = BitConverter.GetBytes bytes1.Length
                let len2Bytes = BitConverter.GetBytes bytes2.Length

                return Array.concat [len1Bytes; bytes1; len2Bytes; bytes2]
            }
          Decode = fun bytes ->
            fio {
                if bytes.Length < 8 then
                    return! FIO.fail(
                        CodecError("Insufficient bytes for pair decoding (need at least 8 bytes for length prefixes)", Exception()))

                // Read first length prefix
                let len1 = BitConverter.ToInt32(bytes, 0)

                if bytes.Length < 4 + len1 + 4 then
                    return! FIO.fail(
                        CodecError($"Incomplete first payload: expected {len1} bytes plus second length prefix", Exception()))

                // Extract first payload
                let bytes1 = bytes.[4 .. 4 + len1 - 1]

                // Read second length prefix
                let len2 = BitConverter.ToInt32(bytes, 4 + len1)

                if bytes.Length < 4 + len1 + 4 + len2 then
                    return! FIO.fail(
                        CodecError($"Incomplete second payload: expected {len2} bytes", Exception()))

                // Extract second payload
                let bytes2 = bytes.[4 + len1 + 4 .. 4 + len1 + 4 + len2 - 1]

                let! a = codec1.Decode bytes1
                let! b = codec2.Decode bytes2
                return a, b
            } }

    /// <summary>
    /// Length-prefixed framing codec (4-byte length header).
    /// </summary>
    /// <param name="innerCodec">The codec to wrap with length-prefixing.</param>
    /// <returns>The length-prefixed codec.</returns>
    let lengthPrefixed<'T> (innerCodec: SocketCodec<'T>) : SocketCodec<'T> =
        { Encode = fun value ->
            fio {
                let! payload = innerCodec.Encode value
                let lengthBytes = BitConverter.GetBytes payload.Length
                return Array.append lengthBytes payload
            }
          Decode = fun bytes ->
            fio {
                if bytes.Length < 4 then
                    return! FIO.fail(
                        CodecError("Insufficient bytes for length prefix", Exception()))

                let length = BitConverter.ToInt32(bytes, 0)
                let payload = bytes.[4..]

                if payload.Length < length then
                    return! FIO.fail(
                        CodecError($"Incomplete payload: expected {length}, got {payload.Length}", Exception()))

                return! innerCodec.Decode payload.[0 .. length - 1]
            } }

    /// <summary>
    /// Creates a codec from encode/decode functions.
    /// </summary>
    /// <param name="encode">Encoding function.</param>
    /// <param name="decode">Decoding function.</param>
    /// <returns>The created codec.</returns>
    let create (encode: 'T -> FIO<byte[], SocketError>) (decode: byte[] -> FIO<'T, SocketError>) : SocketCodec<'T> =
        { Encode = encode; Decode = decode }

    /// <summary>
    /// Creates a codec from pure (non-FIO) encode/decode functions.
    /// </summary>
    /// <param name="encode">Pure encoding function.</param>
    /// <param name="decode">Pure decoding function.</param>
    /// <returns>The created codec.</returns>
    let createPure (encode: 'T -> byte[]) (decode: byte[] -> 'T) : SocketCodec<'T> =
        { Encode = fun value ->
            FIO.attempt(
                (fun () -> encode value),
                fun exn -> CodecError("Encoding failed", exn))
          Decode = fun bytes ->
            FIO.attempt(
                (fun () -> decode bytes),
                fun exn -> CodecError("Decoding failed", exn)) }
