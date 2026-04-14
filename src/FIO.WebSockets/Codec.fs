namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Text.Json

/// <summary>
/// Codec for encoding and decoding values to/from WebSocket frames.
/// </summary>
type WebSocketCodec<'T> =
    {
        /// <summary>
        /// Encodes a value to a WebSocket frame.
        /// </summary>
        /// <returns>An FIO effect producing the encoded WebSocket frame.</returns>
        Encode: 'T -> FIO<WebSocketFrame, WsError>
        /// <summary>
        /// Decodes a WebSocket frame to a value.
        /// </summary>
        /// <returns>An FIO effect producing the decoded value.</returns>
        Decode: WebSocketFrame -> FIO<'T, WsError>
    }

/// <summary>
/// Codec builders and common codecs for WebSocket communications.
/// </summary>
module Codec =

    /// <summary>
    /// Identity codec for WebSocketFrame (no encoding/decoding).
    /// </summary>
    /// <returns>The identity frame codec.</returns>
    let frame: WebSocketCodec<WebSocketFrame> =
        {
            Encode = fun frame -> FIO.succeed frame
            Decode = fun frame -> FIO.succeed frame
        }

    /// <summary>
    /// Codec for byte arrays (binary frames).
    /// </summary>
    /// <returns>The binary frame codec.</returns>
    let binary: WebSocketCodec<byte[]> =
        {
            Encode = fun bytes -> FIO.succeed (Binary bytes)
            Decode =
                fun frame ->
                    match frame with
                    | Binary bytes -> FIO.succeed bytes
                    | Text _ -> FIO.fail (CodecError "Expected binary frame, got text frame")
                    | Close _ -> FIO.fail (CodecError "Expected binary frame, got close frame")
        }

    /// <summary>
    /// UTF-8 string codec (text frames).
    /// </summary>
    /// <returns>The text frame codec.</returns>
    let text: WebSocketCodec<string> =
        {
            Encode = fun str -> FIO.succeed (Text str)
            Decode =
                fun frame ->
                    match frame with
                    | Text str -> FIO.succeed str
                    | Binary _ -> FIO.fail (CodecError "Expected text frame, got binary frame")
                    | Close _ -> FIO.fail (CodecError "Expected text frame, got close frame")
        }

    /// <summary>
    /// JSON codec with custom options (sent as text frames).
    /// </summary>
    /// <typeparam name="T">The type to encode/decode.</typeparam>
    /// <param name="options">JSON serializer options.</param>
    /// <returns>A codec for JSON serialization.</returns>
    let jsonWithOptions<'T> (options: JsonSerializerOptions) =
        {
            Encode =
                fun value ->
                    FIO.attempt (
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, options)
                            Text json),
                        WsError.fromException
                    )
            Decode =
                fun frame ->
                    match frame with
                    | Text json ->
                        FIO.attempt ((fun () -> JsonSerializer.Deserialize<'T>(json, options)), WsError.fromException)
                    | Binary bytes ->
                        FIO.attempt (
                            (fun () ->
                                let json = Encoding.UTF8.GetString bytes
                                JsonSerializer.Deserialize<'T>(json, options)),
                            WsError.fromException
                        )
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as JSON")
        }

    /// <summary>
    /// JSON codec with default options.
    /// </summary>
    /// <returns>The JSON codec with default options.</returns>
    let json<'T> = jsonWithOptions<'T> (JsonSerializerOptions())

    /// <summary>
    /// Line-delimited JSON codec (text frame with newline appended).
    /// </summary>
    /// <typeparam name="T">The type to encode/decode.</typeparam>
    /// <param name="options">Optional JSON serializer options.</param>
    /// <returns>A codec for line-delimited JSON.</returns>
    let jsonLine<'T> (options: JsonSerializerOptions option) =
        let opts = defaultArg options (JsonSerializerOptions())

        {
            Encode =
                fun value ->
                    FIO.attempt (
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, opts)
                            Text(json + "\n")),
                        WsError.fromException
                    )
            Decode =
                fun frame ->
                    match frame with
                    | Text json ->
                        FIO.attempt (
                            (fun () ->
                                let trimmed = json.TrimEnd('\n', '\r')
                                JsonSerializer.Deserialize<'T>(trimmed, opts)),
                            WsError.fromException
                        )
                    | Binary bytes ->
                        FIO.attempt (
                            (fun () ->
                                let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                                JsonSerializer.Deserialize<'T>(json, opts)),
                            WsError.fromException
                        )
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as JSON line")
        }

    /// <summary>
    /// Maps a codec to a different type using bidirectional functions.
    /// </summary>
    /// <typeparam name="A">The source type.</typeparam>
    /// <typeparam name="B">The target type.</typeparam>
    /// <param name="f">Function to convert from 'A to 'B.</param>
    /// <param name="g">Function to convert from 'B to 'A.</param>
    /// <param name="codec">The source codec.</param>
    /// <returns>A new codec for the target type.</returns>
    let map (f: 'A -> 'B) (g: 'B -> 'A) codec =
        {
            Encode = fun b -> codec.Encode(g b)
            Decode =
                fun frame ->
                    fio {
                        let! a = codec.Decode frame
                        return f a
                    }
        }

    /// <summary>
    /// Composes two codecs to handle pairs (both values in same frame).
    /// </summary>
    /// <typeparam name="A">The type of the first element.</typeparam>
    /// <typeparam name="B">The type of the second element.</typeparam>
    /// <param name="codec1">Codec for the first element.</param>
    /// <param name="codec2">Codec for the second element.</param>
    /// <returns>A codec for tuple pairs encoded as JSON array.</returns>

    // TODO: This function seems to be broken.
    let compose (codec1: WebSocketCodec<'A>) (codec2: WebSocketCodec<'B>) =
        {
            Encode =
                fun (a, b) ->
                    FIO.attempt (
                        (fun () ->
                            // Serialize each value separately to preserve type info
                            let jsonA = JsonSerializer.Serialize(a)
                            let jsonB = JsonSerializer.Serialize(b)
                            let json = JsonSerializer.Serialize [| jsonA; jsonB |]
                            Text json),
                        WsError.fromException
                    )
            Decode =
                fun frame ->
                    match frame with
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as composed value")
                    | _ ->
                        FIO.attempt (
                            (fun () ->
                                let json =
                                    match frame with
                                    | Text str -> str
                                    | Binary bytes -> Encoding.UTF8.GetString bytes
                                    | Close _ -> "" // Unreachable due to match above

                                let doc = JsonDocument.Parse json
                                let arr = doc.RootElement.EnumerateArray() |> Seq.toList

                                if arr.Length <> 2 then
                                    raise (Exception $"Expected 2 elements, got {arr.Length}")

                                let jsonA =
                                    match arr.[0].ValueKind with
                                    | JsonValueKind.String -> arr.[0].GetString()
                                    | JsonValueKind.Null -> null
                                    | _ ->
                                        raise (Exception $"Expected string for first element, got {arr.[0].ValueKind}")

                                let jsonB =
                                    match arr.[1].ValueKind with
                                    | JsonValueKind.String -> arr.[1].GetString()
                                    | JsonValueKind.Null -> null
                                    | _ ->
                                        raise (
                                            Exception $"Expected string for second element, got {arr.[1].ValueKind}"
                                        )

                                let a = JsonSerializer.Deserialize<'A> jsonA
                                let b = JsonSerializer.Deserialize<'B> jsonB

                                a, b),
                            WsError.fromException
                        )
        }

    /// <summary>
    /// Creates a codec from encode/decode functions.
    /// </summary>
    /// <typeparam name="T">The type to encode/decode.</typeparam>
    /// <param name="encode">Encoding function.</param>
    /// <param name="decode">Decoding function.</param>
    /// <returns>A new codec.</returns>
    let create (encode: 'T -> FIO<WebSocketFrame, WsError>) (decode: WebSocketFrame -> FIO<'T, WsError>) =
        { Encode = encode; Decode = decode }

    /// <summary>
    /// Creates a codec from pure (non-FIO) encode/decode functions.
    /// </summary>
    /// <typeparam name="T">The type to encode/decode.</typeparam>
    /// <param name="encode">Pure encoding function.</param>
    /// <param name="decode">Pure decoding function.</param>
    /// <returns>A new codec wrapping the pure functions.</returns>
    let createPure (encode: 'T -> WebSocketFrame) (decode: WebSocketFrame -> 'T) =
        {
            Encode = fun value -> FIO.attempt ((fun () -> encode value), WsError.fromException)
            Decode = fun frame -> FIO.attempt ((fun () -> decode frame), WsError.fromException)
        }
