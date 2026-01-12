namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Text
open System.Text.Json

/// <summary>
/// Codec for encoding and decoding values to/from WebSocket frames.
/// </summary>
type WebSocketCodec<'T> =
    {
        /// Encodes a value to a WebSocket frame
        Encode: 'T -> FIO<WebSocketFrame, WsError>
        /// Decodes a WebSocket frame to a value
        Decode: WebSocketFrame -> FIO<'T, WsError>
    }

/// <summary>
/// Codec builders and common codecs for WebSocket communications.
/// </summary>
module Codec =

    /// <summary>
    /// Identity codec for WebSocketFrame (no encoding/decoding).
    /// </summary>
    let frame: WebSocketCodec<WebSocketFrame> =
        { Encode = fun frame -> FIO.Succeed frame
          Decode = fun frame -> FIO.Succeed frame }

    /// <summary>
    /// Codec for byte arrays (binary frames).
    /// </summary>
    let binary: WebSocketCodec<byte[]> =
        { Encode = fun bytes -> FIO.Succeed(Binary bytes)
          Decode = fun frame ->
            match frame with
            | Binary bytes -> FIO.Succeed bytes
            | Text _ -> FIO.Fail(CodecError "Expected binary frame, got text frame")
            | Close _ -> FIO.Fail(CodecError "Expected binary frame, got close frame") }

    /// <summary>
    /// UTF-8 string codec (text frames).
    /// </summary>
    let text: WebSocketCodec<string> =
        { Encode = fun str -> FIO.Succeed(Text str)
          Decode = fun frame ->
            match frame with
            | Text str -> FIO.Succeed str
            | Binary _ -> FIO.Fail(CodecError "Expected text frame, got binary frame")
            | Close _ -> FIO.Fail(CodecError "Expected text frame, got close frame") }

    /// <summary>
    /// JSON codec with custom options (sent as text frames).
    /// </summary>
    /// <param name="options">JSON serializer options.</param>
    let jsonWithOptions<'T> (options: JsonSerializerOptions) : WebSocketCodec<'T> =
        { Encode = fun value ->
            FIO.Attempt(
                (fun () ->
                    let json = JsonSerializer.Serialize(value, options)
                    Text json),
                WsError.FromException)
          Decode = fun frame ->
            match frame with
            | Text json ->
                FIO.Attempt(
                    (fun () -> JsonSerializer.Deserialize<'T>(json, options)),
                    WsError.FromException)
            | Binary bytes ->
                FIO.Attempt(
                    (fun () ->
                        let json = Encoding.UTF8.GetString bytes
                        JsonSerializer.Deserialize<'T>(json, options)),
                    WsError.FromException)
            | Close _ -> FIO.Fail(CodecError "Cannot decode close frame as JSON") }

    /// <summary>
    /// JSON codec with default options.
    /// </summary>
    let json<'T> : WebSocketCodec<'T> =
        jsonWithOptions<'T> (JsonSerializerOptions())

    /// <summary>
    /// Line-delimited JSON codec (text frame with newline appended).
    /// </summary>
    /// <param name="options">Optional JSON serializer options.</param>
    let jsonLine<'T> (options: JsonSerializerOptions option) : WebSocketCodec<'T> =
        let opts = defaultArg options (JsonSerializerOptions())

        { Encode = fun value ->
            FIO.Attempt(
                (fun () ->
                    let json = JsonSerializer.Serialize(value, opts)
                    Text(json + "\n")),
                WsError.FromException)
          Decode = fun frame ->
            match frame with
            | Text json ->
                FIO.Attempt(
                    (fun () ->
                        let trimmed = json.TrimEnd('\n', '\r')
                        JsonSerializer.Deserialize<'T>(trimmed, opts)),
                    WsError.FromException)
            | Binary bytes ->
                FIO.Attempt(
                    (fun () ->
                        let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                        JsonSerializer.Deserialize<'T>(json, opts)),
                    WsError.FromException)
            | Close _ -> FIO.Fail(CodecError "Cannot decode close frame as JSON line") }

    /// <summary>
    /// Maps a codec to a different type using bidirectional functions.
    /// </summary>
    /// <param name="f">Function to convert from 'A to 'B.</param>
    /// <param name="g">Function to convert from 'B to 'A.</param>
    /// <param name="codec">The source codec.</param>
    let map (f: 'A -> 'B) (g: 'B -> 'A) (codec: WebSocketCodec<'A>) : WebSocketCodec<'B> =
        { Encode = fun b -> codec.Encode(g b)
          Decode = fun frame ->
            fio {
                let! a = codec.Decode frame
                return f a
            } }

    /// <summary>
    /// Composes two codecs to handle pairs (both values in same frame).
    /// Encodes as JSON array: [a, b]
    /// </summary>
    /// <param name="codec1">Codec for the first element.</param>
    /// <param name="codec2">Codec for the second element.</param>
    let compose (codec1: WebSocketCodec<'A>) (codec2: WebSocketCodec<'B>) : WebSocketCodec<'A * 'B> =
        { Encode = fun (a, b) ->
            FIO.Attempt(
                (fun () ->
                    // Serialize each value separately to preserve type info
                    let jsonA = JsonSerializer.Serialize(a)
                    let jsonB = JsonSerializer.Serialize(b)
                    let json = JsonSerializer.Serialize [| jsonA; jsonB |]
                    Text json),
                WsError.FromException)
          Decode = fun frame ->
            FIO.Attempt(
                (fun () ->
                    let json =
                        match frame with
                        | Text str -> str
                        | Binary bytes -> Encoding.UTF8.GetString bytes
                        | Close _ -> raise (Exception "Cannot decode close frame")

                    let doc = JsonDocument.Parse json
                    let arr = doc.RootElement.EnumerateArray() |> Seq.toList

                    if arr.Length <> 2 then
                        raise (Exception $"Expected 2 elements, got {arr.Length}")

                    // Decode each element as a separate JSON string
                    let jsonA =
                        match arr.[0].ValueKind with
                        | JsonValueKind.String -> arr.[0].GetString()
                        | JsonValueKind.Null -> null
                        | _ -> raise (Exception $"Expected string for first element, got {arr.[0].ValueKind}")

                    let jsonB =
                        match arr.[1].ValueKind with
                        | JsonValueKind.String -> arr.[1].GetString()
                        | JsonValueKind.Null -> null
                        | _ -> raise (Exception $"Expected string for second element, got {arr.[1].ValueKind}")

                    let a = JsonSerializer.Deserialize<'A> jsonA
                    let b = JsonSerializer.Deserialize<'B> jsonB

                    a, b),
                WsError.FromException) }

    /// <summary>
    /// Creates a codec from encode/decode functions.
    /// </summary>
    /// <param name="encode">Encoding function.</param>
    /// <param name="decode">Decoding function.</param>
    let create
        (encode: 'T -> FIO<WebSocketFrame, WsError>)
        (decode: WebSocketFrame -> FIO<'T, WsError>)
        : WebSocketCodec<'T> =
        { Encode = encode; Decode = decode }

    /// <summary>
    /// Creates a codec from pure (non-FIO) encode/decode functions.
    /// </summary>
    /// <param name="encode">Pure encoding function.</param>
    /// <param name="decode">Pure decoding function.</param>
    let createPure (encode: 'T -> WebSocketFrame) (decode: WebSocketFrame -> 'T) : WebSocketCodec<'T> =
        { Encode = fun value ->
            FIO.Attempt((fun () -> encode value), WsError.FromException)
          Decode = fun frame ->
            FIO.Attempt((fun () -> decode frame), WsError.FromException) }
