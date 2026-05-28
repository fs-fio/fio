namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Text.Json

/// <summary>Represents a codec for encoding values to and decoding values from WebSocket frames.</summary>
type WebSocketCodec<'T> =
    {
        /// <summary>Represents the function that encodes a value into a WebSocket frame effect.</summary>
        Encode: 'T -> FIO<WebSocketFrame, WsError>
        /// <summary>Represents the function that decodes a WebSocket frame into a value effect.</summary>
        Decode: WebSocketFrame -> FIO<'T, WsError>
    }

/// <summary>Builds and combines codecs for WebSocket frame encoding and decoding.</summary>
module Codec =

    /// <summary>Returns the identity codec that passes WebSocket frames through without transformation.</summary>
    /// <returns>A codec where both encoding and decoding return the frame unchanged.</returns>
    let frame: WebSocketCodec<WebSocketFrame> =
        {
            Encode = fun frame -> FIO.succeed frame
            Decode = fun frame -> FIO.succeed frame
        }

    /// <summary>Returns a codec that encodes byte arrays as binary frames and decodes binary frames back to byte arrays.</summary>
    /// <returns>A codec that fails with <c>CodecError</c> when a non-binary frame is received.</returns>
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

    /// <summary>Returns a codec that encodes strings as UTF-8 text frames and decodes text frames back to strings.</summary>
    /// <returns>A codec that fails with <c>CodecError</c> when a non-text frame is received.</returns>
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

    /// <summary>Creates a JSON codec that serializes values as text frames using the specified serializer options.</summary>
    /// <typeparam name="T">The type to serialize and deserialize.</typeparam>
    /// <param name="options">The JSON serializer options to use for encoding and decoding.</param>
    /// <returns>A codec that encodes values as JSON text frames and decodes text or binary frames as JSON.</returns>
    let jsonWithOptions<'T> (options: JsonSerializerOptions) =
        {
            Encode =
                fun value ->
                    FIO.attempt
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, options)
                            Text json)
                        WsError.fromException
            Decode =
                fun frame ->
                    match frame with
                    | Text json ->
                        FIO.attempt
                            (fun () ->
                                JsonSerializer.Deserialize<'T>(json, options))
                            WsError.fromException
                    | Binary bytes ->
                        FIO.attempt
                            (fun () ->
                                let json = Encoding.UTF8.GetString bytes
                                JsonSerializer.Deserialize<'T>(json, options))
                            WsError.fromException
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as JSON")
        }

    /// <summary>Creates a JSON codec that serializes values as text frames using default serializer options.</summary>
    /// <returns>A codec that encodes values as JSON text frames and decodes text or binary frames as JSON.</returns>
    let json<'T> = jsonWithOptions<'T> (JsonSerializerOptions())

    /// <summary>Creates a line-delimited JSON codec that appends a newline to each encoded value.</summary>
    /// <typeparam name="T">The type to serialize and deserialize.</typeparam>
    /// <param name="options">Optional JSON serializer options; uses defaults when <c>None</c>.</param>
    /// <returns>A codec that encodes values as newline-terminated JSON text frames and trims trailing newlines on decode.</returns>
    let jsonLine<'T> (options: JsonSerializerOptions option) =
        let opts = defaultArg options (JsonSerializerOptions())

        {
            Encode =
                fun value ->
                    FIO.attempt
                        (fun () ->
                            let json = JsonSerializer.Serialize(value, opts)
                            Text(json + "\n"))
                        WsError.fromException
            Decode =
                fun frame ->
                    match frame with
                    | Text json ->
                        FIO.attempt
                            (fun () ->
                                let trimmed = json.TrimEnd('\n', '\r')
                                JsonSerializer.Deserialize<'T>(trimmed, opts))
                            WsError.fromException
                    | Binary bytes ->
                        FIO.attempt
                            (fun () ->
                                let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                                JsonSerializer.Deserialize<'T>(json, opts))
                            WsError.fromException
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as JSON line")
        }

    /// <summary>Transforms a codec into one for a different type using bidirectional mapping functions.</summary>
    /// <typeparam name="A">The source codec's value type.</typeparam>
    /// <typeparam name="B">The target value type.</typeparam>
    /// <param name="f">A function that converts a decoded source value to the target type.</param>
    /// <param name="g">A function that converts a target value back to the source type for encoding.</param>
    /// <param name="codec">The source codec to transform.</param>
    /// <returns>A codec that encodes and decodes values of the target type.</returns>
    let map (f: 'M -> 'B) (g: 'B -> 'M) codec =
        {
            Encode = fun b -> codec.Encode(g b)
            Decode =
                fun frame ->
                    fio {
                        let! a = codec.Decode frame
                        return f a
                    }
        }

    /// <summary>Combines two codecs into one that encodes and decodes pairs as a two-element JSON array.</summary>
    /// <typeparam name="A">The type of the first element.</typeparam>
    /// <typeparam name="B">The type of the second element.</typeparam>
    /// <param name="codec1">The codec for the first element of the pair.</param>
    /// <param name="codec2">The codec for the second element of the pair.</param>
    /// <returns>A codec that encodes pairs as JSON array text frames and decodes them back to tuples.</returns>

    // TODO: This function seems to be broken.
    let compose (codec1: WebSocketCodec<'M>) (codec2: WebSocketCodec<'B>) =
        {
            Encode =
                fun (a, b) ->
                    FIO.attempt
                        (fun () ->
                            let jsonA = JsonSerializer.Serialize(a)
                            let jsonB = JsonSerializer.Serialize(b)
                            let json = JsonSerializer.Serialize [| jsonA; jsonB |]
                            Text json)
                        WsError.fromException
            Decode =
                fun frame ->
                    match frame with
                    | Close _ -> FIO.fail (CodecError "Cannot decode close frame as composed value")
                    | _ ->
                        FIO.attempt
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

                                let a = JsonSerializer.Deserialize<'M> jsonA
                                let b = JsonSerializer.Deserialize<'B> jsonB

                                a, b)
                            WsError.fromException
        }

    /// <summary>Creates a codec from effectful encode and decode functions.</summary>
    /// <typeparam name="T">The value type to encode and decode.</typeparam>
    /// <param name="encode">A function that encodes a value into a WebSocket frame effect.</param>
    /// <param name="decode">A function that decodes a WebSocket frame into a value effect.</param>
    /// <returns>A codec that uses the supplied encode and decode functions.</returns>
    let create (encode: 'T -> FIO<WebSocketFrame, WsError>) (decode: WebSocketFrame -> FIO<'T, WsError>) =
        { Encode = encode; Decode = decode }

    /// <summary>Creates a codec from pure encode and decode functions, catching exceptions as <c>WsError</c>.</summary>
    /// <typeparam name="T">The value type to encode and decode.</typeparam>
    /// <param name="encode">A pure function that converts a value to a WebSocket frame.</param>
    /// <param name="decode">A pure function that converts a WebSocket frame to a value.</param>
    /// <returns>A codec that wraps the pure functions in effects with error handling.</returns>
    let createPure (encode: 'T -> WebSocketFrame) (decode: WebSocketFrame -> 'T) =
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () -> encode value)
                    WsError.fromException
            Decode = fun frame ->
                FIO.attempt
                    (fun () -> decode frame)
                    WsError.fromException
        }
