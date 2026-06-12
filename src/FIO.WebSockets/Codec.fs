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

    let private defaultJsonOptions =
        let options = JsonSerializerOptions()
        options.MakeReadOnly true
        options

    /// <summary>Creates a JSON codec that serializes values as text frames using default serializer options.</summary>
    /// <returns>A codec that encodes values as JSON text frames and decodes text or binary frames as JSON.</returns>
    let json<'T> = jsonWithOptions<'T> defaultJsonOptions

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
    /// <typeparam name="T">The source codec's value type.</typeparam>
    /// <typeparam name="T1">The target value type.</typeparam>
    /// <param name="f">A function that converts a decoded source value to the target type.</param>
    /// <param name="g">A function that converts a target value back to the source type for encoding.</param>
    /// <param name="codec">The source codec to transform.</param>
    /// <returns>A codec that encodes and decodes values of the target type.</returns>
    let map (f: 'T -> 'T1) (g: 'T1 -> 'T) codec =
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
    /// <typeparam name="T">The type of the first element.</typeparam>
    /// <typeparam name="T1">The type of the second element.</typeparam>
    /// <param name="codec1">The codec for the first element of the pair.</param>
    /// <param name="codec2">The codec for the second element of the pair.</param>
    /// <returns>A codec that encodes pairs as JSON array text frames and decodes them back to tuples.</returns>

    let compose (codec1: WebSocketCodec<'T>) (codec2: WebSocketCodec<'T1>) : WebSocketCodec<'T * 'T1> =
        let framePart (frame: WebSocketFrame) =
            match frame with
            | Text str -> {| kind = "text"; value = str |}
            | Binary bytes -> {| kind = "binary"; value = Convert.ToBase64String bytes |}
            | Close _ -> raise (Exception "Cannot compose a close frame")

        let partFrame (element: JsonElement) =
            let kind = element.GetProperty("kind").GetString()
            let value = element.GetProperty("value").GetString()

            match kind with
            | "text" -> Text value
            | "binary" -> Binary(Convert.FromBase64String value)
            | other -> raise (Exception $"Unknown composed frame kind: {other}")

        {
            Encode =
                fun (a, b) ->
                    fio {
                        let! frameA = codec1.Encode a
                        let! frameB = codec2.Encode b

                        return!
                            FIO.attempt
                                (fun () -> Text(JsonSerializer.Serialize [| framePart frameA; framePart frameB |]))
                                WsError.fromException
                    }
            Decode =
                fun frame ->
                    fio {
                        let! json =
                            match frame with
                            | Text str -> FIO.succeed str
                            | Binary bytes -> FIO.succeed (Encoding.UTF8.GetString bytes)
                            | Close _ -> FIO.fail (CodecError "Cannot decode close frame as composed value")

                        let! frameA, frameB =
                            FIO.attempt
                                (fun () ->
                                    use doc = JsonDocument.Parse json
                                    let elements = doc.RootElement.EnumerateArray() |> Seq.toArray

                                    if elements.Length <> 2 then
                                        raise (Exception $"Expected 2 composed elements, got {elements.Length}")

                                    partFrame elements.[0], partFrame elements.[1])
                                WsError.fromException

                        let! a = codec1.Decode frameA
                        let! b = codec2.Decode frameB
                        return a, b
                    }
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
