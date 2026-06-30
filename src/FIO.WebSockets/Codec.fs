namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Text.Json

/// Encodes and decodes values of a given type to and from WebSocket frames.
type WebSocketCodec<'A> =
    {
        /// Encodes a value into a frame.
        Encode: 'A -> FIO<WebSocketFrame, WsError>
        /// Decodes a value from a frame.
        Decode: WebSocketFrame -> FIO<'A, WsError>
    }

[<RequireQualifiedAccess>]
module Codec =

    /// A codec that passes WebSocket frames through unchanged.
    let frame =
        {
            Encode = fun frame -> FIO.succeed frame
            Decode = fun frame -> FIO.succeed frame
        }

    /// A codec for binary frames.
    let binary =
        {
            Encode = fun bytes -> FIO.succeed (Binary bytes)
            Decode = fun frame ->
                match frame with
                | Binary bytes -> FIO.succeed bytes
                | Text _ -> FIO.fail (CodecError "Expected binary frame, got text frame")
                | Close _ -> FIO.fail (CodecError "Expected binary frame, got close frame")
        }

    /// A codec for text frames.
    let text =
        {
            Encode = fun str -> FIO.succeed (Text str)
            Decode = fun frame ->
                match frame with
                | Text str -> FIO.succeed str
                | Binary _ -> FIO.fail (CodecError "Expected text frame, got binary frame")
                | Close _ -> FIO.fail (CodecError "Expected text frame, got close frame")
        }

    /// Creates a JSON codec using the given serializer options.
    let jsonWithOptions<'A> (options: JsonSerializerOptions) =
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () ->
                        let json = JsonSerializer.Serialize(value, options)
                        Text json)
                    WsError.fromException
            Decode = fun frame ->
                match frame with
                | Text json ->
                    FIO.attempt
                        (fun () -> JsonSerializer.Deserialize<'A>(json, options))
                        WsError.fromException
                | Binary bytes ->
                    FIO.attempt
                        (fun () ->
                            let json = Encoding.UTF8.GetString bytes
                            JsonSerializer.Deserialize<'A>(json, options))
                        WsError.fromException
                | Close _ ->
                    FIO.fail (CodecError "Cannot decode close frame as JSON")
        }

    let private defaultJsonOptions =
        let options = JsonSerializerOptions()
        options.MakeReadOnly true
        options

    /// A JSON codec using default serializer options.
    let json<'A> = jsonWithOptions<'A> defaultJsonOptions

    /// A newline-terminated JSON codec, optionally using the given serializer options.
    let jsonLine<'A> (options: JsonSerializerOptions option) =
        let opts = defaultArg options (JsonSerializerOptions())
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () ->
                        let json = JsonSerializer.Serialize(value, opts)
                        Text(json + "\n"))
                    WsError.fromException
            Decode = fun frame ->
                match frame with
                | Text json ->
                    FIO.attempt
                        (fun () ->
                            let trimmed = json.TrimEnd('\n', '\r')
                            JsonSerializer.Deserialize<'A>(trimmed, opts))
                        WsError.fromException
                | Binary bytes ->
                    FIO.attempt
                        (fun () ->
                            let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                            JsonSerializer.Deserialize<'A>(json, opts))
                        WsError.fromException
                | Close _ ->
                    FIO.fail (CodecError "Cannot decode close frame as JSON line")
        }

    /// Adapts a codec to a new type using forward and backward conversions.
    let map (forward: 'A -> 'A1) (backward: 'A1 -> 'A) (codec: WebSocketCodec<'A>) =
        {
            Encode = fun value -> codec.Encode(backward value)
            Decode = fun frame ->
                fio {
                    let! decoded = codec.Decode frame
                    return forward decoded
                }
        }

    /// Combines two codecs into one that encodes a pair of values into a single frame.
    let compose (codec: WebSocketCodec<'A>) (codec': WebSocketCodec<'A1>) : WebSocketCodec<'A * 'A1> =
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
            Encode = fun (first, second) ->
                fio {
                    let! frameA = codec.Encode first
                    let! frameB = codec'.Encode second
                    return!
                        FIO.attempt
                            (fun () -> Text(JsonSerializer.Serialize [| framePart frameA; framePart frameB |]))
                            WsError.fromException
                }
            Decode = fun frame ->
                fio {
                    let! json =
                        match frame with
                        | Text str -> FIO.succeed str
                        | Binary bytes -> FIO.succeed (Encoding.UTF8.GetString bytes)
                        | Close _ -> FIO.fail (CodecError "Cannot decode close frame as composed value")

                    let! frameA, frameB =
                        FIO.attempt (fun () ->
                                use doc = JsonDocument.Parse json
                                let elements = doc.RootElement.EnumerateArray() |> Seq.toArray
                                if elements.Length <> 2 then
                                    raise (Exception $"Expected 2 composed elements, got {elements.Length}")
                                partFrame elements[0], partFrame elements[1])
                            WsError.fromException

                    let! first = codec.Decode frameA
                    let! second = codec'.Decode frameB
                    return first, second
                }
        }

    /// Creates a codec from effectful encode and decode functions.
    let create (encode: 'A -> FIO<WebSocketFrame, WsError>) (decode: WebSocketFrame -> FIO<'A, WsError>) =
        { Encode = encode; Decode = decode }

    /// Creates a codec from pure encode and decode functions, mapping thrown exceptions to errors.
    let createPure (encode: 'A -> WebSocketFrame) (decode: WebSocketFrame -> 'A) =
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
