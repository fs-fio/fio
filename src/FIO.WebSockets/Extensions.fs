namespace FIO.WebSockets

open FIO.DSL

open System.Text.Json
open System.Threading

module WebSocketExtensions =

    let private tokenOr (cancellationToken: CancellationToken option) =
        match cancellationToken with
        | Some token -> FIO.succeed token
        | None -> FIO.cancellationToken ()

    // One default-options codec per type, so a message sent or received with default options builds no codec.
    type private DefaultJsonCodec<'A>() =
        static member val Instance: WebSocketCodec<'A> = Codec.jsonWithOptions<'A> JsonSerializerOptions.Default

    let private jsonCodec<'A> (options: JsonSerializerOptions option) =
        match options with
        | None -> DefaultJsonCodec<'A>.Instance
        | Some options -> Codec.jsonWithOptions<'A> options

    type WebSocket with

        /// Sends a value as JSON text, optionally using the given serializer options and cancellation token.
        member this.SendJson<'A>
            (value: 'A, ?options: JsonSerializerOptions, ?cancellationToken: CancellationToken)
            : FIO<unit, WsError> =
            let codec = jsonCodec<'A> options
            (tokenOr cancellationToken).FlatMap(fun token -> this.Send(codec, value, token))

        /// Receives a JSON value, optionally using the given serializer options and cancellation token.
        member this.ReceiveJson<'A>
            (?options: JsonSerializerOptions, ?cancellationToken: CancellationToken)
            : FIO<'A, WsError> =
            let codec = jsonCodec<'A> options
            (tokenOr cancellationToken).FlatMap(fun token -> this.Receive(codec, token))

        /// Sends a text message.
        member this.SendString(text: string) : FIO<unit, WsError> = this.SendText text

        /// Receives a text message, failing if the next frame is not text.
        member this.ReceiveString() : FIO<string, WsError> = this.Receive Codec.text

        /// Sends a binary message.
        member this.SendBytes(data: byte[]) : FIO<unit, WsError> = this.SendBinary data

        /// Receives a binary message, failing if the next frame is not binary.
        member this.ReceiveBytes() : FIO<byte[], WsError> = this.Receive Codec.binary
