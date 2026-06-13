namespace FIO.WebSockets

open System
open System.Net.WebSockets

type WebSocketConfig =
    {
        ReceiveBufferSize: int
        SendBufferSize: int
        MaxMessageSize: int64
        SendTimeout: int
        ReceiveTimeout: int
    }

[<RequireQualifiedAccess>]
module WebSocketConfig =

    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L
            SendTimeout = 30_000
            ReceiveTimeout = 30_000
        }

    let withReceiveBufferSize (size: int) (config: WebSocketConfig) =
        { config with ReceiveBufferSize = size }

    let withSendBufferSize (size: int) (config: WebSocketConfig) =
        { config with SendBufferSize = size }

    let withMaxMessageSize (size: int64) (config: WebSocketConfig) =
        { config with MaxMessageSize = size }

    let withSendTimeout (timeout: int) (config: WebSocketConfig) =
        { config with SendTimeout = timeout }

    let withReceiveTimeout (timeout: int) (config: WebSocketConfig) =
        { config with ReceiveTimeout = timeout }

type WebSocketFrame =
    | Text of string
    | Binary of byte[]
    | Close of WebSocketCloseStatus * string

type WebSocketMessage =
    | Frame of WebSocketFrame
    | ConnectionClosed of WebSocketCloseStatus option * string

type WsError =
    | ConnectionFailed of string
    | SendFailed of string
    | ReceiveFailed of string
    | MessageTooLarge of actual: int64 * max: int64
    | TimeoutError of string
    | CodecError of string
    | Closed of string
    | GeneralError of string

    override this.ToString () : string =
        match this with
        | ConnectionFailed message -> $"Connection failed: {message}"
        | SendFailed message -> $"Send failed: {message}"
        | ReceiveFailed message -> $"Receive failed: {message}"
        | MessageTooLarge(actual, max) -> $"Message size {actual} exceeds maximum {max}"
        | TimeoutError message -> $"Timeout: {message}"
        | CodecError message -> $"Codec error: {message}"
        | Closed message -> $"Connection closed: {message}"
        | GeneralError message -> $"WebSocket error: {message}"

[<RequireQualifiedAccess>]
module WsError =

    let fromException (ex: exn) =
        match ex with
        | :? TimeoutException as ex -> TimeoutError ex.Message
        | :? WebSocketException as ex -> GeneralError ex.Message
        | _ -> GeneralError ex.Message

    let toException (error: WsError) =
        match error with
        | ConnectionFailed message -> Exception $"Connection failed: {message}"
        | SendFailed message -> Exception $"Send failed: {message}"
        | ReceiveFailed message -> Exception $"Receive failed: {message}"
        | MessageTooLarge(actual, max) -> Exception $"Message size {actual} exceeds maximum {max}"
        | TimeoutError message -> TimeoutException message
        | CodecError message -> Exception $"Codec error: {message}"
        | Closed message -> Exception $"Connection closed: {message}"
        | GeneralError message -> WebSocketException message
