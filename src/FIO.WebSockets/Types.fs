namespace FIO.WebSockets

open System
open System.Net.WebSockets

/// Configuration for a WebSocket connection.
type WebSocketConfig =
    {
        /// The receive buffer size, in bytes.
        ReceiveBufferSize: int
        /// The send buffer size, in bytes.
        SendBufferSize: int
        /// The maximum message size, in bytes.
        MaxMessageSize: int64
        /// The send timeout, in milliseconds.
        SendTimeout: int
        /// The receive timeout, in milliseconds.
        ReceiveTimeout: int
    }

[<RequireQualifiedAccess>]
module WebSocketConfig =

    /// The default WebSocket configuration (4 KB buffers, 1 MB message limit, 30 s timeouts).
    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L
            SendTimeout = 30_000
            ReceiveTimeout = 30_000
        }

    /// Sets the receive buffer size on a configuration.
    let withReceiveBufferSize (size: int) (config: WebSocketConfig) =
        { config with ReceiveBufferSize = size }

    /// Sets the send buffer size on a configuration.
    let withSendBufferSize (size: int) (config: WebSocketConfig) =
        { config with SendBufferSize = size }

    /// Sets the maximum message size on a configuration.
    let withMaxMessageSize (size: int64) (config: WebSocketConfig) =
        { config with MaxMessageSize = size }

    /// Sets the send timeout on a configuration.
    let withSendTimeout (timeout: int) (config: WebSocketConfig) =
        { config with SendTimeout = timeout }

    /// Sets the receive timeout on a configuration.
    let withReceiveTimeout (timeout: int) (config: WebSocketConfig) =
        { config with ReceiveTimeout = timeout }

/// A single WebSocket frame.
type WebSocketFrame =
    /// A UTF-8 text frame.
    | Text of string
    /// A binary frame.
    | Binary of byte[]
    /// A close frame carrying a status and reason.
    | Close of WebSocketCloseStatus * string

/// A message received from a WebSocket: either a frame or a closed-connection signal.
type WebSocketMessage =
    /// A received frame.
    | Frame of WebSocketFrame
    /// The connection was closed by the peer, with an optional status and reason.
    | ConnectionClosed of WebSocketCloseStatus option * string

/// An error produced by a WebSocket operation.
type WsError =
    /// Establishing the connection failed.
    | ConnectionFailed of string
    /// Sending a frame failed.
    | SendFailed of string
    /// Receiving a frame failed.
    | ReceiveFailed of string
    /// A message exceeded the configured maximum size.
    | MessageTooLarge of actual: int64 * max: int64
    /// The operation timed out.
    | TimeoutError of string
    /// Encoding or decoding a value failed.
    | CodecError of string
    /// The connection was closed.
    | Closed of string
    /// An otherwise-unclassified error.
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

    /// Wraps an exception as a WebSocket error.
    let fromException (ex: exn) =
        match ex with
        | :? TimeoutException as ex -> TimeoutError ex.Message
        | :? WebSocketException as ex -> GeneralError ex.Message
        | _ -> GeneralError ex.Message

    /// Converts a WebSocket error back into an exception.
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
