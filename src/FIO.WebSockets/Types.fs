namespace FIO.WebSockets

open System
open System.Text.Json
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
        /// The send timeout, in milliseconds; 0 or less waits indefinitely. A send that times out aborts the connection.
        SendTimeout: int
        /// The receive timeout, in milliseconds; 0 or less waits indefinitely. A receive that times out aborts the connection.
        ReceiveTimeout: int
        /// How long, in milliseconds, a shutting-down server gives its handlers to finish after closing their
        /// connections; 0 or less waits indefinitely.
        ShutdownTimeout: int
    }

[<RequireQualifiedAccess>]
module WebSocketConfig =

    /// The default WebSocket configuration (4 KB buffers, 1 MB message limit, a 30 s send timeout, no receive
    /// timeout, a 10 s shutdown). A receive timeout aborts the connection when it elapses, and silence is
    /// normal for a WebSocket, so set one only for a peer that must speak regularly.
    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L
            SendTimeout = 30_000
            ReceiveTimeout = 0
            ShutdownTimeout = 10_000
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

    /// Sets the shutdown timeout on a configuration.
    let withShutdownTimeout (timeout: int) (config: WebSocketConfig) =
        { config with ShutdownTimeout = timeout }

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

/// The outcome of receiving with a codec: a decoded message, an undecodable frame, or a closed connection.
type ReceiveOutcome<'A> =
    /// A message the codec decoded.
    | Received of message: 'A
    /// A frame the codec could not decode, with the reason.
    | Undecodable of reason: string
    /// The connection is closed — by the peer, cleanly or not, or already by this side — with a description.
    | PeerClosed of reason: string

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

    /// Classifies an exception as a WebSocket error, giving timeouts, JSON failures and a prematurely closed connection their own case.
    let fromException (ex: exn) =
        match ex with
        | :? TimeoutException as ex ->
            TimeoutError ex.Message
        | :? JsonException as ex ->
            CodecError ex.Message
        | :? WebSocketException as ex when ex.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely ->
            Closed ex.Message
        | _ ->
            GeneralError ex.Message

    let internal receiveFailed (ex: exn) =
        match fromException ex with
        | GeneralError message -> ReceiveFailed message
        | error -> error

    let internal sendFailed (ex: exn) =
        match fromException ex with
        | GeneralError message -> SendFailed message
        | error -> error

    let internal connectionFailed (ex: exn) =
        match fromException ex with
        | GeneralError message -> ConnectionFailed message
        | error -> error

    let internal codecError (ex: exn) =
        CodecError ex.Message

    let internal describeClose (status: WebSocketCloseStatus option) (description: string) =
        let statusText =
            match status with
            | Some status -> string status
            | None -> "no status"

        if String.IsNullOrEmpty description then
            $"Peer closed the connection ({statusText})"
        else
            $"Peer closed the connection ({statusText}): {description}"

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
