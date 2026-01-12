namespace FSharp.FIO.WebSockets

open System
open System.Net.WebSockets

/// <summary>
/// Configuration options for WebSocket connections.
/// </summary>
type WebSocketConfig =
    {
        /// Size of the buffer used for receiving messages.
        ReceiveBufferSize: int
        /// Size of the buffer used for sending messages.
        SendBufferSize: int
        /// Maximum allowed message size in bytes.
        MaxMessageSize: int64
        /// Timeout for send operations in milliseconds.
        SendTimeout: int
        /// Timeout for receive operations in milliseconds.
        ReceiveTimeout: int
    }

    /// <summary>
    /// Default WebSocket configuration.
    /// </summary>
    static member Default =
        { ReceiveBufferSize = 4096
          SendBufferSize = 4096
          MaxMessageSize = 1_048_576L // 1 MB
          SendTimeout = 30_000 // 30 seconds
          ReceiveTimeout = 30_000 // 30 seconds
        }

/// <summary>
/// Builder functions for WebSocketConfig.
/// </summary>
module WebSocketConfig =

    /// <summary>
    /// Sets the receive buffer size.
    /// </summary>
    let withReceiveBufferSize (size: int, config: WebSocketConfig) : WebSocketConfig =
        { config with ReceiveBufferSize = size }

    /// <summary>
    /// Sets the send buffer size.
    /// </summary>
    let withSendBufferSize (size: int, config: WebSocketConfig) : WebSocketConfig =
        { config with SendBufferSize = size }

    /// <summary>
    /// Sets the maximum message size.
    /// </summary>
    let withMaxMessageSize (size: int64, config: WebSocketConfig) : WebSocketConfig =
        { config with MaxMessageSize = size }

    /// <summary>
    /// Sets the send timeout in milliseconds.
    /// </summary>
    let withSendTimeout (timeout: int, config: WebSocketConfig) : WebSocketConfig =
        { config with SendTimeout = timeout }

    /// <summary>
    /// Sets the receive timeout in milliseconds.
    /// </summary>
    let withReceiveTimeout (timeout: int, config: WebSocketConfig) : WebSocketConfig =
        { config with ReceiveTimeout = timeout }

/// <summary>
/// Represents WebSocket frame types that can be sent and received.
/// </summary>
type WebSocketFrame =
    /// Text frame containing UTF-8 encoded string data.
    | Text of string
    /// Binary frame containing raw byte data.
    | Binary of byte[]
    /// Close frame with status code and reason.
    | Close of WebSocketCloseStatus * string

/// <summary>
/// Messages received from a WebSocket connection.
/// </summary>
type WebSocketMessage =
    /// A WebSocket frame was received.
    | Frame of WebSocketFrame
    /// The connection was closed with optional status and description.
    | ConnectionClosed of WebSocketCloseStatus option * string

/// <summary>
/// Error types that can occur during WebSocket operations.
/// </summary>
type WsError =
    /// Failed to establish connection.
    | ConnectionFailed of string
    /// Failed to send message.
    | SendFailed of string
    /// Failed to receive message.
    | ReceiveFailed of string
    /// Message exceeds maximum allowed size.
    | MessageTooLarge of actual: int64 * max: int64
    /// Operation timed out.
    | TimeoutError of string
    /// Connection pool is exhausted.
    | PoolExhausted of string
    /// Codec encoding/decoding error.
    | CodecError of string
    /// Connection is closed.
    | Closed of string
    /// General error.
    | GeneralError of string

    /// <summary>
    /// Creates a WebSocketError from an exception.
    /// </summary>
    static member FromException (exn: exn) : WsError =
        match exn with
        | :? TimeoutException as e -> TimeoutError e.Message
        | :? WebSocketException as e -> GeneralError e.Message
        | _ -> GeneralError exn.Message

    static member ToException (err: WsError) : exn =
        match err with
        | ConnectionFailed msg -> Exception $"Connection failed: {msg}"
        | SendFailed msg -> Exception $"Send failed: {msg}"
        | ReceiveFailed msg -> Exception $"Receive failed: {msg}"
        | MessageTooLarge (actual, max) -> Exception $"Message size {actual} exceeds maximum {max}"
        | TimeoutError msg -> TimeoutException msg
        | PoolExhausted msg -> Exception $"Pool exhausted: {msg}"
        | CodecError msg -> Exception $"Codec error: {msg}"
        | Closed msg -> Exception $"Connection closed: {msg}"
        | GeneralError msg -> WebSocketException msg

    static member ToString (err: WsError) : string =
        match err with
        | ConnectionFailed msg -> $"Connection failed: {msg}"
        | SendFailed msg -> $"Send failed: {msg}"
        | ReceiveFailed msg -> $"Receive failed: {msg}"
        | MessageTooLarge (actual, max) -> $"Message size {actual} exceeds maximum {max}"
        | TimeoutError msg -> $"Timeout: {msg}"
        | PoolExhausted msg -> $"Pool exhausted: {msg}"
        | CodecError msg -> $"Codec error: {msg}"
        | Closed msg -> $"Connection closed: {msg}"
        | GeneralError msg -> $"WebSocket error: {msg}"

/// <summary>
/// Configuration options for WebSocket connection pools.
/// </summary>
type WebSocketPoolConfig =
    {
        /// Minimum number of connections to maintain in the pool.
        MinPoolSize: int
        /// Maximum number of connections allowed in the pool.
        MaxPoolSize: int
        /// Maximum lifetime of a connection in seconds (0 = infinite).
        ConnectionLifetime: int
        /// Command timeout in seconds.
        CommandTimeout: int
    }

/// <summary>
/// Builder functions for WebSocketPoolConfig.
/// </summary>
module WebSocketPoolConfig =

    /// <summary>
    /// Creates a default pool configuration.
    /// </summary>
    let create (minSize: int, maxSize: int) : WebSocketPoolConfig =
        { MinPoolSize = minSize
          MaxPoolSize = maxSize
          ConnectionLifetime = 300 // 5 minutes
          CommandTimeout = 30 }

    /// <summary>
    /// Default pool configuration (min=5, max=20).
    /// </summary>
    let defaultConfig: WebSocketPoolConfig =
        create (5, 20)

    /// <summary>
    /// Sets the minimum pool size.
    /// </summary>
    let withMinPoolSize (size: int, config: WebSocketPoolConfig) : WebSocketPoolConfig =
        { config with MinPoolSize = size }

    /// <summary>
    /// Sets the maximum pool size.
    /// </summary>
    let withMaxPoolSize (size: int, config: WebSocketPoolConfig) : WebSocketPoolConfig =
        { config with MaxPoolSize = size }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    let withConnectionLifetime (lifetime: int, config: WebSocketPoolConfig) : WebSocketPoolConfig =
        { config with ConnectionLifetime = lifetime }

    /// <summary>
    /// Sets the command timeout in seconds.
    /// </summary>
    let withCommandTimeout (timeout: int, config: WebSocketPoolConfig) : WebSocketPoolConfig =
        { config with CommandTimeout = timeout }
