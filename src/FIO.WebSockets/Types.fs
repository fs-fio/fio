namespace FIO.WebSockets

open System
open System.Net.WebSockets

/// <summary>
/// Configuration options for WebSocket connections.
/// </summary>
type WebSocketConfig =
    {
        /// <summary>
        /// Size of the buffer used for receiving messages.
        /// </summary>
        ReceiveBufferSize: int
        /// <summary>
        /// Size of the buffer used for sending messages.
        /// </summary>
        SendBufferSize: int
        /// <summary>
        /// Maximum allowed message size in bytes.
        /// </summary>
        MaxMessageSize: int64
        /// <summary>
        /// Timeout for send operations in milliseconds.
        /// </summary>
        SendTimeout: int
        /// <summary>
        /// Timeout for receive operations in milliseconds.
        /// </summary>
        ReceiveTimeout: int
    }

/// <summary>
/// Builder functions for WebSocketConfig.
/// </summary>
module WebSocketConfig =

    /// <summary>
    /// Default WebSocket configuration.
    /// </summary>
    /// <returns>The default WebSocket configuration.</returns>
    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L // 1 MB
            SendTimeout = 30_000 // 30 seconds
            ReceiveTimeout = 30_000 // 30 seconds
        }

    /// <summary>
    /// Sets the receive buffer size.
    /// </summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated receive buffer size.</returns>
    let withReceiveBufferSize (size: int, config: WebSocketConfig) =
        { config with ReceiveBufferSize = size }

    /// <summary>
    /// Sets the send buffer size.
    /// </summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated send buffer size.</returns>
    let withSendBufferSize (size: int, config: WebSocketConfig) = { config with SendBufferSize = size }

    /// <summary>
    /// Sets the maximum message size.
    /// </summary>
    /// <param name="size">The maximum message size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated maximum message size.</returns>
    let withMaxMessageSize (size: int64, config: WebSocketConfig) = { config with MaxMessageSize = size }

    /// <summary>
    /// Sets the send timeout in milliseconds.
    /// </summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated send timeout.</returns>
    let withSendTimeout (timeout: int, config: WebSocketConfig) = { config with SendTimeout = timeout }

    /// <summary>
    /// Sets the receive timeout in milliseconds.
    /// </summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated receive timeout.</returns>
    let withReceiveTimeout (timeout: int, config: WebSocketConfig) =
        { config with ReceiveTimeout = timeout }

/// <summary>
/// Represents WebSocket frame types that can be sent and received.
/// </summary>
type WebSocketFrame =
    /// <summary>
    /// Text frame containing UTF-8 encoded string data.
    /// </summary>
    | Text of string
    /// <summary>
    /// Binary frame containing raw byte data.
    /// </summary>
    | Binary of byte[]
    /// <summary>
    /// Close frame with status code and reason.
    /// </summary>
    | Close of WebSocketCloseStatus * string

/// <summary>
/// Messages received from a WebSocket connection.
/// </summary>
type WebSocketMessage =
    /// <summary>
    /// A WebSocket frame was received.
    /// </summary>
    | Frame of WebSocketFrame
    /// <summary>
    /// The connection was closed with optional status and description.
    /// </summary>
    | ConnectionClosed of WebSocketCloseStatus option * string

/// <summary>
/// Error types that can occur during WebSocket operations.
/// </summary>
type WsError =
    /// <summary>
    /// Failed to establish connection.
    /// </summary>
    | ConnectionFailed of string
    /// <summary>
    /// Failed to send message.
    /// </summary>
    | SendFailed of string
    /// <summary>
    /// Failed to receive message.
    /// </summary>
    | ReceiveFailed of string
    /// <summary>
    /// Message exceeds maximum allowed size.
    /// </summary>
    | MessageTooLarge of actual: int64 * max: int64
    /// <summary>
    /// Operation timed out.
    /// </summary>
    | TimeoutError of string
    /// <summary>
    /// Connection pool is exhausted.
    /// </summary>
    | PoolExhausted of string
    /// <summary>
    /// Codec encoding/decoding error.
    /// </summary>
    | CodecError of string
    /// <summary>
    /// Connection is closed.
    /// </summary>
    | Closed of string
    /// <summary>
    /// General error.
    /// </summary>
    | GeneralError of string

    /// <summary>
    /// Gets a human-readable error message.
    /// </summary>
    /// <returns>A human-readable error message.</returns>
    override this.ToString() : string =
        match this with
        | ConnectionFailed msg -> $"Connection failed: {msg}"
        | SendFailed msg -> $"Send failed: {msg}"
        | ReceiveFailed msg -> $"Receive failed: {msg}"
        | MessageTooLarge(actual, max) -> $"Message size {actual} exceeds maximum {max}"
        | TimeoutError msg -> $"Timeout: {msg}"
        | PoolExhausted msg -> $"Pool exhausted: {msg}"
        | CodecError msg -> $"Codec error: {msg}"
        | Closed msg -> $"Connection closed: {msg}"
        | GeneralError msg -> $"WebSocket error: {msg}"

/// <summary>
/// Module functions for working with WsError.
/// </summary>
module WsError =

    /// <summary>
    /// Converts a WebSocketError from an exception.
    /// </summary>
    /// <param name="exn">The exception to convert.</param>
    /// <returns>A WsError representing the exception.</returns>
    let fromException (exn: exn) =
        match exn with
        | :? TimeoutException as e -> TimeoutError e.Message
        | :? WebSocketException as e -> GeneralError e.Message
        | _ -> GeneralError exn.Message

    /// <summary>
    /// Converts a WsError to an exception.
    /// </summary>
    /// <param name="err">The error to convert.</param>
    /// <returns>An exception representing the error.</returns>
    let toException (err: WsError) =
        match err with
        | ConnectionFailed msg -> Exception $"Connection failed: {msg}"
        | SendFailed msg -> Exception $"Send failed: {msg}"
        | ReceiveFailed msg -> Exception $"Receive failed: {msg}"
        | MessageTooLarge(actual, max) -> Exception $"Message size {actual} exceeds maximum {max}"
        | TimeoutError msg -> TimeoutException msg
        | PoolExhausted msg -> Exception $"Pool exhausted: {msg}"
        | CodecError msg -> Exception $"Codec error: {msg}"
        | Closed msg -> Exception $"Connection closed: {msg}"
        | GeneralError msg -> WebSocketException msg

/// <summary>
/// Configuration options for WebSocket connection pools.
/// </summary>
type WebSocketPoolConfig =
    {
        /// <summary>
        /// Minimum number of connections to maintain in the pool.
        /// </summary>
        MinPoolSize: int
        /// <summary>
        /// Maximum number of connections allowed in the pool.
        /// </summary>
        MaxPoolSize: int
        /// <summary>
        /// Maximum lifetime of a connection in seconds (0 = infinite).
        /// </summary>
        ConnectionLifetime: int
        /// <summary>
        /// Command timeout in seconds.
        /// </summary>
        CommandTimeout: int
    }

/// <summary>
/// Builder functions for WebSocketPoolConfig.
/// </summary>
module WebSocketPoolConfig =

    /// <summary>
    /// Creates a default pool configuration.
    /// </summary>
    /// <param name="minSize">The minimum pool size.</param>
    /// <param name="maxSize">The maximum pool size.</param>
    /// <returns>A new pool configuration.</returns>
    let create (minSize: int, maxSize: int) =
        {
            MinPoolSize = minSize
            MaxPoolSize = maxSize
            ConnectionLifetime = 300 // 5 minutes
            CommandTimeout = 30
        }

    /// <summary>
    /// Default pool configuration (min=5, max=20).
    /// </summary>
    /// <returns>The default pool configuration.</returns>
    let defaultConfig = create (5, 20)

    /// <summary>
    /// Sets the minimum pool size.
    /// </summary>
    /// <param name="size">The minimum pool size.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated minimum pool size.</returns>
    let withMinPoolSize (size: int, config: WebSocketPoolConfig) = { config with MinPoolSize = size }

    /// <summary>
    /// Sets the maximum pool size.
    /// </summary>
    /// <param name="size">The maximum pool size.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated maximum pool size.</returns>
    let withMaxPoolSize (size: int, config: WebSocketPoolConfig) = { config with MaxPoolSize = size }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    /// <param name="lifetime">The connection lifetime in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated connection lifetime.</returns>
    let withConnectionLifetime (lifetime: int, config: WebSocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// <summary>
    /// Sets the command timeout in seconds.
    /// </summary>
    /// <param name="timeout">The command timeout in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated command timeout.</returns>
    let withCommandTimeout (timeout: int, config: WebSocketPoolConfig) =
        { config with CommandTimeout = timeout }
