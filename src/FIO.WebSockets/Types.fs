namespace FIO.WebSockets

open System
open System.Net.WebSockets

/// Configuration options for WebSocket connections.
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

/// Builder functions for WebSocketConfig.
module WebSocketConfig =

    /// Default WebSocket configuration.
    /// <returns>The default WebSocket configuration.</returns>
    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L // 1 MB
            SendTimeout = 30_000 // 30 seconds
            ReceiveTimeout = 30_000 // 30 seconds
        }

    /// Sets the receive buffer size.
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated receive buffer size.</returns>
    let withReceiveBufferSize (size: int, config: WebSocketConfig) =
        { config with ReceiveBufferSize = size }

    /// Sets the send buffer size.
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated send buffer size.</returns>
    let withSendBufferSize (size: int, config: WebSocketConfig) = { config with SendBufferSize = size }

    /// Sets the maximum message size.
    /// <param name="size">The maximum message size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated maximum message size.</returns>
    let withMaxMessageSize (size: int64, config: WebSocketConfig) = { config with MaxMessageSize = size }

    /// Sets the send timeout in milliseconds.
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated send timeout.</returns>
    let withSendTimeout (timeout: int, config: WebSocketConfig) = { config with SendTimeout = timeout }

    /// Sets the receive timeout in milliseconds.
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated receive timeout.</returns>
    let withReceiveTimeout (timeout: int, config: WebSocketConfig) =
        { config with ReceiveTimeout = timeout }

/// Represents WebSocket frame types that can be sent and received.
type WebSocketFrame =
    /// Text frame containing UTF-8 encoded string data.
    | Text of string
    /// Binary frame containing raw byte data.
    | Binary of byte[]
    /// Close frame with status code and reason.
    | Close of WebSocketCloseStatus * string

/// Messages received from a WebSocket connection.
type WebSocketMessage =
    /// A WebSocket frame was received.
    | Frame of WebSocketFrame
    /// The connection was closed with optional status and description.
    | ConnectionClosed of WebSocketCloseStatus option * string

/// Error types that can occur during WebSocket operations.
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

    /// Gets a human-readable error message.
    /// <returns>A formatted string describing the error.</returns>
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

/// Module functions for working with WsError.
module WsError =

    /// Converts a WebSocketError from an exception.
    /// <param name="exn">The exception to convert.</param>
    /// <returns>A WsError representing the exception.</returns>
    let fromException (exn: exn) =
        match exn with
        | :? TimeoutException as e -> TimeoutError e.Message
        | :? WebSocketException as e -> GeneralError e.Message
        | _ -> GeneralError exn.Message

    /// Converts a WsError to an exception.
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

/// Configuration options for WebSocket connection pools.
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

/// Builder functions for WebSocketPoolConfig.
module WebSocketPoolConfig =

    /// Creates a default pool configuration.
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

    /// Default pool configuration (min=5, max=20).
    /// <returns>The default pool configuration.</returns>
    let defaultConfig = create (5, 20)

    /// Sets the minimum pool size.
    /// <param name="size">The minimum pool size.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated minimum pool size.</returns>
    let withMinPoolSize (size: int, config: WebSocketPoolConfig) = { config with MinPoolSize = size }

    /// Sets the maximum pool size.
    /// <param name="size">The maximum pool size.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated maximum pool size.</returns>
    let withMaxPoolSize (size: int, config: WebSocketPoolConfig) = { config with MaxPoolSize = size }

    /// Sets the connection lifetime in seconds.
    /// <param name="lifetime">The connection lifetime in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated connection lifetime.</returns>
    let withConnectionLifetime (lifetime: int, config: WebSocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// Sets the command timeout in seconds.
    /// <param name="timeout">The command timeout in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with the updated command timeout.</returns>
    let withCommandTimeout (timeout: int, config: WebSocketPoolConfig) =
        { config with CommandTimeout = timeout }
