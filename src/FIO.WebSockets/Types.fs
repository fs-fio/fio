namespace FIO.WebSockets

open System
open System.Net.WebSockets

/// <summary>Represents configuration options for a WebSocket connection.</summary>
type WebSocketConfig =
    {
        /// <summary>Represents the buffer size in bytes used for receiving messages.</summary>
        ReceiveBufferSize: int
        /// <summary>Represents the buffer size in bytes used for sending messages.</summary>
        SendBufferSize: int
        /// <summary>Represents the maximum allowed message size in bytes.</summary>
        MaxMessageSize: int64
        /// <summary>Represents the timeout for send operations in milliseconds.</summary>
        SendTimeout: int
        /// <summary>Represents the timeout for receive operations in milliseconds.</summary>
        ReceiveTimeout: int
    }

/// <summary>Builds configuration values for WebSocket connections.</summary>
module WebSocketConfig =

    /// <summary>Returns the default WebSocket configuration.</summary>
    /// <returns>A configuration with default buffer sizes, message limits, and timeouts.</returns>
    let defaultConfig =
        {
            ReceiveBufferSize = 4096
            SendBufferSize = 4096
            MaxMessageSize = 1_048_576L // 1 MB
            SendTimeout = 30_000 // 30 seconds
            ReceiveTimeout = 30_000 // 30 seconds
        }

    /// <summary>Returns a new configuration with the specified receive buffer size.</summary>
    /// <param name="size">The buffer size in bytes for receiving messages.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the receive buffer size set to <paramref name="size"/>.</returns>
    let withReceiveBufferSize (size: int, config: WebSocketConfig) =
        { config with ReceiveBufferSize = size }

    /// <summary>Returns a new configuration with the specified send buffer size.</summary>
    /// <param name="size">The buffer size in bytes for sending messages.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the send buffer size set to <paramref name="size"/>.</returns>
    let withSendBufferSize (size: int, config: WebSocketConfig) = { config with SendBufferSize = size }

    /// <summary>Returns a new configuration with the specified maximum message size.</summary>
    /// <param name="size">The maximum message size in bytes.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the maximum message size set to <paramref name="size"/>.</returns>
    let withMaxMessageSize (size: int64, config: WebSocketConfig) = { config with MaxMessageSize = size }

    /// <summary>Returns a new configuration with the specified send timeout.</summary>
    /// <param name="timeout">The send timeout in milliseconds.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the send timeout set to <paramref name="timeout"/>.</returns>
    let withSendTimeout (timeout: int, config: WebSocketConfig) = { config with SendTimeout = timeout }

    /// <summary>Returns a new configuration with the specified receive timeout.</summary>
    /// <param name="timeout">The receive timeout in milliseconds.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the receive timeout set to <paramref name="timeout"/>.</returns>
    let withReceiveTimeout (timeout: int, config: WebSocketConfig) =
        { config with ReceiveTimeout = timeout }

/// <summary>Represents a WebSocket frame that can be sent or received.</summary>
type WebSocketFrame =
    /// <summary>Represents a text frame containing UTF-8 encoded string data.</summary>
    | Text of string
    /// <summary>Represents a binary frame containing raw byte data.</summary>
    | Binary of byte[]
    /// <summary>Represents a close frame with a status code and reason description.</summary>
    | Close of WebSocketCloseStatus * string

/// <summary>Represents a message received from a WebSocket connection.</summary>
type WebSocketMessage =
    /// <summary>Represents a successfully received WebSocket frame.</summary>
    | Frame of WebSocketFrame
    /// <summary>Represents a connection-closed notification with optional status and description.</summary>
    | ConnectionClosed of WebSocketCloseStatus option * string

/// <summary>Represents an error that can occur during a WebSocket operation.</summary>
type WsError =
    /// <summary>Represents a failure to establish a connection.</summary>
    | ConnectionFailed of string
    /// <summary>Represents a failure to send a message.</summary>
    | SendFailed of string
    /// <summary>Represents a failure to receive a message.</summary>
    | ReceiveFailed of string
    /// <summary>Represents a message that exceeds the maximum allowed size.</summary>
    | MessageTooLarge of actual: int64 * max: int64
    /// <summary>Represents an operation that timed out.</summary>
    | TimeoutError of string
    /// <summary>Represents a connection pool that has no available connections.</summary>
    | PoolExhausted of string
    /// <summary>Represents an encoding or decoding error in a codec.</summary>
    | CodecError of string
    /// <summary>Represents an operation attempted on a closed connection.</summary>
    | Closed of string
    /// <summary>Represents a general WebSocket error.</summary>
    | GeneralError of string

    /// <summary>Returns a human-readable description of the error.</summary>
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

/// <summary>Creates WsError values from exceptions and vice versa.</summary>
module WsError =

    /// <summary>Creates a WsError from an exception, mapping known exception types to specific error cases.</summary>
    /// <param name="exn">The exception to convert.</param>
    /// <returns>A WsError representing the exception.</returns>
    let fromException (exn: exn) =
        match exn with
        | :? TimeoutException as e -> TimeoutError e.Message
        | :? WebSocketException as e -> GeneralError e.Message
        | _ -> GeneralError exn.Message

    /// <summary>Creates an exception from a WsError, mapping each error case to an appropriate exception type.</summary>
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

/// <summary>Represents configuration options for a WebSocket connection pool.</summary>
type WebSocketPoolConfig =
    {
        /// <summary>Represents the minimum number of connections to maintain in the pool.</summary>
        MinPoolSize: int
        /// <summary>Represents the maximum number of connections allowed in the pool.</summary>
        MaxPoolSize: int
        /// <summary>Represents the maximum lifetime of a connection in seconds, where zero means infinite.</summary>
        ConnectionLifetime: int
        /// <summary>Represents the command timeout in seconds.</summary>
        CommandTimeout: int
    }

/// <summary>Builds configuration values for WebSocket connection pools.</summary>
module WebSocketPoolConfig =

    /// <summary>Creates a pool configuration with the specified size bounds and default timeouts.</summary>
    /// <param name="minSize">The minimum number of connections to maintain.</param>
    /// <param name="maxSize">The maximum number of connections allowed.</param>
    /// <returns>A pool configuration with the specified size bounds.</returns>
    let create (minSize: int, maxSize: int) =
        {
            MinPoolSize = minSize
            MaxPoolSize = maxSize
            ConnectionLifetime = 300 // 5 minutes
            CommandTimeout = 30
        }

    /// <summary>Returns the default pool configuration with minimum size 5 and maximum size 20.</summary>
    /// <returns>A pool configuration with default size bounds and timeouts.</returns>
    let defaultConfig = create (5, 20)

    /// <summary>Returns a new configuration with the specified minimum pool size.</summary>
    /// <param name="size">The minimum number of connections to maintain.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the minimum pool size set to <paramref name="size"/>.</returns>
    let withMinPoolSize (size: int, config: WebSocketPoolConfig) = { config with MinPoolSize = size }

    /// <summary>Returns a new configuration with the specified maximum pool size.</summary>
    /// <param name="size">The maximum number of connections allowed.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the maximum pool size set to <paramref name="size"/>.</returns>
    let withMaxPoolSize (size: int, config: WebSocketPoolConfig) = { config with MaxPoolSize = size }

    /// <summary>Returns a new configuration with the specified connection lifetime.</summary>
    /// <param name="lifetime">The maximum connection lifetime in seconds.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the connection lifetime set to <paramref name="lifetime"/>.</returns>
    let withConnectionLifetime (lifetime: int, config: WebSocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// <summary>Returns a new configuration with the specified command timeout.</summary>
    /// <param name="timeout">The command timeout in seconds.</param>
    /// <param name="config">The base configuration to derive from.</param>
    /// <returns>A new configuration with the command timeout set to <paramref name="timeout"/>.</returns>
    let withCommandTimeout (timeout: int, config: WebSocketPoolConfig) =
        { config with CommandTimeout = timeout }
