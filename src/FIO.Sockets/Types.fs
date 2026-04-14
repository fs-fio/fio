namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

/// <summary>
/// Socket operation errors.
/// </summary>
type SocketError =
    /// <summary>
    /// Failed to connect to a remote host.
    /// </summary>
    | ConnectionFailed of host: string * port: int * exn
    /// <summary>
    /// Connection was closed.
    /// </summary>
    | ConnectionClosed of string
    /// <summary>
    /// Failed to send data.
    /// </summary>
    | SendFailed of exn
    /// <summary>
    /// Failed to receive data.
    /// </summary>
    | ReceiveFailed of exn
    /// <summary>
    /// Operation timed out.
    /// </summary>
    | TimeoutError of string
    /// <summary>
    /// Buffer overflow occurred.
    /// </summary>
    | BufferOverflow of requested: int * available: int
    /// <summary>
    /// Invalid socket state.
    /// </summary>
    | InvalidState of expected: string * actual: string
    /// <summary>
    /// Failed to bind to a local address.
    /// </summary>
    | BindFailed of address: string * port: int * exn
    /// <summary>
    /// Failed to accept a connection.
    /// </summary>
    | AcceptFailed of exn
    /// <summary>
    /// Connection pool exhausted.
    /// </summary>
    | PoolExhausted of maxSize: int
    /// <summary>
    /// Connection pool is closed.
    /// </summary>
    | PoolClosed
    /// <summary>
    /// Codec encoding/decoding error.
    /// </summary>
    | CodecError of string * exn
    /// <summary>
    /// General socket error.
    /// </summary>
    | GeneralError of exn

    /// <summary>
    /// Gets a human-readable error message.
    /// </summary>
    /// <returns>A human-readable error message.</returns>
    override this.ToString() =
        match this with
        | ConnectionFailed(host, port, exn) -> $"Failed to connect to {host}:{port}: {exn.Message}"
        | ConnectionClosed msg -> $"Connection closed: {msg}"
        | SendFailed exn -> $"Send failed: {exn.Message}"
        | ReceiveFailed exn -> $"Receive failed: {exn.Message}"
        | TimeoutError msg -> $"Timeout: {msg}"
        | BufferOverflow(requested, available) ->
            $"Buffer overflow: requested {requested} bytes, only {available} available"
        | InvalidState(expected, actual) -> $"Invalid state: expected {expected}, actual {actual}"
        | BindFailed(addr, port, exn) -> $"Failed to bind to {addr}:{port}: {exn.Message}"
        | AcceptFailed exn -> $"Accept failed: {exn.Message}"
        | PoolExhausted maxSize -> $"Pool exhausted: maximum {maxSize} connections"
        | PoolClosed -> "Pool is closed"
        | CodecError(msg, exn) -> $"Codec error: {msg} - {exn.Message}"
        | GeneralError exn -> $"Socket error: {exn.Message}"

/// <summary>
/// Module functions for working with SocketError.
/// </summary>
module SocketError =

    /// <summary>
    /// Converts an exception to a SocketError.
    /// </summary>
    /// <param name="exn">The exception to convert.</param>
    /// <returns>The socket error.</returns>
    let fromException exn = GeneralError exn

    /// <summary>
    /// Converts a SocketError to an exception.
    /// </summary>
    /// <param name="err">The socket error.</param>
    /// <returns>The exception.</returns>
    let toException err =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// <summary>
/// Configuration options for TCP socket connections.
/// </summary>
type SocketConfig =
    {
        /// <summary>
        /// Remote host to connect to.
        /// </summary>
        Host: string
        /// <summary>
        /// Remote port to connect to.
        /// </summary>
        Port: int
        /// <summary>
        /// Address family (IPv4, IPv6, etc.).
        /// </summary>
        AddressFamily: Sockets.AddressFamily
        /// <summary>
        /// Socket type (Stream for TCP).
        /// </summary>
        SocketType: Sockets.SocketType
        /// <summary>
        /// Protocol type (Tcp for TCP).
        /// </summary>
        ProtocolType: Sockets.ProtocolType
        /// <summary>
        /// Send buffer size in bytes.
        /// </summary>
        SendBufferSize: int
        /// <summary>
        /// Receive buffer size in bytes.
        /// </summary>
        ReceiveBufferSize: int
        /// <summary>
        /// Send timeout in milliseconds (0 = infinite).
        /// </summary>
        SendTimeout: int
        /// <summary>
        /// Receive timeout in milliseconds (0 = infinite).
        /// </summary>
        ReceiveTimeout: int
        /// <summary>
        /// Enable/disable Nagle's algorithm.
        /// </summary>
        NoDelay: bool
        /// <summary>
        /// Enable linger on close (if true, waits LingerTimeout before closing).
        /// </summary>
        LingerEnabled: bool
        /// <summary>
        /// Linger timeout in seconds (0 = immediate close/abort).
        /// </summary>
        LingerTimeout: int
    }

/// <summary>
/// Builder functions for SocketConfig.
/// </summary>
module SocketConfig =

    /// <summary>
    /// Creates a default TCP socket configuration for the given host and port.
    /// </summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to (1-65535).</param>
    /// <returns>The socket configuration.</returns>
    let create (host: string, port: int) =
        fio {
            if String.IsNullOrWhiteSpace host then
                return! FIO.fail (InvalidState("non-empty host", "empty or whitespace"))

            if port < 1 || port > 65535 then
                return! FIO.fail (InvalidState("port in range 1-65535", $"{port}"))

            return
                {
                    Host = host
                    Port = port
                    AddressFamily = Sockets.AddressFamily.InterNetwork
                    SocketType = Sockets.SocketType.Stream
                    ProtocolType = Sockets.ProtocolType.Tcp
                    SendBufferSize = 8192
                    ReceiveBufferSize = 8192
                    SendTimeout = 30000
                    ReceiveTimeout = 30000
                    NoDelay = true
                    LingerEnabled = true
                    LingerTimeout = 0
                }
        }

    /// <summary>
    /// Sets the send buffer size.
    /// </summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withSendBufferSize (size: int, config: SocketConfig) = { config with SendBufferSize = size }

    /// <summary>
    /// Sets the receive buffer size.
    /// </summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withReceiveBufferSize (size: int, config: SocketConfig) =
        { config with ReceiveBufferSize = size }

    /// <summary>
    /// Sets the send timeout in milliseconds.
    /// </summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withSendTimeout (timeout: int, config: SocketConfig) = { config with SendTimeout = timeout }

    /// <summary>
    /// Sets the receive timeout in milliseconds.
    /// </summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withReceiveTimeout (timeout: int, config: SocketConfig) =
        { config with ReceiveTimeout = timeout }

    /// <summary>
    /// Sets whether to enable/disable Nagle's algorithm.
    /// </summary>
    /// <param name="noDelay">True to disable Nagle's algorithm, false to enable it.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withNoDelay (noDelay: bool, config: SocketConfig) = { config with NoDelay = noDelay }

    /// <summary>
    /// Sets the address family.
    /// </summary>
    /// <param name="family">The address family (IPv4, IPv6, etc.).</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: SocketConfig) = { config with AddressFamily = family }

/// <summary>
/// Configuration options for TCP server sockets.
/// </summary>
type ServerSocketConfig =
    {
        /// <summary>
        /// Local address to bind to.
        /// </summary>
        BindAddress: string
        /// <summary>
        /// Local port to bind to.
        /// </summary>
        BindPort: int
        /// <summary>
        /// Address family (IPv4, IPv6, etc.).
        /// </summary>
        AddressFamily: Sockets.AddressFamily
        /// <summary>
        /// Socket type (Stream for TCP).
        /// </summary>
        SocketType: Sockets.SocketType
        /// <summary>
        /// Protocol type (Tcp for TCP).
        /// </summary>
        ProtocolType: Sockets.ProtocolType
        /// <summary>
        /// Connection backlog queue size.
        /// </summary>
        Backlog: int
        /// <summary>
        /// Default configuration for accepted connections.
        /// </summary>
        AcceptedSocketConfig: SocketConfig option
    }

/// <summary>
/// Builder functions for ServerSocketConfig.
/// </summary>
module ServerSocketConfig =

    /// <summary>
    /// Default configuration for localhost:8080.
    /// </summary>
    let defaultConfig =
        {
            BindAddress = "127.0.0.1"
            BindPort = 8080
            AddressFamily = Sockets.AddressFamily.InterNetwork
            SocketType = Sockets.SocketType.Stream
            ProtocolType = Sockets.ProtocolType.Tcp
            Backlog = 100
            AcceptedSocketConfig = None
        }

    /// <summary>
    /// Creates a default TCP server configuration.
    /// </summary>
    /// <param name="bindAddress">The local address to bind to.</param>
    /// <param name="bindPort">The local port to bind to (0-65535, where 0 = any available port).</param>
    /// <returns>The server socket configuration.</returns>
    let create (bindAddress: string, bindPort: int) =
        fio {
            if String.IsNullOrWhiteSpace bindAddress then
                return! FIO.fail (InvalidState("non-empty bind address", "empty or whitespace"))

            if bindPort < 0 || bindPort > 65535 then
                return! FIO.fail (InvalidState("port in range 0-65535", $"{bindPort}"))

            return
                {
                    BindAddress = bindAddress
                    BindPort = bindPort
                    AddressFamily = Sockets.AddressFamily.InterNetwork
                    SocketType = Sockets.SocketType.Stream
                    ProtocolType = Sockets.ProtocolType.Tcp
                    Backlog = 100
                    AcceptedSocketConfig = None
                }
        }

    /// <summary>
    /// Sets the connection backlog queue size.
    /// </summary>
    /// <param name="backlog">The maximum length of the pending connections queue.</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withBacklog (backlog: int, config: ServerSocketConfig) = { config with Backlog = backlog }

    /// <summary>
    /// Sets the default configuration for accepted connections.
    /// </summary>
    /// <param name="acceptedConfig">The socket configuration to apply to accepted connections.</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withAcceptedConfig (acceptedConfig: SocketConfig, config: ServerSocketConfig) =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// <summary>
    /// Sets the address family.
    /// </summary>
    /// <param name="family">The address family (IPv4, IPv6, etc.).</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: ServerSocketConfig) =
        { config with AddressFamily = family }

/// <summary>
/// Configuration options for socket connection pools.
/// </summary>
type SocketPoolConfig =
    {
        /// <summary>
        /// Socket configuration for connections in the pool.
        /// </summary>
        SocketConfig: SocketConfig
        /// <summary>
        /// Minimum number of connections in the pool.
        /// </summary>
        MinPoolSize: int
        /// <summary>
        /// Maximum number of connections in the pool.
        /// </summary>
        MaxPoolSize: int
        /// <summary>
        /// Connection lifetime in seconds (0 = infinite).
        /// </summary>
        ConnectionLifetime: int
        /// <summary>
        /// Enable connection validation before reuse.
        /// </summary>
        ValidateOnAcquire: bool
    }

/// <summary>
/// Builder functions for SocketPoolConfig.
/// </summary>
module SocketPoolConfig =

    /// <summary>
    /// Creates a default pool configuration.
    /// </summary>
    /// <param name="socketConfig">The socket configuration for connections in the pool.</param>
    /// <returns>The socket pool configuration.</returns>
    let create (socketConfig: SocketConfig) =
        fio {
            // Default values
            let minPoolSize = 0
            let maxPoolSize = 10
            let connectionLifetime = 300

            if minPoolSize < 0 then
                return! FIO.fail (InvalidState("MinPoolSize >= 0", $"{minPoolSize}"))

            if maxPoolSize <= 0 then
                return! FIO.fail (InvalidState("MaxPoolSize > 0", $"{maxPoolSize}"))

            if maxPoolSize < minPoolSize then
                return!
                    FIO.fail (
                        InvalidState(
                            $"MaxPoolSize ({maxPoolSize}) >= MinPoolSize ({minPoolSize})",
                            "MaxPoolSize < MinPoolSize"
                        )
                    )

            if connectionLifetime < 0 then
                return! FIO.fail (InvalidState("ConnectionLifetime >= 0", $"{connectionLifetime}"))

            return
                {
                    SocketConfig = socketConfig
                    MinPoolSize = minPoolSize
                    MaxPoolSize = maxPoolSize
                    ConnectionLifetime = connectionLifetime
                    ValidateOnAcquire = true
                }
        }

    /// <summary>
    /// Sets the minimum pool size.
    /// </summary>
    /// <param name="size">The minimum number of connections in the pool.</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withMinPoolSize (size: int, config: SocketPoolConfig) =
        fio {
            if size < 0 then
                return! FIO.fail (InvalidState("MinPoolSize >= 0", $"{size}"))

            if size > config.MaxPoolSize then
                return!
                    FIO.fail (
                        InvalidState(
                            $"MinPoolSize ({size}) <= MaxPoolSize ({config.MaxPoolSize})",
                            "MinPoolSize > MaxPoolSize"
                        )
                    )

            return { config with MinPoolSize = size }
        }

    /// <summary>
    /// Sets the maximum pool size.
    /// </summary>
    /// <param name="size">The maximum number of connections in the pool.</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withMaxPoolSize (size: int, config: SocketPoolConfig) =
        fio {
            if size <= 0 then
                return! FIO.fail (InvalidState("MaxPoolSize > 0", $"{size}"))

            if size < config.MinPoolSize then
                return!
                    FIO.fail (
                        InvalidState(
                            $"MaxPoolSize ({size}) >= MinPoolSize ({config.MinPoolSize})",
                            "MaxPoolSize < MinPoolSize"
                        )
                    )

            return { config with MaxPoolSize = size }
        }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    /// <param name="lifetime">The connection lifetime in seconds (0 = infinite).</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withConnectionLifetime (lifetime: int, config: SocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// <summary>
    /// Sets whether to validate connections before reuse.
    /// </summary>
    /// <param name="validate">True to validate connections before reuse, false otherwise.</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withValidateOnAcquire (validate: bool, config: SocketPoolConfig) =
        { config with ValidateOnAcquire = validate }

/// <summary>
/// Represents a server socket listener (immutable wrapper).
/// Use ServerSocket module functions for operations.
/// </summary>
type ServerSocket =
    internal
        {
            /// <summary>
            /// Underlying .NET Socket (listener).
            /// </summary>
            NetSocket: Sockets.Socket
            /// <summary>
            /// Server configuration.
            /// </summary>
            Config: ServerSocketConfig
        }
