namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

/// Socket operation errors.
type SocketError =
    /// Failed to connect to a remote host.
    | ConnectionFailed of host: string * port: int * exn
    /// Connection was closed.
    | ConnectionClosed of string
    /// Failed to send data.
    | SendFailed of exn
    /// Failed to receive data.
    | ReceiveFailed of exn
    /// Operation timed out.
    | TimeoutError of string
    /// Buffer overflow occurred.
    | BufferOverflow of requested: int * available: int
    /// Invalid socket state.
    | InvalidState of expected: string * actual: string
    /// Failed to bind to a local address.
    | BindFailed of address: string * port: int * exn
    /// Failed to accept a connection.
    | AcceptFailed of exn
    /// Connection pool exhausted.
    | PoolExhausted of maxSize: int
    /// Connection pool is closed.
    | PoolClosed
    /// Codec encoding/decoding error.
    | CodecError of string * exn
    /// General socket error.
    | GeneralError of exn

    /// Gets a human-readable error message.
    /// <returns>A formatted string describing the error.</returns>
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

/// Module functions for working with SocketError.
module SocketError =

    /// Converts an exception to a SocketError.
    /// <param name="exn">The exception to convert.</param>
    /// <returns>The socket error.</returns>
    let fromException exn = GeneralError exn

    /// Converts a SocketError to an exception.
    /// <param name="err">The socket error.</param>
    /// <returns>The exception.</returns>
    let toException err =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// Configuration options for TCP socket connections.
type SocketConfig =
    {
        /// Remote host to connect to.
        Host: string
        /// Remote port to connect to.
        Port: int
        /// Address family (IPv4, IPv6, etc.).
        AddressFamily: Sockets.AddressFamily
        /// Socket type (Stream for TCP).
        SocketType: Sockets.SocketType
        /// Protocol type (Tcp for TCP).
        ProtocolType: Sockets.ProtocolType
        /// Send buffer size in bytes.
        SendBufferSize: int
        /// Receive buffer size in bytes.
        ReceiveBufferSize: int
        /// Send timeout in milliseconds (0 = infinite).
        SendTimeout: int
        /// Receive timeout in milliseconds (0 = infinite).
        ReceiveTimeout: int
        /// Enable/disable Nagle's algorithm.
        NoDelay: bool
        /// Enable linger on close (if true, waits LingerTimeout before closing).
        LingerEnabled: bool
        /// Linger timeout in seconds (0 = immediate close/abort).
        LingerTimeout: int
    }

/// Builder functions for SocketConfig.
module SocketConfig =

    /// Creates a default TCP socket configuration for the given host and port.
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

    /// Sets the send buffer size.
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withSendBufferSize (size: int, config: SocketConfig) = { config with SendBufferSize = size }

    /// Sets the receive buffer size.
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withReceiveBufferSize (size: int, config: SocketConfig) =
        { config with ReceiveBufferSize = size }

    /// Sets the send timeout in milliseconds.
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withSendTimeout (timeout: int, config: SocketConfig) = { config with SendTimeout = timeout }

    /// Sets the receive timeout in milliseconds.
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withReceiveTimeout (timeout: int, config: SocketConfig) =
        { config with ReceiveTimeout = timeout }

    /// Sets whether to enable/disable Nagle's algorithm.
    /// <param name="noDelay">True to disable Nagle's algorithm, false to enable it.</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withNoDelay (noDelay: bool, config: SocketConfig) = { config with NoDelay = noDelay }

    /// Sets the address family.
    /// <param name="family">The address family (IPv4, IPv6, etc.).</param>
    /// <param name="config">The socket configuration.</param>
    /// <returns>The updated socket configuration.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: SocketConfig) = { config with AddressFamily = family }

/// Configuration options for TCP server sockets.
type ServerSocketConfig =
    {
        /// Local address to bind to.
        BindAddress: string
        /// Local port to bind to.
        BindPort: int
        /// Address family (IPv4, IPv6, etc.).
        AddressFamily: Sockets.AddressFamily
        /// Socket type (Stream for TCP).
        SocketType: Sockets.SocketType
        /// Protocol type (Tcp for TCP).
        ProtocolType: Sockets.ProtocolType
        /// Connection backlog queue size.
        Backlog: int
        /// Default configuration for accepted connections.
        AcceptedSocketConfig: SocketConfig option
    }

/// Builder functions for ServerSocketConfig.
module ServerSocketConfig =

    /// Default configuration for localhost:8080.
    /// <returns>The default server socket configuration.</returns>
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

    /// Creates a default TCP server configuration.
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

    /// Sets the connection backlog queue size.
    /// <param name="backlog">The maximum length of the pending connections queue.</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withBacklog (backlog: int, config: ServerSocketConfig) = { config with Backlog = backlog }

    /// Sets the default configuration for accepted connections.
    /// <param name="acceptedConfig">The socket configuration to apply to accepted connections.</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withAcceptedConfig (acceptedConfig: SocketConfig, config: ServerSocketConfig) =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// Sets the address family.
    /// <param name="family">The address family (IPv4, IPv6, etc.).</param>
    /// <param name="config">The server socket configuration.</param>
    /// <returns>The updated server socket configuration.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: ServerSocketConfig) =
        { config with AddressFamily = family }

/// Configuration options for socket connection pools.
type SocketPoolConfig =
    {
        /// Socket configuration for connections in the pool.
        SocketConfig: SocketConfig
        /// Minimum number of connections in the pool.
        MinPoolSize: int
        /// Maximum number of connections in the pool.
        MaxPoolSize: int
        /// Connection lifetime in seconds (0 = infinite).
        ConnectionLifetime: int
        /// Enable connection validation before reuse.
        ValidateOnAcquire: bool
    }

/// Builder functions for SocketPoolConfig.
module SocketPoolConfig =

    /// Creates a default pool configuration.
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

    /// Sets the minimum pool size.
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

    /// Sets the maximum pool size.
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

    /// Sets the connection lifetime in seconds.
    /// <param name="lifetime">The connection lifetime in seconds (0 = infinite).</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withConnectionLifetime (lifetime: int, config: SocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// Sets whether to validate connections before reuse.
    /// <param name="validate">True to validate connections before reuse, false otherwise.</param>
    /// <param name="config">The socket pool configuration.</param>
    /// <returns>The updated socket pool configuration.</returns>
    let withValidateOnAcquire (validate: bool, config: SocketPoolConfig) =
        { config with ValidateOnAcquire = validate }

/// Represents a server socket listener (immutable wrapper).
/// Use ServerSocket module functions for operations.
type ServerSocket =
    internal
        {
            /// Underlying .NET Socket (listener).
            NetSocket: Sockets.Socket
            /// Server configuration.
            Config: ServerSocketConfig
        }
