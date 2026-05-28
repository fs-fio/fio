namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

/// <summary>Represents a socket operation error.</summary>
type SocketError =
    /// <summary>Represents a failed connection attempt to a remote host.</summary>
    | ConnectionFailed of host: string * port: int * exn
    /// <summary>Represents a closed connection.</summary>
    | ConnectionClosed of string
    /// <summary>Represents a failure during a send operation.</summary>
    | SendFailed of exn
    /// <summary>Represents a failure during a receive operation.</summary>
    | ReceiveFailed of exn
    /// <summary>Represents a timed-out socket operation.</summary>
    | TimeoutError of string
    /// <summary>Represents a buffer overflow when the requested size exceeds available capacity.</summary>
    | BufferOverflow of requested: int * available: int
    /// <summary>Represents an invalid socket state where the expected and actual states differ.</summary>
    | InvalidState of expected: string * actual: string
    /// <summary>Represents a failure to bind to a local address.</summary>
    | BindFailed of address: string * port: int * exn
    /// <summary>Represents a failure to accept an incoming connection.</summary>
    | AcceptFailed of exn
    /// <summary>Represents an exhausted connection pool that has reached its maximum size.</summary>
    | PoolExhausted of maxSize: int
    /// <summary>Represents an attempt to use a closed connection pool.</summary>
    | PoolClosed
    /// <summary>Represents a codec encoding or decoding error.</summary>
    | CodecError of string * exn
    /// <summary>Represents a general socket error wrapping an exception.</summary>
    | GeneralError of exn

    /// <summary>Returns a human-readable description of the error.</summary>
    /// <returns>A formatted string describing the error and its details.</returns>
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

/// <summary>Creates and converts SocketError values.</summary>
module SocketError =

    /// <summary>Creates a SocketError from an exception.</summary>
    /// <param name="exn">The exception to wrap as a general socket error.</param>
    /// <returns>A GeneralError wrapping the given exception.</returns>
    let fromException exn = GeneralError exn

    /// <summary>Returns the exception underlying a SocketError.</summary>
    /// <param name="error">The socket error to convert.</param>
    /// <returns>The original exception for a GeneralError, or a new exception with the error description otherwise.</returns>
    let toException error =
        match error with
        | GeneralError exn -> exn
        | _ -> Exception(error.ToString())

/// <summary>Represents configuration options for a TCP socket connection.</summary>
type SocketConfig =
    {
        /// <summary>Represents the remote host to connect to.</summary>
        Host: string
        /// <summary>Represents the remote port to connect to.</summary>
        Port: int
        /// <summary>Represents the address family for the connection.</summary>
        AddressFamily: Sockets.AddressFamily
        /// <summary>Represents the socket type for the connection.</summary>
        SocketType: Sockets.SocketType
        /// <summary>Represents the protocol type for the connection.</summary>
        ProtocolType: Sockets.ProtocolType
        /// <summary>Represents the send buffer size in bytes.</summary>
        SendBufferSize: int
        /// <summary>Represents the receive buffer size in bytes.</summary>
        ReceiveBufferSize: int
        /// <summary>Represents the send timeout in milliseconds, where zero means infinite.</summary>
        SendTimeout: int
        /// <summary>Represents the receive timeout in milliseconds, where zero means infinite.</summary>
        ReceiveTimeout: int
        /// <summary>Represents whether Nagle's algorithm is disabled.</summary>
        NoDelay: bool
        /// <summary>Represents whether linger is enabled on close.</summary>
        LingerEnabled: bool
        /// <summary>Represents the linger timeout in seconds, where zero means immediate close.</summary>
        LingerTimeout: int
    }

/// <summary>Creates and customizes SocketConfig values for TCP connections.</summary>
module SocketConfig =

    /// <summary>Creates a default TCP socket configuration for the given host and port.</summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to, in the range 1–65535.</param>
    /// <returns>An effect that produces a validated SocketConfig with default buffer, timeout, and linger settings.</returns>
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

    /// <summary>Returns a configuration with the specified send buffer size.</summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated send buffer size.</returns>
    let withSendBufferSize (size: int, config: SocketConfig) = { config with SendBufferSize = size }

    /// <summary>Returns a configuration with the specified receive buffer size.</summary>
    /// <param name="size">The buffer size in bytes.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated receive buffer size.</returns>
    let withReceiveBufferSize (size: int, config: SocketConfig) =
        { config with ReceiveBufferSize = size }

    /// <summary>Returns a configuration with the specified send timeout.</summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated send timeout.</returns>
    let withSendTimeout (timeout: int, config: SocketConfig) = { config with SendTimeout = timeout }

    /// <summary>Returns a configuration with the specified receive timeout.</summary>
    /// <param name="timeout">The timeout in milliseconds.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated receive timeout.</returns>
    let withReceiveTimeout (timeout: int, config: SocketConfig) =
        { config with ReceiveTimeout = timeout }

    /// <summary>Returns a configuration with the specified Nagle's algorithm setting.</summary>
    /// <param name="noDelay">True to disable Nagle's algorithm, false to enable it.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated NoDelay setting.</returns>
    let withNoDelay (noDelay: bool, config: SocketConfig) = { config with NoDelay = noDelay }

    /// <summary>Returns a configuration with the specified address family.</summary>
    /// <param name="family">The address family to use for the connection.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketConfig with the updated address family.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: SocketConfig) = { config with AddressFamily = family }

/// <summary>Represents configuration options for a TCP server socket.</summary>
type ServerSocketConfig =
    {
        /// <summary>Represents the local address to bind to.</summary>
        BindAddress: string
        /// <summary>Represents the local port to bind to.</summary>
        BindPort: int
        /// <summary>Represents the address family for the server socket.</summary>
        AddressFamily: Sockets.AddressFamily
        /// <summary>Represents the socket type for the server socket.</summary>
        SocketType: Sockets.SocketType
        /// <summary>Represents the protocol type for the server socket.</summary>
        ProtocolType: Sockets.ProtocolType
        /// <summary>Represents the maximum length of the pending connections queue.</summary>
        Backlog: int
        /// <summary>Represents the optional default configuration applied to accepted connections.</summary>
        AcceptedSocketConfig: SocketConfig option
    }

/// <summary>Creates and customizes ServerSocketConfig values for TCP server listeners.</summary>
module ServerSocketConfig =

    /// <summary>Returns the default server socket configuration for localhost on port 8080.</summary>
    /// <returns>A ServerSocketConfig bound to 127.0.0.1:8080 with a backlog of 100.</returns>
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

    /// <summary>Creates a validated TCP server socket configuration for the given address and port.</summary>
    /// <param name="bindAddress">The local address to bind to; must not be empty or whitespace.</param>
    /// <param name="bindPort">The local port to bind to, in the range 0–65535, where zero selects any available port.</param>
    /// <returns>An effect that produces a validated ServerSocketConfig with default backlog and protocol settings.</returns>
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

    /// <summary>Returns a configuration with the specified connection backlog size.</summary>
    /// <param name="backlog">The maximum length of the pending connections queue.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new ServerSocketConfig with the updated backlog.</returns>
    let withBacklog (backlog: int, config: ServerSocketConfig) = { config with Backlog = backlog }

    /// <summary>Returns a configuration with the specified default settings for accepted connections.</summary>
    /// <param name="acceptedConfig">The socket configuration to apply to accepted connections.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new ServerSocketConfig with the updated accepted socket configuration.</returns>
    let withAcceptedConfig (acceptedConfig: SocketConfig, config: ServerSocketConfig) =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// <summary>Returns a configuration with the specified address family.</summary>
    /// <param name="family">The address family to use for the server socket.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new ServerSocketConfig with the updated address family.</returns>
    let withAddressFamily (family: Sockets.AddressFamily, config: ServerSocketConfig) =
        { config with AddressFamily = family }

/// <summary>Represents configuration options for a socket connection pool.</summary>
type SocketPoolConfig =
    {
        /// <summary>Represents the socket configuration for connections in the pool.</summary>
        SocketConfig: SocketConfig
        /// <summary>Represents the minimum number of connections maintained in the pool.</summary>
        MinPoolSize: int
        /// <summary>Represents the maximum number of connections allowed in the pool.</summary>
        MaxPoolSize: int
        /// <summary>Represents the connection lifetime in seconds, where zero means infinite.</summary>
        ConnectionLifetime: int
        /// <summary>Represents whether connections are validated before reuse.</summary>
        ValidateOnAcquire: bool
    }

/// <summary>Creates and customizes SocketPoolConfig values for connection pools.</summary>
module SocketPoolConfig =

    /// <summary>Creates a validated pool configuration with default size and lifetime settings.</summary>
    /// <param name="socketConfig">The socket configuration for connections in the pool.</param>
    /// <returns>An effect that produces a validated SocketPoolConfig with default pool sizes and lifetime.</returns>
    let create (socketConfig: SocketConfig) =
        fio {
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

    /// <summary>Returns a pool configuration with the specified minimum size.</summary>
    /// <param name="size">The minimum number of connections in the pool; must not exceed the current maximum.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>An effect that produces the updated SocketPoolConfig, or fails if the size violates constraints.</returns>
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

    /// <summary>Returns a pool configuration with the specified maximum size.</summary>
    /// <param name="size">The maximum number of connections in the pool; must be positive and not less than the current minimum.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>An effect that produces the updated SocketPoolConfig, or fails if the size violates constraints.</returns>
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

    /// <summary>Returns a pool configuration with the specified connection lifetime.</summary>
    /// <param name="lifetime">The connection lifetime in seconds, where zero means infinite.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketPoolConfig with the updated connection lifetime.</returns>
    let withConnectionLifetime (lifetime: int, config: SocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// <summary>Returns a pool configuration with the specified connection validation setting.</summary>
    /// <param name="validate">True to validate connections before reuse, false otherwise.</param>
    /// <param name="config">The base configuration to update.</param>
    /// <returns>A new SocketPoolConfig with the updated validation setting.</returns>
    let withValidateOnAcquire (validate: bool, config: SocketPoolConfig) =
        { config with ValidateOnAcquire = validate }

/// <summary>Represents a TCP server socket listener.</summary>
type ServerSocket =
    internal
        {
            /// <summary>Represents the underlying .NET socket for accepting connections.</summary>
            NetSocket: Sockets.Socket
            /// <summary>Represents the server socket configuration.</summary>
            Config: ServerSocketConfig
        }
