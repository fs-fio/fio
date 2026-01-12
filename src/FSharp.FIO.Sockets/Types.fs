namespace FSharp.FIO.Sockets

open FSharp.FIO.DSL

open System
open System.Net

/// <summary>
/// Socket operation errors.
/// </summary>
type SocketError =
    /// Failed to connect to a remote host
    | ConnectionFailed of host: string * port: int * exn
    /// Connection was closed
    | ConnectionClosed of string
    /// Failed to send data
    | SendFailed of exn
    /// Failed to receive data
    | ReceiveFailed of exn
    /// Operation timed out
    | TimeoutError of string
    /// Buffer overflow occurred
    | BufferOverflow of requested: int * available: int
    /// Invalid socket state
    | InvalidState of expected: string * actual: string
    /// Failed to bind to a local address
    | BindFailed of address: string * port: int * exn
    /// Failed to accept a connection
    | AcceptFailed of exn
    /// Connection pool exhausted
    | PoolExhausted of maxSize: int
    /// Connection pool is closed
    | PoolClosed
    /// Codec encoding/decoding error
    | CodecError of string * exn
    /// General socket error
    | GeneralError of exn

    /// <summary>
    /// Converts an exception to a SocketError.
    /// </summary>
    /// <param name="ex">The exception to convert.</param>
    static member FromException (exn: exn) : SocketError =
        GeneralError exn

    /// <summary>
    /// Gets a human-readable error message.
    /// </summary>
    /// <param name="err">The socket error.</param>
    static member ToString (err: SocketError) : string =
        match err with
        | ConnectionFailed(host, port, exn) ->
            $"Failed to connect to {host}:{port}: {exn.Message}"
        | ConnectionClosed msg ->
            $"Connection closed: {msg}"
        | SendFailed exn ->
            $"Send failed: {exn.Message}"
        | ReceiveFailed exn ->
            $"Receive failed: {exn.Message}"
        | TimeoutError msg ->
            $"Timeout: {msg}"
        | BufferOverflow(requested, available) ->
            $"Buffer overflow: requested {requested} bytes, only {available} available"
        | InvalidState(expected, actual) ->
            $"Invalid state: expected {expected}, actual {actual}"
        | BindFailed(addr, port, exn) ->
            $"Failed to bind to {addr}:{port}: {exn.Message}"
        | AcceptFailed exn ->
            $"Accept failed: {exn.Message}"
        | PoolExhausted maxSize ->
            $"Pool exhausted: maximum {maxSize} connections"
        | PoolClosed ->
            "Pool is closed"
        | CodecError(msg, exn) ->
            $"Codec error: {msg} - {exn.Message}"
        | GeneralError exn ->
            $"Socket error: {exn.Message}"

    static member ToException (err: SocketError) : exn =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(SocketError.ToString err)

/// <summary>
/// Configuration options for TCP socket connections.
/// </summary>
type SocketConfig =
    {
        /// Remote host to connect to
        Host: string
        /// Remote port to connect to
        Port: int
        /// Address family (IPv4, IPv6, etc.)
        AddressFamily: Sockets.AddressFamily
        /// Socket type (Stream for TCP)
        SocketType: Sockets.SocketType
        /// Protocol type (Tcp for TCP)
        ProtocolType: Sockets.ProtocolType
        /// Send buffer size in bytes
        SendBufferSize: int
        /// Receive buffer size in bytes
        ReceiveBufferSize: int
        /// Send timeout in milliseconds (0 = infinite)
        SendTimeout: int
        /// Receive timeout in milliseconds (0 = infinite)
        ReceiveTimeout: int
        /// Enable/disable Nagle's algorithm
        NoDelay: bool
    }

/// <summary>
/// Builder functions for SocketConfig.
/// </summary>
module SocketConfig =

    /// <summary>
    /// Creates a default TCP socket configuration for the given host and port.
    /// Validates the host and port parameters.
    /// </summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to (1-65535).</param>
    let create (host: string, port: int) : FIO<SocketConfig, SocketError> =
        fio {
            if String.IsNullOrWhiteSpace host then
                return! FIO.Fail(InvalidState("non-empty host", "empty or whitespace"))

            if port < 1 || port > 65535 then
                return! FIO.Fail(InvalidState("port in range 1-65535", $"{port}"))

            return
                { Host = host
                  Port = port
                  AddressFamily = Sockets.AddressFamily.InterNetwork
                  SocketType = Sockets.SocketType.Stream
                  ProtocolType = Sockets.ProtocolType.Tcp
                  SendBufferSize = 8192
                  ReceiveBufferSize = 8192
                  SendTimeout = 30000
                  ReceiveTimeout = 30000
                  NoDelay = true }
        }

    /// <summary>
    /// Sets the send buffer size.
    /// </summary>
    let withSendBufferSize (size: int, config: SocketConfig) : SocketConfig =
        { config with SendBufferSize = size }

    /// <summary>
    /// Sets the receive buffer size.
    /// </summary>
    let withReceiveBufferSize (size: int, config: SocketConfig) : SocketConfig =
        { config with ReceiveBufferSize = size }

    /// <summary>
    /// Sets the send timeout in milliseconds.
    /// </summary>
    let withSendTimeout (timeout: int, config: SocketConfig) : SocketConfig =
        { config with SendTimeout = timeout }

    /// <summary>
    /// Sets the receive timeout in milliseconds.
    /// </summary>
    let withReceiveTimeout (timeout: int, config: SocketConfig) : SocketConfig =
        { config with ReceiveTimeout = timeout }

    /// <summary>
    /// Sets whether to enable/disable Nagle's algorithm.
    /// </summary>
    let withNoDelay (noDelay: bool, config: SocketConfig) : SocketConfig =
        { config with NoDelay = noDelay }

    /// <summary>
    /// Sets the address family.
    /// </summary>
    let withAddressFamily (family: Sockets.AddressFamily, config: SocketConfig) : SocketConfig =
        { config with AddressFamily = family }

/// <summary>
/// Configuration options for TCP server sockets.
/// </summary>
type ServerSocketConfig =
    {
        /// Local address to bind to
        BindAddress: string
        /// Local port to bind to
        BindPort: int
        /// Address family (IPv4, IPv6, etc.)
        AddressFamily: Sockets.AddressFamily
        /// Socket type (Stream for TCP)
        SocketType: Sockets.SocketType
        /// Protocol type (Tcp for TCP)
        ProtocolType: Sockets.ProtocolType
        /// Connection backlog queue size
        Backlog: int
        /// Default configuration for accepted connections
        AcceptedSocketConfig: SocketConfig option
    }

    /// <summary>
    /// Default configuration for localhost:8080.
    /// </summary>
    static member Default =
        { BindAddress = "127.0.0.1"
          BindPort = 8080
          AddressFamily = Sockets.AddressFamily.InterNetwork
          SocketType = Sockets.SocketType.Stream
          ProtocolType = Sockets.ProtocolType.Tcp
          Backlog = 100
          AcceptedSocketConfig = None }

/// <summary>
/// Builder functions for ServerSocketConfig.
/// </summary>
module ServerSocketConfig =

    /// <summary>
    /// Creates a default TCP server configuration.
    /// Validates the bind address and port parameters.
    /// </summary>
    /// <param name="bindAddress">The local address to bind to.</param>
    /// <param name="bindPort">The local port to bind to (0-65535, where 0 = any available port).</param>
    let create (bindAddress: string, bindPort: int) : FIO<ServerSocketConfig, SocketError> =
        fio {
            if String.IsNullOrWhiteSpace bindAddress then
                return! FIO.Fail(InvalidState("non-empty bind address", "empty or whitespace"))

            if bindPort < 0 || bindPort > 65535 then
                return! FIO.Fail(InvalidState("port in range 0-65535", $"{bindPort}"))

            return
                { BindAddress = bindAddress
                  BindPort = bindPort
                  AddressFamily = Sockets.AddressFamily.InterNetwork
                  SocketType = Sockets.SocketType.Stream
                  ProtocolType = Sockets.ProtocolType.Tcp
                  Backlog = 100
                  AcceptedSocketConfig = None }
        }

    /// <summary>
    /// Sets the connection backlog queue size.
    /// </summary>
    let withBacklog (backlog: int, config: ServerSocketConfig) : ServerSocketConfig =
        { config with Backlog = backlog }

    /// <summary>
    /// Sets the default configuration for accepted connections.
    /// </summary>
    let withAcceptedConfig (acceptedConfig: SocketConfig, config: ServerSocketConfig) : ServerSocketConfig =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// <summary>
    /// Sets the address family.
    /// </summary>
    let withAddressFamily (family: Sockets.AddressFamily, config: ServerSocketConfig) : ServerSocketConfig =
        { config with AddressFamily = family }

/// <summary>
/// Configuration options for socket connection pools.
/// </summary>
type SocketPoolConfig =
    {
        /// Socket configuration for connections in the pool
        SocketConfig: SocketConfig
        /// Minimum number of connections in the pool
        MinPoolSize: int
        /// Maximum number of connections in the pool
        MaxPoolSize: int
        /// Connection lifetime in seconds (0 = infinite)
        ConnectionLifetime: int
        /// Enable connection validation before reuse
        ValidateOnAcquire: bool
    }

/// <summary>
/// Builder functions for SocketPoolConfig.
/// </summary>
module SocketPoolConfig =

    /// <summary>
    /// Creates a default pool configuration.
    /// Validates pool size parameters.
    /// </summary>
    /// <param name="socketConfig">The socket configuration for connections in the pool.</param>
    let create (socketConfig: SocketConfig) : FIO<SocketPoolConfig, SocketError> =
        fio {
            // Default values
            let minPoolSize = 0
            let maxPoolSize = 10
            let connectionLifetime = 300

            if minPoolSize < 0 then
                return! FIO.Fail(InvalidState("MinPoolSize >= 0", $"{minPoolSize}"))

            if maxPoolSize <= 0 then
                return! FIO.Fail(InvalidState("MaxPoolSize > 0", $"{maxPoolSize}"))

            if maxPoolSize < minPoolSize then
                return! FIO.Fail(InvalidState($"MaxPoolSize ({maxPoolSize}) >= MinPoolSize ({minPoolSize})", "MaxPoolSize < MinPoolSize"))

            if connectionLifetime < 0 then
                return! FIO.Fail(InvalidState("ConnectionLifetime >= 0", $"{connectionLifetime}"))

            return
                { SocketConfig = socketConfig
                  MinPoolSize = minPoolSize
                  MaxPoolSize = maxPoolSize
                  ConnectionLifetime = connectionLifetime
                  ValidateOnAcquire = true }
        }

    /// <summary>
    /// Sets the minimum pool size.
    /// Validates that MinPoolSize >= 0 and MinPoolSize <= MaxPoolSize.
    /// </summary>
    let withMinPoolSize (size: int, config: SocketPoolConfig) : FIO<SocketPoolConfig, SocketError> =
        fio {
            if size < 0 then
                return! FIO.Fail(InvalidState("MinPoolSize >= 0", $"{size}"))

            if size > config.MaxPoolSize then
                return! FIO.Fail(InvalidState($"MinPoolSize ({size}) <= MaxPoolSize ({config.MaxPoolSize})", "MinPoolSize > MaxPoolSize"))

            return { config with MinPoolSize = size }
        }

    /// <summary>
    /// Sets the maximum pool size.
    /// Validates that MaxPoolSize > 0 and MaxPoolSize >= MinPoolSize.
    /// </summary>
    let withMaxPoolSize (size: int, config: SocketPoolConfig) : FIO<SocketPoolConfig, SocketError> =
        fio {
            if size <= 0 then
                return! FIO.Fail(InvalidState("MaxPoolSize > 0", $"{size}"))

            if size < config.MinPoolSize then
                return! FIO.Fail(InvalidState($"MaxPoolSize ({size}) >= MinPoolSize ({config.MinPoolSize})", "MaxPoolSize < MinPoolSize"))

            return { config with MaxPoolSize = size }
        }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    let withConnectionLifetime (lifetime: int, config: SocketPoolConfig) : SocketPoolConfig =
        { config with ConnectionLifetime = lifetime }

    /// <summary>
    /// Sets whether to validate connections before reuse.
    /// </summary>
    let withValidateOnAcquire (validate: bool, config: SocketPoolConfig) : SocketPoolConfig =
        { config with ValidateOnAcquire = validate }

/// <summary>
/// Represents a server socket listener (immutable wrapper).
/// Use ServerSocket module functions for operations.
/// </summary>
type ServerSocket =
    internal
        {
            /// Underlying .NET Socket (listener)
            NetSocket: Sockets.Socket
            /// Server configuration
            Config: ServerSocketConfig
        }
