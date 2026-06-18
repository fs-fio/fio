namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

/// An error produced by a socket operation.
type SocketError =
    /// Connecting to the remote host failed.
    | ConnectionFailed of host: string * port: int * exn
    /// The connection was closed.
    | ConnectionClosed of string
    /// Sending data failed.
    | SendFailed of exn
    /// Receiving data failed.
    | ReceiveFailed of exn
    /// The operation timed out.
    | TimeoutError of string
    /// More data was requested than the buffer could hold.
    | BufferOverflow of requested: int * available: int
    /// The socket was in an unexpected state.
    | InvalidState of expected: string * actual: string
    /// Binding the server socket failed.
    | BindFailed of address: string * port: int * exn
    /// Accepting an incoming connection failed.
    | AcceptFailed of exn
    /// The connection pool ran out of available connections.
    | PoolExhausted of maxSize: int
    /// The connection pool is closed.
    | PoolClosed
    /// Encoding or decoding a value failed.
    | CodecError of string * exn
    /// An otherwise-unclassified error.
    | GeneralError of exn

    override this.ToString () =
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

[<RequireQualifiedAccess>]
module SocketError =

    /// Wraps an exception as a general socket error.
    let fromException ex = GeneralError ex

    /// Converts a socket error back into an exception.
    let toException error =
        match error with
        | GeneralError ex -> ex
        | _ -> Exception(error.ToString())

/// Configuration for a client socket connection.
type SocketConfig =
    {
        /// The remote host to connect to.
        Host: string
        /// The remote port to connect to.
        Port: int
        /// The socket address family.
        AddressFamily: Sockets.AddressFamily
        /// The socket type.
        SocketType: Sockets.SocketType
        /// The protocol type.
        ProtocolType: Sockets.ProtocolType
        /// The send buffer size, in bytes.
        SendBufferSize: int
        /// The receive buffer size, in bytes.
        ReceiveBufferSize: int
        /// The send timeout, in milliseconds.
        SendTimeout: int
        /// The receive timeout, in milliseconds.
        ReceiveTimeout: int
        /// Whether to disable Nagle's algorithm.
        NoDelay: bool
        /// Whether the socket lingers on close.
        LingerEnabled: bool
        /// The linger timeout, in seconds.
        LingerTimeout: int
    }

[<RequireQualifiedAccess>]
module SocketConfig =

    /// Creates a socket configuration for the given host and port, validating them.
    let create (host: string) (port: int) =
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

    /// Sets the send buffer size on a configuration.
    let withSendBufferSize (size: int) (config: SocketConfig) =
        { config with SendBufferSize = size }

    /// Sets the receive buffer size on a configuration.
    let withReceiveBufferSize (size: int) (config: SocketConfig) =
        { config with ReceiveBufferSize = size }

    /// Sets the send timeout on a configuration.
    let withSendTimeout (timeout: int) (config: SocketConfig) =
        { config with SendTimeout = timeout }

    /// Sets the receive timeout on a configuration.
    let withReceiveTimeout (timeout: int) (config: SocketConfig) =
        { config with ReceiveTimeout = timeout }

    /// Sets whether Nagle's algorithm is disabled on a configuration.
    let withNoDelay (noDelay: bool) (config: SocketConfig) =
        { config with NoDelay = noDelay }

    /// Sets the address family on a configuration.
    let withAddressFamily (family: Sockets.AddressFamily) (config: SocketConfig) =
        { config with AddressFamily = family }

/// Configuration for a server socket.
type ServerSocketConfig =
    {
        /// The local address to bind to.
        BindAddress: string
        /// The local port to bind to.
        BindPort: int
        /// The socket address family.
        AddressFamily: Sockets.AddressFamily
        /// The socket type.
        SocketType: Sockets.SocketType
        /// The protocol type.
        ProtocolType: Sockets.ProtocolType
        /// The maximum length of the pending-connection queue.
        Backlog: int
        /// The configuration applied to accepted client sockets, if any.
        AcceptedSocketConfig: SocketConfig option
    }

[<RequireQualifiedAccess>]
module ServerSocketConfig =

    /// The default server socket configuration (binds 127.0.0.1:8080, backlog 100).
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

    /// Creates a server socket configuration for the given bind address and port, validating them.
    let create (bindAddress: string) (bindPort: int) =
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

    /// Sets the pending-connection backlog on a configuration.
    let withBacklog (backlog: int) (config: ServerSocketConfig) =
        { config with Backlog = backlog }

    /// Sets the configuration applied to accepted client sockets.
    let withAcceptedConfig (acceptedConfig: SocketConfig) (config: ServerSocketConfig) =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// Sets the address family on a configuration.
    let withAddressFamily (family: Sockets.AddressFamily) (config: ServerSocketConfig) =
        { config with AddressFamily = family }

/// Configuration for a pool of client socket connections.
type SocketPoolConfig =
    {
        /// The configuration used for each pooled connection.
        SocketConfig: SocketConfig
        /// The minimum number of connections kept in the pool.
        MinPoolSize: int
        /// The maximum number of connections allowed in the pool.
        MaxPoolSize: int
        /// The maximum lifetime of a pooled connection, in seconds.
        ConnectionLifetime: int
        /// Whether to validate a connection when it is acquired.
        ValidateOnAcquire: bool
    }

[<RequireQualifiedAccess>]
module SocketPoolConfig =

    /// Creates a pool configuration from a socket configuration, using default pool sizing.
    let create (socketConfig: SocketConfig) =
        FIO.succeed
            {
                SocketConfig = socketConfig
                MinPoolSize = 0
                MaxPoolSize = 10
                ConnectionLifetime = 300
                ValidateOnAcquire = true
            }

    /// Sets the minimum pool size, validating it against the maximum.
    let withMinPoolSize (size: int) (config: SocketPoolConfig) =
        fio {
            if size < 0 then
                return! FIO.fail (InvalidState("MinPoolSize >= 0", $"{size}"))

            if size > config.MaxPoolSize then
                return!
                    FIO.fail (InvalidState(
                    $"MinPoolSize ({size}) <= MaxPoolSize ({config.MaxPoolSize})",
                    "MinPoolSize > MaxPoolSize"))

            return { config with MinPoolSize = size }
        }

    /// Sets the maximum pool size, validating it against the minimum.
    let withMaxPoolSize (size: int) (config: SocketPoolConfig) =
        fio {
            if size <= 0 then
                return! FIO.fail (InvalidState("MaxPoolSize > 0", $"{size}"))

            if size < config.MinPoolSize then
                return!
                    FIO.fail (InvalidState(
                    $"MaxPoolSize ({size}) >= MinPoolSize ({config.MinPoolSize})",
                    "MaxPoolSize < MinPoolSize"))

            return { config with MaxPoolSize = size }
        }

    /// Sets the maximum lifetime of a pooled connection.
    let withConnectionLifetime (lifetime: int) (config: SocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    /// Sets whether connections are validated on acquisition.
    let withValidateOnAcquire (validate: bool) (config: SocketPoolConfig) =
        { config with ValidateOnAcquire = validate }

/// An open TCP server socket.
type ServerSocket =
    internal
        {
            NetSocket: Sockets.Socket
            Config: ServerSocketConfig
        }
