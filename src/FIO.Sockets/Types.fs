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
    /// The operation timed out.
    | TimeoutError of string
    /// A line or frame from the peer exceeded the size limit passed to the receive.
    | BufferOverflow of requested: int * available: int
    /// An argument was invalid, such as an empty host, a port out of range or a non-positive buffer size.
    | InvalidState of expected: string * actual: string
    /// Binding the server socket failed.
    | BindFailed of address: string * port: int * exn
    /// Accepting an incoming connection failed.
    | AcceptFailed of exn
    /// Encoding or decoding a value failed.
    | CodecError of string * exn
    /// An otherwise-unclassified error.
    | GeneralError of exn

    override this.ToString () : string =
        match this with
        | ConnectionFailed(host, port, ex) ->
            $"Failed to connect to {host}:{port}: {ex.Message}"
        | ConnectionClosed msg ->
            $"Connection closed: {msg}"
        | TimeoutError msg ->
            $"Timeout: {msg}"
        | BufferOverflow(requested, available) ->
            $"Buffer overflow: requested {requested} bytes, only {available} available"
        | InvalidState(expected, actual) ->
            $"Invalid state: expected {expected}, actual {actual}"
        | BindFailed(addr, port, ex) ->
            $"Failed to bind to {addr}:{port}: {ex.Message}"
        | AcceptFailed ex ->
            $"Accept failed: {ex.Message}"
        | CodecError(msg, ex) ->
            $"Codec error: {msg} - {ex.Message}"
        | GeneralError ex ->
            $"Socket error: {ex.Message}"

[<RequireQualifiedAccess>]
module SocketError =

    /// Wraps an exception as a general socket error.
    let fromException ex : SocketError = GeneralError ex

    /// Converts a socket error back into an exception: a GeneralError's own exception, otherwise a new one that keeps
    /// any wrapped exception as its inner exception.
    let toException error : exn =
        match error with
        | GeneralError ex -> ex
        | ConnectionFailed(_, _, ex)
        | BindFailed(_, _, ex)
        | AcceptFailed ex
        | CodecError(_, ex) -> Exception(error.ToString(), ex)
        | ConnectionClosed _
        | TimeoutError _
        | BufferOverflow _
        | InvalidState _ -> Exception(error.ToString())

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
    let create (host: string) (port: int) : FIO<SocketConfig, SocketError> =
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
                    SendTimeout = 0
                    ReceiveTimeout = 0
                    NoDelay = true
                    LingerEnabled = false
                    LingerTimeout = 0
                }
        }

    /// Sets the send buffer size on a configuration.
    let withSendBufferSize (size: int) (config: SocketConfig) : SocketConfig =
        { config with SendBufferSize = size }

    /// Sets the receive buffer size on a configuration.
    let withReceiveBufferSize (size: int) (config: SocketConfig) : SocketConfig =
        { config with ReceiveBufferSize = size }

    /// Sets the send timeout on a configuration.
    let withSendTimeout (timeout: int) (config: SocketConfig) : SocketConfig =
        { config with SendTimeout = timeout }

    /// Sets the receive timeout on a configuration.
    let withReceiveTimeout (timeout: int) (config: SocketConfig) : SocketConfig =
        { config with ReceiveTimeout = timeout }

    /// Sets whether Nagle's algorithm is disabled on a configuration.
    let withNoDelay (noDelay: bool) (config: SocketConfig) : SocketConfig =
        { config with NoDelay = noDelay }

    /// Sets how closing treats unsent data: disabled closes gracefully, enabled with 0 resets the connection at once,
    /// and enabled with a positive timeoutSeconds asks the system to bound the graceful close by that time.
    let withLinger (enabled: bool) (timeoutSeconds: int) (config: SocketConfig) : SocketConfig =
        { config with LingerEnabled = enabled; LingerTimeout = timeoutSeconds }

    /// Sets the address family on a configuration.
    let withAddressFamily (family: Sockets.AddressFamily) (config: SocketConfig) : SocketConfig =
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
    let defaultConfig : ServerSocketConfig =
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
    let create (bindAddress: string) (bindPort: int) : FIO<ServerSocketConfig, SocketError> =
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
    let withBacklog (backlog: int) (config: ServerSocketConfig) : ServerSocketConfig =
        { config with Backlog = backlog }

    /// Sets the configuration applied to accepted client sockets.
    let withAcceptedConfig (acceptedConfig: SocketConfig) (config: ServerSocketConfig) : ServerSocketConfig =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    /// Sets the address family on a configuration.
    let withAddressFamily (family: Sockets.AddressFamily) (config: ServerSocketConfig) : ServerSocketConfig =
        { config with AddressFamily = family }

/// An open TCP server socket.
type ServerSocket =
    internal
        {
            NetSocket: Sockets.Socket
            Config: ServerSocketConfig
        }
