namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

type SocketError =
    | ConnectionFailed of host: string * port: int * exn
    | ConnectionClosed of string
    | SendFailed of exn
    | ReceiveFailed of exn
    | TimeoutError of string
    | BufferOverflow of requested: int * available: int
    | InvalidState of expected: string * actual: string
    | BindFailed of address: string * port: int * exn
    | AcceptFailed of exn
    | PoolExhausted of maxSize: int
    | PoolClosed
    | CodecError of string * exn
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

    let fromException ex = GeneralError ex

    let toException error =
        match error with
        | GeneralError ex -> ex
        | _ -> Exception(error.ToString())

type SocketConfig =
    {
        Host: string
        Port: int
        AddressFamily: Sockets.AddressFamily
        SocketType: Sockets.SocketType
        ProtocolType: Sockets.ProtocolType
        SendBufferSize: int
        ReceiveBufferSize: int
        SendTimeout: int
        ReceiveTimeout: int
        NoDelay: bool
        LingerEnabled: bool
        LingerTimeout: int
    }

[<RequireQualifiedAccess>]
module SocketConfig =

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

    let withSendBufferSize (size: int) (config: SocketConfig) =
        { config with SendBufferSize = size }

    let withReceiveBufferSize (size: int) (config: SocketConfig) =
        { config with ReceiveBufferSize = size }

    let withSendTimeout (timeout: int) (config: SocketConfig) =
        { config with SendTimeout = timeout }

    let withReceiveTimeout (timeout: int) (config: SocketConfig) =
        { config with ReceiveTimeout = timeout }

    let withNoDelay (noDelay: bool) (config: SocketConfig) =
        { config with NoDelay = noDelay }

    let withAddressFamily (family: Sockets.AddressFamily) (config: SocketConfig) =
        { config with AddressFamily = family }

type ServerSocketConfig =
    {
        BindAddress: string
        BindPort: int
        AddressFamily: Sockets.AddressFamily
        SocketType: Sockets.SocketType
        ProtocolType: Sockets.ProtocolType
        Backlog: int
        AcceptedSocketConfig: SocketConfig option
    }

[<RequireQualifiedAccess>]
module ServerSocketConfig =

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

    let withBacklog (backlog: int) (config: ServerSocketConfig) =
        { config with Backlog = backlog }

    let withAcceptedConfig (acceptedConfig: SocketConfig) (config: ServerSocketConfig) =
        { config with AcceptedSocketConfig = Some acceptedConfig }

    let withAddressFamily (family: Sockets.AddressFamily) (config: ServerSocketConfig) =
        { config with AddressFamily = family }

type SocketPoolConfig =
    {
        SocketConfig: SocketConfig
        MinPoolSize: int
        MaxPoolSize: int
        ConnectionLifetime: int
        ValidateOnAcquire: bool
    }

[<RequireQualifiedAccess>]
module SocketPoolConfig =

    let create (socketConfig: SocketConfig) =
        FIO.succeed
            {
                SocketConfig = socketConfig
                MinPoolSize = 0
                MaxPoolSize = 10
                ConnectionLifetime = 300
                ValidateOnAcquire = true
            }

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

    let withConnectionLifetime (lifetime: int) (config: SocketPoolConfig) =
        { config with ConnectionLifetime = lifetime }

    let withValidateOnAcquire (validate: bool) (config: SocketPoolConfig) =
        { config with ValidateOnAcquire = validate }

type ServerSocket =
    internal
        {
            NetSocket: Sockets.Socket
            Config: ServerSocketConfig
        }
