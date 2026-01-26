namespace FSharp.FIO.Sockets

open FSharp.FIO.DSL

open System.Net

/// <summary>
/// Client operations for establishing TCP socket connections.
/// </summary>
[<RequireQualifiedAccess>]
module SocketClient =

    /// <summary>
    /// Connects to a remote host and returns a Socket.
    /// </summary>
    /// <param name="config">Socket configuration including host and port.</param>
    let connect (config: SocketConfig) : FIO<Socket, SocketError> =
        fio {
            let! netSocket = FIO.attempt((fun () ->
                let s = new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType)
                s.SendBufferSize <- config.SendBufferSize
                s.ReceiveBufferSize <- config.ReceiveBufferSize
                s.SendTimeout <- config.SendTimeout
                s.ReceiveTimeout <- config.ReceiveTimeout
                s.NoDelay <- config.NoDelay
                s),
                SocketError.FromException)

            do! FIO.awaitTask(
                netSocket.ConnectAsync(config.Host, config.Port),
                fun ex -> ConnectionFailed(config.Host, config.Port, ex))

            return new Socket(netSocket, config)
        }

    /// <summary>
    /// Connects to a remote host using host and port.
    /// </summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    let connectWith (host: string) (port: int) : FIO<Socket, SocketError> =
        fio {
            let! config = SocketConfig.create (host, port)
            return! connect config
        }

    /// <summary>
    /// Executes an action with a socket connection, automatically closing it.
    /// This is the FIO-idiomatic way to use sockets with guaranteed cleanup.
    /// </summary>
    /// <param name="config">Socket configuration.</param>
    /// <param name="action">Action to execute with the socket.</param>
    let withConnection (config: SocketConfig) (action: Socket -> FIO<'R, SocketError>) : FIO<'R, SocketError> =
        FIO.acquireRelease(
            connect config,
            (fun socket -> socket.Close().CatchAll(fun _ -> FIO.unit())),
            action
        )

    /// <summary>
    /// Executes an action with a socket connection using host and port.
    /// </summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="action">Action to execute with the socket.</param>
    let withConnectionTo (host: string) (port: int) (action: Socket -> FIO<'R, SocketError>) : FIO<'R, SocketError> =
        fio {
            let! config = SocketConfig.create (host, port)
            return! withConnection config action
        }

    /// <summary>
    /// Sends a value with a codec (convenience wrapper with auto-connection).
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="config">Socket configuration.</param>
    let sendWith<'T> (codec: SocketCodec<'T>) (value: 'T) (config: SocketConfig) : FIO<unit, SocketError> =
        withConnection config (fun socket -> socket.Send(codec, value))

    /// <summary>
    /// Receives a value with a codec (convenience wrapper with auto-connection).
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <param name="config">Socket configuration.</param>
    let receiveWith<'T> (codec: SocketCodec<'T>) (maxBytes: int) (config: SocketConfig) : FIO<'T, SocketError> =
        withConnection config (fun socket -> socket.Receive(codec, maxBytes))
