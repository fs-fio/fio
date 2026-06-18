namespace FIO.Sockets

open FIO.DSL

open System.Net

[<RequireQualifiedAccess>]
module SocketClient =

    /// Connects to a remote host using the given configuration, returning an open socket.
    let connect (config: SocketConfig) =
        fio {
            let! netSocket =
                FIO.attempt
                    (fun () ->
                        let socket =
                            new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType)
                        socket.SendBufferSize <- config.SendBufferSize
                        socket.ReceiveBufferSize <- config.ReceiveBufferSize
                        socket.SendTimeout <- config.SendTimeout
                        socket.ReceiveTimeout <- config.ReceiveTimeout
                        socket.NoDelay <- config.NoDelay
                        socket)
                    SocketError.fromException

            let! cancelToken = FIO.cancellationToken ()

            do!
                FIO.awaitUnitTask
                    (netSocket.ConnectAsync(config.Host, config.Port, cancelToken).AsTask())
                    (fun ex -> ConnectionFailed(config.Host, config.Port, ex))

            return new Socket(netSocket, config)
        }

    /// Connects to the given host and port using default configuration, returning an open socket.
    let connectWith (host: string) (port: int) =
        fio {
            let! config = SocketConfig.create host port
            return! connect config
        }

    /// Connects, runs an action with the open socket, then closes the connection.
    let withConnection (config: SocketConfig) (action: Socket -> FIO<'A, SocketError>) =
        FIO.acquireReleaseWith (connect config) (fun socket -> socket.Close().Ignore()) action

    /// Connects to the given host and port, runs an action with the open socket, then closes the connection.
    let withConnectionTo (host: string) (port: int) (action: Socket -> FIO<'A, SocketError>) =
        fio {
            let! config = SocketConfig.create host port
            return! withConnection config action
        }

    /// Connects, sends a single value encoded with the given codec, then closes the connection.
    let sendWith<'A> (codec: SocketCodec<'A>) (value: 'A) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Send(codec, value))

    /// Connects, receives a single value decoded with the given codec, then closes the connection.
    let receiveWith<'A> (codec: SocketCodec<'A>) (maxBytes: int) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Receive(codec, maxBytes))
