namespace FIO.Sockets

open FIO.DSL

open System.Net

[<RequireQualifiedAccess>]
module SocketClient =

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

    let connectWith (host: string) (port: int) =
        fio {
            let! config = SocketConfig.create host port
            return! connect config
        }

    let withConnection (config: SocketConfig) (action: Socket -> FIO<'A, SocketError>) =
        FIO.acquireReleaseWith (connect config) (fun socket -> socket.Close().Ignore()) action

    let withConnectionTo (host: string) (port: int) (action: Socket -> FIO<'A, SocketError>) =
        fio {
            let! config = SocketConfig.create host port
            return! withConnection config action
        }

    let sendWith<'A> (codec: SocketCodec<'A>) (value: 'A) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Send(codec, value))

    let receiveWith<'A> (codec: SocketCodec<'A>) (maxBytes: int) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Receive(codec, maxBytes))
