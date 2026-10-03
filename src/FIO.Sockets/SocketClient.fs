namespace FIO.Sockets

open FIO.DSL

open System.Net
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module SocketClient =

    let private createNetSocket (config: SocketConfig) =
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

    // ConnectAsync throws at once for an argument it rejects, such as a port out of range; as a faulted task that
    // still fails as ConnectionFailed.
    let private connectAsync (netSocket: Sockets.Socket) (config: SocketConfig) (cancellationToken: CancellationToken) =
        try
            netSocket.ConnectAsync(config.Host, config.Port, cancellationToken).AsTask()
        with ex ->
            Task.FromException ex

    /// Connects to a remote host using the given configuration, returning an open socket.
    let connect (config: SocketConfig) : FIO<Socket, SocketError> =
        fio {
            let! netSocket = createNetSocket config

            let! cancellationToken = FIO.cancellationToken ()

            do!
                (FIO.awaitUnitTask
                    (connectAsync netSocket config cancellationToken)
                    (fun ex -> ConnectionFailed(config.Host, config.Port, ex)))
                    .TapError(fun _ ->
                        FIO.succeedWith (fun () ->
                            try netSocket.Dispose()
                            with _ -> ()))

            return new Socket(netSocket, config)
        }

    /// Connects to the given host and port using default configuration, returning an open socket.
    let connectWith (host: string) (port: int) : FIO<Socket, SocketError> =
        fio {
            let! config = SocketConfig.create host port
            return! connect config
        }

    /// Connects, runs an action with the open socket, then closes the connection.
    let withConnection (config: SocketConfig) (action: Socket -> FIO<'A, SocketError>) : FIO<'A, SocketError> =
        // Acquire runs uninterruptibly, so it only creates the socket; the connect happens in use,
        // where an interruption still cancels it, and release also covers a socket that never connected.
        let release (netSocket: Sockets.Socket) =
            (FIO.attempt (fun () -> netSocket.Dispose()) SocketError.fromException).Ignore()

        let connectThenAct (netSocket: Sockets.Socket) =
            fio {
                let! cancellationToken = FIO.cancellationToken ()

                do!
                    FIO.awaitUnitTask
                        (connectAsync netSocket config cancellationToken)
                        (fun ex -> ConnectionFailed(config.Host, config.Port, ex))

                let socket = new Socket(netSocket, config)
                return! (action socket).Ensuring(socket.Close().Ignore())
            }

        FIO.acquireReleaseWith (createNetSocket config) release connectThenAct

    /// Connects to the given host and port, runs an action with the open socket, then closes the connection.
    let withConnectionTo (host: string) (port: int) (action: Socket -> FIO<'A, SocketError>) : FIO<'A, SocketError> =
        fio {
            let! config = SocketConfig.create host port
            return! withConnection config action
        }

    /// Connects, sends a single value encoded with the given codec, then closes the connection.
    let sendWith<'A> (codec: SocketCodec<'A>) (value: 'A) (config: SocketConfig) : FIO<unit, SocketError> =
        withConnection config (fun socket -> socket.Send(codec, value))

    /// Connects, receives a single value decoded with the given codec, then closes the connection.
    let receiveWith<'A> (codec: SocketCodec<'A>) (maxBytes: int) (config: SocketConfig) : FIO<'A, SocketError> =
        withConnection config (fun socket -> socket.Receive(codec, maxBytes))
