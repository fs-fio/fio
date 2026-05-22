namespace FIO.Sockets

open FIO.DSL

open System.Net

/// <summary>Creates TCP client connections and scoped connection effects.</summary>
[<RequireQualifiedAccess>]
module SocketClient =

    /// <summary>Creates an effect that establishes a TCP connection to a remote host.</summary>
    /// <param name="config">The socket configuration specifying host, port, and connection options.</param>
    /// <returns>An effect that produces a connected Socket, or fails with ConnectionFailed if the connection cannot be established.</returns>
    let connect (config: SocketConfig) =
        fio {
            let! netSocket =
                FIO.attempt (
                    (fun () ->
                        let s =
                            new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType)

                        s.SendBufferSize <- config.SendBufferSize
                        s.ReceiveBufferSize <- config.ReceiveBufferSize
                        s.SendTimeout <- config.SendTimeout
                        s.ReceiveTimeout <- config.ReceiveTimeout
                        s.NoDelay <- config.NoDelay
                        s),
                    SocketError.fromException
                )

            let! ct = FIO.cancellationToken ()

            do!
                FIO.awaitUnitTask (
                    netSocket.ConnectAsync(config.Host, config.Port, ct).AsTask(),
                    fun ex -> ConnectionFailed(config.Host, config.Port, ex)
                )

            return new Socket(netSocket, config)
        }

    /// <summary>Creates an effect that establishes a TCP connection using the given host and port with default settings.</summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <returns>An effect that produces a connected Socket.</returns>
    let connectWith (host: string) (port: int) =
        fio {
            let! config = SocketConfig.create (host, port)
            return! connect config
        }

    /// <summary>Builds a resource-scoped effect that connects, runs an action, and closes the connection on every outcome.</summary>
    /// <param name="config">The socket configuration specifying host, port, and connection options.</param>
    /// <param name="action">A function from the connected socket to the effect to run; the connection is closed after this effect completes.</param>
    /// <returns>An effect that produces the action's result and guarantees connection cleanup.</returns>
    let withConnection (config: SocketConfig) (action: Socket -> FIO<'R, SocketError>) =
        FIO.acquireRelease (connect config, (fun socket -> socket.Close().CatchAll(fun _ -> FIO.unit ())), action)

    /// <summary>Builds a resource-scoped effect that connects to the given host and port, runs an action, and closes the connection on every outcome.</summary>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="action">A function from the connected socket to the effect to run; the connection is closed after this effect completes.</param>
    /// <returns>An effect that produces the action's result and guarantees connection cleanup.</returns>
    let withConnectionTo (host: string) (port: int) (action: Socket -> FIO<'R, SocketError>) =
        fio {
            let! config = SocketConfig.create (host, port)
            return! withConnection config action
        }

    /// <summary>Creates an effect that connects, sends a codec-encoded value, and closes the connection.</summary>
    /// <param name="codec">The codec to use for encoding the value to bytes.</param>
    /// <param name="value">The value to encode and send.</param>
    /// <param name="config">The socket configuration specifying host, port, and connection options.</param>
    /// <returns>An effect that completes when the value has been sent and the connection closed.</returns>
    let sendWith<'T> (codec: SocketCodec<'T>) (value: 'T) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Send(codec, value))

    /// <summary>Creates an effect that connects, receives and decodes a value, and closes the connection.</summary>
    /// <param name="codec">The codec to use for decoding bytes to a value.</param>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <param name="config">The socket configuration specifying host, port, and connection options.</param>
    /// <returns>An effect that produces the decoded value and guarantees connection cleanup.</returns>
    let receiveWith<'T> (codec: SocketCodec<'T>) (maxBytes: int) (config: SocketConfig) =
        withConnection config (fun socket -> socket.Receive(codec, maxBytes))
