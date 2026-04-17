namespace FIO.Sockets

open FIO.DSL

open System.Net

/// Low-level server socket operations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ServerSocket =

    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// <param name="context">Description of the operation that failed.</param>
    /// <param name="err">The error to log.</param>
    /// <returns>Effect that logs the error and succeeds.</returns>
    let private logAndSuppress (context: string) (err: SocketError) =
        fio {
            let str = err.ToString()

            do!
                FIO.attempt (
                    (fun () -> eprintfn $"[SocketServer] Error during {context}: {str}"),
                    SocketError.fromException
                )

            return ()
        }

    /// Binds to a local address and starts listening for connections.
    /// <param name="config">Server socket configuration.</param>
    /// <returns>The bound server socket.</returns>
    let bind (config: ServerSocketConfig) =
        fio {
            let! netSocket =
                FIO.attempt (
                    (fun () -> new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType)),
                    SocketError.fromException
                )

            let! endpoint =
                FIO.attempt (
                    (fun () -> IPEndPoint(IPAddress.Parse config.BindAddress, config.BindPort) :> EndPoint),
                    SocketError.fromException
                )

            do!
                FIO.attempt (
                    (fun () ->
                        netSocket.Bind endpoint
                        netSocket.Listen config.Backlog),
                    fun exn -> BindFailed(config.BindAddress, config.BindPort, exn)
                )

            return { NetSocket = netSocket; Config = config }
        }

    /// Closes the server socket.
    /// <param name="serverSocket">The server socket to close.</param>
    /// <returns>Effect that closes the server socket.</returns>
    let close (serverSocket: ServerSocket) =
        FIO
            .attempt(
                (fun () ->
                    serverSocket.NetSocket.Close()
                    serverSocket.NetSocket.Dispose()),
                SocketError.fromException
            )
            .CatchAll(logAndSuppress "server socket close")

    /// Acquires a server socket.
    /// <param name="config">Server socket configuration.</param>
    /// <returns>The acquired server socket.</returns>
    let acquire (config: ServerSocketConfig) = bind config

    /// Releases a server socket.
    /// Suppresses errors during cleanup to avoid masking original failures.
    /// <param name="serverSocket">The server socket to release.</param>
    /// <returns>Effect that releases the server socket.</returns>
    let release (serverSocket: ServerSocket) = close serverSocket

    /// Executes an action with a server socket, automatically closing it.
    /// <param name="config">Server socket configuration.</param>
    /// <param name="action">Action to execute with the server socket.</param>
    /// <returns>The result of the action.</returns>
    let withServerSocket (config: ServerSocketConfig, action: ServerSocket -> FIO<'R, SocketError>) =
        FIO.acquireRelease (acquire config, release, action)

    /// Accepts a single incoming connection.
    /// <param name="serverSocket">The server socket to accept from.</param>
    /// <returns>The connected socket.</returns>
    let accept (serverSocket: ServerSocket) =
        fio {
            let! netSocket =
                FIO.awaitGenericTask (serverSocket.NetSocket.AcceptAsync(), AcceptFailed)

            let config =
                match serverSocket.Config.AcceptedSocketConfig with
                | Some cfg -> cfg
                | None ->
                    {
                        Host = "" // Not applicable for accepted socket
                        Port = 0 // Not applicable
                        AddressFamily = netSocket.AddressFamily
                        SocketType = netSocket.SocketType
                        ProtocolType = netSocket.ProtocolType
                        SendBufferSize = netSocket.SendBufferSize
                        ReceiveBufferSize = netSocket.ReceiveBufferSize
                        SendTimeout = netSocket.SendTimeout
                        ReceiveTimeout = netSocket.ReceiveTimeout
                        NoDelay = netSocket.NoDelay
                        LingerEnabled = netSocket.LingerState.Enabled
                        LingerTimeout = netSocket.LingerState.LingerTime
                    }

            return new Socket(netSocket, config)
        }

    /// Accept loop: accepts connections and processes them with the given handler.
    /// Each connection is handled concurrently via Fork.
    /// Runs until interrupted.
    /// <param name="handler">Handler to process each accepted connection.</param>
    /// <param name="serverSocket">The server socket to accept from.</param>
    /// <returns>Effect that accepts connections until interrupted.</returns>
    let acceptLoop (handler: Socket -> FIO<unit, SocketError>, serverSocket: ServerSocket) : FIO<unit, SocketError> =
        let rec loop () =
            fio {
                let! socket = accept serverSocket

                let handlerWithCleanup =
                    FIO.acquireRelease (
                        FIO.succeed socket,
                        (fun s -> s.Close().CatchAll(logAndSuppress "accepted socket close")),
                        handler
                    )

                let! _fiber = handlerWithCleanup.Fork()
                return! loop ()
            }

        loop ()

    /// Gets the server socket configuration.
    /// <param name="serverSocket">The server socket to query.</param>
    /// <returns>The server socket configuration.</returns>
    let getConfig (serverSocket: ServerSocket) = serverSocket.Config

    /// Gets the local endpoint the server socket is bound to.
    /// <param name="serverSocket">The server socket to query.</param>
    /// <returns>The local endpoint.</returns>
    let getLocalEndPoint (serverSocket: ServerSocket) =
        FIO.attempt ((fun () -> serverSocket.NetSocket.LocalEndPoint), SocketError.fromException)

    /// Starts a server and processes connections with the given handler.
    /// Runs until interrupted.
    /// <param name="config">Server socket configuration.</param>
    /// <param name="handler">Handler to process each accepted connection.</param>
    /// <returns>Effect that runs the server until interrupted.</returns>
    let serve (config: ServerSocketConfig, handler: Socket -> FIO<unit, SocketError>) =
        withServerSocket (config, fun serverSocket -> acceptLoop (handler, serverSocket))

    /// Starts a server with a codec-based request/response handler with configurable buffer size.
    /// Receives a request using requestCodec, processes it, and sends response using responseCodec.
    /// <param name="requestCodec">Codec for decoding requests.</param>
    /// <param name="responseCodec">Codec for encoding responses.</param>
    /// <param name="handler">Handler that processes requests and returns responses.</param>
    /// <param name="config">Server socket configuration.</param>
    /// <param name="bufferSize">Buffer size for receiving requests.</param>
    /// <returns>Effect that runs the server until interrupted.</returns>
    let serveWithBufferSize<'Req, 'Resp>
        (
            requestCodec: SocketCodec<'Req>,
            responseCodec: SocketCodec<'Resp>,
            handler: 'Req -> FIO<'Resp, SocketError>,
            config: ServerSocketConfig,
            bufferSize: int
        ) =
        let connectionHandler (socket: Socket) =
            fio {
                let! request = socket.Receive(requestCodec, bufferSize)
                let! response = handler request
                do! socket.Send(responseCodec, response)
            }

        serve (config, connectionHandler)

    /// Starts a server with a codec-based request/response handler.
    /// Receives a request using requestCodec, processes it, and sends response using responseCodec.
    /// Uses a buffer size of 8192 bytes for receiving requests.
    /// <param name="requestCodec">Codec for decoding requests.</param>
    /// <param name="responseCodec">Codec for encoding responses.</param>
    /// <param name="handler">Handler that processes requests and returns responses.</param>
    /// <param name="config">Server socket configuration.</param>
    /// <returns>Effect that runs the server until interrupted.</returns>
    let serveWith<'Req, 'Resp>
        (
            requestCodec: SocketCodec<'Req>,
            responseCodec: SocketCodec<'Resp>,
            handler: 'Req -> FIO<'Resp, SocketError>,
            config: ServerSocketConfig
        ) =
        serveWithBufferSize (requestCodec, responseCodec, handler, config, 8192)
