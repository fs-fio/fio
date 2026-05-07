namespace FIO.Sockets

open FIO.DSL

open System.Net

/// <summary>Creates and operates TCP server socket listeners.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ServerSocket =

    /// <summary>Transforms a socket error into a logged-and-suppressed unit effect for use in server cleanup paths.</summary>
    /// <param name="context">A description of the operation being cleaned up.</param>
    /// <param name="err">The socket error to log.</param>
    /// <returns>An effect that logs the error to standard error and succeeds with unit.</returns>
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

    /// <summary>Creates an effect that binds to a local address and starts listening for TCP connections.</summary>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <returns>An effect that produces a bound and listening ServerSocket, or fails with BindFailed if binding fails.</returns>
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

    /// <summary>Creates an effect that closes a server socket and releases its resources.</summary>
    /// <param name="serverSocket">The server socket to close.</param>
    /// <returns>An effect that shuts down the listener, suppressing any cleanup errors.</returns>
    let close (serverSocket: ServerSocket) =
        FIO
            .attempt(
                (fun () ->
                    serverSocket.NetSocket.Close()
                    serverSocket.NetSocket.Dispose()),
                SocketError.fromException
            )
            .CatchAll(logAndSuppress "server socket close")

    /// <summary>Creates an effect that acquires a bound and listening server socket.</summary>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <returns>An effect that produces a bound ServerSocket ready to accept connections.</returns>
    let acquire (config: ServerSocketConfig) = bind config

    /// <summary>Creates an effect that releases a server socket by closing it.</summary>
    /// <param name="serverSocket">The server socket to release.</param>
    /// <returns>An effect that closes the listener, suppressing cleanup errors to avoid masking original failures.</returns>
    let release (serverSocket: ServerSocket) = close serverSocket

    /// <summary>Builds a resource-scoped effect that binds a server socket, runs an action, and closes the listener on every outcome.</summary>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <param name="action">A function from the bound server socket to the effect to run; the listener is closed after this effect completes.</param>
    /// <returns>An effect that produces the action's result and guarantees listener cleanup.</returns>
    let withServerSocket (config: ServerSocketConfig, action: ServerSocket -> FIO<'R, SocketError>) =
        FIO.acquireRelease (acquire config, release, action)

    /// <summary>Creates an effect that accepts a single incoming TCP connection.</summary>
    /// <param name="serverSocket">The server socket to accept a connection from.</param>
    /// <returns>An effect that produces a connected Socket for the accepted client, or fails with AcceptFailed.</returns>
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

    /// <summary>Creates an effect that continuously accepts connections and forks a handler for each one until interrupted.</summary>
    /// <param name="handler">A function from a connected socket to the effect to run for each accepted connection; each invocation runs concurrently.</param>
    /// <param name="serverSocket">The server socket to accept connections from.</param>
    /// <returns>An effect that loops accepting and forking handlers, completing only when interrupted.</returns>
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

    /// <summary>Returns the configuration of a server socket.</summary>
    /// <param name="serverSocket">The server socket to query.</param>
    /// <returns>The ServerSocketConfig associated with the listener.</returns>
    let getConfig (serverSocket: ServerSocket) = serverSocket.Config

    /// <summary>Returns the local endpoint a server socket is bound to.</summary>
    /// <param name="serverSocket">The server socket to query.</param>
    /// <returns>An effect that produces the local endpoint address.</returns>
    let getLocalEndPoint (serverSocket: ServerSocket) =
        FIO.attempt ((fun () -> serverSocket.NetSocket.LocalEndPoint), SocketError.fromException)

    /// <summary>Builds a server effect that binds, accepts connections, and forks a handler for each one until interrupted.</summary>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <param name="handler">A function from a connected socket to the effect to run for each accepted connection.</param>
    /// <returns>An effect that runs the server until interrupted, closing the listener on every outcome.</returns>
    let serve (config: ServerSocketConfig, handler: Socket -> FIO<unit, SocketError>) =
        withServerSocket (config, fun serverSocket -> acceptLoop (handler, serverSocket))

    /// <summary>Builds a server effect that decodes requests, processes them, and sends encoded responses with a configurable receive buffer size.</summary>
    /// <param name="requestCodec">The codec for decoding incoming requests from bytes.</param>
    /// <param name="responseCodec">The codec for encoding outgoing responses to bytes.</param>
    /// <param name="handler">A function from a decoded request to an effect that produces the response.</param>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <param name="bufferSize">The maximum number of bytes to receive per request.</param>
    /// <returns>An effect that runs the server until interrupted, closing the listener on every outcome.</returns>
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

    /// <summary>Builds a server effect that decodes requests, processes them, and sends encoded responses using a default 8192-byte receive buffer.</summary>
    /// <param name="requestCodec">The codec for decoding incoming requests from bytes.</param>
    /// <param name="responseCodec">The codec for encoding outgoing responses to bytes.</param>
    /// <param name="handler">A function from a decoded request to an effect that produces the response.</param>
    /// <param name="config">The server socket configuration specifying bind address, port, and backlog.</param>
    /// <returns>An effect that runs the server until interrupted, closing the listener on every outcome.</returns>
    let serveWith<'Req, 'Resp>
        (
            requestCodec: SocketCodec<'Req>,
            responseCodec: SocketCodec<'Resp>,
            handler: 'Req -> FIO<'Resp, SocketError>,
            config: ServerSocketConfig
        ) =
        serveWithBufferSize (requestCodec, responseCodec, handler, config, 8192)
