namespace FIO.Sockets

open FIO.DSL

open System
open System.Net

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ServerSocket =

    let private logAndSuppress (context: string) (error: SocketError) =
        fio {
            let str = error.ToString()
            do! FIO.attempt
                    (fun () -> eprintfn $"SocketServer encountered error during {context}: {str}")
                    SocketError.fromException
            return ()
        }

    let private resolveBindAddress (config: ServerSocketConfig) =
        match IPAddress.TryParse config.BindAddress with
        | true, address -> address
        | _ ->
            let addresses = Dns.GetHostAddresses config.BindAddress
            match addresses |> Array.tryFind (fun address -> address.AddressFamily = config.AddressFamily) with
            | Some address -> address
            | None ->
                match Array.tryHead addresses with
                | Some address -> address
                | None -> raise (ArgumentException $"Could not resolve bind address '{config.BindAddress}'")

    /// Binds and starts listening on the given configuration, returning an open server socket.
    let bind (config: ServerSocketConfig) =
        fio {
            let! netSocket =
                FIO.attempt
                    (fun () -> new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType))
                    SocketError.fromException

            let! endpoint =
                FIO.attempt
                    (fun () -> IPEndPoint(resolveBindAddress config, config.BindPort) :> EndPoint)
                    (fun ex -> BindFailed(config.BindAddress, config.BindPort, ex))

            do! FIO.attempt
                    (fun () ->
                        netSocket.Bind endpoint
                        netSocket.Listen config.Backlog)
                    (fun ex -> BindFailed(config.BindAddress, config.BindPort, ex))

            return { NetSocket = netSocket; Config = config }
        }

    /// Closes a server socket, suppressing errors.
    let close (serverSocket: ServerSocket) =
        (FIO.attempt
            (fun () ->
                serverSocket.NetSocket.Close()
                serverSocket.NetSocket.Dispose())
            SocketError.fromException
        ).CatchAll(logAndSuppress "server socket close")

    /// Binds a server socket for use as a resource acquisition. Alias for bind.
    let acquire (config: ServerSocketConfig) =
        bind config

    /// Closes a server socket for use as a resource release. Alias for close.
    let release (serverSocket: ServerSocket) =
        close serverSocket

    /// Binds a server socket, runs an action with it, then closes it.
    let withServerSocket (config: ServerSocketConfig) (action: ServerSocket -> FIO<'A, SocketError>) =
        FIO.acquireReleaseWith (acquire config) release action

    /// Accepts the next incoming connection, returning a socket for the accepted client.
    let accept (serverSocket: ServerSocket) =
        fio {
            let! cancelToken = FIO.cancellationToken ()

            let! netSocket =
                FIO.awaitTask (serverSocket.NetSocket.AcceptAsync(cancelToken).AsTask()) AcceptFailed

            let config =
                match serverSocket.Config.AcceptedSocketConfig with
                | Some cfg -> cfg
                | None ->
                    let linger = netSocket.LingerState
                    {
                        Host = ""
                        Port = 0
                        AddressFamily = netSocket.AddressFamily
                        SocketType = netSocket.SocketType
                        ProtocolType = netSocket.ProtocolType
                        SendBufferSize = netSocket.SendBufferSize
                        ReceiveBufferSize = netSocket.ReceiveBufferSize
                        SendTimeout = netSocket.SendTimeout
                        ReceiveTimeout = netSocket.ReceiveTimeout
                        NoDelay = netSocket.NoDelay
                        LingerEnabled = not (isNull linger) && linger.Enabled
                        LingerTimeout = if isNull linger then 0 else linger.LingerTime
                    }

            return new Socket(netSocket, config)
        }

    /// The default maximum number of concurrently running connection handlers.
    [<Literal>]
    let DefaultMaxConcurrentHandlers = 1024

    /// Continuously accepts connections, running the handler for each with bounded concurrency.
    let acceptLoopWith
        (maxConcurrentHandlers: int)
        (handler: Socket -> FIO<unit, SocketError>)
        (serverSocket: ServerSocket) =
        fio {
            let slots = Channel<unit>()

            do! FIO.forEachDiscard [ 1 .. max 1 maxConcurrentHandlers ] (fun _ -> slots.Write())

            let step =
                (fio {
                    do! slots.Read()
                    let! socket = accept serverSocket

                    let handlerWithCleanup =
                        FIO.acquireReleaseWith
                            (FIO.succeed socket)
                            (fun socket -> socket.Close().CatchAll(logAndSuppress "accepted socket close"))
                            handler

                    let! _fiber = handlerWithCleanup.Ensuring(slots.Write()).Fork()
                    return ()
                }).CatchAll(fun error ->
                    fio {
                        do! logAndSuppress "accept loop iteration" error
                        do! slots.Write()
                        do! FIO.sleep (System.TimeSpan.FromMilliseconds 25.0) SocketError.fromException
                    })

            return! step.Forever()
        }

    /// Continuously accepts connections, running the handler for each using the default concurrency limit.
    let acceptLoop (handler: Socket -> FIO<unit, SocketError>) (serverSocket: ServerSocket) =
        acceptLoopWith DefaultMaxConcurrentHandlers handler serverSocket

    /// Gets the configuration a server socket was created with.
    let getConfig (serverSocket: ServerSocket) =
        serverSocket.Config

    /// Gets the local endpoint a server socket is bound to.
    let getLocalEndPoint (serverSocket: ServerSocket) =
        FIO.attempt (fun () -> serverSocket.NetSocket.LocalEndPoint) SocketError.fromException

    /// Binds, accepts connections, and runs the handler for each until interrupted, then closes the server.
    let serve (config: ServerSocketConfig) (handler: Socket -> FIO<unit, SocketError>) =
        withServerSocket config (fun serverSocket -> acceptLoop handler serverSocket)

    /// Serves a request/response protocol, decoding each request and encoding each reply with the given buffer size.
    let serveWithBufferSize<'A, 'A1>
        (requestCodec: SocketCodec<'A>)
        (responseCodec: SocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, SocketError>)
        (config: ServerSocketConfig)
        (bufferSize: int) =
        let connectionHandler (socket: Socket) =
            fio {
                let! request = socket.Receive(requestCodec, bufferSize)
                let! response = handler request
                do! socket.Send(responseCodec, response)
            }
        serve config connectionHandler

    /// Serves a request/response protocol, decoding each request and encoding each reply using a default buffer size.
    let serveWith<'A, 'A1>
        (requestCodec: SocketCodec<'A>)
        (responseCodec: SocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, SocketError>)
        (config: ServerSocketConfig) =
        serveWithBufferSize requestCodec responseCodec handler config 8192
