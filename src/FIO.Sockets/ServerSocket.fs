namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Threading

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

    let private acceptWith (serverSocket: ServerSocket) (cancellationToken: CancellationToken) =
        fio {
            let! netSocket =
                FIO.awaitTask (serverSocket.NetSocket.AcceptAsync(cancellationToken).AsTask()) AcceptFailed

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

    /// Accepts the next incoming connection, returning a socket for the accepted client.
    let accept (serverSocket: ServerSocket) =
        FIO.cancellationToken().FlatMap(acceptWith serverSocket)

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

            // The accept is awaited uninterruptibly and cancelled through the fiber's token instead, so a
            // connection it accepts always reaches a handler whose finalizer closes it. Only the fork is restored
            // to the loop's interruptibility, which keeps the handler an ordinary child, interrupted with the
            // loop; an interruption that lands before the fork leaves the socket and its slot to the finalizer.
            let acceptAndHandOff (cancellationToken: CancellationToken) =
                FIO.uninterruptibleMask <| fun restore ->
                    fio {
                        let! socket = acceptWith serverSocket cancellationToken
                        let closeSocket = socket.Close().CatchAll(logAndSuppress "accepted socket close")
                        let handedOff = ref false

                        // The handler is called in its own fiber: one that throws ends its connection, not the loop.
                        let handlerWithCleanup =
                            (FIO.suspend (fun () -> handler socket))
                                .Ensuring(closeSocket)
                                .Ensuring(slots.Write())

                        do! restore
                                .Restore(handlerWithCleanup.Fork().Map(fun _ -> handedOff.Value <- true))
                                .Ensuring(FIO.suspend (fun () ->
                                    if handedOff.Value then FIO.unit ()
                                    else closeSocket.FlatMap(fun () -> slots.Write())))
                    }

            let step =
                (fio {
                    do! slots.Read()
                    let! cancellationToken = FIO.cancellationToken ()
                    do! acceptAndHandOff cancellationToken
                }).CatchAll(fun error ->
                    fio {
                        do! logAndSuppress "accept loop iteration" error
                        do! slots.Write()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 25.0)
                    })

            return! step.Forever<unit>()
        }

    /// Continuously accepts connections, running the handler for each using the default concurrency limit.
    let acceptLoop (handler: Socket -> FIO<unit, SocketError>) (serverSocket: ServerSocket) =
        acceptLoopWith DefaultMaxConcurrentHandlers handler serverSocket

    /// The configuration the given server socket was created with.
    let getConfig (serverSocket: ServerSocket) =
        serverSocket.Config

    /// Returns an effect that yields the local endpoint the given server socket is bound to.
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
