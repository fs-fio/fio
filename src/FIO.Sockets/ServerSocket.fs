namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Threading
open System.Threading.Tasks

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
    let bind (config: ServerSocketConfig) : FIO<ServerSocket, SocketError> =
        fio {
            let! netSocket =
                FIO.attempt
                    (fun () -> new Sockets.Socket(config.AddressFamily, config.SocketType, config.ProtocolType))
                    SocketError.fromException

            let listen =
                fio {
                    let! endpoint =
                        FIO.attempt
                            (fun () -> IPEndPoint(resolveBindAddress config, config.BindPort) :> EndPoint)
                            (fun ex -> BindFailed(config.BindAddress, config.BindPort, ex))

                    do! FIO.attempt
                            (fun () ->
                                netSocket.Bind endpoint
                                netSocket.Listen config.Backlog)
                            (fun ex -> BindFailed(config.BindAddress, config.BindPort, ex))
                }

            do! listen.TapError(fun _ ->
                    FIO.succeedWith (fun () ->
                        try netSocket.Dispose()
                        with _ -> ()))

            return { NetSocket = netSocket; Config = config }
        }

    /// Closes a server socket, suppressing errors.
    let close (serverSocket: ServerSocket) : FIO<unit, SocketError> =
        (FIO.attempt
            (fun () ->
                serverSocket.NetSocket.Close()
                serverSocket.NetSocket.Dispose())
            SocketError.fromException
        ).CatchAll(logAndSuppress "server socket close")

    /// Binds a server socket for use as a resource acquisition. Alias for bind.
    let acquire (config: ServerSocketConfig) : FIO<ServerSocket, SocketError> =
        bind config

    /// Closes a server socket for use as a resource release. Alias for close.
    let release (serverSocket: ServerSocket) : FIO<unit, SocketError> =
        close serverSocket

    /// Binds a server socket, runs an action with it, then closes it.
    let withServerSocket (config: ServerSocketConfig) (action: ServerSocket -> FIO<'A, SocketError>) : FIO<'A, SocketError> =
        FIO.acquireReleaseWith (acquire config) release action

    // AcceptAsync throws at once on a closed server socket; as a faulted task that still fails as AcceptFailed.
    let private acceptAsync (listener: Sockets.Socket) (cancellationToken: CancellationToken) =
        try
            listener.AcceptAsync(cancellationToken).AsTask()
        with ex ->
            Task.FromException<Sockets.Socket> ex

    let private isClosed (serverSocket: ServerSocket) =
        try
            serverSocket.NetSocket.SafeHandle.IsClosed
        with _ ->
            true

    // A failure of the effect is the listener's; an Error is one accepted connection whose options could not be set,
    // already closed, which says nothing about the listener.
    let private acceptWith (serverSocket: ServerSocket) (cancellationToken: CancellationToken) =
        (FIO.awaitTask (acceptAsync serverSocket.NetSocket cancellationToken) AcceptFailed).FlatMap(fun netSocket ->
            FIO.succeedWith (fun () ->
                try
                    let config =
                        match serverSocket.Config.AcceptedSocketConfig with
                        | Some cfg ->
                            netSocket.NoDelay <- cfg.NoDelay
                            netSocket.SendBufferSize <- cfg.SendBufferSize
                            netSocket.ReceiveBufferSize <- cfg.ReceiveBufferSize
                            cfg
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

                    Ok(new Socket(netSocket, config))
                with ex ->
                    try netSocket.Dispose()
                    with _ -> ()

                    Error(AcceptFailed ex)))

    /// Accepts the next incoming connection, returning a socket for the accepted client; a connection accepted just as
    /// this fiber is interrupted is closed, barring a narrow race (serve and acceptLoop always close theirs).
    let accept (serverSocket: ServerSocket) : FIO<Socket, SocketError> =
        FIO.cancellationToken().FlatMap(fun cancellationToken ->
            FIO.uninterruptible (
                (acceptWith serverSocket cancellationToken).FlatMap(fun accepted ->
                    match accepted with
                    | Error error -> FIO.fail error
                    | Ok socket when cancellationToken.IsCancellationRequested -> socket.Close().Map(fun () -> socket)
                    | Ok socket -> FIO.succeed socket)))

    /// The default maximum number of concurrently running connection handlers.
    [<Literal>]
    let DefaultMaxConcurrentHandlers : int = 1024

    // A handler runs on its own fiber and nobody joins it, so what ended it is reported here.
    let private reportHandlerOutcome (handler: Fiber<unit, SocketError>) =
        handler.Task().ContinueWith(
            (fun (ended: Task<FiberResult<unit, SocketError>>) ->
                try
                    if ended.IsCompletedSuccessfully then
                        match ended.Result with
                        | Failed error -> eprintfn $"SocketServer connection handler failed: {error}"
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect thrown -> eprintfn $"SocketServer connection handler crashed: {thrown}"
                            | _ -> ()
                        | Succeeded () -> ()
                with _ ->
                    ()),
            TaskContinuationOptions.ExecuteSynchronously)
        |> ignore

    /// Continuously accepts connections, running the handler for each with bounded concurrency.
    let acceptLoopWith
        (maxConcurrentHandlers: int)
        (handler: Socket -> FIO<unit, SocketError>)
        (serverSocket: ServerSocket) : FIO<unit, SocketError> =
        fio {
            let slots = Channel<unit>()
            let failures = ref 0
            let listenerFailures = ref 0

            do! FIO.forEachDiscard [ 1 .. max 1 maxConcurrentHandlers ] (fun _ -> slots.Write())

            // A streak of failures is reported by its first and then every fortieth, so a listener that keeps failing
            // does not flood the log.
            let report (error: SocketError) =
                FIO.suspend <| fun () ->
                    failures.Value <- failures.Value + 1

                    if failures.Value = 1 || failures.Value % 40 = 0 then
                        logAndSuppress $"accept loop iteration (failure {failures.Value} in a row)" error
                    else
                        FIO.unit ()

            // The accept is awaited uninterruptibly, cancelled through the fiber's token, so an accepted socket always
            // reaches its closing finalizer; only the fork is restored, which keeps the handler an ordinary child.
            let acceptAndHandOff (cancellationToken: CancellationToken) =
                FIO.uninterruptibleMask <| fun restore ->
                    (acceptWith serverSocket cancellationToken).FlatMap(fun accepted ->
                        match accepted with
                        | Error error -> FIO.succeed (Some error)
                        | Ok socket ->
                            let closeSocket = socket.Close().CatchAll(logAndSuppress "accepted socket close")
                            let handedOff = ref false

                            // Suspended: a handler that throws ends its connection, not the loop.
                            let handlerWithCleanup =
                                (FIO.suspend (fun () -> handler socket))
                                    .Ensuring(closeSocket)
                                    .Ensuring(slots.Write())

                            let handOff =
                                handlerWithCleanup.Fork().Map(fun fiber ->
                                    handedOff.Value <- true
                                    reportHandlerOutcome fiber)

                            restore
                                .Restore(handOff)
                                .Ensuring(FIO.suspend (fun () ->
                                    if handedOff.Value then FIO.unit ()
                                    else closeSocket.FlatMap(fun () -> slots.Write())))
                                .Map(fun () -> None))

            // One connection that could not be set up is dropped and the loop goes on at once; a failing listener is
            // retried after a pause that grows from 25 ms to 1 s.
            let step =
                (fio {
                    do! slots.Read()
                    let! cancellationToken = FIO.cancellationToken ()

                    let! dropped = acceptAndHandOff cancellationToken
                    listenerFailures.Value <- 0

                    match dropped with
                    | None -> failures.Value <- 0
                    | Some error ->
                        do! report error
                        do! slots.Write()
                }).CatchAll(fun error ->
                    if isClosed serverSocket then
                        FIO.fail error
                    else
                        fio {
                            do! report error
                            do! slots.Write()
                            listenerFailures.Value <- listenerFailures.Value + 1
                            do! FIO.sleep (TimeSpan.FromMilliseconds(float (min 1000 (25 <<< min 6 (listenerFailures.Value - 1)))))
                        })

            return! step.Forever<unit>()
        }

    /// Continuously accepts connections, running the handler for each using the default concurrency limit.
    let acceptLoop (handler: Socket -> FIO<unit, SocketError>) (serverSocket: ServerSocket) : FIO<unit, SocketError> =
        acceptLoopWith DefaultMaxConcurrentHandlers handler serverSocket

    /// The configuration the given server socket was created with.
    let getConfig (serverSocket: ServerSocket) : ServerSocketConfig =
        serverSocket.Config

    /// Returns an effect that yields the local endpoint the given server socket is bound to.
    let getLocalEndPoint (serverSocket: ServerSocket) : FIO<EndPoint, SocketError> =
        FIO.attempt (fun () -> serverSocket.NetSocket.LocalEndPoint) SocketError.fromException

    /// Binds, accepts connections, and runs the handler for each until interrupted, then closes the server.
    let serve (config: ServerSocketConfig) (handler: Socket -> FIO<unit, SocketError>) : FIO<unit, SocketError> =
        withServerSocket config (fun serverSocket -> acceptLoop handler serverSocket)

    /// Serves a request/response protocol, one request and one reply per connection, reading the request with at most the given buffer size.
    let serveWithBufferSize<'A, 'A1>
        (requestCodec: SocketCodec<'A>)
        (responseCodec: SocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, SocketError>)
        (config: ServerSocketConfig)
        (bufferSize: int) : FIO<unit, SocketError> =
        let connectionHandler (socket: Socket) =
            fio {
                let! request = socket.Receive(requestCodec, bufferSize)
                let! response = handler request
                do! socket.Send(responseCodec, response)
            }
        serve config connectionHandler

    /// Serves a request/response protocol, one request and one reply per connection, reading the request with at most 8192 bytes.
    let serveWith<'A, 'A1>
        (requestCodec: SocketCodec<'A>)
        (responseCodec: SocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, SocketError>)
        (config: ServerSocketConfig) : FIO<unit, SocketError> =
        serveWithBufferSize requestCodec responseCodec handler config 8192
