namespace FIO.WebSockets

open FIO.DSL

open System
open System.Net
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module WebSocketServer =

    type private Connection =
        {
            Socket: WebSocket
            // An interrupted fiber publishes before its finalizers run, so the shutdown waits on this, which the
            // handler's finalizers complete once the connection is closed.
            Finished: TaskCompletionSource
            mutable Handler: Fiber<unit, WsError> option
        }

    let private logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()

            do! FIO.attempt
                    (fun () -> eprintfn $"WebSocketServer encountered error during {context}: {str}")
                    WsError.fromException

            return ()
        }

    // HttpListener spells "every interface" as `+` and fails to start on 0.0.0.0 or [::].
    let private allInterfaces (url: string) =
        Regex.Replace(url, @"^(\w+://)(0\.0\.0\.0|\[::\])(?=[:/])", "$1+")

    /// Starts an HTTP listener on the given URL prefix; a specific host also filters by Host header, 0.0.0.0 and [::]
    /// mean any host.
    let start (url: string) : FIO<HttpListener, WsError> =
        let url = allInterfaces url
        fio {
            let! listener =
                FIO.attempt
                    (fun () -> new HttpListener())
                    WsError.connectionFailed
            do! FIO.attempt
                    (fun () -> listener.Prefixes.Add url)
                    WsError.connectionFailed
            do! FIO.attempt
                    (fun () -> listener.Start())
                    WsError.connectionFailed
            return listener
        }

    /// Starts an HTTP listener bound to the given URL prefix. Alias for start.
    let startDefault (url: string) : FIO<HttpListener, WsError> =
        start url

    /// Stops a listener, gracefully completing in-flight requests, suppressing errors.
    let close (listener: HttpListener) : FIO<unit, WsError> =
        (FIO.attempt
            (fun () -> listener.Stop())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener close")

    /// Aborts a listener immediately, dropping in-flight requests, suppressing errors.
    let abort (listener: HttpListener) : FIO<unit, WsError> =
        (FIO.attempt
            (fun () -> listener.Abort())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener abort")

    let private dispose (listener: HttpListener) =
        (FIO.attempt
            (fun () -> listener.Close())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener disposal")

    let private upgrade (listenerCtx: HttpListenerContext) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some protocol -> protocol
                    | None -> null

                let handshake =
                    fio {
                        let! ctxTask =
                            FIO.attempt
                                (fun () -> listenerCtx.AcceptWebSocketAsync subProto)
                                WsError.connectionFailed

                        return! FIO.awaitTask ctxTask WsError.connectionFailed
                    }

                // A failed handshake leaves the request unanswered, so the client would wait and the connection stay open.
                let reject =
                    FIO.attempt
                        (fun () ->
                            listenerCtx.Response.StatusCode <- 400
                            listenerCtx.Response.Close())
                        WsError.connectionFailed

                let! ctx =
                    handshake.CatchAll(fun error ->
                        reject.CatchAll(fun _ -> FIO.unit ()).FlatMap(fun () -> FIO.fail error))

                let endPoint (get: HttpListenerRequest -> IPEndPoint) =
                    match get listenerCtx.Request with
                    | null -> None
                    | endPoint -> Some(endPoint :> EndPoint)

                return Some(new WebSocket(ctx.WebSocket, config, endPoint _.RemoteEndPoint, endPoint _.LocalEndPoint))
            else
                do! FIO.attempt
                        (fun () -> listenerCtx.Response.StatusCode <- 400)
                        WsError.connectionFailed
                do! FIO.attempt
                        (fun () -> listenerCtx.Response.Close())
                        WsError.connectionFailed
                return None
        }

    let private tryAccept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) (cancellationToken: CancellationToken) =
        fio {
            let! listenerCtx =
                FIO.awaitTask
                    (Task.Run<HttpListenerContext>(fun () ->
                        task {
                            use _reg =
                                cancellationToken.Register(fun () ->
                                    try
                                        listener.Stop()
                                    with _ ->
                                        ())
                            return! listener.GetContextAsync()
                        }))
                    WsError.connectionFailed

            return! upgrade listenerCtx config subProtocol
        }

    /// Accepts the next WebSocket connection, optionally negotiating the given subprotocol; interrupting it stops the
    /// listener and with it the connections accepted earlier.
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) : FIO<WebSocket, WsError> =
        fio {
            let! cancellationToken = FIO.cancellationToken ()

            match! tryAccept listener config subProtocol cancellationToken with
            | Some ws -> return ws
            | None -> return! FIO.fail (ConnectionFailed "Not a WebSocket request")
        }

    /// Accepts the next WebSocket connection without negotiating a subprotocol.
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) : FIO<WebSocket, WsError> =
        accept listener config None

    /// Accepts connections continuously, forking the handler for each. Interrupting it shuts down: open connections get
    /// a going-away close and the shutdown timeout to finish, the rest are interrupted, and the listener stops.
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) : FIO<unit, WsError> =
        FIO.suspend <| fun () ->
            let connections = ConcurrentDictionary<WebSocket, Connection>(HashIdentity.Reference)
            // The request being awaited outlives an interruption, so the shutdown refuses it instead of losing it.
            let pending = ref null

            let nextRequest =
                (FIO.attempt
                    (fun () ->
                        if isNull pending.Value then
                            pending.Value <- listener.GetContextAsync()

                        pending.Value)
                    WsError.connectionFailed)
                    .FlatMap(fun request ->
                        (FIO.awaitTask request WsError.connectionFailed).CatchAll(fun error ->
                            pending.Value <- null
                            FIO.fail error))

            let disposeConnection (ws: WebSocket) =
                (FIO.attempt (fun () -> (ws :> IDisposable).Dispose()) WsError.fromException)
                    .CatchAll(logAndSuppress "websocket disposal")

            let finish (connection: Connection) =
                FIO.succeedWith (fun () ->
                    connections.TryRemove connection.Socket |> ignore
                    connection.Finished.TrySetResult() |> ignore)

            // Suspended: a handler that throws ends its connection, not the loop.
            let handleConnection (connection: Connection) =
                (FIO.suspend (fun () -> handler connection.Socket))
                    .CatchAll(logAndSuppress "connection handler")
                    .Ensuring(connection.Socket.CloseIfOpen())
                    .Ensuring(disposeConnection connection.Socket)
                    .Ensuring(finish connection)

            // Forked in the uninterruptible hand-off, a handler is protected: the loop's interruption leaves it running
            // for the shutdown's close, and the loop's exit interrupts what the shutdown left behind.
            let handOff (request: HttpListenerContext) =
                FIO.uninterruptible (
                    fio {
                        pending.Value <- null

                        match! upgrade request config None with
                        | Some ws ->
                            let connection =
                                {
                                    Socket = ws
                                    Finished = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
                                    Handler = None
                                }

                            connections[ws] <- connection
                            let! fiber = (handleConnection connection).Fork()
                            connection.Handler <- Some fiber
                        | None -> ()
                    })

            let step =
                (nextRequest.FlatMap handOff)
                    .CatchAll(fun error ->
                        fio {
                            do! logAndSuppress "accept loop iteration" error
                            do! FIO.sleep (TimeSpan.FromMilliseconds 25.0)
                        })

            let rec refuse () =
                fio {
                    match! nextRequest.Map(Some).CatchAll(fun _ -> FIO.succeed None) with
                    | Some request ->
                        pending.Value <- null

                        do! (FIO.attempt
                                (fun () ->
                                    request.Response.StatusCode <- 503
                                    request.Response.Close())
                                WsError.fromException)
                                .CatchAll(logAndSuppress "refusing a request during shutdown")

                        return! refuse ()
                    | None -> ()
                }

            let closeGoingAway (connection: Connection) =
                fio {
                    match! connection.Socket.State().CatchAll(fun _ -> FIO.succeed Net.WebSockets.WebSocketState.Closed) with
                    | Net.WebSockets.WebSocketState.Open ->
                        do! connection.Socket
                                .CloseOutput(WebSockets.WebSocketCloseStatus.EndpointUnavailable, "Server is shutting down")
                                .CatchAll(function
                                    | Closed _ -> FIO.unit ()
                                    | error -> logAndSuppress "going-away close" error)
                    | _ -> ()
                }

            let shutdown =
                fio {
                    let! refusal = (refuse ()).Fork()
                    let open' = Seq.toArray connections.Values
                    do! FIO.forEachParDiscard open' closeGoingAway

                    // The hand-off is uninterruptible, so a connection still without a handler fiber will never be
                    // marked finished.
                    let finished =
                        open'
                        |> Array.choose (fun connection -> connection.Handler |> Option.map (fun _ -> connection.Finished.Task))
                    let limit = if config.ShutdownTimeout > 0 then config.ShutdownTimeout else Timeout.Infinite
                    let! _ = FIO.awaitTask (Task.WhenAny(Task.WhenAll finished, Task.Delay limit)) WsError.fromException

                    do! FIO.forEachDiscard open' (fun connection ->
                            match connection.Handler with
                            | Some fiber when not connection.Finished.Task.IsCompleted -> fiber.InterruptNow()
                            | _ -> FIO.unit ())

                    do! FIO.awaitUnitTask (Task.WhenAll finished) WsError.fromException
                    do! close listener
                    do! refusal.Await().Unit()
                }

            // As a finalizer the shutdown is uninterruptible, and what it forks is protected like the handlers.
            step.Forever<unit>().Ensuring(shutdown.CatchAll(logAndSuppress "shutdown"))

    /// Starts a listener and accepts connections until interrupted, then shuts down as acceptLoop does and closes it.
    let serve (url: string) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) : FIO<unit, WsError> =
        FIO.acquireReleaseWith
            (start url)
            (fun listener -> (close listener).FlatMap(fun () -> dispose listener))
            (fun listener -> acceptLoop listener config handler)

    /// Serves a request/response protocol, decoding each request and encoding each reply with the given codecs.
    let serveWith<'A, 'A1>
        (url: string)
        (config: WebSocketConfig)
        (requestCodec: WebSocketCodec<'A>)
        (responseCodec: WebSocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, WsError>) : FIO<unit, WsError> =
        let wsHandler (ws: WebSocket) =
            fio {
                let! request = ws.Receive requestCodec
                let! response = handler request
                do! ws.Send(responseCodec, response)
            }

        serve url config wsHandler
