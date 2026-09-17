namespace FIO.WebSockets

open FIO.DSL

open System
open System.Net
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module WebSocketServer =

    let private logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()

            do! FIO.attempt
                    (fun () -> eprintfn $"WebSocketServer encountered error during {context}: {str}")
                    WsError.fromException

            return ()
        }

    /// Starts an HTTP listener bound to the given URL prefix for accepting WebSocket connections.
    let start (url: string) =
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
    let startDefault (url: string) =
        start url

    /// Stops a listener, gracefully completing in-flight requests, suppressing errors.
    let close (listener: HttpListener) =
        (FIO.attempt
            (fun () -> listener.Stop())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener close")

    /// Aborts a listener immediately, dropping in-flight requests, suppressing errors.
    let abort (listener: HttpListener) =
        (FIO.attempt
            (fun () -> listener.Abort())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener abort")

    let private tryAccept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            let! cancelToken = FIO.cancellationToken ()

            let! listenerCtx =
                FIO.awaitTask
                    (Task.Run<HttpListenerContext>(fun () ->
                        task {
                            use _reg =
                                cancelToken.Register(fun () ->
                                    try
                                        listener.Stop()
                                    with _ ->
                                        ())
                            return! listener.GetContextAsync()
                        }))
                    WsError.connectionFailed

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some protocol -> protocol
                    | None -> null

                let! ctxTask =
                    FIO.attempt
                        (fun () -> listenerCtx.AcceptWebSocketAsync subProto)
                        WsError.connectionFailed

                let! ctx = FIO.awaitTask ctxTask WsError.connectionFailed

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

    /// Accepts the next WebSocket connection, optionally negotiating the given subprotocol.
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            match! tryAccept listener config subProtocol with
            | Some ws -> return ws
            | None -> return! FIO.fail (ConnectionFailed "Not a WebSocket request")
        }

    /// Accepts the next WebSocket connection without negotiating a subprotocol.
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) =
        accept listener config None

    /// Continuously accepts connections, forking the handler for each.
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        let disposeConnection (ws: WebSocket) =
            (FIO.attempt (fun () -> (ws :> IDisposable).Dispose()) WsError.fromException)
                .CatchAll(logAndSuppress "websocket disposal")

        let handleConnection (ws: WebSocket) =
            (handler ws)
                .CatchAll(logAndSuppress "connection handler")
                .Ensuring(ws.CloseIfOpen())
                .Ensuring(disposeConnection ws)

        let step =
            (fio {
                match! tryAccept listener config None with
                | Some ws ->
                    let! _ = (handleConnection ws).Fork()
                    return ()
                | None ->
                    return ()
            })
                .CatchAll(fun error ->
                    fio {
                        do! logAndSuppress "accept loop iteration" error
                        do! FIO.sleep (TimeSpan.FromMilliseconds 25.0)
                    })

        step.Forever()

    /// Starts a listener, accepts connections, and runs the handler for each until interrupted, then stops it.
    let serve (url: string) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        FIO.acquireReleaseWith
            (start url)
            (fun listener -> close listener)
            (fun listener -> acceptLoop listener config handler)

    /// Serves a request/response protocol, decoding each request and encoding each reply with the given codecs.
    let serveWith<'A, 'A1>
        (url: string)
        (config: WebSocketConfig)
        (requestCodec: WebSocketCodec<'A>)
        (responseCodec: WebSocketCodec<'A1>)
        (handler: 'A -> FIO<'A1, WsError>) =
        let wsHandler (ws: WebSocket) =
            fio {
                let! request = ws.Receive requestCodec
                let! response = handler request
                do! ws.Send(responseCodec, response)
            }

        serve url config wsHandler
