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
                    WsError.fromException
            do! FIO.attempt
                    (fun () -> listener.Prefixes.Add url)
                    WsError.fromException
            do! FIO.attempt
                    (fun () -> listener.Start())
                    WsError.fromException
            return listener
        }

    /// Starts an HTTP listener bound to the given URL prefix. Alias for start.
    let startDefault (url: string) =
        start url

    /// Stops a listener, gracefully completing in-flight requests.
    let close (listener: HttpListener) =
        FIO.attempt
            (fun () -> listener.Stop())
            WsError.fromException

    /// Aborts a listener immediately, dropping in-flight requests.
    let abort (listener: HttpListener) =
        FIO.attempt
            (fun () -> listener.Abort())
            WsError.fromException

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
                    WsError.fromException

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some protocol -> protocol
                    | None -> null

                let! ctxTask =
                    FIO.attempt
                        (fun () -> listenerCtx.AcceptWebSocketAsync subProto)
                        WsError.fromException

                let! ctx = FIO.awaitTask ctxTask WsError.fromException
                return Some(new WebSocket(ctx.WebSocket, config))
            else
                do! FIO.attempt
                        (fun () -> listenerCtx.Response.StatusCode <- 400)
                        WsError.fromException
                do! FIO.attempt
                        (fun () -> listenerCtx.Response.Close())
                        WsError.fromException
                return None
        }

    /// Accepts the next WebSocket connection, optionally negotiating the given subprotocol.
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            match! tryAccept listener config subProtocol with
            | Some ws -> return ws
            | None -> return! FIO.fail (WsError.fromException <| Exception "Not a WebSocket request")
        }

    /// Accepts the next WebSocket connection without negotiating a subprotocol.
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) =
        accept listener config None

    /// Continuously accepts connections, forking the handler for each.
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        let handleConnection (ws: WebSocket) =
            (handler ws)
                .CatchAll(logAndSuppress "connection handler")
                .Ensuring(ws.Close().CatchAll(fun _ -> FIO.unit ()))

        let step =
            fio {
                match! tryAccept listener config None with
                | Some ws ->
                    let! _ = (handleConnection ws).Fork()
                    return ()
                | None ->
                    return ()
            }

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
