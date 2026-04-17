namespace FIO.WebSockets

open FIO.DSL

open System
open System.Net

/// Server operations for accepting WebSocket connections.
[<RequireQualifiedAccess>]
module WebSocketServer =

    /// Starts a WebSocket server listening on the specified URL prefix.
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>The HTTP listener for the server.</returns>
    let start (url: string) =
        fio {
            let! listener = FIO.attempt ((fun () -> new HttpListener()), WsError.fromException)
            do! FIO.attempt ((fun () -> listener.Prefixes.Add url), WsError.fromException)
            do! FIO.attempt ((fun () -> listener.Start()), WsError.fromException)
            return listener
        }

    /// Starts a server with default configuration.
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>The HTTP listener for the server.</returns>
    let startDefault (url: string) = start url

    /// Closes the server.
    /// <param name="listener">The HTTP listener to close.</param>
    /// <returns>Effect that stops the server.</returns>
    let close (listener: HttpListener) =
        FIO.attempt ((fun () -> listener.Stop()), WsError.fromException)

    /// Aborts the server immediately.
    /// <param name="listener">The HTTP listener to abort.</param>
    /// <returns>Effect that aborts the server.</returns>
    let abort (listener: HttpListener) =
        FIO.attempt ((fun () -> listener.Abort()), WsError.fromException)

    /// Accepts a single incoming WebSocket connection.
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="subProtocol">Optional subprotocol to negotiate.</param>
    /// <returns>The accepted WebSocket connection.</returns>
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            let! listenerCtxTask =
                FIO.attempt ((fun () -> listener.GetContextAsync()), WsError.fromException)

            let! listenerCtx = FIO.awaitGenericTask (listenerCtxTask, WsError.fromException)

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some s -> s
                    | None -> null

                let! ctxTask =
                    FIO.attempt ((fun () -> listenerCtx.AcceptWebSocketAsync subProto), WsError.fromException)

                let! ctx = FIO.awaitGenericTask (ctxTask, WsError.fromException)
                return new WebSocket(ctx.WebSocket, config)
            else
                do! FIO.attempt ((fun () -> listenerCtx.Response.StatusCode <- 400), WsError.fromException)
                do! FIO.attempt ((fun () -> listenerCtx.Response.Close()), WsError.fromException)
                return! FIO.fail (WsError.fromException <| Exception "Not a WebSocket request")
        }

    /// Accepts a connection with default subprotocol.
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <returns>The accepted WebSocket connection.</returns>
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) = accept listener config None

    /// Continuously accepts connections and handles them with the provided handler.
    /// Each connection is forked into its own fiber for concurrent handling.
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="handler">Handler function for each connection.</param>
    /// <returns>Effect that accepts connections until interrupted.</returns>
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        let rec loop () =
            fio {
                let! ws = accept listener config None
                let! _ = (handler ws).Fork()
                return! loop ()
            }

        loop ()

    /// Runs a complete WebSocket server with the given handler.
    /// Starts listening, accepts connections in a loop, and handles them concurrently.
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="handler">Handler function for each connection.</param>
    /// <returns>Effect that runs the server until interrupted.</returns>
    let serve (url: string) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        FIO.acquireRelease (
            start url,
            (fun listener -> close listener),
            fun listener -> acceptLoop listener config handler
        )

    /// Runs a codec-based request/response server.
    /// <typeparam name="Req">The request type.</typeparam>
    /// <typeparam name="Resp">The response type.</typeparam>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="requestCodec">Codec for decoding requests.</param>
    /// <param name="responseCodec">Codec for encoding responses.</param>
    /// <param name="handler">Handler function that processes requests and returns responses.</param>
    /// <returns>Effect that runs the codec-based server until interrupted.</returns>
    let serveWith<'Req, 'Resp>
        (url: string)
        (config: WebSocketConfig)
        (requestCodec: WebSocketCodec<'Req>)
        (responseCodec: WebSocketCodec<'Resp>)
        (handler: 'Req -> FIO<'Resp, WsError>)
        =
        let wsHandler (ws: WebSocket) =
            fio {
                let! request = ws.Receive requestCodec
                let! response = handler request
                do! ws.Send(responseCodec, response)
                do! ws.Close(WebSockets.WebSocketCloseStatus.NormalClosure, "Request processed")
            }

        serve url config wsHandler
