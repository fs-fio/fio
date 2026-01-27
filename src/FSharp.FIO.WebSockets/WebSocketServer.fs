namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Net

/// <summary>
/// Server operations for accepting WebSocket connections.
/// </summary>
[<RequireQualifiedAccess>]
module WebSocketServer =

    /// <summary>
    /// Starts a WebSocket server listening on the specified URL prefix.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>The HTTP listener for the server.</returns>
    let start (url: string) : FIO<HttpListener, WsError> =
        fio {
            let! listener = FIO.attempt((fun () -> new HttpListener()), WsError.fromException)
            do! FIO.attempt((fun () -> listener.Prefixes.Add url), WsError.fromException)
            do! FIO.attempt((fun () -> listener.Start()), WsError.fromException)
            return listener
        }

    /// <summary>
    /// Starts a server with default configuration.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>The HTTP listener for the server.</returns>
    let startDefault (url: string) : FIO<HttpListener, WsError> =
        start url

    /// <summary>
    /// Closes the server.
    /// </summary>
    /// <param name="listener">The HTTP listener to close.</param>
    let close (listener: HttpListener) : FIO<unit, WsError> =
        FIO.attempt((fun () -> listener.Stop()), WsError.fromException)

    /// <summary>
    /// Aborts the server immediately.
    /// </summary>
    /// <param name="listener">The HTTP listener to abort.</param>
    let abort (listener: HttpListener) : FIO<unit, WsError> =
        FIO.attempt((fun () -> listener.Abort()), WsError.fromException)

    /// <summary>
    /// Accepts a single incoming WebSocket connection.
    /// </summary>
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="subProtocol">Optional subprotocol to negotiate.</param>
    /// <returns>The accepted WebSocket connection.</returns>
    let accept
        (listener: HttpListener)
        (config: WebSocketConfig)
        (subProtocol: string option)
        : FIO<WebSocket, WsError> =
        fio {
            let! listenerCtxTask = FIO.attempt((fun () -> listener.GetContextAsync()), WsError.fromException)
            let! listenerCtx = FIO.awaitGenericTask(listenerCtxTask, WsError.fromException)

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some s -> s
                    | None -> null

                let! ctxTask = FIO.attempt((fun () -> listenerCtx.AcceptWebSocketAsync subProto), WsError.fromException)
                let! ctx = FIO.awaitGenericTask(ctxTask, WsError.fromException)
                return new WebSocket(ctx.WebSocket, config)
            else
                do! FIO.attempt((fun () -> listenerCtx.Response.StatusCode <- 400), WsError.fromException)
                do! FIO.attempt((fun () -> listenerCtx.Response.Close()), WsError.fromException)
                return! FIO.fail(WsError.fromException <| Exception "Not a WebSocket request")
        }

    /// <summary>
    /// Accepts a connection with default subprotocol.
    /// </summary>
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <returns>The accepted WebSocket connection.</returns>
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) : FIO<WebSocket, WsError> =
        accept listener config None

    /// <summary>
    /// Continuously accepts connections and handles them with the provided handler.
    /// Each connection is forked into its own fiber for concurrent handling.
    /// </summary>
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="handler">Handler function for each connection.</param>
    let acceptLoop
        (listener: HttpListener)
        (config: WebSocketConfig)
        (handler: WebSocket -> FIO<unit, WsError>)
        : FIO<unit, WsError> =
        let rec loop () =
            fio {
                let! ws = accept listener config None
                let! _ = (handler ws).Fork()
                return! loop ()
            }
        loop ()

    /// <summary>
    /// Runs a complete WebSocket server with the given handler.
    /// Starts listening, accepts connections in a loop, and handles them concurrently.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="handler">Handler function for each connection.</param>
    let serve
        (url: string)
        (config: WebSocketConfig)
        (handler: WebSocket -> FIO<unit, WsError>)
        : FIO<unit, WsError> =
        FIO.acquireRelease(
            start url,
            (fun listener -> close listener),
            fun listener -> acceptLoop listener config handler
        )

    /// <summary>
    /// Runs a codec-based request/response server.
    /// </summary>
    /// <typeparam name="Req">The request type.</typeparam>
    /// <typeparam name="Resp">The response type.</typeparam>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="requestCodec">Codec for decoding requests.</param>
    /// <param name="responseCodec">Codec for encoding responses.</param>
    /// <param name="handler">Handler function that processes requests and returns responses.</param>
    let serveWith<'Req, 'Resp>
        (url: string)
        (config: WebSocketConfig)
        (requestCodec: WebSocketCodec<'Req>)
        (responseCodec: WebSocketCodec<'Resp>)
        (handler: 'Req -> FIO<'Resp, WsError>)
        : FIO<unit, WsError> =
        let wsHandler (ws: WebSocket) =
            fio {
                let! request = ws.Receive requestCodec
                let! response = handler request
                do! ws.Send(responseCodec, response)
                do! ws.Close(WebSockets.WebSocketCloseStatus.NormalClosure, "Request processed")
            }
        serve url config wsHandler
