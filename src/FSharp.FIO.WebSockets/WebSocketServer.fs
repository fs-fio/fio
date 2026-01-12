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
    /// <param name="config">WebSocket configuration options.</param>
    let start (url: string) : FIO<HttpListener, WsError> =
        fio {
            let! listener = FIO.Attempt((fun () -> new HttpListener()), WsError.FromException)
            do! FIO.Attempt((fun () -> listener.Prefixes.Add url), WsError.FromException)
            do! FIO.Attempt((fun () -> listener.Start()), WsError.FromException)
            return listener
        }

    /// <summary>
    /// Starts a server with default configuration.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    let startDefault (url: string) : FIO<HttpListener, WsError> =
        start url

    /// <summary>
    /// Closes the server.
    /// </summary>
    /// <param name="listener">The HTTP listener to close.</param>
    let close (listener: HttpListener) : FIO<unit, WsError> =
        FIO.Attempt((fun () -> listener.Stop()), WsError.FromException)

    /// <summary>
    /// Aborts the server immediately.
    /// </summary>
    /// <param name="listener">The HTTP listener to abort.</param>
    let abort (listener: HttpListener) : FIO<unit, WsError> =
        FIO.Attempt((fun () -> listener.Abort()), WsError.FromException)

    /// <summary>
    /// Accepts a single incoming WebSocket connection.
    /// </summary>
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="subProtocol">Optional subprotocol to negotiate.</param>
    let accept
        (listener: HttpListener)
        (config: WebSocketConfig)
        (subProtocol: string option)
        : FIO<WebSocket, WsError> =
        fio {
            let! listenerCtxTask = FIO.Attempt((fun () -> listener.GetContextAsync()), WsError.FromException)
            let! listenerCtx = FIO.AwaitTask(listenerCtxTask, WsError.FromException)

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto =
                    match subProtocol with
                    | Some s -> s
                    | None -> null

                let! ctxTask = FIO.Attempt((fun () -> listenerCtx.AcceptWebSocketAsync subProto), WsError.FromException)
                let! ctx = FIO.AwaitTask(ctxTask, WsError.FromException)
                return new WebSocket(ctx.WebSocket, config)
            else
                do! FIO.Attempt((fun () -> listenerCtx.Response.StatusCode <- 400), WsError.FromException)
                do! FIO.Attempt((fun () -> listenerCtx.Response.Close()), WsError.FromException)
                return! FIO.Fail(WsError.FromException <| Exception "Not a WebSocket request")
        }

    /// <summary>
    /// Accepts a connection with default subprotocol.
    /// </summary>
    /// <param name="listener">The HTTP listener.</param>
    /// <param name="config">WebSocket configuration options.</param>
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
        FIO.AcquireRelease(
            start url,
            (fun listener -> close listener),
            fun listener -> acceptLoop listener config handler
        )

    /// <summary>
    /// Runs a codec-based request/response server.
    /// Each request is decoded, passed to handler, and response is encoded back.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="onError">Function to map exceptions to the error type.</param>
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

/// <summary>
/// Backward compatibility: WebSocketServer type matching original API.
/// </summary>
type WebSocketServer private (listener: HttpListener, config: WebSocketConfig) =

    /// <summary>
    /// Creates a new WebSocket server with custom configuration.
    /// </summary>
    /// <param name="config">WebSocket configuration options.</param>
    static member Create(config: WebSocketConfig) : FIO<WebSocketServer, WsError> =
        fio {
            let! listener = FIO.Attempt((fun () -> new HttpListener()), WsError.FromException)
            return WebSocketServer(listener, config)
        }

    /// <summary>
    /// Creates a new WebSocket server with default configuration.
    /// </summary>
    static member Create() : FIO<WebSocketServer, WsError> =
        WebSocketServer.Create WebSocketConfig.Default

    /// <summary>
    /// Starts the server listening on the specified URL prefix.
    /// </summary>
    /// <param name="url">The URL prefix to listen on.</param>
    member _.Start(url: string) : FIO<unit, WsError> =
        fio {
            do! FIO.Attempt((fun () -> listener.Prefixes.Add url), WsError.FromException)
            do! FIO.Attempt((fun () -> listener.Start()), WsError.FromException)
        }

    /// <summary>
    /// Accepts an incoming WebSocket connection.
    /// </summary>
    /// <param name="subProtocol">Optional subprotocol to negotiate.</param>
    member _.Accept(subProtocol: string option) : FIO<WebSocket, WsError> =
        WebSocketServer.accept listener config subProtocol

    /// <summary>
    /// Accepts an incoming WebSocket connection.
    /// </summary>
    member this.Accept() : FIO<WebSocket, WsError> =
        this.Accept None

    /// <summary>
    /// Stops the server.
    /// </summary>
    member _.Close() : FIO<unit, WsError> =
        FIO.Attempt((fun () -> listener.Stop()), WsError.FromException)

    /// <summary>
    /// Aborts the server immediately.
    /// </summary>
    member _.Abort() : FIO<unit, WsError> =
        FIO.Attempt((fun () -> listener.Abort()), WsError.FromException)
