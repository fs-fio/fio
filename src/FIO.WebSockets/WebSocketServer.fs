namespace FIO.WebSockets

open FIO.DSL

open System
open System.Net
open System.Threading.Tasks

/// <summary>Builds effects for accepting incoming WebSocket connections on a server.</summary>
[<RequireQualifiedAccess>]
module WebSocketServer =

    /// <summary>Creates an effect that starts a WebSocket server listening on the specified URL prefix.</summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>An effect that produces the running HTTP listener.</returns>
    let start (url: string) =
        fio {
            let! listener = FIO.attempt ((fun () -> new HttpListener()), WsError.fromException)
            do! FIO.attempt ((fun () -> listener.Prefixes.Add url), WsError.fromException)
            do! FIO.attempt ((fun () -> listener.Start()), WsError.fromException)
            return listener
        }

    /// <summary>Creates an effect that starts a WebSocket server with default configuration on the specified URL prefix.</summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <returns>An effect that produces the running HTTP listener.</returns>
    let startDefault (url: string) = start url

    /// <summary>Creates an effect that stops the server gracefully.</summary>
    /// <param name="listener">The HTTP listener to stop.</param>
    /// <returns>An effect that completes when the server has stopped.</returns>
    let close (listener: HttpListener) =
        FIO.attempt ((fun () -> listener.Stop()), WsError.fromException)

    /// <summary>Creates an effect that aborts the server immediately without waiting for pending requests.</summary>
    /// <param name="listener">The HTTP listener to abort.</param>
    /// <returns>An effect that completes when the server has been aborted.</returns>
    let abort (listener: HttpListener) =
        FIO.attempt ((fun () -> listener.Abort()), WsError.fromException)

    /// <summary>Creates an effect that accepts a single incoming WebSocket connection.</summary>
    /// <param name="listener">The HTTP listener to accept connections from.</param>
    /// <param name="config">The configuration options for the accepted connection.</param>
    /// <param name="subProtocol">An optional subprotocol to negotiate during the handshake.</param>
    /// <returns>An effect that produces the accepted <c>WebSocket</c>, or fails if the request is not a WebSocket upgrade.</returns>
    /// <remarks>The accept observes the running fiber's cancellation token. Because <c>HttpListener.GetContextAsync</c> has no native cancellation overload, interruption stops the listener via <c>HttpListener.Stop()</c>, which aborts every outstanding accept on that listener — acceptable when the listener is owned by the fiber via <c>serve</c>.</remarks>
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) =
        fio {
            let! ct = FIO.cancellationToken ()

            let! listenerCtx =
                FIO.awaitGenericTask (
                    Task.Run<HttpListenerContext>(fun () ->
                        task {
                            use _reg =
                                ct.Register(fun () ->
                                    try
                                        listener.Stop()
                                    with _ ->
                                        ())

                            return! listener.GetContextAsync()
                        }),
                    WsError.fromException
                )

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

    /// <summary>Creates an effect that accepts a single incoming WebSocket connection with no subprotocol negotiation.</summary>
    /// <param name="listener">The HTTP listener to accept connections from.</param>
    /// <param name="config">The configuration options for the accepted connection.</param>
    /// <returns>An effect that produces the accepted <c>WebSocket</c>.</returns>
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) = accept listener config None

    /// <summary>Creates an effect that continuously accepts connections and forks a handler fiber for each one.</summary>
    /// <param name="listener">The HTTP listener to accept connections from.</param>
    /// <param name="config">The configuration options for each accepted connection.</param>
    /// <param name="handler">A function from an accepted WebSocket to the effect that handles the connection.</param>
    /// <returns>An effect that loops indefinitely until interrupted.</returns>
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        let rec loop () =
            fio {
                let! ws = accept listener config None
                let! _ = (handler ws).Fork()
                return! loop ()
            }

        loop ()

    /// <summary>Builds a resource-managed effect that starts a server, accepts connections in a loop, and stops the server on every outcome.</summary>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">The configuration options for each accepted connection.</param>
    /// <param name="handler">A function from an accepted WebSocket to the effect that handles the connection.</param>
    /// <returns>An effect that runs the server until interrupted, then stops the listener.</returns>
    let serve (url: string) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) =
        FIO.acquireRelease (
            start url,
            (fun listener -> close listener),
            fun listener -> acceptLoop listener config handler
        )

    /// <summary>Builds a resource-managed server that decodes each request with a codec, passes it to a handler, and encodes the response back.</summary>
    /// <typeparam name="Req">The request type decoded from incoming frames.</typeparam>
    /// <typeparam name="Resp">The response type encoded into outgoing frames.</typeparam>
    /// <param name="url">The URL prefix to listen on.</param>
    /// <param name="config">The configuration options for each accepted connection.</param>
    /// <param name="requestCodec">The codec for decoding incoming request frames.</param>
    /// <param name="responseCodec">The codec for encoding outgoing response frames.</param>
    /// <param name="handler">A function from a decoded request to the response effect.</param>
    /// <returns>An effect that runs the codec-based server until interrupted.</returns>
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
