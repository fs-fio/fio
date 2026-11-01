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

    let private stop (listener: HttpListener) : FIO<unit, WsError> =
        (FIO.attempt
            (fun () -> listener.Stop())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener close")

    /// Stops and releases a listener, which also drops the connections it accepted, suppressing errors; closing it again
    /// does nothing.
    let close (listener: HttpListener) : FIO<unit, WsError> =
        (FIO.attempt
            (fun () ->
                try
                    listener.Stop()
                    // Abort after Stop releases what Stop keeps (http.sys's session on Windows) without Close re-binding the port.
                    listener.Abort()
                with :? ObjectDisposedException ->
                    ())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener close")

    /// Aborts a listener immediately, dropping in-flight requests, suppressing errors.
    let abort (listener: HttpListener) : FIO<unit, WsError> =
        (FIO.attempt
            (fun () -> listener.Abort())
            WsError.fromException
        ).CatchAll(logAndSuppress "websocket listener abort")


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

    let private nextContext (listener: HttpListener) (cancellationToken: CancellationToken) =
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

    /// Accepts the next WebSocket connection, optionally negotiating the given subprotocol; interrupting it stops the
    /// listener and with it the connections accepted earlier, and closes a connection upgraded just then, barring a race.
    let accept (listener: HttpListener) (config: WebSocketConfig) (subProtocol: string option) : FIO<WebSocket, WsError> =
        FIO.cancellationToken().FlatMap(fun cancellationToken ->
            // Uninterruptible: the token stops the listener, which ends the wait, so a request that arrived is always
            // upgraded or answered.
            FIO.uninterruptible (
                (nextContext listener cancellationToken).FlatMap(fun listenerCtx ->
                    (upgrade listenerCtx config subProtocol).FlatMap(fun upgraded ->
                        match upgraded with
                        | None -> FIO.fail (ConnectionFailed "Not a WebSocket request")
                        | Some ws when cancellationToken.IsCancellationRequested ->
                            FIO.succeedWith (fun () ->
                                ws.Kill()
                                ws)
                        | Some ws -> FIO.succeed ws))))

    /// Accepts the next WebSocket connection without negotiating a subprotocol.
    let acceptDefault (listener: HttpListener) (config: WebSocketConfig) : FIO<WebSocket, WsError> =
        accept listener config None

    // A handler runs on its own fiber and nobody joins it; its typed failure is logged where it is caught, a crash here.
    let private reportCrash (handler: Fiber<unit, WsError>) =
        handler.Task().ContinueWith(
            (fun (ended: Task<FiberResult<unit, WsError>>) ->
                try
                    if ended.IsCompletedSuccessfully then
                        match ended.Result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect thrown -> eprintfn $"WebSocketServer connection handler crashed: {thrown}"
                            | _ -> ()
                        | _ -> ()
                with _ ->
                    ()),
            TaskContinuationOptions.ExecuteSynchronously)
        |> ignore

    /// Accepts connections continuously, forking the handler for each; interrupting it gives handlers the shutdown timeout,
    /// then interrupts them and ends their connections. A listener stopped under it ends it with an error.
    let acceptLoop (listener: HttpListener) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) : FIO<unit, WsError> =
        FIO.suspend <| fun () ->
            let connections = ConcurrentDictionary<WebSocket, Connection>(HashIdentity.Reference)
            // The request being awaited outlives an interruption, so the shutdown refuses it instead of losing it.
            let pending = ref null
            let listenerFailures = ref 0

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

            let refuseAtLimit (request: HttpListenerContext) =
                (FIO.attempt
                    (fun () ->
                        request.Response.StatusCode <- 503
                        request.Response.Close())
                    WsError.fromException)
                    .CatchAll(logAndSuppress "refusing a request at the connection limit")

            // Forked in the uninterruptible hand-off, a handler is protected: the loop's interruption leaves it running
            // for the shutdown's close, and the loop's exit interrupts what the shutdown left behind.
            let handOff (request: HttpListenerContext) =
                FIO.uninterruptible (
                    fio {
                        pending.Value <- null

                        if config.MaxConnections > 0 && connections.Count >= config.MaxConnections then
                            do! refuseAtLimit request
                        else
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
                                reportCrash fiber
                            | None -> ()
                    })

            let isListening () =
                try
                    listener.IsListening
                with _ ->
                    false

            // A failed handshake says nothing about the listener, so it costs no pause; a failing listener backs off
            // from 25 ms to 1 s, and a stopped one ends the loop.
            let step =
                nextRequest
                    .FlatMap(fun request ->
                        listenerFailures.Value <- 0
                        (handOff request).CatchAll(logAndSuppress "handshake"))
                    .CatchAll(fun error ->
                        if not (isListening ()) then
                            FIO.fail error
                        else
                            fio {
                                listenerFailures.Value <- listenerFailures.Value + 1

                                if listenerFailures.Value = 1 || listenerFailures.Value % 40 = 0 then
                                    do! logAndSuppress $"accept loop iteration (failure {listenerFailures.Value} in a row)" error

                                do! FIO.sleep (TimeSpan.FromMilliseconds(float (min 1000 (25 <<< min 6 (listenerFailures.Value - 1)))))
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
                    let limit = if config.ShutdownTimeout > 0 then config.ShutdownTimeout else Timeout.Infinite
                    let deadline = Task.Delay limit

                    // An abort does not end I/O that is pending without a token, such as a close waiting in a handler's
                    // finalizer for a peer that never answers; ending the connection does.
                    let endConnection (connection: Connection) =
                        FIO.succeedWith (fun () -> connection.Socket.Kill())

                    let! goingAway = (FIO.forEachParDiscard open' closeGoingAway).Fork()
                    let! _ = FIO.awaitTask (Task.WhenAny(goingAway.Task() :> Task, deadline)) WsError.fromException

                    if goingAway.Task().IsCompleted then
                        do! goingAway.Await().Unit()
                    else
                        do! goingAway.InterruptNow()

                    let finished =
                        open'
                        |> Array.choose (fun connection -> connection.Handler |> Option.map (fun _ -> connection.Finished.Task))
                    let! _ = FIO.awaitTask (Task.WhenAny(Task.WhenAll finished, deadline)) WsError.fromException

                    let unfinished =
                        open'
                        |> Array.filter (fun connection -> connection.Handler.IsSome && not connection.Finished.Task.IsCompleted)

                    do! FIO.forEachDiscard unfinished (fun connection -> connection.Handler.Value.InterruptNow())
                    do! FIO.forEachDiscard unfinished endConnection
                    do! FIO.awaitUnitTask (Task.WhenAll finished) WsError.fromException
                    do! stop listener
                    // Stopping the listener fails the request the refusal awaits, unless the stop throws midway and strands
                    // it; nothing else would interrupt the refusal, so it gets a moment to finish and is then interrupted.
                    let! _ = FIO.awaitTask (Task.WhenAny(refusal.Task() :> Task, Task.Delay 1_000)) WsError.fromException
                    do! refusal.InterruptNow()
                    do! refusal.Await().Unit()
                }

            step.Forever<unit>().Ensuring(shutdown.CatchAll(logAndSuppress "shutdown"))

    /// Starts a listener and accepts connections until interrupted, then shuts down as acceptLoop does and closes it.
    let serve (url: string) (config: WebSocketConfig) (handler: WebSocket -> FIO<unit, WsError>) : FIO<unit, WsError> =
        FIO.acquireReleaseWith
            (start url)
            close
            (fun listener -> acceptLoop listener config handler)

    /// Serves a request/response protocol with the given codecs, one request and one reply per connection.
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
