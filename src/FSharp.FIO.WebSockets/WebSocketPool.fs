namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Net.WebSockets

/// <summary>
/// Pool state for managing WebSocket connections.
/// </summary>
type WebSocketPool =
    {
        Config: WebSocketPoolConfig
        mutable Available: (FSharp.FIO.WebSockets.WebSocket * DateTime) list
        Lock: obj
        mutable TotalCreated: int
        mutable Closed: bool
        Uri: Uri
        WsConfig: WebSocketConfig
    }

/// <summary>
/// Connection pool for WebSocket clients.
/// </summary>
[<RequireQualifiedAccess>]
module WebSockPool =

    /// <summary>
    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// </summary>
    let private logAndSuppress (context: string) (err: WsError) : FIO<unit, WsError> =
        fio {
            let str = WsError.ToString err
            do! FIO.Attempt((fun () ->
                eprintfn $"[WebSocketPool] Error during {context}: {str}"), WsError.FromException)
            return ()
        }

    /// <summary>
    /// Internal: Validates if a WebSocket is still usable.
    /// </summary>
    let private validateSocket (ws: FSharp.FIO.WebSockets.WebSocket) : FIO<bool, WsError> =
        fio {
            let! state = ws.State()
            return state = WebSocketState.Open
        }

    /// <summary>
    /// Internal: Checks if connection has exceeded lifetime.
    /// </summary>
    let private isExpired (createdAt: DateTime, config: WebSocketPoolConfig) : bool =
        if config.ConnectionLifetime = 0 then
            false
        else
            let lifetime = TimeSpan.FromSeconds(float config.ConnectionLifetime)
            DateTime.UtcNow - createdAt > lifetime

    /// <summary>
    /// Creates a new WebSocket connection pool.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="wsConfig">WebSocket configuration.</param>
    /// <param name="poolConfig">Pool configuration.</param>
    let create (uri: Uri) (wsConfig: WebSocketConfig) (poolConfig: WebSocketPoolConfig) : FIO<WebSocketPool, WsError> =
        FIO.Attempt((fun () ->
            {
                Config = poolConfig
                Available = []
                Lock = obj()
                TotalCreated = 0
                Closed = false
                Uri = uri
                WsConfig = wsConfig
            }),
        WsError.FromException)

    /// <summary>
    /// Creates a pool from a URL string.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="wsConfig">WebSocket configuration.</param>
    /// <param name="poolConfig">Pool configuration.</param>
    let createFromUrl (url: string) (wsConfig: WebSocketConfig) (poolConfig: WebSocketPoolConfig) : FIO<WebSocketPool, WsError> =
        fio {
            let! uri = FIO.Attempt((fun () -> Uri url), WsError.FromException)
            return! create uri wsConfig poolConfig
        }

    /// <summary>
    /// Closes all connections in the pool.
    /// </summary>
    /// <param name="pool">The pool to close.</param>
    let close (pool: WebSocketPool) : FIO<unit, WsError> =
        fio {
            let sockets = lock pool.Lock (fun () ->
                if pool.Closed then
                    []
                else
                    pool.Closed <- true
                    let s = pool.Available
                    pool.Available <- []
                    pool.TotalCreated <- 0
                    s)

            for ws, _createdAt in sockets do
                do! ws.Close().CatchAll(logAndSuppress "websocket close on pool close")
                do! ws.Dispose().CatchAll(logAndSuppress "websocket dispose on pool close")
        }

    /// <summary>
    /// Acquires a WebSocket from the pool.
    /// Creates a new connection if pool is empty and under max size.
    /// </summary>
    /// <param name="pool">The pool to acquire from.</param>
    let rec acquire (pool: WebSocketPool) : FIO<FSharp.FIO.WebSockets.WebSocket, FSharp.FIO.WebSockets.WsError> =
        fio {
            if pool.Closed then
                return! FIO.Fail(Closed "Pool is closed")

            let socketOpt = lock pool.Lock (fun () ->
                match pool.Available with
                | (ws, createdAt) :: rest ->
                    if isExpired (createdAt, pool.Config) then
                        pool.Available <- rest
                        pool.TotalCreated <- pool.TotalCreated - 1
                        None
                    else
                        pool.Available <- rest
                        Some ws
                | [] ->
                    if pool.TotalCreated >= pool.Config.MaxPoolSize then
                        None
                    else
                        pool.TotalCreated <- pool.TotalCreated + 1
                        None)

            match socketOpt with
            | Some ws ->
                let! isValid = validateSocket ws
                if isValid then
                    return ws
                else
                    // Socket expired or closed, create new one
                    do! ws.Dispose().CatchAll(logAndSuppress "invalid websocket disposal")
                    lock pool.Lock (fun () ->
                        pool.TotalCreated <- pool.TotalCreated - 1)
                    return! acquire pool

            | None ->
                // Need to create new connection
                let canCreate = lock pool.Lock (fun () ->
                    if pool.TotalCreated < pool.Config.MaxPoolSize then
                        pool.TotalCreated <- pool.TotalCreated + 1
                        true
                    else
                        false)

                if canCreate then
                    return! WebSocketClient.connect pool.Uri pool.WsConfig System.Threading.CancellationToken.None
                else
                    return! FIO.Fail(PoolExhausted $"Pool exhausted: {pool.TotalCreated}/{pool.Config.MaxPoolSize} connections")
        }

    /// <summary>
    /// Returns a WebSocket to the pool for reuse.
    /// </summary>
    /// <param name="ws">The WebSocket to return.</param>
    /// <param name="pool">The pool to return to.</param>
    let releaseToPool (ws: FSharp.FIO.WebSockets.WebSocket) (pool: WebSocketPool) : FIO<unit, WsError> =
        (fio {
            if pool.Closed then
                do! ws.Close().CatchAll(logAndSuppress "websocket close on closed pool")
                do! ws.Dispose().CatchAll(logAndSuppress "websocket dispose on closed pool")
            else
                let! state = ws.State()

                if state = WebSocketState.Open then
                    // Connection still good, return to pool
                    lock pool.Lock (fun () ->
                        if not pool.Closed && pool.TotalCreated <= pool.Config.MaxPoolSize then
                            pool.Available <- (ws, DateTime.UtcNow) :: pool.Available
                        else
                            // Pool closed or over limit during release
                            pool.TotalCreated <- pool.TotalCreated - 1)
                    return ()
                else
                    // Connection closed, don't return to pool
                    do! ws.Dispose().CatchAll(logAndSuppress "closed websocket disposal")
                    lock pool.Lock (fun () ->
                        pool.TotalCreated <- pool.TotalCreated - 1)
        }).CatchAll(logAndSuppress "release to pool")

    /// <summary>
    /// Executes an action with a pooled WebSocket using AcquireRelease pattern.
    /// </summary>
    /// <param name="action">The action to execute with the WebSocket.</param>
    /// <param name="pool">The pool to acquire from.</param>
    let withConnection<'R>
        (action: FSharp.FIO.WebSockets.WebSocket -> FIO<'R, WsError>)
        (pool: WebSocketPool)
        : FIO<'R, WsError> =
        FIO.AcquireRelease(
            acquire pool,
            (fun ws -> releaseToPool ws pool),
            action
        )

    /// <summary>
    /// Sends a value using a codec through a pooled connection.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="pool">The pool to use.</param>
    let sendPooled<'T> (codec: WebSocketCodec<'T>) (value: 'T) (pool: WebSocketPool) : FIO<unit, WsError> =
        withConnection (fun ws -> ws.Send(codec, value)) pool

    /// <summary>
    /// Receives and decodes a value using a codec through a pooled connection.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="pool">The pool to use.</param>
    let receivePooled<'T> (codec: WebSocketCodec<'T>) (pool: WebSocketPool) : FIO<'T, WsError> =
        withConnection (fun ws -> ws.Receive codec) pool

    /// <summary>
    /// Gets the current pool configuration.
    /// </summary>
    /// <param name="pool">The pool.</param>
    let getConfig (pool: WebSocketPool) : WebSocketPoolConfig =
        pool.Config

    /// <summary>
    /// Gets pool statistics.
    /// </summary>
    /// <param name="pool">The pool.</param>
    let getStats (pool: WebSocketPool) : int * int =
        lock pool.Lock (fun () ->
            (pool.Available.Length, pool.TotalCreated))

    /// <summary>
    /// Checks if the pool is closed.
    /// </summary>
    /// <param name="pool">The pool.</param>
    let isClosed (pool: WebSocketPool) : bool =
        pool.Closed
