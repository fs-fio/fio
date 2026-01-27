namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Net.WebSockets

/// <summary>
/// Pool state for managing WebSocket connections.
/// </summary>
type WebSocketPool =
    {
        /// <summary>
        /// The pool configuration.
        /// </summary>
        Config: WebSocketPoolConfig
        /// <summary>
        /// List of available connections with their creation timestamps.
        /// </summary>
        mutable Available: (FSharp.FIO.WebSockets.WebSocket * DateTime) list
        /// <summary>
        /// Lock object for thread-safe pool operations.
        /// </summary>
        Lock: obj
        /// <summary>
        /// Total number of connections created by this pool.
        /// </summary>
        mutable TotalCreated: int
        /// <summary>
        /// Whether the pool has been closed.
        /// </summary>
        mutable Closed: bool
        /// <summary>
        /// The WebSocket server URI.
        /// </summary>
        Uri: Uri
        /// <summary>
        /// The WebSocket configuration for new connections.
        /// </summary>
        WsConfig: WebSocketConfig
    }

/// <summary>
/// Connection pool for WebSocket clients.
/// </summary>
[<RequireQualifiedAccess>]
module WebSocketPool =

    /// <summary>
    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// </summary>
    let private logAndSuppress (context: string) (err: WsError) : FIO<unit, WsError> =
        fio {
            let str = err.ToString()
            do! FIO.attempt((fun () ->
                eprintfn $"[WebSocketPool] Error during {context}: {str}"), WsError.fromException)
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
    /// <returns>A new connection pool.</returns>
    let create (uri: Uri) (wsConfig: WebSocketConfig) (poolConfig: WebSocketPoolConfig) : FIO<WebSocketPool, WsError> =
        FIO.attempt((fun () ->
            {
                Config = poolConfig
                Available = []
                Lock = obj()
                TotalCreated = 0
                Closed = false
                Uri = uri
                WsConfig = wsConfig
            }),
        WsError.fromException)

    /// <summary>
    /// Creates a pool from a URL string.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="wsConfig">WebSocket configuration.</param>
    /// <param name="poolConfig">Pool configuration.</param>
    /// <returns>A new connection pool.</returns>
    let createFromUrl (url: string) (wsConfig: WebSocketConfig) (poolConfig: WebSocketPoolConfig) : FIO<WebSocketPool, WsError> =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.fromException)
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
    /// </summary>
    /// <param name="pool">The pool to acquire from.</param>
    /// <returns>A WebSocket connection from the pool.</returns>
    let rec acquire (pool: WebSocketPool) : FIO<FSharp.FIO.WebSockets.WebSocket, FSharp.FIO.WebSockets.WsError> =
        fio {
            if pool.Closed then
                return! FIO.fail(Closed "Pool is closed")

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
                    return! FIO.fail(PoolExhausted $"Pool exhausted: {pool.TotalCreated}/{pool.Config.MaxPoolSize} connections")
        }

    /// <summary>
    /// Returns a WebSocket to the pool for reuse.
    /// </summary>
    /// <param name="ws">The WebSocket to return.</param>
    /// <param name="pool">The pool to return to.</param>
    let releaseToPool (ws: FSharp.FIO.WebSockets.WebSocket) (pool: WebSocketPool) : FIO<unit, WsError> =
        (fio {
            // Check state first (outside lock is OK for this read)
            let! state = ws.State()

            // Then do atomic pool update inside lock
            let action = lock pool.Lock (fun () ->
                if pool.Closed then
                    "close_disposed"
                elif state = WebSocketState.Open && pool.TotalCreated <= pool.Config.MaxPoolSize then
                    pool.Available <- (ws, DateTime.UtcNow) :: pool.Available
                    "returned"
                else
                    pool.TotalCreated <- pool.TotalCreated - 1
                    "dispose")

            match action with
            | "close_disposed" ->
                do! ws.Close().CatchAll(logAndSuppress "websocket close on closed pool")
                do! ws.Dispose().CatchAll(logAndSuppress "websocket dispose on closed pool")
            | "dispose" ->
                do! ws.Dispose().CatchAll(logAndSuppress "closed websocket disposal")
            | _ -> () // "returned" - nothing more to do
        }).CatchAll(logAndSuppress "release to pool")

    /// <summary>
    /// Executes an action with a pooled WebSocket using AcquireRelease pattern.
    /// </summary>
    /// <typeparam name="R">The result type of the action.</typeparam>
    /// <param name="action">The action to execute with the WebSocket.</param>
    /// <param name="pool">The pool to acquire from.</param>
    /// <returns>The result of the action.</returns>
    let withConnection<'R>
        (action: FSharp.FIO.WebSockets.WebSocket -> FIO<'R, WsError>)
        (pool: WebSocketPool)
        : FIO<'R, WsError> =
        FIO.acquireRelease(
            acquire pool,
            (fun ws -> releaseToPool ws pool),
            action
        )

    /// <summary>
    /// Sends a value using a codec through a pooled connection.
    /// </summary>
    /// <typeparam name="T">The type of value to send.</typeparam>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="pool">The pool to use.</param>
    let sendPooled<'T> (codec: WebSocketCodec<'T>) (value: 'T) (pool: WebSocketPool) : FIO<unit, WsError> =
        withConnection (fun ws -> ws.Send(codec, value)) pool

    /// <summary>
    /// Receives and decodes a value using a codec through a pooled connection.
    /// </summary>
    /// <typeparam name="T">The type of value to receive.</typeparam>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="pool">The pool to use.</param>
    /// <returns>The received and decoded value.</returns>
    let receivePooled<'T> (codec: WebSocketCodec<'T>) (pool: WebSocketPool) : FIO<'T, WsError> =
        withConnection (fun ws -> ws.Receive codec) pool

    /// <summary>
    /// Gets the current pool configuration.
    /// </summary>
    /// <param name="pool">The pool.</param>
    /// <returns>The pool configuration.</returns>
    let getConfig (pool: WebSocketPool) : WebSocketPoolConfig =
        pool.Config

    /// <summary>
    /// Gets pool statistics.
    /// </summary>
    /// <param name="pool">The pool.</param>
    /// <returns>A tuple of (available count, total created count).</returns>
    let getStats (pool: WebSocketPool) : int * int =
        lock pool.Lock (fun () ->
            (pool.Available.Length, pool.TotalCreated))

    /// <summary>
    /// Checks if the pool is closed.
    /// </summary>
    /// <param name="pool">The pool.</param>
    /// <returns>True if the pool is closed.</returns>
    let isClosed (pool: WebSocketPool) : bool =
        pool.Closed
