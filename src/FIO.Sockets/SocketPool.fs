namespace FIO.Sockets

open FIO.DSL

open System

/// <summary>
/// Represents a connection pool for client sockets.
/// Use SocketPool module functions for operations.
/// </summary>
type SocketPool =
    internal
        {
            /// <summary>
            /// Pool configuration.
            /// </summary>
            Config: SocketPoolConfig
            /// <summary>
            /// Available connections (managed internally).
            /// </summary>
            mutable Available: (Socket * DateTime) list
            /// <summary>
            /// Lock for thread-safe pool operations.
            /// </summary>
            Lock: obj
            /// <summary>
            /// Total connections created.
            /// </summary>
            mutable TotalCreated: int
            /// <summary>
            /// Whether the pool is closed.
            /// </summary>
            mutable Closed: bool
        }

/// <summary>
/// Connection pool for client sockets.
/// </summary>
[<RequireQualifiedAccess>]
module SocketPool =

    /// <summary>
    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// </summary>
    let private logAndSuppress (context: string) (err: SocketError) : FIO<unit, SocketError> =
        fio {
            let message = err.ToString()
            do! FIO.attempt((fun () ->
                eprintfn $"[SocketPool] Error during {context}: {message}"), SocketError.fromException)
            return ()
        }

    /// <summary>
    /// Internal: Validates if a socket is still usable.
    /// </summary>
    let private validateSocket (socket: Socket, config: SocketPoolConfig) : FIO<bool, SocketError> =
        fio {
            if not config.ValidateOnAcquire then
                return true
            else
                return socket.IsConnected()
        }

    /// <summary>
    /// Internal: Checks if connection has exceeded lifetime.
    /// </summary>
    let private isExpired (createdAt: DateTime, config: SocketPoolConfig) : bool =
        if config.ConnectionLifetime = 0 then
            false
        else
            let lifetime = TimeSpan.FromSeconds(float config.ConnectionLifetime)
            DateTime.UtcNow - createdAt > lifetime

    /// <summary>
    /// Creates a new socket connection pool.
    /// </summary>
    /// <param name="config">Pool configuration.</param>
    /// <returns>The created socket pool.</returns>
    let create (config: SocketPoolConfig) : FIO<SocketPool, SocketError> =
        FIO.attempt((fun () ->
            {
                Config = config
                Available = []
                Lock = obj()
                TotalCreated = 0
                Closed = false
            }),
        SocketError.fromException)

    /// <summary>
    /// Closes all connections in the pool.
    /// </summary>
    /// <param name="pool">The pool to close.</param>
    let close (pool: SocketPool) : FIO<unit, SocketError> =
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

            for socket, _createdAt in sockets do
                do! socket.Close().CatchAll(logAndSuppress "pool close")
        }

    /// <summary>
    /// Internal type to track acquire decision from the pool.
    /// </summary>
    type private AcquireDecision =
        | ReuseSocket of Socket
        | CreateNew
        | WaitForSlot

    /// <summary>
    /// Acquires a socket from the pool.
    /// </summary>
    /// <param name="pool">The pool to acquire from.</param>
    /// <returns>A socket from the pool.</returns>
    let acquire (pool: SocketPool) : FIO<Socket, SocketError> =
        fio {
            if pool.Closed then
                return! FIO.fail PoolClosed

            let decision = lock pool.Lock (fun () ->
                match pool.Available with
                | (socket, createdAt) :: rest ->
                    if isExpired (createdAt, pool.Config) then
                        // Expired socket: remove it and try to create a new one
                        pool.Available <- rest
                        pool.TotalCreated <- pool.TotalCreated - 1
                        if pool.TotalCreated < pool.Config.MaxPoolSize then
                            pool.TotalCreated <- pool.TotalCreated + 1
                            CreateNew
                        else
                            WaitForSlot
                    else
                        // Valid socket: remove from pool and reuse
                        pool.Available <- rest
                        ReuseSocket socket
                | [] ->
                    // No available sockets
                    if pool.TotalCreated < pool.Config.MaxPoolSize then
                        pool.TotalCreated <- pool.TotalCreated + 1
                        CreateNew
                    else
                        WaitForSlot)

            match decision with
            | ReuseSocket socket ->
                let! isValid = validateSocket (socket, pool.Config)
                if isValid then
                    return socket
                else
                    do! socket.Close().CatchAll(logAndSuppress "invalid socket close")
                    lock pool.Lock (fun () ->
                        pool.TotalCreated <- pool.TotalCreated - 1)
                    return! SocketClient.connect pool.Config.SocketConfig

            | CreateNew ->
                return! SocketClient.connect pool.Config.SocketConfig

            | WaitForSlot ->
                return! FIO.fail (PoolExhausted pool.Config.MaxPoolSize)
        }

    /// <summary>
    /// Returns a socket to the pool.
    /// </summary>
    /// <param name="socket">The socket to return.</param>
    /// <param name="pool">The pool to return to.</param>
    let releaseToPool (socket: Socket, pool: SocketPool) : FIO<unit, SocketError> =
        (fio {
            if pool.Closed then
                do! socket.Close().CatchAll(logAndSuppress "socket close on closed pool")
            else
                lock pool.Lock (fun () ->
                    if socket.IsConnected() then
                        pool.Available <- (socket, DateTime.UtcNow) :: pool.Available
                    else
                        pool.TotalCreated <- pool.TotalCreated - 1)
            return ()
        }).CatchAll(logAndSuppress "release to pool")

    /// <summary>
    /// Executes an action with a socket from the pool.
    /// </summary>
    /// <param name="action">Action to execute with the socket.</param>
    /// <param name="pool">The pool to acquire from.</param>
    /// <returns>The result of the action.</returns>
    let withSocket (action: Socket -> FIO<'R, SocketError>, pool: SocketPool) : FIO<'R, SocketError> =
        fio {
            let! socket = acquire pool

            let actionWithCleanup = fio {
                let! result = action socket
                do! releaseToPool (socket, pool)
                return result
            }

            let errorHandler = fio {
                do! socket.Close().CatchAll(logAndSuppress "socket close on error")
                lock pool.Lock (fun () ->
                    pool.TotalCreated <- pool.TotalCreated - 1)
            }

            return! actionWithCleanup.Ensuring errorHandler
        }

    /// <summary>
    /// Gets pool configuration.
    /// </summary>
    /// <param name="pool">The pool to query.</param>
    /// <returns>The pool configuration.</returns>
    let getConfig (pool: SocketPool) : SocketPoolConfig =
        pool.Config

    /// <summary>
    /// Gets pool statistics.
    /// </summary>
    /// <param name="pool">The pool to query.</param>
    /// <returns>A tuple of (available, total created, max size).</returns>
    let getStats (pool: SocketPool) : int * int * int =
        lock pool.Lock (fun () ->
            pool.Available.Length, pool.TotalCreated, pool.Config.MaxPoolSize)

    /// <summary>
    /// Checks if the pool is closed.
    /// </summary>
    /// <param name="pool">The pool to check.</param>
    /// <returns>True if the pool is closed, false otherwise.</returns>
    let isClosed (pool: SocketPool) : bool =
        pool.Closed

    /// <summary>
    /// Sends data using a codec via pool.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="pool">The connection pool.</param>
    let sendPooled<'T> (codec: SocketCodec<'T>, value: 'T, pool: SocketPool) : FIO<unit, SocketError> =
        withSocket ((fun socket -> socket.Send(codec, value)), pool)

    /// <summary>
    /// Receives data using a codec via pool.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <param name="pool">The connection pool.</param>
    /// <returns>The decoded value.</returns>
    let receivePooled<'T> (codec: SocketCodec<'T>, maxBytes: int, pool: SocketPool) : FIO<'T, SocketError> =
        withSocket ((fun socket -> socket.Receive(codec, maxBytes)), pool)

    /// <summary>
    /// Executes an action with a pooled socket (alias for withSocket).
    /// </summary>
    /// <param name="action">Action to execute with the socket.</param>
    /// <param name="pool">The connection pool.</param>
    /// <returns>The result of the action.</returns>
    let withPooledConnection (action: Socket -> FIO<'R, SocketError>, pool: SocketPool) : FIO<'R, SocketError> =
        withSocket (action, pool)
