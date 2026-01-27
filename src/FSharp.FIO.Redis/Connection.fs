namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

/// <summary>
/// Functions for managing Redis connections.
/// </summary>
[<RequireQualifiedAccess>]
module Redis =

    /// <summary>
    /// Creates a RedisConnection from a multiplexer and config.
    /// </summary>
    /// <param name="mux">The connection multiplexer.</param>
    /// <param name="config">The Redis configuration.</param>
    let internal create (mux: IConnectionMultiplexer) (config: RedisConfig) : RedisConnection =
        { Multiplexer = mux; Config = config; Database = config.Database }

    /// <summary>
    /// Connects to Redis using the given configuration.
    /// </summary>
    /// <param name="config">The Redis configuration.</param>
    let connect (config: RedisConfig) : FIO<RedisConnection, RedisError> =
        let opts = RedisConfig.toConfigurationOptions config
        let endpoint = String.concat "," config.Endpoints
        FIO.awaitGenericTask(
            ConnectionMultiplexer.ConnectAsync opts,
            fun e -> ConnectionFailed(endpoint, e))
            .Map(fun mux -> create mux config)

    /// <summary>
    /// Connects to Redis using a connection string.
    /// </summary>
    /// <param name="connStr">The connection string.</param>
    let connectString (connStr: string) : FIO<RedisConnection, RedisError> =
        FIO.awaitGenericTask(
            ConnectionMultiplexer.ConnectAsync connStr,
            fun e -> ConnectionFailed(connStr, e))
            .Map(fun mux ->
                let config = RedisConfig.create connStr
                create mux config)

    /// <summary>
    /// Closes the Redis connection.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let close (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.awaitTask(
            conn.Multiplexer.CloseAsync(),
            fun e -> GeneralError e)

    /// <summary>
    /// Disposes the Redis connection.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let dispose (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () -> conn.Multiplexer.Dispose()),
            GeneralError)

    /// <summary>
    /// Gets the database interface for operations.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let getDatabase (conn: RedisConnection) : IDatabase =
        conn.Multiplexer.GetDatabase conn.Database

    /// <summary>
    /// Gets the database interface for a specific database index.
    /// </summary>
    /// <param name="db">The database index.</param>
    /// <param name="conn">The Redis connection.</param>
    let getDatabaseAt (db: int) (conn: RedisConnection) : IDatabase =
        conn.Multiplexer.GetDatabase db

    /// <summary>
    /// Gets the subscriber interface for pub/sub.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let getSubscriber (conn: RedisConnection) : ISubscriber =
        conn.Multiplexer.GetSubscriber()

    /// <summary>
    /// Gets the server interface for server commands.
    /// </summary>
    /// <param name="endpoint">The server endpoint.</param>
    /// <param name="conn">The Redis connection.</param>
    let getServer (endpoint: string) (conn: RedisConnection) : IServer =
        conn.Multiplexer.GetServer endpoint

    /// <summary>
    /// Checks if the connection is connected.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let isConnected (conn: RedisConnection) : bool =
        conn.Multiplexer.IsConnected

    /// <summary>
    /// Checks if the connection is connecting.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let isConnecting (conn: RedisConnection) : bool =
        conn.Multiplexer.IsConnecting

    /// <summary>
    /// Gets the client name.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let clientName (conn: RedisConnection) : string =
        conn.Multiplexer.ClientName

    /// <summary>
    /// Gets the connection configuration.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let configuration (conn: RedisConnection) : string =
        conn.Multiplexer.Configuration

    /// <summary>
    /// Creates a connection scoped to a different database.
    /// </summary>
    /// <param name="db">The database index.</param>
    /// <param name="conn">The Redis connection.</param>
    let withDatabase (db: int) (conn: RedisConnection) : RedisConnection =
        { conn with Database = db }

    /// <summary>
    /// Executes an action with a connection, ensuring cleanup.
    /// </summary>
    /// <param name="config">The Redis configuration.</param>
    /// <param name="action">The action to execute.</param>
    let withConnection (config: RedisConfig) (action: RedisConnection -> FIO<'R, RedisError>) : FIO<'R, RedisError> =
        FIO.acquireRelease(
            connect config,
            (fun conn -> (close conn).CatchAll(fun _ -> FIO.unit())),
            action)

    /// <summary>
    /// Executes an action with a connection from a connection string.
    /// </summary>
    /// <param name="connStr">The connection string.</param>
    /// <param name="action">The action to execute.</param>
    let withConnectionString (connStr: string) (action: RedisConnection -> FIO<'R, RedisError>) : FIO<'R, RedisError> =
        FIO.acquireRelease(
            connectString connStr,
            (fun conn -> (close conn).CatchAll(fun _ -> FIO.unit())),
            action)

    /// <summary>
    /// Pings the Redis server.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let ping (conn: RedisConnection) : FIO<System.TimeSpan, RedisError> =
        let db = getDatabase conn
        FIO.awaitGenericTask(
            db.PingAsync(),
            fun e -> CommandFailed("PING", e))
