namespace FSharp.FIO.PostgreSQL

open Npgsql
open System
open System.Data

/// <summary>
/// PostgreSQL operation errors.
/// </summary>
type PgError =
    /// Failed to connect to the database
    | ConnectionFailed of connectionString: string * exn
    /// Connection was closed unexpectedly
    | ConnectionClosed of string
    /// Failed to open connection
    | OpenFailed of exn
    /// Failed to close connection
    | CloseFailed of exn
    /// Query execution failed
    | QueryFailed of sql: string * exn
    /// Command execution failed
    | CommandFailed of sql: string * exn
    /// Transaction operation failed
    | TransactionFailed of operation: string * exn
    /// Result set reading failed
    | ResultSetFailed of exn
    /// Pool exhausted - no connections available
    | PoolExhausted of maxSize: int
    /// Pool is closed
    | PoolClosed
    /// General PostgreSQL error
    | GeneralError of exn

    /// <summary>
    /// Converts an exception to a PgError.
    /// </summary>
    static member FromException (exn: exn) : PgError =
        GeneralError exn

    /// <summary>
    /// Gets a human-readable error message.
    /// </summary>
    override this.ToString() =
        match this with
        | ConnectionFailed(connStr, exn) ->
            $"Failed to connect: {exn.Message}"
        | ConnectionClosed msg ->
            $"Connection closed: {msg}"
        | OpenFailed exn ->
            $"Open failed: {exn.Message}"
        | CloseFailed exn ->
            $"Close failed: {exn.Message}"
        | QueryFailed(sql, exn) ->
            $"Query failed [{sql}]: {exn.Message}"
        | CommandFailed(sql, exn) ->
            $"Command failed [{sql}]: {exn.Message}"
        | TransactionFailed(op, exn) ->
            $"Transaction {op} failed: {exn.Message}"
        | ResultSetFailed exn ->
            $"ResultSet error: {exn.Message}"
        | PoolExhausted maxSize ->
            $"Pool exhausted: max {maxSize} connections"
        | PoolClosed ->
            "Pool is closed"
        | GeneralError exn ->
            $"PostgreSQL error: {exn.Message}"

    /// <summary>
    /// Converts a PgError to an exception.
    /// </summary>
    static member ToException (err: PgError) : exn =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// <summary>
/// Configuration for PostgreSQL connection pool.
/// </summary>
type ConnectionConfig =
    {
        /// <summary>PostgreSQL connection string.</summary>
        ConnectionString: string
        /// <summary>Minimum number of connections in the pool.</summary>
        MinPoolSize: int
        /// <summary>Maximum number of connections in the pool.</summary>
        MaxPoolSize: int
        /// <summary>Connection lifetime in seconds (0 = infinite).</summary>
        ConnectionLifetime: int
        /// <summary>Command timeout in seconds.</summary>
        CommandTimeout: int
    }

/// <summary>
/// Functions for creating connection configurations.
/// </summary>
module ConnectionConfig =

    /// <summary>
    /// Creates a default connection configuration with the given connection string.
    /// </summary>
    /// <param name="connectionString">The PostgreSQL connection string.</param>
    let create connectionString =
        {
            ConnectionString = connectionString
            MinPoolSize = 0
            MaxPoolSize = 100
            ConnectionLifetime = 0
            CommandTimeout = 30
        }

    /// <summary>
    /// Sets the minimum pool size.
    /// </summary>
    /// <param name="size">The minimum number of connections.</param>
    /// <param name="config">The configuration to modify.</param>
    let withMinPoolSize size config =
        { config with MinPoolSize = size }

    /// <summary>
    /// Sets the maximum pool size.
    /// </summary>
    /// <param name="size">The maximum number of connections.</param>
    /// <param name="config">The configuration to modify.</param>
    let withMaxPoolSize size config =
        { config with MaxPoolSize = size }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    /// <param name="seconds">The connection lifetime in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    let withConnectionLifetime seconds config =
        { config with ConnectionLifetime = seconds }

    /// <summary>
    /// Sets the command timeout in seconds.
    /// </summary>
    /// <param name="seconds">The command timeout in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    let withCommandTimeout seconds config =
        { config with CommandTimeout = seconds }

/// <summary>
/// Represents a managed PostgreSQL connection pool using NpgsqlDataSource.
/// </summary>
type ConnectionPool =
    private {
        DataSource: NpgsqlDataSource
        Config: ConnectionConfig
    }

/// <summary>
/// Represents a PostgreSQL database connection.
/// Wraps NpgsqlConnection with FIO-friendly operations.
/// </summary>
type Connection =
    private {
        NpgsqlConnection: NpgsqlConnection
        Pool: ConnectionPool
    }

/// <summary>
/// Represents the result of a SQL query.
/// </summary>
type ResultSet =
    private {
        Reader: NpgsqlDataReader
    }

/// <summary>
/// SQL parameter for parameterized queries.
/// </summary>
type SqlParameter =
    {
        /// <summary>Parameter name (e.g., "@id").</summary>
        Name: string
        /// <summary>Parameter value.</summary>
        Value: obj
    }

/// <summary>
/// Functions for creating SQL parameters.
/// </summary>
module SqlParameter =

    /// <summary>
    /// Creates a SQL parameter with name and value.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="value">The parameter value.</param>
    let create name value =
        { Name = name; Value = value }

    /// <summary>
    /// Creates a SQL parameter from a tuple.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="value">The parameter value.</param>
    let ofTuple (name, value) =
        { Name = name; Value = value }

/// <summary>
/// Transaction isolation levels for PostgreSQL.
/// </summary>
type IsolationLevel =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

/// <summary>
/// Functions for working with isolation levels.
/// </summary>
module IsolationLevel =

    /// <summary>
    /// Converts to System.Data.IsolationLevel.
    /// </summary>
    /// <param name="level">The isolation level to convert.</param>
    let toSystemIsolationLevel = function
        | ReadUncommitted -> System.Data.IsolationLevel.ReadUncommitted
        | ReadCommitted -> System.Data.IsolationLevel.ReadCommitted
        | RepeatableRead -> System.Data.IsolationLevel.RepeatableRead
        | Serializable -> System.Data.IsolationLevel.Serializable

    /// <summary>
    /// Default isolation level (Read Committed).
    /// </summary>
    let defaultLevel = ReadCommitted
