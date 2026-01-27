namespace FSharp.FIO.PostgreSQL

open Npgsql
open System

/// <summary>
/// PostgreSQL operation errors.
/// </summary>
type PgError =
    /// <summary>Failed to connect to the database.</summary>
    | ConnectionFailed of connectionString: string * exn
    /// <summary>Connection was closed unexpectedly.</summary>
    | ConnectionClosed of string
    /// <summary>Failed to open connection.</summary>
    | OpenFailed of exn
    /// <summary>Failed to close connection.</summary>
    | CloseFailed of exn
    /// <summary>Query execution failed.</summary>
    | QueryFailed of sql: string * exn
    /// <summary>Command execution failed.</summary>
    | CommandFailed of sql: string * exn
    /// <summary>Transaction operation failed.</summary>
    | TransactionFailed of operation: string * exn
    /// <summary>Result set reading failed.</summary>
    | ResultSetFailed of exn
    /// <summary>Pool exhausted, no connections available.</summary>
    | PoolExhausted of maxSize: int
    /// <summary>Pool is closed.</summary>
    | PoolClosed
    /// <summary>General PostgreSQL error.</summary>
    | GeneralError of exn

/// <summary>
/// Module functions for working with PgError.
/// </summary>
module PgError =

    /// <summary>
    /// Converts an exception to a PgError.
    /// </summary>
    /// <param name="exn">The exception to convert.</param>
    /// <returns>A GeneralError wrapping the exception.</returns>
    let fromException (exn: exn) : PgError =
        GeneralError exn

    /// <summary>
    /// Converts a PgError to an exception.
    /// </summary>
    /// <param name="err">The PgError to convert.</param>
    /// <returns>The underlying exception or a new exception with error details.</returns>
    let toException (err: PgError) : exn =
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
    /// <returns>A new ConnectionConfig with default pool settings.</returns>
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
    /// <returns>A new configuration with updated minimum pool size.</returns>
    let withMinPoolSize size config =
        { config with MinPoolSize = size }

    /// <summary>
    /// Sets the maximum pool size.
    /// </summary>
    /// <param name="size">The maximum number of connections.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with updated maximum pool size.</returns>
    let withMaxPoolSize size config =
        { config with MaxPoolSize = size }

    /// <summary>
    /// Sets the connection lifetime in seconds.
    /// </summary>
    /// <param name="seconds">The connection lifetime in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with updated connection lifetime.</returns>
    let withConnectionLifetime seconds config =
        { config with ConnectionLifetime = seconds }

    /// <summary>
    /// Sets the command timeout in seconds.
    /// </summary>
    /// <param name="seconds">The command timeout in seconds.</param>
    /// <param name="config">The configuration to modify.</param>
    /// <returns>A new configuration with updated command timeout.</returns>
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
    /// <returns>A new SqlParameter with the specified name and value.</returns>
    let create name value =
        { Name = name; Value = value }

    /// <summary>
    /// Creates a SQL parameter from a tuple.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="value">The parameter value.</param>
    /// <returns>A new SqlParameter with the specified name and value.</returns>
    let ofTuple (name, value) =
        { Name = name; Value = value }

/// <summary>
/// Transaction isolation levels for PostgreSQL.
/// </summary>
type IsolationLevel =
    /// <summary>Read uncommitted isolation level.</summary>
    | ReadUncommitted
    /// <summary>Read committed isolation level.</summary>
    | ReadCommitted
    /// <summary>Repeatable read isolation level.</summary>
    | RepeatableRead
    /// <summary>Serializable isolation level.</summary>
    | Serializable

/// <summary>
/// Functions for working with isolation levels.
/// </summary>
module IsolationLevel =

    /// <summary>
    /// Converts to System.Data.IsolationLevel.
    /// </summary>
    /// <param name="level">The isolation level to convert.</param>
    /// <returns>The corresponding System.Data.IsolationLevel value.</returns>
    let toSystemIsolationLevel = function
        | ReadUncommitted -> Data.IsolationLevel.ReadUncommitted
        | ReadCommitted -> Data.IsolationLevel.ReadCommitted
        | RepeatableRead -> Data.IsolationLevel.RepeatableRead
        | Serializable -> Data.IsolationLevel.Serializable

    /// <summary>
    /// Default isolation level (Read Committed).
    /// </summary>
    /// <returns>ReadCommitted isolation level.</returns>
    let defaultLevel = ReadCommitted
