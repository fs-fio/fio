namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

open Npgsql
open System

/// <summary>
/// Functions for managing PostgreSQL connection pools.
/// </summary>
[<RequireQualifiedAccess>]
module Pool =

    /// <summary>
    /// Creates a new connection pool from configuration.
    /// Uses NpgsqlDataSource for efficient connection pooling.
    /// </summary>
    /// <param name="config">The connection pool configuration.</param>
    /// <returns>A new ConnectionPool instance.</returns>
    let create (config: ConnectionConfig) : FIO<ConnectionPool, PgError> =
        FIO.attempt(
            (fun () ->
                let builder = NpgsqlDataSourceBuilder(config.ConnectionString)

                // Configure connection pool settings
                builder.ConnectionStringBuilder.MinPoolSize <- config.MinPoolSize
                builder.ConnectionStringBuilder.MaxPoolSize <- config.MaxPoolSize
                builder.ConnectionStringBuilder.ConnectionLifetime <- config.ConnectionLifetime
                builder.ConnectionStringBuilder.CommandTimeout <- config.CommandTimeout

                let dataSource = builder.Build()

                {
                    DataSource = dataSource
                    Config = config
                }),
            fun e -> ConnectionFailed(config.ConnectionString, e))

    /// <summary>
    /// Acquires a connection from the pool.
    /// The connection should be released by calling Conn.close or using withConn.
    /// </summary>
    /// <param name="pool">The connection pool to acquire from.</param>
    /// <returns>A Connection from the pool.</returns>
    let acquire (pool: ConnectionPool) : FIO<Connection, PgError> =
        FIO.attempt(
            (fun () -> pool.DataSource.CreateConnection()),
            GeneralError)
            .Map(fun npgsqlConn -> Connection.create npgsqlConn pool)

    /// <summary>
    /// Acquires a connection from the pool asynchronously.
    /// </summary>
    /// <param name="pool">The connection pool to acquire from.</param>
    /// <returns>A Connection from the pool.</returns>
    let acquireAsync (pool: ConnectionPool) : FIO<Connection, PgError> =
        FIO.awaitGenericTask(pool.DataSource.OpenConnectionAsync().AsTask(), GeneralError)
            .Map(fun npgsqlConn -> Connection.create npgsqlConn pool)

    /// <summary>
    /// Executes an action with a connection from the pool.
    /// The connection is automatically opened and closed.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="pool">The connection pool to acquire from.</param>
    /// <returns>The result of the action.</returns>
    let withConnection (action: Connection -> FIO<'R, PgError>) (pool: ConnectionPool) : FIO<'R, PgError> =
        fio {
            let! conn = acquire pool
            do! Connection.openAsync conn
            let closeConn = Connection.closeAsync(conn).CatchAll(fun _ -> FIO.unit())
            return! (action conn).Ensuring(closeConn)
        }

    /// <summary>
    /// Executes an action with a connection from the pool asynchronously.
    /// The connection is automatically acquired and released.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="pool">The connection pool to acquire from.</param>
    /// <returns>The result of the action.</returns>
    let withConnectionAsync (action: Connection -> FIO<'R, PgError>) (pool: ConnectionPool) : FIO<'R, PgError> =
        fio {
            let! conn = acquireAsync pool
            let closeConn = Connection.closeAsync(conn).CatchAll(fun _ -> FIO.unit())
            return! (action conn).Ensuring(closeConn)
        }

    /// <summary>
    /// Closes and disposes the connection pool.
    /// All connections in the pool will be closed.
    /// </summary>
    /// <param name="pool">The connection pool to close.</param>
    let close (pool: ConnectionPool) : FIO<unit, PgError> =
        FIO.attempt(
            (fun () -> pool.DataSource.Dispose()),
            CloseFailed)

    /// <summary>
    /// Closes and disposes the connection pool asynchronously.
    /// </summary>
    /// <param name="pool">The connection pool to close.</param>
    let closeAsync (pool: ConnectionPool) : FIO<unit, PgError> =
        FIO.awaitTask(pool.DataSource.DisposeAsync().AsTask(), CloseFailed)

    /// <summary>
    /// Gets the connection configuration for the pool.
    /// </summary>
    /// <param name="pool">The connection pool.</param>
    /// <returns>The ConnectionConfig for this pool.</returns>
    let getConfig (pool: ConnectionPool) : ConnectionConfig =
        pool.Config

    /// <summary>
    /// Gets the underlying NpgsqlDataSource.
    /// </summary>
    /// <param name="pool">The connection pool.</param>
    /// <returns>The underlying NpgsqlDataSource.</returns>
    let internal getDataSource (pool: ConnectionPool) : NpgsqlDataSource =
        pool.DataSource
