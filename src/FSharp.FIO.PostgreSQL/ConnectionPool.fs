/// <summary>
/// PostgreSQL connection pool management using NpgsqlDataSource.
/// </summary>
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
    let create (config: ConnectionConfig) : FIO<ConnectionPool, exn> =
        FIO.Attempt(
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
            id)

    /// <summary>
    /// Acquires a connection from the pool.
    /// The connection should be released by calling Connection.close or using withConnection.
    /// </summary>
    /// <param name="pool">The connection pool to acquire from.</param>
    let acquire (pool: ConnectionPool) : FIO<Connection, exn> =
        fio {
            let! npgsqlConn =
                FIO.Attempt(
                    (fun () -> pool.DataSource.CreateConnection()),
                    id)

            return Conn.create npgsqlConn pool
        }

    /// <summary>
    /// Acquires a connection from the pool asynchronously.
    /// </summary>
    /// <param name="pool">The connection pool to acquire from.</param>
    let acquireAsync (pool: ConnectionPool) : FIO<Connection, exn> =
        fio {
            let! npgsqlConn =
                FIO.AwaitTask(pool.DataSource.OpenConnectionAsync().AsTask(), id)

            return Conn.create npgsqlConn pool
        }

    /// <summary>
    /// Executes an action with a connection from the pool.
    /// The connection is automatically opened and closed.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="pool">The connection pool to acquire from.</param>
    let withConnection (action: Connection -> FIO<'R, 'E>) (pool: ConnectionPool) : FIO<'R, 'E> =
        fio {
            let! conn = (acquire pool).MapError (fun e -> Unchecked.defaultof<'E>)
            let! _ = (Conn.openAsync conn).MapError (fun e -> Unchecked.defaultof<'E>)
            let! result = action conn
            let! _ = (Conn.closeAsync conn).MapError (fun _ -> Unchecked.defaultof<'E>)
            return result
        }

    /// <summary>
    /// Executes an action with a connection from the pool asynchronously.
    /// The connection is automatically acquired and released.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="pool">The connection pool to acquire from.</param>
    let withConnectionAsync (action: Connection -> FIO<'R, 'E>) (pool: ConnectionPool) : FIO<'R, 'E> =
        fio {
            let! conn = (acquireAsync pool).MapError (fun e -> Unchecked.defaultof<'E>)
            let! result = action conn
            let! _ = (Conn.closeAsync conn).MapError (fun _ -> Unchecked.defaultof<'E>)
            return result
        }

    /// <summary>
    /// Closes and disposes the connection pool.
    /// All connections in the pool will be closed.
    /// </summary>
    /// <param name="pool">The connection pool to close.</param>
    let close (pool: ConnectionPool) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                pool.DataSource.Dispose()),
            id)

    /// <summary>
    /// Closes and disposes the connection pool asynchronously.
    /// </summary>
    /// <param name="pool">The connection pool to close.</param>
    let closeAsync (pool: ConnectionPool) : FIO<unit, exn> =
        fio {
            return! FIO.AwaitTask(pool.DataSource.DisposeAsync().AsTask(), id)
        }

    /// <summary>
    /// Gets the connection configuration for the pool.
    /// </summary>
    /// <param name="pool">The connection pool.</param>
    let getConfig (pool: ConnectionPool) : ConnectionConfig =
        pool.Config

    /// <summary>
    /// Gets the underlying NpgsqlDataSource.
    /// </summary>
    /// <param name="pool">The connection pool.</param>
    let internal getDataSource (pool: ConnectionPool) : NpgsqlDataSource =
        pool.DataSource
