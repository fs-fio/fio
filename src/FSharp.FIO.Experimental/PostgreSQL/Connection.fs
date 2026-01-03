/// <summary>
/// PostgreSQL connection operations.
/// </summary>
namespace FSharp.FIO.Experimental.PostgreSQL

open FSharp.FIO.DSL

open Npgsql
open System
open System.Data

/// <summary>
/// Functions for working with PostgreSQL connections.
/// </summary>
[<RequireQualifiedAccess>]
module Conn =

    /// <summary>
    /// Creates a connection from an NpgsqlConnection and pool.
    /// </summary>
    /// <param name="npgsqlConn">The underlying NpgsqlConnection.</param>
    /// <param name="pool">The connection pool this connection belongs to.</param>
    let internal create (npgsqlConn: NpgsqlConnection) (pool: ConnectionPool) : Connection =
        {
            NpgsqlConnection = npgsqlConn
            Pool = pool
        }

    /// <summary>
    /// Gets the underlying NpgsqlConnection.
    /// </summary>
    /// <param name="conn">The connection to get the underlying connection from.</param>
    let internal getNpgsqlConnection (conn: Connection) : NpgsqlConnection =
        conn.NpgsqlConnection

    /// <summary>
    /// Opens the connection if not already open.
    /// </summary>
    /// <param name="conn">The connection to open.</param>
    let open' (conn: Connection) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                if conn.NpgsqlConnection.State <> ConnectionState.Open then
                    conn.NpgsqlConnection.Open()),
            id)

    /// <summary>
    /// Opens the connection asynchronously if not already open.
    /// </summary>
    /// <param name="conn">The connection to open.</param>
    let openAsync (conn: Connection) : FIO<unit, exn> =
        fio {
            if conn.NpgsqlConnection.State <> ConnectionState.Open then
                do! FIO.AwaitTask(conn.NpgsqlConnection.OpenAsync(), id)
        }

    /// <summary>
    /// Closes the connection.
    /// </summary>
    /// <param name="conn">The connection to close.</param>
    let close (conn: Connection) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                if conn.NpgsqlConnection.State <> ConnectionState.Closed then
                    conn.NpgsqlConnection.Close()),
            id)

    /// <summary>
    /// Closes the connection asynchronously.
    /// </summary>
    /// <param name="conn">The connection to close.</param>
    let closeAsync (conn: Connection) : FIO<unit, exn> =
        fio {
            if conn.NpgsqlConnection.State <> ConnectionState.Closed then
                do! FIO.AwaitTask(conn.NpgsqlConnection.CloseAsync(), id)
        }

    /// <summary>
    /// Executes an action with the connection, ensuring it's opened and closed properly.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="conn">The connection to use.</param>
    let withConnection (action: Connection -> FIO<'R, 'E>) (conn: Connection) : FIO<'R, 'E> =
        fio {
            let! _ = (openAsync conn).MapError (fun e -> Unchecked.defaultof<'E>)
            let! result = action conn
            let! _ = (closeAsync conn).MapError (fun _ -> Unchecked.defaultof<'E>)
            return result
        }

    /// <summary>
    /// Begins a new transaction on the connection.
    /// </summary>
    /// <param name="conn">The connection to start a transaction on.</param>
    let beginTransaction (conn: Connection) : FIO<NpgsqlTransaction, exn> =
        FIO.Attempt(
            (fun () -> conn.NpgsqlConnection.BeginTransaction()),
            id)

    /// <summary>
    /// Begins a new transaction with the specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="conn">The connection to start a transaction on.</param>
    let beginTransactionWithIsolation (isolationLevel: FSharp.FIO.Experimental.PostgreSQL.IsolationLevel) (conn: Connection) : FIO<NpgsqlTransaction, exn> =
        FIO.Attempt(
            (fun () ->
                let sysLevel = FSharp.FIO.Experimental.PostgreSQL.IsolationLevel.toSystemIsolationLevel isolationLevel
                conn.NpgsqlConnection.BeginTransaction(sysLevel)),
            id)

    /// <summary>
    /// Begins a new transaction asynchronously.
    /// </summary>
    /// <param name="conn">The connection to start a transaction on.</param>
    let beginTransactionAsync (conn: Connection) : FIO<NpgsqlTransaction, exn> =
        fio {
            return! FIO.AwaitTask(conn.NpgsqlConnection.BeginTransactionAsync().AsTask(), id)
        }

    /// <summary>
    /// Begins a new transaction asynchronously with the specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="conn">The connection to start a transaction on.</param>
    let beginTransactionAsyncWithIsolation (isolationLevel: FSharp.FIO.Experimental.PostgreSQL.IsolationLevel) (conn: Connection) : FIO<NpgsqlTransaction, exn> =
        fio {
            let sysLevel = FSharp.FIO.Experimental.PostgreSQL.IsolationLevel.toSystemIsolationLevel isolationLevel
            return! FIO.AwaitTask(conn.NpgsqlConnection.BeginTransactionAsync(sysLevel).AsTask(), id)
        }

    /// <summary>
    /// Creates a command for the connection.
    /// </summary>
    /// <param name="sql">The SQL command text.</param>
    /// <param name="conn">The connection to create the command for.</param>
    let createCommand (sql: string) (conn: Connection) : NpgsqlCommand =
        let cmd = conn.NpgsqlConnection.CreateCommand()
        cmd.CommandText <- sql
        cmd

    /// <summary>
    /// Creates a command with parameters.
    /// </summary>
    /// <param name="sql">The SQL command text.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to create the command for.</param>
    let createCommandWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : NpgsqlCommand =
        let cmd = createCommand sql conn
        for param in parameters do
            cmd.Parameters.AddWithValue(param.Name, param.Value) |> ignore
        cmd

    /// <summary>
    /// Checks if the connection is open.
    /// </summary>
    /// <param name="conn">The connection to check.</param>
    let isOpen (conn: Connection) : bool =
        conn.NpgsqlConnection.State = ConnectionState.Open

    /// <summary>
    /// Gets the connection state.
    /// </summary>
    /// <param name="conn">The connection to get the state of.</param>
    let getState (conn: Connection) : ConnectionState =
        conn.NpgsqlConnection.State
