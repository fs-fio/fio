namespace FIO.PostgreSQL

open FIO.DSL

open Npgsql
open System.Data

/// <summary>
/// Functions for working with PostgreSQL connections.
/// </summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

    /// <summary>
    /// Creates a connection from an NpgsqlConnection and pool.
    /// </summary>
    /// <param name="npgsqlConn">The underlying NpgsqlConnection.</param>
    /// <param name="pool">The connection pool this connection belongs to.</param>
    /// <returns>A new Connection wrapping the NpgsqlConnection.</returns>
    let internal create (npgsqlConn: NpgsqlConnection) (pool: ConnectionPool) : Connection =
        {
            NpgsqlConnection = npgsqlConn
            Pool = pool
        }

    /// <summary>
    /// Gets the underlying NpgsqlConnection.
    /// </summary>
    /// <param name="conn">The connection to get the underlying connection from.</param>
    /// <returns>The underlying NpgsqlConnection.</returns>
    let internal getNpgsqlConnection (conn: Connection) : NpgsqlConnection =
        conn.NpgsqlConnection

    /// <summary>
    /// Opens the connection if not already open.
    /// </summary>
    /// <param name="conn">The connection to open.</param>
    let open' (conn: Connection) : FIO<unit, PgError> =
        FIO.attempt(
            (fun () ->
                if conn.NpgsqlConnection.State <> ConnectionState.Open then
                    conn.NpgsqlConnection.Open()),
            OpenFailed)

    /// <summary>
    /// Opens the connection asynchronously if not already open.
    /// </summary>
    /// <param name="conn">The connection to open.</param>
    let openAsync (conn: Connection) : FIO<unit, PgError> =
        fio {
            if conn.NpgsqlConnection.State <> ConnectionState.Open then
                do! FIO.awaitTask(conn.NpgsqlConnection.OpenAsync(), OpenFailed)
        }

    /// <summary>
    /// Closes the connection.
    /// </summary>
    /// <param name="conn">The connection to close.</param>
    let close (conn: Connection) : FIO<unit, PgError> =
        FIO.attempt(
            (fun () ->
                if conn.NpgsqlConnection.State <> ConnectionState.Closed then
                    conn.NpgsqlConnection.Close()),
            CloseFailed)

    /// <summary>
    /// Closes the connection asynchronously.
    /// </summary>
    /// <param name="conn">The connection to close.</param>
    let closeAsync (conn: Connection) : FIO<unit, PgError> =
        fio {
            if conn.NpgsqlConnection.State <> ConnectionState.Closed then
                do! FIO.awaitTask(conn.NpgsqlConnection.CloseAsync(), CloseFailed)
        }

    /// <summary>
    /// Executes an action with the connection, ensuring it's opened and closed properly.
    /// </summary>
    /// <param name="action">The action to execute with the connection.</param>
    /// <param name="conn">The connection to use.</param>
    /// <returns>The result of the action.</returns>
    let withConnection (action: Connection -> FIO<'R, PgError>) (conn: Connection) : FIO<'R, PgError> =
        fio {
            do! openAsync conn
            let! result = action conn
            do! closeAsync conn
            return result
        }

    /// <summary>
    /// Begins a new transaction on the connection.
    /// </summary>
    /// <param name="conn">The connection to start a transaction on.</param>
    /// <returns>A new NpgsqlTransaction.</returns>
    let beginTransaction (conn: Connection) : FIO<NpgsqlTransaction, PgError> =
        FIO.attempt(
            (fun () -> conn.NpgsqlConnection.BeginTransaction()),
            fun e -> TransactionFailed("begin", e))

    /// <summary>
    /// Begins a new transaction with the specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="conn">The connection to start a transaction on.</param>
    /// <returns>A new NpgsqlTransaction with the specified isolation level.</returns>
    let beginTransactionWithIsolation (isolationLevel: FIO.PostgreSQL.IsolationLevel) (conn: Connection) : FIO<NpgsqlTransaction, PgError> =
        FIO.attempt(
            (fun () ->
                let sysLevel = FIO.PostgreSQL.IsolationLevel.toSystemIsolationLevel isolationLevel
                conn.NpgsqlConnection.BeginTransaction(sysLevel)),
            fun e -> TransactionFailed("begin", e))

    /// <summary>
    /// Begins a new transaction asynchronously.
    /// </summary>
    /// <param name="conn">The connection to start a transaction on.</param>
    /// <returns>A new NpgsqlTransaction.</returns>
    let beginTransactionAsync (conn: Connection) : FIO<NpgsqlTransaction, PgError> =
        FIO.awaitGenericTask(conn.NpgsqlConnection.BeginTransactionAsync().AsTask(), fun e -> TransactionFailed("begin", e))

    /// <summary>
    /// Begins a new transaction asynchronously with the specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="conn">The connection to start a transaction on.</param>
    /// <returns>A new NpgsqlTransaction with the specified isolation level.</returns>
    let beginTransactionAsyncWithIsolation (isolationLevel: FIO.PostgreSQL.IsolationLevel) (conn: Connection) : FIO<NpgsqlTransaction, PgError> =
        let sysLevel = FIO.PostgreSQL.IsolationLevel.toSystemIsolationLevel isolationLevel
        FIO.awaitGenericTask(conn.NpgsqlConnection.BeginTransactionAsync(sysLevel).AsTask(), fun e -> TransactionFailed("begin", e))

    /// <summary>
    /// Creates a command for the connection.
    /// </summary>
    /// <param name="sql">The SQL command text.</param>
    /// <param name="conn">The connection to create the command for.</param>
    /// <returns>A new NpgsqlCommand with the specified SQL.</returns>
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
    /// <returns>A new NpgsqlCommand with the specified SQL and parameters.</returns>
    let createCommandWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : NpgsqlCommand =
        let cmd = createCommand sql conn
        for param in parameters do
            cmd.Parameters.AddWithValue(param.Name, param.Value) |> ignore
        cmd

    /// <summary>
    /// Checks if the connection is open.
    /// </summary>
    /// <param name="conn">The connection to check.</param>
    /// <returns>True if the connection is open, false otherwise.</returns>
    let isOpen (conn: Connection) : bool =
        conn.NpgsqlConnection.State = ConnectionState.Open

    /// <summary>
    /// Gets the connection state.
    /// </summary>
    /// <param name="conn">The connection to get the state of.</param>
    /// <returns>The current ConnectionState of the connection.</returns>
    let getState (conn: Connection) : ConnectionState =
        conn.NpgsqlConnection.State
