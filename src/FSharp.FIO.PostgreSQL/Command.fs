/// <summary>
/// PostgreSQL command execution for INSERT, UPDATE, DELETE statements.
/// </summary>
namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

open Npgsql
open System

/// <summary>
/// Functions for executing PostgreSQL commands (non-query operations).
/// </summary>
module Command =

    /// <summary>
    /// Executes a SQL command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The SQL command to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    let execute (sql: string) (conn: Connection) : FIO<int, exn> =
        fio {
            let cmd = Conn.createCommand sql conn
            return! FIO.AwaitTask(cmd.ExecuteNonQueryAsync(), id)
        }

    /// <summary>
    /// Executes a SQL command with parameters and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The SQL command to execute.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, exn> =
        fio {
            let cmd = Conn.createCommandWithParams sql parameters conn
            return! FIO.AwaitTask(cmd.ExecuteNonQueryAsync(), id)
        }

    /// <summary>
    /// Executes an INSERT command and returns the number of inserted rows.
    /// </summary>
    /// <param name="sql">The INSERT SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let insert (sql: string) (conn: Connection) : FIO<int, exn> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized INSERT command and returns the number of inserted rows.
    /// </summary>
    /// <param name="sql">The INSERT SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let insertWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, exn> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes an INSERT command that returns a scalar value (e.g., RETURNING id).
    /// </summary>
    /// <param name="sql">The INSERT SQL command with RETURNING clause.</param>
    /// <param name="conn">The connection to execute on.</param>
    let insertReturning<'T> (sql: string) (conn: Connection) : FIO<'T, exn> =
        Query.executeScalar<'T> sql conn

    /// <summary>
    /// Executes a parameterized INSERT command that returns a scalar value.
    /// </summary>
    /// <param name="sql">The INSERT SQL command with RETURNING clause.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let insertReturningWithParams<'T> (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<'T, exn> =
        Query.executeScalarWithParams<'T> sql parameters conn

    /// <summary>
    /// Executes an UPDATE command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The UPDATE SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let update (sql: string) (conn: Connection) : FIO<int, exn> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized UPDATE command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The UPDATE SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let updateWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, exn> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes a DELETE command and returns the number of deleted rows.
    /// </summary>
    /// <param name="sql">The DELETE SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let delete (sql: string) (conn: Connection) : FIO<int, exn> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized DELETE command and returns the number of deleted rows.
    /// </summary>
    /// <param name="sql">The DELETE SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    let deleteWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, exn> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes multiple commands in a batch and returns the total number of affected rows.
    /// </summary>
    /// <param name="commands">List of SQL commands to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeBatch (commands: string list) (conn: Connection) : FIO<int, exn> =
        fio {
            let mutable total = 0
            for cmd in commands do
                let! affected = execute cmd conn
                total <- total + affected
            return total
        }

    /// <summary>
    /// Executes multiple parameterized commands in a batch.
    /// </summary>
    /// <param name="commands">List of (SQL, parameters) tuples.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeBatchWithParams (commands: (string * SqlParameter list) list) (conn: Connection) : FIO<int, exn> =
        fio {
            let mutable total = 0
            for (sql, parameters) in commands do
                let! affected = executeWithParams sql parameters conn
                total <- total + affected
            return total
        }
