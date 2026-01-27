namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

/// <summary>
/// Functions for executing PostgreSQL commands (non-query operations).
/// </summary>
module Command =

    /// <summary>
    /// Executes a SQL command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The SQL command to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of affected rows.</returns>
    let execute (sql: string) (conn: Connection) : FIO<int, PgError> =
        fio {
            let cmd = Connection.createCommand sql conn
            let disposeCmd = FIO.attempt((fun () -> cmd.Dispose()), fun _ -> ()).CatchAll(fun _ -> FIO.unit())
            return! FIO.awaitGenericTask(cmd.ExecuteNonQueryAsync(), fun e -> CommandFailed(sql, e))
                        .Ensuring(disposeCmd)
        }

    /// <summary>
    /// Executes a SQL command with parameters and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The SQL command to execute.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of affected rows.</returns>
    let executeWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, PgError> =
        fio {
            let cmd = Connection.createCommandWithParams sql parameters conn
            let disposeCmd = FIO.attempt((fun () -> cmd.Dispose()), fun _ -> ()).CatchAll(fun _ -> FIO.unit())
            return! FIO.awaitGenericTask(cmd.ExecuteNonQueryAsync(), fun e -> CommandFailed(sql, e))
                        .Ensuring(disposeCmd)
        }

    /// <summary>
    /// Executes an INSERT command and returns the number of inserted rows.
    /// </summary>
    /// <param name="sql">The INSERT SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of inserted rows.</returns>
    let insert (sql: string) (conn: Connection) : FIO<int, PgError> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized INSERT command and returns the number of inserted rows.
    /// </summary>
    /// <param name="sql">The INSERT SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of inserted rows.</returns>
    let insertWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, PgError> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes an INSERT command that returns a scalar value (e.g., RETURNING id).
    /// </summary>
    /// <param name="sql">The INSERT SQL command with RETURNING clause.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The returned scalar value.</returns>
    let insertReturning<'T> (sql: string) (conn: Connection) : FIO<'T, PgError> =
        Query.executeScalar<'T> sql conn

    /// <summary>
    /// Executes a parameterized INSERT command that returns a scalar value.
    /// </summary>
    /// <param name="sql">The INSERT SQL command with RETURNING clause.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The returned scalar value.</returns>
    let insertReturningWithParams<'T> (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<'T, PgError> =
        Query.executeScalarWithParams<'T> sql parameters conn

    /// <summary>
    /// Executes an UPDATE command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The UPDATE SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of affected rows.</returns>
    let update (sql: string) (conn: Connection) : FIO<int, PgError> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized UPDATE command and returns the number of affected rows.
    /// </summary>
    /// <param name="sql">The UPDATE SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of affected rows.</returns>
    let updateWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, PgError> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes a DELETE command and returns the number of deleted rows.
    /// </summary>
    /// <param name="sql">The DELETE SQL command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of deleted rows.</returns>
    let delete (sql: string) (conn: Connection) : FIO<int, PgError> =
        execute sql conn

    /// <summary>
    /// Executes a parameterized DELETE command and returns the number of deleted rows.
    /// </summary>
    /// <param name="sql">The DELETE SQL command.</param>
    /// <param name="parameters">The parameters for the command.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The number of deleted rows.</returns>
    let deleteWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<int, PgError> =
        executeWithParams sql parameters conn

    /// <summary>
    /// Executes multiple commands in a batch and returns the total number of affected rows.
    /// </summary>
    /// <param name="commands">List of SQL commands to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The total number of affected rows.</returns>
    let executeBatch (commands: string list) (conn: Connection) : FIO<int, PgError> =
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
    /// <returns>The total number of affected rows.</returns>
    let executeBatchWithParams (commands: (string * SqlParameter list) list) (conn: Connection) : FIO<int, PgError> =
        fio {
            let mutable total = 0
            for (sql, parameters) in commands do
                let! affected = executeWithParams sql parameters conn
                total <- total + affected
            return total
        }
