namespace FIO.PostgreSQL

open FIO.DSL

open Npgsql

/// <summary>
/// Functions for executing PostgreSQL queries.
/// </summary>
module Query =

    /// <summary>
    /// Executes a SQL query and returns a result set.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>A ResultSet containing the query results.</returns>
    let execute (sql: string) (conn: Connection) : FIO<ResultSet, PgError> =
        FIO.awaitGenericTask(
            (Connection.createCommand sql conn).ExecuteReaderAsync(),
            fun e -> QueryFailed(sql, e))
            .Map(fun reader -> Results.create (reader :?> NpgsqlDataReader))

    /// <summary>
    /// Executes a SQL query with parameters and returns a result set.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>A ResultSet containing the query results.</returns>
    let executeWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<ResultSet, PgError> =
        FIO.awaitGenericTask(
            (Connection.createCommandWithParams sql parameters conn).ExecuteReaderAsync(),
            fun e -> QueryFailed(sql, e))
            .Map(fun reader -> Results.create (reader :?> NpgsqlDataReader))

    /// <summary>
    /// Executes a query and maps all rows to a list of values.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map each row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>A list of mapped values.</returns>
    let query (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T list, PgError> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readAll mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a parameterized query and maps all rows to a list of values.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="mapper">Function to map each row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>A list of mapped values.</returns>
    let queryWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T list, PgError> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readAll mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a query and returns the first row, if any.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>Some mapped value if row exists, None otherwise.</returns>
    let queryFirst (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T option, PgError> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readFirst mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a parameterized query and returns the first row, if any.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>Some mapped value if row exists, None otherwise.</returns>
    let queryFirstWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T option, PgError> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readFirst mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a query and returns a single row.
    /// Fails if there are zero or more than one row.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The mapped value from the single row.</returns>
    let querySingle (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T, PgError> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readSingle mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a parameterized query and returns a single row.
    /// Fails if there are zero or more than one row.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The mapped value from the single row.</returns>
    let querySingleWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T, PgError> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readSingle mapper resultSet
            do! Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a scalar query returning a single value.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The scalar value from the query.</returns>
    let executeScalar<'T> (sql: string) (conn: Connection) : FIO<'T, PgError> =
        fio {
            let cmd = Connection.createCommand sql conn
            let disposeCmd = FIO.attempt((fun () -> cmd.Dispose()), fun _ -> ()).CatchAll(fun _ -> FIO.unit())
            return! FIO.awaitGenericTask(cmd.ExecuteScalarAsync(), fun e -> QueryFailed(sql, e))
                        .Map(fun result ->
                            if isNull result then Unchecked.defaultof<'T>
                            else result :?> 'T)
                        .Ensuring(disposeCmd)
        }

    /// <summary>
    /// Executes a parameterized scalar query returning a single value.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="conn">The connection to execute on.</param>
    /// <returns>The scalar value from the query.</returns>
    let executeScalarWithParams<'T> (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<'T, PgError> =
        fio {
            let cmd = Connection.createCommandWithParams sql parameters conn
            let disposeCmd = FIO.attempt((fun () -> cmd.Dispose()), fun _ -> ()).CatchAll(fun _ -> FIO.unit())
            return! FIO.awaitGenericTask(cmd.ExecuteScalarAsync(), fun e -> QueryFailed(sql, e))
                        .Map(fun result ->
                            if isNull result then Unchecked.defaultof<'T>
                            else result :?> 'T)
                        .Ensuring(disposeCmd)
        }
