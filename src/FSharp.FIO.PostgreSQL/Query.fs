/// <summary>
/// PostgreSQL query execution for SELECT statements.
/// </summary>
namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

open Npgsql
open System

/// <summary>
/// Functions for executing PostgreSQL queries.
/// </summary>
module Query =

    /// <summary>
    /// Executes a SQL query and returns a result set.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    let execute (sql: string) (conn: Connection) : FIO<ResultSet, exn> =
        fio {
            let cmd = Conn.createCommand sql conn
            let! reader = FIO.AwaitTask(cmd.ExecuteReaderAsync(), id)
            return Results.create (reader :?> NpgsqlDataReader)
        }

    /// <summary>
    /// Executes a SQL query with parameters and returns a result set.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeWithParams (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<ResultSet, exn> =
        fio {
            let cmd = Conn.createCommandWithParams sql parameters conn
            let! reader = FIO.AwaitTask(cmd.ExecuteReaderAsync(), id)
            return Results.create (reader :?> NpgsqlDataReader)
        }

    /// <summary>
    /// Executes a query and maps all rows to a list of values.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map each row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    let query (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T list, exn> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readAll mapper resultSet
            let! _ = Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a parameterized query and maps all rows to a list of values.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="mapper">Function to map each row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    let queryWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T list, exn> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readAll mapper resultSet
            let! _ = Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a query and returns the first row, if any.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    let queryFirst (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T option, exn> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readFirst mapper resultSet
            let! _ = Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a parameterized query and returns the first row, if any.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    let queryFirstWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T option, exn> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readFirst mapper resultSet
            let! _ = Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a query and returns a single row.
    /// Fails if there are zero or more than one row.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="conn">The connection to execute on.</param>
    let querySingle (sql: string) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T, exn> =
        fio {
            let! resultSet = execute sql conn
            let! result = Results.readSingle mapper resultSet
            let! _ = Results.close resultSet
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
    let querySingleWithParams (sql: string) (parameters: SqlParameter list) (mapper: ResultSet -> 'T) (conn: Connection) : FIO<'T, exn> =
        fio {
            let! resultSet = executeWithParams sql parameters conn
            let! result = Results.readSingle mapper resultSet
            let! _ = Results.close resultSet
            return result
        }

    /// <summary>
    /// Executes a scalar query returning a single value.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeScalar<'T> (sql: string) (conn: Connection) : FIO<'T, exn> =
        fio {
            let cmd = Conn.createCommand sql conn
            let! result = FIO.AwaitTask(cmd.ExecuteScalarAsync(), id)
            return
                if isNull result then
                    Unchecked.defaultof<'T>
                else
                    result :?> 'T
        }

    /// <summary>
    /// Executes a parameterized scalar query returning a single value.
    /// </summary>
    /// <param name="sql">The SQL query to execute.</param>
    /// <param name="parameters">The parameters for the query.</param>
    /// <param name="conn">The connection to execute on.</param>
    let executeScalarWithParams<'T> (sql: string) (parameters: SqlParameter list) (conn: Connection) : FIO<'T, exn> =
        fio {
            let cmd = Conn.createCommandWithParams sql parameters conn
            let! result = FIO.AwaitTask(cmd.ExecuteScalarAsync(), id)
            return
                if isNull result then
                    Unchecked.defaultof<'T>
                else
                    result :?> 'T
        }
