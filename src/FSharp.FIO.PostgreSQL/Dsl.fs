/// <summary>
/// High-level DSL functions for FIO PostgreSQL integration.
/// </summary>
namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

/// <summary>
/// High-level DSL functions that combine connection pool, query, and transaction operations.
/// </summary>
[<AutoOpen>]
module Dsl =

    /// <summary>
    /// Operator for creating SQL parameters.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="value">The parameter value.</param>
    let inline (@=) name value = SqlParameter.create name value

    /// <summary>
    /// Executes a query with the connection pool, automatically managing the connection.
    /// </summary>
    /// <param name="sql">The SQL query.</param>
    /// <param name="mapper">Function to map each row.</param>
    /// <param name="pool">The connection pool.</param>
    let query sql mapper pool =
        Pool.withConnectionAsync
            (fun conn -> Query.query sql mapper conn)
            pool

    /// <summary>
    /// Executes a parameterized query with the connection pool.
    /// </summary>
    /// <param name="sql">The SQL query.</param>
    /// <param name="parameters">The query parameters.</param>
    /// <param name="mapper">Function to map each row.</param>
    /// <param name="pool">The connection pool.</param>
    let queryWithParams sql parameters mapper pool =
        Pool.withConnectionAsync
            (fun conn -> Query.queryWithParams sql parameters mapper conn)
            pool

    /// <summary>
    /// Executes a query and returns the first result.
    /// </summary>
    /// <param name="sql">The SQL query.</param>
    /// <param name="mapper">Function to map the row.</param>
    /// <param name="pool">The connection pool.</param>
    let queryFirst sql mapper pool =
        Pool.withConnectionAsync
            (fun conn -> Query.queryFirst sql mapper conn)
            pool

    /// <summary>
    /// Executes a parameterized query and returns the first result.
    /// </summary>
    /// <param name="sql">The SQL query.</param>
    /// <param name="parameters">The query parameters.</param>
    /// <param name="mapper">Function to map the row.</param>
    /// <param name="pool">The connection pool.</param>
    let queryFirstWithParams sql parameters mapper pool =
        Pool.withConnectionAsync
            (fun conn -> Query.queryFirstWithParams sql parameters mapper conn)
            pool

    /// <summary>
    /// Executes a command with the connection pool.
    /// </summary>
    /// <param name="sql">The SQL command.</param>
    /// <param name="pool">The connection pool.</param>
    let execute sql pool =
        Pool.withConnectionAsync
            (fun conn -> Command.execute sql conn)
            pool

    /// <summary>
    /// Executes a parameterized command with the connection pool.
    /// </summary>
    /// <param name="sql">The SQL command.</param>
    /// <param name="parameters">The command parameters.</param>
    /// <param name="pool">The connection pool.</param>
    let executeWithParams sql parameters pool =
        Pool.withConnectionAsync
            (fun conn -> Command.executeWithParams sql parameters conn)
            pool

    /// <summary>
    /// Executes an INSERT command with RETURNING clause.
    /// </summary>
    /// <param name="sql">The INSERT SQL with RETURNING clause.</param>
    /// <param name="parameters">The command parameters.</param>
    /// <param name="pool">The connection pool.</param>
    let insertReturning<'T> sql parameters pool =
        Pool.withConnectionAsync
            (fun conn -> Command.insertReturningWithParams<'T> sql parameters conn)
            pool

    /// <summary>
    /// Runs an action within a transaction with the connection pool.
    /// </summary>
    /// <param name="action">The action to run in the transaction.</param>
    /// <param name="pool">The connection pool.</param>
    let transaction action pool =
        Pool.withConnectionAsync
            (fun conn -> Transaction.run action conn)
            pool

    /// <summary>
    /// Runs an action within a transaction with specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The transaction isolation level.</param>
    /// <param name="action">The action to run in the transaction.</param>
    /// <param name="pool">The connection pool.</param>
    let transactionWithIsolation isolationLevel action pool =
        Pool.withConnectionAsync
            (fun conn -> Transaction.runWithIsolation isolationLevel action conn)
            pool
