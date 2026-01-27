namespace FIO.PostgreSQL

open FIO.DSL

open Npgsql

/// <summary>
/// Functions for managing PostgreSQL transactions.
/// </summary>
module Transaction =

    /// <summary>
    /// Runs an action within a transaction.
    /// Automatically commits on success and rolls back on error.
    /// </summary>
    /// <param name="action">The action to run in the transaction.</param>
    /// <param name="conn">The connection to use for the transaction.</param>
    /// <returns>The result of the action.</returns>
    let run (action: Connection -> FIO<'R, PgError>) (conn: Connection) : FIO<'R, PgError> =
        fio {
            let! transaction = Connection.beginTransactionAsync conn
            let disposeTransaction =
                FIO.awaitTask(transaction.DisposeAsync().AsTask(), fun _ -> ())
                    .CatchAll(fun _ -> FIO.unit())
            let! result =
                ((action conn)
                    .FlatMap(fun r ->
                        FIO.awaitTask(transaction.CommitAsync(), fun e -> TransactionFailed("commit", e))
                            .Map(fun () -> r))
                    .CatchAll(fun err ->
                        FIO.awaitTask(transaction.RollbackAsync(), fun e -> TransactionFailed("rollback", e))
                            .FlatMap(fun () -> FIO.fail err)))
                    .Ensuring(disposeTransaction)
            return result
        }

    /// <summary>
    /// Runs an action within a transaction with specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="action">The action to run in the transaction.</param>
    /// <param name="conn">The connection to use for the transaction.</param>
    /// <returns>The result of the action.</returns>
    let runWithIsolation (isolationLevel: FIO.PostgreSQL.IsolationLevel) (action: Connection -> FIO<'R, PgError>) (conn: Connection) : FIO<'R, PgError> =
        fio {
            let! transaction = Connection.beginTransactionAsyncWithIsolation isolationLevel conn
            let disposeTransaction =
                FIO.awaitTask(transaction.DisposeAsync().AsTask(), fun _ -> ())
                    .CatchAll(fun _ -> FIO.unit())
            let! result =
                ((action conn)
                    .FlatMap(fun r ->
                        FIO.awaitTask(transaction.CommitAsync(), fun e -> TransactionFailed("commit", e))
                            .Map(fun () -> r))
                    .CatchAll(fun err ->
                        FIO.awaitTask(transaction.RollbackAsync(), fun e -> TransactionFailed("rollback", e))
                            .FlatMap(fun () -> FIO.fail err)))
                    .Ensuring(disposeTransaction)
            return result
        }

    /// <summary>
    /// Commits a transaction explicitly.
    /// </summary>
    /// <param name="transaction">The transaction to commit.</param>
    let commit (transaction: NpgsqlTransaction) : FIO<unit, PgError> =
        FIO.awaitTask(transaction.CommitAsync(), fun e -> TransactionFailed("commit", e))

    /// <summary>
    /// Rolls back a transaction explicitly.
    /// </summary>
    /// <param name="transaction">The transaction to roll back.</param>
    let rollback (transaction: NpgsqlTransaction) : FIO<unit, PgError> =
        FIO.awaitTask(transaction.RollbackAsync(), fun e -> TransactionFailed("rollback", e))

    /// <summary>
    /// Creates a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint.</param>
    /// <param name="transaction">The transaction to create the savepoint in.</param>
    let save (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, PgError> =
        FIO.awaitTask(transaction.SaveAsync(savepointName), fun e -> TransactionFailed("save", e))

    /// <summary>
    /// Rolls back to a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint to roll back to.</param>
    /// <param name="transaction">The transaction containing the savepoint.</param>
    let rollbackTo (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, PgError> =
        FIO.awaitTask(transaction.RollbackAsync(savepointName), fun e -> TransactionFailed("rollbackTo", e))

    /// <summary>
    /// Releases a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint to release.</param>
    /// <param name="transaction">The transaction containing the savepoint.</param>
    let release (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, PgError> =
        FIO.awaitTask(transaction.ReleaseAsync(savepointName), fun e -> TransactionFailed("release", e))

    /// <summary>
    /// Runs an action with a savepoint.
    /// Automatically releases the savepoint on success and rolls back to it on error.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint.</param>
    /// <param name="transaction">The transaction to use.</param>
    /// <param name="action">The action to run with the savepoint.</param>
    /// <returns>The result of the action.</returns>
    let withSavepoint (savepointName: string) (transaction: NpgsqlTransaction) (action: FIO<'R, PgError>) : FIO<'R, PgError> =
        fio {
            do! save savepointName transaction
            let! result =
                action
                    .FlatMap(fun r ->
                        (release savepointName transaction).Map(fun () -> r))
                    .CatchAll(fun err ->
                        (rollbackTo savepointName transaction).FlatMap(fun () -> FIO.fail err))
            return result
        }
