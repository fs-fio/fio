/// <summary>
/// PostgreSQL transaction management with FIO effects.
/// </summary>
namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

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
    let run (action: Connection -> FIO<'R, 'E>) (conn: Connection) : FIO<'R, 'E> =
        fio {
            // Begin transaction
            let! transaction =
                (Conn.beginTransactionAsync conn).MapError (fun e -> Unchecked.defaultof<'E>)

            try
                // Execute action
                let! result = action conn

                // Commit transaction
                let! _ = (FIO.AwaitTask(transaction.CommitAsync(), id)).MapError (fun e -> Unchecked.defaultof<'E>)

                return result
            with
            | ex ->
                // Rollback on error
                let! _ = (FIO.AwaitTask(transaction.RollbackAsync(), id)).MapError (fun _ -> Unchecked.defaultof<'E>)

                return! FIO.Fail (Unchecked.defaultof<'E>)
        }

    /// <summary>
    /// Runs an action within a transaction with specified isolation level.
    /// </summary>
    /// <param name="isolationLevel">The isolation level for the transaction.</param>
    /// <param name="action">The action to run in the transaction.</param>
    /// <param name="conn">The connection to use for the transaction.</param>
    let runWithIsolation (isolationLevel: FSharp.FIO.PostgreSQL.IsolationLevel) (action: Connection -> FIO<'R, 'E>) (conn: Connection) : FIO<'R, 'E> =
        fio {
            // Begin transaction with isolation level
            let! transaction =
                (Conn.beginTransactionAsyncWithIsolation isolationLevel conn).MapError (fun e -> Unchecked.defaultof<'E>)

            try
                // Execute action
                let! result = action conn

                // Commit transaction
                let! _ = (FIO.AwaitTask(transaction.CommitAsync(), id)).MapError (fun e -> Unchecked.defaultof<'E>)

                return result
            with
            | ex ->
                // Rollback on error
                let! _ = (FIO.AwaitTask(transaction.RollbackAsync(), id)).MapError (fun _ -> Unchecked.defaultof<'E>)

                return! FIO.Fail (Unchecked.defaultof<'E>)
        }

    /// <summary>
    /// Commits a transaction explicitly.
    /// </summary>
    /// <param name="transaction">The transaction to commit.</param>
    let commit (transaction: NpgsqlTransaction) : FIO<unit, exn> =
        FIO.AwaitTask(transaction.CommitAsync(), id)

    /// <summary>
    /// Rolls back a transaction explicitly.
    /// </summary>
    /// <param name="transaction">The transaction to roll back.</param>
    let rollback (transaction: NpgsqlTransaction) : FIO<unit, exn> =
        FIO.AwaitTask(transaction.RollbackAsync(), id)

    /// <summary>
    /// Creates a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint.</param>
    /// <param name="transaction">The transaction to create the savepoint in.</param>
    let save (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, exn> =
        FIO.AwaitTask(transaction.SaveAsync(savepointName), id)

    /// <summary>
    /// Rolls back to a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint to roll back to.</param>
    /// <param name="transaction">The transaction containing the savepoint.</param>
    let rollbackTo (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, exn> =
        FIO.AwaitTask(transaction.RollbackAsync(savepointName), id)

    /// <summary>
    /// Releases a savepoint within a transaction.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint to release.</param>
    /// <param name="transaction">The transaction containing the savepoint.</param>
    let release (savepointName: string) (transaction: NpgsqlTransaction) : FIO<unit, exn> =
        FIO.AwaitTask(transaction.ReleaseAsync(savepointName), id)

    /// <summary>
    /// Runs an action with a savepoint.
    /// Automatically releases the savepoint on success and rolls back to it on error.
    /// </summary>
    /// <param name="savepointName">The name of the savepoint.</param>
    /// <param name="transaction">The transaction to use.</param>
    /// <param name="action">The action to run with the savepoint.</param>
    let withSavepoint (savepointName: string) (transaction: NpgsqlTransaction) (action: FIO<'R, 'E>) : FIO<'R, 'E> =
        fio {
            // Create savepoint
            let! _ = (save savepointName transaction).MapError (fun e -> Unchecked.defaultof<'E>)

            try
                // Execute action
                let! result = action

                // Release savepoint
                let! _ = (release savepointName transaction).MapError (fun e -> Unchecked.defaultof<'E>)

                return result
            with
            | ex ->
                // Rollback to savepoint on error
                let! _ = (rollbackTo savepointName transaction).MapError (fun _ -> Unchecked.defaultof<'E>)

                return! FIO.Fail (Unchecked.defaultof<'E>)
        }
