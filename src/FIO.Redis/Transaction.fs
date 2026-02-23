namespace FIO.Redis

open FIO.DSL

open StackExchange.Redis

open System
open System.Threading.Tasks

/// <summary>
/// Functions for Redis transaction operations.
/// </summary>
[<RequireQualifiedAccess>]
module Tx =

    /// <summary>
    /// Represents a transaction builder that queues commands for atomic execution.
    /// </summary>
    type TransactionBuilder =
        internal {
            Transaction: ITransaction
        }

    /// <summary>
    /// Creates a transaction builder.
    /// </summary>
    /// <param name="tx">The transaction interface.</param>
    let internal create (tx: ITransaction) : TransactionBuilder =
        { Transaction = tx }

    /// <summary>
    /// Adds a condition to the transaction (optimistic locking).
    /// </summary>
    /// <param name="condition">The transaction condition.</param>
    /// <param name="builder">The transaction builder.</param>
    let addCondition (condition: Condition) (builder: TransactionBuilder) : TransactionBuilder =
        builder.Transaction.AddCondition(condition) |> ignore
        builder

    /// <summary>
    /// Adds a key exists condition.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="builder">The transaction builder.</param>
    let whenKeyExists (key: string) (builder: TransactionBuilder) : TransactionBuilder =
        addCondition (Condition.KeyExists(RedisKey.op_Implicit key)) builder

    /// <summary>
    /// Adds a key not exists condition.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="builder">The transaction builder.</param>
    let whenKeyNotExists (key: string) (builder: TransactionBuilder) : TransactionBuilder =
        addCondition (Condition.KeyNotExists(RedisKey.op_Implicit key)) builder

    /// <summary>
    /// Adds a string equals condition.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let whenStringEquals (key: string) (value: string) (builder: TransactionBuilder) : TransactionBuilder =
        addCondition (Condition.StringEqual(RedisKey.op_Implicit key, RedisValue.ofString value)) builder

    /// <summary>
    /// Adds a hash field exists condition.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="builder">The transaction builder.</param>
    let whenHashFieldExists (key: string) (field: string) (builder: TransactionBuilder) : TransactionBuilder =
        addCondition (Condition.HashExists(RedisKey.op_Implicit key, RedisValue.ofString field)) builder

    /// <summary>
    /// Queues a string set command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueStringSet (key: string) (value: string) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value)

    /// <summary>
    /// Queues a string set with expiry command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueStringSetEx (key: string) (value: string) (expiry: TimeSpan) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value, Expiration expiry)

    /// <summary>
    /// Queues a string get command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueStringGet (key: string) (builder: TransactionBuilder) : Task<RedisValue> =
        builder.Transaction.StringGetAsync(RedisKey.op_Implicit key)

    /// <summary>
    /// Queues a string increment command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="amount">The increment amount.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueStringIncrement (key: string) (amount: int64) (builder: TransactionBuilder) : Task<int64> =
        builder.Transaction.StringIncrementAsync(RedisKey.op_Implicit key, amount)

    /// <summary>
    /// Queues a hash set command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueHashSet (key: string) (field: string) (value: string) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.HashSetAsync(RedisKey.op_Implicit key, RedisValue.ofString field, RedisValue.ofString value)

    /// <summary>
    /// Queues a hash get command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueHashGet (key: string) (field: string) (builder: TransactionBuilder) : Task<RedisValue> =
        builder.Transaction.HashGetAsync(RedisKey.op_Implicit key, RedisValue.ofString field)

    /// <summary>
    /// Queues a hash increment command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="amount">The increment amount.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueHashIncrement (key: string) (field: string) (amount: int64) (builder: TransactionBuilder) : Task<int64> =
        builder.Transaction.HashIncrementAsync(RedisKey.op_Implicit key, RedisValue.ofString field, amount)

    /// <summary>
    /// Queues a key delete command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueKeyDelete (key: string) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.KeyDeleteAsync(RedisKey.op_Implicit key)

    /// <summary>
    /// Queues a key expire command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueKeyExpire (key: string) (expiry: TimeSpan) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.KeyExpireAsync(RedisKey.op_Implicit key, Nullable expiry)

    /// <summary>
    /// Queues a list left push command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueListLeftPush (key: string) (value: string) (builder: TransactionBuilder) : Task<int64> =
        builder.Transaction.ListLeftPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value)

    /// <summary>
    /// Queues a list right push command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueListRightPush (key: string) (value: string) (builder: TransactionBuilder) : Task<int64> =
        builder.Transaction.ListRightPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value)

    /// <summary>
    /// Queues a set add command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueSetAdd (key: string) (value: string) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.SetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value)

    /// <summary>
    /// Queues a set remove command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueSetRemove (key: string) (value: string) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.SetRemoveAsync(RedisKey.op_Implicit key, RedisValue.ofString value)

    /// <summary>
    /// Queues a sorted set add command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="score">The sorted set score.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueSortedSetAdd (key: string) (value: string) (score: float) (builder: TransactionBuilder) : Task<bool> =
        builder.Transaction.SortedSetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value, score)

    /// <summary>
    /// Queues a sorted set increment command.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="increment">The score increment.</param>
    /// <param name="builder">The transaction builder.</param>
    let queueSortedSetIncrement (key: string) (value: string) (increment: float) (builder: TransactionBuilder) : Task<float> =
        builder.Transaction.SortedSetIncrementAsync(RedisKey.op_Implicit key, RedisValue.ofString value, increment)

    /// <summary>
    /// Executes a transaction with a builder function.
    /// </summary>
    /// <param name="build">The builder function.</param>
    /// <param name="conn">The Redis connection.</param>
    let multi (build: TransactionBuilder -> unit) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        let tx = db.CreateTransaction()
        let builder = create tx
        build builder
        FIO.awaitGenericTask(
            tx.ExecuteAsync(),
            fun e -> TransactionAborted e)

    /// <summary>
    /// Executes a transaction and collects results.
    /// </summary>
    /// <param name="build">The builder function.</param>
    /// <param name="conn">The Redis connection.</param>
    let multiWithResults (build: TransactionBuilder -> 'T) (conn: RedisConnection) : FIO<bool * 'T, RedisError> =
        let db = Redis.getDatabase conn
        let tx = db.CreateTransaction()
        let builder = create tx
        let results = build builder
        FIO.awaitGenericTask(
            tx.ExecuteAsync(),
            fun e -> TransactionAborted e)
            .Map(fun success -> success, results)

    /// <summary>
    /// Watches keys for optimistic locking (use before multi).
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let watch (keys: string list) (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let db = Redis.getDatabase conn
                // StackExchange.Redis handles watch through transaction conditions
                // This is a no-op placeholder for API compatibility
                ()),
            fun e -> CommandFailed("WATCH", e))

    /// <summary>
    /// Executes a simple transaction with two operations.
    /// </summary>
    /// <param name="op1">The transaction operation.</param>
    /// <param name="op2">The transaction operation.</param>
    /// <param name="conn">The Redis connection.</param>
    let exec2
        (op1: TransactionBuilder -> Task<'A>)
        (op2: TransactionBuilder -> Task<'B>)
        (conn: RedisConnection) : FIO<('A * 'B) option, RedisError> =
        let db = Redis.getDatabase conn
        let tx = db.CreateTransaction()
        let builder = create tx
        let t1 = op1 builder
        let t2 = op2 builder
        FIO.awaitGenericTask(tx.ExecuteAsync(), fun e -> TransactionAborted e)
            .FlatMap(fun success ->
                if success then
                    fio {
                        let! r1 = FIO.awaitGenericTask(t1, fun e -> TransactionAborted e)
                        let! r2 = FIO.awaitGenericTask(t2, fun e -> TransactionAborted e)
                        return Some (r1, r2)
                    }
                else FIO.succeed None)

    /// <summary>
    /// Executes a simple transaction with three operations.
    /// </summary>
    /// <param name="op1">The transaction operation.</param>
    /// <param name="op2">The transaction operation.</param>
    /// <param name="op3">The transaction operation.</param>
    /// <param name="conn">The Redis connection.</param>
    let exec3
        (op1: TransactionBuilder -> Task<'A>)
        (op2: TransactionBuilder -> Task<'B>)
        (op3: TransactionBuilder -> Task<'C>)
        (conn: RedisConnection) : FIO<('A * 'B * 'C) option, RedisError> =
        let db = Redis.getDatabase conn
        let tx = db.CreateTransaction()
        let builder = create tx
        let t1 = op1 builder
        let t2 = op2 builder
        let t3 = op3 builder
        FIO.awaitGenericTask(tx.ExecuteAsync(), fun e -> TransactionAborted e)
            .FlatMap(fun success ->
                if success then
                    fio {
                        let! r1 = FIO.awaitGenericTask(t1, fun e -> TransactionAborted e)
                        let! r2 = FIO.awaitGenericTask(t2, fun e -> TransactionAborted e)
                        let! r3 = FIO.awaitGenericTask(t3, fun e -> TransactionAborted e)
                        return Some (r1, r2, r3)
                    }
                else FIO.succeed None)
