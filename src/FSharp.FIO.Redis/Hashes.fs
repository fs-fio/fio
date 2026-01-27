namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

/// <summary>
/// Functions for Redis hash operations.
/// </summary>
[<RequireQualifiedAccess>]
module Hash =

    /// <summary>
    /// Gets a hash field value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="conn">The Redis connection.</param>
    let hget (key: string) (field: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashGetAsync(RedisKey.op_Implicit key, RedisValue.ofString field),
            fun e -> CommandFailed("HGET", e))
            .Map RedisValue.toString

    /// <summary>
    /// Sets a hash field value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="value">The field value.</param>
    /// <param name="conn">The Redis connection.</param>
    let hset (key: string) (field: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashSetAsync(RedisKey.op_Implicit key, RedisValue.ofString field, RedisValue.ofString value),
            fun e -> CommandFailed("HSET", e))

    /// <summary>
    /// Sets a hash field only if it doesn't exist.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="value">The field value.</param>
    /// <param name="conn">The Redis connection.</param>
    let hsetNx (key: string) (field: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashSetAsync(RedisKey.op_Implicit key, RedisValue.ofString field, RedisValue.ofString value, When.NotExists),
            fun e -> CommandFailed("HSETNX", e))

    /// <summary>
    /// Gets multiple hash field values.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="fields">The hash field names.</param>
    /// <param name="conn">The Redis connection.</param>
    let hmget (key: string) (fields: string list) (conn: RedisConnection) : FIO<(string * string option) list, RedisError> =
        let db = Redis.getDatabase conn
        let redisFields = fields |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.HashGetAsync(RedisKey.op_Implicit key, redisFields),
            fun e -> CommandFailed("HMGET", e))
            .Map(fun values ->
                List.zip fields (values |> Array.map RedisValue.toString |> Array.toList))

    /// <summary>
    /// Sets multiple hash field values.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pairs">The field-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let hmset (key: string) (pairs: (string * string) list) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        let entries =
            pairs
            |> List.map (fun (f, v) -> HashEntry(RedisValue.ofString f, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitTask(
            db.HashSetAsync(RedisKey.op_Implicit key, entries),
            fun e -> CommandFailed("HMSET", e))

    /// <summary>
    /// Gets all fields and values.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let hgetAll (key: string) (conn: RedisConnection) : FIO<Map<string, string>, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashGetAllAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("HGETALL", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Name, RedisValue.toString e.Value with
                    | Some n, Some v -> Some (n, v)
                    | _ -> None)
                |> Map.ofArray)

    /// <summary>
    /// Deletes hash fields.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="fields">The hash field names.</param>
    /// <param name="conn">The Redis connection.</param>
    let hdel (key: string) (fields: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisFields = fields |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.HashDeleteAsync(RedisKey.op_Implicit key, redisFields),
            fun e -> CommandFailed("HDEL", e))

    /// <summary>
    /// Deletes a single hash field.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="conn">The Redis connection.</param>
    let hdelOne (key: string) (field: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashDeleteAsync(RedisKey.op_Implicit key, RedisValue.ofString field),
            fun e -> CommandFailed("HDEL", e))

    /// <summary>
    /// Checks if a hash field exists.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="conn">The Redis connection.</param>
    let hexists (key: string) (field: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashExistsAsync(RedisKey.op_Implicit key, RedisValue.ofString field),
            fun e -> CommandFailed("HEXISTS", e))

    /// <summary>
    /// Gets all hash field names.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let hkeys (key: string) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashKeysAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("HKEYS", e))
            .Map(fun keys -> keys |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets all hash values.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let hvals (key: string) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashValuesAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("HVALS", e))
            .Map(fun vals -> vals |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets the number of fields in a hash.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let hlen (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashLengthAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("HLEN", e))

    /// <summary>
    /// Increments a hash field by an integer.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let hincrBy (key: string) (field: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashIncrementAsync(RedisKey.op_Implicit key, RedisValue.ofString field, amount),
            fun e -> CommandFailed("HINCRBY", e))

    /// <summary>
    /// Increments a hash field by a float.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let hincrByFloat (key: string) (field: string) (amount: float) (conn: RedisConnection) : FIO<float, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashIncrementAsync(RedisKey.op_Implicit key, RedisValue.ofString field, amount),
            fun e -> CommandFailed("HINCRBYFLOAT", e))

    /// <summary>
    /// Decrements a hash field by an integer.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let hdecrBy (key: string) (field: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashDecrementAsync(RedisKey.op_Implicit key, RedisValue.ofString field, amount),
            fun e -> CommandFailed("HDECRBY", e))

    /// <summary>
    /// Gets the string length of a hash field value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="field">The hash field name.</param>
    /// <param name="conn">The Redis connection.</param>
    let hstrlen (key: string) (field: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashStringLengthAsync(RedisKey.op_Implicit key, RedisValue.ofString field),
            fun e -> CommandFailed("HSTRLEN", e))

    /// <summary>
    /// Gets a random field from a hash.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let hrandfield (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashRandomFieldAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("HRANDFIELD", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets multiple random fields from a hash.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let hrandfieldMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashRandomFieldsAsync(RedisKey.op_Implicit key, count),
            fun e -> CommandFailed("HRANDFIELD", e))
            .Map(fun fields -> fields |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets random fields with values.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let hrandfieldWithValues (key: string) (count: int64) (conn: RedisConnection) : FIO<(string * string) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.HashRandomFieldsWithValuesAsync(RedisKey.op_Implicit key, count),
            fun e -> CommandFailed("HRANDFIELD", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Name, RedisValue.toString e.Value with
                    | Some n, Some v -> Some (n, v)
                    | _ -> None)
                |> Array.toList)

    /// <summary>
    /// Scans hash fields.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pattern">The field pattern to match.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let hscan (key: string) (pattern: string) (count: int) (conn: RedisConnection) : FIO<(string * string) list, RedisError> =
        FIO.attempt(
            (fun () ->
                let db = Redis.getDatabase conn
                db.HashScan(RedisKey.op_Implicit key, RedisValue.op_Implicit pattern, count)
                |> Seq.choose (fun e ->
                    match RedisValue.toString e.Name, RedisValue.toString e.Value with
                    | Some n, Some v -> Some (n, v)
                    | _ -> None)
                |> Seq.toList),
            fun e -> CommandFailed("HSCAN", e))
