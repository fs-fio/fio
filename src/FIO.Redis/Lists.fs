namespace FIO.Redis

open FIO.DSL

open StackExchange.Redis

open System

/// <summary>
/// Functions for Redis list operations.
/// </summary>
[<RequireQualifiedAccess>]
module List =

    /// <summary>
    /// Pushes values to the left (head) of a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="values">The list element values.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpush (key: string) (values: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = values |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.ListLeftPushAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("LPUSH", e))

    /// <summary>
    /// Pushes a single value to the left.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpushOne (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListLeftPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("LPUSH", e))

    /// <summary>
    /// Pushes to the left only if the key exists.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpushx (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListLeftPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value, When.Exists),
            fun e -> CommandFailed("LPUSHX", e))

    /// <summary>
    /// Pushes values to the right (tail) of a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="values">The list element values.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpush (key: string) (values: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = values |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.ListRightPushAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("RPUSH", e))

    /// <summary>
    /// Pushes a single value to the right.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpushOne (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRightPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("RPUSH", e))

    /// <summary>
    /// Pushes to the right only if the key exists.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpushx (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRightPushAsync(RedisKey.op_Implicit key, RedisValue.ofString value, When.Exists),
            fun e -> CommandFailed("RPUSHX", e))

    /// <summary>
    /// Pops from the left (head) of a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpop (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListLeftPopAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("LPOP", e))
            .Map(RedisValue.toString)

    /// <summary>
    /// Pops multiple elements from the left.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of elements.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpopMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListLeftPopAsync(RedisKey.op_Implicit key, int count),
            fun e -> CommandFailed("LPOP", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Pops from the right (tail) of a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpop (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRightPopAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("RPOP", e))
            .Map RedisValue.toString

    /// <summary>
    /// Pops multiple elements from the right.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of elements.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpopMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRightPopAsync(RedisKey.op_Implicit key, int count),
            fun e -> CommandFailed("RPOP", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Pops from one list and pushes to another.
    /// </summary>
    /// <param name="source">The source list key.</param>
    /// <param name="destination">The destination list key.</param>
    /// <param name="conn">The Redis connection.</param>
    let rpopLpush (source: string) (destination: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRightPopLeftPushAsync(RedisKey.op_Implicit source, RedisKey.op_Implicit destination),
            fun e -> CommandFailed("RPOPLPUSH", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets a range of elements from a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let lrange (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRangeAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("LRANGE", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets the length of a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let llen (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListLengthAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("LLEN", e))

    /// <summary>
    /// Gets an element by index.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="index">The list index.</param>
    /// <param name="conn">The Redis connection.</param>
    let lindex (key: string) (index: int64) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListGetByIndexAsync(RedisKey.op_Implicit key, index),
            fun e -> CommandFailed("LINDEX", e))
            .Map RedisValue.toString

    /// <summary>
    /// Sets an element at index.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="index">The list index.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let lset (key: string) (index: int64) (value: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitTask(
            db.ListSetByIndexAsync(RedisKey.op_Implicit key, index, RedisValue.ofString value),
            fun e -> CommandFailed("LSET", e))

    /// <summary>
    /// Removes elements from a list.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of elements.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let lrem (key: string) (count: int64) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListRemoveAsync(RedisKey.op_Implicit key, RedisValue.ofString value, count),
            fun e -> CommandFailed("LREM", e))

    /// <summary>
    /// Trims a list to the specified range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let ltrim (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitTask(
            db.ListTrimAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("LTRIM", e))

    /// <summary>
    /// Inserts before a pivot element.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pivot">The pivot element.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let linsertBefore (key: string) (pivot: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListInsertBeforeAsync(RedisKey.op_Implicit key, RedisValue.ofString pivot, RedisValue.ofString value),
            fun e -> CommandFailed("LINSERT", e))

    /// <summary>
    /// Inserts after a pivot element.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pivot">The pivot element.</param>
    /// <param name="value">The list element value.</param>
    /// <param name="conn">The Redis connection.</param>
    let linsertAfter (key: string) (pivot: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListInsertAfterAsync(RedisKey.op_Implicit key, RedisValue.ofString pivot, RedisValue.ofString value),
            fun e -> CommandFailed("LINSERT", e))

    /// <summary>
    /// Blocking left pop.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="timeout">The blocking timeout.</param>
    /// <param name="conn">The Redis connection.</param>
    let blpop (keys: string list) (timeout: TimeSpan) (conn: RedisConnection) : FIO<(string * string) option, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.ListLeftPopAsync(redisKeys, 1L),
            fun e -> CommandFailed("BLPOP", e))
            .Map(fun (result: ListPopResult) ->
                if result.IsNull then None
                else Some (result.Key.ToString(), result.Values.[0].ToString()))

    /// <summary>
    /// Blocking right pop.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="timeout">The blocking timeout.</param>
    /// <param name="conn">The Redis connection.</param>
    let brpop (keys: string list) (timeout: TimeSpan) (conn: RedisConnection) : FIO<(string * string) option, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.ListRightPopAsync(redisKeys, 1L),
            fun e -> CommandFailed("BRPOP", e))
            .Map(fun (result: ListPopResult) ->
                if result.IsNull then None
                else Some (result.Key.ToString(), result.Values.[0].ToString()))

    /// <summary>
    /// Blocking pop from one list and push to another.
    /// </summary>
    /// <param name="source">The source list key.</param>
    /// <param name="destination">The destination list key.</param>
    /// <param name="timeout">The blocking timeout.</param>
    /// <param name="conn">The Redis connection.</param>
    let brpoplpush (source: string) (destination: string) (timeout: TimeSpan) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListMoveAsync(RedisKey.op_Implicit source, RedisKey.op_Implicit destination, ListSide.Right, ListSide.Left),
            fun e -> CommandFailed("BRPOPLPUSH", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets the position of an element.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="element">The element to find.</param>
    /// <param name="conn">The Redis connection.</param>
    let lpos (key: string) (element: string) (conn: RedisConnection) : FIO<int64 option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListPositionAsync(RedisKey.op_Implicit key, RedisValue.ofString element),
            fun e -> CommandFailed("LPOS", e))
            .Map(fun pos -> if pos = -1L then None else Some pos)

    /// <summary>
    /// Moves an element between lists.
    /// </summary>
    /// <param name="source">The source list key.</param>
    /// <param name="destination">The destination list key.</param>
    /// <param name="srcSide">The source list side.</param>
    /// <param name="destSide">The destination list side.</param>
    /// <param name="conn">The Redis connection.</param>
    let lmove (source: string) (destination: string) (srcSide: ListSide) (destSide: ListSide) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.ListMoveAsync(RedisKey.op_Implicit source, RedisKey.op_Implicit destination, srcSide, destSide),
            fun e -> CommandFailed("LMOVE", e))
            .Map RedisValue.toString
