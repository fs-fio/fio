namespace FIO.Redis

open FIO.DSL

open StackExchange.Redis

/// <summary>
/// Functions for Redis set operations.
/// </summary>
[<RequireQualifiedAccess>]
module Set =

    /// <summary>
    /// Adds members to a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The set members.</param>
    /// <param name="conn">The Redis connection.</param>
    let sadd (key: string) (members: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = members |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.SetAddAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("SADD", e))

    /// <summary>
    /// Adds a single member to a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The set member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let saddOne (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("SADD", e))

    /// <summary>
    /// Removes members from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The set members.</param>
    /// <param name="conn">The Redis connection.</param>
    let srem (key: string) (members: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = members |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.SetRemoveAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("SREM", e))

    /// <summary>
    /// Removes a single member from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The set member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let sremOne (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetRemoveAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("SREM", e))

    /// <summary>
    /// Gets all members of a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let smembers (key: string) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetMembersAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("SMEMBERS", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Checks if a member exists in a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The set member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let sismember (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetContainsAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("SISMEMBER", e))

    /// <summary>
    /// Checks multiple members for existence.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The set members.</param>
    /// <param name="conn">The Redis connection.</param>
    let smismember (key: string) (members: string list) (conn: RedisConnection) : FIO<(string * bool) list, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = members |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.SetContainsAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("SMISMEMBER", e))
            .Map(fun results ->
                List.zip members (results |> Array.toList))

    /// <summary>
    /// Gets the number of members in a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let scard (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetLengthAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("SCARD", e))

    /// <summary>
    /// Pops a random member from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let spop (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetPopAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("SPOP", e))
            .Map RedisValue.toString

    /// <summary>
    /// Pops multiple random members from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let spopMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetPopAsync(RedisKey.op_Implicit key, count),
            fun e -> CommandFailed("SPOP", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets a random member from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let srandmember (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetRandomMemberAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("SRANDMEMBER", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets multiple random members from a set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let srandmemberMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetRandomMembersAsync(RedisKey.op_Implicit key, count),
            fun e -> CommandFailed("SRANDMEMBER", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets the union of multiple sets.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sunion (keys: string list) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAsync(SetOperation.Union, redisKeys),
            fun e -> CommandFailed("SUNION", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Stores the union of multiple sets.
    /// </summary>
    /// <param name="destination">The destination key.</param>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sunionStore (destination: string) (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAndStoreAsync(SetOperation.Union, RedisKey.op_Implicit destination, redisKeys),
            fun e -> CommandFailed("SUNIONSTORE", e))

    /// <summary>
    /// Gets the intersection of multiple sets.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sinter (keys: string list) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAsync(SetOperation.Intersect, redisKeys),
            fun e -> CommandFailed("SINTER", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Stores the intersection of multiple sets.
    /// </summary>
    /// <param name="destination">The destination key.</param>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sinterStore (destination: string) (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAndStoreAsync(SetOperation.Intersect, RedisKey.op_Implicit destination, redisKeys),
            fun e -> CommandFailed("SINTERSTORE", e))

    /// <summary>
    /// Gets the cardinality of intersection without computing it.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="limit">The maximum count limit.</param>
    /// <param name="conn">The Redis connection.</param>
    let sinterCard (keys: string list) (limit: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetIntersectionLengthAsync(redisKeys, limit),
            fun e -> CommandFailed("SINTERCARD", e))

    /// <summary>
    /// Gets the difference of sets.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sdiff (keys: string list) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAsync(SetOperation.Difference, redisKeys),
            fun e -> CommandFailed("SDIFF", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Stores the difference of sets.
    /// </summary>
    /// <param name="destination">The destination key.</param>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let sdiffStore (destination: string) (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SetCombineAndStoreAsync(SetOperation.Difference, RedisKey.op_Implicit destination, redisKeys),
            fun e -> CommandFailed("SDIFFSTORE", e))

    /// <summary>
    /// Moves a member from one set to another.
    /// </summary>
    /// <param name="source">The source key.</param>
    /// <param name="destination">The destination key.</param>
    /// <param name="member'">The set member.</param>
    /// <param name="conn">The Redis connection.</param>
    let smove (source: string) (destination: string) (member': string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SetMoveAsync(RedisKey.op_Implicit source, RedisKey.op_Implicit destination, RedisValue.ofString member'),
            fun e -> CommandFailed("SMOVE", e))

    /// <summary>
    /// Scans set members.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pattern">The member pattern to match.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let sscan (key: string) (pattern: string) (count: int) (conn: RedisConnection) : FIO<string list, RedisError> =
        FIO.attempt(
            (fun () ->
                let db = Redis.getDatabase conn
                db.SetScan(RedisKey.op_Implicit key, RedisValue.op_Implicit pattern, count)
                |> Seq.choose RedisValue.toString
                |> Seq.toList),
            fun e -> CommandFailed("SSCAN", e))
