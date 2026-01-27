namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

/// <summary>
/// Functions for Redis sorted set operations.
/// </summary>
[<RequireQualifiedAccess>]
module ZSet =

    /// <summary>
    /// Adds members with scores to a sorted set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The score-member pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let zadd (key: string) (members: (float * string) list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let entries =
            members
            |> List.map (fun (score, value) -> SortedSetEntry(RedisValue.ofString value, score))
            |> List.toArray
        FIO.awaitGenericTask(
            db.SortedSetAddAsync(RedisKey.op_Implicit key, entries),
            fun e -> CommandFailed("ZADD", e))

    /// <summary>
    /// Adds a single member with score.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="score">The member score.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zaddOne (key: string) (score: float) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value, score),
            fun e -> CommandFailed("ZADD", e))

    /// <summary>
    /// Adds only if member doesn't exist.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="score">The member score.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zaddNx (key: string) (score: float) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value, score, SortedSetWhen.NotExists),
            fun e -> CommandFailed("ZADD NX", e))

    /// <summary>
    /// Adds only if member exists (update).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="score">The member score.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zaddXx (key: string) (score: float) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetAddAsync(RedisKey.op_Implicit key, RedisValue.ofString value, score, SortedSetWhen.Exists),
            fun e -> CommandFailed("ZADD XX", e))

    /// <summary>
    /// Removes members from a sorted set.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The score-member pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrem (key: string) (members: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = members |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.SortedSetRemoveAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("ZREM", e))

    /// <summary>
    /// Removes a single member.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zremOne (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRemoveAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("ZREM", e))

    /// <summary>
    /// Gets the score of a member.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zscore (key: string) (value: string) (conn: RedisConnection) : FIO<float option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetScoreAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("ZSCORE", e))
            .Map(fun s -> if s.HasValue then Some s.Value else None)

    /// <summary>
    /// Gets the scores of multiple members.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="members">The score-member pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let zmscore (key: string) (members: string list) (conn: RedisConnection) : FIO<(string * float option) list, RedisError> =
        let db = Redis.getDatabase conn
        let redisValues = members |> List.map RedisValue.ofString |> List.toArray
        FIO.awaitGenericTask(
            db.SortedSetScoresAsync(RedisKey.op_Implicit key, redisValues),
            fun e -> CommandFailed("ZMSCORE", e))
            .Map(fun scores ->
                List.zip members
                    (scores |> Array.map (fun s -> if s.HasValue then Some s.Value else None) |> Array.toList))

    /// <summary>
    /// Gets the rank of a member (0-based, ascending).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrank (key: string) (value: string) (conn: RedisConnection) : FIO<int64 option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRankAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("ZRANK", e))
            .Map(fun r -> if r.HasValue then Some r.Value else None)

    /// <summary>
    /// Gets the reverse rank of a member (0-based, descending).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrevrank (key: string) (value: string) (conn: RedisConnection) : FIO<int64 option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRankAsync(RedisKey.op_Implicit key, RedisValue.ofString value, Order.Descending),
            fun e -> CommandFailed("ZREVRANK", e))
            .Map(fun r -> if r.HasValue then Some r.Value else None)

    /// <summary>
    /// Gets members by rank range (ascending).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrange (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByRankAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("ZRANGE", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets members with scores by rank range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrangeWithScores (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByRankWithScoresAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("ZRANGE WITHSCORES", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Element with
                    | Some v -> Some (v, e.Score)
                    | None -> None)
                |> Array.toList)

    /// <summary>
    /// Gets members by rank range (descending).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrevrange (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByRankAsync(RedisKey.op_Implicit key, start, stop, Order.Descending),
            fun e -> CommandFailed("ZREVRANGE", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets members with scores by rank range (descending).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrevrangeWithScores (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByRankWithScoresAsync(RedisKey.op_Implicit key, start, stop, Order.Descending),
            fun e -> CommandFailed("ZREVRANGE WITHSCORES", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Element with
                    | Some v -> Some (v, e.Score)
                    | None -> None)
                |> Array.toList)

    /// <summary>
    /// Gets members by score range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="min">The minimum score.</param>
    /// <param name="max">The maximum score.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrangeByScore (key: string) (min: float) (max: float) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByScoreAsync(RedisKey.op_Implicit key, min, max),
            fun e -> CommandFailed("ZRANGEBYSCORE", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets members by score range with offset and count.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="min">The minimum score.</param>
    /// <param name="max">The maximum score.</param>
    /// <param name="skip">The offset for pagination.</param>
    /// <param name="take">The count for pagination.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrangeByScoreWithLimit (key: string) (min: float) (max: float) (skip: int64) (take: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRangeByScoreAsync(RedisKey.op_Implicit key, min, max, Exclude.None, Order.Ascending, skip, take),
            fun e -> CommandFailed("ZRANGEBYSCORE", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Gets the number of members.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let zcard (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetLengthAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("ZCARD", e))

    /// <summary>
    /// Counts members in a score range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="min">The minimum score.</param>
    /// <param name="max">The maximum score.</param>
    /// <param name="conn">The Redis connection.</param>
    let zcount (key: string) (min: float) (max: float) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetLengthAsync(RedisKey.op_Implicit key, min, max),
            fun e -> CommandFailed("ZCOUNT", e))

    /// <summary>
    /// Increments a member's score.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="increment">The score increment.</param>
    /// <param name="conn">The Redis connection.</param>
    let zincrby (key: string) (value: string) (increment: float) (conn: RedisConnection) : FIO<float, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetIncrementAsync(RedisKey.op_Implicit key, RedisValue.ofString value, increment),
            fun e -> CommandFailed("ZINCRBY", e))

    /// <summary>
    /// Decrements a member's score.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The member value.</param>
    /// <param name="decrement">The score decrement.</param>
    /// <param name="conn">The Redis connection.</param>
    let zdecrby (key: string) (value: string) (decrement: float) (conn: RedisConnection) : FIO<float, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetDecrementAsync(RedisKey.op_Implicit key, RedisValue.ofString value, decrement),
            fun e -> CommandFailed("ZINCRBY", e))

    /// <summary>
    /// Pops the member with lowest score.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let zpopmin (key: string) (conn: RedisConnection) : FIO<(string * float) option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetPopAsync(RedisKey.op_Implicit key, Order.Ascending),
            fun e -> CommandFailed("ZPOPMIN", e))
            .Map(fun entry ->
                if entry.HasValue then
                    match RedisValue.toString entry.Value.Element with
                    | Some v -> Some (v, entry.Value.Score)
                    | None -> None
                else None)

    /// <summary>
    /// Pops multiple members with lowest scores.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let zpopminMany (key: string) (count: int64) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetPopAsync(RedisKey.op_Implicit key, count, Order.Ascending),
            fun e -> CommandFailed("ZPOPMIN", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Element with
                    | Some v -> Some (v, e.Score)
                    | None -> None)
                |> Array.toList)

    /// <summary>
    /// Pops the member with highest score.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let zpopmax (key: string) (conn: RedisConnection) : FIO<(string * float) option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetPopAsync(RedisKey.op_Implicit key, Order.Descending),
            fun e -> CommandFailed("ZPOPMAX", e))
            .Map(fun entry ->
                if entry.HasValue then
                    match RedisValue.toString entry.Value.Element with
                    | Some v -> Some (v, entry.Value.Score)
                    | None -> None
                else None)

    /// <summary>
    /// Pops multiple members with highest scores.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let zpopmaxMany (key: string) (count: int64) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetPopAsync(RedisKey.op_Implicit key, count, Order.Descending),
            fun e -> CommandFailed("ZPOPMAX", e))
            .Map(fun entries ->
                entries
                |> Array.choose (fun e ->
                    match RedisValue.toString e.Element with
                    | Some v -> Some (v, e.Score)
                    | None -> None)
                |> Array.toList)

    /// <summary>
    /// Removes members by rank range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let zremrangeByRank (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRemoveRangeByRankAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("ZREMRANGEBYRANK", e))

    /// <summary>
    /// Removes members by score range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="min">The minimum score.</param>
    /// <param name="max">The maximum score.</param>
    /// <param name="conn">The Redis connection.</param>
    let zremrangeByScore (key: string) (min: float) (max: float) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRemoveRangeByScoreAsync(RedisKey.op_Implicit key, min, max),
            fun e -> CommandFailed("ZREMRANGEBYSCORE", e))

    /// <summary>
    /// Gets a random member.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrandmember (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRandomMemberAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("ZRANDMEMBER", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets multiple random members.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let zrandmemberMany (key: string) (count: int64) (conn: RedisConnection) : FIO<string list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.SortedSetRandomMembersAsync(RedisKey.op_Implicit key, count),
            fun e -> CommandFailed("ZRANDMEMBER", e))
            .Map(fun values -> values |> Array.choose RedisValue.toString |> Array.toList)

    /// <summary>
    /// Stores the union of sorted sets.
    /// </summary>
    /// <param name="destination">The destination key.</param>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let zunionStore (destination: string) (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SortedSetCombineAndStoreAsync(SetOperation.Union, RedisKey.op_Implicit destination, redisKeys),
            fun e -> CommandFailed("ZUNIONSTORE", e))

    /// <summary>
    /// Stores the intersection of sorted sets.
    /// </summary>
    /// <param name="destination">The destination key.</param>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let zinterStore (destination: string) (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.SortedSetCombineAndStoreAsync(SetOperation.Intersect, RedisKey.op_Implicit destination, redisKeys),
            fun e -> CommandFailed("ZINTERSTORE", e))

    /// <summary>
    /// Scans sorted set members.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="pattern">The member pattern to match.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let zscan (key: string) (pattern: string) (count: int) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
        FIO.attempt(
            (fun () ->
                let db = Redis.getDatabase conn
                db.SortedSetScan(RedisKey.op_Implicit key, RedisValue.op_Implicit pattern, count)
                |> Seq.choose (fun e ->
                    match RedisValue.toString e.Element with
                    | Some v -> Some (v, e.Score)
                    | None -> None)
                |> Seq.toList),
            fun e -> CommandFailed("ZSCAN", e))
