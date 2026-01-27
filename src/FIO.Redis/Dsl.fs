namespace FIO.Redis

open FIO.DSL

open System

// Alias to avoid shadowing from our List module
module FMap = Map

/// <summary>
/// High-level Redis operations DSL.
/// </summary>
[<AutoOpen>]
module Dsl =

    /// <summary>
    /// Creates a Redis configuration from an endpoint.
    /// </summary>
    /// <param name="endpoint">The Redis endpoint.</param>
    let redisConfig endpoint = RedisConfig.create endpoint

    /// <summary>
    /// Creates a Redis configuration from a cluster.
    /// </summary>
    /// <param name="endpoints">The Redis cluster endpoints.</param>
    let redisCluster endpoints = RedisConfig.createCluster endpoints

    /// <summary>
    /// Combines get and set operations for caching.
    /// </summary>
    module Cache =

        /// <summary>
        /// Gets a value or computes and caches it if missing.
        /// </summary>
        /// <param name="key">The cache key.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="compute">The computation function for cache miss.</param>
        /// <param name="conn">The Redis connection.</param>
        let getOrSet (key: string) (expiry: TimeSpan) (compute: unit -> FIO<string, RedisError>) (conn: RedisConnection) : FIO<string, RedisError> =
            fio {
                let! cached = Str.get key conn
                match cached with
                | Some value -> return value
                | None ->
                    let! value = compute ()
                    do! Str.setEx key value expiry conn
                    return value
            }

        /// <summary>
        /// Gets a value or computes and caches it if missing (no expiry).
        /// </summary>
        /// <param name="key">The cache key.</param>
        /// <param name="compute">The computation function for cache miss.</param>
        /// <param name="conn">The Redis connection.</param>
        let getOrSetForever (key: string) (compute: unit -> FIO<string, RedisError>) (conn: RedisConnection) : FIO<string, RedisError> =
            fio {
                let! cached = Str.get key conn
                match cached with
                | Some value -> return value
                | None ->
                    let! value = compute ()
                    do! Str.set key value conn
                    return value
            }

        /// <summary>
        /// Invalidates a cache entry.
        /// </summary>
        /// <param name="key">The cache key.</param>
        /// <param name="conn">The Redis connection.</param>
        let invalidate (key: string) (conn: RedisConnection) : FIO<bool, RedisError> =
            Key.delete key conn

        /// <summary>
        /// Invalidates multiple cache entries.
        /// </summary>
        /// <param name="keys">The keys to invalidate.</param>
        /// <param name="conn">The Redis connection.</param>
        let invalidateMany (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
            Key.del keys conn

        /// <summary>
        /// Refreshes a cache entry's expiration.
        /// </summary>
        /// <param name="key">The cache key.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="conn">The Redis connection.</param>
        let refresh (key: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<bool, RedisError> =
            Key.expire key expiry conn

    /// <summary>
    /// Rate limiting operations.
    /// </summary>
    module RateLimit =

        /// <summary>
        /// Checks and increments a rate limit counter.
        /// </summary>
        /// <param name="key">The rate limit key.</param>
        /// <param name="limit">The rate limit.</param>
        /// <param name="window">The time window.</param>
        /// <param name="conn">The Redis connection.</param>
        let check (key: string) (limit: int64) (window: TimeSpan) (conn: RedisConnection) : FIO<bool * int64, RedisError> =
            fio {
                let! current = Str.incr key conn
                if current = 1L then
                    do! (Key.expire key window conn).Map(fun _ -> ())
                return (current <= limit, current)
            }

        /// <summary>
        /// Sliding window rate limiter using sorted sets.
        /// </summary>
        /// <param name="key">The rate limit key.</param>
        /// <param name="limit">The rate limit.</param>
        /// <param name="window">The time window.</param>
        /// <param name="conn">The Redis connection.</param>
        let slidingWindow (key: string) (limit: int64) (window: TimeSpan) (conn: RedisConnection) : FIO<bool, RedisError> =
            let now = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            let windowStart = now - int64 window.TotalMilliseconds
            fio {
                // Remove old entries
                do! (ZSet.zremrangeByScore key (float Int64.MinValue) (float windowStart) conn).Map(fun _ -> ())
                // Add current request
                do! (ZSet.zaddOne key (float now) (string now) conn).Map(fun _ -> ())
                // Count requests in window
                let! count = ZSet.zcard key conn
                // Set expiry on the key
                do! (Key.expire key window conn).Map(fun _ -> ())
                return count <= limit
            }

    /// <summary>
    /// Counter operations.
    /// </summary>
    module Counter =

        /// <summary>
        /// Gets a counter value.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="conn">The Redis connection.</param>
        let get (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
            (Str.get key conn).Map(fun v ->
                match v with
                | Some s ->
                    match Int64.TryParse s with
                    | true, n -> n
                    | _ -> 0L
                | None -> 0L)

        /// <summary>
        /// Increments a counter.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="conn">The Redis connection.</param>
        let incr (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
            Str.incr key conn

        /// <summary>
        /// Increments a counter by an amount.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="amount">The increment amount.</param>
        /// <param name="conn">The Redis connection.</param>
        let incrBy (key: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
            Str.incrBy key amount conn

        /// <summary>
        /// Decrements a counter.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="conn">The Redis connection.</param>
        let decr (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
            Str.decr key conn

        /// <summary>
        /// Decrements a counter by an amount.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="amount">The increment amount.</param>
        /// <param name="conn">The Redis connection.</param>
        let decrBy (key: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
            Str.decrBy key amount conn

        /// <summary>
        /// Resets a counter.
        /// </summary>
        /// <param name="key">The counter key.</param>
        /// <param name="conn">The Redis connection.</param>
        let reset (key: string) (conn: RedisConnection) : FIO<unit, RedisError> =
            Str.set key "0" conn

    /// <summary>
    /// Lock operations.
    /// </summary>
    module Lock =

        /// <summary>
        /// Tries to acquire a distributed lock.
        /// </summary>
        /// <param name="key">The lock key.</param>
        /// <param name="lockId">The lock identifier.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="conn">The Redis connection.</param>
        let tryAcquire (key: string) (lockId: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<bool, RedisError> =
            let lockKey = sprintf "lock:%s" key
            (Str.setNx lockKey lockId conn).FlatMap(fun acquired ->
                if acquired then
                    (Key.expire lockKey expiry conn).Map(fun _ -> true)
                else
                    FIO.succeed false)

        /// <summary>
        /// Releases a distributed lock.
        /// </summary>
        /// <param name="key">The lock key.</param>
        /// <param name="lockId">The lock identifier.</param>
        /// <param name="conn">The Redis connection.</param>
        let release (key: string) (lockId: string) (conn: RedisConnection) : FIO<bool, RedisError> =
            let lockKey = sprintf "lock:%s" key
            (Str.get lockKey conn).FlatMap(fun value ->
                match value with
                | Some v when v = lockId -> Key.delete lockKey conn
                | _ -> FIO.succeed false)

        /// <summary>
        /// Executes an action with a distributed lock.
        /// </summary>
        /// <param name="key">The lock key.</param>
        /// <param name="lockId">The lock identifier.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="action">The action to execute.</param>
        /// <param name="conn">The Redis connection.</param>
        let withLock (key: string) (lockId: string) (expiry: TimeSpan) (action: unit -> FIO<'R, RedisError>) (conn: RedisConnection) : FIO<'R option, RedisError> =
            fio {
                let! acquired = tryAcquire key lockId expiry conn
                if acquired then
                    let! result = (action ()).Ensuring((release key lockId conn).Map(fun _ -> ()))
                    return Some result
                else
                    return None
            }

    /// <summary>
    /// Leaderboard operations.
    /// </summary>
    module Leaderboard =

        /// <summary>
        /// Adds or updates a score.
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="member'">The leaderboard member.</param>
        /// <param name="score">The member score.</param>
        /// <param name="conn">The Redis connection.</param>
        let setScore (key: string) (member': string) (score: float) (conn: RedisConnection) : FIO<bool, RedisError> =
            ZSet.zaddOne key score member' conn

        /// <summary>
        /// Increments a score.
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="member'">The leaderboard member.</param>
        /// <param name="amount">The increment amount.</param>
        /// <param name="conn">The Redis connection.</param>
        let incrScore (key: string) (member': string) (amount: float) (conn: RedisConnection) : FIO<float, RedisError> =
            ZSet.zincrby key member' amount conn

        /// <summary>
        /// Gets the rank (0-based, highest score = rank 0).
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="member'">The leaderboard member.</param>
        /// <param name="conn">The Redis connection.</param>
        let getRank (key: string) (member': string) (conn: RedisConnection) : FIO<int64 option, RedisError> =
            ZSet.zrevrank key member' conn

        /// <summary>
        /// Gets the top N members.
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="count">The number of items.</param>
        /// <param name="conn">The Redis connection.</param>
        let getTop (key: string) (count: int) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
            ZSet.zrevrangeWithScores key 0L (int64 count - 1L) conn

        /// <summary>
        /// Gets a member's score.
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="member'">The leaderboard member.</param>
        /// <param name="conn">The Redis connection.</param>
        let getScore (key: string) (member': string) (conn: RedisConnection) : FIO<float option, RedisError> =
            ZSet.zscore key member' conn

        /// <summary>
        /// Gets members around a specific rank.
        /// </summary>
        /// <param name="key">The leaderboard key.</param>
        /// <param name="rank">The leaderboard rank.</param>
        /// <param name="count">The number of items.</param>
        /// <param name="conn">The Redis connection.</param>
        let getAroundRank (key: string) (rank: int64) (count: int64) (conn: RedisConnection) : FIO<(string * float) list, RedisError> =
            let start = max 0L (rank - count / 2L)
            let stop = start + count - 1L
            ZSet.zrevrangeWithScores key start stop conn

    /// <summary>
    /// Session management operations.
    /// </summary>
    module Session =

        /// <summary>
        /// Creates a session.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="data">The session data.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="conn">The Redis connection.</param>
        let create (sessionId: string) (data: Map<string, string>) (expiry: TimeSpan) (conn: RedisConnection) : FIO<unit, RedisError> =
            let key = sprintf "session:%s" sessionId
            fio {
                do! Hash.hmset key (FMap.toList data) conn
                do! (Key.expire key expiry conn).Map(fun _ -> ())
            }

        /// <summary>
        /// Gets session data.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="conn">The Redis connection.</param>
        let get (sessionId: string) (conn: RedisConnection) : FIO<Map<string, string> option, RedisError> =
            let key = sprintf "session:%s" sessionId
            fio {
                let! exists = Key.exists key conn
                if exists then
                    let! data = Hash.hgetAll key conn
                    return Some data
                else
                    return None
            }

        /// <summary>
        /// Updates a session field.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="field">The session field name.</param>
        /// <param name="value">The field value.</param>
        /// <param name="conn">The Redis connection.</param>
        let setField (sessionId: string) (field: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
            let key = sprintf "session:%s" sessionId
            Hash.hset key field value conn

        /// <summary>
        /// Gets a session field.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="field">The session field name.</param>
        /// <param name="conn">The Redis connection.</param>
        let getField (sessionId: string) (field: string) (conn: RedisConnection) : FIO<string option, RedisError> =
            let key = sprintf "session:%s" sessionId
            Hash.hget key field conn

        /// <summary>
        /// Refreshes session expiry.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="expiry">The expiration time.</param>
        /// <param name="conn">The Redis connection.</param>
        let refresh (sessionId: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<bool, RedisError> =
            let key = sprintf "session:%s" sessionId
            Key.expire key expiry conn

        /// <summary>
        /// Destroys a session.
        /// </summary>
        /// <param name="sessionId">The session identifier.</param>
        /// <param name="conn">The Redis connection.</param>
        let destroy (sessionId: string) (conn: RedisConnection) : FIO<bool, RedisError> =
            let key = sprintf "session:%s" sessionId
            Key.delete key conn

    /// <summary>
    /// Queue operations using lists.
    /// </summary>
    module Queue =

        /// <summary>
        /// Enqueues an item.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="value">The queue value.</param>
        /// <param name="conn">The Redis connection.</param>
        let enqueue (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
            List.rpushOne key value conn

        /// <summary>
        /// Dequeues an item.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="conn">The Redis connection.</param>
        let dequeue (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
            List.lpop key conn

        /// <summary>
        /// Dequeues with blocking wait.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="timeout">The blocking timeout.</param>
        /// <param name="conn">The Redis connection.</param>
        let dequeueBlocking (key: string) (timeout: TimeSpan) (conn: RedisConnection) : FIO<string option, RedisError> =
            (List.blpop [key] timeout conn).Map(Option.map snd)

        /// <summary>
        /// Peeks at the front of the queue.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="conn">The Redis connection.</param>
        let peek (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
            List.lindex key 0L conn

        /// <summary>
        /// Gets the queue length.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="conn">The Redis connection.</param>
        let length (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
            List.llen key conn

        /// <summary>
        /// Clears the queue.
        /// </summary>
        /// <param name="key">The queue key.</param>
        /// <param name="conn">The Redis connection.</param>
        let clear (key: string) (conn: RedisConnection) : FIO<bool, RedisError> =
            Key.delete key conn
