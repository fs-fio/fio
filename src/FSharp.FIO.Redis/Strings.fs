namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

open System
open System.Collections.Generic

/// <summary>
/// Functions for Redis string operations.
/// </summary>
[<RequireQualifiedAccess>]
module Str =

    /// <summary>
    /// Gets a string value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let get (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("GET", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets a byte array value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let getBytes (key: string) (conn: RedisConnection) : FIO<byte[] option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("GET", e))
            .Map RedisValue.toBytes

    /// <summary>
    /// Sets a string value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let set (key: string) (value: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("SET", e))
            .Map(fun _ -> ())

    /// <summary>
    /// Sets a byte array value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let setBytes (key: string) (value: byte[]) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofBytes value),
            fun e -> CommandFailed("SET", e))
            .Map(fun _ -> ())

    /// <summary>
    /// Sets a string value with expiration.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="conn">The Redis connection.</param>
    let setEx (key: string) (value: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value, Nullable expiry),
            fun e -> CommandFailed("SETEX", e))
            .Map(fun _ -> ())

    /// <summary>
    /// Sets a string value only if it doesn't exist.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let setNx (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value, Nullable(), When.NotExists),
            fun e -> CommandFailed("SETNX", e))

    /// <summary>
    /// Sets a string value only if it exists.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let setXx (key: string) (value: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAsync(RedisKey.op_Implicit key, RedisValue.ofString value, Nullable(), When.Exists),
            fun e -> CommandFailed("SET XX", e))

    /// <summary>
    /// Gets and sets a string value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let getSet (key: string) (value: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetAndGetAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("GETSET", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets and deletes a string value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let getDel (key: string) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetDeleteAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("GETDEL", e))
            .Map RedisValue.toString

    /// <summary>
    /// Gets and sets expiration.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="conn">The Redis connection.</param>
    let getEx (key: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetSetExpiryAsync(RedisKey.op_Implicit key, expiry),
            fun e -> CommandFailed("GETEX", e))
            .Map(fun _ -> None)  // StringGetSetExpiryAsync returns bool
        >>= fun _ -> get key conn

    /// <summary>
    /// Increments a value by 1.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let incr (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringIncrementAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("INCR", e))

    /// <summary>
    /// Increments a value by a specified amount.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let incrBy (key: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringIncrementAsync(RedisKey.op_Implicit key, amount),
            fun e -> CommandFailed("INCRBY", e))

    /// <summary>
    /// Increments a float value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let incrByFloat (key: string) (amount: float) (conn: RedisConnection) : FIO<float, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringIncrementAsync(RedisKey.op_Implicit key, amount),
            fun e -> CommandFailed("INCRBYFLOAT", e))

    /// <summary>
    /// Decrements a value by 1.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let decr (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringDecrementAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("DECR", e))

    /// <summary>
    /// Decrements a value by a specified amount.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="amount">The increment/decrement amount.</param>
    /// <param name="conn">The Redis connection.</param>
    let decrBy (key: string) (amount: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringDecrementAsync(RedisKey.op_Implicit key, amount),
            fun e -> CommandFailed("DECRBY", e))

    /// <summary>
    /// Appends a value to a string.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let append (key: string) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringAppendAsync(RedisKey.op_Implicit key, RedisValue.ofString value),
            fun e -> CommandFailed("APPEND", e))

    /// <summary>
    /// Gets the length of a string.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let strlen (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringLengthAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("STRLEN", e))

    /// <summary>
    /// Gets a substring.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let getRange (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<string, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetRangeAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("GETRANGE", e))
            .Map(fun v -> v.ToString())

    /// <summary>
    /// Sets a substring.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="offset">The bit/string offset.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let setRange (key: string) (offset: int64) (value: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetRangeAsync(RedisKey.op_Implicit key, offset, RedisValue.ofString value),
            fun e -> CommandFailed("SETRANGE", e))
            .Map(fun (v: RedisValue) -> int64 (v.ToString().Length))

    /// <summary>
    /// Gets multiple string values.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let mget (keys: string list) (conn: RedisConnection) : FIO<(string * string option) list, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.StringGetAsync redisKeys,
            fun e -> CommandFailed("MGET", e))
            .Map(fun values ->
                List.zip keys (values |> Array.map RedisValue.toString |> Array.toList))

    /// <summary>
    /// Sets multiple string values.
    /// </summary>
    /// <param name="pairs">The key-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let mset (pairs: (string * string) list) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        let kvs =
            pairs
            |> List.map (fun (k, v) ->
                KeyValuePair(RedisKey.op_Implicit k, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitGenericTask(
            db.StringSetAsync kvs,
            fun e -> CommandFailed("MSET", e))
            .Map(fun _ -> ())

    /// <summary>
    /// Sets multiple string values only if none exist.
    /// </summary>
    /// <param name="pairs">The key-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let msetNx (pairs: (string * string) list) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        let kvs =
            pairs
            |> List.map (fun (k, v) ->
                KeyValuePair(RedisKey.op_Implicit k, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitGenericTask(
            db.StringSetAsync(kvs, When.NotExists),
            fun e -> CommandFailed("MSETNX", e))

    /// <summary>
    /// Sets a bit.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="offset">The bit/string offset.</param>
    /// <param name="value">The string value.</param>
    /// <param name="conn">The Redis connection.</param>
    let setBit (key: string) (offset: int64) (value: bool) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringSetBitAsync(RedisKey.op_Implicit key, offset, value),
            fun e -> CommandFailed("SETBIT", e))

    /// <summary>
    /// Gets a bit.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="offset">The bit/string offset.</param>
    /// <param name="conn">The Redis connection.</param>
    let getBit (key: string) (offset: int64) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringGetBitAsync(RedisKey.op_Implicit key, offset),
            fun e -> CommandFailed("GETBIT", e))

    /// <summary>
    /// Counts bits set to 1.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let bitCount (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringBitCountAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("BITCOUNT", e))

    /// <summary>
    /// Counts bits in a range.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="start">The start index.</param>
    /// <param name="stop">The stop index.</param>
    /// <param name="conn">The Redis connection.</param>
    let bitCountRange (key: string) (start: int64) (stop: int64) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StringBitCountAsync(RedisKey.op_Implicit key, start, stop),
            fun e -> CommandFailed("BITCOUNT", e))
