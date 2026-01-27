namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

open System

/// <summary>
/// Functions for Redis key operations.
/// </summary>
[<RequireQualifiedAccess>]
module Key =

    /// <summary>
    /// Deletes one or more keys.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let del (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.KeyDeleteAsync(redisKeys),
            fun e -> CommandFailed("DEL", e))

    /// <summary>
    /// Deletes a single key.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let delete (key: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyDeleteAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("DEL", e))

    /// <summary>
    /// Checks if a key exists.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let exists (key: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyExistsAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("EXISTS", e))

    /// <summary>
    /// Checks how many keys exist.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let existsMany (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.KeyExistsAsync(redisKeys),
            fun e -> CommandFailed("EXISTS", e))

    /// <summary>
    /// Sets a key's expiration time.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="conn">The Redis connection.</param>
    let expire (key: string) (expiry: TimeSpan) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyExpireAsync(RedisKey.op_Implicit key, Nullable expiry),
            fun e -> CommandFailed("EXPIRE", e))

    /// <summary>
    /// Sets a key's expiration to a specific date/time.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="expiry">The expiration time.</param>
    /// <param name="conn">The Redis connection.</param>
    let expireAt (key: string) (expiry: DateTime) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyExpireAsync(RedisKey.op_Implicit key, Nullable expiry),
            fun e -> CommandFailed("EXPIREAT", e))

    /// <summary>
    /// Gets the time-to-live for a key.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let ttl (key: string) (conn: RedisConnection) : FIO<TimeSpan option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyTimeToLiveAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("TTL", e))
            .Map(fun ts -> if ts.HasValue then Some ts.Value else None)

    /// <summary>
    /// Removes the expiration from a key (makes it persistent).
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let persist (key: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyPersistAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("PERSIST", e))

    /// <summary>
    /// Renames a key.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="newKey">The new key name.</param>
    /// <param name="conn">The Redis connection.</param>
    let rename (key: string) (newKey: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyRenameAsync(RedisKey.op_Implicit key, RedisKey.op_Implicit newKey),
            fun e -> CommandFailed("RENAME", e))
            .Map(fun _ -> ())

    /// <summary>
    /// Renames a key only if the new key doesn't exist.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="newKey">The new key name.</param>
    /// <param name="conn">The Redis connection.</param>
    let renameNx (key: string) (newKey: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyRenameAsync(RedisKey.op_Implicit key, RedisKey.op_Implicit newKey, When.NotExists),
            fun e -> CommandFailed("RENAMENX", e))

    /// <summary>
    /// Gets the type of a key.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let type' (key: string) (conn: RedisConnection) : FIO<string, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyTypeAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("TYPE", e))
            .Map(fun t -> t.ToString().ToLowerInvariant())

    /// <summary>
    /// Scans keys matching a pattern.
    /// </summary>
    /// <param name="pattern">The key pattern to match.</param>
    /// <param name="cursor">The scan cursor.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="conn">The Redis connection.</param>
    let scan (pattern: string) (cursor: int64) (count: int) (conn: RedisConnection) : FIO<int64 * string list, RedisError> =
        FIO.attempt(
            (fun () ->
                let db = Redis.getDatabase conn
                let server =
                    conn.Multiplexer.GetEndPoints()
                    |> Array.head
                    |> conn.Multiplexer.GetServer
                let keys =
                    server.Keys(conn.Database, RedisValue.op_Implicit pattern, count)
                    |> Seq.map (fun k -> k.ToString())
                    |> Seq.toList
                (0L, keys)),
            fun e -> CommandFailed("SCAN", e))

    /// <summary>
    /// Gets all keys matching a pattern (use with caution on large datasets).
    /// </summary>
    /// <param name="pattern">The key pattern to match.</param>
    /// <param name="conn">The Redis connection.</param>
    let keys (pattern: string) (conn: RedisConnection) : FIO<string list, RedisError> =
        FIO.attempt(
            (fun () ->
                let server =
                    conn.Multiplexer.GetEndPoints()
                    |> Array.head
                    |> conn.Multiplexer.GetServer
                server.Keys(conn.Database, RedisValue.op_Implicit pattern)
                |> Seq.map (fun k -> k.ToString())
                |> Seq.toList),
            fun e -> CommandFailed("KEYS", e))

    /// <summary>
    /// Gets a random key.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let randomKey (conn: RedisConnection) : FIO<string option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyRandomAsync(),
            fun e -> CommandFailed("RANDOMKEY", e))
            .Map(fun (k: RedisKey) ->
                let str = k.ToString()
                if System.String.IsNullOrEmpty str then None else Some str)

    /// <summary>
    /// Touches a key (updates last access time).
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let touch (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.KeyTouchAsync(redisKeys),
            fun e -> CommandFailed("TOUCH", e))

    /// <summary>
    /// Unlinks (async deletes) one or more keys.
    /// </summary>
    /// <param name="keys">The Redis keys.</param>
    /// <param name="conn">The Redis connection.</param>
    let unlink (keys: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisKeys = keys |> List.map (fun k -> RedisKey.op_Implicit k) |> List.toArray
        FIO.awaitGenericTask(
            db.KeyDeleteAsync(redisKeys, CommandFlags.FireAndForget),
            fun e -> CommandFailed("UNLINK", e))

    /// <summary>
    /// Dumps a key's value.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="conn">The Redis connection.</param>
    let dump (key: string) (conn: RedisConnection) : FIO<byte[] option, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyDumpAsync(RedisKey.op_Implicit key),
            fun e -> CommandFailed("DUMP", e))
            .Map(fun b -> if isNull b then None else Some b)

    /// <summary>
    /// Restores a key from a dump.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="ttl">The time-to-live.</param>
    /// <param name="value">The serialized value.</param>
    /// <param name="conn">The Redis connection.</param>
    let restore (key: string) (ttl: TimeSpan) (value: byte[]) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitTask(
            db.KeyRestoreAsync(RedisKey.op_Implicit key, value, Nullable ttl),
            fun e -> CommandFailed("RESTORE", e))

    /// <summary>
    /// Moves a key to another database.
    /// </summary>
    /// <param name="key">The Redis key.</param>
    /// <param name="database">The database index.</param>
    /// <param name="conn">The Redis connection.</param>
    let move (key: string) (database: int) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyMoveAsync(RedisKey.op_Implicit key, database),
            fun e -> CommandFailed("MOVE", e))

    /// <summary>
    /// Copies a key.
    /// </summary>
    /// <param name="source">The source key.</param>
    /// <param name="destination">The destination key.</param>
    /// <param name="replace">Whether to replace existing key.</param>
    /// <param name="conn">The Redis connection.</param>
    let copy (source: string) (destination: string) (replace: bool) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.KeyCopyAsync(RedisKey.op_Implicit source, RedisKey.op_Implicit destination, -1, replace),
            fun e -> CommandFailed("COPY", e))
