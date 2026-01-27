namespace FSharp.FIO.Redis

open FSharp.FIO.DSL

open StackExchange.Redis

open System

/// <summary>
/// Represents a stream entry with ID and field-value pairs.
/// </summary>
type StreamEntry =
    {
        /// <summary>
        /// The stream entry ID.
        /// </summary>
        Id: string
        /// <summary>
        /// The field-value pairs.
        /// </summary>
        Fields: (string * string) list
    }

/// <summary>
/// Functions for stream entry conversion.
/// </summary>
module StreamEntry =

    /// <summary>
    /// Creates a stream entry from StackExchange.Redis entry.
    /// </summary>
    /// <param name="entry">The StackExchange.Redis stream entry.</param>
    /// <returns>A converted StreamEntry.</returns>
    let fromRedis (entry: StackExchange.Redis.StreamEntry) : StreamEntry =
        {
            Id = entry.Id.ToString()
            Fields =
                entry.Values
                |> Array.choose (fun nv ->
                    match RedisValue.toString nv.Name, RedisValue.toString nv.Value with
                    | Some n, Some v -> Some (n, v)
                    | _ -> None)
                |> Array.toList
        }

/// <summary>
/// Represents pending entry info for consumer groups.
/// </summary>
type PendingEntry =
    {
        /// <summary>
        /// The pending entry ID.
        /// </summary>
        Id: string
        /// <summary>
        /// The consumer name.
        /// </summary>
        Consumer: string
        /// <summary>
        /// The idle time since last delivery.
        /// </summary>
        IdleTime: TimeSpan
        /// <summary>
        /// The number of times the message was delivered.
        /// </summary>
        DeliveryCount: int64
    }

/// <summary>
/// Functions for Redis stream operations.
/// </summary>
[<RequireQualifiedAccess>]
module Stream =

    /// <summary>
    /// Adds an entry to a stream with auto-generated ID.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="fields">The field-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xadd (key: string) (fields: (string * string) list) (conn: RedisConnection) : FIO<string, RedisError> =
        let db = Redis.getDatabase conn
        let entries =
            fields
            |> List.map (fun (n, v) -> NameValueEntry(RedisValue.ofString n, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitGenericTask(
            db.StreamAddAsync(RedisKey.op_Implicit key, entries),
            fun e -> StreamError(key, e))
            .Map(fun id -> id.ToString())

    /// <summary>
    /// Adds an entry with a specific ID.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="id">The entry ID.</param>
    /// <param name="fields">The field-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xaddWithId (key: string) (id: string) (fields: (string * string) list) (conn: RedisConnection) : FIO<string, RedisError> =
        let db = Redis.getDatabase conn
        let entries =
            fields
            |> List.map (fun (n, v) -> NameValueEntry(RedisValue.ofString n, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitGenericTask(
            db.StreamAddAsync(RedisKey.op_Implicit key, entries, Nullable (RedisValue.op_Implicit id)),
            fun e -> StreamError(key, e))
            .Map(fun id -> id.ToString())

    /// <summary>
    /// Adds an entry with max length trimming.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="maxLen">The maximum stream length.</param>
    /// <param name="fields">The field-value pairs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xaddMaxLen (key: string) (maxLen: int) (fields: (string * string) list) (conn: RedisConnection) : FIO<string, RedisError> =
        let db = Redis.getDatabase conn
        let entries =
            fields
            |> List.map (fun (n, v) -> NameValueEntry(RedisValue.ofString n, RedisValue.ofString v))
            |> List.toArray
        FIO.awaitGenericTask(
            db.StreamAddAsync(RedisKey.op_Implicit key, entries, Nullable(), Nullable maxLen),
            fun e -> StreamError(key, e))
            .Map(fun id -> id.ToString())

    /// <summary>
    /// Reads entries from a stream.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="position">The stream position.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xread (key: string) (position: string) (count: int option) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let pos = RedisValue.op_Implicit position
        let cnt = count |> Option.map Nullable |> Option.defaultValue (Nullable())
        FIO.awaitGenericTask(
            db.StreamReadAsync(RedisKey.op_Implicit key, pos, cnt),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                if isNull entries then []
                else entries |> Array.map StreamEntry.fromRedis |> Array.toList)

    /// <summary>
    /// Reads from multiple streams.
    /// </summary>
    /// <param name="keys">The stream keys with positions.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xreadMulti (keys: (string * string) list) (count: int option) (conn: RedisConnection) : FIO<(string * StreamEntry list) list, RedisError> =
        let db = Redis.getDatabase conn
        let positions =
            keys
            |> List.map (fun (k, pos) -> StreamPosition(RedisKey.op_Implicit k, RedisValue.op_Implicit pos))
            |> List.toArray
        let cnt = count |> Option.map Nullable |> Option.defaultValue (Nullable())
        FIO.awaitGenericTask(
            db.StreamReadAsync(positions, cnt),
            fun e -> StreamError("multi", e))
            .Map(fun (results: RedisStream array) ->
                if isNull results then []
                else
                    results
                    |> Array.map (fun r ->
                        r.Key.ToString(),
                        r.Entries |> Array.map StreamEntry.fromRedis |> Array.toList)
                    |> Array.toList)

    /// <summary>
    /// Blocking read from streams (note: StackExchange.Redis doesn't support blocking reads, this is non-blocking).
    /// </summary>
    /// <param name="keys">The stream keys with positions.</param>
    /// <param name="timeout">The blocking timeout.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xreadBlock (keys: (string * string) list) (timeout: TimeSpan) (count: int option) (conn: RedisConnection) : FIO<(string * StreamEntry list) list, RedisError> =
        // StackExchange.Redis doesn't have a blocking XREAD - use regular xreadMulti
        xreadMulti keys count conn

    /// <summary>
    /// Gets entries in a range.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="min">The minimum ID.</param>
    /// <param name="max">The maximum ID.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xrange (key: string) (min: string) (max: string) (count: int option) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let cnt = count |> Option.map Nullable |> Option.defaultValue (Nullable())
        FIO.awaitGenericTask(
            db.StreamRangeAsync(RedisKey.op_Implicit key, Nullable (RedisValue.op_Implicit min), Nullable (RedisValue.op_Implicit max), cnt),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                if isNull entries then []
                else entries |> Array.map StreamEntry.fromRedis |> Array.toList)

    /// <summary>
    /// Gets entries in reverse range.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="max">The maximum ID.</param>
    /// <param name="min">The minimum ID.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xrevrange (key: string) (max: string) (min: string) (count: int option) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let cnt = count |> Option.map Nullable |> Option.defaultValue (Nullable())
        FIO.awaitGenericTask(
            db.StreamRangeAsync(RedisKey.op_Implicit key, Nullable (RedisValue.op_Implicit max), Nullable (RedisValue.op_Implicit min), cnt, Order.Descending),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                if isNull entries then []
                else entries |> Array.map StreamEntry.fromRedis |> Array.toList)

    /// <summary>
    /// Gets the length of a stream.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="conn">The Redis connection.</param>
    let xlen (key: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamLengthAsync(RedisKey.op_Implicit key),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Deletes entries from a stream.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="ids">The entry IDs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xdel (key: string) (ids: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisIds = ids |> List.map (fun id -> RedisValue.op_Implicit id) |> List.toArray
        FIO.awaitGenericTask(
            db.StreamDeleteAsync(RedisKey.op_Implicit key, redisIds),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Trims a stream to a maximum length.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="maxLen">The maximum stream length.</param>
    /// <param name="approximate">Whether to use approximate trimming.</param>
    /// <param name="conn">The Redis connection.</param>
    let xtrim (key: string) (maxLen: int) (approximate: bool) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamTrimAsync(RedisKey.op_Implicit key, maxLen, approximate),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Creates a consumer group.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="position">The stream position.</param>
    /// <param name="mkStream">Whether to create the stream if it doesn't exist.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupCreate (key: string) (group: string) (position: string) (mkStream: bool) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamCreateConsumerGroupAsync(RedisKey.op_Implicit key, RedisValue.ofString group, Nullable (RedisValue.op_Implicit position), mkStream),
            fun e -> StreamError(key, e))
            .Map(fun _ -> ())

    /// <summary>
    /// Creates a consumer group starting from the beginning.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="mkStream">Whether to create the stream if it doesn't exist.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupCreateFromBeginning (key: string) (group: string) (mkStream: bool) (conn: RedisConnection) : FIO<unit, RedisError> =
        xgroupCreate key group "0-0" mkStream conn

    /// <summary>
    /// Creates a consumer group starting from the end (new messages only).
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="mkStream">Whether to create the stream if it doesn't exist.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupCreateFromEnd (key: string) (group: string) (mkStream: bool) (conn: RedisConnection) : FIO<unit, RedisError> =
        xgroupCreate key group "$" mkStream conn

    /// <summary>
    /// Destroys a consumer group.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupDestroy (key: string) (group: string) (conn: RedisConnection) : FIO<bool, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamDeleteConsumerGroupAsync(RedisKey.op_Implicit key, RedisValue.ofString group),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Deletes a consumer from a group.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupDelConsumer (key: string) (group: string) (consumer: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamDeleteConsumerAsync(RedisKey.op_Implicit key, RedisValue.ofString group, RedisValue.ofString consumer),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Sets the group's last delivered ID.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="id">The entry ID.</param>
    /// <param name="conn">The Redis connection.</param>
    let xgroupSetId (key: string) (group: string) (id: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamConsumerGroupSetPositionAsync(RedisKey.op_Implicit key, RedisValue.ofString group, RedisValue.op_Implicit id),
            fun e -> StreamError(key, e))
            .Map(fun _ -> ())

    /// <summary>
    /// Reads from a stream as a consumer group member.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="position">The stream position.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xreadGroup (key: string) (group: string) (consumer: string) (position: string) (count: int option) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let pos = RedisValue.op_Implicit position
        let cnt = count |> Option.map Nullable |> Option.defaultValue (Nullable())
        FIO.awaitGenericTask(
            db.StreamReadGroupAsync(RedisKey.op_Implicit key, RedisValue.ofString group, RedisValue.ofString consumer, pos, cnt),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                if isNull entries then []
                else entries |> Array.map StreamEntry.fromRedis |> Array.toList)

    /// <summary>
    /// Reads new messages for a consumer group member.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xreadGroupNew (key: string) (group: string) (consumer: string) (count: int option) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        xreadGroup key group consumer ">" count conn

    /// <summary>
    /// Acknowledges messages.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="ids">The entry IDs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xack (key: string) (group: string) (ids: string list) (conn: RedisConnection) : FIO<int64, RedisError> =
        let db = Redis.getDatabase conn
        let redisIds = ids |> List.map (fun id -> RedisValue.op_Implicit id) |> List.toArray
        FIO.awaitGenericTask(
            db.StreamAcknowledgeAsync(RedisKey.op_Implicit key, RedisValue.ofString group, redisIds),
            fun e -> StreamError(key, e))

    /// <summary>
    /// Gets pending entries summary.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="conn">The Redis connection.</param>
    let xpending (key: string) (group: string) (conn: RedisConnection) : FIO<int64 * string option * string option * (string * int64) list, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamPendingAsync(RedisKey.op_Implicit key, RedisValue.ofString group),
            fun e -> StreamError(key, e))
            .Map(fun info ->
                let minId = if info.LowestPendingMessageId.IsNull then None else Some (info.LowestPendingMessageId.ToString())
                let maxId = if info.HighestPendingMessageId.IsNull then None else Some (info.HighestPendingMessageId.ToString())
                let consumers =
                    info.Consumers
                    |> Array.map (fun c -> (c.Name.ToString(), int64 c.PendingMessageCount))
                    |> Array.toList
                (info.PendingMessageCount, minId, maxId, consumers))

    /// <summary>
    /// Gets pending entries detail.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="min">The minimum ID.</param>
    /// <param name="max">The maximum ID.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="conn">The Redis connection.</param>
    let xpendingRange (key: string) (group: string) (min: string) (max: string) (count: int) (consumer: string option) (conn: RedisConnection) : FIO<PendingEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let consumerValue = consumer |> Option.map RedisValue.ofString |> Option.defaultValue (RedisValue.Null)
        FIO.awaitGenericTask(
            db.StreamPendingMessagesAsync(RedisKey.op_Implicit key, RedisValue.ofString group, count, consumerValue, Nullable (RedisValue.op_Implicit min), Nullable (RedisValue.op_Implicit max)),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                entries
                |> Array.map (fun e ->
                    {
                        Id = e.MessageId.ToString()
                        Consumer = e.ConsumerName.ToString()
                        IdleTime = TimeSpan.FromMilliseconds(float e.IdleTimeInMilliseconds)
                        DeliveryCount = int64 e.DeliveryCount
                    })
                |> Array.toList)

    /// <summary>
    /// Claims pending messages.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="minIdleTime">The minimum idle time.</param>
    /// <param name="ids">The entry IDs.</param>
    /// <param name="conn">The Redis connection.</param>
    let xclaim (key: string) (group: string) (consumer: string) (minIdleTime: TimeSpan) (ids: string list) (conn: RedisConnection) : FIO<StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let redisIds = ids |> List.map (fun id -> RedisValue.op_Implicit id) |> List.toArray
        FIO.awaitGenericTask(
            db.StreamClaimAsync(RedisKey.op_Implicit key, RedisValue.ofString group, RedisValue.ofString consumer, int64 minIdleTime.TotalMilliseconds, redisIds),
            fun e -> StreamError(key, e))
            .Map(fun entries ->
                entries |> Array.map StreamEntry.fromRedis |> Array.toList)

    /// <summary>
    /// Auto-claims pending messages.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="group">The consumer group name.</param>
    /// <param name="consumer">The consumer name.</param>
    /// <param name="minIdleTime">The minimum idle time.</param>
    /// <param name="start">The starting entry ID.</param>
    /// <param name="count">The maximum number of entries.</param>
    /// <param name="conn">The Redis connection.</param>
    let xautoclaim (key: string) (group: string) (consumer: string) (minIdleTime: TimeSpan) (start: string) (count: int option) (conn: RedisConnection) : FIO<string * StreamEntry list, RedisError> =
        let db = Redis.getDatabase conn
        let cnt = count |> Option.defaultValue 100
        FIO.awaitGenericTask(
            db.StreamAutoClaimAsync(RedisKey.op_Implicit key, RedisValue.ofString group, RedisValue.ofString consumer, int64 minIdleTime.TotalMilliseconds, RedisValue.op_Implicit start, cnt),
            fun e -> StreamError(key, e))
            .Map(fun result ->
                let nextStart = result.NextStartId.ToString()
                let entries = result.ClaimedEntries |> Array.map StreamEntry.fromRedis |> Array.toList
                nextStart, entries)

    /// <summary>
    /// Gets stream info.
    /// </summary>
    /// <param name="key">The stream key.</param>
    /// <param name="conn">The Redis connection.</param>
    let xinfo (key: string) (conn: RedisConnection) : FIO<Map<string, string>, RedisError> =
        let db = Redis.getDatabase conn
        FIO.awaitGenericTask(
            db.StreamInfoAsync(RedisKey.op_Implicit key),
            fun e -> StreamError(key, e))
            .Map(fun info ->
                [
                    "length", string info.Length
                    "radix-tree-keys", string info.RadixTreeKeys
                    "radix-tree-nodes", string info.RadixTreeNodes
                    "groups", string info.ConsumerGroupCount
                    "first-entry", if info.FirstEntry.IsNull then "" else info.FirstEntry.Id.ToString()
                    "last-entry", if info.LastEntry.IsNull then "" else info.LastEntry.Id.ToString()
                ]
                |> Map.ofList)
