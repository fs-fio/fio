namespace FIO.Redis

open FIO.DSL

open StackExchange.Redis

open System.Threading.Channels

/// <summary>
/// Functions for Redis pub/sub operations.
/// </summary>
[<RequireQualifiedAccess>]
module PubSub =

    /// <summary>
    /// Publishes a message to a channel.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="message">The message to publish.</param>
    /// <param name="conn">The Redis connection.</param>
    let publish (channel: string) (message: string) (conn: RedisConnection) : FIO<int64, RedisError> =
        let sub = Redis.getSubscriber conn
        FIO.awaitGenericTask(
            sub.PublishAsync(RedisChannel(channel, RedisChannel.PatternMode.Literal), RedisValue.ofString message),
            fun e -> CommandFailed("PUBLISH", e))

    /// <summary>
    /// Publishes a byte array message.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="message">The message to publish.</param>
    /// <param name="conn">The Redis connection.</param>
    let publishBytes (channel: string) (message: byte[]) (conn: RedisConnection) : FIO<int64, RedisError> =
        let sub = Redis.getSubscriber conn
        FIO.awaitGenericTask(
            sub.PublishAsync(RedisChannel(channel, RedisChannel.PatternMode.Literal), RedisValue.ofBytes message),
            fun e -> CommandFailed("PUBLISH", e))

    /// <summary>
    /// Subscribes to a channel with a handler.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="handler">The message handler callback.</param>
    /// <param name="conn">The Redis connection.</param>
    let subscribe (channel: string) (handler: string -> string -> unit) (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let sub = Redis.getSubscriber conn
                sub.Subscribe(
                    RedisChannel(channel, RedisChannel.PatternMode.Literal),
                    fun ch msg -> handler (ch.ToString()) (msg.ToString()))),
            fun e -> SubscriptionFailed(channel, e))

    /// <summary>
    /// Subscribes to a channel pattern with a handler.
    /// </summary>
    /// <param name="pattern">The channel pattern.</param>
    /// <param name="handler">The message handler callback.</param>
    /// <param name="conn">The Redis connection.</param>
    let psubscribe (pattern: string) (handler: string -> string -> string -> unit) (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let sub = Redis.getSubscriber conn
                sub.Subscribe(
                    RedisChannel(pattern, RedisChannel.PatternMode.Pattern),
                    fun ch msg -> handler pattern (ch.ToString()) (msg.ToString()))),
            fun e -> SubscriptionFailed(pattern, e))

    /// <summary>
    /// Subscribes and returns a channel for async message reading.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="conn">The Redis connection.</param>
    let subscribeChannel (channel: string) (conn: RedisConnection) : FIO<Channel<string * string>, RedisError> =
        FIO.attempt(
            (fun () ->
                let msgChannel = Channel.CreateUnbounded<string * string>()
                let sub = Redis.getSubscriber conn
                sub.Subscribe(
                    RedisChannel(channel, RedisChannel.PatternMode.Literal),
                    fun ch msg ->
                        msgChannel.Writer.TryWrite((ch.ToString(), msg.ToString())) |> ignore)
                msgChannel),
            fun e -> SubscriptionFailed(channel, e))

    /// <summary>
    /// Subscribes asynchronously.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="handler">The message handler callback.</param>
    /// <param name="conn">The Redis connection.</param>
    let subscribeAsync (channel: string) (handler: string -> string -> unit) (conn: RedisConnection) : FIO<unit, RedisError> =
        let sub = Redis.getSubscriber conn
        let redisChannel = RedisChannel(channel, RedisChannel.PatternMode.Literal)
        let msgQueue = sub.Subscribe redisChannel
        FIO.attempt(
            (fun () ->
                msgQueue.OnMessage(fun msg -> handler (msg.Channel.ToString()) (msg.Message.ToString()))),
            fun e -> SubscriptionFailed(channel, e))

    /// <summary>
    /// Unsubscribes from a channel.
    /// </summary>
    /// <param name="channel">The channel name.</param>
    /// <param name="conn">The Redis connection.</param>
    let unsubscribe (channel: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let sub = Redis.getSubscriber conn
                sub.Unsubscribe(RedisChannel(channel, RedisChannel.PatternMode.Literal))),
            fun e -> CommandFailed("UNSUBSCRIBE", e))

    /// <summary>
    /// Unsubscribes from all channels.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let unsubscribeAll (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let sub = Redis.getSubscriber conn
                sub.UnsubscribeAll()),
            fun e -> CommandFailed("UNSUBSCRIBE", e))

    /// <summary>
    /// Unsubscribes from a pattern.
    /// </summary>
    /// <param name="pattern">The channel pattern.</param>
    /// <param name="conn">The Redis connection.</param>
    let punsubscribe (pattern: string) (conn: RedisConnection) : FIO<unit, RedisError> =
        FIO.attempt(
            (fun () ->
                let sub = Redis.getSubscriber conn
                sub.Unsubscribe(RedisChannel(pattern, RedisChannel.PatternMode.Pattern))),
            fun e -> CommandFailed("PUNSUBSCRIBE", e))

    /// <summary>
    /// Gets subscription counts.
    /// </summary>
    /// <param name="channels">The channel names.</param>
    /// <param name="conn">The Redis connection.</param>
    let numsub (channels: string list) (conn: RedisConnection) : FIO<(string * int64) list, RedisError> =
        FIO.attempt(
            (fun () ->
                let server =
                    conn.Multiplexer.GetEndPoints()
                    |> Array.head
                    |> conn.Multiplexer.GetServer
                channels
                |> List.map (fun ch -> ch, server.SubscriptionSubscriberCount(RedisChannel(ch, RedisChannel.PatternMode.Literal)))),
            fun e -> CommandFailed("PUBSUB NUMSUB", e))

    /// <summary>
    /// Gets active channels.
    /// </summary>
    /// <param name="pattern">The channel pattern.</param>
    /// <param name="conn">The Redis connection.</param>
    let channels (pattern: string option) (conn: RedisConnection) : FIO<string list, RedisError> =
        FIO.attempt(
            (fun () ->
                let server =
                    conn.Multiplexer.GetEndPoints()
                    |> Array.head
                    |> conn.Multiplexer.GetServer
                let p = pattern |> Option.defaultValue "*"
                server.SubscriptionChannels(RedisChannel(p, RedisChannel.PatternMode.Pattern))
                |> Array.map (fun ch -> ch.ToString())
                |> Array.toList),
            fun e -> CommandFailed("PUBSUB CHANNELS", e))

    /// <summary>
    /// Gets number of pattern subscriptions.
    /// </summary>
    /// <param name="conn">The Redis connection.</param>
    let numpat (conn: RedisConnection) : FIO<int64, RedisError> =
        FIO.attempt(
            (fun () ->
                let server =
                    conn.Multiplexer.GetEndPoints()
                    |> Array.head
                    |> conn.Multiplexer.GetServer
                server.SubscriptionPatternCount()),
            fun e -> CommandFailed("PUBSUB NUMPAT", e))
