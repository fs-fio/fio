# FSharp.FIO.Redis

Redis client library for FSharp.FIO built on StackExchange.Redis.

## Installation

```bash
dotnet add package FSharp.FIO.Redis
```

## Quick Start

```fsharp
open FSharp.FIO.Redis
open FSharp.FIO.DSL

// Connect to Redis
let program = fio {
    let! conn = Redis.connectString "localhost:6379"

    // String operations
    do! Str.set "mykey" "hello" conn
    let! value = Str.get "mykey" conn

    // Hash operations
    do! Hash.hset "user:1" "name" "Alice" conn
    let! name = Hash.hget "user:1" "name" conn

    // List operations
    do! List.lpush "queue" ["item1"; "item2"] conn
    let! item = List.rpop "queue" conn

    // Close connection
    do! Redis.close conn
    return value
}
```

## Modules

### Connection (Redis)
- `connect` - Connect with configuration
- `connectString` - Connect with connection string
- `close` - Close connection
- `withConnection` - Scoped connection

### Strings (Str)
- `get`, `set`, `setEx`, `setNx`
- `incr`, `incrBy`, `decr`, `decrBy`
- `append`, `strlen`, `getRange`
- `mget`, `mset`

### Hashes (Hash)
- `hget`, `hset`, `hsetNx`
- `hmget`, `hmset`, `hgetAll`
- `hdel`, `hexists`
- `hkeys`, `hvals`, `hlen`
- `hincrBy`, `hincrByFloat`

### Lists (List)
- `lpush`, `rpush`, `lpushx`, `rpushx`
- `lpop`, `rpop`, `blpop`, `brpop`
- `lrange`, `llen`, `lindex`
- `lset`, `lrem`, `ltrim`

### Sets (Set)
- `sadd`, `srem`, `smembers`
- `sismember`, `scard`
- `spop`, `srandmember`
- `sunion`, `sinter`, `sdiff`

### Sorted Sets (ZSet)
- `zadd`, `zrem`, `zscore`, `zrank`
- `zrange`, `zrevrange`
- `zrangeByScore`
- `zpopmin`, `zpopmax`
- `zincrby`, `zcard`, `zcount`

### Keys (Key)
- `del`, `exists`, `type'`
- `expire`, `expireAt`, `ttl`, `persist`
- `rename`, `renameNx`
- `scan`, `keys`

### Pub/Sub (PubSub)
- `publish`
- `subscribe`, `psubscribe`
- `unsubscribe`

### Streams (Stream)
- `xadd`, `xread`, `xrange`
- `xgroupCreate`, `xreadGroup`
- `xack`, `xpending`, `xclaim`

### Transactions (Tx)
- `multi` - Execute transaction
- `queueStringSet`, `queueHashSet`, etc.
- Condition-based (optimistic locking)

## High-Level DSL

### Cache
```fsharp
let! value = Cache.getOrSet "key" (TimeSpan.FromMinutes 5.0) compute conn
do! Cache.invalidate "key" conn
```

### Rate Limiting
```fsharp
let! (allowed, count) = RateLimit.check "api:user:1" 100L (TimeSpan.FromMinutes 1.0) conn
```

### Distributed Locks
```fsharp
let! result = Lock.withLock "resource" "lock-id" (TimeSpan.FromSeconds 30.0) action conn
```

### Leaderboard
```fsharp
do! Leaderboard.setScore "game:scores" "player1" 1500.0 conn
let! top10 = Leaderboard.getTop "game:scores" 10 conn
```

### Session
```fsharp
do! Session.create sessionId (Map.ofList ["user", "123"]) (TimeSpan.FromHours 1.0) conn
let! data = Session.get sessionId conn
```

### Queue
```fsharp
do! Queue.enqueue "jobs" "task1" conn
let! job = Queue.dequeue "jobs" conn
```

## Configuration

```fsharp
let config =
    RedisConfig.create "localhost:6379"
    |> RedisConfig.withPassword "secret"
    |> RedisConfig.withDatabase 1
    |> RedisConfig.withSsl
    |> RedisConfig.withConnectTimeout 5000

let! conn = Redis.connect config
```

## Error Handling

```fsharp
type RedisError =
    | ConnectionFailed of endpoint: string * exn
    | CommandFailed of command: string * exn
    | KeyNotFound of key: string
    | TransactionAborted of exn
    | SubscriptionFailed of channel: string * exn
    | StreamError of stream: string * exn
    | Timeout of operation: string
    | GeneralError of exn
```

## License

MIT
