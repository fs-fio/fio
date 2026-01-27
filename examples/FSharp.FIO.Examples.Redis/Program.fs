/// <summary>
/// FSharp.FIO.Redis example demonstrating Redis operations.
/// </summary>
module private FSharp.FIO.Examples.Redis

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Redis

open System

/// <summary>
/// Redis demo app showing strings, hashes, lists, sets, caching, and distributed locks.
/// </summary>
type private RedisApp() =
    inherit FIOApp<unit, RedisError>()
    
    let stringDemo conn = fio {
        printfn "\n=== Strings ==="
        do! Str.set "msg" "Hello, Redis!" conn
        let! msg = Str.get "msg" conn
        printfn "  msg = %s" (msg |> Option.defaultValue "")

        do! Str.setEx "temp" "expires soon" (TimeSpan.FromSeconds 10.0) conn
        do! Str.set "counter" "0" conn
        let! c1 = Str.incr "counter" conn
        let! c2 = Str.incrBy "counter" 10L conn
        printfn "  counter: 0 -> %d -> %d" c1 c2

        do! Str.mset ["a", "1"; "b", "2"] conn
        let! vals = Str.mget ["a"; "b"; "missing"] conn
        for k, v in vals do
            printfn "  %s = %s" k (v |> Option.defaultValue "(nil)")
    }

    let hashDemo conn = fio {
        printfn "\n=== Hashes ==="
        let key = "user:1"
        let data = ["name", "Alice"; "email", "alice@example.com"; "age", "30"]
        do! Hash.hmset key data conn
        printfn "  Created %s" key

        let! name = Hash.hget key "name" conn
        printfn "  name = %s" (name |> Option.defaultValue "")

        let! all = Hash.hgetAll key conn
        for KeyValue(f, v) in all do printfn "  %s: %s" f v

        let! _ = Hash.hincrBy key "visits" 1L conn
        let! visits = Hash.hget key "visits" conn
        printfn "  visits = %s" (visits |> Option.defaultValue "0")
    }

    let listDemo conn = fio {
        printfn "\n=== Lists ==="
        let key = "queue"
        let! _ = Key.delete key conn

        let! _ = List.rpush key ["task1"; "task2"; "task3"] conn
        let! _ = List.lpushOne key "urgent" conn
        let! len = List.llen key conn
        printfn "  Queue length: %d" len

        let! items = List.lrange key 0L -1L conn
        printfn "  Items: %s" (String.Join(", ", items))

        let! first = List.lpop key conn
        printfn "  Popped: %s" (first |> Option.defaultValue "")
    }

    let setDemo conn = fio {
        printfn "\n=== Sets ==="
        let k1, k2 = "tags:1", "tags:2"
        let! _ = Key.del [k1; k2] conn

        let! _ = Set.sadd k1 ["fsharp"; "redis"; "functional"] conn
        let! _ = Set.sadd k2 ["fsharp"; "redis"; "dotnet"] conn

        let! members = Set.smembers k1 conn
        printfn "  tags:1 = %s" (String.Join(", ", members))

        let! common = Set.sinter [k1; k2] conn
        printfn "  Common: %s" (String.Join(", ", common))

        let! all = Set.sunion [k1; k2] conn
        printfn "  All unique: %s" (String.Join(", ", all))
    }

    let cacheDemo conn = fio {
        printfn "\n=== Cache ==="
        let key = "cache:data"
        let! _ = Cache.invalidate key conn

        let compute () = fio {
            printfn "  (computing...)"
            return "computed-" + DateTime.Now.ToString("ss")
        }
        let ttl = TimeSpan.FromMinutes 5.0

        let! v1 = Cache.getOrSet key ttl compute conn
        printfn "  First call: %s" v1

        let! v2 = Cache.getOrSet key ttl compute conn
        printfn "  Second call (cached): %s" v2

        let! _ = Cache.invalidate key conn
        let! v3 = Cache.getOrSet key ttl compute conn
        printfn "  After invalidate: %s" v3
    }

    let lockDemo conn = fio {
        printfn "\n=== Distributed Lock ==="
        let resource = "resource:critical"
        let lockId = Guid.NewGuid().ToString()
        let expiry = TimeSpan.FromSeconds 30.0

        let work () = fio {
            printfn "  (in critical section)"
            return "done"
        }
        let! result = Lock.withLock resource lockId expiry work conn
        match result with
        | Some r -> printfn "  Result: %s" r
        | None -> printfn "  Lock not acquired"
    }

    let cleanup conn = fio {
        let keys = ["msg"; "temp"; "counter"; "a"; "b"; "user:1"; "queue"; "tags:1"; "tags:2"; "cache:data"]
        let! n = Key.del keys conn
        printfn "\nCleanup: deleted %d keys" n
    }

    let redisDemo =
        fio {
            let! conn = Redis.connectString "localhost:6379"
            let! latency = Redis.ping conn
            printfn "Connected (ping: %.1fms)" latency.TotalMilliseconds

            do! stringDemo conn
            do! hashDemo conn
            do! listDemo conn
            do! setDemo conn
            do! cacheDemo conn
            do! lockDemo conn
            do! cleanup conn

            do! Redis.close conn
            printfn "\nDone."
        }

    override _.effect =
        redisDemo

[<EntryPoint>]
let main _ =
    printfn "Requires Redis: docker run --name redis -p 6379:6379 -d redis\n"
    RedisApp().Run()
