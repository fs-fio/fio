# FIO.Redis

Redis client library for [FIO](https://github.com/fs-fio/fio), the type-safe functional effect system for F#.

## Installation

```bash
dotnet add package FIO.Redis
```

Requires the core `FIO` package.

## Features

- **String Operations** - Get, set, increment, append
- **Hash Operations** - Hash maps with field operations
- **List Operations** - Push, pop, range queries
- **Set Operations** - Add, remove, unions, intersections
- **Sorted Set Operations** - Scored sets with range queries
- **Pub/Sub** - Publish and subscribe to channels
- **Streams** - Stream operations with consumer groups
- **Transactions** - MULTI/EXEC support

## Quick Example

```fsharp
open FIO.DSL
open FIO.Redis

let program = fio {
    let! conn = Redis.connectString "localhost:6379"
    do! Str.set "mykey" "hello" conn
    let! value = Str.get "mykey" conn
    do! Redis.close conn
    return value
}
```

## Documentation

See the [FIO repository](https://github.com/fs-fio/fio) for full documentation.

## License

MIT License - see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
