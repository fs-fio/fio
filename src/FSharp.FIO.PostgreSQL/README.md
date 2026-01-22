# FSharp.FIO.PostgreSQL

PostgreSQL database library for [FSharp.FIO](https://github.com/fio-fsharp/fio), built on Npgsql.

## Installation

```bash
dotnet add package FSharp.FIO.PostgreSQL
```

Requires the core `FSharp.FIO` package.

## Features

- **Connection Pooling** - Configurable connection pool management
- **Query Execution** - Execute queries with typed result mapping
- **Transactions** - Atomic operations with automatic commit/rollback
- **Parameterized Queries** - Safe parameter binding with `@=` operator

## Quick Example

```fsharp
open FSharp.FIO.DSL
open FSharp.FIO.PostgreSQL

let config = {
    ConnectionString = "Host=localhost;Database=mydb;Username=user;Password=pass"
    MinPoolSize = 5; MaxPoolSize = 20
    ConnectionLifetime = 300; CommandTimeout = 30
}

let pool = Pool.create config

let getUser id = fio {
    let sql = "SELECT id, name FROM users WHERE id = @id"
    return! Dsl.queryFirstWithParams sql ["id" @= id] userMapper pool
}
```

## Documentation

See the [FIO repository](https://github.com/fio-fsharp/fio) for full documentation and examples.

## License

MIT License - see [LICENSE.md](https://github.com/fio-fsharp/fio/blob/main/LICENSE.md)
