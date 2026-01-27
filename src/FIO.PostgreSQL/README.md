# FIO.PostgreSQL

PostgreSQL database library for [FIO](https://github.com/fs-fio/fio), built on Npgsql.

## Installation

```bash
dotnet add package FIO.PostgreSQL
```

Requires the core `FIO` package.

## Features

- **Connection Pooling** - Configurable connection pool management
- **Query Execution** - Execute queries with typed result mapping
- **Transactions** - Atomic operations with automatic commit/rollback
- **Parameterized Queries** - Safe parameter binding with `@=` operator

## Quick Example

```fsharp
open FIO.DSL
open FIO.PostgreSQL

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

See the [FIO repository](https://github.com/fs-fio/fio) for full documentation and examples.

## License

MIT License - see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
