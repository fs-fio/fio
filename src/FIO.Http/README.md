# FIO.Http

HTTP server library for [FIO](https://github.com/fs-fio/fio), built on ASP.NET Core Kestrel.

## Installation

```bash
dotnet add package FIO.Http
```

Requires the core `FIO` package.

## Features

- **Composable Routes** - Build routes with `++` operator
- **HTTP Handlers** - Functional request handlers returning FIO effects
- **Middleware** - Before, after, and around middleware composition
- **Path Parameters** - Type-safe path parameter extraction

## Quick Example

```fsharp
open FIO.Http
open FIO.Http.RoutesOperators

let routes =
    GET "/" (HttpHandler.text "Hello!")
    ++ GET "/json" (HttpHandler.okJson {| msg = "Hello" |})

let config = ServerConfig.defaultConfig
Server.runServer config routes
```

## Documentation

See the [FIO repository](https://github.com/fs-fio/fio) for full documentation and examples.

## License

MIT License - see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
