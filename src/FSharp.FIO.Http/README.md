# FSharp.FIO.Http

HTTP server library for [FSharp.FIO](https://github.com/fio-fsharp/fio), built on ASP.NET Core Kestrel.

## Installation

```bash
dotnet add package FSharp.FIO.Http
```

Requires the core `FSharp.FIO` package.

## Features

- **Composable Routes** - Build routes with `++` operator
- **HTTP Handlers** - Functional request handlers returning FIO effects
- **Middleware** - Before, after, and around middleware composition
- **Path Parameters** - Type-safe path parameter extraction

## Quick Example

```fsharp
open FSharp.FIO.Http
open FSharp.FIO.Http.RoutesOperators

let routes =
    GET "/" (HttpHandler.text "Hello!")
    ++ GET "/json" (HttpHandler.okJson {| msg = "Hello" |})

let config = ServerConfig.defaultConfig
Server.runServer config routes
```

## Documentation

See the [FIO repository](https://github.com/fio-fsharp/fio) for full documentation and examples.

## License

MIT License - see [LICENSE.md](https://github.com/fio-fsharp/fio/blob/main/LICENSE.md)
