# FIO.Http

HTTP server library for [FIO](https://github.com/fs-fio/fio), built on ASP.NET Core Kestrel.

## Install

```bash
dotnet add package FIO.Http
```

## Features

- **Composable routes** — build routes with the `++` operator
- **HTTP handlers** — functional request handlers returning FIO effects
- **Middleware** — before, after, and around middleware composition
- **Path parameters** — type-safe path parameter extraction

## Quick Start

```fsharp
open FIO.Http
open FIO.Http.RoutesOperators

let routes =
    GET "/" (HttpHandler.text "Hello!")
    ++ GET "/json" (HttpHandler.okJson {| msg = "Hello" |})

let config = ServerConfig.defaultConfig
Server.runServer config routes
```

See the [examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Http) for more.

## License

MIT — see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md).
