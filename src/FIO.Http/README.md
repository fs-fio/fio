# FIO.Http

HTTP server for [FIO](https://github.com/fs-fio/fio), built on Kestrel.

```bash
dotnet add package FIO.Http
```

```fsharp
open FIO.Http
open FIO.Http.RoutesOperators

let routes =
    GET "/" (HttpHandler.text "Hello!")
    ++ GET "/json" (HttpHandler.okJson {| msg = "Hello" |})

Server.runServer ServerConfig.defaultConfig routes
```

Composable routes, functional handlers, middleware, path parameters.

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Http) · [MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
