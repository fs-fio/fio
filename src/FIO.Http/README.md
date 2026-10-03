# FIO.Http

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.Http.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.Http)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

HTTP server for [FIO](https://github.com/fs-fio/fio), built on [Kestrel](https://learn.microsoft.com/aspnet/core/fundamentals/servers/kestrel).
Routes and handlers are plain values you compose, so an endpoint is a description you build
up and hand to the server.

- **Composable routes** — build a routing table with `get` / `post` and the `++` operator
- **Functional handlers** — `HttpRequest -> FIO<HttpResponse, exn>`, with JSON and text helpers
- **Middleware** — attach cross-cutting behavior with `@@` (e.g. `Middleware.before`)
- **Path & query parameters** — typed access to request data

## Install

```bash
dotnet add package FSharp.FIO.Http --prerelease
```

## Quick Start

```fsharp
open FIO.App
open FIO.Http
open FIO.Http.SimpleRoutes
open FIO.Http.RoutesOperators

type App() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        let routes =
            get "/" (HttpHandler.text "Hello!")
            ++ get "/json" (HttpHandler.okJson {| msg = "Hello" |})

        Server.runServer ServerConfig.defaultConfig routes

[<EntryPoint>]
let main _ = App().Run()
```

## Handlers & middleware

```fsharp
open System
open FIO.DSL
open FIO.Http
open FIO.Http.SimpleRoutes
open FIO.Http.RoutesOperators
open FIO.Http.MiddlewareOperators

// Read the 'n' query parameter and return its square, validating the input.
let squareHandler request =
    match Map.tryFind "n" request.QueryParams with
    | Some(value :: _) ->
        match Int32.TryParse value with
        | true, n -> FIO.succeed (Response.okJson {| input = n; square = n * n |})
        | _       -> FIO.succeed (Response.badRequestText "Query parameter 'n' must be an integer.")
    | _ -> FIO.succeed (Response.badRequestText "Missing query parameter 'n'.")

// Middleware that logs each incoming request.
let logging =
    Middleware.before (fun request ->
        FIO.attempt (fun () -> printfn $"{request.Method} {request.Path}") id)

let routes =
    get "/" (HttpHandler.text "Hello!")
    ++ get "/square" squareHandler

// Attach the middleware and hand the result to FIOApp's `effect`, as in Quick Start.
let server = Server.runServer ServerConfig.defaultConfig (routes @@ logging)
```

## Errors

The server pipeline is typed as `Routes<exn>`, so a handler is `HttpRequest -> FIO<HttpResponse, exn>`
and a handler that fails becomes a `500`. To model failures precisely *inside* a handler, use the
`HttpError` union — `InvalidRoute`, `ParsingFailed`, `HandlerFailed`, `MiddlewareFailed`,
`ServerFailed`, `BodyReadFailed`, `JsonFailed`, `TimeoutError`, `GeneralError` — and map back to
`exn` at the edge:

```fsharp
let handler request =
    (businessLogic request)              // FIO<HttpResponse, HttpError>
        .MapError HttpError.toException  // FIO<HttpResponse, exn>
```

`HttpError.fromException` bridges the other way.

A request the server refuses before any handler runs gets a client error, not a `500`: a body over
`MaxRequestBodySize` is answered `413`, a body shorter than its `Content-Length` or a path segment such
as `..` or one containing a backslash `400`, and a request still in flight when the server stops `503`.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Http) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
