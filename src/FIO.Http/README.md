# FIO.Http

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.Http.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.Http)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 11](https://img.shields.io/badge/.NET-11-512BD4.svg?logo=dotnet)

HTTP server for [FIO](https://github.com/fs-fio/fio), built on [Kestrel](https://learn.microsoft.com/aspnet/core/fundamentals/servers/kestrel).
Routes and handlers are plain values you compose and hand to the server.

- **Composable routes** — build a routing table with `get` / `post` and the `++` operator
- **Functional handlers** — `HttpRequest -> FIO<HttpResponse, exn>`, with JSON and text helpers
- **Middleware** — attach cross-cutting behavior with `@@` (e.g. `Middleware.before`)
- **Path & query parameters** — typed access to request data

> **Status: beta.** The API may still change between releases.

## Install

```bash
dotnet add package FSharp.FIO.Http --prerelease
```

## Quick start

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

## Handlers and middleware

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

JSON follows ASP.NET Core's web defaults: camelCase names out, any casing in.

## Errors

A handler that fails or throws is answered `500` and logged on standard error. To model failures precisely inside a
handler, use the `HttpError` union and map back to `exn` at the edge:

```fsharp
let handler request =
    (businessLogic request)              // FIO<HttpResponse, HttpError>
        .MapError HttpError.toException  // FIO<HttpResponse, exn>
```

## Stopping

`Server.stop` (and `runServer`, on interruption or the first Ctrl+C, SIGTERM or SIGQUIT) answers new requests `503`,
gives requests in flight `ShutdownTimeout` (10 s by default) to finish, then interrupts the rest. A stopped server
cannot be started again.

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Http) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
