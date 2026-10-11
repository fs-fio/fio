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

JSON follows ASP.NET Core's web defaults in both directions: `okJson` and the other JSON responses write
camelCase names, and `HttpHandler.jsonBody` binds a request body whatever the casing of its names, so a
client can post back what the server sent. Reading is lenient in the same way as ASP.NET Core's: numbers
written as strings (`"id": "1"`) are accepted. `HttpHandler.jsonBodyWith` takes other
`JsonSerializerOptions`; use it with the same options if you pass your own to
`KestrelBridge.handleRequestWithOptions`.

## Errors

The server pipeline is typed as `Routes<exn>`, so a handler is `HttpRequest -> FIO<HttpResponse, exn>`
and a handler that fails, or throws, becomes a `500` and is logged on standard error (without the
query string); a handler that interrupts itself is answered `503`. To model failures precisely *inside* a handler, use the
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
`MaxRequestBodySize` is answered `413` and a path segment containing a backslash `400`. Kestrel
resolves `.` and `..` itself before routing, so `/files/../secret` is routed as `/secret`; a body
shorter than its `Content-Length` gets no response when the client closes, or `408` when it stalls.

## Stopping

`Server.stop` — and `runServer`, which stops the server when it is interrupted or gets the first
Ctrl+C, SIGTERM or SIGQUIT (a second one ends the process) — answers new requests
`503` at once, gives requests in flight `ShutdownTimeout` (10 s by default; set it with
`ServerConfig.withShutdownTimeout`, and 0 or less waits indefinitely) to finish, then interrupts the
handlers still running, which are answered `503`. Their finalizers run. A handler is interrupted the
same way when its client disconnects, since nobody waits for the answer; wrap work that must finish
regardless in `FIO.uninterruptible`.

A server that was stopped cannot be started again: create a new one. The server listens only where
its `ServerConfig` says: `appsettings.json` and environment variables cannot add listeners or change
its limits or its stop timeout. Never call `stop` from one of the server's own handlers: it would
wait for that handler and then interrupt it. A handler that has returned but still awaits children
it forked is answered `503` at the cut, while its children unwind.

An error response replaces whatever the failed response had set: its headers are cleared and its body
is plain text. A response that fails after it has started — a stream that throws part-way — can no
longer change its status, so the connection is aborted and the client sees an incomplete response
instead of a truncated success.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Http) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
