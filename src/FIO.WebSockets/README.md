# FIO.WebSockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.WebSockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.WebSockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 11](https://img.shields.io/badge/.NET-11-512BD4.svg?logo=dotnet)

WebSockets for [FIO](https://github.com/fs-fio/fio). Open connections, send frames, and pattern match on incoming
messages as composable effects; connections are scoped and released for you, and failures surface as a typed `WsError`.

- **Client** — `WebSocketClient.connectDefault` opens a connection (dispose it with `use!`), or
  `withConnectionString` scopes it and closes it for you
- **Server** — `WebSocketServer.start` / `acceptDefault` / `close` for manual control, or `WebSocketServer.serve` /
  `acceptLoop` to run a handler per connection, shutting down gracefully when interrupted
- **Typed messages** — match on `Frame(Text …)` / `Frame(Binary …)` / `ConnectionClosed`
- **Custom codecs** — send and receive typed payloads

> **Status: beta.** The API may still change between releases.

## Install

```bash
dotnet add package FSharp.FIO.WebSockets --prerelease
```

## Quick start

```fsharp
open FIO.DSL
open FIO.WebSockets
open FIO.WebSockets.WebSocketExtensions

let client = fio {
    use! ws = WebSocketClient.connectDefault "ws://localhost:8080/ws"
    do! ws.SendString "Hello, server!"
    return! ws.ReceiveString()
}
```

The string, bytes and JSON helpers are extension members: open `FIO.WebSockets.WebSocketExtensions` to use them.

## Server

```fsharp
open FIO.DSL
open FIO.WebSockets

let server = fio {
    let! listener = WebSocketServer.start "http://localhost:8080/ws/"
    let! webSocket = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig

    match! webSocket.ReceiveMessage() with
    | Frame(Text text) -> do! webSocket.SendText $"echo: {text}"
    | _ -> ()

    do! webSocket.CloseIfOpen()
    do! webSocket.Dispose()
    do! WebSocketServer.close listener
}
```

With `start` and `accept` the listener and the connection are yours to close; `serve` and `acceptLoop` close the
connections for you.

The URL prefix's host is both the address bound and a filter on the `Host` header, so `http://127.0.0.1:8080/` answers a
client that dials `localhost` with 404. Use `+` to listen on every interface.

Known issue on Linux and macOS: `HttpListener` accepts before it has finished starting
([dotnet/runtime#28658](https://github.com/dotnet/runtime/issues/28658)), so a client connecting at that moment can make
`start` fail with `ConnectionFailed "Value cannot be null."`. The port stays bound until the next garbage collection, so
retry on another port or after `GC.Collect()` and `GC.WaitForPendingFinalizers()`.

## Errors

| Case | Raised by |
|------|-----------|
| `Closed` | the peer closed, or the socket is already closed or aborted |
| `CodecError` | encoding or decoding a payload |
| `ReceiveFailed` / `SendFailed` | a transport fault or a cancelled receive or send |
| `ConnectionFailed` | connecting, listening, or accepting |
| `TimeoutError` | the configured send or receive timeout elapsed |
| `MessageTooLarge` | a message exceeded `MaxMessageSize` |
| `GeneralError` | anything unclassified |

A receive loop that stops on close and skips malformed messages uses `TryReceive`:

```fsharp
let rec loop () = fio {
    match! socket.TryReceive codec with
    | Received message ->
        do! handle message
        return! loop ()
    | Undecodable reason ->
        do! report reason
        return! loop ()
    | Disconnected _ -> return ()
}
```

## Codecs

`Codec.frame`, `Codec.binary`, `Codec.text`, `Codec.json` and `Codec.jsonLine` are built in; `Codec.map` and
`Codec.compose` build on them, and `Codec.createPure` wraps your own format (Protobuf, say).

## Closing and shutdown

- Interrupting a fiber while it reads aborts the connection; to close gracefully, call `Close` from another fiber.
- `Close` is bounded by `SendTimeout`, and a send or receive that times out in flight aborts the connection.
- Interrupting `serve` or `acceptLoop` sends every connection a going-away close (1001), gives handlers
  `ShutdownTimeout` (10 s by default) to finish, then interrupts the rest.

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
