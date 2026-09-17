# FIO.WebSockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.WebSockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.WebSockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

WebSockets for [FIO](https://github.com/fs-fio/fio). Open connections, send frames, and pattern
match on incoming messages as composable effects — connections are scoped and released for you,
and failures surface as a typed `WsError` rather than raw exceptions.

- **Client** — `WebSocketClient.connectDefault` opens a connection (dispose it with `use!`), or
  `withConnectionString` scopes it and closes it for you
- **Server** — `WebSocketServer.start` / `acceptDefault` / `close` for manual control, or
  `WebSocketServer.serve` / `acceptLoop` to run a handler per connection, stopping the listener when interrupted;
  accepted sockets expose the peer's `RemoteEndPoint` and the `LocalEndPoint`
- **Typed messages** — match on `Frame(Text …)` / `Frame(Binary …)` / `ConnectionClosed`
- **Custom codecs** — send and receive typed payloads

## Install

```bash
dotnet add package FSharp.FIO.WebSockets
```

## Quick Start

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

## Server

```fsharp
open FIO.DSL
open FIO.WebSockets

let server = fio {
    let! listener = WebSocketServer.start "http://localhost:8080/ws/"
    let! webSocket = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig

    match! webSocket.ReceiveMessage() with
    | Frame(Text text) -> do! webSocket.SendText $"echo: {text}"
    | _ -> do! WebSocketServer.close listener
}
```

## Errors

Operations fail with a typed `WsError`; each case says what went wrong, so a receive loop can decide
without inspecting messages:

| Case | Raised by |
|------|-----------|
| `Closed` | the peer closed (`Receive` with a codec, `ReceiveString`/`ReceiveBytes`/`ReceiveJson`), a send after the peer's close frame, or any send/receive/close on a socket that is already closed or aborted |
| `CodecError` | encoding or decoding a payload, including a frame of the wrong kind |
| `ReceiveFailed` / `SendFailed` | a transport fault while receiving or sending |
| `ConnectionFailed` | connecting, listening, or accepting |
| `TimeoutError` | the configured send or receive timeout elapsed |
| `MessageTooLarge` | a message exceeded `MaxMessageSize` |
| `GeneralError` | anything unclassified |

`WsError.fromException` / `WsError.toException` bridge raw exceptions. A chat-style loop that stops on
close and skips malformed messages is a `CatchAll` away:

```fsharp
socket.Receive(codec).Map(Message).CatchAll(function
    | Closed _ -> FIO.succeed PeerClosed
    | CodecError reason -> FIO.succeed (Unreadable reason)
    | error -> FIO.fail error)
```

## Closing

`withConnection` and `acceptLoop` close the socket for you, but only when it can still complete the
closing handshake. Interrupting a fiber that is blocked in `ReceiveMessage` — losing a race, `Stop()` —
aborts the connection instead. For a graceful close while a reader is pending, call `Close` from another
fiber first: it closes the outgoing side, and the pending receive then observes the peer's close frame as
`ConnectionClosed`.

The handshake waits for the peer's close frame, so `Close` is bounded by `SendTimeout` like a send: a
peer that has stopped reading makes it fail with `TimeoutError` and the connection is aborted, instead
of holding the finalizer — and with it an app's shutdown — open indefinitely.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
