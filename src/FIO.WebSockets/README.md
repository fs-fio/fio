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
  `WebSocketServer.serve` / `acceptLoop` to run a handler per connection, shutting down gracefully when interrupted;
  accepted sockets expose the peer's `RemoteEndPoint` and the `LocalEndPoint`
- **Typed messages** — match on `Frame(Text …)` / `Frame(Binary …)` / `ConnectionClosed`
- **Custom codecs** — send and receive typed payloads; `TryReceive` reports a close or an undecodable
  frame as an outcome instead of a failure

## Install

```bash
dotnet add package FSharp.FIO.WebSockets --prerelease
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

`start` and `serve` take an `HttpListener` URL prefix, and its host does two jobs: it is the address
bound — only the first one a name resolves to — and a filter on each request's `Host` header. So
`http://localhost:8080/` may bind only `::1`, and `http://127.0.0.1:8080/` answers a client that dials
`localhost` with 404. To listen on every interface, use `+`; `0.0.0.0` and `[::]` mean the same. On
Linux and macOS `+` binds IPv4 only; on Windows it needs a URL reservation (`netsh http add urlacl`) or
an elevated process.

A request that is not a WebSocket upgrade, or whose handshake is invalid (a missing key or version, or
a subprotocol the client did not offer), is answered `400` and closed, and `accept` fails with
`ConnectionFailed`; `acceptLoop` and `serve` log it and keep serving.

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
| `MessageTooLarge` | a message exceeded `MaxMessageSize`; the connection is aborted, since the rest of the message is unread |
| `GeneralError` | anything unclassified |

`WsError.fromException` / `WsError.toException` bridge raw exceptions. A chat-style loop that stops on
close and skips malformed messages uses `TryReceive`, which yields those two as outcomes and fails only
on the rest:

```fsharp
let rec loop () = fio {
    match! socket.TryReceive codec with
    | Received message ->
        do! handle message
        return! loop ()
    | Undecodable reason ->
        do! report reason
        return! loop ()
    | PeerClosed _ -> return ()
}
```

## Closing

`withConnection` and `acceptLoop` close the socket for you with `CloseIfOpen`, which closes a connection
that is still open — answering a close the peer began — and logs rather than fails when that goes
wrong; call it yourself for the same best-effort close. `withConnection` owns the socket from before it
connects, so an interrupt while connecting disposes it, and `serve` and `acceptLoop` hand every
accepted connection to a handler that closes it, even when interrupted mid-accept.

A close needs a socket that can still complete the closing handshake: interrupting a fiber that is
blocked in `ReceiveMessage` — losing a race, `Stop()` — aborts the connection instead. For a graceful
close while a reader is pending, call `Close` from another fiber first: it closes the outgoing side, and
the pending receive then observes the peer's close frame as `ConnectionClosed`.

The handshake waits for the peer's close frame, so `Close` is bounded by `SendTimeout` like a send: a
peer that has stopped reading makes it fail with `TimeoutError` and the connection is aborted, instead
of holding the finalizer — and with it an app's shutdown — open indefinitely.

A send or receive that reaches its own timeout is cancelled the same way, so it aborts the connection too:
after a `TimeoutError` nothing more can be sent or received. Silence is normal for a WebSocket, so the
default configuration has no receive timeout; `withReceiveTimeout` sets one for a peer that must speak
regularly, and it should be longer than that peer's longest silence.

## Shutdown

Interrupting `serve` or `acceptLoop` shuts the server down gracefully, as the section above recommends:

1. Every open connection is sent a going-away close (1001) without interrupting its handler, so a
   handler blocked in a receive sees `ConnectionClosed` and ends normally.
2. Handlers get `ShutdownTimeout` (10 s by default; set it with `WebSocketConfig.withShutdownTimeout`,
   and 0 or less waits indefinitely) to finish. Any still running are then interrupted, and their finalizers
   run.
3. The listener is stopped. `serve` also disposes it.

Requests that arrive during the shutdown are refused with 503. Handlers are the loop's children, forked
while it hands a connection off uninterruptibly, so interrupting the loop leaves them running until the
shutdown is done with them rather than aborting their receives. Interrupting a single `accept` is
different: it stops the listener, which drops every connection accepted earlier.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
