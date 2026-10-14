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

`SendString`/`ReceiveString` (and the other string, bytes and JSON helpers) are extension members from
`FIO.WebSockets.WebSocketExtensions`, which is not opened automatically; the type's own members are
`SendText`/`SendBinary`/`SendFrame`/`Send` and `ReceiveMessage`/`Receive`/`TryReceive`.

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

With `start` and `accept`/`acceptDefault` the listener and the connection are yours to close, as above;
`close` stops the listener and releases it, and closing it again does nothing. `serve` does it for you;
`acceptLoop` closes the connections and only stops the listener it was given.

`start` and `serve` take an `HttpListener` URL prefix, and its host does two jobs: it is the address
bound — only the first one a name resolves to — and a filter on each request's `Host` header. So
`http://localhost:8080/` may bind only `::1`, and `http://127.0.0.1:8080/` answers a client that dials
`localhost` with 404. To listen on every interface, use `+`; `0.0.0.0` and `[::]` mean the same. On
Linux and macOS `+` binds IPv4 only; on Windows it needs a URL reservation (`netsh http add urlacl`) or
an elevated process.

Known issue on Linux and macOS: .NET's `HttpListener` accepts before it has finished starting
([dotnet/runtime#28658](https://github.com/dotnet/runtime/issues/28658)), so a client that connects at
that moment makes `start` (and `serve`) fail with `ConnectionFailed "Value cannot be null."`, and the
port stays bound until the next garbage collection. A process restarted by its supervisor recovers; a
server restarted in-process while clients keep reconnecting should retry, on another port or after
`GC.Collect()` followed by `GC.WaitForPendingFinalizers()`, which releases the port.

A request that is not a WebSocket upgrade, or whose handshake is invalid (a missing key or version, or
a subprotocol the client did not offer), is answered `400` and closed, and `accept` fails with
`ConnectionFailed`. `acceptLoop` and `serve` keep serving without a pause; they log an invalid
handshake, not a plain request. `WebSocketConfig.withMaxConnections` caps the connections `serve` and `acceptLoop` keep open;
requests beyond the cap are answered `503`. A message may arrive in any number of fragments: what
bounds it is `MaxMessageSize` and, if you set one, `ReceiveTimeout`.

## Errors

Operations fail with a typed `WsError`; each case says what went wrong, so a receive loop can decide
without inspecting messages:

| Case | Raised by |
|------|-----------|
| `Closed` | the peer closed (`Receive` with a codec, `ReceiveString`/`ReceiveBytes`/`ReceiveJson`), a send after the peer's close frame, or any send/receive/close on a socket that is already closed or aborted |
| `CodecError` | encoding or decoding a payload, including a frame of the wrong kind |
| `ReceiveFailed` / `SendFailed` | a transport fault while receiving or sending, or a receive or send cancelled through the token you passed |
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
    | Disconnected _ -> return ()
}
```

`Disconnected` covers every way the connection ended: the peer's close frame, a dropped connection, or
a close or abort on this side. A receive that times out or is cancelled through its token fails with
`TimeoutError` or `ReceiveFailed`; if it was already reading, later receives on that connection yield `Disconnected`.

## Codecs

A codec turns a value into a frame and back. `Codec.frame`, `Codec.binary`, `Codec.text`, `Codec.json`
and `Codec.jsonLine` come built in, and `Codec.map` and `Codec.compose` build on them. For any other
format, `Codec.createPure` wraps your own encode and decode functions and reports a throw as
`CodecError`, which `TryReceive` returns as `Undecodable`. Protocol buffers, for example, with
Google.Protobuf and a message type `Order` generated by protoc:

```fsharp
open Google.Protobuf

let protobuf (parser: MessageParser<'T>) : WebSocketCodec<'T> =
    Codec.createPure
        (fun message -> Binary((message :> IMessage).ToByteArray()))
        (function
         | Binary bytes -> parser.ParseFrom(bytes: byte[])
         | _ -> failwith "Expected a binary frame")

let codec = protobuf Order.Parser

let exchange (socket: WebSocket) (order: Order) =
    fio {
        do! socket.Send(codec, order)
        return! socket.Receive codec
    }
```

The WebSocket protocol already delimits each message, so no extra framing is needed.

## Closing

`withConnection` and `acceptLoop` close the socket for you with `CloseIfOpen`, which closes a connection
that is still open — answering a close the peer began — and logs rather than fails when that goes
wrong; call it yourself for the same best-effort close. `withConnection` owns the socket from before it
connects, so an interrupt while connecting disposes it, and `serve` and `acceptLoop` hand every
accepted connection to a handler that closes it, even when interrupted mid-accept.

A close needs a socket that can still complete the closing handshake: interrupting a fiber while it
reads — losing a race, `Stop()` — aborts the connection instead, whichever overload it used, because
the read it left behind would otherwise take the peer's next message (a receive still waiting for its
turn is not reading, and does not abort). For a graceful
close while a reader is pending, call `Close` from another fiber first: it closes the outgoing side, and
the pending receive then observes the peer's close frame as `ConnectionClosed`. `Abort` on a connection
a server accepted marks it closed on this side; its peer is told when the connection is disposed, which
`serve` and `acceptLoop` do when the handler ends.

The handshake waits for the peer's close frame, so `Close` is bounded by `SendTimeout` like a send: a
peer that has stopped reading makes it fail with `TimeoutError` and the connection is aborted, instead
of holding the finalizer — and with it an app's shutdown — open indefinitely.

A send or receive that reaches its own timeout while in flight is cancelled the same way, so it aborts
the connection too: after such a `TimeoutError` nothing more can be sent or received. The time counts
from the call, so it includes waiting for another send or receive on the same connection; a call that
times out while still waiting leaves the connection as it was. A receive's timeout covers one whole
message. Silence is normal for a WebSocket, so the default configuration has no receive timeout;
`withReceiveTimeout` sets one for a peer that must speak regularly, and it should be longer than that
peer's longest silence.

## Shutdown

Interrupting `serve` or `acceptLoop` shuts the server down gracefully, as the section above recommends:

1. Every open connection is sent a going-away close (1001) without interrupting its handler, so a
   handler blocked in a receive sees `ConnectionClosed` and ends normally.
2. Handlers get `ShutdownTimeout` (10 s by default; set it with `WebSocketConfig.withShutdownTimeout`,
   and 0 or less waits indefinitely) to finish, counted from the start of the shutdown. Any handlers
   still running are then interrupted and their connections ended, so a peer that never reads or
   never answers the close cannot hold the shutdown open, even with a `SendTimeout` of 0. A handler's
   own finalizers and uninterruptible regions still run to their end, and the shutdown waits for them.
3. The listener is stopped, and with it the refusing of new requests. If .NET's `Stop()` fails midway
   and leaves the refusal waiting for a request that cannot come, the refusal is cut after a second
   rather than holding the shutdown. `serve` also closes the listener.

Requests that arrive during the shutdown are refused with 503. Handlers are the loop's children, forked
while it hands a connection off uninterruptibly, so interrupting the loop leaves them running until the
shutdown is done with them rather than aborting their receives. Interrupting a single `accept` is
different: it stops the listener, which drops every connection accepted earlier. An `acceptLoop`
whose listener is stopped by someone else ends with `ConnectionFailed`. A handler that fails or
throws is reported on standard error.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
