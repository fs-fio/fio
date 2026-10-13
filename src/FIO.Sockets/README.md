# FIO.Sockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.Sockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.Sockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

TCP sockets for [FIO](https://github.com/fs-fio/fio). Connect, send, and receive as composable
effects — connections are scoped and released for you, and failures surface as a typed
`SocketError` rather than raw exceptions.

- **Client connections** — `SocketClient.withConnectionTo` / `withConnection` scope a socket's lifetime
- **Server** — `ServerSocket.bind` / `accept` / `close` for manual control, or `ServerSocket.serve` /
  `acceptLoop` to run a handler per connection with bounded concurrency; `serve` also binds the server
  socket and closes it when interrupted
- **Custom codecs** — send and receive typed messages, not just strings

## Install

```bash
dotnet add package FSharp.FIO.Sockets --prerelease
```

## Quick Start

```fsharp
open FIO.DSL
open FIO.Sockets

let program =
    SocketClient.withConnectionTo "localhost" 8080 (fun socket ->
        fio {
            do! socket.SendString "Hello, server!"
            return! socket.ReceiveString 1024
        })
```

## Server

```fsharp
open FIO.DSL
open FIO.Sockets

let server = fio {
    let! config = ServerSocketConfig.create "localhost" 8080
    let! serverSocket = ServerSocket.bind config
    let! clientSocket = ServerSocket.accept serverSocket
    let! message = clientSocket.ReceiveString 1024
    do! clientSocket.SendString $"echo: {message}"
    do! clientSocket.Close()
    do! ServerSocket.close serverSocket
}
```

With `bind` and `accept` the sockets are yours to close, as above. `serve` and `acceptLoop` close every
accepted connection for you; `acceptLoop` leaves the server socket to its caller.

## Errors

Operations fail with a typed `SocketError` — `ConnectionFailed`, `ConnectionClosed`, `TimeoutError`,
`BufferOverflow`, `InvalidState`, `BindFailed`, `AcceptFailed`, `CodecError`, `GeneralError` — with
`SocketError.fromException` / `SocketError.toException` to bridge raw exceptions. A failed read or
write surfaces as `GeneralError` with the underlying exception. `InvalidState` also covers argument
validation (an empty host, a port out of range, a non-positive buffer size); `BufferOverflow` is
raised when a line (a JSON line included) or frame exceeds the limit you passed (16 MB for a frame
when `ReceiveFramed` is given none).

## Receiving

A socket buffers what it reads, and a receive takes nothing from that buffer until its whole item
is there. So a receive that times out or is interrupted before its item is complete loses nothing:
the bytes that had arrived stay for the next receive.

To poll, give the socket a receive timeout (`SocketConfig.withReceiveTimeout`): the receive itself
decides whether its item arrived in time, so a poll that gives up takes nothing. Polling with
`socket.ReceiveLine(n).Timeout(...)` races the receive against a timer instead, and a line that
arrives as the timer fires can be taken by the losing receive and lost, though never split or
repeated.

A receive that rejects what the peer sent — a line or frame over its limit, a negative frame length
— cannot read past it, so the socket's receive side stops there: every later receive fails with
`ConnectionClosed`. Sends still work, so you can answer before closing.

`ReceiveTimeout` bounds a whole receive call, however many reads it takes, counted from when the call
has its turn: receives on one socket take turns, so fibers receiving at once each get whole items, in
the order they asked. A receive interrupted at the very moment its item completes may lose that item,
but never part of one.

## Codecs

A codec turns a value into bytes and back. `Codec.bytes`, `Codec.string`, `Codec.line`, `Codec.json` and
`Codec.jsonLine` come built in, and `Codec.map`, `Codec.compose` and `Codec.lengthPrefixed` build on them.
For any other format, `Codec.createPure` wraps your own encode and decode functions and reports a throw
as `CodecError`. Protocol buffers, for example, with Google.Protobuf and a message type `Order` generated
by protoc:

```fsharp
open Google.Protobuf

let protobuf (parser: MessageParser<'T>) : SocketCodec<'T> =
    Codec.createPure (fun message -> (message :> IMessage).ToByteArray()) (fun bytes -> parser.ParseFrom(bytes: byte[]))

let codec = protobuf Order.Parser

let exchange (socket: Socket) (order: Order) =
    fio {
        do! socket.SendFramed(codec, order)
        return! socket.ReceiveFramed codec
    }
```

`SendFramed` and `ReceiveFramed` send each message as one frame behind a 4-byte big-endian length, so a
peer written in another language has to frame its messages the same way. Protobuf's own
`writeDelimitedTo` uses a varint length instead and does not match.

The socket's own `SendJson`/`ReceiveJson` and `SendJsonLine`/`ReceiveJsonLine` use `Codec.json` and
`Codec.jsonLine` with the serializer's default options (property names as declared, matched
case-sensitively); `open FIO.Sockets.SocketExtensions` adds overloads that take `JsonSerializerOptions`.
`ReceiveJson`, like `ReceiveString`, decodes whatever has arrived, up to its limit, so a message that may
span several reads needs the line or framed forms.

## Closing

`withConnection` owns the socket from before it connects, so interrupting the fiber at any point —
while connecting included — closes it. `serve` and `acceptLoop` hand every accepted connection to a
handler whose finalizer closes it; a connection the loop accepted just as it was interrupted, before
any handler had it, is closed by the loop. Handlers are the
loop's ordinary children: interrupting the loop interrupts them at once, alongside its own cleanup.

Closing is graceful by default: data still queued is sent before the connection ends.
`SocketConfig.withLinger true 0` opts into an immediate reset instead. Interrupting `serve` or
`acceptLoop` is the clean way to stop a server; closing its server socket underneath ends the loop with
`AcceptFailed`, and `accept` on a closed server socket fails the same way. The socket options set with
`ServerSocketConfig.withAcceptedConfig` — Nagle, buffer sizes, timeouts — apply to every accepted
socket; a connection they cannot be applied to is closed and the loop goes on. A handler that fails or throws is reported
on standard error, and so is a failing listener, which the loop retries with a growing pause.

`accept` closes a connection it accepted just as its fiber was interrupted, since nothing could take
the socket over, except in a narrow race; `serve` and `acceptLoop` close every connection they accept.

## Pooling

Connection pooling is not implemented yet; it may come in a later release.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
