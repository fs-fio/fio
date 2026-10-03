# FIO.Sockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.Sockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.Sockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

TCP sockets for [FIO](https://github.com/fs-fio/fio). Connect, send, and receive as composable
effects — connections are scoped and released for you, and failures surface as a typed
`SocketError` rather than raw exceptions.

- **Client connections** — `SocketClient.withConnectionTo` / `withConnection` scope a socket's lifetime
- **Server** — `ServerSocket.bind` / `accept` / `close` for manual control, or `ServerSocket.serve` /
  `acceptLoop` to run a handler per connection with bounded concurrency, closing the server when interrupted
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
    do! ServerSocket.close serverSocket
}
```

## Errors

Operations fail with a typed `SocketError` — `ConnectionFailed`, `ConnectionClosed`, `SendFailed`,
`ReceiveFailed`, `TimeoutError`, `BufferOverflow`, `InvalidState`, `BindFailed`, `AcceptFailed`,
`CodecError`, `GeneralError` — with `SocketError.fromException` / `SocketError.toException` to
bridge raw exceptions. `InvalidState` also covers argument validation (an empty host, a port out
of range, a non-positive buffer size); `BufferOverflow` is raised when a line (a JSON line included) or
frame exceeds the buffer you passed.

A receive that gives up part-way through a message — a line or frame over its limit, a negative frame
length, or a timeout or interruption mid-message — has consumed bytes the next read would misread, so
the socket's receive side stops there: every later receive fails with `ConnectionClosed`. Sends still
work, so you can answer before closing. A receive that times out before reading anything leaves the
socket usable.

## Closing

`withConnection` owns the socket from before it connects, so interrupting the fiber at any point —
while connecting included — closes it. `serve` and `acceptLoop` hand every accepted connection to a
handler whose finalizer closes it, even when the loop is interrupted mid-accept. Handlers are the
loop's ordinary children: interrupting the loop interrupts them at once, alongside its own cleanup.

Closing is graceful by default: data still queued is sent before the connection ends.
`SocketConfig.withLinger true 0` opts into an immediate reset instead. Interrupting `serve` or
`acceptLoop` is the clean way to stop a server; closing its server socket underneath ends the loop with
`AcceptFailed`, and `accept` on a closed server socket fails the same way. The options of an
`AcceptedSocketConfig` — Nagle, buffer sizes, timeouts — apply to every accepted socket.

## Pooling

Connection pooling is not implemented yet: `SocketPoolConfig` and the `PoolExhausted` / `PoolClosed`
error cases are reserved for it and are not produced by any current operation.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
