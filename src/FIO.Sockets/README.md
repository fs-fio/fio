# FIO.Sockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.Sockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.Sockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 11](https://img.shields.io/badge/.NET-11-512BD4.svg?logo=dotnet)

TCP sockets for [FIO](https://github.com/fs-fio/fio). Connect, send, and receive as composable effects; connections are
scoped and released for you, and failures surface as a typed `SocketError`.

- **Client connections** — `SocketClient.withConnectionTo` / `withConnection` scope a socket's lifetime
- **Server** — `ServerSocket.bind` / `accept` / `close` for manual control, or `ServerSocket.serve` / `acceptLoop` to
  run a handler per connection with bounded concurrency
- **Custom codecs** — send and receive typed messages, not just strings

> **Status: beta.** The API may still change between releases.

## Install

```bash
dotnet add package FSharp.FIO.Sockets --prerelease
```

## Quick start

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

With `bind` and `accept` the sockets are yours to close. `serve` and `acceptLoop` close every accepted connection, and
`serve` also closes its server socket. Interrupting either is the clean way to stop a server.

## Errors

Operations fail with a `SocketError` (`ConnectionFailed`, `ConnectionClosed`, `TimeoutError`, `BufferOverflow`,
`InvalidState`, `BindFailed`, `AcceptFailed`, `CodecError`, `GeneralError`); `SocketError.fromException` and
`toException` bridge raw exceptions.

## Receiving

A receive takes nothing until its whole item has arrived, so a receive that times out or is interrupted loses no bytes.
To poll, set `SocketConfig.withReceiveTimeout` rather than racing `ReceiveLine(n).Timeout(...)`, which can drop a line
that arrives just as the timer fires. A line or frame over its limit stops the receive side: later receives fail with
`ConnectionClosed`.

## Codecs

`Codec.bytes`, `Codec.string`, `Codec.line`, `Codec.json` and `Codec.jsonLine` are built in; `Codec.map`,
`Codec.compose` and `Codec.lengthPrefixed` build on them, and `Codec.createPure` wraps your own format (Protobuf, say).
`SendFramed`/`ReceiveFramed` prefix each message with a 4-byte big-endian length.

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
