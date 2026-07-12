# FIO.WebSockets

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.WebSockets.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO.WebSockets)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

WebSockets for [FIO](https://github.com/fs-fio/fio). Open connections, send frames, and pattern
match on incoming messages as composable effects — connections are scoped and released for you,
and failures surface as a typed `WsError` rather than raw exceptions.

- **Client** — `WebSocketClient.connectDefault` opens a scoped connection
- **Server** — `WebSocketServer.start` / `acceptDefault` / `close` to accept clients
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

Operations fail with a typed `WsError` — `ConnectionFailed`, `SendFailed`, `ReceiveFailed`,
`MessageTooLarge`, `TimeoutError`, `CodecError`, `Closed`, `GeneralError` — with
`WsError.fromException` / `WsError.toException` to bridge raw exceptions.

## Links

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) ·
[FIO core](https://github.com/fs-fio/fio) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
