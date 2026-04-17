# FIO.WebSockets

WebSocket library for [FIO](https://github.com/fs-fio/fio), the type-safe functional effect system for F#.

## Install

```bash
dotnet add package FIO.WebSockets
```

## Features

- **WebSocket client** — connect to WebSocket servers
- **WebSocket server** — accept incoming WebSocket connections
- **Codec support** — encode/decode messages with custom codecs

## Quick Start

```fsharp
open FIO.DSL
open FIO.WebSockets

let client = fio {
    use! ws = WebSocketClient.connect "ws://localhost:8080/ws"
    do! WebSocket.sendText ws "Hello, server!"
    let! response = WebSocket.receiveText ws
    return response
}
```

See the [examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) for more.

## License

MIT — see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md).
