# FIO.WebSockets

WebSocket library for [FIO](https://github.com/fs-fio/fio), the type-safe functional effect system for F#.

## Installation

```bash
dotnet add package FIO.WebSockets
```

Requires the core `FIO` package.

## Features

- **WebSocket Client** - Connect to WebSocket servers
- **WebSocket Server** - Accept incoming WebSocket connections
- **Connection Pooling** - Efficient connection reuse
- **Codec Support** - Encode/decode messages with custom codecs

## Quick Example

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

## Documentation

See the [FIO repository](https://github.com/fs-fio/fio) for full documentation.

## License

MIT License - see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
