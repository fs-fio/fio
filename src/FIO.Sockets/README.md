# FIO.Sockets

TCP socket library for [FIO](https://github.com/fs-fio/fio), the type-safe functional effect system for F#.

## Installation

```bash
dotnet add package FIO.Sockets
```

Requires the core `FIO` package.

## Features

- **Socket Client** - Connect to TCP servers with configurable options
- **Server Socket** - Accept incoming TCP connections
- **Connection Pooling** - Efficient connection reuse
- **Codec Support** - Encode/decode messages with custom codecs

## Quick Example

```fsharp
open FIO.DSL
open FIO.Sockets

let client = fio {
    use! socket = SocketClient.connect "localhost" 8080
    do! Socket.send socket "Hello, server!"B
    let! response = Socket.receive socket 1024
    return response
}
```

## Documentation

See the [FIO repository](https://github.com/fs-fio/fio) for full documentation.

## License

MIT License - see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
