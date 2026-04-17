# FIO.Sockets

TCP socket library for [FIO](https://github.com/fs-fio/fio), the type-safe functional effect system for F#.

## Install

```bash
dotnet add package FIO.Sockets
```

## Features

- **Socket client** — connect to TCP servers with configurable options
- **Server socket** — accept incoming TCP connections
- **Codec support** — encode/decode messages with custom codecs

## Quick Start

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

See the [examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) for more.

## License

MIT — see [LICENSE.md](https://github.com/fs-fio/fio/blob/main/LICENSE.md).
