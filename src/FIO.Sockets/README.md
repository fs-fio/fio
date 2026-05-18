# FIO.Sockets

TCP sockets for [FIO](https://github.com/fs-fio/fio).

```bash
dotnet add package FIO.Sockets
```

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

Client connections, server accept, custom codecs.

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) · [MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
