# FIO.WebSockets

WebSockets for [FIO](https://github.com/fs-fio/fio).

```bash
dotnet add package FIO.WebSockets
```

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

Client and server connections, custom codecs.

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) · [MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
