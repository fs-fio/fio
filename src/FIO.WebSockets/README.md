# FIO.WebSockets

WebSockets for [FIO](https://github.com/fs-fio/fio).

```bash
dotnet add package FIO.WebSockets
```

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

Client and server connections, custom codecs.

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.WebSockets) · [MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
