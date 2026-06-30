# FIO.Sockets

TCP sockets for [FIO](https://github.com/fs-fio/fio).

```bash
dotnet add package FSharp.FIO.Sockets
```

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

Client connections, server accept, custom codecs.

[Examples](https://github.com/fs-fio/fio/tree/main/examples/FIO.Examples.Sockets) · [MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
