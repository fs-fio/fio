# FIO Examples

Runnable example projects for [FIO](https://github.com/fs-fio/fio) — the core DSL, the
`FIOApp` framework, and each extension library. Every project is a self-contained console
app you can run directly with `dotnet run`.

| Example | Shows | Run |
|---------|-------|-----|
| **[FIO.Examples.DSL](FIO.Examples.DSL)** | A tour of the core DSL — `FIO.succeed`/`fail`, running effects on a runtime, fork/join, the `fio { }` CE, interruption, and the operators. | `dotnet run --project examples/FIO.Examples.DSL` |
| **[FIO.Examples.App](FIO.Examples.App)** | The big `FIOApp` showcase — CE control flow, channels, error handling, async/task interop, 100k-fiber concurrency, race/timeout, resources, shutdown hooks, and custom exit codes. | `dotnet run --project examples/FIO.Examples.App` |
| **[FIO.Examples.Http](FIO.Examples.Http)** | An HTTP server with routing, JSON, query params, body parsing, and middleware (`FSharp.FIO.Http`). | `dotnet run --project examples/FIO.Examples.Http` |
| **[FIO.Examples.Sockets](FIO.Examples.Sockets)** | A TCP echo server and interactive client running concurrently in one app (`FSharp.FIO.Sockets`). | `dotnet run --project examples/FIO.Examples.Sockets` |
| **[FIO.Examples.WebSockets](FIO.Examples.WebSockets)** | A WebSocket echo server and interactive client running concurrently in one app (`FSharp.FIO.WebSockets`). | `dotnet run --project examples/FIO.Examples.WebSockets` |

New to FIO? Start with **DSL** for the fundamentals, then **App** for how real programs are
structured. The **Http**, **Sockets**, and **WebSockets** examples each pair with their library's
README.

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Http](https://github.com/fs-fio/fio/blob/main/src/FIO.Http/README.md) ·
[Sockets](https://github.com/fs-fio/fio/blob/main/src/FIO.Sockets/README.md) ·
[WebSockets](https://github.com/fs-fio/fio/blob/main/src/FIO.WebSockets/README.md) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
