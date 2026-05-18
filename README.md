<div align="center">
  <a href="https://github.com/fs-fio/fio/">
    <img src="assets/images/fio_logo_wide.png" width="auto" height="200" alt="FIO">
  </a>

  <p><strong>A type-safe, purely functional effect system for F#</strong></p>
</div>

---

IO monad + fibers for building concurrent F# applications.

## Install

```bash
dotnet add package FIO
```

## Quick Start

```fsharp
open FIO.DSL
open FIO.Console

type App() =
    inherit FIOApp<unit, exn>()

    override _.effect = fio {
        do! Console.printLine "What is your name?"
        let! name = Console.readLine id
        do! Console.printLine $"Hello, {name}!"
    }

[<EntryPoint>]
let main _ = App().Run()
```

More in [examples/](examples/).

## Features

- **Effects** — lazy, composable `FIO<'R, 'E>` with typed errors
- **Fibers** — green threads for scalable concurrency
- **Channels** — typed message passing between fibers
- **Composition** — `fio { }` CE, operators (`>>=`, `<&>`, `<|>`), combinators
- **Primitives** — `Promise`, `Ref`, `Semaphore`
- **Modules** — `Console`, `Clock`, `Environment`, `Random`

## Packages

| Package | Description |
|---------|-------------|
| [`FIO`](https://www.nuget.org/packages/FIO) | Core — effects, fibers, channels, runtimes |
| [`FIO.Http`](https://www.nuget.org/packages/FIO.Http) | HTTP server (Kestrel) |
| [`FIO.Sockets`](https://www.nuget.org/packages/FIO.Sockets) | TCP sockets |
| [`FIO.WebSockets`](https://www.nuget.org/packages/FIO.WebSockets) | WebSockets |

## Contributing

[Issues](https://github.com/fs-fio/fio/issues) and pull requests welcome.

## License

[MIT](LICENSE.md)
