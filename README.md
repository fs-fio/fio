<div align="center">
  <a href="https://github.com/fs-fio/fio/">
    <img src="https://raw.githubusercontent.com/fs-fio/fio/main/assets/fio_logo_wide.png" width="auto" height="200" alt="FIO">
  </a>

  <p><strong>🪻 A Type-Safe, Purely Functional Effect System for F#</strong></p>
</div>

---

IO monad + fibers for building concurrent F# applications.

## Install

```bash
dotnet add package FSharp.FIO
```

## Quick Start

```fsharp
open FIO.DSL
open FIO.App
open FIO.Console

type App() =
    inherit FIOApp<unit, exn>()

    override _.effect = fio {
        do! Console.printLine "What is your name?" id
        let! name = Console.readLine id
        do! Console.printLine $"Hello, {name}!" id
    }

[<EntryPoint>]
let main _ = App().Run()
```

More in [examples/](examples/).

## Features

- **Effects** — lazy, composable `FIO<'A, 'E>` with typed errors
- **Fibers** — green threads for scalable concurrency
- **Channels** — typed message passing between fibers
- **Structured concurrency** — fail-fast `ZipPar`, `Race`, and `forEachPar` that interrupt losers automatically
- **Composition** — `fio { }` CE, operators (`>>=`, `<&>`, `<|>`), combinators
- **Modules** — `Console`

## Packages

| Package | Description |
|---------|-------------|
| [`FSharp.FIO`](https://www.nuget.org/packages/FSharp.FIO) | Core — effects, fibers, channels, runtimes |
| [`FSharp.FIO.Http`](https://www.nuget.org/packages/FSharp.FIO.Http) | HTTP server (Kestrel) |
| [`FSharp.FIO.Sockets`](https://www.nuget.org/packages/FSharp.FIO.Sockets) | TCP sockets |
| [`FSharp.FIO.WebSockets`](https://www.nuget.org/packages/FSharp.FIO.WebSockets) | WebSockets |

## Contributing

[Issues](https://github.com/fs-fio/fio/issues) and pull requests welcome.

## License

[MIT](LICENSE.md)
