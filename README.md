<div align="center">
  <a href="https://github.com/fs-fio/fio/">
    <img src="assets/images/fio_logo_wide.png" width="auto" height="250" alt="FIO Logo">
  </a>

  <p><strong>A type-safe, purely functional effect system for F#</strong></p>

  [![NuGet](https://img.shields.io/nuget/v/FIO.svg?style=flat-square)](https://www.nuget.org/packages/FIO)
  [![License](https://img.shields.io/github/license/fs-fio/fio.svg?style=flat-square)](LICENSE.md)
  [![CI](https://img.shields.io/github/actions/workflow/status/fs-fio/fio/ci.yml?style=flat-square&label=CI)](https://github.com/fs-fio/fio/actions)
</div>

---

FIO gives you an **IO monad** and **fibers** (green threads) for building concurrent, asynchronous F# applications with composable, type-safe effects.

## Install

```bash
dotnet add package FIO
```

## Quick Start

```fsharp
open FIO.DSL
open FIO.Console

type HelloApp() =
    inherit FIOApp<unit, exn>()

    override _.effect = fio {
        do! Console.printLine "What is your name?"
        let! name = Console.ReadLine
        do! Console.printLine $"Hello, {name}! 🪻"
    }

[<EntryPoint>]
let main _ = HelloApp().Run()
```

See the [examples](examples/) for more — including HTTP servers, sockets, and WebSockets.

## Features

- **Effect system** — lazy, composable `FIO<'R, 'E>` values with full type safety
- **Fibers** — lightweight green threads for scalable concurrency
- **Channels** — typed, unbounded message passing between fibers
- **Operators** — expressive DSL (`>>=`, `<&>`, `<!>`, `<|>`, and more) plus `fio { }` computation expressions
- **Concurrency primitives** — `Promise`, `Ref`, `Semaphore`
- **Library modules** — `Console`, `Clock`, `Environment`, `Random`
- **App framework** — `FIOApp` with lifecycle management, shutdown hooks, and configurable runtimes

## Packages

| Package | Description |
|---------|-------------|
| [`FIO`](https://www.nuget.org/packages/FIO) | Core — effect system, fibers, channels, runtimes, library modules |
| [`FIO.Http`](https://www.nuget.org/packages/FIO.Http) | HTTP server (Kestrel) |
| [`FIO.Sockets`](https://www.nuget.org/packages/FIO.Sockets) | TCP sockets |
| [`FIO.WebSockets`](https://www.nuget.org/packages/FIO.WebSockets) | WebSockets |

## Contributing

Contributions are welcome — open an [issue](https://github.com/fs-fio/fio/issues) or submit a pull request.

## License

MIT — see [LICENSE.md](LICENSE.md).

## Contact

Daniel Larsen — [itsdaniel.dk](https://itsdaniel.dk)

## Acknowledgments

Alceste Scalas — [people.compute.dtu.dk](https://people.compute.dtu.dk/alcsc/)
