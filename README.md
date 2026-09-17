<div align="center">
  <a href="https://github.com/fs-fio/fio/">
    <img src="https://raw.githubusercontent.com/fs-fio/fio/main/assets/logo.png" width="auto" height="250" alt="FIO">
  </a>

  <p><strong>🪻 A Type-Safe, Purely Functional Effect System for F#</strong></p>

  <p>
    <a href="https://www.nuget.org/packages/FSharp.FIO"><img src="https://img.shields.io/nuget/v/FSharp.FIO.svg?logo=nuget&label=nuget" alt="NuGet"></a>
    <a href="https://github.com/fs-fio/fio/actions/workflows/test.yml"><img src="https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg" alt="Run Tests"></a>
    <a href="https://github.com/fs-fio/fio/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT"></a>
    <img src="https://img.shields.io/badge/.NET-10-512BD4.svg?logo=dotnet" alt=".NET 10">
  </p>
</div>

---

FIO is an [IO monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) plus
lightweight fibers (green threads) for building concurrent and asynchronous F# applications.
Effects are described as pure, lazy values and run by a pluggable runtime — so your program
is a composable description that stays referentially transparent until you hand it to a runtime.
The API takes its cues from [ZIO](https://zio.dev).

- **Typed effects** — `FIO<'A, 'E>` tracks both the success value and the error in the type
- **Fibers & channels** — green threads via `.Fork()` / `.Join()` and typed message passing
- **Structured concurrency** — fail-fast parallel combinators that interrupt losers automatically
- **Finalizer guarantees** — `Ensuring` finalizers run on success, error, *and* interruption
- **Composable** — the `fio { }` computation expression plus a rich set of operators

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

`FIOApp` runs `effect`, then `onOutcome` with the settled result, then `onShutdown`, and exits with
`mapExitCode`. Ctrl+C and SIGTERM interrupt the effect; its finalizers run before the hooks, so
cleanup that needs effect state belongs in `Ensuring`/`acquireReleaseWith`, and `onShutdown` is for
process-level goodbyes. A finalizer that never completes holds shutdown; a second Ctrl+C or SIGTERM
terminates the process. A defect or an invalid argument in the effect is a fatal error (exit code 2),
not an interruption (130).

## Concurrency

Fork effects onto fibers, run them in parallel, and compose the results — losers are
interrupted automatically on the first failure.

```fsharp
open FIO.DSL

// Run two effects in parallel with <&> and collect both results as a tuple.
let taskA: FIO<string, exn> = FIO.succeed "Task A completed! ✅"
let taskB: FIO<int * string, exn> = FIO.succeed (200, "Task B OK ✅")
let both = taskA <&> taskB

// Or fork/join explicitly.
let forked: FIO<string, exn> =
    FIO.succeed("Hello, concurrency! 🚀").Fork() >>= fun fiber -> fiber.Join()
```

More in [examples/](https://github.com/fs-fio/fio/tree/main/examples) — the DSL, App, HTTP, Sockets, and WebSockets tours.

## Features

- **Effects** — lazy, composable `FIO<'A, 'E>` with typed errors
- **Fibers** — green threads for scalable concurrency
- **Channels** — typed message passing between fibers
- **Structured concurrency** — fail-fast `ZipPar`, `Race`, and `forEachPar` that interrupt losers automatically
- **Composition** — `fio { }` CE, operators (`>>=`, `<&>`, `<|>`), combinators
- **Refs** — `Ref<'A>`, an atomic reference cell shared between fibers (with `FIO.DSL` open it shadows FSharp.Core's `Ref<'T>` annotation; `'T ref` is unaffected)
- **Modules** — `Console`

## Runtimes

Effects are interpreted by a runtime. Pick one explicitly, or use `DefaultRuntime`.

| Runtime | Notes |
|---------|-------|
| `DirectRuntime` | Multi-threaded via the .NET thread pool — one task per fiber, no scheduler of its own. Handy for tests and as a baseline. |
| `PollingRuntime` | Multi-threaded, linear-time handling of blocked fibers (polling). |
| `SignalingRuntime` | Multi-threaded, event-driven handling of blocked fibers. |
| `WorkStealingRuntime` | Multi-threaded, work-stealing scheduler. **The default.** |

`DefaultRuntime = WorkStealingRuntime` — `FIOApp` uses it unless you `override _.runtime`.

## Benchmarks

Twelve concurrency workloads (Pingpong, Threadring, Chameneos, Philosophers, …) run against every
runtime, reporting execution time and allocations. Pingpong is tracked per commit on the
[**live benchmark dashboard**](https://fs-fio.github.io/fio/dev/bench/); the full suite,
its parameters, and the A/B comparison protocol are documented in
[`benchmarks/FIO.Benchmarks/README.md`](https://github.com/fs-fio/fio/blob/main/benchmarks/FIO.Benchmarks/README.md).

## Packages

| Package | Description |
|---------|-------------|
| [`FSharp.FIO`](https://www.nuget.org/packages/FSharp.FIO) | Core — effects, fibers, channels, runtimes |
| [`FSharp.FIO.Http`](https://www.nuget.org/packages/FSharp.FIO.Http) | HTTP server (Kestrel) |
| [`FSharp.FIO.Sockets`](https://www.nuget.org/packages/FSharp.FIO.Sockets) | TCP sockets |
| [`FSharp.FIO.WebSockets`](https://www.nuget.org/packages/FSharp.FIO.WebSockets) | WebSockets |

Each extension library has its own README with API details:
[Http](https://github.com/fs-fio/fio/blob/main/src/FIO.Http/README.md) · [Sockets](https://github.com/fs-fio/fio/blob/main/src/FIO.Sockets/README.md) · [WebSockets](https://github.com/fs-fio/fio/blob/main/src/FIO.WebSockets/README.md).

## Contributing

[Issues](https://github.com/fs-fio/fio/issues) and pull requests welcome. See
[CONTRIBUTING.md](https://github.com/fs-fio/fio/blob/main/CONTRIBUTING.md), the
[Code of Conduct](https://github.com/fs-fio/fio/blob/main/CODE_OF_CONDUCT.md), and the
[Security Policy](https://github.com/fs-fio/fio/blob/main/SECURITY.md).

## License

[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
