[![FIO](https://raw.githubusercontent.com/fs-fio/fio/main/assets/logo-readme.png)](https://github.com/fs-fio/fio)

**🪻 A Type-Safe, Purely Functional Effect System for F#**

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 11](https://img.shields.io/badge/.NET-11-512BD4.svg?logo=dotnet)

FIO is an [IO monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) plus lightweight fibers (green
threads) for concurrent and asynchronous F# applications. Effects are lazy values that a runtime interprets, and the API
takes its cues from [ZIO](https://zio.dev).

- **Typed effects** — `FIO<'A, 'E>` tracks both the success value and the error in the type
- **Fibers & channels** — green threads via `.Fork()` / `.Join()` and typed message passing
- **Structured concurrency** — fail-fast parallel combinators that interrupt losers automatically
- **Finalizer guarantees** — `Ensuring` finalizers run on success, error, *and* interruption
- **Composable** — the `fio { }` computation expression plus a rich set of operators

> **Status: beta.** The API may still change between releases, and FIO has not yet been proven in production.

## Install

```bash
dotnet add package FSharp.FIO --prerelease
```

FSharp.FIO needs FSharp.Core 11.0.101 or newer.

## Quick start

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

`FIOApp` runs `effect`, then the `onOutcome` and `onShutdown` hooks, and exits with `mapExitCode`: 0 on success, 1 on a
failure, 130 when interrupted. Ctrl+C, SIGTERM and SIGQUIT interrupt the effect, and its finalizers run before the hooks.

## Concurrency

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

More in [examples/](https://github.com/fs-fio/fio/tree/main/examples): the DSL, App, HTTP, Sockets, and WebSockets tours.

## Combinators

| To… | Use |
|-----|-----|
| Discard or replace the value | `.Unit()`, `.As value` |
| Make the outcome a value | `.Result()`, `.Option()`, `.Choice()`, `.Fold onError onSuccess` |
| Ignore success and failure alike | `.Ignore()` |
| Recover from failure | `.CatchAll handler`, `.CatchSome handler`, `.OrElse other`, `.OrElseSucceed value`, `.OrElseFail error` |
| Continue on either outcome | `.FoldFIO onError onSuccess` |
| Look without changing | `.Tap`, `.TapError`, `.TapBoth onSuccess onError`, `.Debug()` |
| Sequence | `.FlatMap`, `.Zip`, `.ZipRight`, `.ZipLeft`, or `fio { }` |
| Run concurrently | `.ZipPar` (`<&>`), `FIO.forEachPar`, `FIO.forEachParN n` (at most `n` at a time), `.Race` (first to succeed), `.RaceFirst` (first to settle) |
| Wait or bound time | `FIO.sleep`, `.Delay`, `.Timeout`, `.TimeoutFail`, `.Timed()` |
| Retry or repeat | `.Retry`, `.RetryWhile`, `.Eventually()`, `.RepeatN`, `.RepeatUntil`, `.Forever()` |
| Clean up | `.Ensuring finalizer`, `.OnExit finalizer` (told how the effect ended), `FIO.acquireReleaseWith`, `FIO.acquireReleaseExitWith` |
| Shield from interruption | `.Uninterruptible()`, `FIO.uninterruptibleMask` |

## Resource safety

`Ensuring` and `FIO.acquireReleaseWith` run their cleanup on success, failure and interruption, and acquire and release
run uninterruptibly, so a resource that was acquired is always released. `.OnExit` and `FIO.acquireReleaseExitWith` also
tell the cleanup how the effect ended. `FIO.uninterruptible` and `FIO.uninterruptibleMask` shield regions of your own.

```fsharp
// Keep the file only if writing it succeeded; delete a partial one on a failure or an interruption.
let writeReport (path: string) (lines: string list) =
    FIO.acquireReleaseExitWith
        (FIO.attempt (fun () -> new StreamWriter(path)) id)
        (fun writer outcome ->
            FIO.attempt (fun () ->
                writer.Dispose()
                match outcome with
                | Succeeded _ -> ()
                | Failed _ | Interrupted _ -> File.Delete path) id)
        (fun writer -> FIO.forEachDiscard lines (fun line -> FIO.attempt (fun () -> writer.WriteLine line) id))
```

## Channels, promises and semaphores

| Constructor | A write to a full channel… |
|-------------|----------------------------|
| `Channel<'A>()` | never happens: unbounded |
| `Channel<'A>.Bounded n` | suspends until a reader makes room |
| `Channel<'A>.Dropping n` | drops the new message |
| `Channel<'A>.Sliding n` | drops the oldest message |

`Read` and `Write` suspend when they must; `TryRead` and `TryWrite` never do.

A `Promise<'A, 'E>` is completed once and awaited by any number of fibers. A `Semaphore n` bounds how many effects run
at once (`WithPermit`), and a `Ref<'A>` is an atomic cell shared between fibers.

## Runtimes

| Runtime | Notes |
|---------|-------|
| `DirectRuntime` | One .NET task per fiber, no scheduler of its own. Handy for tests and as a baseline. |
| `WorkStealingRuntime` | Work-stealing scheduler. **The default** (`DefaultRuntime`, used by `FIOApp`). |

Both run on the .NET thread pool, so don't block it (`.Result`, `.Wait()`, `Thread.Sleep`); wait with `FIO.sleep` or
`FIO.awaitTask` instead. Disposing a runtime interrupts the fibers still running and waits up to 10 seconds for their
finalizers.

## Benchmarks

Twelve workloads (Pingpong, Threadring, Chameneos, …) run on both runtimes and report time and allocations. Pingpong is
tracked per commit on the [live dashboard](https://fs-fio.github.io/fio/dev/bench/); the suite is described in
[`benchmarks/FIO.Benchmarks/README.md`](https://github.com/fs-fio/fio/blob/main/benchmarks/FIO.Benchmarks/README.md).

## Packages

| Package | Description |
|---------|-------------|
| [`FSharp.FIO`](https://www.nuget.org/packages/FSharp.FIO) | Core — effects, fibers, channels, runtimes |
| [`FSharp.FIO.Http`](https://www.nuget.org/packages/FSharp.FIO.Http) | HTTP server (Kestrel) |
| [`FSharp.FIO.Sockets`](https://www.nuget.org/packages/FSharp.FIO.Sockets) | TCP sockets |
| [`FSharp.FIO.WebSockets`](https://www.nuget.org/packages/FSharp.FIO.WebSockets) | WebSockets |

Each extension has its own README:
[Http](https://github.com/fs-fio/fio/blob/main/src/FIO.Http/README.md) ·
[Sockets](https://github.com/fs-fio/fio/blob/main/src/FIO.Sockets/README.md) ·
[WebSockets](https://github.com/fs-fio/fio/blob/main/src/FIO.WebSockets/README.md).

## Contributing

[Issues](https://github.com/fs-fio/fio/issues) and pull requests welcome. See
[CONTRIBUTING.md](https://github.com/fs-fio/fio/blob/main/CONTRIBUTING.md), the
[Code of Conduct](https://github.com/fs-fio/fio/blob/main/CODE_OF_CONDUCT.md), and the
[Security Policy](https://github.com/fs-fio/fio/blob/main/SECURITY.md).

## License

[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
