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
terminates the process. A defect or an invalid argument in the effect is a fatal error, not an
interruption: like a typed failure it exits with 1, as every non-success exit does in ZIO; an
interruption exits with 130.

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

## Combinators

The methods on `FIO<'A, 'E>` cover most control flow; reach for one before writing it by hand.

| To… | Use |
|-----|-----|
| Discard or replace the value | `.Unit()`, `.As value` |
| Make the outcome a value | `.Result()`, `.Option()`, `.Choice()`, `.Fold onError onSuccess` |
| Ignore success and failure alike | `.Ignore()` |
| Recover from failure | `.CatchAll handler`, `.CatchSome handler`, `.OrElse other`, `.OrElseSucceed value`, `.OrElseFail error` |
| Continue on either outcome | `.FoldFIO onError onSuccess` |
| Look without changing | `.Tap`, `.TapError`, `.TapBoth`, `.Debug()` |
| Sequence | `.FlatMap`, `.Zip`, `.ZipRight`, `.ZipLeft`, or `fio { }` |
| Run concurrently | `.ZipPar` (`<&>`), `FIO.forEachPar`, `.Race` (first to succeed), `.RaceFirst` (first to settle) |
| Wait or bound time | `FIO.sleep`, `.Delay`, `.Timeout`, `.TimeoutFail`, `.Timed()` |
| Retry or repeat | `.Retry`, `.RetryWhile`, `.Eventually()`, `.RepeatN`, `.RepeatUntil`, `.Forever()` |
| Clean up | `.Ensuring finalizer`, `FIO.acquireReleaseWith` |
| Shield from interruption | `.Uninterruptible()`, `FIO.uninterruptibleMask` |

As in ZIO, `FoldFIO` and `TapBoth` hand `onError` only this effect's failure; a failure raised by
`onSuccess` propagates. `Forever()` never succeeds, so it takes whatever result type its context needs.

## Resource safety

`Ensuring` runs its finalizer on success, failure and interruption, and so does
`FIO.acquireReleaseWith acquire release use`, which also runs `acquire` and `release`
uninterruptibly: a resource that was acquired is always released. For regions of your own, as in
ZIO, `FIO.uninterruptible` (or `.Uninterruptible()`) defers an interruption until the region ends,
where it takes effect at once (nothing after the region runs), and `FIO.uninterruptibleMask` gives
the body a restorer that makes chosen parts interruptible again:

```fsharp
// Waiting for a job can be interrupted; once taken, a job always runs to the end.
let worker (jobs: Channel<Job>) (run: Job -> FIO<unit, exn>) =
    FIO.uninterruptibleMask <| fun restore ->
        fio {
            let! job = restore.Restore(jobs.Read())
            do! run job
        }
```

Inside an uninterruptible region, `FIO.cancellationToken()` yields a token that never cancels, and
work the region forks, or that a finalizer forks (a `Timeout`, a `ZipPar`), keeps running until the
fiber exits instead of being interrupted with it.

## Channels

`Channel<'A>()` is unbounded. For back-pressure or a drop policy, use ZIO's queue family:

| Constructor | A write to a full channel… |
|-------------|----------------------------|
| `Channel<'A>.Bounded n` | suspends until a reader makes room |
| `Channel<'A>.Dropping n` | drops the new message |
| `Channel<'A>.Sliding n` | drops the oldest message |

`Write` yields the message it wrote (use `.Write(message).Unit()` to discard it); on a full bounded
channel it suspends. `TryWrite` never suspends and yields whether the message was written — `false`
from a full bounded or dropping channel. `Read` suspends until a message arrives.

## Features

- **Effects** — lazy, composable `FIO<'A, 'E>` with typed errors
- **Fibers** — green threads for scalable concurrency
- **Channels** — typed message passing between fibers: unbounded, bounded, dropping or sliding
- **Resource safety** — `acquireReleaseWith` and uninterruptible regions, as in ZIO
- **Structured concurrency** — fail-fast `ZipPar`, `Race`, and `forEachPar` that interrupt losers automatically
- **Composition** — `fio { }` CE, operators (`>>=`, `<&>`, `<|>`), combinators
- **Refs** — `Ref<'A>`, an atomic reference cell shared between fibers (with `FIO.DSL` open it shadows FSharp.Core's `Ref<'T>` annotation; `'T ref` is unaffected)
- **Modules** — `Console` (lines and keys; `tryReadLine` yields `None` at end of input) and `Signal`
  (`Signal.subscribe` awaits POSIX signals such as `SIGWINCH`; on Windows only `SIGINT`, `SIGQUIT`,
  `SIGTERM` and `SIGHUP`)

## Runtimes

Effects are interpreted by a runtime. Pick one explicitly, or use `DefaultRuntime`.

| Runtime | Notes |
|---------|-------|
| `DirectRuntime` | Multi-threaded via the .NET thread pool — one task per fiber, no scheduler of its own. Handy for tests and as a baseline. |
| `PollingRuntime` | Multi-threaded, linear-time handling of blocked fibers (polling). |
| `SignalingRuntime` | Multi-threaded, event-driven handling of blocked fibers. |
| `WorkStealingRuntime` | Multi-threaded, work-stealing scheduler. **The default.** |

`DefaultRuntime = WorkStealingRuntime` — `FIOApp` uses it unless you `override _.runtime`.

A runtime is `IDisposable`. `Dispose()` interrupts every fiber still running on it — roots started
with `Run`, and daemons — waits up to 10 seconds for their finalizers, then stops its workers;
`Shutdown timeout` does the same with your own bound. `Run` afterwards throws
`ObjectDisposedException`. `FIOApp` disposes its runtime when the app ends.

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
