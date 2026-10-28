[![FIO](https://raw.githubusercontent.com/fs-fio/fio/main/assets/logo-readme.png)](https://github.com/fs-fio/fio)

**🪻 A Type-Safe, Purely Functional Effect System for F#**

[![NuGet](https://img.shields.io/nuget/v/FSharp.FIO.svg?logo=nuget&label=nuget)](https://www.nuget.org/packages/FSharp.FIO)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 10](https://img.shields.io/badge/.NET-10-512BD4.svg?logo=dotnet)

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

> **Status: beta.** The API may still change between releases, and FIO has not yet been proven in production.

## Install

```bash
dotnet add package FSharp.FIO --prerelease
```

FSharp.FIO needs FSharp.Core 10.1.402 or newer: an F# project on an older FSharp.Core (one it pins, or the
implicit one of an older SDK) builds with only a downgrade warning (NU1605) and then fails at run time.

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
`mapExitCode`. Ctrl+C, SIGTERM and SIGQUIT (`Ctrl+\`, or Ctrl+Break on Windows) interrupt the
effect; its finalizers run before the hooks, so cleanup that needs effect state belongs in
`Ensuring`/`acquireReleaseWith`, and `onShutdown` is for process-level goodbyes. A finalizer that
never completes holds shutdown; a second such signal terminates the process, and so does the first
one after `Stop()`. Once the effect has ended, the first signal during `onOutcome` or `onShutdown`
does not interrupt the hook and the next one ends the process; a signal after the hooks, while the
runtime is disposed, ends it at once. SIGHUP is not handled: it ends the process without running
finalizers or hooks. A defect, an invalid argument or resource exhaustion in
the effect is a fatal error, not an interruption: like a typed failure it exits with 1, as every
non-success exit does in ZIO; an interruption exits with 130. The default hooks print nothing, so
report a failure from `onOutcome`.

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
| Look without changing | `.Tap`, `.TapError`, `.TapBoth onSuccess onError`, `.Debug()` |
| Sequence | `.FlatMap`, `.Zip`, `.ZipRight`, `.ZipLeft`, or `fio { }` |
| Run concurrently | `.ZipPar` (`<&>`), `FIO.forEachPar`, `FIO.forEachParN n` (at most `n` at a time), `.Race` (first to succeed), `.RaceFirst` (first to settle) |
| Wait or bound time | `FIO.sleep`, `.Delay`, `.Timeout`, `.TimeoutFail`, `.Timed()` |
| Retry or repeat | `.Retry`, `.RetryWhile`, `.Eventually()`, `.RepeatN`, `.RepeatUntil`, `.Forever()` |
| Clean up | `.Ensuring finalizer`, `.OnExit finalizer` (told how the effect ended), `FIO.acquireReleaseWith`, `FIO.acquireReleaseExitWith` |
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
fiber exits instead of being interrupted with it. Use a restorer only in the body of its own mask:
in a finalizer it would make the cleanup interruptible again. A finalizer that fails replaces a
success with its failure, but never an earlier failure or interruption.

When the cleanup depends on how the effect ended, `.OnExit` and `FIO.acquireReleaseExitWith` hand the
finalizer the outcome as a `FiberResult`: `Succeeded`, `Failed`, or `Interrupted` with its cause.

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

## Channels

`Channel<'A>()` is unbounded. For back-pressure or a drop policy, use ZIO's queue family:

| Constructor | A write to a full channel… |
|-------------|----------------------------|
| `Channel<'A>.Bounded n` | suspends until a reader makes room |
| `Channel<'A>.Dropping n` | drops the new message |
| `Channel<'A>.Sliding n` | drops the oldest message |

`Write` yields its message (use `.Write(message).Unit()` to discard it), also when a full dropping
channel dropped it; on a full bounded channel it suspends. `TryWrite` never suspends and yields
whether the message was written — `false` from a full bounded or dropping channel, `true` from a
full sliding one. `Read` suspends until a message arrives; `TryRead` never suspends, yielding the next
message as `Some`, or `None` from an empty channel. A capacity below 1 throws.

## Promises and semaphores

`Promise<'A, 'E>()` is completed once — `Succeed` or `Fail`, each yielding whether this call completed
it — and `Await` suspends any number of fibers until then: a readiness signal, or a reply slot.

```fsharp
type Command = GetBalance of reply: Promise<int, exn>

let balance (inbox: Channel<Command>) =
    fio {
        let reply = Promise<int, exn>()
        do! inbox.Write(GetBalance reply).Unit()
        return! reply.Await()
    }
```

`Semaphore n` bounds how many effects run at once: `WithPermit effect` waits for a permit (interruptibly,
unless the caller is not), runs the effect, and gives the permit back however it ends. To run many
items at most `n` at a time, `FIO.forEachParN n items f` does the same on just `n` fibers.
`System.Threading` has a `Semaphore` too: when it is opened after `FIO.DSL`, write `FIO.DSL.Semaphore`.

## Features

- **Effects** — lazy, composable `FIO<'A, 'E>` with typed errors
- **Fibers** — green threads for scalable concurrency
- **Channels** — typed message passing between fibers: unbounded, bounded, dropping or sliding
- **Resource safety** — `acquireReleaseWith`, outcome-aware `OnExit`/`acquireReleaseExitWith`, and uninterruptible regions, as in ZIO
- **Structured concurrency** — fail-fast `ZipPar`, `forEachPar` and `forEachParN`, plus `Race` (first to succeed) and `RaceFirst` (first to settle), all of which interrupt losers automatically
- **Composition** — `fio { }` CE, operators (`>>=`, `<&>`, `<|>`), combinators
- **Refs** — `Ref<'A>`, an atomic reference cell shared between fibers (with `FIO.DSL` open it shadows FSharp.Core's `Ref<'T>` annotation; `'T ref` is unaffected)
- **Promises & semaphores** — `Promise<'A, 'E>`, completed once and awaited by many, and `Semaphore`, which bounds how many effects run at once
- **Console** — lines and keys as effects; `tryReadLine` yields `None` at end of input

## Runtimes

Effects are interpreted by a runtime. Pick one explicitly — `DirectRuntime` (`open FIO.Runtime.Direct`) or
`WorkStealingRuntime` (`open FIO.Runtime.WorkStealing`) — or use `DefaultRuntime` (`open FIO.Runtime.Default`).

| Runtime | Notes |
|---------|-------|
| `DirectRuntime` | Multi-threaded via the .NET thread pool — one task per fiber, no scheduler of its own. Handy for tests and as a baseline. |
| `WorkStealingRuntime` | Multi-threaded, work-stealing scheduler. **The default.** |

`DefaultRuntime = WorkStealingRuntime` — `FIOApp` uses it unless you `override _.runtime`.

A runtime is `IDisposable` (`use runtime = new DefaultRuntime()`). Disposing it interrupts every fiber still running on it — roots started
with `Run`, and daemons — waits up to 10 seconds for their finalizers, then stops its workers;
`Shutdown timeout` does the same with your own bound. A fiber still unwinding when the time is up is
abandoned: on `WorkStealingRuntime` it runs no further step once its current slice of steps ends (a
step that is running your code runs to its end), while on `DirectRuntime`, which has no workers to
stop, it goes on unwinding after `Shutdown` returns. A root or daemon that had finished
and only waited for its children gets its result; a fiber deeper in the tree in that state does not.
`Run` afterwards throws `ObjectDisposedException`.
`FIOApp` disposes its runtime when the app ends.

## Benchmarks

Eleven concurrency workloads (Pingpong, Threadring, Chameneos, Philosophers, …) and a combinator
microbenchmark run against every runtime, reporting execution time and allocations. Pingpong is tracked
per commit on the [**live benchmark dashboard**](https://fs-fio.github.io/fio/dev/bench/); the full suite,
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
