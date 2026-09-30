# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

> Personal, machine-specific guidance (working preferences and local setup) lives in an untracked
> `CLAUDE.local.md` alongside this file. It is git-ignored; this file is the shared, committed guidance.

## Project Overview

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10, `.slnx` solution format (`FIO.slnx`). SDK pinned to `10.0.400` via `global.json` (`rollForward: latestMinor`).

Repository: <https://github.com/fs-fio/fio> · License: MIT · Baseline version: `0.5.0-beta` (single source of truth in `Directory.Build.props`).

## Build Commands

```bash
dotnet build                                    # Build all (or: dotnet build ./FIO.slnx)
dotnet test                                     # Run all tests
dotnet test tests/FIO.Tests/                    # Core tests only
dotnet test tests/FIO.Sockets.Tests/            # Sockets tests only
dotnet test tests/FIO.WebSockets.Tests/         # WebSockets tests only
dotnet test tests/FIO.Http.Tests/               # HTTP tests only
dotnet test --filter "Name~TestName"            # Run specific test
dotnet test --filter "Name~PropertyTests"       # Run test file/group

# Run examples (five example projects: DSL, App, Http, Sockets, WebSockets)
dotnet run --project examples/FIO.Examples.DSL
dotnet run --project examples/FIO.Examples.App
dotnet run --project examples/FIO.Examples.Http
dotnet run --project examples/FIO.Examples.Sockets
dotnet run --project examples/FIO.Examples.WebSockets

# Benchmarks (Release mode, BenchmarkDotNet) — see the Benchmarks section below
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"           # all benchmarks
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Pingpong*"  # one benchmark
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --list flat            # list benchmarks
python benchmarks/plot.py                       # visualize results (HTML + PNG/SVG)
python benchmarks/compare.py <dirA> <dirB>      # A/B diff of two results dirs (stdlib-only)
```

Formatting follows `.editorconfig` (4-space F#, LF, UTF-8); the build is warning-clean
(`TreatWarningsAsErrors=true`). See **Formatting & Tooling**.

## Package Structure

Four NuGet packages. Folder/assembly names use the `FIO*` prefix; published **NuGet IDs use the `FSharp.` prefix**:

| Folder / assembly | NuGet package ID | Scope | Error type |
|-------------------|------------------|-------|------------|
| `FIO` | `FSharp.FIO` | Core (FIO monad, fibers, channels, runtimes, App framework, Console I/O) | `exn` (user-chosen) |
| `FIO.Sockets` | `FSharp.FIO.Sockets` | TCP sockets | `SocketError` |
| `FIO.WebSockets` | `FSharp.FIO.WebSockets` | WebSockets | `WsError` |
| `FIO.Http` | `FSharp.FIO.Http` | HTTP server, Kestrel-based | `HttpError` |

Each extension library has its own README that is the source of truth for its API design:
`src/FIO.Sockets/README.md`, `src/FIO.WebSockets/README.md`, `src/FIO.Http/README.md`. Benchmarks are
documented in `benchmarks/FIO.Benchmarks/README.md`, and the runnable example projects are indexed in
`examples/README.md`.

## Key Files

Core DSL (`src/FIO/DSL/`), compile order matters:
- `Utilities.fs` - Internal boxing/atomics helpers (`boxOnError`, `boxFunc`, `boxTask`, `boxVoidTask`; `tryClaim`, `tryTransition`, `transitionFrom`, `initIfNull`)
- `Exceptions.fs` - `InterruptionCause` DU and `FiberInterruptedException`
- `Core.fs` - `FIO<'A,'E>` DU, `Fiber<'A,'E>`, `Channel<'A>` (unbounded, or `Bounded`/`Dropping`/`Sliding` static constructors), `FiberContext`, `WorkItem`, `ContStack`, `JoinAllLatch`. Also hosts the type's primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork`, and the transformation cluster `Map` / `MapError` / `MapBoth` / `Result` / `Option` / `Choice` (derived purely from `Success`/`Failure` constructors + the four primitives).
- `Factories.fs` - `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.succeedWith` (thunk that must not throw; a throw is a `Defect`), `FIO.suspend`, `FIO.sleep`, `FIO.collectAll`, `FIO.collectAllPar`, `FIO.forkTask`, etc. Also ZIO's uninterruptible regions: `FIO.uninterruptible`, `FIO.uninterruptibleMask` (its body gets an `InterruptibilityRestorer`), and `FIO.acquireReleaseWith`, built on the internal `AcquireRelease` primitive — acquire and release run uninterruptibly, use at the caller's level, and a `useResource` that throws still runs release
- `Ref.fs` - `Ref<'A>`: atomic reference cell (boxed CAS), `Get`/`Set`/`Update`/`Modify`/`GetAndSet`/`GetAndUpdate`/`UpdateAndGet` plus `Unsafe*` accessors for non-effect code. Built on `FIO.succeedWith`; no interpreter support needed
- `Extensions.fs` - Instance methods built on the Core cluster (`Zip`, `Tap`, `Race`, `RaceFirst`, `Retry`, `Timeout`, `OrElse`, etc.). The parallel `ZipPar`/`Race` family is fail-fast — built on the internal `JoinFirst` primitive, losers are interrupted
- `Operators.fs` - Infix operators (`>>=`, `<!>`, `<&>`, `<|>`, etc.), in an `[<AutoOpen>]` module
- `CE.fs` - `fio { }` computation expression builder

Console I/O (`src/FIO/Console.fs`):
- Namespace `FIO.Console`, module `Console` (`[<RequireQualifiedAccess>]`). Output functions wrap `System.Console` via `FIO.attempt`; every function takes an `onError: exn -> 'E` argument because console I/O genuinely throws. `print`/`printLine` take a `Printf.TextWriterFormat<unit>` (formatted output); `write`/`writeLine` take a plain `string`; plus `clear`.
- `readLine` and `readKey` go through one process-global background stdin reader thread (`StdinReader`, private) and await a `TaskCompletionSource`, so a waiting fiber is interruptible and no evaluation worker is blocked. Input that arrives for an interrupted read is stashed and delivered to the next read of the same kind — tests that abandon a read must release and drain it (`withBlockingStdIn` in `ConsoleTests.fs`). `readKey` fails through `onError` when stdin is redirected, and `readLine` at end of input (`EndOfStreamException`); `tryReadLine` yields `None` there instead.

Signals (`src/FIO/Signal.fs`, after `Console.fs`):
- Namespace `FIO.Signal`, module `Signal`. `Signal.subscribe signals onError body` registers a `PosixSignalRegistration` per signal for the body's duration (through `acquireReleaseWith`, so they are disposed on every outcome) and feeds an internal unbounded .NET channel that `SignalSubscription.Next()` awaits with the fiber's token. It never sets `context.Cancel`, so each signal's default action still runs. `Next()` after the body has ended fails through `onError` (`ChannelClosedException`). On Windows only `SIGINT`, `SIGQUIT`, `SIGTERM` and `SIGHUP` register; any other signal fails through `onError` with `PlatformNotSupportedException`.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` - `FIORuntime` (abstract base, `IDisposable`: `Shutdown timeout` and `Dispose` interrupt the live roots and daemons it tracks, wait for them to unwind, then stop the workers through the per-runtime `StopWorkers` hook), `WorkerConfig`, `ContStackPool`, `WorkItemPool`
- `WorkerInfrastructure.fs` - `FIOWorkerRuntime` (adds EvaluationWorkers/EvaluationSteps/BlockingWorkers params), `WorkerLifecycle`
- `InterpreterCore.fs` - Shared interpreter logic (`InterpreterState` struct, `processOutcome`/`processResult`/`handleSharedCase`, `Outcome` DU, `RuntimeCase` DU for runtime-specific dispatch, and the park helpers for the `JoinFirst`/`JoinAllFailFast` primitives)
- `DirectRuntime.fs` - .NET Tasks, waits for blocked fibers
- `PollingRuntime.fs` - Custom fibers, linear-time blocked handling
- `SignalingRuntime.fs` - Custom fibers, event-driven blocked handling (a blocked read parks on the channel's own wait, a blocked join on the joined fiber's waiter queue; constant-time reschedule, no blocking worker). A comparison/legacy runtime — superseded as the default by `WorkStealingRuntime`
- `WorkStealingRuntime.fs` - Custom fibers, work-stealing scheduler (per-worker `runNext` slot + work-stealing deque + shared global queue; at-most-one-waker async parking). The default runtime
- `DefaultRuntime.fs` - Type alias: `DefaultRuntime = WorkStealingRuntime`

Framework (`src/FIO/App.fs`):
- `App.fs` - `FIOApp<'A,'E>` abstract base class. 7-member surface: `effect`, `runtime`, `onOutcome`, `onOutcomeTimeout`, `onShutdown`, `onShutdownTimeout`, `mapExitCode` over `AppResult<'A,'E>` (`AppSucceeded`/`AppFailed`/`AppInterrupted`/`AppFatalError`). The effect runs as a child of a root fiber that awaits it (`scoped`): an interrupted fiber publishes before its finalizers run, but the root *completes*, and completion waits for the child to unwind — that is what guarantees every finalizer has run before `onOutcome`/`onShutdown` and before the runtime is disposed. `Stop()` and the signal handlers interrupt the child; a request that races startup is applied when the child is forked. An interrupted child maps by cause: `ExplicitInterrupt`/`ParentInterrupted` → `AppInterrupted` (130); `Defect` → `AppFatalError` with the thrown exception, `InvalidArgument`/`ResourceExhaustion` → `AppFatalError` (1, as in ZIO — the same code as `AppFailed`).

Extension libs expose `[<RequireQualifiedAccess>]` modules named after their domain (e.g. `SocketClient.connect`, `ServerSocket.serve`, `WebSocketClient.connectDefault`, `Routes`, `Codec`). Type-extension modules (`SocketExtensions`, `WebSocketExtensions`, `SimpleRoutes`) are **opt-in** — they are not `[<AutoOpen>]` and must be `open`ed explicitly.

## Core Architecture

### FIO Type (`src/FIO/DSL/Core.fs`)

`FIO<'A, 'E>` is a discriminated union representing lazy effects:
- `Success`/`Failure` - Terminal values
- `Interrupt` - Self-interruption with cause and message
- `Action` - Synchronous side effects
- `WriteChan`/`ReadChan` - Channel message passing (`WriteChan` carries whether it backs `Write`, which yields the message, or `TryWrite`, which never suspends and yields whether the message was written)
- `ForkEffect` - Fork a fiber
- `JoinFiber` - Wait for fiber
- `JoinFirst` - Wait for the first of several fibers to settle
- `JoinAllFailFast` - Wait for all fibers, settling early on the first failure
- `AwaitTask` - .NET Task interop
- `ChainSuccess`/`ChainError`/`ChainBoth` - Effect composition (bind); each pushes one `ChainCont`, `ChainBoth` with both handlers
- `OnFinalize` - Interrupt-safe finalizer infrastructure
- `WithSuppression` - Uninterruptible regions and `restore` (sets an absolute suppression level derived from the outer one)
- `AcquireRelease` - `acquireReleaseWith`: runs acquire one level more suppressed; its `AcquiredCont` frame restores the caller's level and registers release in the same step
- `FiberCancellationToken` - Access the current fiber's cancellation token
- `Suspend` - Defer effect construction (thunk)

The DU cases are `internal` — external code uses factory functions (`FIO.succeed`, `FIO.fail`, etc.) and instance methods (`FlatMap`, `Map`, `Fork`, `CatchAll`).

### Runtime Hierarchy

```
FIORuntime (abstract)
├── DirectRuntime
└── FIOWorkerRuntime (abstract, adds WorkerConfig: EvaluationWorkers/EvaluationSteps/BlockingWorkers)
    ├── PollingRuntime
    ├── SignalingRuntime
    └── WorkStealingRuntime (= DefaultRuntime)
```

- **DirectRuntime** - .NET Tasks, waits for blocked fibers
- **PollingRuntime** - Custom fibers, linear-time blocked handling (polling `BlockingItem` list). A parked read waits for the blocking worker's next pass, up to its 1 ms cold sleep, which makes a small `Bounded` channel slow here (~1 ms per message)
- **SignalingRuntime** - Custom fibers, event-driven blocked handling: a blocked read parks on the channel's own wait and a blocked join on the joined fiber's waiter queue, so a fiber is rescheduled in constant time without a blocking worker. Kept as a comparison runtime; superseded as the default by WorkStealingRuntime.
- **WorkStealingRuntime** - Custom fibers, **work-stealing** scheduler: per-worker local queues (a `runNext` slot + a work-stealing deque) with work-stealing across idle workers, at-most-one-waker wakeups, and async parking.

**DefaultRuntime = WorkStealingRuntime** (recommended)

Worker config fields: **EvaluationWorkers** (worker count), **EvaluationSteps** (interpreter steps per work item before a fiber yields), **BlockingWorkers** (used by `PollingRuntime`; **ignored by `SignalingRuntime` and `WorkStealingRuntime`**, which have no dedicated blocking worker, though it must still be positive). The `EWC`/`EWS`/`BWC` acronyms are the `ConfigString` display labels and the benchmark spec shorthand (`WorkStealing-{EWC}-{EWS}-{BWC}`).

### Key Internal Types

- **Fiber<'A,'E>** - Green thread, returns `FiberResult<'A, 'E>` (Succeeded/Failed/Interrupted)
- **FiberContext** - Internal execution state: completion, interruption, cancellation token, blocking work item queue
- **Channel<'A>** - Type-safe channel backed by an internal `MailboxQueue<'A>` (wrapper over `System.Threading.Channels`) with blocking work item rescheduling. `Bounded` and `Dropping` use a bounded .NET channel in `Wait` mode, `Sliding` in `DropOldest`; only a full `Bounded` channel makes a writer wait (`parkUntilWritable` on the worker runtimes, `WriteAsync` with the fiber's token on Direct), and an interrupted writer never writes. `TryWrite` never waits: a full `Bounded` or `Dropping` channel yields `false`
- **WorkItem** - Mutable work unit: effect + fiber context + continuation stack + interruption suppression counter
- **Cont** - Continuation types: `ChainCont`/`FinalizerCont`/`PostFinalizerCont`/`RestoreSuppressionCont`/`AcquiredCont`. `ChainCont` holds a success and a failure handler, either of which may be null (a `FlatMap` has no failure handler, a `CatchAll` no success handler); a null side passes the outcome on. It is one frame, popped before either handler runs, so a failure of the success side is not handed to the error side (ZIO's `foldZIO`). One kind instead of three halves the exception-handling regions in each inlined `processOutcome`: the interpreter microbenchmarks run 3–7% faster than with three kinds (16–74% faster than 0.4.0-beta). Code added to the inlined `processOutcome` has a measurable cost, since it is expanded at every call site of each runtime's loop — and a hard limit: the JIT compiles a method with more than 2,000 basic blocks without optimization (MinOpts), silently. The Polling and Signaling loops crossed it once and ran 5–37% slower on channel workloads, so only the hot `ChainCont` arm is inline; the finalizer and suppression arms live in the `NoInlining` helpers `processRegionCont` and `unwindFinalizers`. `tests/FIO.Tests/Runtime/InterpreterSizeTests.fs` fails when any runtime's loop exceeds ~1,800 estimated blocks (it runs under `dotnet test -c Release` only — `test.yml`'s `release` job — since Debug builds have no state machine to measure), and `DOTNET_JitDisasmSummary=1` shows each loop's tier. `FinalizerCont` ensures finalizers run on interruption, not just success/error; `PostFinalizerCont` restores the saved outcome and the saved suppression level after a finalizer completes. `RestoreSuppressionCont` sets `InterruptionSuppressed` back to an absolute level on any outcome. `AcquiredCont` does the same when an acquire ends and, on success, pushes the `FinalizerCont` for release (via `unwindFinalizers`) before the region-exit check, so no step separates acquiring from registering release. `Cont` is copied on every push and pop, so a new case reuses existing fields by name and type (F# shares their storage) and `InterpreterSizeTests` fails if it grows past 56 bytes.
- **ContStack** / **ContStackPool** - Continuation stacks, pooled per-thread to reduce GC
- **WorkItemPool** - Thread-local pool for WorkItems to reduce GC pressure
- **InterruptionCause** - `ParentInterrupted` | `ExplicitInterrupt` | `InvalidArgument` | `ResourceExhaustion` | `Defect` (user code threw where no typed error could be produced)

### Concurrency Primitives

Concurrency is built on the core types: **Fiber<'A,'E>** (green threads via `.Fork()`/`.Join()`), **Channel<'A>** (typed message passing) and **Ref<'A>** (atomic reference cell, `src/FIO/DSL/Ref.fs`). There are no Promise/Semaphore primitives yet; `Console` and `Signal` are the library modules.

### Operator Reference

| Op | Exec | Returns | Use |
|----|------|---------|-----|
| `>>=` | Seq | fn result | Bind/chain |
| `<!>` | - | Transformed | Map |
| `<*>` | Seq | Tuple | Combine |
| `*>` | Seq | Second | Side effect then result |
| `<*` | Seq | First | Result then side effect |
| `<&>` | Par | Tuple | Concurrent exec |
| `&>` | Par | Second | Concurrent, keep second |
| `<&` | Par | First | Concurrent, keep first |
| `<&&>` | Par | Unit | Parallel, discard both (awaits both, fail-fast) |
| `<\|>` | Seq | First success | Fallback/recovery |
| `<+>` | Seq | Choice | Either-fallback (OrElseEither) |
| `<?>` | Par | Choice | Race for first completion (RaceEither) |

(`<\|>` is the `<|>` operator; the backslash escapes the pipe inside this markdown table.)

## Development Patterns

### Writing Effects

```fsharp
// Computation expression (preferred for complex logic)
let effect = fio {
    let! x = someEffect
    do! Console.printLine "msg" id
    return x + 1
}

// Operators (preferred for pipelines)
let effect = someEffect >>= fun x -> FIO.succeed (x + 1)
```

### Running Effects

```fsharp
// Direct (a runtime is IDisposable: disposing it interrupts what is still running)
use runtime = new DefaultRuntime()
let fiber = runtime.Run effect
match fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously with
| Succeeded v -> ...
| Failed e -> ...
| Interrupted exn -> ...

// FIOApp (recommended)
type MyApp() =
    inherit FIOApp<unit, exn>()
    override _.effect = myEffect

[<EntryPoint>]
let main _ = MyApp().Run()
```

### API Naming Convention

Factory functions use **lowercase** F#-idiomatic style: `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.sleep`, `FIO.never`, `FIO.collectAll`, `FIO.collectAllPar`, `FIO.forEach`, `FIO.forEachPar`, `FIO.suspend`, `FIO.acquireReleaseWith`.

Instance methods use **PascalCase**: `effect.Map(f)`, `effect.FlatMap(f)`, `effect.Fork()`, `effect.CatchAll(f)`, `effect.Ensuring(fin)`, `effect.ZipRight(eff)`.

Library modules use **qualified access**: e.g. `Console.printLine "msg" id`.

### Tail recursion

`[<TailCall>]` marks ordinary (non-effect) recursion that must compile to a loop: `Ref.cas`, the HTTP route
matchers (`RoutePattern.fs`, `Routes.fs`), and the four `InterpretAsync` members, where it stops the
interpreter from calling itself. It can go on module-level and class-level `let` functions and on members,
but not on a local `let rec` (FS0824). FIO effect loops (`FlatMap`, `fio { return! loop () }`) never get
it: the interpreter runs each iteration from its own loop, so they cannot overflow the stack, and the
check, which cannot tell that `FlatMap` runs its lambda later, wrongly rejects some of them. For them,
keep `return! loop ()` last so no continuation piles up. The attribute changes no IL and costs nothing at
runtime.

## Benchmarks

Macro benchmarks live in `benchmarks/FIO.Benchmarks/` (BenchmarkDotNet 0.15.8). Twelve workloads:
**Bang, Big, BoundedBuffer, Chameneos, Counting, Fibonacci, Fork, Philosophers, Pingpong, Threadring, Trapezoidal, ZipRace**. Eleven are classic concurrency workloads; **ZipRace** is a combinator microbenchmark that regression-guards the parallel-combinator primitives. Each is `[<MemoryDiagnoser>]` + `[<RankColumn>]`, sweeping its parameters × the configured runtimes, so every run reports **execution time and allocated memory**.

- **Runtimes:** spec format `Direct | Polling-{EWC}-{EWS}-{BWC} | Signaling-{EWC}-{EWS}-{BWC} | WorkStealing-{EWC}-{EWS}-{BWC}`. Set via `FIO_BENCH_RUNTIMES` (default `Direct,Polling-12-200-1,Signaling-12-200-1,WorkStealing-12-200-1`). Recommended `EWC = CPU cores − 2`.
- **Iteration control:** `FIO_BENCH_WARMUP` (default 3), `FIO_BENCH_ITERATIONS` (default 30). A CLI `--job` (e.g. `--job Dry`, `--job Short`) **overrides** these env vars.
- **Per-benchmark params:** `FIO_BENCH_<NAME>_<PARAM>` (e.g. `FIO_BENCH_PINGPONG_ROUNDS`, `FIO_BENCH_FORK_ACTORS`, `FIO_BENCH_BOUNDEDBUFFER_PRODUCERS`). Full table in `benchmarks/FIO.Benchmarks/README.md`.
- **Output:** BenchmarkDotNet writes CSV/GitHub-markdown/HTML reports to `BenchmarkDotNet.Artifacts/results/` (git-ignored).
- **Plotting (`benchmarks/plot.py`):** reads the `*-report.csv` files and writes per-benchmark + `summary` charts to `BenchmarkDotNet.Artifacts/plots/` as interactive HTML **and** static images (PNG/SVG, configurable via `--image-formats`; `pdf` also supported). Requires `pandas`, `plotly`, `kaleido`. `python benchmarks/plot.py --self-test` validates the parsers without touching artifacts.
- **A/B comparison (`benchmarks/compare.py`, stdlib-only):** diffs two results directories and emits a markdown Δtime/Δalloc table with regression/win flags. Allocations are deterministic (comparable across sessions); **wall time drifts 20–50% between sessions** on dev machines — compare times only from same-session adjacent A/B runs, bracketed by sentinel re-runs (full protocol in `benchmarks/FIO.Benchmarks/README.md`).

`benchmarks/FIO.Benchmarks/README.md` is the source of truth (parameter defaults, tuning guidance, result interpretation, allocation/boxing notes).

## Testing

- **Expecto + FsCheck** for property-based testing; test runner config: `Parallel`, `Summary`, `Colours 256`. Pinned versions (`Directory.Packages.props`): Expecto 11.1.0, FsCheck 3.4.0.
- `tests/FIO.Tests/Utils/Utilities.fs` is the single source of runtime test helpers: `allRuntimes()`, `testAllRuntimes`, `testAllRuntimesSequenced` (for `System.Console`'s process-global state), and the FsCheck `Generators` `Arb`. All of them cover **all four** runtimes — `DirectRuntime`, `PollingRuntime`, `SignalingRuntime`, `WorkStealingRuntime`. Do not redefine these per test file: a helper named "all runtimes" that quietly omits one is how a runtime-specific defect survives a green suite
- They share one `testConfig` (`EvaluationWorkers = 2`), not `WorkerConfig.Default`, and so do the extension suites' `Utils/Utilities.fs`. `allRuntimes()` builds four runtimes for every test list and Expecto runs lists in parallel, so the default (`ProcessorCount - 1`) would keep thousands of threads alive at once and make wall-clock deadline assertions flake under load. `testAllRuntimes` disposes each runtime after its test, which interrupts anything the test left running. Stress tests build their own runtimes with an explicit config
- `tests/FIO.Tests/Runtime/ConformanceTests.fs` asserts the four runtimes are observationally equivalent — defect paths, typed-error integrity, `Await`/`UnsafeResult` agreement, `RunConcurrent`. It deliberately uses `'E = string`, because `Fiber.Task()` casts the error channel with `error :?> 'E`: a non-`'E` value there raises `InvalidCastException`, which `'E = exn` silently absorbs
- Heavy stress/regression tests (deadlock & lost-wakeup guards) are **opt-in** via the `FIO_RUN_STRESS=1` env var (`stressEnabled`/`stressTestCase` in `tests/FIO.Tests/Utils/Utilities.fs`) — off by default locally, enabled in CI
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `System.Console` has process-global state
- Signal tests (`Lib/SignalTests.fs`) are sequenced too, since a signal reaches every subscription in the process; the `SIGWINCH` ones send it to the test process with `kill` and are skipped on Windows
- Four test projects:
  - `FIO.Tests` — core library, organized into `DSL/`, `Lib/`, `Framework/`, `Runtime/` subfolders; the factory functions and extension methods are split by sub-group into `DSL/Factories/*.fs` and `DSL/Extensions/*.fs`, each file a `[<Tests>]` list under the same top label ("Factory Functions" / "Extension Methods"). Test names follow `Subject - sentence` (the member under test, then the behaviour) in all four projects
  - `FIO.Sockets.Tests` — TCP sockets, flat structure with `testAllRuntimes` + `withTestServer`/`withTestEchoServer` helpers
  - `FIO.WebSockets.Tests` — WebSockets, flat structure
  - `FIO.Http.Tests` — HTTP server tests
- Core tests use `Generators` type for FsCheck Arb across all 4 runtimes; extension tests use a `testAllRuntimes` helper. The Sockets and Http helpers wrap `testSequenced`; the WebSockets suite runs in parallel, so a test there that touches process-global state (`Console.SetError`, `Console.SetIn`, …) must be wrapped in `testSequenced` explicitly
- `InternalsVisibleTo("FIO.Tests")` is set on the core project only (extension libs do not expose internals to tests)
- All WebSocket test files are enabled in the `.fsproj` (including `WebSocketServerTests.fs`); the suite passes (no hang)
- `tests/FIO.Tests/Runtime/InterpreterSizeTests.fs` guards the JIT's optimization limit for each runtime's interpreter loop (see **Cont** above). It is skipped in Debug, so it is enforced by `dotnet test -c Release` — `test.yml`'s `release` job and `publish.yml` run it
- Stack-safety canaries live in `tests/FIO.Tests/DSL/FIOTests.fs` — the four "Stack safety - deep left-chained FlatMap/CatchAll/Ensuring/MapBoth" tests at depth 10000 are load-bearing for the iterative-flattening design of `UpcastResult`/`UpcastError`/`UpcastBoth`. Do not "simplify" those methods to plain recursion.

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent in fiber runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Fail-fast parallelism**: the parallel combinators (`ZipPar`/`Race` family, `forEachPar`) settle on the first relevant completion and interrupt losers/peers — they must never hang on a stuck sibling
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and
  interruption. There is no fourth outcome: a fiber may never be discarded without unwinding. In
  particular a runtime must not drop queued work items to "reset" itself
- **Fork is scope-attached** (ZIO's `fork`, not its `forkDaemon`): when a fiber finishes it interrupts
  every fiber it forked, and **does not publish its own result until they have unwound** — so once you
  observe a fiber's result, every finalizer in its subtree has already run. `ForkDaemon` opts out: a
  daemon fiber is neither interrupted nor awaited, and its lifetime becomes the caller's to manage.
  Consequence for API design: to observe a forked child's result you must await it *inside* the parent,
  or the parent finishing will interrupt it first
- **Uninterruptible forks are protected.** An interrupted fiber interrupts its children at once, except
  those it forked while `InterruptionSuppressed > 0` (a finalizer, an uninterruptible region,
  acquire/release): `attachFork` puts them in `FiberContext`'s `protectedScope`, which is cancelled
  only when the fiber finishes unwinding (`CompleteInternal`). Without it, a finalizer's `Timeout`,
  `RaceFirst` or `ZipPar` forked children that were born interrupted (once the fiber had forked
  anything before), and an uninterruptible region lost its forked work to the interruption it defers
- **Scope completion must never block a scheduler thread.** The parent/child rendezvous is the latch in
  `FiberContext` (`completing`/`outstanding`/`published`, settled by `TryFinish`), modelled on
  `JoinAllLatch`. An earlier attempt awaited children inside `Complete` and deadlocked: the await
  occupied a worker, and with few workers no thread was left to run the children being waited for
- **Suppression means uncancellable.** While `InterruptionSuppressed > 0` (an `Ensuring` finalizer, or an `uninterruptible`/`uninterruptibleMask` region outside `restore`),
  the fiber is uninterruptible, so `FIO.cancellationToken()` yields `CancellationToken.None` and
  `awaitedTask` skips `WaitAsync`. Both follow the same rule, and it is what lets a finalizer `sleep`,
  `async` or `awaitAsync` after its fiber was interrupted — handing out the already-cancelled token
  made `Task.Delay` fault instantly and `Register` fire immediately, truncating cleanup
- **Suppression levels are absolute.** Every change of `InterruptionSuppressed` is undone by restoring a saved level, never by counting: `WithSuppression(update, body)` captures the level on entry, sets it to `update outer`, runs `body outer`, and pushes `RestoreSuppressionCont` with the captured level (a mask raises the level by one, `restore.Restore` sets the level its mask captured). So a `restore` inside a finalizer stays uninterruptible — its captured level is already above 0. `acquireReleaseWith` must keep acquire suppressed until release is registered: an acquire that takes a step after creating its resource, or resumes from a task, leaked the resource on interrupt when it ran interruptibly
- **An interruption takes effect the moment a region ends.** When a `RestoreSuppressionCont` or `PostFinalizerCont` brings the level back to 0 while the fiber's token is cancelled, `processOutcome` turns the outcome into an interruption before popping further, so no continuation code after an uninterruptible region or a finalizer runs once the fiber is interrupted (ZIO's rule). Finalizers further out still run
- **Disposal interrupts, then waits.** `Dispose`/`Shutdown` interrupt every tracked root (from `Run`) and daemon (from `ForkDaemon`), wait until their `FiberContext`s publish (the `SetOnUnwound` hook removes them from the registry; the last removal completes a latch), and only then stop the workers; scoped children are covered by their roots. The wait is bounded by the timeout: fibers still unwinding after it are abandoned with the workers. A concurrent or later `Shutdown` waits for the first. `Watch` adds a fiber before reading the disposed flag, so a `Run` racing disposal is either seen by `Shutdown` or interrupts itself; `Run` after disposal throws `ObjectDisposedException`, and a daemon forked after it is interrupted at once
- **Interrupted fibers publish immediately.** A fiber that is *interrupted* surfaces its result at once
  while its subtree unwinds behind it; only a fiber that *completes* holds its result back. Children are
  interrupted on both paths (protected ones only once the fiber has unwound), so no finalizer is
  skipped either way — only the ordering differs
- **Error typing**: extensions must not leak raw exceptions as public errors. Nothing may place a
  non-`'E` value in the error channel: when user code throws where no `'E` can be produced (a `Suspend`
  thunk, a `FlatMap`/`CatchAll` continuation, a throwing `onError`), the fiber dies with
  `InterruptionCause.Defect`, and joining an interrupted fiber propagates interruption rather than a
  typed failure

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs` as needed
- **New shared effect case**: add handling to `handleSharedCase` in `InterpreterCore.fs`
- **New runtime-specific effect case**: add to `RuntimeCase` DU in `InterpreterCore.fs`, route from `handleSharedCase`, then handle it in each runtime's `RuntimeCase` match. Before writing it out per runtime, check whether it fits an existing shared helper — `awaitedTask` (how to wait, given interruption suppression), `settledTaskEffect` (settled task → resume effect), `parkOnTask` (park on an unfinished task; the reschedule seam is the only per-runtime part), `resumeWith` (carry suspension state onto a rented work item). Only the scheduling seam should differ between runtimes
- **New runtime DU case**: update `Core.fs` and all four runtime interpreters (`DirectRuntime.fs`, `PollingRuntime.fs`, `SignalingRuntime.fs`, `WorkStealingRuntime.fs`)
- **Runtime change**: update interpreter logic, add tests, and validate benchmarks. A runtime has one
  entry point, `Run`: schedule the effect on a new fiber and return. It must never wait for, interrupt,
  or discard fibers already running — clearing scheduler queues destroys in-flight work *without*
  running finalizers, which `tests/FIO.Tests/Runtime/ConformanceTests.fs` guards against. Only
  `Shutdown`/`Dispose` interrupt running fibers, and they wait for them to unwind rather than discard them
- **Extension change**: update error model, DSL surface, and extension README
- **Behavior change**: update examples and tests to match new semantics
- **New public API**: add a concise XML doc comment per [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md)

## CI

Three GitHub Actions workflows in `.github/workflows/`:

- **`test.yml` (Run Tests)** — push/PR on **`main`** + manual. Matrix: Ubuntu, Windows, macOS (`fail-fast: false`), one identical test step per OS. Sets `FIO_RUN_STRESS=1` to enable the opt-in stress/regression tests, bounded by `timeout-minutes: 30` because a deadlock is a plausible failure mode here. Writes a TRX per OS and uploads it `if: always()` — a CI-only flake cannot be re-run with better capture. A second job, `release`, builds and tests in **Release** on Ubuntu with stress: Release is what ships, F# compiles `task { }` into state machines only when optimizing, and it is where `InterpreterSizeTests` runs (one TRX per test project via `LogFilePrefix`, as in the matrix job). **No coverage collection:** Codecov was wired up but never received a single upload (`activated: false`, 0 commits), so it was removed rather than left to pay 3–5× instrumentation cost on every Ubuntu run for nothing. Coverage is a local `dotnet test --collect:"XPlat Code Coverage"` measurement.
- **`benchmark.yml` (Performance Benchmarks)** — push/PR to **main** (skipping docs-only changes) + manual, Ubuntu only. First a **smoke test** (all benchmarks × all 4 runtimes — Direct, Polling, Signaling, WorkStealing — tiny params, `--job Dry`, 10-min timeout) to fail fast on hang/throw; then a measured **Pingpong** run across those runtimes, exported as JSON/GitHub-markdown and published to <https://fs-fio.github.io/fio/dev/bench/> via `github-action-benchmark` (`customSmallerIsBetter`, auto-pushed to the `gh-pages` branch on `main` only). That dashboard is a **tracker, not a gate** — shared-runner variance swamps any useful threshold, so `fail-on-alert` is off and the real perf gate is the local sentinel-bracketed A/B protocol. The series names embed the runtime spec (`Pingpong - WorkStealing-2-200-1`), so changing a spec starts a new series and orphans the history. It does not generate plots — plotting is a local step.
- **`publish.yml` (Publish NuGet Packages)** — on tags. `v*` = **lockstep** (all four packages; tag must equal `Directory.Build.props` `<Version>`); `core-v*`/`http-v*`/`sockets-v*`/`websockets-v*` = **per-package** release (sets `PackageReleaseVersion`, leaving the FIO dependency pinned to the baseline). Builds Release, runs tests, packs, pushes to NuGet.org, and creates a GitHub release. The NuGet push is gated on `refs/tags/`, so a `workflow_dispatch` run is a dry run: it builds, tests, packs and uploads the artifact without publishing.

## Commit Style

Short, sentence-style messages (e.g., "Fix benchmark output", "Improve App.fs"). No strict prefixes; keep messages descriptive.

## Comment Style

Two tiers — see [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md) for the full guide and per-construct examples:

- **Public, user-facing API** (callable from a referencing package): a concise, ZIO-style XML doc comment (`///`). One verb-first summary line by default; "this effect" voice; describe behavior, not the signature. **Never** document internal/private items, the `FIO` DU cases, runtime internals, or the `FIOBuilder` CE methods.
- **Internals**: comment-free. Use an inline `//` only when the *why* isn't obvious from the code. Never restate what the code does. No commented-out code (use git history).

Keep doc comments well-formed XML: rephrase types out of prose ("an effect") or escape them (`FIO&lt;'A,'E&gt;`). The public, user-facing API across all four packages is now documented; `WarnOn 3390` is enabled so malformed doc XML fails the build. Internals remain bare by design — keep them that way, and add `///` docs to any new public members you introduce.

## Formatting & Tooling

- **`.editorconfig`** governs formatting: UTF-8, LF line endings, final newline, trim trailing whitespace (except `*.md`). 4-space indent for F# (`*.fs/fsi/fsx`) and project files (`*.fsproj/props/targets/slnx`); 2-space for JSON/YAML.
- **`TreatWarningsAsErrors=true`** — set once in `Directory.Build.props`, applies to every project. Fix all warnings. Use `TreatWarningsAsErrors`, **not** `WarningsAsErrors` (the F# SDK reads the latter as a warning-number list).
- **XML docs:** packable libraries set `GenerateDocumentationFile=true` and `WarnOn 3390`, so malformed doc XML fails the build.
- **Central Package Management:** all package versions are pinned in `Directory.Packages.props` (e.g. FSharp.Core 10.1.401, BenchmarkDotNet 0.15.8). FSharp.Core's implicit reference is disabled in favor of an explicit, version-less `PackageReference` so the central version wins.
- **Versioning:** the baseline `<Version>` lives once in `Directory.Build.props`; the publish workflow overrides it from the git tag. Only the four `src/` libraries are packable (`IsPackable`); tests/benchmarks/examples are not.

## Important Notes

- F# `ParallelCompilation` and `Deterministic` use SDK defaults (no project-level overrides)
- F# compile order matters — file order in `.fsproj` is the compilation order
- Match existing style in `src/` and `tests/`; the build is warning-clean, so keep it that way
