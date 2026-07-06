# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

> Personal, machine-specific guidance (working preferences and local setup) lives in an untracked
> `CLAUDE.local.md` alongside this file. It is git-ignored; this file is the shared, committed guidance.

## Project Overview

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10, `.slnx` solution format (`FIO.slnx`). SDK pinned to `10.0.301` via `global.json` (`rollForward: latestMinor`).

Repository: <https://github.com/fs-fio/fio> · License: MIT · Baseline version: `0.2.0-beta` (single source of truth in `Directory.Build.props`).

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
documented in `benchmarks/FIO.Benchmarks/README.md`.

## Key Files

Core DSL (`src/FIO/DSL/`), compile order matters:
- `Utilities.fs` - Internal boxing/atomics helpers (`boxOnError`, `boxFunc`, `boxTask`, `boxVoidTask`; `tryClaim`, `tryTransition`, `transitionFrom`, `initIfNull`)
- `Exceptions.fs` - `InterruptionCause` DU and `FiberInterruptedException`
- `Core.fs` - `FIO<'A,'E>` DU, `Fiber<'A,'E>`, `Channel<'A>`, `FiberContext`, `WorkItem`, `ContStack`, `JoinAllLatch`. Also hosts the type's primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork`, and the transformation cluster `Map` / `MapError` / `MapBoth` / `Result` / `Option` / `Choice` (derived purely from `Success`/`Failure` constructors + the four primitives).
- `Factories.fs` - `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.suspend`, `FIO.sleep`, `FIO.collectAll`, `FIO.collectAllPar`, `FIO.forkTask`, etc.
- `Extensions.fs` - Instance methods built on the Core cluster (`Zip`, `Tap`, `Race`, `RaceFirst`, `Retry`, `Timeout`, `OrElse`, etc.). The parallel `ZipPar`/`Race` family is fail-fast — built on the internal `JoinFirst` primitive, losers are interrupted
- `Operators.fs` - Infix operators (`>>=`, `<!>`, `<&>`, `<|>`, etc.), in an `[<AutoOpen>]` module
- `CE.fs` - `fio { }` computation expression builder

Console I/O (`src/FIO/Console.fs`):
- Namespace `FIO.Console`, module `Console` (`[<RequireQualifiedAccess>]`). Functions wrap `System.Console` via `FIO.attempt`; each takes an `onError: exn -> 'E` argument. `print`/`printLine` take a `Printf.TextWriterFormat<unit>` (formatted output); `write`/`writeLine` take a plain `string`; plus `readLine` and `clear`.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` - `FIORuntime` (abstract base), `WorkerConfig`, `ContStackPool`, `WorkItemPool`
- `WorkerInfrastructure.fs` - `FIOWorkerRuntime` (adds EvaluationWorkers/EvaluationSteps/BlockingWorkers params), `WorkerLifecycle`
- `InterpreterCore.fs` - Shared interpreter logic (`InterpreterState` struct, `processOutcome`/`processResult`/`handleSharedCase`, `Outcome` DU, `RuntimeCase` DU for runtime-specific dispatch, and the park helpers for the `JoinFirst`/`JoinAllFailFast` primitives)
- `DirectRuntime.fs` - .NET Tasks, waits for blocked fibers
- `PollingRuntime.fs` - Custom fibers, linear-time blocked handling
- `SignalingRuntime.fs` - Custom fibers, event-driven blocked handling (dedicated `BlockingWorker` + signal queue; constant-time reschedule). A comparison/legacy runtime — superseded as the default by `WorkStealingRuntime`
- `WorkStealingRuntime.fs` - Custom fibers, work-stealing scheduler (per-worker `runNext` slot + work-stealing deque + shared global queue; at-most-one-waker async parking). The default runtime
- `DefaultRuntime.fs` - Type alias: `DefaultRuntime = WorkStealingRuntime`

Framework (`src/FIO/App.fs`):
- `App.fs` - `FIOApp<'A,'E>` abstract base class. 7-member surface: `effect`, `runtime`, `onOutcome`, `onOutcomeTimeout`, `onShutdown`, `onShutdownTimeout`, `mapExitCode` over `AppResult<'A,'E>` (`AppSucceeded`/`AppFailed`/`AppInterrupted`/`AppFatalError`).

Extension libs expose `[<RequireQualifiedAccess>]` modules named after their domain (e.g. `SocketClient.connect`, `ServerSocket.serve`, `WebSocketClient.connectDefault`, `Routes`, `Codec`). Type-extension modules (`SocketExtensions`, `WebSocketExtensions`, `SimpleRoutes`) are **opt-in** — they are not `[<AutoOpen>]` and must be `open`ed explicitly.

## Core Architecture

### FIO Type (`src/FIO/DSL/Core.fs`)

`FIO<'A, 'E>` is a discriminated union representing lazy effects:
- `Success`/`Failure` - Terminal values
- `Interrupt` - Self-interruption with cause and message
- `Action` - Synchronous side effects
- `WriteChan`/`ReadChan` - Channel message passing
- `ForkEffect` - Fork a fiber
- `JoinFiber` - Wait for fiber
- `JoinFirst` - Wait for the first of several fibers to settle
- `JoinAllFailFast` - Wait for all fibers, settling early on the first failure
- `AwaitTask` - .NET Task interop
- `ChainSuccess`/`ChainError`/`ChainBoth` - Effect composition (bind)
- `OnFinalize` - Interrupt-safe finalizer infrastructure
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
- **PollingRuntime** - Custom fibers, linear-time blocked handling (polling `BlockingItem` list)
- **SignalingRuntime** - Custom fibers, event-driven blocked handling: a dedicated `BlockingWorker` reschedules blocked fibers via a signal queue (constant-time). Kept as a comparison runtime; superseded as the default by WorkStealingRuntime.
- **WorkStealingRuntime** - Custom fibers, **work-stealing** scheduler: per-worker local queues (a `runNext` slot + a work-stealing deque) with work-stealing across idle workers, at-most-one-waker wakeups, and async parking.

**DefaultRuntime = WorkStealingRuntime** (recommended)

Worker config fields: **EvaluationWorkers** (worker count), **EvaluationSteps** (interpreter steps per work item before a fiber yields), **BlockingWorkers** (used by `PollingRuntime`; **ignored by `WorkStealingRuntime`**, which has no dedicated blocking worker). The `EWC`/`EWS`/`BWC` acronyms are the `ConfigString` display labels and the benchmark spec shorthand (`WorkStealing-{EWC}-{EWS}-{BWC}`).

### Key Internal Types

- **Fiber<'A,'E>** - Green thread, returns `FiberResult<'A, 'E>` (Succeeded/Failed/Interrupted)
- **FiberContext** - Internal execution state: completion, interruption, cancellation token, blocking work item queue
- **Channel<'A>** - Type-safe channel backed by an internal `MailboxQueue<'A>` (wrapper over `System.Threading.Channels`) with blocking work item rescheduling
- **WorkItem** - Mutable work unit: effect + fiber context + continuation stack + interruption suppression counter
- **Cont** - Continuation types: `SuccessCont`/`FailureCont`/`FinalizerCont`/`PostFinalizerCont`. `FinalizerCont` ensures finalizers run on interruption, not just success/error; `PostFinalizerCont` restores the saved outcome after a finalizer completes.
- **ContStack** / **ContStackPool** - Continuation stacks, pooled per-thread to reduce GC
- **WorkItemPool** - Thread-local pool for WorkItems to reduce GC pressure
- **InterruptionCause** - `ParentInterrupted` | `ExplicitInterrupt` | `InvalidArgument` | `ResourceExhaustion`

### Concurrency Primitives

Concurrency is built on the core types: **Fiber<'A,'E>** (green threads via `.Fork()`/`.Join()`) and **Channel<'A>** (typed message passing). There are currently no higher-level primitive modules (Promise/Ref/Semaphore); `Console` is the only library module.

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
// Direct
let fiber = DefaultRuntime().Run effect
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

- **Expecto + FsCheck** for property-based testing; test runner config: `Parallel`, `Summary`, `Colours 256`. Pinned versions (`Directory.Packages.props`): Expecto 10.2.3, FsCheck 2.16.6.
- Custom FsCheck `Generators` type in `tests/FIO.Tests/Utils/Utilities.fs` provides `Arb` for all four runtimes — tests run against `DirectRuntime`, `PollingRuntime`, `SignalingRuntime`, and `WorkStealingRuntime`
- Heavy stress/regression tests (deadlock & lost-wakeup guards) are **opt-in** via the `FIO_RUN_STRESS=1` env var (`stressEnabled`/`stressTestCase` in `tests/FIO.Tests/Utils/Utilities.fs`) — off by default locally, enabled in CI
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `System.Console` has process-global state
- Four test projects:
  - `FIO.Tests` — core library, organized into `DSL/`, `Lib/`, `Framework/` subfolders
  - `FIO.Sockets.Tests` — TCP sockets, flat structure with `testAllRuntimes` + `withTestServer`/`withTestEchoServer` helpers
  - `FIO.WebSockets.Tests` — WebSockets, flat structure
  - `FIO.Http.Tests` — HTTP server tests
- Core tests use `Generators` type for FsCheck Arb across all 4 runtimes; extension tests use `testAllRuntimes` helper wrapping `testSequenced`
- `InternalsVisibleTo("FIO.Tests")` is set on the core project only (extension libs do not expose internals to tests)
- All WebSocket test files are enabled in the `.fsproj` (including `WebSocketServerTests.fs`); the suite passes (no hang)
- Stack-safety canaries live in `tests/FIO.Tests/DSL/FIOTests.fs` — the three "Stack safety - deep left-chained FlatMap/CatchAll/Ensuring" tests at depth 10000 are load-bearing for the iterative-flattening design of `UpcastResult`/`UpcastError`/`UpcastBoth`. Do not "simplify" those methods to plain recursion.

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent in fiber runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Fail-fast parallelism**: the parallel combinators (`ZipPar`/`Race` family, `forEachPar`) settle on the first relevant completion and interrupt losers/peers — they must never hang on a stuck sibling
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and interruption
- **Error typing**: extensions must not leak raw exceptions as public errors

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs` as needed
- **New shared effect case**: add handling to `handleSharedCase` in `InterpreterCore.fs`
- **New runtime-specific effect case**: add to `RuntimeCase` DU in `InterpreterCore.fs`, route from `handleSharedCase`, update all four runtime `RuntimeCase` matches
- **New runtime DU case**: update `Core.fs` and all four runtime interpreters (`DirectRuntime.fs`, `PollingRuntime.fs`, `SignalingRuntime.fs`, `WorkStealingRuntime.fs`)
- **Runtime change**: update interpreter logic, add tests, and validate benchmarks
- **Extension change**: update error model, DSL surface, and extension README
- **Behavior change**: update examples and tests to match new semantics
- **New public API**: add a concise XML doc comment per [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md)

## CI

Three GitHub Actions workflows in `.github/workflows/`:

- **`test.yml` (Run Tests)** — push/PR on **all branches** + manual. Matrix: Ubuntu, Windows, macOS (`fail-fast: false`). Sets `FIO_RUN_STRESS=1` to enable the opt-in stress/regression tests. Restores tools + deps, builds `./FIO.slnx`. Non-Ubuntu runs tests; Ubuntu runs tests with `XPlat Code Coverage` and uploads to **Codecov**.
- **`benchmark.yml` (Performance Benchmarks)** — push/PR to **main** + manual, Ubuntu only. First a **smoke test** (all benchmarks × all 4 runtimes — Direct, Polling, Signaling, WorkStealing — tiny params, `--job Dry`, 10-min timeout) to fail fast on hang/throw; then a measured **Pingpong** run across those runtimes, exported as JSON/GitHub-markdown and tracked over time via `github-action-benchmark` (`customSmallerIsBetter`, alerts on regression). It does not generate plots — plotting is a local step.
- **`publish.yml` (Publish NuGet Packages)** — on tags. `v*` = **lockstep** (all four packages; tag must equal `Directory.Build.props` `<Version>`); `core-v*`/`http-v*`/`sockets-v*`/`websockets-v*` = **per-package** release (sets `PackageReleaseVersion`, leaving the FIO dependency pinned to the baseline). Builds Release, runs tests, packs, pushes to NuGet.org, and creates a GitHub release.

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
- **Central Package Management:** all package versions are pinned in `Directory.Packages.props` (e.g. FSharp.Core 10.1.301, BenchmarkDotNet 0.15.8). FSharp.Core's implicit reference is disabled in favor of an explicit, version-less `PackageReference` so the central version wins.
- **Versioning:** the baseline `<Version>` lives once in `Directory.Build.props`; the publish workflow overrides it from the git tag. Only the four `src/` libraries are packable (`IsPackable`); tests/benchmarks/examples are not.

## Important Notes

- F# `ParallelCompilation` and `Deterministic` use SDK defaults (no project-level overrides)
- F# compile order matters — file order in `.fsproj` is the compilation order
- Match existing style in `src/` and `tests/`; the build is warning-clean, so keep it that way
