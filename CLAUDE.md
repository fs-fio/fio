# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10 (LangVersion=preview), `.slnx` solution format

## Build Commands

```bash
dotnet build                                    # Build all
dotnet test                                     # Run all tests
dotnet test tests/FIO.Tests/                    # Core tests only
dotnet test tests/FIO.Sockets.Tests/            # Sockets tests only
dotnet test tests/FIO.WebSockets.Tests/         # WebSockets tests only
dotnet test --filter "Name~TestName"            # Run specific test
dotnet test --filter "Name~PropertyTests"       # Run test file/group

# Run examples (five example projects: DSL, App, Http, Sockets, WebSockets)
dotnet run --project examples/FIO.Examples.DSL
dotnet run --project examples/FIO.Examples.App
dotnet run --project examples/FIO.Examples.Http
dotnet run --project examples/FIO.Examples.Sockets
dotnet run --project examples/FIO.Examples.WebSockets

# Benchmarks (Release mode, BenchmarkDotNet)
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"           # all benchmarks
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Pingpong*"  # one benchmark
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --list flat            # list benchmarks
# Configure via env vars: FIO_BENCH_RUNTIMES, FIO_BENCH_ITERATIONS, FIO_BENCH_<NAME>_ROUNDS/ACTORS
```

## Package Structure

Four NuGet packages:
- **FIO** - Core (FIO monad, fibers, channels, runtimes, App framework, Console I/O)
- **FIO.Sockets** - TCP sockets (error type: `SocketError`)
- **FIO.WebSockets** - WebSockets (error type: `WsError`)
- **FIO.Http** - HTTP server, Kestrel-based (error type: `HttpError`)

## Key Files

Core DSL (`src/FIO/DSL/`), compile order matters:
- `Utilities.fs` - Internal boxing/atomics helpers (`boxOnError`, `boxFunc`, `boxTask`, `boxVoidTask`; `tryClaim`, `tryTransition`, `transitionFrom`, `initIfNull`)
- `Exceptions.fs` - `InterruptionCause` DU and `FiberInterruptedException`
- `Core.fs` - `FIO<'A,'E>` DU, `Fiber<'A,'E>`, `Channel<'A>`, `FiberContext`, `WorkItem`, `ContStack`. Also hosts the type's primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork`, and the transformation cluster `Map` / `MapError` / `MapBoth` / `Result` / `Option` (derived purely from `Success`/`Failure` constructors + the four primitives).
- `Factories.fs` - `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.suspend`, `FIO.sleep`, `FIO.collectAll`, `FIO.collectAllPar`, `FIO.forkTask`, etc.
- `Extensions.fs` - Instance methods built on the Core cluster (`Zip`, `Tap`, `Race`, `RaceFirst`, `Retry`, `Timeout`, `OrElse`, etc.)
- `Operators.fs` - Infix operators (`>>=`, `<!>`, `<&>`, `<|>`, etc.)
- `CE.fs` - `fio { }` computation expression builder

Console I/O (`src/FIO/Console.fs`):
- `print`, `printLine`, `readLine`, `write`, `writeLine`, `clear` wrap `System.Console` directly via `FIO.attempt`. Namespace `FIO.Console`, module `Console` (`[<RequireQualifiedAccess>]`); each function takes an `onError: exn -> 'E` argument.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` - `FIORuntime` (abstract base), `WorkerConfig`, `ContStackPool`, `WorkItemPool`
- `WorkerInfrastructure.fs` - `FIOWorkerRuntime` (adds EvaluationWorkers/EvaluationSteps/BlockingWorkers params), `WorkerLifecycle`
- `InterpreterCore.fs` - Shared interpreter logic (`InterpreterState` struct, `processOutcome`/`processResult`/`handleSharedCase`, `Outcome` DU, `RuntimeCase` DU for runtime-specific dispatch)
- `DirectRuntime.fs` - .NET Tasks, waits for blocked fibers
- `CooperativeRuntime.fs` - Custom fibers, linear-time blocked handling
- `ConcurrentRuntime.fs` - Custom fibers, constant-time blocked handling (event-driven, uses `EvaluationWorker` + `BlockingWorker`)
- `DefaultRuntime.fs` - Type alias: `DefaultRuntime = ConcurrentRuntime`

Framework (`src/FIO/App.fs`):
- `App.fs` - `FIOApp<'A,'E>` abstract base class. 5-member surface: `effect`, `runtime`, `onShutdown`, `onShutdownTimeout`, `mapExitCode` over `AppResult<'A,'E>` (`AppSucceeded`/`AppFailed`/`AppInterrupted`/`AppFatalError`).

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
    ├── CooperativeRuntime
    └── ConcurrentRuntime (= DefaultRuntime)
```

- **DirectRuntime** - .NET Tasks, waits for blocked fibers
- **CooperativeRuntime** - Custom fibers, linear-time blocked handling (polling `BlockingItem` list)
- **ConcurrentRuntime** - Custom fibers, constant-time blocked handling (event-driven via `Channel.TryRescheduleNextBlockingWorkItem` / `FiberContext.CompleteAndReschedule`)

**DefaultRuntime = ConcurrentRuntime** (recommended)

Worker config fields: **EvaluationWorkers** (evaluation worker count), **EvaluationSteps** (eval steps per work item before rescheduling), **BlockingWorkers** (blocking worker count). The `EWC`/`EWS`/`BWC` acronyms are retained only as the `ConfigString` display labels and the benchmark spec shorthand (`Concurrent-{EWC}-{EWS}-{BWC}`).

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
| `<&&>` | Par | Unit | Fire-and-forget parallel |
| `<\|>` | Seq | First success | Fallback/recovery |
| `<+>` | Seq | Choice | Either-fallback (OrElseEither) |
| `<?>` | Par | Choice | Race for first completion (RaceEither) |

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

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent in fiber runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and interruption
- **Error typing**: extensions must not leak raw exceptions as public errors

## Testing

- **Expecto + FsCheck** for property-based testing; test runner config: `Parallel`, `Summary`, `Colours 256`
- Custom FsCheck `Generators` type in `tests/FIO.Tests/Utils/Utilities.fs` provides `Arb` for all three runtimes — tests run against `DirectRuntime`, `CooperativeRuntime`, and `ConcurrentRuntime`
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `System.Console` has process-global state
- Four test projects:
  - `FIO.Tests` — core library, organized into `DSL/`, `Lib/`, `Framework/` subfolders
  - `FIO.Sockets.Tests` — TCP sockets, flat structure with `testAllRuntimes` + `withTestServer`/`withTestEchoServer` helpers
  - `FIO.WebSockets.Tests` — WebSockets, flat structure
  - `FIO.Http.Tests` — HTTP server tests
- Core tests use `Generators` type for FsCheck Arb across all 3 runtimes; extension tests use `testAllRuntimes` helper wrapping `testSequenced`
- `InternalsVisibleTo("FIO.Tests")` is set on the core project only (extension libs do not expose internals to tests)
- All WebSocket test files are enabled in the `.fsproj` (including `WebSocketServerTests.fs`); the suite passes (155 tests, no hang)
- Stack-safety canaries live in `tests/FIO.Tests/DSL/FIOTests.fs` — the three "Stack safety - deep left-chained FlatMap/CatchAll/Ensuring" tests at depth 10000 are load-bearing for the iterative-flattening design of `UpcastResult`/`UpcastError`/`UpcastBoth`. Do not "simplify" those methods to plain recursion.

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs` as needed
- **New shared effect case**: add handling to `handleSharedCase` in `InterpreterCore.fs`
- **New runtime-specific effect case**: add to `RuntimeCase` DU in `InterpreterCore.fs`, route from `handleSharedCase`, update all three runtime `RuntimeCase` matches
- **New runtime DU case**: update `Core.fs` and all three runtime interpreters (`DirectRuntime.fs`, `CooperativeRuntime.fs`, `ConcurrentRuntime.fs`)
- **Runtime change**: update interpreter logic, add tests, and validate benchmarks
- **Extension change**: update error model, DSL surface, and extension README
- **Behavior change**: update examples and tests to match new semantics

## CI

- Tests run on Ubuntu, Windows, macOS; Ubuntu collects coverage (Codecov)
- Benchmarks run on Ubuntu only (main branch + PRs)
- Publishing triggered by `v*` git tags — packs and pushes 4 NuGet packages

## Commit Style

Short, sentence-style messages (e.g., "Fix benchmark output", "Improve App.fs"). No strict prefixes; keep messages descriptive.

## Comment Style

The codebase is currently **zero-comment**: no XML documentation comments on public or non-public items anywhere (`src/`, the extensions, benchmarks, examples) — strip any that appear.

Inline `//` comments: use sparingly, only when the *why* isn't obvious from the code. Never restate what the code does. No commented-out code (use git history).

## Important Notes

- **WarningsAsErrors=true** on all projects — fix all warnings
- F# `ParallelCompilation` and `Deterministic` use SDK defaults (no project-level overrides)
- F# compile order matters — file order in `.fsproj` is the compilation order
- 4-space indentation, match existing style in `src/` and `tests/`
- Experimental compile-time flags: `DETECT_DEADLOCK`, `MONITOR` (in `Runtime.Tools/`)
