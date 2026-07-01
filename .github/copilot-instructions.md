# Copilot Instructions

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10, `.slnx` solution format

## Build & Test

```bash
dotnet build                                    # Build all
dotnet test                                     # Run all tests
dotnet test tests/FIO.Tests/                    # Core tests only
dotnet test tests/FIO.Sockets.Tests/            # Sockets tests only
dotnet test tests/FIO.WebSockets.Tests/         # WebSockets tests only
dotnet test --filter "Name~TestName"            # Run specific test
dotnet test --filter "Name~PropertyTests"       # Run test file/group

# Examples (five projects: DSL, App, Http, Sockets, WebSockets)
dotnet run --project examples/FIO.Examples.DSL

# Benchmarks (Release mode, BenchmarkDotNet)
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"           # all benchmarks
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --list flat            # list benchmarks
# Configure via env vars: FIO_BENCH_RUNTIMES, FIO_BENCH_ITERATIONS, FIO_BENCH_<NAME>_ROUNDS/ACTORS
```

## Architecture

### FIO Type

`FIO<'A, 'E>` is a discriminated union representing lazy effects. DU cases are `internal` — external code uses factory functions and instance methods.

Key DU cases: `Success`/`Failure` (terminal), `Interrupt` (self-interrupt), `Action` (sync side effects), `WriteChan`/`ReadChan` (channels), `ForkEffect` (forking), `JoinFiber` (waiting), `AwaitTask` (.NET Task interop), `ChainSuccess`/`ChainError`/`ChainBoth` (bind), `OnFinalize` (finalizer infrastructure), `FiberCancellationToken` (current fiber token), `Suspend` (deferred construction).

### Runtime Hierarchy

```
FIORuntime (abstract)
├── DirectRuntime              — .NET Tasks, waits for blocked fibers
└── FIOWorkerRuntime (abstract, adds WorkerConfig: EvaluationWorkers/EvaluationSteps/BlockingWorkers)
    ├── PollingRuntime         — Custom fibers, linear-time blocked handling (polling)
    ├── SignalingRuntime       — Custom fibers, event-driven blocked handling (dedicated BlockingWorker + signal queue; constant-time). Comparison/legacy runtime
    └── WorkStealingRuntime    — Custom fibers, work-stealing scheduler (per-worker queues + work-stealing). The default
```

`DefaultRuntime = WorkStealingRuntime` (recommended). Worker config fields: **EvaluationWorkers** (evaluation worker count), **EvaluationSteps** (eval steps per work item before rescheduling), **BlockingWorkers** (used by `PollingRuntime`; **ignored by `WorkStealingRuntime`**). The `EWC`/`EWS`/`BWC` acronyms remain as the `ConfigString` display labels and benchmark spec shorthand.

### Key Files (compile order matters)

Core DSL (`src/FIO/DSL/`):
- `Core.fs` — `FIO<'A,'E>` DU, `Fiber`, `Channel`, `FiberContext`, `WorkItem`, `ContStack`. Hosts primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork` plus the transformation cluster (`Map`/`MapError`/`MapBoth`/`Result`/`Option`/`Choice`) derived from `Success`/`Failure` + the four primitives.
- `Factories.fs` / `Extensions.fs` / `Operators.fs` / `CE.fs` — public surface built on Core.

Console I/O (`src/FIO/Console.fs`):
- `FIO.Console.Console` (`[<RequireQualifiedAccess>]`): `print`, `printLine`, `readLine`, `write`, `writeLine`, `clear`, each taking `onError: exn -> 'E`.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` — `FIORuntime` base, `WorkerConfig`, `ContStackPool`, `WorkItemPool`
- `WorkerInfrastructure.fs` — `FIOWorkerRuntime`, `WorkerLifecycle`
- `InterpreterCore.fs` — shared interpreter (`InterpreterState`, `processOutcome`/`processResult`/`handleSharedCase`, `Outcome` DU, `RuntimeCase` DU for runtime-specific dispatch)
- `DirectRuntime.fs` / `PollingRuntime.fs` / `SignalingRuntime.fs` / `WorkStealingRuntime.fs` / `DefaultRuntime.fs` (alias for `WorkStealingRuntime`)

Framework (`src/FIO/App.fs`): `FIOApp<'A,'E>` with 7-member surface (`effect`, `runtime`, `onOutcome`, `onOutcomeTimeout`, `onShutdown`, `onShutdownTimeout`, `mapExitCode`) over `AppResult` (`AppSucceeded`/`AppFailed`/`AppInterrupted`/`AppFatalError`).

### Concurrency Primitives

- **Fiber<'A,'E>** — green thread via `.Fork()` / `.Join()`
- **Channel<'A>** — typed message passing between fibers
- **InterruptionCause** — `ParentInterrupted` | `ExplicitInterrupt` | `InvalidArgument` | `ResourceExhaustion`

### Packages

- **FIO** — Core (effect system, fibers, channels, runtimes, App framework, Console)
- **FIO.Sockets** — TCP sockets (error type: `SocketError`)
- **FIO.WebSockets** — WebSockets (error type: `WsError`)
- **FIO.Http** — HTTP server, Kestrel-based (error type: `HttpError`)

## Conventions

### API Naming

- Factory functions use **lowercase** F#-idiomatic style: `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.sleep`, `FIO.collectAllPar`, `FIO.acquireReleaseWith`
- Instance methods use **PascalCase**: `effect.Map(f)`, `effect.FlatMap(f)`, `effect.Fork()`, `effect.CatchAll(f)`, `effect.Ensuring(fin)`
- Lib modules use **qualified access**: e.g. `Console.printLine "msg" id`
- Extension libs expose `[<RequireQualifiedAccess>]` modules (`SocketClient.connect`, `ServerSocket.serve`, `WebSocketClient.connectDefault`, `Routes`, `Codec`); type-extension modules (`SocketExtensions`, `WebSocketExtensions`, `SimpleRoutes`) are opt-in (not `[<AutoOpen>]`, must be `open`ed)

### Operators

| Op | Execution | Returns | Use |
|----|-----------|---------|-----|
| `>>=` | Sequential | fn result | Bind/chain |
| `<!>` | — | Transformed | Map |
| `<*>` | Sequential | Tuple | Combine |
| `*>` / `<*` | Sequential | Second / First | Side effect ordering |
| `<&>` | Parallel | Tuple | Concurrent exec |
| `&>` / `<&` | Parallel | Second / First | Concurrent, keep one |
| `<&&>` | Parallel | Unit | Fire-and-forget parallel |
| `<\|>` | Sequential | First success | Fallback/recovery |
| `<+>` | Sequential | Choice | Either-fallback (OrElseEither) |
| `<?>` | Parallel | Choice | Race for first completion (RaceEither) |

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

### Style

- **TreatWarningsAsErrors=true** (in `Directory.Build.props`, applies to all projects) — fix all warnings
- 4-space indentation
- F# compile order matters — file order in `.fsproj` is the compilation order
- Short, sentence-style commit messages (e.g., "Fix benchmark output", "Improve App.fs")

### Comments / XML Docs

Two tiers — see [`docs/COMMENT_STYLE.md`](../docs/COMMENT_STYLE.md):

- **Public, user-facing API** (callable from a referencing package): a concise, ZIO-style `///` XML doc comment — one verb-first summary line, "this effect" voice, describe behavior not signature. The public surface is already fully documented; keep it in sync and add a `///` to any new public member. Never document internal/private items, the `FIO` DU cases, runtime internals, or the `FIOBuilder` CE methods.
- **Internals**: comment-free. Inline `//` only when the *why* isn't obvious — never restate what the code does. No commented-out code.

`WarnOn 3390` is enabled, so malformed doc XML fails the build.

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent in fiber runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and interruption
- **Error typing**: extensions must not leak raw exceptions as public errors

## Testing

- **Expecto + FsCheck** for property-based testing
- Core tests use a `Generators` type (`tests/FIO.Tests/Utils/Utilities.fs`) that provides FsCheck `Arb` for all four runtimes — every property test runs against `DirectRuntime`, `PollingRuntime`, `SignalingRuntime`, and `WorkStealingRuntime`
- Heavy stress/regression tests (deadlock & lost-wakeup guards) are opt-in via `FIO_RUN_STRESS=1` (`stressEnabled`/`stressTestCase` in the same file) — off by default locally, set in CI by `test.yml`
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `System.Console` has process-global state
- Extension tests use `testAllRuntimes` helper wrapping `testSequenced`
- Stack-safety canary tests in `tests/FIO.Tests/DSL/FIOTests.fs` (depth 10000) are load-bearing — do not simplify `UpcastResult`/`UpcastError`/`UpcastBoth` to plain recursion
- `InternalsVisibleTo("FIO.Tests")` is set on the core project only; extension libs do not expose internals to their tests
- All WebSocket test files are enabled in the `.fsproj` (including `WebSocketServerTests.fs`); the suite passes (no hang)

## CI / Release

- Tests run on Ubuntu, Windows, macOS; Ubuntu collects coverage (Codecov)
- Benchmarks run on Ubuntu only (main branch + PRs)
- Publishing triggered by `v*` git tags — packs and pushes the NuGet packages

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs`
- **New runtime DU case**: update `Core.fs` and all four runtime interpreters (`DirectRuntime.fs`, `PollingRuntime.fs`, `SignalingRuntime.fs`, `WorkStealingRuntime.fs`)
- **Runtime change**: update interpreter logic, add tests, validate benchmarks
- **Extension change**: update error model, DSL surface, and extension README
