# Copilot Instructions

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10 (`LangVersion=preview`), `.slnx` solution format

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

# Benchmarks (Release mode)
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --concurrent-runtime 39 200 1 --runs 30 --pingpong 150000
```

## Architecture

### FIO Type

`FIO<'A, 'E>` is a discriminated union representing lazy effects. DU cases are `internal` — external code uses factory functions and instance methods.

Key DU cases: `Success`/`Failure` (terminal), `Action` (sync side effects), `SendChan`/`ReceiveChan` (channels), `ForkEffect`/`ForkTPLTask` (forking), `JoinFiber` (waiting), `AwaitTPLTask`/`AwaitGenericTPLTask` (.NET Task interop), `ChainSuccess`/`ChainError` (bind), `InterruptFiber`/`InterruptSelf` (interruption), `OnFinalize`/`ResumeInterrupt`/`FinalizerResult` (finalizer infrastructure).

### Runtime Hierarchy

```
FIORuntime (abstract)
├── DirectRuntime              — .NET Tasks, waits for blocked fibers
└── FIOWorkerRuntime (abstract, adds WorkerConfig: EWC/EWS/BWC)
    ├── CooperativeRuntime     — Custom fibers, linear-time blocked handling
    └── ConcurrentRuntime      — Custom fibers, constant-time blocked handling (event-driven)
```

`DefaultRuntime = ConcurrentRuntime` (recommended). Worker config params: **EWC** (evaluation worker count), **EWS** (eval steps per work item before rescheduling), **BWC** (blocking worker count).

### Key Files (compile order matters)

Core DSL (`src/FIO/DSL/`):
- `Core.fs` — `FIO<'A,'E>` DU, `Fiber`, `Channel`, `FiberContext`, `WorkItem`, `ContStack`. Hosts primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork` plus the transformation cluster (`Map`/`MapError`/`MapBoth`/`Result`/`Option`/`Choice`) derived from `Success`/`Failure` + the four primitives.
- `Factories.fs` / `Extensions.fs` / `Operators.fs` / `CE.fs` — public surface built on Core.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` — `FIORuntime` base, `WorkerConfig`, `ContStackPool`, `WorkItemPool`
- `WorkerInfrastructure.fs` — `FIOWorkerRuntime`, `WorkerLifecycle`
- `InterpreterCore.fs` — shared interpreter (`InterpreterState`, `processSuccess`/`processError`/`processInterruptError`, `handleSharedCase`, `RuntimeCase` DU for runtime-specific dispatch)
- `DirectRuntime.fs` / `CooperativeRuntime.fs` / `ConcurrentRuntime.fs` / `DefaultRuntime.fs`

Framework: `src/FIO/Framework/App.fs` — `FIOApp<'A,'E>` with 5-member surface (`effect`, `runtime`, `onShutdown`, `onShutdownTimeout`, `mapExitCode`) over `AppOutcome` (Succeeded/Failed/Interrupted/FatalError).

### Concurrency Primitives (`src/FIO/Lib/`)

- **Promise<'A,'E>** — one-shot, completed once with success/failure
- **Ref<'T>** — atomic reference, lock-free CAS (`Get`, `Set`, `GetAndUpdate`, …)
- **Semaphore** — counting semaphore, `WithPermit` for scoped acquisition
- **InterruptionCause** — `Timeout` | `ParentInterrupted` | `ExplicitInterrupt` | `InvalidArgument` | `ResourceExhaustion`

### Packages

- **FIO** — Core (effect system, fibers, channels, runtimes, App framework, Console/Clock/Environment/Random/Promise/Ref/Semaphore)
- **FIO.Sockets** — TCP sockets (error type: `SocketError`)
- **FIO.WebSockets** — WebSockets (error type: `WsError`)
- **FIO.Http** — HTTP server, Kestrel-based (error type: `HttpError`)
- **FIO.PostgreSQL** — PostgreSQL, Npgsql-based (error type: `PgError`)
- **FIO.Redis** — Redis, StackExchange.Redis-based (error type: `RedisError`)

## Conventions

### API Naming

- Factory functions use **lowercase** F#-idiomatic style: `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.sleep`, `FIO.collectAllPar`, `FIO.acquireRelease`
- Instance methods use **PascalCase**: `effect.Map(f)`, `effect.FlatMap(f)`, `effect.Fork()`, `effect.CatchAll(f)`, `effect.Ensuring(fin)`
- Lib modules use **qualified access**: `Console.printLine`, `Clock.Now()`, `Ref.Get counter`, `Semaphore.WithPermit`
- Extension libs use abbreviated module names: `Conn.OpenAsync`, `Socket.Connect`, `Ws.Send`

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

### Writing Effects

```fsharp
// Computation expression (preferred for complex logic)
let effect = fio {
    let! x = someEffect
    do! Console.printLine "msg"
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

- **WarningsAsErrors=true** on all projects — fix all warnings
- 4-space indentation
- F# compile order matters — file order in `.fsproj` is the compilation order
- Short, sentence-style commit messages (e.g., "Fix benchmark output", "Improve App.fs")
- Experimental compile-time flags: `DETECT_DEADLOCK`, `MONITOR` (in `Runtime.Tools/`)

### Comments / XML Docs

- Public items: XML doc comments required.
- Non-public items (`internal`, `private`): **no** XML doc comments — strip any that exist.
- Inline `//` comments: only when the *why* isn't obvious. Never restate what the code does. No commented-out code.

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent in fiber runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and interruption
- **Error typing**: extensions must not leak raw exceptions as public errors

## Testing

- **Expecto + FsCheck** for property-based testing
- Core tests use a `Generators` type (`tests/FIO.Tests/Utils/Utilities.fs`) that provides FsCheck `Arb` for all three runtimes — every property test runs against `DirectRuntime`, `CooperativeRuntime`, and `ConcurrentRuntime`
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `ConsoleBackend` has process-global state
- Extension tests use `testAllRuntimes` helper wrapping `testSequenced`
- Stack-safety canary tests in `tests/FIO.Tests/DSL/FIOTests.fs` (depth 10000) are load-bearing — do not simplify `UpcastResult`/`UpcastError`/`UpcastBoth` to plain recursion
- `InternalsVisibleTo("FIO.Tests")` is set on the core project only; extension libs do not expose internals to their tests
- Disabled WebSocket test files (commented out in `.fsproj`): `WebSocketServerTests.fs`, `WebSocketPoolTests.fs` (marked "THIS ONE HANGS?") — check before assuming full coverage

## CI / Release

- Tests run on Ubuntu, Windows, macOS; Ubuntu collects coverage (Codecov)
- Benchmarks run on Ubuntu only (main branch + PRs)
- Publishing triggered by `v*` git tags — packs and pushes the NuGet packages

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs`
- **New runtime DU case**: update `Core.fs` and all three runtime interpreters (`DirectRuntime.fs`, `CooperativeRuntime.fs`, `ConcurrentRuntime.fs`)
- **Runtime change**: update interpreter logic, add tests, validate benchmarks
- **Extension change**: update error model, DSL surface, and extension README
