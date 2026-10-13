# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

> Personal, machine-specific guidance (working preferences and local setup) lives in an untracked
> `CLAUDE.local.md` alongside this file. It is git-ignored; this file is the shared, committed guidance.

## Project Overview

FIO is a type-safe, purely functional effect system for F#. IO monad + fibers (green threads) for concurrent/async apps.

**Target:** .NET 10, F# 10 (`TargetFramework` is set once, in `Directory.Build.props`), `.slnx` solution format (`FIO.slnx`). SDK pinned to `10.0.400` via `global.json` (`rollForward: latestMinor`).

Repository: <https://github.com/fs-fio/fio> · License: MIT · Baseline version: `0.6.0-beta` (single source of truth in `Directory.Build.props`).

## Build Commands

```bash
dotnet build                                    # Build all (or: dotnet build ./FIO.slnx)
dotnet test                                     # Run all tests
dotnet test tests/FIO.Tests/                    # Core tests only
dotnet test tests/FIO.Sockets.Tests/            # Sockets tests only
dotnet test tests/FIO.WebSockets.Tests/         # WebSockets tests only
dotnet test tests/FIO.Http.Tests/               # HTTP tests only
dotnet test --filter "Name~TestName"            # Run specific test
dotnet test --filter "Name~Runtime conformance" # Run test file/group

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
python3 benchmarks/plot.py                      # visualize results (HTML + PNG/SVG; needs pandas, plotly, kaleido)
python3 benchmarks/compare.py <dirA> <dirB>     # A/B diff of two results dirs (stdlib-only)

# Scripts (the only two; there is deliberately no Makefile — the dotnet CLI is the build interface)
bash scripts/pack.sh                            # pack the four packages as a lockstep publish.yml run does, into artifacts/
bash scripts/stress-test.sh 50 [dotnet test args]  # run the Release suite N times, counting failures (prefix FIO_RUN_STRESS=1 for the stress tests)
```

## Package Structure

Four NuGet packages. Folder/assembly names use the `FIO*` prefix; published **NuGet IDs use the `FSharp.` prefix**:

| Folder / assembly | NuGet package ID | Scope | Error type |
|-------------------|------------------|-------|------------|
| `FIO` | `FSharp.FIO` | Core (FIO monad, fibers, channels, runtimes, App framework, Console I/O) | `'E` (user-chosen, e.g. `exn`) |
| `FIO.Sockets` | `FSharp.FIO.Sockets` | TCP sockets | `SocketError` |
| `FIO.WebSockets` | `FSharp.FIO.WebSockets` | WebSockets | `WsError` |
| `FIO.Http` | `FSharp.FIO.Http` | HTTP server, Kestrel-based | `exn` (`HttpError` models failures inside a handler) |

Each extension library has its own README that is the source of truth for its API design:
`src/FIO.Sockets/README.md`, `src/FIO.WebSockets/README.md`, `src/FIO.Http/README.md`. Benchmarks are
documented in `benchmarks/FIO.Benchmarks/README.md`, and the runnable example projects are indexed in
`examples/README.md`.

## Key Files

Core DSL (`src/FIO/DSL/`), compile order matters:
- `Utilities.fs` - Internal boxing/atomics helpers (`boxOnError`, `boxFunc`, `boxTask`, `boxVoidTask`; `tryClaim`, `tryTransition`, `transitionFrom`, `initIfNull`) and `validateTimeout`
- `Exceptions.fs` - `InterruptionCause` DU and `FiberInterruptedException`
- `Core.fs` - `FIO<'A,'E>` DU, `Fiber<'A,'E>`, `Channel<'A>` (unbounded, or `Bounded`/`Dropping`/`Sliding` static constructors), `FiberContext`, `WorkItem`, `Cont`, `JoinAllLatch`. Also hosts the type's primitive instance members: `FlatMap`, `CatchAll`, `Ensuring`, `Fork`, `ForkDaemon`, and the transformation cluster `Map` / `MapError` / `MapBoth` / `Result` / `Option` / `Choice` (built from `Success`/`Failure` constructors on `FlatMap`/`CatchAll`, or, for `MapBoth`/`Result`/`Option`/`Choice`, on a single `ChainBoth`).
- `Factories.fs` - `FIO.succeed`, `FIO.fail`, `FIO.attempt`, `FIO.succeedWith` (thunk that must not throw; a throw is a `Defect`), `FIO.suspend`, `FIO.sleep`, `FIO.collectAll`, `FIO.collectAllPar`, `FIO.forkTask`, etc. Also ZIO's uninterruptible regions: `FIO.uninterruptible`, `FIO.uninterruptibleMask` (its body gets an `InterruptibilityRestorer`), and `FIO.acquireReleaseWith`, built on the internal `AcquireRelease` primitive — acquire and release run uninterruptibly, use at the caller's level, and a `useResource` that throws still runs release
- `Ref.fs` - `Ref<'A>`: atomic reference cell (boxed CAS), `Get`/`Set`/`Update`/`Modify`/`GetAndSet`/`GetAndUpdate`/`UpdateAndGet` plus `Unsafe*` accessors for non-effect code. Built on `FIO.succeedWith`; no interpreter support needed
- `Extensions.fs` - Instance methods built on the Core cluster (`Zip`, `Tap`, `Race`, `RaceFirst`, `Retry`, `Timeout`, `OrElse`, etc.). The parallel `ZipPar`/`Race` family is built on the internal `JoinFirst` primitive and interrupts its losers; `ZipPar` is fail-fast, while the first-to-succeed `Race`/`RaceEither`/`ZipParError` wait for the other side when the first to settle fails
- `Operators.fs` - Infix operators (`>>=`, `<!>`, `<&>`, `<|>`, etc.), in an `[<AutoOpen>]` module
- `CE.fs` - `fio { }` computation expression builder

Console I/O (`src/FIO/Console.fs`):
- Namespace `FIO.Console`, module `Console` (`[<RequireQualifiedAccess>]`). Output functions wrap `System.Console` via `FIO.attempt`; every function takes an `onError: exn -> 'E` argument because console I/O genuinely throws. `print`/`printLine` take a `Printf.TextWriterFormat<unit>` (formatted output); `write`/`writeLine` take a plain `string`; plus `clear`.
- `readLine` and `readKey` go through one process-global background stdin reader thread (`StdinReader`, internal for the tests' `readKey` seam) and await a `TaskCompletionSource`, so a waiting fiber is interruptible and no evaluation worker is blocked. Input that arrives for an interrupted read is kept and delivered to the next read: a line to the next `readLine`, a key to the next `readKey` or as the first character of the next `readLine`; a value handed to a fiber interrupted before using it goes back (`awaitStdin`'s finalizer) to a queue served before anything read later, though a read already waiting on the terminal can overtake it — tests that abandon a read must release and drain it (`withBlockingStdIn` in `ConsoleTests.fs`). `StdinReader.readKey` is a test seam (`withTypedKeys`), since `Console.ReadKey` cannot be driven without a terminal. `readKey` fails through `onError` when stdin is redirected, and `readLine` at end of input (`EndOfStreamException`); `tryReadLine` yields `None` there instead.

Runtime (`src/FIO/Runtime/`):
- `Runtime.fs` - `FIORuntime` (abstract base, `IDisposable`: `Shutdown timeout` and `Dispose` interrupt the live roots and daemons it tracks, wait for them to unwind, then stop the workers through the per-runtime `StopWorkers` hook), `WorkerConfig`, `ContStackPool`, `LockedWorkDeque`
- `WorkerInfrastructure.fs` - `FIOWorkerRuntime` (adds EvaluationWorkers/EvaluationSteps params), `WorkerLifecycle`
- `InterpreterCore.fs` - Shared interpreter logic (`InterpreterState` struct, `processOutcome`/`processResult`/`handleSharedCase`, `Outcome` DU, `RuntimeCase` DU for runtime-specific dispatch, and the shared park/resume helpers)
- `DirectRuntime.fs` - .NET Tasks, waits for blocked fibers
- `WorkStealingRuntime.fs` - Custom fibers, work-stealing scheduler (per-worker `runNext` slot + work-stealing deque + shared global queue; at-most-one-waker async parking). The default runtime
- `DefaultRuntime.fs` - Type alias: `DefaultRuntime = WorkStealingRuntime`

Framework (`src/FIO/App.fs`):
- `App.fs` - `FIOApp<'A,'E>` abstract base class. 7 overridable members (beside `Run`, `RunAsync`, `Stop()` and `IsRunning`): `effect`, `runtime`, `onOutcome`, `onOutcomeTimeout`, `onShutdown`, `onShutdownTimeout`, `mapExitCode` over `AppResult<'A,'E>` (`AppSucceeded`/`AppFailed`/`AppInterrupted`/`AppFatalError`). The effect runs as a child of a root fiber that awaits it (`scoped`): an interrupted fiber publishes before its finalizers run, but the root *completes*, and completion waits for the child to unwind — that is what guarantees every finalizer has run before `onOutcome`/`onShutdown` and before the runtime is disposed. `Stop()` and the signal handlers interrupt the child; the handlers are registered before the runtime is created and the effect starts, and a request that races startup is applied when the child is forked. `onOutcomeTimeout`/`onShutdownTimeout` outside 0..`Int32.MaxValue` ms (or `Timeout.InfiniteTimeSpan`) make `Run` throw before the effect starts — the rule `FIORuntime.Shutdown` uses, shared via `validateTimeout` in `DSL/Utilities.fs`. An interrupted child maps by cause: `ExplicitInterrupt`/`ParentInterrupted` → `AppInterrupted` (130); `Defect` → `AppFatalError` with the thrown exception, `InvalidArgument`/`ResourceExhaustion` → `AppFatalError` (1, as in ZIO — the same code as `AppFailed`).

Extension libs expose `[<RequireQualifiedAccess>]` modules named after their domain (e.g. `SocketClient.connect`, `ServerSocket.serve`, `WebSocketClient.connectDefault`, `Routes`, `Codec`). The type-extension modules (`SocketExtensions`, `WebSocketExtensions`) and `SimpleRoutes` are **opt-in**, as are the Http operator and builder modules (`RoutesOperators`, `MiddlewareOperators`, `HttpHandlerOperators`, `RouteBuilder`, `TypedRoutes`, `ServerBuilder`) — nothing in the extension libraries is `[<AutoOpen>]`: open them (extension members need it) or qualify them.

Extension design decisions that are easy to undo by accident (each README describes the user-facing side):
- **Sockets** (`Socket.fs`): each socket owns its receive buffer and at most one read, started with no token (it ends when the socket is closed), so a receive that times out or is interrupted leaves the bytes for the next one. Receives take turns (a `SemaphoreSlim`) rather than failing a second receive: `ReceiveLine(n).Timeout(...)` polling starts the next receive while the timed-out one is still unwinding. That polling can still drop a line that arrives as the timer fires: the race is decided before the interrupt reaches the receive, which takes the line and is discarded. `SocketConfig.withReceiveTimeout` decides inside the receive and is the loss-free way to poll; the README says so. Only rejected input (a line or frame over its limit, a negative frame length) poisons the receive side.
- **WebSockets** (`WebSocketServer.fs`): `shutdown` sends 1001 to every connection, then at `ShutdownTimeout` interrupts the unfinished handlers *before* ending their connections with `Kill` (which disposes the .NET socket and so ends I/O pending without a token), so a routine shutdown logs no handler errors. `close` is `Stop()` then `Abort()` (Abort releases what Stop keeps, such as http.sys's session on Windows, while `Close()` after `Stop()` re-binds the port on macOS/Linux), and it is idempotent; `acceptLoop` only stops a listener it was given, and `serve`, which owns its listener, closes it. `accept` waits uninterruptibly: the fiber's token stops the listener, which ends the wait, so a request that has arrived is always upgraded or answered. After stopping the listener, the shutdown gives its refusal fiber one second and then interrupts it. The refusal is a protected child that nothing else would interrupt, and a managed `HttpListener.Stop()` that throws midway strands the request it awaits, which hangs the shutdown (about 1 run in 650, so only a stress loop shows it).
- **Http** (`Server.fs`, `KestrelBridge.fs`): the embedded host gets a no-op `IHostLifetime` (`QuietLifetime`) and no configuration sources, because the default `ConsoleLifetime` cancels every Ctrl+C and SIGTERM (which defeats `FIOApp`'s second-signal escape) and `appsettings.json` could add listeners. `Server.run` (behind `runServer`) handles only the first SIGINT/SIGTERM/SIGQUIT. `stop` refuses new requests with 503 through the `RequestTracker`, drains for `ShutdownTimeout`, then cuts the rest (interrupt, 503); `start` and `stop` are serialised by a gate, and a stopped server cannot start again.

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
└── FIOWorkerRuntime (abstract, adds WorkerConfig: EvaluationWorkers/EvaluationSteps)
    └── WorkStealingRuntime (= DefaultRuntime)
```

- **DirectRuntime** - .NET Tasks, waits for blocked fibers. The reference implementation and benchmark baseline
- **WorkStealingRuntime** - Custom fibers, **work-stealing** scheduler: per-worker local queues (a `runNext` slot + a work-stealing deque) and a shared global queue, with work-stealing across idle workers, at-most-one-waker wakeups, and async parking.

**DefaultRuntime = WorkStealingRuntime** (recommended)

Worker config fields: **EvaluationWorkers** (worker count), **EvaluationSteps** (interpreter steps per work item before a fiber yields). The `EWC`/`EWS` acronyms are the `ConfigString` display labels and the benchmark spec shorthand (`WorkStealing-{EWC}-{EWS}`).

### Key Internal Types

- **Fiber<'A,'E>** - Green thread, returns `FiberResult<'A, 'E>` (Succeeded/Failed/Interrupted)
- **FiberContext** - Internal execution state: completion, interruption, cancellation token, blocking work item queue (parked joiners) and the reschedule sink (`RescheduleOnEnd`) a joiner leaves behind, through which both `Interrupt` and a publish that happens later on another thread resume the joiners. Disposing a fiber's handle only records the request while it runs (`disposed` 0 → 2); the token sources are released once it has unwound (2 → 1), so a running fiber is never disturbed. It also holds two one-slot hooks, each fired once: `SetOnTerminal`, used by `JoinAllLatch`, and `SetOnUnwound`, used by the runtime registry (see **Disposal interrupts, then waits**). `SetOnTerminal` has **one slot**, and the latch parks are safe only because nothing parks on the same fibers twice. A new park that can be woken mid-install, on fibers that may be parked on again, must keep its waiter un-claimable until every hook is written (or a stale wake can overwrite a freshly installed hook), and must not touch the parent's context after making it claimable, since a woken parent may already have finished and released its token source
- **Channel<'A>** - Type-safe channel backed by an internal `MailboxQueue<obj>` (wrapper over `System.Threading.Channels`; messages are boxed) with blocking work item rescheduling. `Bounded` and `Dropping` use a bounded .NET channel in `Wait` mode, `Sliding` in `DropOldest`; only a full `Bounded` channel makes a writer wait (`parkUntilWritable` on WorkStealing, `WriteAsync` with the fiber's token on Direct), and an interrupted writer never writes unless it waits uninterruptibly. `TryWrite` never waits: a full `Bounded` or `Dropping` channel yields `false`
- **WorkItem** - Mutable work unit: effect + fiber context + continuation stack + interruption suppression level
- **Cont** - Continuation types: `ChainCont`/`FinalizerCont`/`PostFinalizerCont`/`RestoreSuppressionCont`/`AcquiredCont`. `ChainCont` holds a success and a failure handler, either of which may be null (a `FlatMap` has no failure handler, a `CatchAll` no success handler); a null side passes the outcome on. It is one frame, popped before either handler runs, so a failure of the success side is not handed to the error side (ZIO's `foldZIO`). One kind instead of three halves the exception-handling regions in each inlined `processOutcome`, which made the interpreter microbenchmarks 3–7% faster. Code added to the inlined `processOutcome` has a measurable cost, since it is expanded at every call site of each runtime's loop — and a hard limit: the JIT compiles a method with more than 2,000 basic blocks without optimization (MinOpts), silently. Loops that crossed it ran 5–37% slower on channel workloads, so only the hot `ChainCont` arm is inline; the finalizer and suppression arms live in the `NoInlining` helpers `processRegionCont` and `unwindFinalizers`. `tests/FIO.Tests/Runtime/InterpreterSizeTests.fs` fails when any runtime's loop exceeds ~1,800 estimated blocks (that check runs under `dotnet test -c Release` only — `test.yml`'s `release` job and `publish.yml` — since Debug builds have no state machine to measure), and `DOTNET_JitDisasmSummary=1` shows each loop's tier. `FinalizerCont` ensures finalizers run on interruption, not just success/error; `PostFinalizerCont` restores the saved suppression level after a finalizer completes, and the saved outcome unless the finalizer's is worse (a failure replaces a success, never an earlier failure or interruption). `RestoreSuppressionCont` sets `InterruptionSuppressed` back to an absolute level on any outcome. `AcquiredCont` does the same when an acquire ends and, on success, pushes release's `FinalizerCont` (via `unwindFinalizers`) before the region-exit check, so no step separates acquiring from registering release. `Cont` is copied on every push and pop, so a new case reuses existing fields by name and type (F# shares their storage) and `InterpreterSizeTests` fails if it grows past 56 bytes.
- **Continuation stack** (a `Stack<Cont>`: on the `WorkItem` in WorkStealing, in `InterpreterState` in Direct) / **ContStackPool** - thread-static pool that `DirectRuntime` rents from and returns to
- **Per-worker pools** - `WorkStealingRuntime.Run` allocates a fiber's first `WorkItem` and continuation stack; after that
  WorkStealing recycles both in its own per-worker pools on the `Scheduler`. Collapsing them onto thread-static pools
  cost 18–33% WorkStealing wall time on park-heavy workloads while staying alloc-neutral — the per-worker pools are a
  deliberate, measured fence
- **InterruptionCause** - `ParentInterrupted` | `ExplicitInterrupt` | `InvalidArgument` | `ResourceExhaustion` | `Defect` (user code threw where no typed error could be produced)

### Concurrency Primitives

Concurrency is built on the core types: **Fiber<'A,'E>** (green threads via `.Fork()`/`.Join()`), **Channel<'A>** (typed message passing) and **Ref<'A>** (atomic reference cell, `src/FIO/DSL/Ref.fs`). There are no Promise/Semaphore primitives yet; `Console` is the library module.

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
| `<?>` | Par | Choice | Race for first success (RaceEither) |

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
// Run on a runtime directly (a runtime is IDisposable: disposing it interrupts what is still running)
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
matchers (`RoutePattern.fs`, `Routes.fs`), and both `InterpretAsync` members, where it stops the
interpreter from calling itself. It can go on module-level and class-level `let` functions and on members,
but not on a local `let rec` (FS0824). FIO effect loops (`FlatMap`, `fio { return! loop () }`) never get
it: the interpreter runs each iteration from its own loop, so they cannot overflow the stack, and the
check, which cannot tell that `FlatMap` runs its lambda later, wrongly rejects some of them. For them,
keep `return! loop ()` last so no continuation piles up. The attribute changes no IL and costs nothing at
runtime.

## Benchmarks

Macro benchmarks live in `benchmarks/FIO.Benchmarks/` (BenchmarkDotNet 0.15.8); `benchmarks/FIO.Benchmarks/README.md`
is the source of truth (parameters and defaults, tuning, result interpretation, the A/B protocol). Twelve workloads:
**Bang, Big, BoundedBuffer, Chameneos, Counting, Fibonacci, Fork, Philosophers, Pingpong, Threadring, Trapezoidal, ZipRace**. Eleven are classic concurrency workloads; **ZipRace** is a combinator microbenchmark that regression-guards the parallel-combinator primitives. Every run reports **execution time and allocated memory**.

- **Layout:** one file per benchmark in `Benchmarks/`: its workload module and a class that carries `[<MemoryDiagnoser>]` + `[<RankColumn>]` (on the class, not the base), inherits `FIOBenchmark` (the shared runtime parameter, setup, cleanup and measured `Run`) and builds the effect. The `FIO.Benchmarks.Benchmarks` namespace, the class name, the `Run` method name, its parameter names and the `Runtime` parameter name are the report and series identity — `compare.py` and the gh-pages tracker key off them — so do not rename them.
- **Runtimes:** spec format `Direct | WorkStealing-{EWC}-{EWS}`, via `FIO_BENCH_RUNTIMES` (default `Direct,WorkStealing-12-200`). `compare.py`/`plot.py` (sharing `bench_common.py`) still parse the removed runtimes (Polling, Signaling) and the old three-leg labels, so stored results stay readable; `compare.py` pairs rows on the exact label, so a three-leg `WorkStealing-12-200-1` row never pairs with a two-leg one (it is listed under **Unmatched rows**).
- **Iteration control:** `FIO_BENCH_WARMUP` (default 3), `FIO_BENCH_ITERATIONS` (default 30); a CLI `--job`/`-j` (e.g. `--job Dry`) **overrides** them. Per-benchmark params: `FIO_BENCH_<NAME>_<PARAM>`.
- **A/B comparison:** allocations are comparable across sessions; **wall time drifts 20–50% between sessions** on dev machines — compare times only from same-session adjacent A/B runs, bracketed by sentinel re-runs.

## Testing

- **Expecto + FsCheck** for property-based testing. `dotnet test` runs through YoloDev.Expecto.TestSdk, which never calls `Program.main`, so the runner config there (`Parallel`, `Summary`, `Colours 256`) applies only to `dotnet exec FIO.Tests.dll` (with `--child`, `main` runs a child-process scenario instead). Pinned versions (`Directory.Packages.props`): Expecto 11.1.1, FsCheck 3.4.0.
- `tests/FIO.Tests/Utils/Utilities.fs` is the single source of runtime test helpers: `allRuntimes()`, `testAllRuntimes`, `testAllRuntimesSequenced` (for `System.Console`'s process-global state), `stressTestAllRuntimes` (`testAllRuntimes` behind the `FIO_RUN_STRESS` gate), and the FsCheck `Generators` `Arb` (one lazily built runtime pair per assembly, since FsCheck asks for it once per property and never disposes it). All of them cover **both** runtimes — `DirectRuntime`, `WorkStealingRuntime`. Do not redefine these per test file: a helper named "all runtimes" that quietly omits one is how a runtime-specific defect survives a green suite
- They share one `testConfig` (`EvaluationWorkers = 2`), not `WorkerConfig.Default`, and so do the extension suites' `Utils/Utilities.fs`. `allRuntimes()` builds both runtimes for every test list and Expecto runs lists in parallel, so the default (`ProcessorCount - 1`) would keep thousands of workers alive at once and make wall-clock deadline assertions flake under load. `testAllRuntimes` disposes each runtime after its test, which interrupts anything the test left running. Stress tests mostly use `testConfig` too (through `stressTestAllRuntimes`, or a `WorkStealingRuntime(testConfig)` they build per test); a test that needs a particular worker count builds its runtime with an explicit config, and the bounded-buffer channel stress test uses `WorkerConfig.Default` for real parallelism
- `tests/FIO.Tests/Runtime/ConformanceTests.fs` asserts the two runtimes are observationally equivalent — defect paths, typed-error integrity, `Await`/`UnsafeResult` agreement, runtime disposal and the registry's release of finished fibers (`TrackedCount`), joins resuming at a target's interruption. It deliberately uses `'E = string`, because `Fiber.Task()` casts the error channel with `error :?> 'E`: a non-`'E` value there raises `InvalidCastException`, which `'E = exn` silently absorbs
- Heavy stress/regression tests (deadlock & lost-wakeup guards) are **opt-in** via the `FIO_RUN_STRESS=1` env var (`stressEnabled`/`stressTestCase` in `tests/FIO.Tests/Utils/Utilities.fs`) — off by default locally, enabled in CI
- Behaviour that must run in its own process — `FIOApp` signal handling, or anything that could crash the test host — uses the shared wrapper `tests/Shared/ChildProcessRunner.fs` (linked into `FIO.Tests` and `FIO.Http.Tests` as `Utils/ChildProcessRunner.fs`): `new ChildProcess([ scenario; args... ])` re-runs the test assembly as `dotnet exec <Tests>.dll --child <scenario> [args]`, with `WaitForLine`, `Signal` and `WaitForExit`. Each project's own `Utils/ChildProcess.fs` keeps just its scenarios and the `--child` dispatch (`run`, called from `Program.fs`). Signal tests skip on Windows
- Console tests use `System.Console.SetOut`/`SetIn` with `StringWriter`/`StringReader` for deterministic capture — must use `testSequenced` (not parallel) because `System.Console` has process-global state
- Four test projects:
  - `FIO.Tests` — core library, organized into `DSL/`, `Lib/`, `Framework/`, `Runtime/` subfolders; the factory functions and extension methods are split by sub-group into `DSL/Factories/*.fs` and `DSL/Extensions/*.fs`, each file a `[<Tests>]` list under the same top label ("Factory Functions" / "Extension Methods"). Test names follow `Subject - sentence` (the member under test, then the behaviour) in all four projects
  - `FIO.Sockets.Tests` — TCP sockets, flat structure with `testAllRuntimes` + `withTestServer`/`withTestEchoServer` helpers
  - `FIO.WebSockets.Tests` — WebSockets, flat structure
  - `FIO.Http.Tests` — HTTP server tests
- **Test layout is Arrange / Act / Assert, shown by blank lines, never by comments.** A test body is up to three blocks: Arrange (inputs, effects, runtimes, servers), Act (the one call under test, bound with `let`, e.g. `let result = runtime.Run(effect).UnsafeResult()`), and Assert (`Expect…`, or a `match` on the bound result ending in `failtest`). Exactly one blank line separates blocks and none appears inside a block — a long Arrange is a cue to extract a helper. A block may be absent (a pure function has no Arrange), but Act and Assert are always separated, even in a two-line test
  - `Expect.throwsT`/`Expect.throws` (`fun () -> act`) are the only forms that merge Act into Assert, since the throw is the act; otherwise `Run` is never passed straight into `Expect` or `match`
  - Assertions run outside the effect: an effect run inside `withTestServer` and similar wrappers returns what it observed, and the test asserts on the returned value after the wrapper returns
  - A test whose point is a sequence (lifecycle, interruptibility, resource tests) may repeat Act/Assert pairs after one Arrange
  - Property tests follow the same layout; their generated arguments count as Arrange
- Core tests use `Generators` type for FsCheck Arb across both runtimes; the Sockets and WebSockets suites have their own FsCheck generators, while `FIO.Http.Tests` uses none (and references no FsCheck packages); every extension suite has a `testAllRuntimes` helper. The Sockets and Http helpers wrap `testSequenced`; the WebSockets suite runs in parallel, so a test there that touches process-global state (`Console.SetError`, `Console.SetIn`, …) must be wrapped in `testSequenced` explicitly
- `InternalsVisibleTo` exposes the core project to `FIO.Tests`, `FIO.Sockets` to `FIO.Sockets.Tests` (a socket's OS options, via the internal `Socket.NetSocket`) and `FIO.Http` to `FIO.Http.Tests` (a server's own runtime, via `Server.runtimeOf`); `FIO.WebSockets` exposes nothing. The three `InternalsVisibleTo` items are conditional on `StripInternals != true`, which `publish.yml` and `scripts/pack.sh` set when packing, so published assemblies grant no friend access
- `tests/FIO.Tests/Runtime/InterpreterSizeTests.fs` guards the JIT's optimization limit for each runtime's interpreter loop (see **Cont** above). Its two loop-size tests skip in Debug (its `Cont - stays 56 bytes` test runs in every configuration), so the loop guard is enforced by `dotnet test -c Release` — `test.yml`'s `release` job and `publish.yml` run it
- Stack-safety canaries live in `tests/FIO.Tests/DSL/FIOTests.fs` — the four "Stack safety - deep left-chained FlatMap/CatchAll/Ensuring/MapBoth" tests at depth 10000 are load-bearing for the iterative-flattening design of `UpcastResult`/`UpcastError`/`UpcastBoth`. Do not "simplify" those methods to plain recursion.

## Semantic Invariants (Do Not Break)

- **Effect laziness**: constructing an effect must NOT execute side effects
- **Sequential ordering**: `>>=`, `<*>`, `*>`, `<*` preserve left-to-right semantics
- **Parallel operators**: `<&>`, `&>`, `<&`, `<&&>` must be genuinely concurrent on both runtimes
- **Interruption semantics**: interruption must propagate through fibers consistently across all runtimes
- **Fail-fast parallelism**: the parallel combinators (`ZipPar`/`Race` family, `forEachPar`) settle on the first relevant completion and interrupt losers/peers — they must never hang on a stuck sibling (except the first-to-succeed waits of `Race`, `RaceEither`, `ZipParError` and `FIO.raceAll`: when the first to settle fails they wait for the others, as a `Race` test pins)
- **Finalizer guarantee**: `Ensuring` finalizers run on all three outcomes — success, error, and
  interruption. There is no fourth outcome: short of a `Shutdown`/`Dispose` whose timeout expires (see **Disposal interrupts, then waits**), a fiber may never be discarded without unwinding. In
  particular a runtime must not drop queued work items to "reset" itself
- **Fork is scope-attached** (ZIO's `fork`, not its `forkDaemon`): when a fiber finishes it interrupts
  every fiber it forked, and **does not publish its own result until they have unwound** — so once you
  observe a completed fiber's result, every finalizer in its subtree has already run (an interrupted fiber
  publishes at once: see **Interrupted fibers publish immediately**). `ForkDaemon` opts out: a daemon fiber
  is neither interrupted nor awaited by its parent (nothing interrupts or awaits it automatically except the
  runtime's `Shutdown`/`Dispose`), and its lifetime becomes the caller's to manage.
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
- **Suppression means uncancellable.** While `InterruptionSuppressed > 0` (an `Ensuring` finalizer, an `uninterruptible`/`uninterruptibleMask` region outside `restore`, or `acquireReleaseWith`'s acquire and release),
  the fiber is uninterruptible, so `FIO.cancellationToken()` yields `CancellationToken.None` and
  `awaitedTask` skips `WaitAsync`. Both follow the same rule, and it is what lets a finalizer `sleep`,
  `async` or `awaitAsync` after its fiber was interrupted — handing out the already-cancelled token
  made `Task.Delay` fault instantly and `Register` fire immediately, truncating cleanup
- **Suppression levels are absolute.** Every change of `InterruptionSuppressed` is undone by restoring a saved level, never by counting: `WithSuppression(update, body)` captures the level on entry, sets it to `update outer`, runs `body outer`, and pushes `RestoreSuppressionCont` with the captured level (a mask raises the level by one, `restore.Restore` sets the level its mask captured). So a `restore` from a mask opened inside a finalizer stays uninterruptible — its captured level is already above 0 — but a restorer captured outside (at level 0) and used in a finalizer makes that part interruptible, as in ZIO: use a restorer only in its own mask's body. `acquireReleaseWith` must keep acquire suppressed until release is registered: an acquire that takes a step after creating its resource, or resumes from a task, leaked the resource on interrupt when it ran interruptibly
- **An interruption takes effect the moment a region ends.** When a `RestoreSuppressionCont` or `PostFinalizerCont` brings the level back to 0 while the fiber's token is cancelled, `processOutcome` turns the outcome into an interruption before popping further, so no continuation code after an uninterruptible region or a finalizer runs once the fiber is interrupted (ZIO's rule). Finalizers further out still run
- **Disposal interrupts, then waits.** The registry (`Runtime.fs`) holds every root (from `Run`) and daemon (from `ForkDaemon`) not yet unwound, in 16 lock-striped reference sets; `Watch` installs one shared `SetOnUnwound` hook that removes a fiber the moment it unwinds, so nothing keeps a finished fiber or its result reachable (a per-thread design that pruned lazily once leaked them — the review's blocker). `SetOnUnwound` fires once the result is set (`published` 0 → 1 claimed → 2 set; a `Publish` that lost the state to an `Interrupt` spins until that interrupt has set its result). `Dispose`/`Shutdown` interrupt every tracked fiber, set `draining` and wait until the stripes are empty, and only then stop the workers; scoped children are covered by their roots. The wait is bounded by the timeout: a tracked fiber that only waits for its children then publishes the value it has (`Abandon`), and fibers still unwinding are abandoned with the workers, their remaining finalizers unrun: `StopWorkers` first sets a `stopping` flag that the step-budget check reads, so a fiber in the middle of its steps leaves its worker at its next yield point (on `DirectRuntime`, which has no workers, fibers keep unwinding after `Shutdown` returns). Only roots and daemons are tracked, so a fiber deeper in the tree that only waits for its children gets no result. A concurrent or later `Shutdown` waits for the first. `Watch` adds a fiber before reading the disposed flag, so a `Run` racing disposal is either seen by `Shutdown` or interrupts itself; `Run` after disposal throws `ObjectDisposedException`, and a daemon forked after it is interrupted at once
- **Interrupted fibers publish immediately.** A fiber that is *interrupted* surfaces its result at once
  while its subtree unwinds behind it; only a fiber that *completes* holds its result back. Children are
  interrupted on both paths (protected ones only once the fiber has unwound), so no finalizer is
  skipped either way — only the ordering differs. Joiners (`Join`, `JoinFirst`) parked on an interrupted
  fiber resume at the interruption on every runtime, not after its finalizers
- **Error typing**: extensions must not leak raw exceptions as public errors (FIO.Http's server pipeline is
  typed `exn` by design; Sockets and WebSockets surface `SocketError`/`WsError`). Nothing may place a
  non-`'E` value in the error channel: when user code throws where no `'E` can be produced (a `Suspend`
  thunk, a `FlatMap`/`CatchAll` continuation, a throwing `onError`), the fiber dies with
  `InterruptionCause.Defect`, and joining an interrupted fiber propagates interruption rather than a
  typed failure

## Architecture Change Checklist

- **New effect constructor**: update `Core.fs` (FIO DU + `UpcastResult`/`UpcastError`/`UpcastBoth`), `Factories.fs`, `Extensions.fs`, `Operators.fs`, and `CE.fs` as needed
- **New shared effect case**: add handling to `handleSharedCase` in `InterpreterCore.fs`
- **New runtime-specific effect case**: add to `RuntimeCase` DU in `InterpreterCore.fs`, route from `handleSharedCase`, then handle it in each runtime's `RuntimeCase` match. Before writing it out per runtime, check whether it fits an existing shared helper — `awaitedTask` (how to wait, given interruption suppression), `settledTaskEffect` (settled task → resume effect), `parkOnTask` (park on an unfinished task; the reschedule seam is the only per-runtime part), `resumeWith` (carry the suppression level onto a rented work item). Only the scheduling seam should differ between runtimes
- **Runtime change**: update interpreter logic, add tests, and validate benchmarks. A runtime has one
  entry point, `Run`: schedule the effect on a new fiber and return. It must never wait for, interrupt,
  or discard fibers already running — clearing scheduler queues destroys in-flight work *without*
  running finalizers, which `tests/FIO.Tests/Runtime/ConformanceTests.fs` guards against. Only
  `Shutdown`/`Dispose` interrupt running fibers, and they wait up to their timeout for them to unwind rather than discard them
- **Extension change**: update error model, DSL surface, and extension README
- **Behavior change**: update examples and tests to match new semantics
- **New public API**: add a concise XML doc comment per [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md)

## CI

Three GitHub Actions workflows in `.github/workflows/`:

- **`test.yml` (Run Tests)** — push/PR on **`main`** + manual. Matrix: Ubuntu, Windows, macOS (`fail-fast: false`), one identical test step per OS. Sets `FIO_RUN_STRESS=1` to enable the opt-in stress/regression tests, bounded by `timeout-minutes: 30` because a deadlock is a plausible failure mode here. Uploads a TRX per test project per OS `if: always()`; `--blame-hang-timeout 5m` turns a hang into a failed test with a name — a CI-only flake cannot be re-run with better capture. The ruleset requires the `build (<os>)` checks, so keep the job and matrix names. A second job, `release`, builds and tests in **Release** with stress on Ubuntu and macOS (x64 and ARM64): Release is what ships, F# compiles `task { }` into state machines only when optimizing, and it is where `InterpreterSizeTests` runs. **No coverage in CI** (instrumentation slows every run, and nothing consumed the reports); coverage is a local `dotnet test --collect:"XPlat Code Coverage"` measurement.
- **`benchmark.yml` (Performance Benchmarks)** — push/PR to **main** (skipping docs-only changes) + manual, Ubuntu only. First a **smoke test** (all benchmarks × both runtimes, tiny params, `--job Dry`) to fail fast on hang/throw; then a measured **Pingpong** run. That job runs the repository's code with a read-only token and no persisted credentials; a separate `track` job, which runs none, gets the write scopes and publishes to <https://fs-fio.github.io/fio/dev/bench/> (`gh-pages`, on `main` only). That dashboard is a **tracker, not a gate** — shared-runner variance swamps any useful threshold, so `fail-on-alert` is off and the real perf gate is the local sentinel-bracketed A/B protocol. The series names embed the runtime spec (`Pingpong - WorkStealing-2-200`), so changing a spec starts a new series and orphans the history.
- **`publish.yml` (Publish NuGet Packages)** — on tags. `v*` = **lockstep** (all four packages; tag must equal `Directory.Build.props` `<Version>`); `core-v*` packs the core with `-p:Version` (and must equal the baseline too); `http-v*`/`sockets-v*`/`websockets-v*` = **per-package** release (sets `PackageReleaseVersion`; the FIO dependency stays the baseline as a minimum, `>=`, not a pin). A pushed tag matching none of these starts no run; a manual run on such a tag fails. The version is resolved and checked before the build; then it builds Release, runs tests, packs with `-p:StripInternals=true` (the packed assemblies carry no `InternalsVisibleTo`), pushes to NuGet.org (key through `env:`), and creates a GitHub release, marked prerelease for a version with a suffix. Push and release run only for a **pushed tag** (`github.event_name == 'push'`), so every `workflow_dispatch` run — on a branch or on a tag — is a dry run that uploads the packages as an artifact. All actions are pinned to commit SHAs (Dependabot keeps them current).

## Commit Style

Short, sentence-style messages (e.g., "Fix benchmark output", "Improve App.fs"). No strict prefixes; keep messages descriptive.

## Comment Style

Two tiers — see [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md) for the full guide and per-construct examples:

- **Public, user-facing API** (callable from a referencing package): a concise, ZIO-style XML doc comment (`///`). One verb-first summary line by default; "this effect" voice; describe behavior, not the signature. **Never** document internal/private items, the `FIO` DU cases, runtime internals, or the `FIOBuilder` CE methods.
- **Internals**: comment-free. Use an inline `//` only when the *why* isn't obvious from the code. Never restate what the code does. No commented-out code (use git history).

Keep doc comments well-formed XML — rephrase types out of prose ("an effect") or, in a comment that opens with a tag, escape them (`FIO&lt;'A,'E&gt;`) — since `WarnOn 3390` fails the build on malformed doc XML. A bare `///` comment is XML-escaped by the compiler, so markup in it (`<c>…</c>`, entities) ships as literal text: a comment with markup opens with `<summary>` (`/// <summary>… <c>FlatMap</c>.</summary>`).

## Formatting & Tooling

- **`.editorconfig`** governs formatting: UTF-8, LF line endings, final newline, trim trailing whitespace (except `*.md`). 4-space indent by default (F#, project files and the rest); 2-space for JSON/YAML.
- **`TreatWarningsAsErrors=true`** — set once in `Directory.Build.props`, applies to every project. Fix all warnings. Use `TreatWarningsAsErrors`, **not** `WarningsAsErrors` (the F# SDK reads the latter as a warning-number list).
- **XML docs:** packable libraries set `GenerateDocumentationFile=true` and `WarnOn 3390` (see **Comment Style**).
- **Central Package Management:** all package versions are pinned in `Directory.Packages.props` (e.g. BenchmarkDotNet 0.15.8). FSharp.Core's implicit reference is disabled in favor of an explicit, version-less `PackageReference` so the central version wins. **FSharp.Core 10.1.401 is the consumers' floor**, packed as `>= 10.1.401`: an F# consumer on an older FSharp.Core gets only a warning and a crash at run time, so it is raised only when the code needs a newer API, and Dependabot ignores it.
- **Versioning:** the baseline `<Version>` lives once in `Directory.Build.props`; the publish workflow requires a `v*`/`core-v*` tag to equal it, and an extension tag sets that package's version through `PackageReleaseVersion`. Only the four `src/` libraries are packable (`IsPackable`); tests/benchmarks/examples are not.
- F# `ParallelCompilation` and `Deterministic` use SDK defaults (no project-level overrides). F# compile order matters — file order in `.fsproj` is the compilation order.
- Match existing style in `src/` and `tests/`.
