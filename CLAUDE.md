# CLAUDE.md

Guidance for Claude Code in this repository. Personal, machine-specific notes live in the untracked `CLAUDE.local.md`.

## Project

FIO is a type-safe, purely functional effect system for F#: an IO monad plus fibers. <https://github.com/fs-fio/fio>, MIT.

- .NET 11 / F# 11. `TargetFramework` is set once in `Directory.Build.props`; solution `FIO.slnx`; SDK pinned in `global.json`.
- Version `0.7.0-beta`, set once in `Directory.Build.props`; a release tag `v<Version>` must equal it.
- Runtime async: .NET 11 needs nothing from FIO on the runtime side; FIO's `task { }` stays a state machine until FSharp.Core ships a runtime-async `task` (dotnet/fsharp#20918). No hand-written builder, no `LangVersion preview`.

## Commands

```bash
dotnet build                                    # build everything
dotnet test                                     # all suites; add -c Release to run the JIT-limit guard
dotnet test tests/FIO.Tests/                    # one suite (also FIO.Sockets.Tests, FIO.WebSockets.Tests, FIO.Http.Tests)
dotnet test --filter "Name~TestName"            # one test or group
dotnet tool restore && dotnet fantomas .        # format; CI runs dotnet fantomas --check .
dotnet run --project examples/FIO.Examples.App  # also .DSL, .Http, .Sockets, .WebSockets
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Pingpong*"
python3 benchmarks/compare.py <dirA> <dirB>     # A/B diff; benchmarks/plot.py draws charts
dotnet pack -c Release -p:StripInternals=true -o artifacts  # pack the four packages, as publish.yml does
for i in $(seq 50); do FIO_RUN_STRESS=1 dotnet test -c Release --no-build || echo "run $i failed"; done  # soak, after dotnet build -c Release
```

The dotnet CLI is the build interface: there is deliberately no Makefile or scripts folder.

## Packages

| Folder | NuGet ID | Errors |
|---|---|---|
| `src/FIO` | `FSharp.FIO` | user-chosen `'E` |
| `src/FIO.Sockets` | `FSharp.FIO.Sockets` | `SocketError` |
| `src/FIO.WebSockets` | `FSharp.FIO.WebSockets` | `WsError` |
| `src/FIO.Http` | `FSharp.FIO.Http` (Kestrel) | `exn`; `HttpError` inside handlers |

Each extension's README describes its user-facing API. Extension modules are `[<RequireQualifiedAccess>]` and nothing in them is `[<AutoOpen>]`: `SocketExtensions`, `WebSocketExtensions`, `SimpleRoutes` and the Http operator/builder modules are opt-in.

## Key files (`.fsproj` order is compile order)

- `src/FIO/DSL/`: `Utilities.fs` (boxing/atomic helpers, `validateTimeout`), `Exceptions.fs` (`InterruptionCause`), `Core.fs` (`FIOBase`, `FIO<'A,'E>`, node classes, `Fiber`, `Channel`, `FiberContext`, `WorkItem`, `Cont`, `JoinAllLatch`, the primitive members), `Factories.fs`, `Primitives.fs` (`Ref`, `Promise`, `Semaphore`), `Extensions.fs` (combinators built on Core), `Operators.fs`, `CE.fs`.
- `src/FIO/Console.fs`: console effects; `readLine`/`readKey` go through one background stdin reader, so a waiting fiber stays interruptible.
- `src/FIO/Runtime/`: `Runtime.fs` (`FIORuntime`, the registry, `Shutdown`), `InterpreterCore.fs` (`processOutcome`, `handleSharedCase`, `RuntimeCase`, `attachFork`), `DirectRuntime.fs`, `WorkStealingRuntime.fs`, `DefaultRuntime.fs` (`DefaultRuntime = WorkStealingRuntime`).
- `src/FIO/App.fs`: `FIOApp<'A,'E>` runs the effect as a child of a root fiber, so every finalizer has run before `onOutcome`/`onShutdown`; exit codes 0 / 1 / 130.

## Architecture rules

- Effects are internal sealed node classes under the public `FIOBase`/`FIO<'A,'E>` (internal constructors, `[<NoEquality; NoComparison>]`). A node holds a `NodeKind`, slots `A` and `B`, and reaches user code only through its `NodeOps`, a lazy singleton per instantiation; nothing in `src` may compare ops by identity.
- A null child, finalizer or acquire throws where the node is built. A continuation, thunk or body that throws or returns null becomes a `Defect` when it runs.
- Slot reads are unchecked casts in Release and checked in Debug or with `-p:FioCheckedCasts=true`; the "Nodes - every node kind holds its payload in the slots the interpreter reads" test covers every kind.
- Runtimes: `DirectRuntime` (one .NET task per fiber, the reference) and `WorkStealingRuntime` (the default; its workers are async loops on the thread pool). `Run` is the only entry point and never waits for, interrupts or discards running fibers.
- Only the `ChainCont` arm is inline in `processOutcome`; the others live in the `NoInlining` helpers (`processRegionCont`, `unwindFinalizers`, `runExitFinalizer`). A loop over 2,000 basic blocks silently compiles with MinOpts: `InterpreterSizeTests` fails above ~1,800 (Release only); check tiers with `DOTNET_JitDisasmSummary=1`.
- `Cont` stays ≤ 56 bytes: a new case reuses existing fields by name and type.
- No non-generic statics (`static let`, `static member val`): they add a class-init check to every closure in the file. Cache in generic holders (`NullSuccess`, `CancellationTokenNode<'E>`, the ops singletons, `ForkScopeCallback<'Marker>`).
- The per-worker pools in WorkStealing are a measured fence: never move them to thread-static pools.
- `FiberContext.SetOnTerminal` has one slot. A new park on fibers that may be parked on again keeps its waiter unclaimable until every hook is written, and never touches the parent after making it claimable.
- `Publish` runs every step even when one throws (reschedule waiters, tell the parent, set the result, release tokens, fire the unwound hook), then rethrows the first exception; `OnChildUnwound`'s stack-depth fallback logs instead.
- A continuation frame keeps its node's subtree reachable until popped: bounded, never accumulating across a loop.

## Semantic invariants (do not break)

- Constructing an effect runs nothing.
- `>>=`, `<*>`, `*>`, `<*` run left to right; `<&>`, `&>`, `<&`, `<&&>` are genuinely concurrent on both runtimes.
- Interruption propagates the same way on every runtime.
- Parallel combinators (`ZipPar`/`Race` family, `forEachPar`, `forEachParN`) settle on the first relevant completion and interrupt the rest; only the first-to-succeed `Race`, `RaceEither`, `ZipParError` and `FIO.raceAll` wait for the others after a failure.
- `Ensuring`/`OnExit` finalizers run on success, error and interruption. A fiber is never discarded without unwinding, short of an expired `Shutdown` timeout; a runtime never drops queued work.
- A failing finalizer replaces a success with its failure, never an earlier failure or interruption.
- `FoldFIO`, `TapBoth` and the other fold nodes hand `onError` only this effect's failure: a failure raised by `onSuccess` propagates (ZIO's `foldZIO`).
- `Semaphore.WithPermit` returns a permit exactly when its `SemaphoreSlim` wait succeeded, decided by the wait, never by a flag or an outcome.
- `Fork` is scope-attached: a finishing fiber interrupts its children and publishes only after they unwind. `ForkDaemon` opts out, so its lifetime is the caller's.
- Children forked while `InterruptionSuppressed > 0` go to `protectedScope` and are interrupted only once the fiber has unwound.
- Scope completion never blocks a scheduler thread: it is the `completing`/`outstanding`/`published` latch settled by `TryFinish`.
- Suppression means uncancellable: `FIO.cancellationToken()` yields `CancellationToken.None` and awaits skip `WaitAsync`.
- Suppression levels are absolute (save and restore, never count); acquire stays suppressed until release is registered. A restorer belongs only in its own mask's body: used in a finalizer it makes the cleanup interruptible.
- When a region ends at level 0 with the token cancelled, the outcome becomes an interruption before any further continuation runs.
- Disposal interrupts, then waits: the registry tracks roots and daemons until they unwind (one `SetOnUnwound` hook; nothing finished stays reachable); `Shutdown` interrupts them, waits up to its timeout, then `Abandon`s the rest and stops the workers through the `stopping` flag.
- An interrupted fiber publishes at once while its subtree unwinds, and its joiners resume at the interruption; a completing fiber waits for its children.
- No non-`'E` value ever enters the error channel: user code that throws where no `'E` can be produced is a `Defect` interruption.

## Extension decisions (easy to undo by accident)

- **Sockets:** each socket owns its buffer and at most one read, started without a token; receives take turns on a `SemaphoreSlim`; only rejected input poisons the receive side; `withReceiveTimeout` is the loss-free poll.
- **WebSockets:** shutdown sends 1001, then at `ShutdownTimeout` interrupts handlers *before* ending connections with `Kill`; `close` is `Stop()` then `Abort()` and idempotent; `accept` waits uninterruptibly; the refusal fiber is cut after one second.
- **Http:** the host gets a no-op `QuietLifetime` (the default `ConsoleLifetime` swallows every Ctrl+C and defeats `FIOApp`'s second-signal escape) and no configuration sources (`appsettings.json` could add listeners); `runServer` handles only the first signal; `start`/`stop` share a gate, and a stopped server cannot restart.

## Change checklist

- **New effect constructor:** build it from existing nodes; a new node class goes in `Core.fs`, with an ops singleton overriding only what its kind uses.
- **New node kind:** add the `NodeKind` case, handle it in `handleSharedCase` (check `isFinalizerNode` and `processRegionCont` by hand), check its child in the constructor, add it to the slot test, measure.
- **New `Cont` case:** handle it in `processRegionCont`, reusing existing fields.
- **New runtime-specific case:** add it to `RuntimeCase`, route it from `handleSharedCase`, and reuse `awaitedTask`, `settledTaskEffect`, `parkOnTask` and `resumeWith`.
- **Runtime change:** tests and benchmarks; never clear queues; only `Shutdown`/`Dispose` interrupt.
- **Extension change:** error model, API, README. **Behavior change:** examples and tests. **New public API:** a doc comment per `docs/COMMENT_STYLE.md`.

## Testing

- Expecto + FsCheck. `dotnet test` runs through YoloDev.Expecto.TestSdk and never calls `Program.main`.
- Use the helpers in `tests/Shared/Utilities.fs` (`allRuntimes()`, `testAllRuntimes`, `testAllRuntimesSequenced`, `stressTestAllRuntimes`), linked into every test project, and `Generators` in `tests/Shared/FsCheckProperties.fs`; they cover both runtimes and use `testConfig` (2 workers). Never redefine them per file.
- Conformance tests use `'E = string`: `Fiber.Task()` casts the error, and `exn` would hide a wrong value.
- Stress tests are opt-in with `FIO_RUN_STRESS=1`; CI sets it.
- Behaviour that needs its own process (signals, possible crashes) uses `tests/Shared/ChildProcessRunner.fs` (`ChildProcess`); signal tests skip on Windows.
- Tests that touch `System.Console` run sequenced, and one that abandons a read releases and drains it (`withBlockingStdIn`).
- Layout: Arrange / Act / Assert separated by single blank lines, no comments. Act is one bound call; only `Expect.throws*` merges Act into Assert; asserts run outside the effect. Names are `Subject - sentence`.
- `InternalsVisibleTo` (core → `FIO.Tests`, Sockets → `FIO.Sockets.Tests`, Http → `FIO.Http.Tests`) is dropped when packing with `StripInternals=true`.
- The "Stack safety - deep left-chained …" tests (depth 10,000) guard that no node kind recurses when built or run.

## Benchmarks

- 12 workloads in `benchmarks/FIO.Benchmarks/Benchmarks/`, one file each: a class inheriting `FIOBenchmark` that carries `[<MemoryDiagnoser>]` and `[<RankColumn>]` itself. The namespace, class names, `Run`, its parameter names and `Runtime` are the series identity: never rename them.
- Runtime spec `Direct | WorkStealing-{EWC}-{EWS}` (worker count, steps per slice) via `FIO_BENCH_RUNTIMES`.
- Allocations compare across sessions; times only within one session, in adjacent A/B pairs bracketed by sentinels.
- Decision rule, WorkStealing first: a change may slow Direct or raise memory only if it makes WorkStealing confirmed faster with no WorkStealing case slower; a leak fails regardless. Report every Direct and memory cost.

## CI (`.github/workflows/`)

- `test.yml`, one matrix job: Debug on Ubuntu/Windows/macOS (the ruleset requires the `build (<os>)` checks), Release on Ubuntu/macOS (where `InterpreterSizeTests` runs), `checked-casts`; `build (ubuntu-latest)` also checks formatting. Stress on, `--blame-hang-timeout 5m`, no coverage.
- `soak.yml`: nightly, the Release core suite N times (default 20) on Ubuntu/macOS.
- `benchmark.yml`: a Dry smoke test of every benchmark, then a measured Pingpong run published to the gh-pages tracker (not a gate; series names embed the runtime spec).
- `publish.yml`: tags `v*` (must equal `<Version>`); packs with `dotnet pack -c Release -p:StripInternals=true`; only a pushed tag publishes, via NuGet trusted publishing in the `production` environment, so renaming the file or the environment breaks it. Actions are pinned to SHAs.

## Style & tooling

- Naming: factories lowercase (`FIO.succeed`, `FIO.acquireReleaseWith`), instance members PascalCase (`effect.FlatMap`), library modules qualified (`Console.printLine`).
- Opens: none unused; groups in the order project-local (`FIO.*.Tests`, `FIO.Benchmarks`), FIO, third-party (Expecto, FsCheck, BenchmarkDotNet), `System`, `Microsoft`, a blank line between groups, shortest line first within a group.
- Commits: short, sentence-style ("Fix benchmark output").
- Comments: see `docs/COMMENT_STYLE.md`. One-line `///` on public API, none on internals; doc XML must be well-formed (`WarnOn 3390`), and a comment with markup opens with `<summary>`.
- Fantomas (pinned in `dotnet-tools.json`) owns layout, with one `.editorconfig` override (`fsharp_blank_lines_around_nested_multiline_expressions = false`) that keeps the AAA blank lines. Run it before calling work done; a Fantomas bump is its own PR. Write `new T({ r with … })`: Fantomas 8.0.10–8.1.0 cannot print `new T { r with … }`.
- `TreatWarningsAsErrors=true` everywhere; use it, not `WarningsAsErrors` (which takes a list of warning numbers).
- Package versions live in `Directory.Packages.props`. FSharp.Core 11.0.101 is the consumers' floor: raise it only when the code needs a newer API.
- `[<TailCall>]` goes on ordinary recursion that must be a loop (`Ref.cas`, the HTTP route matchers, both `InterpretAsync` members), never on FIO effect loops; keep `return! loop ()` last.
