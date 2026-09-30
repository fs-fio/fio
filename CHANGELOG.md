# Changelog

All notable changes to FIO. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versions are the NuGet package versions (`FSharp.FIO`, `FSharp.FIO.Sockets`, `FSharp.FIO.WebSockets`,
`FSharp.FIO.Http` ship in lockstep).

## 0.5.0-beta

### Breaking

- **FIO.WebSockets:** every `cancelToken` parameter is now `cancellationToken` (`Close`, `CloseOutput`,
  `Receive`, `ReceiveMessage`, `Send`, `SendBinary`, `SendFrame`, `SendText`, `WebSocketClient.connect`,
  `connectString`, and the optional `?cancelToken` of `SendJson`/`ReceiveJson`). Positional calls are
  unaffected; named and optional arguments must be renamed.
- **FIO.WebSockets:** `WebSocketConfig` gained `ShutdownTimeout`. A record expression that lists every
  field must add it; `{ WebSocketConfig.defaultConfig with … }` keeps working.
- **FIO.WebSockets:** `WebSocketConfig.defaultConfig` no longer has a receive timeout (`ReceiveTimeout = 0`).
  A receive timeout aborts the connection when it elapses, and 30 s of silence is normal for a WebSocket;
  set one with `withReceiveTimeout` for peers that must speak regularly. `SendTimeout` stays at 30 s.
- **FIO:** `Forever()` is now `Forever<'B>() : FIO<'B, 'E>` — it never succeeds, so it takes whatever
  result type its context needs. A module-level binding whose type nothing later fixes needs an
  annotation.
- **FIO:** `DirectRuntime` is `IDisposable` like the other runtimes. Under `TreatWarningsAsErrors`,
  `DirectRuntime()` without `new` is now error FS0760.
- **FIO:** `FIOApp.mapExitCode` maps a fatal error (`AppFatalError`) to exit code 1 instead of 2, as every
  non-success exit does in ZIO; an interruption still exits with 130.
- **FIO:** disposing a runtime — which `FIOApp` does when the app ends — now interrupts every fiber still
  running on it, roots and daemons alike, and waits up to 10 seconds for their finalizers before stopping
  the workers. An app that left daemons running used to exit at once.
- **FIO:** `FoldFIO` and `TapBoth` hand `onError` only the effect's own failure; a failure raised by
  `onSuccess` now propagates instead of being routed into `onError` (ZIO's `foldZIO`).

### Added

- Uninterruptible regions: `FIO.uninterruptible`, `.Uninterruptible()`, `FIO.uninterruptibleMask` with an
  `InterruptibilityRestorer`; an interruption takes effect the moment a region ends. Work forked inside a
  region or a finalizer is protected until the fiber exits.
- `FIO.acquireReleaseWith` runs `acquire` and `release` uninterruptibly on a primitive of its own; a `use`
  that throws still releases.
- Bounded, dropping and sliding channels (`Channel<'A>.Bounded`, `.Dropping`, `.Sliding`) and
  `Channel.TryWrite`, which never suspends.
- `FIORuntime.Shutdown timeout` and `Dispose` that interrupt live fibers and wait for them to unwind;
  `Run` after disposal throws `ObjectDisposedException`.
- `Console.tryReadLine` (`None` at end of input) and the `Signal` module (`Signal.subscribe`,
  `SignalSubscription.Next`).
- `WebSocket.TryReceive`, `WebSocket.CloseIfOpen`, `ReceiveOutcome`; `WebSocketConfig.ShutdownTimeout` and
  `withShutdownTimeout`; a graceful server shutdown (going-away close, bounded wait, 503 for late requests).
- Sockets: `acceptLoop`/`serve` await the accept uninterruptibly and hand every accepted socket to a
  closing finalizer; `withConnection` owns the socket before connecting.

### Changed

- One continuation frame per bind (`ChainCont`) and no allocation per continuation popped: the interpreter
  allocates 104 bytes less per continuation, which is −4 % to −24 % across the benchmark suite.
- A fiber's completion waits for the fibers it forked to unwind; an interrupted fiber publishes its result
  at once while its subtree unwinds.
- `WebSocketServer.start`/`serve` rewrite `0.0.0.0` and `[::]` to `+`.

### Fixed

- `Channel<'A>.Bounded`, `.Dropping` and `.Sliding` compiled as generic methods that ignored the channel's
  element type.
- A socket handler function that threw stopped the accept loop (0.4.0 lost only that connection); a
  WebSocket handler function that threw left the server stuck in its shutdown for ever. Both handlers now
  run in their own fiber.
- `FIORuntime.Shutdown` is exception-safe and rejects a timeout it cannot wait for.
- A `runtime.Run` no longer pays for disposal tracking: root fibers are tracked per thread.
- The interruption and unwinding cascades of deeply nested forks no longer overflow the stack.
- Two memory-ordering holes in `FiberContext`: a `forEachPar`/`collectAllPar` wake-up could be lost, and
  an interruption racing completion could publish a success on a fiber that reported itself interrupted.
- A `FIOApp` hook that exceeded its timeout had its remaining finalizers skipped.
- The `FIOApp` signal handlers cleared a cancellation another handler had set.
- After `MessageTooLarge` the WebSocket is aborted instead of delivering the rest of the message as a new one.
- A WebSocket receive buffer was returned to the pool while an abandoned read could still write into it.
- `SocketClient.connect` left its socket to the garbage collector when the connection failed.
- CI kept one test-result file of four per operating system.
