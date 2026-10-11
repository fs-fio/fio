window.BENCHMARK_DATA = {
  "lastUpdate": 1791723918090,
  "repoUrl": "https://github.com/fs-fio/fio",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "aac8758699d216b1f68814a253b0bead0c9ea29d",
          "message": "optimization (#47)\n\n* Cont stack pooling\n\n* More optimization and fixes\n\n* More fixes.\n\n* More fixes\n\n* Added more tests (TODO) and started implementing fiber interruption\n\n* More intterupt stuff for direct runtime\n\n* Tests works for now\n\n* Cleanup\n\n* Fixed benchmark running problem\n\n* Removed unused import\n\n* More optimization\n\n* More stuff\n\n* More fixes.\n\n* Optimizations\n\n* Updates\n\n* More optimization\n\n* More optimizations\n\n* More optimizations\n\n* More optimizations\n\n* Optimized App.fs.\n\n* Little bit more refinement\n\n* Refined retry logic\n\n* Added experimental libraries\n\n* Fixes\n\n* Improved App.fs.\n\n* Improved FWebSockets.\n\n* Fixed namings\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Fixed benchmark runner console output\n\n* Tested performance improvements (#43)\n\n* Update\n\n* Update publish\n\n* Update NuGet versions\n\n* Updated CI/CD\n\n* More updates\n\n* Update NuGet\n\n* Fixed fatal interruption bug\n\n* Codebase is now more F#-idiomatic.\n\n* House keeping\n\n* More\n\n* Tests\n\n* Feature/optimization test improvements (#44)\n\n* Safety commit\n\n* Working on tests\n\n* Updated tests\n\n* Updates\n\n* Update, added fantomas etc\n\n* update\n\n* Add param and returns XML doc tags to Fiber, Promise, Ref, and Semaphore\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>\n\n* Update\n\n* Implemented phase 1\n\n* Implemented shared interpreter\n\n* Parts of phase 2\n\n* Worker refactor, back pressure, bug fix\n\n* Updates\n\n* Updates\n\n* Updated comments\n\n* Comments\n\n* Updates\n\n* Benchmarks\n\n* Update\n\n* Major updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Updates\n\n* work stealing scheduler (#46)\n\n* Add work-stealing SignalingRuntime; benchmark, plotting, and docs updates\n\nReplace SignalingRuntime's single global queue and dedicated blocking worker with\na work-stealing scheduler: per-worker runNext slot + work-stealing deque\n(Runtime.fs) + a shared global queue, with a register/recheck/block park. Adds\nChannel.TryDequeueBlockingWorkItem (Core.fs). Channel unblocks, forks, and\nstep-yields schedule locally and are reclaimed by the signalling worker or stolen\nby an idle peer, removing the two-wakeup rendezvous detour.\n\nResult vs the previous runtime (Signaling/Direct at largest params): Pingpong\n3.3x->1.9x, Threadring 3.0x->1.4x, BoundedBuffer 2.2x->1.2x, Bang ~tied,\nFibonacci now 2.4x faster than Direct; one regression (Counting +12%). All 1530\ntests pass across the three runtimes.\n\nAlso includes the earlier benchmark-suite verification, plot.py static-image\nexport, CLAUDE.md rewrite + CLAUDE.local.md git-ignore, and docs updates.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01Ntr7CeQVkK9GWLNYHyVEha\n\n* Updates\n\n* Updates\n\n* Updates\n\n* Document accepted WorkStealing write/wake StoreLoad race\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01QR73SLCP4mYGjc5oB4T1JP\n\n* Make worker shutdown tolerate runtime-disposal races (fixes Windows CI crash)\n\nCapture the cancellation token before the worker task starts and treat ObjectDisposedException\nas a benign stop, so a worker racing runtime Dispose cannot throw an unhandled exception and\ncrash the test host. Surfaced by the Windows CI matrix; not reproduced on macOS/Linux.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01QR73SLCP4mYGjc5oB4T1JP\n\n---------\n\nCo-authored-by: Claude Opus 4.8 <noreply@anthropic.com>\n\n* Updates\n\n---------\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>\nCo-authored-by: Claude Opus 4.8 <noreply@anthropic.com>",
          "timestamp": "2026-07-01T00:30:01+02:00",
          "tree_id": "ccf47e3f58e350ba022da654af8b3c9aa0eb19cd",
          "url": "https://github.com/fs-fio/fio/commit/aac8758699d216b1f68814a253b0bead0c9ea29d"
        },
        "date": 1782858703116,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 201.81202180000002,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 538.6482113999999,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 272.7917038,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 198.29962325,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "itsdanieldk",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "hey@itsdaniel.dk",
            "name": "itsdanieldk",
            "username": "itsdanieldk"
          },
          "distinct": true,
          "id": "1aa62e5efb796ea30948750f21d3e56c5d3ccd3f",
          "message": "Cleanup commit",
          "timestamp": "2026-07-01T00:38:02+02:00",
          "tree_id": "ccf47e3f58e350ba022da654af8b3c9aa0eb19cd",
          "url": "https://github.com/fs-fio/fio/commit/1aa62e5efb796ea30948750f21d3e56c5d3ccd3f"
        },
        "date": 1782859261696,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 202.9394716,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 552.91084425,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 266.0187735,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 205.8157094,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "892d840d612ab39a1fc9aa8566de31e3f4d8feff",
          "message": "Improve naming consistency for FIOApp and exceptions (#56)\n\n* improved FIOApp and exn naming consistency\n\n* fix: missed one exn\n\n* Fix flaky Windows-only WebSocket/HTTP test harness (#57)\n\nRetry HttpListener/Kestrel bind on a fresh ephemeral port to avoid the\nfindAvailablePort port-reuse race (Windows HTTP.sys machine-wide URL\nregistration conflicts), and raise the runWithTimeout safety-net cap from\n10s to 30s for the slower Windows WebSocket path.\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>\n\n---------\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-07-02T16:03:07+02:00",
          "tree_id": "5f7681e440d76557ea1bdbd3a4bbba287b7717a7",
          "url": "https://github.com/fs-fio/fio/commit/892d840d612ab39a1fc9aa8566de31e3f4d8feff"
        },
        "date": 1783001087649,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 223.2647142,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 534.69066425,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 268.0505364,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 205.549811,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "512d12c61d2fa5edde6ba63470660e7e05e14729",
          "message": "Add JoinFirst and JoinAllFailFast primitives with updated parallel combinators (#58)\n\n* Add JoinFirst and JoinAllFailFast primitives and rebuild the parallel combinators on them\n\nTwo runtime primitives let a fiber park until the first of N fibers settles\n(JoinFirst) or until all succeed / any fails (JoinAllFailFast, hook-driven\nlatch). ZipPar, ZipParError, Race, RaceFirst and forEachPar are rebuilt on\nthem: fail-fast semantics, losers interrupted, children forked bare, and\nforEachPar no longer hangs on a stuck peer. Fiber completion now publishes\nthe result before firing on-terminal hooks so observers can read it.\n\nIncludes primitive test suites with stress guards across all four runtimes,\na RaceFirst semantics pinning test (first terminal wins, including\ninterruption), the ZipRace combinator microbenchmark, the stdlib-only\nbenchmarks/compare.py A/B diff tool, a four-runtime benchmark CI smoke test,\nand updated CLAUDE.md/benchmark docs.\n\nCo-Authored-By: Claude Fable 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01SAG6jSxb65GureQqFHbE6M\n\n* Sync documentation with the JoinFirst/JoinAllFailFast work\n\nCLAUDE.md and copilot-instructions.md gain the two new DU cases, the park\nhelpers, the fail-fast parallelism invariant, and the benchmark suite's\nworkload/microbenchmark split; the benchmarks README inlines the full A/B\ncomparison protocol (previously referenced externally); the root README\nadvertises structured concurrency. Removes the last dangling references to\nthe deleted docs/adr directory.\n\nCo-Authored-By: Claude Fable 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01SAG6jSxb65GureQqFHbE6M\n\n* Rebuild raceAll on the JoinFirst primitive\n\nraceAll was left on the pre-JoinFirst watcher/channel pattern: interrupted\nracers silently left its all-failed accounting, so mixes of failed and\ninterrupted racers could park it forever, and it paid two fibers per racer.\nIt now forks racers bare and loops joinFirst over the survivors — first\nsuccess wins and interrupts the rest; failed or interrupted racers retire;\nall-non-success fails with the last error or propagates interruption.\n\nRepeated parking over surviving fibers exposed a lost-wakeup window in\nparkJoinFirstOnHooks (a stale fired hook consumes the once-per-context\ngate), closed with an explicit post-install terminal recheck. Adds\nregression tests (including one that hangs on the old implementation) and\na re-park stress loop.\n\nCo-Authored-By: Claude Fable 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01SAG6jSxb65GureQqFHbE6M\n\n* Bump version to 0.2.0-beta\n\nThe JoinFirst/JoinAllFailFast primitives and the fail-fast semantics of the\nparallel combinators (including the RaceFirst first-to-settle change and the\nraceAll rebuild) warrant a minor version step.\n\nCo-Authored-By: Claude Fable 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01SAG6jSxb65GureQqFHbE6M\n\n* fixed comments\n\n* Address API audit follow-ups and deflake the SetOnTerminal ordering test\n\nOperator tables now describe <&&> truthfully (awaits both, fail-fast);\nRepeatN interrupts with InvalidArgument for n < 1 instead of silently\nrunning once; OrInterrupt and FilterOrInterrupt interrupt with\nExplicitInterrupt carrying the derived message instead of mislabeling as\nResourceExhaustion; Race's first-to-succeed semantics are pinned by tests\nfrom both sides.\n\nThe pre-terminal SetOnTerminal test raced completion on Windows CI: fiber\ncompletion publishes the result before firing the on-terminal hook (hooks\nread Task.Result by design), so Task awaiters may resume before the hook\nruns. The test now waits for the hook and asserts before installing the\nsecond callback, making it deterministic on any scheduler.\n\nCo-Authored-By: Claude Fable 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01SAG6jSxb65GureQqFHbE6M\n\n---------\n\nCo-authored-by: Claude Fable 5 <noreply@anthropic.com>",
          "timestamp": "2026-07-06T14:37:50+02:00",
          "tree_id": "3744f49de5bddff6829e85f56e0d28cbbb603052",
          "url": "https://github.com/fs-fio/fio/commit/512d12c61d2fa5edde6ba63470660e7e05e14729"
        },
        "date": 1783341578940,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 231.944122125,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 567.4578834,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 298.981415,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 213.095671,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7bb373a69970986e0e057e8543d8017f555bfdfc",
          "message": "Improve app shutdown hooks and update NuGet packages (#60)\n\n* feat: improve app shutdown hooks and update nuget packages\n\n* Build(deps): Bump actions/setup-dotnet from 4 to 5 (#59)\n\nBumps [actions/setup-dotnet](https://github.com/actions/setup-dotnet) from 4 to 5.\n- [Release notes](https://github.com/actions/setup-dotnet/releases)\n- [Commits](https://github.com/actions/setup-dotnet/compare/v4...v5)\n\n---\nupdated-dependencies:\n- dependency-name: actions/setup-dotnet\n  dependency-version: '5'\n  dependency-type: direct:production\n  update-type: version-update:semver-major\n...\n\nSigned-off-by: dependabot[bot] <support@github.com>\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>\n\n* fixed shutdown handler\n\n---------\n\nSigned-off-by: dependabot[bot] <support@github.com>\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
          "timestamp": "2026-07-09T14:33:46+02:00",
          "tree_id": "1682b8aad067029bef020d1620bdd935df0acdb8",
          "url": "https://github.com/fs-fio/fio/commit/7bb373a69970986e0e057e8543d8017f555bfdfc"
        },
        "date": 1783602140846,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 231.4311838,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 554.94241025,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 294.5758505,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 196.24696775,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "36d456fe14079cd834c41f0535fc4a36ccccb08a",
          "message": "feat: add new logo (#61)\n\n* feat: new logo",
          "timestamp": "2026-07-12T21:11:19+02:00",
          "tree_id": "6514b6dc3436b952c63e9135e7378ea6e70d6c24",
          "url": "https://github.com/fs-fio/fio/commit/36d456fe14079cd834c41f0535fc4a36ccccb08a"
        },
        "date": 1783883584672,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 221.850564,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 555.8893717999999,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 292.67114275,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 198.144321,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "eacb88e7c301b8fb73dd7ddcc8429d6e1760caef",
          "message": "feat: update logo again (#62)",
          "timestamp": "2026-07-12T21:59:45+02:00",
          "tree_id": "f23d08607a42e244eb3b63a2e346855068339bcf",
          "url": "https://github.com/fs-fio/fio/commit/eacb88e7c301b8fb73dd7ddcc8429d6e1760caef"
        },
        "date": 1783886484429,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 219.277231375,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 555.1637728,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 289.631489,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 203.28752740000002,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "298a8727e56117d2acfee54940d323e25102935b",
          "message": "feat: update readme and bump version (#63)\n\n* feat: update readmes\n\n* feat: added new readme for examples, bumped version",
          "timestamp": "2026-07-12T22:37:45+02:00",
          "tree_id": "720e17ec899065cea4945013a2313c5636f4c869",
          "url": "https://github.com/fs-fio/fio/commit/298a8727e56117d2acfee54940d323e25102935b"
        },
        "date": 1783888758598,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 261.1486915,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 774.5056856,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 319.4795794,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 215.34197980000002,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b41b95994d44190c44ec35372b731bda1106cad6",
          "message": "Fix interruptions and improve CI and test suite (#73)\n\n* bulk update\n\n* chore: cleanup\n\n* fix: interruption cause lost when a fiber is interrupted while blocked\n\n* fix: make WebSocketServer.close/abort suppress, matching ServerSocket\n\n* fix: ci pipeline repair\n\n* fix: test suite improvements\n\n* chore: README updates\n\n* fix: test contention",
          "timestamp": "2026-09-11T19:21:57+02:00",
          "tree_id": "a4e359967bf834800a1eb81419f7c0d7ee0421e7",
          "url": "https://github.com/fs-fio/fio/commit/b41b95994d44190c44ec35372b731bda1106cad6"
        },
        "date": 1789147417999,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 229.8038674,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 924.0263116,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 280.535378,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 220.3543564,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "49699333+dependabot[bot]@users.noreply.github.com",
            "name": "dependabot[bot]",
            "username": "dependabot[bot]"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d66715c735fb8b50b018a60d3fc6b89c24a4dbb7",
          "message": "Build(deps): bump the actions group with 4 updates (#74)\n\nBumps the actions group with 4 updates: [actions/checkout](https://github.com/actions/checkout), [actions/cache](https://github.com/actions/cache), [actions/upload-artifact](https://github.com/actions/upload-artifact) and [softprops/action-gh-release](https://github.com/softprops/action-gh-release).\n\n\nUpdates `actions/checkout` from 4 to 7\n- [Release notes](https://github.com/actions/checkout/releases)\n- [Changelog](https://github.com/actions/checkout/blob/main/CHANGELOG.md)\n- [Commits](https://github.com/actions/checkout/compare/v4...v7)\n\nUpdates `actions/cache` from 4 to 6\n- [Release notes](https://github.com/actions/cache/releases)\n- [Changelog](https://github.com/actions/cache/blob/main/RELEASES.md)\n- [Commits](https://github.com/actions/cache/compare/v4...v6)\n\nUpdates `actions/upload-artifact` from 4 to 7\n- [Release notes](https://github.com/actions/upload-artifact/releases)\n- [Commits](https://github.com/actions/upload-artifact/compare/v4...v7)\n\nUpdates `softprops/action-gh-release` from 2 to 3\n- [Release notes](https://github.com/softprops/action-gh-release/releases)\n- [Changelog](https://github.com/softprops/action-gh-release/blob/master/CHANGELOG.md)\n- [Commits](https://github.com/softprops/action-gh-release/compare/v2...v3)\n\n---\nupdated-dependencies:\n- dependency-name: actions/checkout\n  dependency-version: '7'\n  dependency-type: direct:production\n  update-type: version-update:semver-major\n  dependency-group: actions\n- dependency-name: actions/cache\n  dependency-version: '6'\n  dependency-type: direct:production\n  update-type: version-update:semver-major\n  dependency-group: actions\n- dependency-name: actions/upload-artifact\n  dependency-version: '7'\n  dependency-type: direct:production\n  update-type: version-update:semver-major\n  dependency-group: actions\n- dependency-name: softprops/action-gh-release\n  dependency-version: '3'\n  dependency-type: direct:production\n  update-type: version-update:semver-major\n  dependency-group: actions\n...\n\nSigned-off-by: dependabot[bot] <support@github.com>\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
          "timestamp": "2026-09-11T19:29:09+02:00",
          "tree_id": "e8ffd6ad6b9f439c312e41c1e13f8f2c2b0d3bbd",
          "url": "https://github.com/fs-fio/fio/commit/d66715c735fb8b50b018a60d3fc6b89c24a4dbb7"
        },
        "date": 1789147842810,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 233.0711565,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 561.9414707999999,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 282.447795,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 195.11521580000002,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "cb15c12a33de4825990080d1515d3ddf208f2d8d",
          "message": "Release 0.4.0-beta (#76)",
          "timestamp": "2026-09-17T14:38:42+02:00",
          "tree_id": "3c221e9691a4e2920355c8e327e09b3f30f2a40f",
          "url": "https://github.com/fs-fio/fio/commit/cb15c12a33de4825990080d1515d3ddf208f2d8d"
        },
        "date": 1789648818492,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 221.66999140000001,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 549.66643,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 284.51331639999995,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 205.415925,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hey@itsdaniel.dk",
            "name": "Daniel Larsen",
            "username": "itsdanieldk"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fe10c67c0277e05f5fd6761b6ca4a871d3a97efe",
          "message": "Release 0.5.0-beta (#78)\n\n* Release 0.5.0-beta\n\n* Fix the element type of Channel.Bounded, Dropping and Sliding\n\nChannel<'A>.Bounded, .Dropping and .Sliding built `Channel(...)` without a type argument or a return\nannotation, so F# generalised each into a generic method of its own (`Bounded<'c> : int -> Channel<'c>`)\nand the element type the caller wrote was ignored: `let c : Channel<string> = Channel<int>.Bounded 8`\ncompiled, and `let c = Channel<int>.Bounded 8` at module level failed with FS0030. They now return\nChannel<'A>. The test reads the signature by reflection, because a call site cannot tell the two apart.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Run socket handlers in their own fiber\n\nServerSocket.acceptLoopWith called `handler socket` in the accept loop's own fiber, before the fork. A\nhandler function that threw was therefore a defect of the loop: the server stopped accepting and the\nsocket it had just accepted was never closed. 0.4.0 called the handler inside the forked fiber. The\nhandler is now called there again (FIO.suspend), under the finalizers that close the socket and hand\nback the slot.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Run WebSocket handlers in their own fiber and bound the shutdown wait\n\nWebSocketServer.acceptLoop registered a connection and then called `handler connection.Socket` in the\nloop's own fiber. A handler function that threw killed the loop, whose shutdown finalizer then waited\nforever for that connection's `Finished` task: it had no handler fiber to complete it, and the wait is\nuninterruptible. The server never finished unwinding and never stopped its listener. The handler is\nnow called inside the forked fiber (FIO.suspend), so its finalizers close the connection and mark it\nfinished whatever the handler does. As a second line of defence the shutdown no longer waits for a\nconnection that has no handler fiber: nothing could ever mark it finished.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Make FIORuntime.Shutdown exception-safe and validate its timeout\n\nFIORuntime.Shutdown claimed `disposed` and then ran a sequence in which any exception skipped the\nrest: the remaining fibers were not interrupted, the workers were not stopped and `stopped` was never\ncompleted, so Dispose threw and a later Dispose blocked for its whole timeout. Interrupting a fiber\nthrows when its handle was disposed while it ran, or when a cancellation callback registered on its\ntoken throws; Task.Wait throws for a timeout it cannot represent. The timeout is now validated before\nanything is claimed, a fiber that cannot be interrupted is skipped, and the workers are stopped in a\nfinally.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Test that an interrupted acquire's failure reaches no handler\n\nTest only. The existing test for an acquire that fails after an interrupt asserts that the fiber's\nresult is Interrupted, which holds for any interrupted fiber, so deleting the region-exit check in the\nfailure branch of AcquiredCont left the suite green. These tests put an error handler after the acquire\nand assert it does not run, which is what that check is for: once directly at level 0, where only the\nhandler function itself could run, and once inside a restored region of an uninterruptibleMask, where\nwithout the check the handler's whole effect would run uninterruptibly.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Keep one TRX per test project in the CI matrix\n\nThe OS matrix job logged every test project to one file name, so each project overwrote the last\nand one TRX of four survived per OS. LogFilePrefix, which the release job already uses, keeps all\nfour.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Fix stale documentation and comments\n\nStatements that no longer match the code. SignalingRuntime has no blocking worker and ignores\nBlockingWorkers, as WorkStealingRuntime does; the default worker count reserves one core, not two; the\nHttp test helper is sequenced as well as the Sockets one; two test comments describe an optimisation\n(restoresOnSameStep) that is not in the source; two assertion messages still call TryWrite an offer;\nthe copilot instructions list neither Signal.fs, readKey/tryReadLine nor the Defect cause, and\nCLAUDE.md's test layout lacks the Runtime folder. Also says what a WebSocket timeout costs (the\nconnection), which signals a subscription leaves fatal, and that 130 is FIO's own convention rather\nthan ZIO's.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Abort the WebSocket after an oversized message\n\nPresent in 0.4.0 too. ReceiveMessage failed with MessageTooLarge in the middle of a message and left\nthe socket open with the rest of it unread, so the next receive returned the tail of the rejected\nmessage as a valid frame. The connection is now aborted before the failure is raised: what follows an\noversized message cannot be framed, and the peer is not owed a closing handshake it would have to wait\nbehind megabytes for.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Guard the interruption and unwinding cascades against stack overflow\n\nPresent in 0.4.0 too. Two recursions run once per level of a chain of nested forks and overflow the\nstack, which kills the process. A fiber interrupts its children from inside the cancellation callback\nof its own interruption (between 300 and 400 levels on DirectRuntime, between 3,000 and 4,000 on the\nworker runtimes); and a child's publish finishes its parent, whose publish finishes its own parent,\nall on the thread that completed the leaf (between 4,000 and 6,000 levels once the first recursion is\nguarded). When the stack is nearly used up, both now continue on a thread-pool thread instead. Shallow\ntrees, which is nearly all of them, take the same path as before.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Fix the fiber terminal hook's ordering and the publish race\n\nPresent in 0.4.0 too. Two memory-ordering defects in FiberContext, both found by reading, neither\nreproducible on demand. (1) SetOnTerminal stored the callback and then read the fiber's state with no\nfull fence between them, while a publisher changes the state and then reads the callback: both may\nsee the old value and the wake-up of a JoinAllFailFast latch (forEachPar, collectAllPar) is lost; the\nsibling SetOnUnwound already has the barrier. The callback was also a two-word voption struct that a\nreader can see torn. (2) A fiber whose Interrupt wins the state after Complete has claimed\n`completing` could have its success published anyway, reporting IsInterrupted() and a Succeeded\nresult at once; Publish now sets the result only when its own transition from Running succeeded,\nsince the interruption has set it otherwise.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Keep a timed-out hook's finalizers running\n\nPresent in 0.4.0 too. When a hook exceeded its timeout, FIOApp interrupted its fiber, waited up to two\nseconds for the fiber's task, and disposed the fiber. An interrupted fiber publishes its task before\nits finalizers run, so the wait returned at once and the disposal freed the cancellation source while\nthe finalizers were still running; the next read of the fiber's token threw ObjectDisposedException,\nwhich ended the hook as a defect with its remaining finalizers skipped and a \"recovered from an\nunhandled effect exception\" line on stderr. The timed-out hook is now only interrupted: the runtime's\ndisposal, which waits for every root to unwind, is what bounds its finalizers. The test's hook has a\nfinalizer that outlives the timeout and an outer finalizer that must still run.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Only set Cancel in the FIOApp signal handlers\n\nPresent in 0.4.0 too. FIOApp's SIGTERM and Ctrl+C handlers wrote `Cancel <- claimed` unconditionally,\nso a second signal, which they do not claim, also cleared a cancellation that another handler in the\nprocess had set. They now only ever set it.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Keep the receive buffer until an orphaned read completes\n\nPresent in 0.4.0 too. ReceiveMessage's release returned the pooled receive buffer, released the\nreceive lock and disposed the timeout sources as soon as the fiber left the receive, whether or not\nthe .NET receive it had started was over. ManagedWebSocket reads straight into the caller's array, so\na receive the fiber gave up on kept writing into an array the pool had already handed to someone\nelse, and a second receive could queue behind it and lose its message. With the fiber's own token the\nread is aborted in the same cancellation, which makes the window a few instructions wide; with a\ncaller's token (the ReceiveMessage/Receive/TryReceive/ReceiveJson overloads that take one) the\norphaned read simply carried on. The clean-up now runs when the last receive started has completed.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Harden the accept-interruptibility and signal tests\n\nTest only. The Sockets test that guards the uninterruptible accept could not tell a socket closed by the\nhandler's finalizer from one closed by the garbage collector's finalizer of an orphaned socket, and in\nthe parallel phase of the suite a collection within its 5 s window let the guarded regression pass 3\nruns out of 3; run alone it failed 3 out of 3. It now runs sequenced, gives the close one second (the\ncorrect code closes within milliseconds) and disposes its control runtime. The Signal test for\n`Next()` after the body had no timeout, so a regression there hung the suite instead of failing.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Dispose the socket when connect fails\n\nPresent in 0.4.0 too. SocketClient.connect created a .NET socket and, when the connection failed, left it\nto the garbage collector: a reconnect loop built with Retry accumulated one unclosed socket per\nattempt until a collection. The socket is now disposed when the connection fails. (An interrupted\nconnect needs nothing: Socket.ConnectAsync disposes the socket itself when cancelled.)\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Track root fibers per thread\n\nThe disposal-tracking registry cost every `runtime.Run` about 180-240 ns on every runtime (RunBurst64:\nWorkStealing 49.7 us on 0.4.0, 62.8 us on 0.5.0, 51.2 us with the registry switched off): a shared\ncounter, a striped lock and a hash set touched at Run and again, from another core, at unwind. The roots\nare now kept in a list per thread that ran them, touched only by that thread under a lock nobody else\ntakes while the runtime is live, and pruned of unwound fibers as it grows; the unwinding fiber does\nnothing. Shutdown snapshots the lists and installs its unwound hook only on the fibers it found live.\nThe Run-racing-Shutdown protocol is unchanged: a Run adds before reading the disposed flag, Shutdown\nsets the flag before snapshotting, with a full fence on both sides.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Disable the receive timeout by default\n\nA receive timeout aborts the connection when it elapses, which is the only thing ManagedWebSocket\ncan do with a cancelled receive, and thirty seconds of silence is normal for a WebSocket. The default\nconfiguration therefore had every idle connection abort after 30 s. It now has no receive timeout;\nwithReceiveTimeout sets one for a peer that must speak regularly. SendTimeout stays at 30 s: a send\nthat stalls means a peer that stopped reading.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Add a changelog\n\nThe 0.5.0-beta diff carries eight breaking changes that no document named: the cancelToken renames\nin FIO.WebSockets, the WebSocketConfig field, Forever<'B>, DirectRuntime becoming IDisposable, the\nfatal-error exit code, the daemon wait at FIOApp exit, the FoldFIO/TapBoth routing and now the receive\ntimeout default. CHANGELOG.md lists them, the additions, and the review fixes, in Keep a Changelog\nform; the README links to it.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Document Fiber.Dispose, remove dead code and the answered TODO list\n\nFiber is IDisposable with no stated contract, and two of the review's findings came from disposing a\nfiber that was still running: its next token read throws ObjectDisposedException, which ends it as a\ndefect with its remaining finalizers skipped. The doc comment now says to dispose a finished fiber\nonly. MailboxQueue.Clear, FiberContext.Cancel (which cancelled without publishing) and the\nsetupForkRegistration helper had no callers and are gone. The App example notes that its exit code 2\nfor a fatal error is a custom mapping, the default being 1. TODO.md's five questions are resolved:\nChainCont is one frame, the MailboxQueue constructor is private, no cancelToken identifier remains,\nChannel exposes Write/TryWrite/Read, and the AcquireRelease primitive measured 30 % faster and 38 %\nleaner than the derived form, so both new effect cases stay.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Sequence the WebSocket shutdown-refusal test\n\n\"serve refuses a connection that arrives during its shutdown with 503\" connects 300 ms into a 2 s\nshutdown window. Under the load of the suite's parallel phase, with other builds running on the\nmachine, that sleep overran the window on three runtimes and the connect met a closed listener. The\ntest now runs sequenced, like the other tests that depend on wall-clock windows, and the window is\n5 s; what it asserts is unchanged.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Let the client answer the going-away close before the shutdown test ends\n\n\"serve interrupts a handler that outlasts the shutdown timeout\" forked a client fiber to answer the\nserver's going-away close and then ended its effect right after interrupting the server. A finishing\nfiber interrupts the fibers it forked, so the closer was aborted before the close reached it, and the\nhandler's finalizer closed against an aborted peer. On macOS that read fails at once; on Linux and\nWindows it waited out the 30 s send timeout, past the test's own bound, on every CI run of the\ncandidate. The test now awaits the closer after the interruption; what it asserts is unchanged.\n\nCo-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01V9p1qsm1WYEKVmExV3QaiQ\n\n* Update\n\n* Update\n\n* Updates\n\n* Update\n\n* Update\n\n* Update\n\n* Update\n\n* Update\n\n---------\n\nCo-authored-by: Claude Fable 5.1 <noreply@anthropic.com>",
          "timestamp": "2026-10-11T15:03:51+02:00",
          "tree_id": "7813ed80b8bddfb1a309d8340a52761f9293e8e7",
          "url": "https://github.com/fs-fio/fio/commit/fe10c67c0277e05f5fd6761b6ca4a871d3a97efe"
        },
        "date": 1791723917519,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Pingpong - Direct",
            "value": 113.56008625,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Polling-2-200-1",
            "value": 420.38510825,
            "unit": "ms"
          },
          {
            "name": "Pingpong - Signaling-2-200-1",
            "value": 153.01698775,
            "unit": "ms"
          },
          {
            "name": "Pingpong - WorkStealing-2-200-1",
            "value": 81.0232025,
            "unit": "ms"
          }
        ]
      }
    ]
  }
}