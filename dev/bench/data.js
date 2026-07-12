window.BENCHMARK_DATA = {
  "lastUpdate": 1783888759100,
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
      }
    ]
  }
}