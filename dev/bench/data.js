window.BENCHMARK_DATA = {
  "lastUpdate": 1783001088494,
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
      }
    ]
  }
}