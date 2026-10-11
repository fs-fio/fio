# FIO Benchmarks

[![Performance Benchmarks](https://github.com/fs-fio/fio/actions/workflows/benchmark.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/benchmark.yml)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)

Macro benchmarks for the [FIO](https://github.com/fs-fio/fio) effect system, built on
[BenchmarkDotNet](https://benchmarkdotnet.org/). Every workload runs across all four runtimes
and reports **both execution time and allocated memory**, so a run is a full performance profile
of the scheduler and channel machinery — not just a single number.

- **12 workloads** — 11 classic concurrency benchmarks + a parallel-combinator microbenchmark
- **4 runtimes** — Direct, Polling, Signaling, WorkStealing, compared side by side
- **Time + memory** — each benchmark is `[<MemoryDiagnoser>]`, so allocations show up too
- **Tunable** — worker config and per-benchmark parameters set via environment variables
- **Plotting & A/B** — `plot.py` for charts, `compare.py` for regression-flagging diffs

## Benchmarks

| Benchmark | Measures |
|-----------|----------|
| **Bang** | Many-to-one message passing |
| **Big** | Mailbox contention (many-to-many) |
| **BoundedBuffer** | Producer/consumer backpressure (bounded queue) |
| **Chameneos** | Rendezvous + shared-broker contention |
| **Counting** | Single-actor throughput + request/response |
| **Fibonacci** | Recursive fork/join tree (divide & conquer) |
| **Fork** | Fiber creation and scheduling overhead |
| **Philosophers** | Resource arbitration / deadlock-freedom |
| **Pingpong** | Message delivery overhead (2 actors) |
| **Threadring** | Message passing + context switching (ring topology) |
| **Trapezoidal** | CPU-bound master/worker map-reduce |
| **ZipRace** | Parallel-combinator overhead (ZipPar + RaceFirst per round) |

The suite contains two kinds of benchmark. The first eleven are **classic concurrency
workloads** (largely from the actor-benchmark literature) that measure the scheduler and
channel machinery under recognizable shapes. **ZipRace** is a **combinator microbenchmark**:
it invokes the parallel combinators over trivial effects so that per-invocation primitive cost
(fork, `JoinFirst` park/settle, loser interruption, join) dominates — it exists to
regression-guard the parallel-combinator runtime primitives, and doubles as the CI hang canary
for their park/wake protocol. Future primitive-level guards belong in this second category.

## Usage

```bash
# List available benchmarks
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --list flat

# Default full run (all benchmarks, all runtimes, 30 iterations)
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"

# Run a specific benchmark
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Pingpong*"

# Quick smoke test (1 iteration)
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*" --job Dry

# Short run
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*" --job Short

# Export Markdown tables
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*" --exporters GitHub
```

The full run (`--filter "*"`) executes all 12 benchmarks × all parameter combinations × 4 runtimes × 30 iterations. This produces a comprehensive performance profile across Direct, Polling, Signaling, and WorkStealing runtimes.

> Passing `--job` (or `-j`) on the command line (e.g. `--job Dry` or `-j Short`) **overrides** the
> env-var-configured job — only the CLI job runs. Omit it to honor `FIO_BENCH_WARMUP` /
> `FIO_BENCH_ITERATIONS`; for a quick run without a CLI job, set `FIO_BENCH_ITERATIONS=1 FIO_BENCH_WARMUP=0`.

Results are written to `BenchmarkDotNet.Artifacts/results/`.

## Configuration

All parameters are configurable via environment variables. If unset, sensible defaults are used.

### Runtimes

```bash
FIO_BENCH_RUNTIMES="Direct,WorkStealing-8-100-2"
```

Default: `Direct,Polling-12-200-1,Signaling-12-200-1,WorkStealing-12-200-1`

Runtime spec format: `Direct` | `Polling-{EWC}-{EWS}-{BWC}` | `Signaling-{EWC}-{EWS}-{BWC}` | `WorkStealing-{EWC}-{EWS}-{BWC}`

| Param | Meaning | Recommended |
|-------|---------|-------------|
| **EWC** | Evaluation worker count (fiber schedulers) | `CPU cores - 2` (reserves 1 for the `Polling` blocking worker + 1 for OS/BDN) |
| **EWS** | Evaluation steps per work item before rescheduling | `100`–`300` (default: `200`) |
| **BWC** | Blocking worker count — used by `PollingRuntime`; **ignored by `SignalingRuntime` and `WorkStealingRuntime`** (must still be > 0) | `1` |

**Tuning guidance:**
- **EWC** — Set to `nproc - 2` to leave one core for the `Polling` blocking worker and one for the OS/BDN harness. More workers than available cores causes contention; fewer underutilizes the machine.
- **EWS** — Higher values reduce scheduling overhead but increase tail latency. `200` is a good balance. Use `100` for latency-sensitive workloads, `300`+ for throughput-heavy ones.
- **BWC** — `1`.

**Quick reference by machine:**

| Cores | Recommended spec |
|:-----:|-----------------|
| 4 | `WorkStealing-2-200-1` |
| 8 | `WorkStealing-6-200-1` |
| 10 | `WorkStealing-8-200-1` |
| 14 | `WorkStealing-12-200-1` |
| 16 | `WorkStealing-14-200-1` |
| 32 | `WorkStealing-30-200-1` |
| 64 | `WorkStealing-62-200-1` |

### Benchmark Parameters

| Variable | Default | Description |
|----------|---------|-------------|
| `FIO_BENCH_WARMUP` | `3` | Warmup iterations before measurement |
| `FIO_BENCH_ITERATIONS` | `30` | Measured iterations per benchmark |
| `FIO_BENCH_BANG_ACTORS` | `50,200` | Bang actor counts |
| `FIO_BENCH_BANG_ROUNDS` | `1000,5000` | Bang round counts |
| `FIO_BENCH_BIG_ACTORS` | `10,25` | Big actor counts |
| `FIO_BENCH_BIG_ROUNDS` | `100,500` | Big round counts |
| `FIO_BENCH_BOUNDEDBUFFER_PRODUCERS` | `4` | Bounded-buffer producer count |
| `FIO_BENCH_BOUNDEDBUFFER_CONSUMERS` | `4` | Bounded-buffer consumer count |
| `FIO_BENCH_BOUNDEDBUFFER_CAPACITY` | `10` | Bounded-buffer capacity |
| `FIO_BENCH_BOUNDEDBUFFER_ITEMS` | `100000` | Bounded-buffer items per producer |
| `FIO_BENCH_CHAMENEOS_CREATURES` | `100` | Chameneos creature count |
| `FIO_BENCH_CHAMENEOS_MEETINGS` | `100000` | Chameneos meeting count |
| `FIO_BENCH_COUNTING_MESSAGES` | `1000000` | Counting-actor message count |
| `FIO_BENCH_FIBONACCI_N` | `25,30` | Fibonacci input(s) |
| `FIO_BENCH_FIBONACCI_THRESHOLD` | `12` | Value of `n` at/below which fib is computed sequentially |
| `FIO_BENCH_FORK_ACTORS` | `1000,10000,50000` | Fork actor counts |
| `FIO_BENCH_PHILOSOPHERS_COUNT` | `20` | Dining-philosophers count |
| `FIO_BENCH_PHILOSOPHERS_ROUNDS` | `10000` | Dining-philosophers eat rounds |
| `FIO_BENCH_PINGPONG_ROUNDS` | `10000,50000,150000` | Pingpong round counts |
| `FIO_BENCH_THREADRING_ACTORS` | `50,100` | Threadring actor counts |
| `FIO_BENCH_THREADRING_ROUNDS` | `1000,10000` | Threadring round counts |
| `FIO_BENCH_TRAPEZOIDAL_WORKERS` | `8,16` | Trapezoidal worker count |
| `FIO_BENCH_TRAPEZOIDAL_POINTS` | `1000000` | Trapezoidal total points |
| `FIO_BENCH_ZIPRACE_ROUNDS` | `1000,10000` | ZipRace round counts |

### Full Example

```bash
FIO_BENCH_RUNTIMES="WorkStealing-12-200-1" FIO_BENCH_FORK_ACTORS="5000,25000" \
  dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*"
```

## Interpreting results

- **Runtime ranking is not a size crossover.** A full 112-case sweep (all 12 workloads × every
  parameter combination × all four runtimes, 30 measured iterations, M4 Max / .NET 10.0.400, measured September 2026, before 0.5.0-beta) gives
  these geometric-mean speedups against `Direct` — above 1.00× is faster than `Direct`:

  | Runtime | smallest params | largest params | overall | faster than `Direct` in |
  |---------|:---------------:|:--------------:|:-------:|:-----------------------:|
  | `Polling-12-200-1` | 0.63× | 0.61× | **0.60×** | 8 / 28 cases |
  | `Signaling-12-200-1` | 0.72× | 0.71× | **0.68×** | 6 / 28 cases |
  | `WorkStealing-12-200-1` | 1.18× | 1.26× | **1.20×** | 23 / 28 cases |

  `WorkStealing` already beats `Direct` at smoke-test sizes and extends its lead at scale, so there is
  no size threshold below which it should be discounted. `Polling` and `Signaling` are net *slower*
  than `Direct` at both ends and essentially flat in between — they do not "win at scale", and
  restricting the comparison to large parameter sizes will not show them in a better light. Their
  worst case is **Threadring** (`Polling` 0.16×, `Signaling` 0.33×): a ring hands off strictly
  sequentially, so there is one runnable fiber at a time and no parallelism to offset the park/wake
  cost. Both remain useful as comparison runtimes; neither is a recommendation.
- **Direct + huge fork counts:** a large `FIO_BENCH_FORK_ACTORS` (e.g. `50000`) spawns one .NET task
  per actor under `Direct`. Measured (September 2026, before 0.5.0-beta), this is *not* a handicap — at 50 000 fibers `Direct` is both the
  fastest and the lightest runtime in the set:

  | Fibers | `Direct` | `Polling` | `Signaling` | `WorkStealing` |
  |-------:|---------:|----------:|------------:|---------------:|
  | 1 000 | 1.48 ms / 2.4 MB | 1.88 ms / 3.7 MB | 2.01 ms / 3.8 MB | 1.25 ms / 3.7 MB |
  | 10 000 | 13.8 ms / 24 MB | 21.9 ms / 40 MB | 22.4 ms / 40 MB | 13.3 ms / 39 MB |
  | 50 000 | **60.3 ms / 120 MB** | 104 ms / 201 MB | 108 ms / 203 MB | 62.1 ms / 197 MB |

  `WorkStealing`'s lead on Fork *shrinks* with scale rather than growing — 1.18× at 1 000 fibers,
  1.04× at 10 000, 0.97× at 50 000. Mass fiber creation is the one axis where the .NET thread pool is
  the thing to beat, so keep the large counts in the sweep: they are the interesting part.
- **Allocations and boxing:** the channel implementation boxes every message to `obj`. **Bang**, **Pingpong** and
  **Threadring** pass `int` messages (and **BoundedBuffer** replies with `int` items), so their `Allocated` column
  includes per-message boxing as well as scheduling allocations; **Big**, **Chameneos**, **Counting** and
  **Philosophers** send reference-typed messages, which do not box, and **Fibonacci**, **Fork**, **Trapezoidal**
  and **ZipRace** use no channels.
- **Reading the `Allocated` column for fiber runtimes:** `Polling`, `Signaling`, and `WorkStealing` keep worker
  threads alive continuously, and those workers perform small scheduler-housekeeping allocations
  proportional to *wall-clock time*, not to the workload. For short iterations this inflates the per-op
  `Allocated` number. The Job is configured with `MinIterationTime = 100ms` to dilute this noise, but
  treat the absolute byte counts on the fiber runtimes as an **upper bound**. The relative ordering
  (and the cross-runtime ratio shown in `summary.html`) remains meaningful; the precise byte count does
  not.
- **macOS "high priority" warning:** BDN emits `Failed to set up high priority (Permission denied)` on
  macOS because raising thread priority needs `sudo`. This is a permission warning, not a measurement
  problem — results are still valid. CI runners on Linux/Windows typically succeed.

## Plotting Results

After running benchmarks, generate charts comparing runtimes. By default the script writes both
interactive HTML **and** static images (PNG + SVG), so the results are ready to view offline without
a browser or network:

```bash
pip install pandas plotly kaleido
python benchmarks/plot.py
# choose static formats (png, svg, pdf), or disable static export with an empty value / "none":
python benchmarks/plot.py --image-formats png,svg,pdf
python benchmarks/plot.py --image-formats none
# self-test the parsers without touching artifacts (no kaleido needed):
python benchmarks/plot.py --self-test
# or point at non-default locations:
python benchmarks/plot.py --results-dir path/to/results --output-dir path/to/plots
```

`kaleido` is what renders the static images; if it is missing the script still writes the HTML and
prints a short notice. This reads all `*-report.csv` files from
`BenchmarkDotNet.Artifacts/results/` and writes to `BenchmarkDotNet.Artifacts/plots/` — an
interactive `.html` (loads Plotly from a CDN, needs network to render) plus `.png`/`.svg` static
images per output. Two outputs are produced:

- **`<Benchmark>.html`** (one per benchmark): three stacked subplots — mean time, mean ± StdDev, and
  allocated memory — grouped by runtime across parameter combinations. Y axes auto-switch to log scale
  when a benchmark spans more than 10× in value.
- **`summary.html`** (one file): cross-benchmark dashboard with a speedup heatmap vs `Direct`, an
  allocation-ratio heatmap vs `Direct`, and a per-runtime bar grid showing absolute mean times for every
  benchmark on a log axis. Uses the largest configured parameter set per benchmark, so comparisons
  reflect at-scale behaviour.

## Comparing Two Runs (A/B)

`benchmarks/compare.py` (stdlib-only — no pip installs needed) diffs two directories of
`*-report.csv` files and emits a markdown table of Δtime/Δalloc per case, flagging regressions
and wins:

```bash
python3 benchmarks/compare.py results/A results/B --label-a baseline --label-b candidate
python3 benchmarks/compare.py results/A results/B --output comparison.md
python3 benchmarks/compare.py --self-test
```

Directories are searched recursively; rows are matched on (method, params, runtime). Thresholds
default to time ±10% / alloc ±5% (`--time-threshold` / `--alloc-threshold`).

**Interpreting deltas:** allocated bytes are deterministic and comparable across sessions; wall
time drifts with machine load/thermals (20–50% observed on an M4 Max across one day, July 2026), so only
compare times from runs taken back-to-back in the same session — ideally adjacent A/B pairs per
benchmark, bracketed by re-runs of a fixed sentinel benchmark to measure the session noise floor.

**Full protocol for a working-tree-vs-baseline comparison:**

1. Check out the baseline in a git worktree and copy the *current* benchmark project into it —
   both sides must run byte-identical benchmark code; only the library may differ.
2. Prebuild both trees in Release; run with a fixed `FIO_BENCH_WARMUP`/`FIO_BENCH_ITERATIONS`.
3. For each benchmark, run baseline then candidate back-to-back and copy each `*-report.csv`
   into `results/A` / `results/B`.
4. Run a cheap fixed sentinel (e.g. Fork with `FIO_BENCH_FORK_ACTORS=1000`) on the baseline at
   the start, one or two midpoints, and the end: its max/min spread per runtime is the session's
   time-noise floor — time deltas below it are noise; allocation deltas are trustworthy regardless.
5. Diff with `compare.py`. Run the machine idle and sleep-inhibited (`caffeinate -i` on macOS).

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Examples](https://github.com/fs-fio/fio/tree/main/examples) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
