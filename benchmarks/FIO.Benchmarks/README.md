# FIO Benchmarks

Macro benchmarks for the FIO effect system using [BenchmarkDotNet](https://benchmarkdotnet.org/).

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

The default run (no `--filter`) executes all 11 benchmarks × all parameter combinations × 3 runtimes × 30 iterations. This produces a comprehensive performance profile across Direct, Polling, and Signaling runtimes.

> Passing `--job` on the command line (e.g. `--job Dry` or `--job Short`) **overrides** the
> env-var-configured job — only the CLI job runs. Omit `--job` to honor `FIO_BENCH_WARMUP` /
> `FIO_BENCH_ITERATIONS`; for a quick run without `--job`, set `FIO_BENCH_ITERATIONS=1 FIO_BENCH_WARMUP=0`.

Results are written to `BenchmarkDotNet.Artifacts/results/`.

## Configuration

All parameters are configurable via environment variables. If unset, sensible defaults are used.

### Runtimes

```bash
FIO_BENCH_RUNTIMES="Direct,Signaling-8-100-2"
```

Default: `Direct,Polling-12-200-1,Signaling-12-200-1`

Runtime spec format: `Direct` | `Polling-{EWC}-{EWS}-{BWC}` | `Signaling-{EWC}-{EWS}-{BWC}`

| Param | Meaning | Recommended |
|-------|---------|-------------|
| **EWC** | Evaluation worker count (fiber schedulers) | `CPU cores - 2` (reserves 1 for BWC + 1 for OS/BDN) |
| **EWS** | Evaluation steps per work item before rescheduling | `100`–`300` (default: `200`) |
| **BWC** | Blocking worker count (handles blocking I/O) | `1`–`2` |

**Tuning guidance:**
- **EWC** — Set to `nproc - 2` to leave one core for the blocking worker and one for the OS/BDN harness. More workers than available cores causes contention; fewer underutilizes the machine.
- **EWS** — Higher values reduce scheduling overhead but increase tail latency. `200` is a good balance. Use `100` for latency-sensitive workloads, `300`+ for throughput-heavy ones.
- **BWC** — `1`.

**Quick reference by machine:**

| Cores | Recommended spec |
|:-----:|-----------------|
| 4 | `Signaling-2-200-1` |
| 8 | `Signaling-6-200-1` |
| 10 | `Signaling-8-200-1` |
| 14 | `Signaling-12-200-1` |
| 16 | `Signaling-14-200-1` |
| 32 | `Signaling-30-200-1` |
| 64 | `Signaling-62-200-1` |

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
| `FIO_BENCH_FIBONACCI_THRESHOLD` | `12` | Depth at/below which fib is computed sequentially |
| `FIO_BENCH_FORK_ACTORS` | `1000,10000,50000` | Fork actor counts |
| `FIO_BENCH_PHILOSOPHERS_COUNT` | `20` | Dining-philosophers count |
| `FIO_BENCH_PHILOSOPHERS_ROUNDS` | `10000` | Dining-philosophers eat rounds |
| `FIO_BENCH_PINGPONG_ROUNDS` | `10000,50000,150000` | Pingpong round counts |
| `FIO_BENCH_THREADRING_ACTORS` | `50,100` | Threadring actor counts |
| `FIO_BENCH_THREADRING_ROUNDS` | `1000,10000` | Threadring round counts |
| `FIO_BENCH_TRAPEZOIDAL_WORKERS` | `8,16` | Trapezoidal worker count |
| `FIO_BENCH_TRAPEZOIDAL_POINTS` | `1000000` | Trapezoidal total points |

### Full Example

```bash
FIO_BENCH_RUNTIMES="Signaling-12-200-1" FIO_BENCH_FORK_ACTORS="5000,25000" \
  dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*"
```

## Interpreting results

- **Runtime crossover:** at small parameter sizes `DirectRuntime` (built on .NET tasks) often
  *outperforms* the fiber runtimes, whose fixed scheduling overhead dominates when there is little work.
  `PollingRuntime` / `SignalingRuntime` are designed to win at scale — compare runtimes at realistic
  (large) parameter sizes, not at smoke-test sizes.
- **Direct + huge fork counts:** a large `FIO_BENCH_FORK_ACTORS` (e.g. `50000`) spawns one .NET task per
  actor under `Direct` and is correspondingly heavy on time/memory. Lower the count when profiling
  `Direct`, or focus large fork runs on the fiber runtimes.
- **Allocations and boxing:** every benchmark except **Big** passes `int` messages, which the channel
  implementation boxes to `obj`. The `Allocated` column therefore includes per-message boxing as well as
  scheduling allocations; **Big** uses reference-typed messages and does not box.
- **Reading the `Allocated` column for fiber runtimes:** `Polling` and `Signaling` keep worker
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
prints a one-line notice. This reads all `*-report.csv` files from
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
