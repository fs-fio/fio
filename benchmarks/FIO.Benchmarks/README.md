# FIO Benchmarks

Macro benchmarks for the FIO effect system using [BenchmarkDotNet](https://benchmarkdotnet.org/).

## Benchmarks

| Benchmark | Measures |
|-----------|----------|
| **Pingpong** | Message delivery overhead (2 actors) |
| **Threadring** | Message passing + context switching (ring topology) |
| **Big** | Mailbox contention (many-to-many) |
| **Bang** | Many-to-one message passing |
| **Fork** | Fiber creation and scheduling overhead |

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

The default run (no `--filter`) executes all 5 benchmarks × all parameter combinations × 3 runtimes × 30 iterations. This produces a comprehensive performance profile across Direct, Cooperative, and Concurrent runtimes.

Results are written to `BenchmarkDotNet.Artifacts/results/`.

## Configuration

All parameters are configurable via environment variables. If unset, sensible defaults are used.

### Runtimes

```bash
FIO_BENCH_RUNTIMES="Direct,Concurrent-8-100-2"
```

Default: `Direct,Cooperative-12-200-1,Concurrent-12-200-1`

Runtime spec format: `Direct` | `Cooperative-{EWC}-{EWS}-{BWC}` | `Concurrent-{EWC}-{EWS}-{BWC}`

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
| 4 | `Concurrent-2-200-1` |
| 8 | `Concurrent-6-200-1` |
| 10 | `Concurrent-8-200-1` |
| 14 | `Concurrent-12-200-1` |
| 16 | `Concurrent-14-200-1` |
| 32 | `Concurrent-30-200-1` |
| 64 | `Concurrent-62-200-1` |

### Benchmark Parameters

| Variable | Default | Description |
|----------|---------|-------------|
| `FIO_BENCH_WARMUP` | `3` | Warmup iterations before measurement |
| `FIO_BENCH_ITERATIONS` | `30` | Measured iterations per benchmark |
| `FIO_BENCH_PINGPONG_ROUNDS` | `10000,50000,150000` | Pingpong round counts |
| `FIO_BENCH_THREADRING_ACTORS` | `50,100` | Threadring actor counts |
| `FIO_BENCH_THREADRING_ROUNDS` | `1000,10000` | Threadring round counts |
| `FIO_BENCH_BIG_ACTORS` | `10,25` | Big actor counts |
| `FIO_BENCH_BIG_ROUNDS` | `100,500` | Big round counts |
| `FIO_BENCH_BANG_ACTORS` | `50,200` | Bang actor counts |
| `FIO_BENCH_BANG_ROUNDS` | `1000,5000` | Bang round counts |
| `FIO_BENCH_FORK_ACTORS` | `1000,10000,50000` | Fork actor counts |

### Full Example

```bash
FIO_BENCH_RUNTIMES="Concurrent-12-200-1" FIO_BENCH_FORK_ACTORS="5000,25000" \
  dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*"
```

## Plotting Results

After running benchmarks, generate interactive HTML charts comparing runtimes:

```bash
pip install pandas plotly
python benchmarks/plot.py
```

This reads all CSV reports from `BenchmarkDotNet.Artifacts/results/` and produces one interactive HTML chart per benchmark in `BenchmarkDotNet.Artifacts/plots/`. Each chart includes:

- **Grouped bar chart** — Mean time by runtime for each parameter combination
- **Error bar chart** — Mean ± StdDev showing variance across iterations
