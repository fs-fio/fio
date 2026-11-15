# FIO Benchmarks

[![Performance Benchmarks](https://github.com/fs-fio/fio/actions/workflows/benchmark.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/benchmark.yml)
[![Run Tests](https://github.com/fs-fio/fio/actions/workflows/test.yml/badge.svg)](https://github.com/fs-fio/fio/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
![.NET 11](https://img.shields.io/badge/.NET-11-512BD4.svg?logo=dotnet)

Macro benchmarks for [FIO](https://github.com/fs-fio/fio), built on [BenchmarkDotNet](https://benchmarkdotnet.org/).
Every workload runs on both runtimes and reports execution time and allocated memory.

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

The first eleven are classic concurrency workloads. ZipRace is a microbenchmark that guards the parallel-combinator
primitives and doubles as CI's hang canary for them.

## Usage

```bash
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --list flat              # list benchmarks
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"             # full run
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Pingpong*"    # one benchmark
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*Fork*" --job Dry   # smoke test
```

A CLI `--job` (or `-j`) replaces the job set by `FIO_BENCH_WARMUP`/`FIO_BENCH_ITERATIONS`. Results go to
`BenchmarkDotNet.Artifacts/results/`.

## Configuration

Runtimes: `FIO_BENCH_RUNTIMES="Direct,WorkStealing-{EWC}-{EWS}"`, default `Direct,WorkStealing-12-200`. EWC is the worker
count (use `CPU cores - 2`), EWS the interpreter steps per work item before a fiber yields (100–300).

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

## Reading results

- Channels box every message, so the int-message workloads (Bang, Pingpong, Threadring, BoundedBuffer) allocate per
  message.
- WorkStealing's workers allocate a little for housekeeping over wall-clock time, so treat its `Allocated` as an upper
  bound; the env-var job's 100 ms minimum iteration time dilutes this.
- BDN's macOS warning `Failed to set up high priority (Permission denied)` is harmless.

## Plotting

```bash
python3 -m venv .venv && . .venv/bin/activate && python -m pip install pandas plotly kaleido
python benchmarks/plot.py            # --image-formats png,svg,pdf | none; --self-test
```

It writes one `<Benchmark>.html` per benchmark and a `summary.html` (speedup and allocation heatmaps against `Direct`)
to `BenchmarkDotNet.Artifacts/plots/`, plus PNG/SVG images when `kaleido` is installed.

## Comparing two runs (A/B)

```bash
python3 benchmarks/compare.py results/A results/B --label-a baseline --label-b candidate
```

It matches rows on (method, params, runtime) and flags time ±10% / alloc ±5%. Allocations compare across sessions; wall
time drifts 20–50% between sessions, so:

1. Run byte-identical benchmark code on both trees (copy the current benchmark project into the baseline), in Release.
2. Run baseline and candidate back-to-back per benchmark, with the machine idle (`caffeinate -i` on macOS).
3. Bracket the session with a cheap sentinel (Fork with `FIO_BENCH_FORK_ACTORS=1000`) at the start, middle and end;
   its spread is the noise floor for time.

## Links

[FIO core](https://github.com/fs-fio/fio) ·
[Examples](https://github.com/fs-fio/fio/tree/main/examples) ·
[MIT](https://github.com/fs-fio/fio/blob/main/LICENSE.md)
