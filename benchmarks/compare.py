#!/usr/bin/env python3
"""Compare two directories of BenchmarkDotNet results (A/B) and emit a markdown delta report.

Usage:
    python benchmarks/compare.py <dir-a> <dir-b> [--label-a baseline] [--label-b candidate]
                                 [--output report.md] [--time-threshold 10] [--alloc-threshold 5]

Both directories are searched recursively for `*Benchmark-report.csv` files; benchmarks present
in only one directory are skipped (with a note). Rows are matched on (Method, params, runtime).

Unlike plot.py this script is stdlib-only (no pandas/plotly), so it runs without the plotting
venv. Unit tables and column detection mirror plot.py — keep them in sync.

Interpreting deltas: allocated bytes are deterministic and comparable across sessions; wall time
drifts with machine conditions, so only compare times from runs taken in the same session
(ideally adjacent, per benchmark). Negative Δ = improvement in B.
"""

import argparse
import csv
import re
import sys
from pathlib import Path

TIME_UNITS = {
    "ns": 1e-9,
    "us": 1e-6,
    "μs": 1e-6,
    "µs": 1e-6,
    "ms": 1e-3,
    "s": 1.0,
    "m": 60.0,
}
BYTE_UNITS = {
    "B": 1.0,
    "KB": 1024.0,
    "MB": 1024.0 ** 2,
    "GB": 1024.0 ** 3,
}
RUNTIME_PATTERN = re.compile(r"^(Direct|Polling|Signaling|WorkStealing)(-\d+-\d+-\d+)?$")

JOB_CHARACTERISTIC_COLS = {
    "AnalyzeLaunchVariance", "EvaluateOverhead", "MaxAbsoluteError", "MaxRelativeError",
    "MinInvokeCount", "MinIterationTime", "OutlierMode", "Affinity", "EnvironmentVariables",
    "Jit", "LargeAddressAware", "Platform", "PowerPlanMode", "Runtime", "AllowVeryLargeObjects",
    "Concurrent", "CpuGroups", "Force", "HeapAffinitizeMask", "HeapCount", "NoAffinitize",
    "RetainVm", "Server", "Arguments", "BuildConfiguration", "Clock", "EngineFactory",
    "NuGetReferences", "Toolchain", "IsMutator", "InvocationCount", "IterationCount",
    "IterationTime", "LaunchCount", "MaxIterationCount", "MaxWarmupIterationCount",
    "MemoryRandomization", "MinIterationCount", "MinWarmupIterationCount", "RunStrategy",
    "UnrollFactor", "WarmupCount",
}


def parse_scalar(value: str, units: dict[str, float]) -> float:
    parts = str(value).strip().split()
    if len(parts) < 2 or parts[1] not in units:
        return float("nan")
    try:
        return float(parts[0].replace(",", "")) * units[parts[1]]
    except ValueError:
        return float("nan")


def extract_benchmark_name(filename: str) -> str:
    match = re.search(r"\.(\w+?)Benchmark-report", filename)
    return match.group(1) if match else filename


def load_results(csv_path: Path) -> dict[tuple, dict]:
    """Map (method, params..., runtime) -> {param_str, runtime, mean_str, mean, alloc_str, alloc}.

    Parses positionally: the header contains *two* `Runtime` columns (the BenchmarkDotNet job
    characteristic and the FIO runtime param), so name-keyed access loses one of them.
    """
    with open(csv_path, newline="", encoding="utf-8-sig") as f:
        rows = list(csv.reader(f))
    if len(rows) < 2:
        return {}
    header, data = rows[0], rows[1:]

    # The FIO runtime param is the last column whose values all look like a runtime spec —
    # distinguishing it by value from the job-characteristic Runtime column (".NET 10.0").
    runtime_idx = next(
        (i for i in reversed(range(len(header)))
         if all(RUNTIME_PATTERN.match(r[i]) for r in data)),
        None,
    )
    if runtime_idx is None:
        raise ValueError(f"Could not find FIO runtime column in {csv_path}")

    method_idx, mean_idx = header.index("Method"), header.index("Mean")
    alloc_idx = header.index("Allocated") if "Allocated" in header else None
    param_idxs = [
        i for i in range(mean_idx)
        if i != runtime_idx
        and header[i] not in ("Method", "Job")
        and header[i].split(".")[0] not in JOB_CHARACTERISTIC_COLS
    ]

    results = {}
    for r in data:
        key = (r[method_idx], *(r[i] for i in param_idxs), r[runtime_idx])
        results[key] = {
            "params": ", ".join(f"{header[i]}={r[i]}" for i in param_idxs) or r[method_idx],
            "runtime": r[runtime_idx],
            "mean_str": r[mean_idx],
            "mean": parse_scalar(r[mean_idx], TIME_UNITS),
            "alloc_str": r[alloc_idx] if alloc_idx is not None else "",
            "alloc": parse_scalar(r[alloc_idx], BYTE_UNITS) if alloc_idx is not None else float("nan"),
        }
    return results


def find_reports(directory: Path) -> dict[str, Path]:
    return {extract_benchmark_name(p.name): p for p in sorted(directory.rglob("*Benchmark-report.csv"))}


def delta(a: float, b: float) -> float:
    return (b - a) / a * 100.0 if a == a and b == b and a != 0 else float("nan")


def flag(pct: float, threshold: float) -> str:
    if pct != pct:
        return ""
    if pct > threshold:
        return " 🔴"
    if pct < -threshold:
        return " 🟢"
    return ""


def fmt_delta(pct: float) -> str:
    return f"{pct:+.1f}%" if pct == pct else "n/a"


def compare(dir_a: Path, dir_b: Path, label_a: str, label_b: str,
            time_threshold: float, alloc_threshold: float) -> str:
    reports_a, reports_b = find_reports(dir_a), find_reports(dir_b)
    common = [n for n in reports_a if n in reports_b]
    only_a = [n for n in reports_a if n not in reports_b]
    only_b = [n for n in reports_b if n not in reports_a]

    lines = [
        f"# Benchmark comparison: {label_a} → {label_b}",
        "",
        f"Δ = ({label_b} − {label_a}) / {label_a}. Negative = improvement.",
        f"Flags: time ±{time_threshold:.0f}%, alloc ±{alloc_threshold:.0f}% (🔴 regression / 🟢 win).",
        "",
    ]
    time_regr = time_win = alloc_regr = alloc_win = compared = 0

    for name in common:
        a_rows, b_rows = load_results(reports_a[name]), load_results(reports_b[name])
        keys = [k for k in a_rows if k in b_rows]
        if not keys:
            continue
        lines += [f"## {name}", "",
                  f"| Params | Runtime | Time {label_a} → {label_b} | Δtime | Alloc {label_a} → {label_b} | Δalloc |",
                  "|---|---|---:|---:|---:|---:|"]
        for k in keys:
            a, b = a_rows[k], b_rows[k]
            dt, da = delta(a["mean"], b["mean"]), delta(a["alloc"], b["alloc"])
            compared += 1
            time_regr += dt == dt and dt > time_threshold
            time_win += dt == dt and dt < -time_threshold
            alloc_regr += da == da and da > alloc_threshold
            alloc_win += da == da and da < -alloc_threshold
            lines.append(
                f"| {a['params']} | {a['runtime']} "
                f"| {a['mean_str']} → {b['mean_str']} | {fmt_delta(dt)}{flag(dt, time_threshold)} "
                f"| {a['alloc_str']} → {b['alloc_str']} | {fmt_delta(da)}{flag(da, alloc_threshold)} |"
            )
        lines.append("")

    summary = (f"**{compared} case(s) compared** — time: {time_regr} regression(s) / {time_win} win(s); "
               f"alloc: {alloc_regr} regression(s) / {alloc_win} win(s).")
    for skipped, where in ((only_a, label_a), (only_b, label_b)):
        if skipped:
            summary += f" Skipped (only in {where}): {', '.join(skipped)}."
    lines.insert(4, summary)
    return "\n".join(lines)


def self_test() -> int:
    assert parse_scalar("1,831.27 ms", TIME_UNITS) == 1831.27 * 1e-3
    assert parse_scalar("241.0 μs", TIME_UNITS) == 241.0 * 1e-6
    assert parse_scalar("3.41 GB", BYTE_UNITS) == 3.41 * 1024 ** 3
    assert parse_scalar("NA", BYTE_UNITS) != parse_scalar("NA", BYTE_UNITS)  # NaN
    assert extract_benchmark_name("FIO.Benchmarks.Benchmarks.ForkBenchmark-report.csv") == "Fork"
    assert RUNTIME_PATTERN.match("WorkStealing-12-200-1")
    assert RUNTIME_PATTERN.match("Direct")
    assert not RUNTIME_PATTERN.match(".NET 10.0")
    assert delta(100.0, 90.0) == -10.0
    assert flag(11.0, 10.0) == " 🔴" and flag(-11.0, 10.0) == " 🟢" and flag(5.0, 10.0) == ""
    print("self-test: OK")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("dir_a", nargs="?", type=Path, help="results directory A (baseline)")
    parser.add_argument("dir_b", nargs="?", type=Path, help="results directory B (candidate)")
    parser.add_argument("--label-a", default="A", help="display label for directory A")
    parser.add_argument("--label-b", default="B", help="display label for directory B")
    parser.add_argument("--output", type=Path, help="write markdown to this file instead of stdout")
    parser.add_argument("--time-threshold", type=float, default=10.0, help="Δtime %% flag threshold")
    parser.add_argument("--alloc-threshold", type=float, default=5.0, help="Δalloc %% flag threshold")
    parser.add_argument("--self-test", action="store_true", help="validate parsers and exit")
    args = parser.parse_args()

    if args.self_test:
        return self_test()
    if not args.dir_a or not args.dir_b:
        parser.error("dir_a and dir_b are required (or use --self-test)")
    for d in (args.dir_a, args.dir_b):
        if not d.is_dir():
            parser.error(f"not a directory: {d}")

    report = compare(args.dir_a, args.dir_b, args.label_a, args.label_b,
                     args.time_threshold, args.alloc_threshold)
    if args.output:
        args.output.write_text(report + "\n", encoding="utf-8")
        print(f"wrote {args.output}")
    else:
        print(report)
    return 0


if __name__ == "__main__":
    sys.exit(main())
