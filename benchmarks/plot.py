#!/usr/bin/env python3
"""Plot BenchmarkDotNet results comparing FIO runtimes."""

import argparse
import re
import sys
from pathlib import Path

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots


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
RUNTIME_PATTERN = re.compile(r"^(Direct|Polling|Signaling)(-\d+-\d+-\d+)?$")
LOG_SCALE_THRESHOLD = 10.0


def parse_time(value: str) -> tuple[float, str] | None:
    parts = str(value).strip().split()
    if len(parts) < 2 or parts[1] not in TIME_UNITS:
        return None
    try:
        return float(parts[0].replace(",", "")), parts[1]
    except ValueError:
        return None


def normalize_to_ms(value: str) -> float:
    parsed = parse_time(value)
    if parsed is None:
        return float("nan")
    num, unit = parsed
    return num * TIME_UNITS[unit] * 1000


def normalize_to_bytes(value: str) -> float:
    parts = str(value).strip().split()
    if len(parts) < 2 or parts[1] not in BYTE_UNITS:
        return float("nan")
    try:
        return float(parts[0].replace(",", "")) * BYTE_UNITS[parts[1]]
    except ValueError:
        return float("nan")


def find_fio_runtime_col(df: pd.DataFrame) -> str:
    for col in reversed(df.columns):
        if pd.api.types.is_string_dtype(df[col]):
            values = df[col].dropna()
            if len(values) > 0 and all(RUNTIME_PATTERN.match(str(v)) for v in values):
                return col
    raise ValueError("Could not find FIO runtime column in CSV")


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


def find_param_cols(df: pd.DataFrame, runtime_col: str) -> list[str]:
    cols = list(df.columns)
    rt_idx = cols.index(runtime_col)

    params = []
    for col in cols[:rt_idx]:
        if col in ("Method", "Job"):
            continue
        base = col.split(".")[0]
        if base in JOB_CHARACTERISTIC_COLS:
            continue
        params.append(col)
    return params


def extract_benchmark_name(filename: str) -> str:
    """FIO.Benchmarks.Benchmarks.ForkBenchmark-report.csv -> Fork."""
    match = re.search(r"\.(\w+?)Benchmark-report", filename)
    return match.group(1) if match else filename.replace("-report.csv", "")


def runtime_sort_key(rt: str) -> tuple[int, str]:
    if "Direct" in rt:
        return (0, rt)
    if "Polling" in rt:
        return (1, rt)
    return (2, rt)


RUNTIME_COLORS = {
    "Direct": "#636EFA",
    "Polling": "#EF553B",
    "Signaling": "#00CC96",
}


def runtime_color(rt: str) -> str:
    for prefix, color in RUNTIME_COLORS.items():
        if rt.startswith(prefix):
            return color
    return "#AB63FA"


def should_log_scale(values: list[float]) -> bool:
    finite = [v for v in values if v is not None and v == v and v > 0]
    if len(finite) < 2:
        return False
    return max(finite) / min(finite) > LOG_SCALE_THRESHOLD


VALID_IMAGE_FORMATS = {"png", "svg", "pdf", "jpg", "jpeg", "webp"}
_static_export_warned = False


def parse_image_formats(spec: str) -> list[str]:
    seen: list[str] = []
    for raw in str(spec).split(","):
        fmt = raw.strip().lower()
        if fmt in VALID_IMAGE_FORMATS and fmt not in seen:
            seen.append(fmt)
    return seen


def write_static(fig, output_dir: Path, name: str, formats: list[str], scale: int = 2) -> list[Path]:
    """Render the figure to static image files; warn once and skip if kaleido is unavailable."""
    global _static_export_warned
    written: list[Path] = []
    for fmt in formats:
        out = output_dir / f"{name}.{fmt}"
        try:
            fig.write_image(str(out), format=fmt, scale=scale)
            written.append(out)
        except Exception as e:  # kaleido missing/broken, or an unrenderable format
            if not _static_export_warned:
                print(f"  ! static image export skipped ({fmt}): {e}")
                print("    install kaleido to enable PNG/SVG/PDF output: pip install kaleido")
                _static_export_warned = True
    return written


def load_benchmark_csv(csv_path: Path) -> tuple[pd.DataFrame, str, list[str]]:
    df = pd.read_csv(csv_path)
    runtime_col = find_fio_runtime_col(df)
    param_cols = find_param_cols(df, runtime_col)

    df["Mean_ms"] = df["Mean"].apply(normalize_to_ms)
    if "StdDev" in df.columns:
        df["StdDev_ms"] = df["StdDev"].apply(normalize_to_ms)
    else:
        df["StdDev_ms"] = float("nan")
    if "Allocated" in df.columns:
        df["Allocated_bytes"] = df["Allocated"].apply(normalize_to_bytes)
    else:
        df["Allocated_bytes"] = float("nan")

    if param_cols:
        df["Params"] = df[param_cols].astype(str).agg(", ".join, axis=1)
    else:
        df["Params"] = "default"

    return df, runtime_col, param_cols


def plot_benchmark(csv_path: Path, output_dir: Path, image_formats: list[str] | None = None) -> Path:
    df, runtime_col, param_cols = load_benchmark_csv(csv_path)

    param_label = " / ".join(param_cols) if param_cols else "Configuration"
    bench_name = extract_benchmark_name(csv_path.name)
    runtimes = sorted(df[runtime_col].unique(), key=runtime_sort_key)
    params_sorted = sorted(
        df["Params"].unique(),
        key=lambda p: tuple(int(x) for x in re.findall(r"\d+", p)) if re.findall(r"\d+", p) else (0,),
    )

    fig = make_subplots(
        rows=3, cols=1,
        subplot_titles=(
            f"{bench_name} — Mean Time by Runtime",
            f"{bench_name} — Mean ± StdDev by Runtime",
            f"{bench_name} — Allocated Memory by Runtime",
        ),
        vertical_spacing=0.10,
    )

    all_means: list[float] = []
    all_allocs: list[float] = []

    for rt in runtimes:
        rt_data = df[df[runtime_col] == rt].set_index("Params")
        means = [rt_data.loc[p, "Mean_ms"] if p in rt_data.index else None for p in params_sorted]
        stddevs = [rt_data.loc[p, "StdDev_ms"] if p in rt_data.index else None for p in params_sorted]
        allocs = [rt_data.loc[p, "Allocated_bytes"] if p in rt_data.index else None for p in params_sorted]
        color = runtime_color(rt)

        fig.add_trace(
            go.Bar(name=rt, x=params_sorted, y=means, marker_color=color, legendgroup=rt),
            row=1, col=1,
        )
        fig.add_trace(
            go.Bar(
                name=rt, x=params_sorted, y=means,
                error_y=dict(type="data", array=stddevs, visible=True),
                marker_color=color, legendgroup=rt, showlegend=False,
            ),
            row=2, col=1,
        )
        fig.add_trace(
            go.Bar(
                name=rt, x=params_sorted, y=allocs,
                marker_color=color, legendgroup=rt, showlegend=False,
            ),
            row=3, col=1,
        )

        all_means.extend(v for v in means if v is not None)
        all_allocs.extend(v for v in allocs if v is not None)

    for row in (1, 2):
        fig.update_xaxes(title_text=param_label, row=row, col=1)
        fig.update_yaxes(title_text="Time (ms)", row=row, col=1)
        if should_log_scale(all_means):
            fig.update_yaxes(type="log", row=row, col=1)

    fig.update_xaxes(title_text=param_label, row=3, col=1)
    fig.update_yaxes(title_text="Allocated (bytes)", row=3, col=1)
    if should_log_scale(all_allocs):
        fig.update_yaxes(type="log", row=3, col=1)

    fig.update_layout(
        barmode="group",
        height=1200,
        title_text=f"FIO Benchmark: {bench_name}",
        template="plotly_white",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
    )

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{bench_name}.html"
    fig.write_html(str(output_path), include_plotlyjs="cdn")
    if image_formats:
        write_static(fig, output_dir, bench_name, image_formats)
    return output_path


def build_summary_frame(csv_paths: list[Path]) -> pd.DataFrame:
    rows = []
    for csv_path in csv_paths:
        df, runtime_col, param_cols = load_benchmark_csv(csv_path)
        bench_name = extract_benchmark_name(csv_path.name)
        if not param_cols:
            largest_param = df["Params"].iloc[0]
        else:
            def param_key(p: str) -> tuple:
                ints = re.findall(r"\d+", p)
                return tuple(int(x) for x in ints) if ints else (0,)

            largest_param = max(df["Params"].unique(), key=param_key)

        subset = df[df["Params"] == largest_param]
        for _, row in subset.iterrows():
            rows.append({
                "Benchmark": bench_name,
                "Runtime": row[runtime_col],
                "Params": largest_param,
                "Mean_ms": row["Mean_ms"],
                "Allocated_bytes": row["Allocated_bytes"],
            })

    return pd.DataFrame(rows)


def plot_summary(csv_paths: list[Path], output_dir: Path, image_formats: list[str] | None = None) -> Path | None:
    summary = build_summary_frame(csv_paths)
    if summary.empty:
        return None

    benchmarks = sorted(summary["Benchmark"].unique())
    runtimes = sorted(summary["Runtime"].unique(), key=runtime_sort_key)
    if not runtimes:
        return None

    direct_runtime = next((r for r in runtimes if r.startswith("Direct")), runtimes[0])

    pivot_time = summary.pivot_table(index="Benchmark", columns="Runtime", values="Mean_ms", aggfunc="mean")
    pivot_alloc = summary.pivot_table(index="Benchmark", columns="Runtime", values="Allocated_bytes", aggfunc="mean")
    pivot_time = pivot_time.reindex(index=benchmarks, columns=runtimes)
    pivot_alloc = pivot_alloc.reindex(index=benchmarks, columns=runtimes)

    if direct_runtime in pivot_time.columns:
        speedup = pivot_time[direct_runtime].to_numpy().reshape(-1, 1) / pivot_time.to_numpy()
        memory_ratio = pivot_alloc.to_numpy() / pivot_alloc[direct_runtime].to_numpy().reshape(-1, 1)
    else:
        speedup = pivot_time.to_numpy()
        memory_ratio = pivot_alloc.to_numpy()

    fig = make_subplots(
        rows=3, cols=1,
        subplot_titles=(
            f"Speedup vs {direct_runtime} (higher is better)",
            f"Memory usage vs {direct_runtime} (lower is better)",
            "Mean time per benchmark, by runtime",
        ),
        vertical_spacing=0.12,
        row_heights=[0.3, 0.3, 0.4],
        specs=[[{"type": "heatmap"}], [{"type": "heatmap"}], [{"type": "bar"}]],
    )

    fig.add_trace(
        go.Heatmap(
            z=speedup,
            x=runtimes,
            y=benchmarks,
            colorscale="RdYlGn",
            zmid=1.0,
            text=[[f"{v:.2f}×" if v == v else "" for v in row] for row in speedup],
            texttemplate="%{text}",
            colorbar=dict(title="Speedup", len=0.28, y=0.86),
        ),
        row=1, col=1,
    )

    fig.add_trace(
        go.Heatmap(
            z=memory_ratio,
            x=runtimes,
            y=benchmarks,
            colorscale="RdYlGn_r",
            zmid=1.0,
            text=[[f"{v:.2f}×" if v == v else "" for v in row] for row in memory_ratio],
            texttemplate="%{text}",
            colorbar=dict(title="Alloc ratio", len=0.28, y=0.50),
        ),
        row=2, col=1,
    )

    for rt in runtimes:
        means = pivot_time[rt].reindex(benchmarks).tolist() if rt in pivot_time.columns else [None] * len(benchmarks)
        fig.add_trace(
            go.Bar(
                name=rt, x=benchmarks, y=means,
                marker_color=runtime_color(rt),
                legendgroup=rt,
            ),
            row=3, col=1,
        )

    fig.update_xaxes(title_text="Runtime", row=1, col=1)
    fig.update_xaxes(title_text="Runtime", row=2, col=1)
    fig.update_xaxes(title_text="Benchmark", row=3, col=1)
    fig.update_yaxes(title_text="Benchmark", row=1, col=1, autorange="reversed")
    fig.update_yaxes(title_text="Benchmark", row=2, col=1, autorange="reversed")
    fig.update_yaxes(title_text="Mean time (ms)", row=3, col=1, type="log")

    fig.update_layout(
        barmode="group",
        height=1400,
        title_text="FIO Benchmarks — cross-benchmark summary (largest parameter set)",
        template="plotly_white",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
    )

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "summary.html"
    fig.write_html(str(output_path), include_plotlyjs="cdn")
    if image_formats:
        write_static(fig, output_dir, "summary", image_formats)
    return output_path


def resolve_default_dir(rel: str) -> Path:
    cwd_candidate = Path.cwd() / rel
    if cwd_candidate.exists():
        return cwd_candidate
    script_candidate = Path(__file__).resolve().parent.parent / rel
    return script_candidate if script_candidate.exists() else cwd_candidate


def self_test() -> None:
    cases = {
        "FIO.Benchmarks.Benchmarks.PingpongBenchmark-report.csv": "Pingpong",
        "FIO.Benchmarks.Benchmarks.BoundedBufferBenchmark-report.csv": "BoundedBuffer",
        "FIO.Benchmarks.Benchmarks.TrapezoidalBenchmark-report.csv": "Trapezoidal",
    }
    for filename, expected in cases.items():
        actual = extract_benchmark_name(filename)
        assert actual == expected, f"{filename}: expected {expected}, got {actual}"
    assert normalize_to_bytes("4.56 KB") == 4.56 * 1024
    assert normalize_to_bytes("1.23 MB") == 1.23 * 1024 ** 2
    assert normalize_to_bytes("-") != normalize_to_bytes("-")
    assert normalize_to_ms("3.850 ms") == 3.850
    assert abs(normalize_to_ms("1,111.87 ms") - 1111.87) < 1e-6

    # Runtime-column detection must recognise the current runtime names — regression guard for
    # the Cooperative/Concurrent -> Polling/Signaling rename (the formatted path was silently
    # broken before because this case was untested).
    runtime_frame = pd.DataFrame({
        "RoundCount": [10000, 10000, 10000],
        "Runtime": ["Direct", "Polling-4-200-1", "Signaling-4-200-1"],
        "Mean": ["1.0 ms", "2.0 ms", "3.0 ms"],
    })
    assert find_fio_runtime_col(runtime_frame) == "Runtime"
    assert runtime_sort_key("Direct")[0] == 0
    assert runtime_sort_key("Polling-4-200-1")[0] == 1
    assert runtime_sort_key("Signaling-4-200-1")[0] == 2
    assert runtime_color("Signaling-4-200-1") == "#00CC96"

    assert parse_image_formats("png, svg ,pdf") == ["png", "svg", "pdf"]
    assert parse_image_formats("PNG,png,svg") == ["png", "svg"]
    assert parse_image_formats("png,bogus") == ["png"]
    assert parse_image_formats("") == []
    assert parse_image_formats("none") == []


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--results-dir", type=Path, default=None,
        help="Directory containing BenchmarkDotNet *-report.csv files",
    )
    parser.add_argument(
        "--output-dir", type=Path, default=None,
        help="Directory to write HTML plots into",
    )
    parser.add_argument(
        "--image-formats", type=str, default="png,svg",
        help="Comma-separated static image formats to also emit (png,svg,pdf); "
             "empty or 'none' to skip. Requires kaleido.",
    )
    parser.add_argument(
        "--self-test", action="store_true",
        help="Run internal self-tests and exit",
    )
    args = parser.parse_args()

    if args.self_test:
        self_test()
        print("self-test passed")
        return 0

    results_dir = args.results_dir or resolve_default_dir("BenchmarkDotNet.Artifacts/results")
    output_dir = args.output_dir or resolve_default_dir("BenchmarkDotNet.Artifacts/plots")

    if not results_dir.exists():
        print(f"No results directory found at {results_dir}")
        print("Run benchmarks first: dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter \"*\"")
        return 1

    csvs = sorted(results_dir.glob("*-report.csv"))
    if not csvs:
        print(f"No CSV reports found in {results_dir}")
        return 1

    if output_dir.exists() and any(output_dir.glob("*.html")):
        existing = sorted(p.name for p in output_dir.glob("*.html"))
        print(f"Warning: {output_dir} already contains {len(existing)} HTML file(s); they will be overwritten")

    image_formats = parse_image_formats(args.image_formats)

    print(f"Found {len(csvs)} benchmark report(s)")
    if image_formats:
        print(f"Static image formats: {', '.join(image_formats)}")
    for csv_path in csvs:
        try:
            output = plot_benchmark(csv_path, output_dir, image_formats)
            print(f"  ✓ {csv_path.name} → {output.name}")
        except Exception as e:
            print(f"  ✗ {csv_path.name}: {e}")

    try:
        summary = plot_summary(csvs, output_dir, image_formats)
        if summary is not None:
            print(f"  ✓ summary → {summary.name}")
    except Exception as e:
        print(f"  ✗ summary: {e}")

    print(f"\nPlots written to {output_dir}/")
    return 0


if __name__ == "__main__":
    sys.exit(main())
