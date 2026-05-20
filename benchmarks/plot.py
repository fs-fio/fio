#!/usr/bin/env python3
"""Plot BenchmarkDotNet results comparing FIO runtimes."""

import re
import sys
from pathlib import Path

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots


RESULTS_DIR = Path("BenchmarkDotNet.Artifacts/results")
OUTPUT_DIR = Path("BenchmarkDotNet.Artifacts/plots")

TIME_UNITS = {"ns": 1e-9, "us": 1e-6, "μs": 1e-6, "ms": 1e-3, "s": 1.0}
RUNTIME_PATTERN = re.compile(r"^(Direct|Cooperative|Concurrent)(-\d+-\d+-\d+)?$")


def parse_time(value: str) -> tuple[float, str]:
    """Parse BDN time string like '3.850 ms' or '1,111.87 ms' into (value, unit)."""
    parts = value.strip().split()
    return float(parts[0].replace(",", "")), parts[1]


def normalize_to_ms(value: str) -> float:
    """Convert BDN time string to milliseconds."""
    num, unit = parse_time(value)
    seconds = num * TIME_UNITS[unit]
    return seconds * 1000


def find_fio_runtime_col(df: pd.DataFrame) -> str:
    """Find the column containing FIO runtime specs (handles pandas duplicate renaming)."""
    for col in reversed(df.columns):
        if pd.api.types.is_string_dtype(df[col]):
            values = df[col].dropna()
            if len(values) > 0 and all(RUNTIME_PATTERN.match(str(v)) for v in values):
                return col
    raise ValueError("Could not find FIO runtime column in CSV")


def find_param_cols(df: pd.DataFrame, runtime_col: str) -> list[str]:
    """Find benchmark parameter columns (between WarmupCount and stats)."""
    stats = {"Mean", "Error", "StdDev", "Median", "Rank", "Gen0", "Gen1", "Gen2", "Allocated"}
    cols = list(df.columns)
    try:
        warmup_idx = cols.index("WarmupCount")
    except ValueError:
        warmup_idx = 0

    params = []
    for col in cols[warmup_idx + 1:]:
        if col in stats or col == runtime_col:
            continue
        if col.startswith("Gen") or col == "Allocated" or col == "Rank":
            continue
        if pd.api.types.is_integer_dtype(df[col]):
            params.append(col)
    return params


def extract_benchmark_name(filename: str) -> str:
    """Extract human-readable name from CSV filename."""
    # FIO.Benchmarks.Benchmarks.ForkBenchmarks-report.csv -> Fork
    match = re.search(r"\.(\w+)Benchmarks-report", filename)
    return match.group(1) if match else filename.replace("-report.csv", "")


def plot_benchmark(csv_path: Path) -> Path:
    """Generate interactive HTML chart for a benchmark CSV."""
    df = pd.read_csv(csv_path)

    runtime_col = find_fio_runtime_col(df)
    param_cols = find_param_cols(df, runtime_col)

    # Parse times to ms
    df["Mean_ms"] = df["Mean"].apply(normalize_to_ms)
    df["StdDev_ms"] = df["StdDev"].apply(normalize_to_ms)

    # Build param label for x-axis
    if param_cols:
        df["Params"] = df[param_cols].astype(str).agg(", ".join, axis=1)
        if len(param_cols) == 1:
            param_label = param_cols[0]
        else:
            param_label = " / ".join(param_cols)
    else:
        df["Params"] = "default"
        param_label = ""

    bench_name = extract_benchmark_name(csv_path.name)
    runtimes = sorted(df[runtime_col].unique(), key=lambda r: (0 if "Direct" in r else 1 if "Cooperative" in r else 2))

    # Create figure with two subplots
    fig = make_subplots(
        rows=2, cols=1,
        subplot_titles=(
            f"{bench_name} — Mean Time by Runtime",
            f"{bench_name} — Mean ± StdDev by Runtime",
        ),
        vertical_spacing=0.15,
    )

    colors = {
        "Direct": "#636EFA",
        "Cooperative": "#EF553B",
        "Concurrent": "#00CC96",
    }

    def runtime_color(rt: str) -> str:
        for key, color in colors.items():
            if rt.startswith(key):
                return color
        return "#AB63FA"

    params_sorted = sorted(df["Params"].unique(), key=lambda p: tuple(int(x) for x in re.findall(r"\d+", p)) if re.findall(r"\d+", p) else (0,))

    # Grouped bar chart (top)
    for rt in runtimes:
        rt_data = df[df[runtime_col] == rt].set_index("Params")
        means = [rt_data.loc[p, "Mean_ms"] if p in rt_data.index else 0 for p in params_sorted]
        fig.add_trace(
            go.Bar(name=rt, x=params_sorted, y=means, marker_color=runtime_color(rt), legendgroup=rt),
            row=1, col=1,
        )

    # Error bar chart (bottom)
    for rt in runtimes:
        rt_data = df[df[runtime_col] == rt].set_index("Params")
        means = [rt_data.loc[p, "Mean_ms"] if p in rt_data.index else 0 for p in params_sorted]
        stddevs = [rt_data.loc[p, "StdDev_ms"] if p in rt_data.index else 0 for p in params_sorted]
        fig.add_trace(
            go.Bar(
                name=rt, x=params_sorted, y=means,
                error_y=dict(type="data", array=stddevs, visible=True),
                marker_color=runtime_color(rt), legendgroup=rt, showlegend=False,
            ),
            row=2, col=1,
        )

    x_title = param_label if param_label else "Configuration"
    fig.update_xaxes(title_text=x_title, row=1, col=1)
    fig.update_xaxes(title_text=x_title, row=2, col=1)
    fig.update_yaxes(title_text="Time (ms)", row=1, col=1)
    fig.update_yaxes(title_text="Time (ms)", row=2, col=1)

    fig.update_layout(
        barmode="group",
        height=800,
        title_text=f"FIO Benchmark: {bench_name}",
        template="plotly_white",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    output_path = OUTPUT_DIR / f"{bench_name}.html"
    fig.write_html(str(output_path), include_plotlyjs="cdn")
    return output_path


def main():
    if not RESULTS_DIR.exists():
        print(f"No results directory found at {RESULTS_DIR}")
        print("Run benchmarks first: dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter \"*\"")
        sys.exit(1)

    csvs = sorted(RESULTS_DIR.glob("*-report.csv"))
    if not csvs:
        print(f"No CSV reports found in {RESULTS_DIR}")
        sys.exit(1)

    print(f"Found {len(csvs)} benchmark report(s)")
    for csv_path in csvs:
        try:
            output = plot_benchmark(csv_path)
            print(f"  ✓ {csv_path.name} → {output}")
        except Exception as e:
            print(f"  ✗ {csv_path.name}: {e}")

    print(f"\nPlots written to {OUTPUT_DIR}/")


if __name__ == "__main__":
    main()
