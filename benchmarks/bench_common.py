"""Parsing of BenchmarkDotNet report CSVs shared by plot.py and compare.py.

Stdlib-only, so compare.py still runs without the plotting venv.
"""

import re

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
# Also matches the removed Polling/Signaling runtimes and the old three-leg specs, so stored results stay readable.
RUNTIME_PATTERN = re.compile(r"^(Direct|Polling|Signaling|WorkStealing)(-\d+-\d+(-\d+)?)?$")

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
    """FIO.Benchmarks.Benchmarks.ForkBenchmark-report.csv -> Fork."""
    match = re.search(r"\.(\w+?)Benchmark-report", filename)
    return match.group(1) if match else filename.replace("-report.csv", "")
