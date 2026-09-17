#!/usr/bin/env bash

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

if [[ $# -lt 1 || ! "$1" =~ ^[0-9]+$ || "$1" -lt 1 ]]; then
    echo "Usage: $0 <number-of-runs> [dotnet test arguments...]"
    echo "Example: $0 1000"
    exit 1
fi

RUNS=$1
shift

PASSED=0
FAILED=0

echo "==> Building Release"
dotnet build -c Release

echo
echo "==> Running tests $RUNS times"
echo

for ((i = 1; i <= RUNS; i++)); do
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Run $i/$RUNS"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    if dotnet test -c Release --no-build "$@"; then
        ((PASSED++))
        echo "✅ Passed"
    else
        ((FAILED++))
        echo "❌ Failed"
    fi

    echo
done

echo "=========================================="
echo "Stress test complete"
echo "Runs:    $RUNS"
echo "Passed:  $PASSED"
echo "Failed:  $FAILED"
echo "=========================================="

[[ $FAILED -eq 0 ]]
