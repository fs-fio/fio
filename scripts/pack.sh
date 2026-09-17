#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

ARTIFACTS_DIR="$REPO_ROOT/artifacts"

rm -rf "$ARTIFACTS_DIR"
mkdir -p "$ARTIFACTS_DIR"

echo "==> Packing FIO"

dotnet pack \
    -c Release \
    -o "$ARTIFACTS_DIR"

echo
echo "Packages:"
find "$ARTIFACTS_DIR" -maxdepth 1 -type f -name '*.nupkg' -print
