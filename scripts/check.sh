#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

echo "==> Restoring"
dotnet restore

# Fantomas
# echo "==> Checking formatting"
# dotnet format --verify-no-changes

echo "==> Building"
dotnet build -c Release --no-restore

echo "==> Running tests"
dotnet test -c Release --no-build

echo "✅ All checks passed"
