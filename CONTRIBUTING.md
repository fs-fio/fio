# Contributing to FIO

Thanks for your interest in contributing to FIO — a type-safe, purely functional
effect system for F#. This guide covers how to build, test, and submit changes.

## Prerequisites

- **.NET 10 SDK** (the repository pins the SDK in [`global.json`](global.json))
- F# 10

## Build & test

```bash
dotnet build                          # build all projects
dotnet test                           # run all tests
dotnet test tests/FIO.Tests/          # core tests only
dotnet test --filter "Name~TestName"  # a specific test
```

Benchmarks (Release, BenchmarkDotNet):

```bash
dotnet run -c Release --project benchmarks/FIO.Benchmarks -- --filter "*"
```

Two scripts cover what a single `dotnet` command does not:

```bash
bash scripts/pack.sh             # pack the four packages as the release does, into artifacts/
bash scripts/stress-test.sh 50   # build Release and run the whole suite 50 times, counting failures
```

`stress-test.sh` passes any further arguments to `dotnet test`, so a `--filter` narrows the runs; put
`FIO_RUN_STRESS=1` in front to include the opt-in stress tests.

## Project layout

- `src/` — the four packages: `FIO` (core), `FIO.Http`, `FIO.Sockets`, `FIO.WebSockets`
- `tests/` — one test project per package (Expecto; FsCheck in all but the HTTP tests)
- `benchmarks/` — BenchmarkDotNet suite
- `examples/` — runnable example apps
- `docs/` — the comment style guide (`COMMENT_STYLE.md`)

## Coding conventions

- **4-space indentation.** Match the surrounding style in `src/` and `tests/`.
- **`TreatWarningsAsErrors=true`** (in [`Directory.Build.props`](Directory.Build.props)) — fix every warning.
- **F# compile order matters** — file order in each `.fsproj` is the compile order.
- **Comment style:** two tiers — see [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md).
  Public, user-facing API gets a concise, ZIO-style XML doc comment (`///`);
  internals stay comment-free, with an inline `//` only when the *why* isn't
  obvious. Never restate what the code does, and never commit commented-out code.
- **Package versions** are centralized in
  [`Directory.Packages.props`](Directory.Packages.props) (Central Package
  Management) — add or change versions there, not in individual `.fsproj` files.

## Semantic invariants (do not break)

- Effect construction must be lazy (no side effects until run).
- Sequential operators (`>>=`, `<*>`, `*>`, `<*`) preserve left-to-right order.
- Parallel operators (`<&>`, `&>`, `<&`, `<&&>`) must be genuinely concurrent.
- Interruption propagates consistently across all runtimes.
- `Ensuring` finalizers run on success, error, **and** interruption.
- Extensions must not leak raw exceptions as public errors.

## Pull requests

1. Branch off `main`.
2. Keep changes focused; update tests and examples to match behavior changes.
3. Ensure `dotnet build` and `dotnet test` pass.
4. Use short, descriptive, sentence-style commit messages (e.g. "Fix benchmark output") — no strict prefixes.
5. Open the PR against `main` and fill out the template.

CI runs the tests, stress tests included, in Debug on Ubuntu, Windows, and macOS, and in Release on Ubuntu and
macOS; run `dotnet test -c Release` locally too, since the interpreter's JIT-limit guard runs only there.

## License

By contributing, you agree that your contributions are licensed under the
[MIT License](LICENSE.md).
