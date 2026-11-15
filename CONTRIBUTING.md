# Contributing to FIO

Thanks for your interest in contributing to FIO. This guide covers how to build, test, and submit changes.

## Prerequisites

- The .NET SDK pinned in [`global.json`](global.json) (it brings the F# compiler)

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

Packing, and soaking the suite to catch rare failures:

```bash
dotnet pack -c Release -p:StripInternals=true -o artifacts   # pack the four packages as the release does
dotnet build -c Release
for i in $(seq 50); do dotnet test -c Release --no-build || echo "run $i failed"; done
```

The loop runs the whole Release suite 50 times and names each failed run, as the nightly `soak.yml` does for the core
suite. Add a `--filter` to `dotnet test` to narrow the runs, and put `FIO_RUN_STRESS=1` in front of it to include the
opt-in stress tests.

## Project layout

- `src/` — the four packages: `FIO` (core), `FIO.Http`, `FIO.Sockets`, `FIO.WebSockets`
- `tests/` — one test project per package (Expecto; FsCheck in all but the HTTP tests)
- `benchmarks/` — BenchmarkDotNet suite
- `examples/` — runnable example apps
- `docs/` — the comment style guide (`COMMENT_STYLE.md`)

## Coding conventions

- **Formatting is Fantomas's.** Run `dotnet tool restore` once, then `dotnet fantomas .` before you commit; CI
  checks it with `dotnet fantomas --check .`. Naming and idiom still follow the surrounding code in `src/` and
  `tests/`.
- **`TreatWarningsAsErrors=true`** (in [`Directory.Build.props`](Directory.Build.props)) — fix every warning.
- **F# compile order matters** — file order in each `.fsproj` is the compile order.
- **Comments:** a one-line `///` on public API, none on internals — see [`docs/COMMENT_STYLE.md`](docs/COMMENT_STYLE.md).
- **Package versions** live in [`Directory.Packages.props`](Directory.Packages.props), not in the `.fsproj` files.

## Semantic invariants (do not break)

The runtime's guarantees are listed once, in [CLAUDE.md](CLAUDE.md#semantic-invariants-do-not-break). A change that
breaks one is not a refactor.

## Pull requests

1. Branch off `main`.
2. Keep changes focused; update tests and examples to match behavior changes.
3. Ensure `dotnet build` and `dotnet test` pass, and `dotnet fantomas --check .` finds nothing to format.
4. Use short, descriptive, sentence-style commit messages (e.g. "Fix benchmark output") — no strict prefixes.
5. Open the PR against `main` and fill out the template.

CI also runs the tests in Release, so run `dotnet test -c Release` locally too: the interpreter's JIT-limit guard runs
only there.

## License

By contributing, you agree that your contributions are licensed under the
[MIT License](LICENSE.md).
