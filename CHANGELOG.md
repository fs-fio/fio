# Changelog

All notable changes to the FIO packages are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
FIO follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html); while the
project is pre-1.0 (`0.x`), any release may contain breaking changes.

## Versioning policy

The four packages — **FIO**, **FIO.Http**, **FIO.Sockets**, **FIO.WebSockets** —
are versioned in **lockstep**: they share a single version and are always
released together. The shared version lives in `Directory.Build.props`; at
release time it is overridden by the git tag (`vX.Y.Z` → `X.Y.Z`), so the tag is
the authoritative published version. See
[`docs/versioning-strategy.md`](docs/versioning-strategy.md) for the rationale
and the migration path to independent versioning.

## [Unreleased]

### Changed
- Centralized the package version, shared NuGet metadata, and the `FSharp.Core`
  pin into `Directory.Build.props` / `Directory.Build.targets` (single source of
  truth; removes per-project duplication).
- The publish workflow now derives the published package version from the git
  tag, so a tag and the published version can no longer diverge.
- Internal naming consistency pass across the core (`src/FIO`): runtime worker
  config fields renamed to `EvaluationWorkers` / `EvaluationSteps` /
  `BlockingWorkers`; assorted internal identifiers clarified. No public
  behavioral change.

### Added
- Per-package (independent) release support alongside lockstep. A
  `<pkg>-v*` tag (`core-v*`, `http-v*`, `sockets-v*`, `websockets-v*`) publishes
  that single package; an extension's `FIO` dependency stays pinned to the core
  baseline version. Plain `v*` tags still release all four in lockstep. See
  [`docs/versioning-strategy.md`](docs/versioning-strategy.md) §0/§8.

## [0.0.40-alpha] and earlier

Released as git tags (`v0.0.36-alpha` … `v0.0.40-alpha`). These predate this
changelog; see the GitHub releases and commit history for details.

[Unreleased]: https://github.com/fs-fio/fio/compare/v0.0.40-alpha...HEAD
[0.0.40-alpha]: https://github.com/fs-fio/fio/releases/tag/v0.0.40-alpha
