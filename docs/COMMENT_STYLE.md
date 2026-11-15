# FIO comment style

Two tiers:

- **Public, user-facing API** gets a concise XML doc comment (`///`). It feeds IDE tooltips and the `.xml` file in each
  NuGet package.
- **Internals** stay comment-free. An inline `//` only when the *why* isn't visible in the code; never restate the code;
  no commented-out code.

The voice is [ZIO](https://zio.dev)'s: short, verb-first, about what the effect does.

## Principles

1. **One line** by default. A second only for a caveat the caller must know (interruption, finalizers, laziness,
   thread-safety); never a third — cut it instead.
2. **Lead with a verb**, present tense: "Creates…", "Returns…", "Runs…". Types, properties and DU cases take a noun
   phrase.
3. **Describe the effect, not the signature.** The types are already on the member.
4. **"this effect" voice** on instance members: "this effect", "this fiber", "this channel".
5. **Don't restate the obvious.** Fold a needed clarification (e.g. `onError`) into the summary instead of a `<param>`.
6. **Consistent vocabulary:** effect, fiber, channel, succeed/fail, interrupt, finalizer.

## Scope

**Document** whatever a package consumer can call: functions in public modules, public members and types, public DU
cases and record fields, `FIOApp`'s overridable members, operators, and the `fio` value.

**Don't document:**

- anything `internal` or `private` (node classes, `NodeOps`, `FiberContext`, runtime internals). F# ships a `///` on
  them in the package's `.xml` file, where it shows up as IntelliSense for members no one can call;
- computation-expression builders and their methods (document the `fio` value instead);
- `System.Object` overrides.

Tests, examples and benchmarks ship no XML docs. Tests carry no `//` (put the *why* in the test name or the assertion
message); examples get one line per app and per example function; benchmarks get one line per benchmark class and per
workload actor or builder.

## XML mechanics

- A bare `///` comment is plain text: the compiler wraps it in `<summary>` and escapes it, so markup in it ships
  literally. A comment that uses markup (`<c>…</c>`, entities) opens with `<summary>`.
- A comment that opens with a tag must be well-formed XML. Don't write `FIO<'A,'E>` in it: say "an effect" (house
  style), or escape it as `FIO&lt;'A,'E&gt;`.
- `WarnOn 3390` (set on the four libraries, with `TreatWarningsAsErrors`) fails the build on malformed doc XML and on
  incomplete `<param>` sets, so use `<param>` for every parameter or none (prefer none).
- Type parameters drop the apostrophe: `<typeparam name="A">`.
- Cross-reference with `<c>FlatMap</c>`, not `<see cref>`, which warns when it doesn't resolve.

```fsharp
/// Creates an effect that runs a side-effecting function, mapping any thrown exception to a typed error via onError.
let attempt<'A, 'E> (func: unit -> 'A) (onError: exn -> 'E) : FIO<'A, 'E> = ...

/// Returns an effect that runs the given finalizer on success, error, and interruption.
member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> = ...

/// <summary>Runs two effects concurrently and pairs their values. Operator form of <c>ZipPar</c>.</summary>
let inline (<&>) ...

/// The outcome of running a fiber to completion.
type FiberResult<'A, 'E> =
    /// The fiber completed successfully with a value.
    | Succeeded of value: 'A
```

## Inline comments on internals

An inline `//` earns its place only for a *why* the code can't show: an ordering another thread depends on, a JIT limit,
a .NET quirk, a guarantee the next line keeps.

- One line, two at most; the long story goes in the commit message.
- Name the pairing ("read under the list's lock, which Shutdown takes after claiming the flag"), don't narrate the
  protocol.
- No `TODO` without an issue number.

## Checks

Run both before finishing. After a Release build, every name this prints is an internal that leaked into the docs:

```bash
for p in FIO FIO.Sockets FIO.WebSockets FIO.Http; do
  grep -o '<member name="[MPTF]:[^("]*' src/$p/bin/Release/*/$p.xml; done \
  | grep -E "FiberContext|InterpreterCore|WorkItem|ContStack|Scheduler|StdinReader|JoinAllLatch|RequestTracker"
```

Every `///` block over two lines, and the `//` count per area:

```bash
for f in $(find src -name '*.fs' -not -path '*/obj/*' -not -path '*/bin/*'); do
  awk -v f="$f" '/^[[:space:]]*\/\/\//{ if(n==0) s=NR; n++; next } { if(n>2) print f":"s" ("n" lines)"; n=0 }' "$f"; done
for d in src tests examples benchmarks; do
  echo "$d: $(find $d -name '*.fs' -not -path '*/obj/*' -not -path '*/bin/*' -exec grep -hE '^\s*//[^/]' {} + | wc -l)"; done
```

When you change what an effect does, update its summary in the same change, and don't bulk-add comments the code
deliberately omits.
