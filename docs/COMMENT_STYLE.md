# FIO comment & documentation style

How FIO documents its code. The rule is two-tier:

- **Public, user-facing API** — gets a concise XML doc comment (`///`). This feeds
  IDE tooltips and the `.xml` doc file shipped inside each NuGet package.
- **Everything internal** — stays comment-free. Use an inline `//` only when the
  *why* isn't obvious from the code; never restate what the code does; never
  commit commented-out code.

The voice is modelled on [ZIO](https://zio.dev): short, verb-first, and about the
*effect's behaviour* — not its types or its implementation. One line is the
default. Reach for more only when it genuinely adds information.

This document is also the operating manual for AI agents working in this repo —
see [For AI agents & contributors](#for-ai-agents--contributors).

---

## 1. Principles

1. **One line.** A single `///` summary is the default. If you need a paragraph,
   you're probably explaining the implementation — stop.
2. **Lead with a verb, present tense, third person.** "Creates…", "Returns…",
   "Runs…", "Maps…", "Races…". Not "This function will create…". This is for
   functions and members that do something; types, properties and DU cases take a
   noun phrase ("The outcome of running a fiber…", "The effect this application runs.").
3. **Describe the effect, not the signature.** The types are already on the
   member. Say what running it *does*, not what it returns structurally.
4. **"this effect" voice for members.** On instance members, refer to the
   receiver as "this effect", "this fiber", "this channel".
5. **Don't restate the obvious.** If the parameter is `value: 'A`, don't add
   `<param name="value">The value.</param>`. When the name doesn't tell the
   whole story (e.g. `onError`), fold the clarification into the summary rather
   than adding a `<param>` (§3).
6. **Consistent vocabulary.** effect, fiber, channel, succeed / fail, interrupt,
   finalizer. Use these words the same way everywhere.

---

## 2. Scope — what gets a doc comment

**Document (public, user-facing):**

- Functions in public modules (`FIO.succeed`, `Console.printLine`,
  `SocketClient.connect`, `Codec.json`, `Routes.route`).
- Public members on public types (`FIO.Map`, `Fiber.Join`, `Channel.Read`).
- Public types: DUs, records, classes, and type aliases (`HttpHandler<'E>`).
- Public DU cases and record fields (one short line each; a second only for a caveat, §6).
- The abstract members of `FIOApp` that a user overrides (`effect`, `runtime`,
  `onOutcome`, `onOutcomeTimeout`, `onShutdown`, `onShutdownTimeout`, `mapExitCode`).
- Infix operators.
- The `fio` computation-expression value.

**Do not document:**

- Anything `internal` or `private` — the `FIO<_,_>` node classes and `NodeOps`, `FiberContext`,
  `WorkItem`, the pools, and all runtime internals.
- The computation-expression builders (`FIOBuilder`, `RouteCollector`) and their *methods* (`Bind`, `Return`, `Delay`,
  …). They're infrastructure the user never calls by name. Document the `fio`
  value instead.
- Test, benchmark, and example projects — they ship no XML docs, and their `//`
  budget is fixed: **tests carry none** (a non-obvious choice such as sequencing, a
  held worker or a mutant guard goes into the test's name or its assertion
  message; a shared helper may carry one line when its *why* is invisible);
  **examples carry one line per app and per example function**, nothing on the
  helpers inside; **benchmarks carry one line per benchmark class and per
  workload actor or builder**, nothing on `[<ParamsSource>]` properties, setup, cleanup
  or `Run`.
- `System.Object` overrides (`ToString`, `Finalize`, `Equals`, `GetHashCode`) —
  conventional, left bare uniformly.

When in doubt: if a consumer of the NuGet package can *call it*, document it. If
they can't, leave it bare.

### Why this one has teeth

A `///` on an internal or private item is not merely off-style — F# emits it into the
generated `.xml` doc file that ships **inside the NuGet package**, so it surfaces as
IntelliSense for members no consumer can call. The check is mechanical:

```bash
# after a Release build — every name listed here is a leak
for p in FIO FIO.Sockets FIO.WebSockets FIO.Http; do
  grep -o '<member name="[MPTF]:[^("]*' src/$p/bin/Release/net10.0/$p.xml; done \
  | grep -E "FiberContext|InterpreterCore|WorkItem|ContStack|Scheduler|StdinReader|JoinAllLatch|RequestTracker"
```

Use an inline `//` for these instead: the explanation is usually worth keeping, it is
only the marker that is wrong.

The length rules are mechanical too. Every `///` block of more than two lines, and
every `//` line per area:

```bash
for f in $(find src -name '*.fs' -not -path '*/obj/*' -not -path '*/bin/*'); do
  awk -v f="$f" '/^[[:space:]]*\/\/\//{ if(n==0) s=NR; n++; next } { if(n>2) print f":"s" ("n" lines)"; n=0 }' "$f"; done
for d in src tests examples benchmarks; do
  echo "$d: $(find $d -name '*.fs' -not -path '*/obj/*' -not -path '*/bin/*' -exec grep -hE '^\s*//[^/]' {} + | wc -l)"; done
```

---

## 3. F# XML mechanics

- A bare `///` first line **is** the `<summary>` — you don't need to write the
  tag. Use explicit tags only when you add more than a summary.
- **A bare comment is plain text.** When a comment does not open with a tag, the
  compiler wraps it in `<summary>` and XML-escapes it, so any markup inside it
  (`<c>…</c>`, `&lt;`) reaches the `.xml` file and the tooltip as literal text.
  Tags and entities take effect only in a comment that opens with a tag
  (`/// <summary>…`). So a comment that uses markup opens with `<summary>` and stays one
  line (two with a caveat): `/// <summary>… Operator form of <c>FlatMap</c>.</summary>`.
  Markdown backticks are plain text in every form. Check what ships in
  `src/<Project>/bin/Release/net10.0/<Project>.xml`.
- Tags, in order of usefulness: `<summary>`, `<param name="…">`,
  `<typeparam name="…">`, `<returns>`, `<c>` (inline code), `<example>` /
  `<code>`, `<see cref="…">`, `<remarks>` (rare).
- **Type parameters drop the apostrophe.** F#'s `'A` is referenced as
  `<typeparam name="A">` — *not* `name="'A"`.
- **Cross-reference with `<c>FlatMap</c>`, not `<see cref>`.** A `cref` that
  doesn't resolve is a malformed-doc warning waiting to happen (see below). `<c>`
  never warns, but it renders as code only in a comment that opens with a tag, so a
  comment that uses it opens with `<summary>`; in a bare comment it ships as the literal
  text `<c>FlatMap</c>`.

### The angle-bracket rule (important)

A doc comment that opens with a tag must be **well-formed XML**. FIO's central
type is written `FIO<'A,'E>`, and a raw `<` there starts a tag the XML parser
won't understand. (A bare comment is escaped for you, so its `<` cannot break the
XML — and an entity in it is escaped a second time.) Two ways out, in order of
preference:

1. **Rephrase.** Say "an effect", "a fiber", "a new fiber" instead of writing the
   type. This is almost always cleaner and is the house style.
2. **Escape** — in a comment that opens with a tag. If you must name the type in
   prose, write `FIO&lt;'A,'E&gt;`. Escape `&` as `&amp;` too.

```fsharp
// ✗ malformed XML — '<' opens a tag
/// <summary>Returns a FIO<Fiber<'A,'E>,'E1> that runs this effect concurrently.</summary>

// ✓ rephrased (house style)
/// Returns an effect that runs this effect on a new fiber.
```

### Make the build enforce it

The F# "invalid XML doc" check (warning **3390**) is **off by default**, so a
malformed comment would otherwise fail silently. It is enabled on each of the four
library projects so the build validates every comment that opens with a tag (a bare
comment is escaped and cannot be malformed):

```xml
<!-- in the library .fsproj PropertyGroup -->
<WarnOn>3390</WarnOn>
```

This surfaces malformed XML — and *incomplete* `<param>` sets (see below) — as
warnings on the four library projects, where `GenerateDocumentationFile` is already
enabled. They are promoted to hard build errors by
`<TreatWarningsAsErrors>true</TreatWarningsAsErrors>` in `Directory.Build.props`
(note: a bare `<WarningsAsErrors>true</WarningsAsErrors>` does *not* do this — that
property expects a list of warning numbers, so on its own it's a no-op).

**Param-completeness:** once you add a `<param>` tag to a member, WarnOn 3390 expects a
`<param>` for *every* parameter, or it warns. So the house rule is **all params or
none** — and "none" (fold any clarification into the summary) is preferred.

---

## 4. Patterns by construct

### Factory function

Most factories need only a summary. When a parameter's role isn't obvious from its
name (`onError` is the classic case), fold the clarification into the summary rather
than adding a `<param>` tag — see the param-completeness note above.

```fsharp
/// Creates an effect that always succeeds with the given value.
let succeed<'A, 'E> (value: 'A) : FIO<'A, 'E> = ...

/// Creates an effect that always fails with the given error.
let fail<'A, 'E> (error: 'E) : FIO<'A, 'E> = ...

/// Creates an effect that runs a side-effecting function, mapping any thrown exception to a typed error via onError.
let attempt<'A, 'E> (func: unit -> 'A) (onError: exn -> 'E) : FIO<'A, 'E> = ...

/// Defers construction of an effect until it is run.
let suspend<'A, 'E> (effect: unit -> FIO<'A, 'E>) : FIO<'A, 'E> = ...
```

### Combinator / instance method

Pure ZIO voice — "Returns an effect that …", described as a transformation of
"this effect".

```fsharp
/// Returns an effect that applies the given function to this effect's success value.
member this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> = ...

/// Returns an effect that passes this effect's success value into the given function.
member this.FlatMap<'A1> (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> = ...

/// Returns an effect that runs this effect on a new fiber scoped to this one, yielding its handle.
member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> = ...

/// Returns an effect that recovers from this effect's error with the given handler.
member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> = ...

/// Returns an effect that runs the given finalizer on success, error, and interruption.
member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> = ...
```

### Operator

Document the behaviour in one line and point to the named method with `<c>`, in a comment that opens with `<summary>`.

```fsharp
/// <summary>Passes the first effect's success value into the function. Operator form of <c>FlatMap</c>.</summary>
let inline (>>=) ...

/// <summary>Runs two effects concurrently and pairs their values. Operator form of <c>ZipPar</c>.</summary>
let inline (<&>) ...

/// <summary>Returns the first effect, or the second if the first fails. Operator form of <c>OrElse</c>.</summary>
let inline (<|>) ...
```

### Discriminated union

A summary on the type, then one short line per **public** case.

```fsharp
/// The outcome of running a fiber to completion.
type FiberResult<'A, 'E> =
    /// The fiber completed successfully with a value.
    | Succeeded of value: 'A
    /// The fiber failed with a typed error.
    | Failed of error: 'E
    /// The fiber was interrupted before producing a result.
    | Interrupted of ex: FiberInterruptedException
```

(Same shape for `AppResult`, `HttpError`, `WsError`, `SocketError`.)

### Configuration record

A summary on the type, then one line per field. Field docs go on the line above
the field.

```fsharp
/// Configuration for an HTTP server.
type ServerConfig =
    {
        /// The host or IP address to bind to.
        Host: string
        /// The port to listen on.
        Port: int
        /// The maximum request body size, in bytes.
        MaxRequestBodySize: int64
        /// How long, in milliseconds, a stopping server lets requests in flight finish before it interrupts their
        /// handlers; 0 or less waits indefinitely.
        ShutdownTimeout: int
    }
```

### Class with public members

Summary on the type; document each public member. For `FIOApp`, make clear which
members the user is expected to override.

```fsharp
/// A green thread running an effect. Join, await, interrupt, or poll it.
type Fiber<'A, 'E> = ...
    /// Returns an effect that waits for this fiber and yields its success value.
    member _.Join () : FIO<'A, 'E> = ...
    /// Returns an effect that interrupts this fiber with the given cause and message.
    member _.Interrupt (cause: InterruptionCause) (message: string) : FIO<unit, 'E> = ...

/// <summary>Base class for a FIO application. Override <c>effect</c>; optionally override the rest.</summary>
type FIOApp<'A, 'E>() =
    /// The effect this application runs. Override this.
    abstract member effect : FIO<'A, 'E>
    /// The runtime used to run the effect. Defaults to the recommended runtime.
    abstract member runtime : FIORuntime
    /// An effect run on shutdown, before the process exits. Defaults to no-op.
    abstract member onShutdown : unit -> FIO<unit, 'E>
```

### Type alias

One line on what the alias *means*.

```fsharp
/// A function that turns an HTTP request into an effect producing a response.
type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>
```

### Modules and the `fio` builder

- `[<RequireQualifiedAccess>]` modules (`Console`, `Codec`, `Routes`, …): no
  module-level comment — document the functions inside. A module banner is noise.
- The computation expression: document the `fio` value, not the builder methods.

```fsharp
/// <summary>Builds effects with <c>let!</c>, <c>do!</c>, <c>return</c>, <c>for</c>, <c>while</c>, <c>use</c>,
/// <c>try/with</c> and <c>try/finally</c>, and <c>and!</c>, which runs its effects concurrently.</summary>
let fio = FIOBuilder()
```

---

## 5. Wording cheat-sheet

| Use | For |
|-----|-----|
| `Creates an effect that …` | factory functions (`succeed`, `fail`, `attempt`) |
| `Returns an effect that …` | combinators on an existing effect (`Map`, `CatchAll`, `Race`, `Retry`, `Fold`) |
| `Runs two effects …` | the zip operators, sequential and concurrent (`<*>`, `<*`, `<&>`, `&>`); other operators follow §4 |

**Avoid:**

- "This function / method …" — start with the verb.
- "Gets or sets …" — C# boilerplate; not our voice.
- Restating the signature: "Takes an `'A` and returns a `FIO<'A,'E>`."
- Marketing ("a powerful, blazing-fast …").

---

## 6. Length budget

- **Default:** one `///` summary line. A second line only for a caveat the caller
  must know — interruption, the finalizer guarantee, laziness, thread-safety, a
  destructive default. Never a third: if it does not fit in two, it belongs in
  the README.
- **`//` on internals:** one line; two at most (§7).
- **`<param>`:** avoid it — fold any needed clarification into the summary; if you
  do use one, it is all or none (§3).
- **`<returns>` / `<typeparam>`:** only when they add information the name doesn't
  already convey.
- **`<example>`:** only when correct composition is non-obvious.
- **`<remarks>`:** only for a genuine caveat — laziness, interruption behaviour,
  the finalizer guarantee, or thread-safety. Not for general prose.

---

## 7. Inline comments on internals

Internals are comment-free by default. An inline `//` earns its place only when the
*why* is not visible in the code — an ordering another thread depends on, a
JIT limit, a .NET quirk being worked around, a guarantee the next line exists to
keep. Then:

- **One line; two at most.** The long form — the interleaving, the measurement,
  the history — goes in the commit message, where `git blame` finds it.
- **Why, never what.** A comment that could be replaced by reading the next line
  is deleted, not shortened.
- **Name the pairing, don't narrate the protocol.** For ordering between threads
  say what pairs with what ("read under the list's lock, which Shutdown takes only
  after claiming the flag"), not the sequence of events on each side.
- **No commented-out code**, and no `TODO` without an issue number.

```fsharp
// ✗ three lines that narrate
// A receive the fiber gave up on, through an interruption or a caller's token that is not the fiber's, still
// reads into the rented array and holds the lock, so the clean-up waits for it instead of handing the array
// to the next renter under a pending read.

// ✓ one line that says why
// A receive the fiber abandoned still writes into the array and holds the lock: clean up once it ends.
```

---

## For AI agents & contributors

Everything above applies. Two rules are easy to miss:

1. **Keep comments in sync with behaviour.** If you change what an effect does,
   update its summary in the same change.
2. **Don't "tidy" by adding comments the existing code deliberately omits**, and
   don't bulk-add `///` docs to internals, tests, or examples. Before finishing, run
   the two checks in §2.
