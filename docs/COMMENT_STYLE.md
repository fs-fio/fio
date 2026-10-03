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
   "Runs…", "Maps…", "Races…". Not "This function will create…".
3. **Describe the effect, not the signature.** The types are already on the
   member. Say what running it *does*, not what it returns structurally.
4. **"this effect" voice for members.** On instance members, refer to the
   receiver as "this effect", "this fiber", "this channel".
5. **Don't restate the obvious.** If the parameter is `value: 'A`, don't add
   `<param name="value">The value.</param>`. Add a `<param>` only when the name
   doesn't already tell the whole story (e.g. `onError`).
6. **Consistent vocabulary.** effect, fiber, channel, succeed / fail, interrupt,
   finalizer. Use these words the same way everywhere.

---

## 2. Scope — what gets a doc comment

**Document (public, user-facing):**

- Functions in public modules (`FIO.succeed`, `Console.printLine`,
  `SocketClient.connect`, `Codec.json`, `Routes.route`).
- Public members on public types (`FIO.Map`, `Fiber.Join`, `Channel.Read`).
- Public types: DUs, records, classes, and type aliases (`HttpHandler<'E>`).
- Public DU cases and record fields (one short line each).
- The abstract members of `FIOApp` that a user overrides (`effect`, `runtime`,
  `onShutdown`, `onShutdownTimeout`, `mapExitCode`).
- Infix operators.
- The `fio` computation-expression value.

**Do not document:**

- Anything `internal` or `private` — the `FIO<_,_>` DU cases, `FiberContext`,
  `WorkItem`, the pools, and all runtime internals.
- The `FIOBuilder` computation-expression *methods* (`Bind`, `Return`, `Delay`,
  …). They're infrastructure the user never calls by name. Document the `fio`
  value instead.
- Test, benchmark, and example projects — they ship no XML docs, and their `//`
  budget is fixed: **tests carry none** (a non-obvious choice such as sequencing, a
  held worker or a mutant guard goes into the test's name or its assertion
  message; a shared helper may carry one line when its *why* is invisible);
  **examples carry one line per app and per example function**, nothing on the
  helpers inside; **benchmarks carry one line per benchmark class and per
  workload actor or builder**, nothing on `[<Params>]` properties, setup, cleanup
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
grep -o '<member name="[MPTF]:[^("]*' src/FIO/bin/Release/net10.0/FIO.xml \
  | grep -E "FiberContext|InterpreterCore|WorkItem|ContStack"
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
- Tags, in order of usefulness: `<summary>`, `<param name="…">`,
  `<typeparam name="…">`, `<returns>`, `<c>` (inline code), `<example>` /
  `<code>`, `<see cref="…">`, `<remarks>` (rare).
- **Type parameters drop the apostrophe.** F#'s `'A` is referenced as
  `<typeparam name="A">` — *not* `name="'A"`.
- **Cross-reference with `<c>FlatMap</c>`, not `<see cref>`.** A `cref` that
  doesn't resolve is a malformed-doc warning waiting to happen (see below). Plain
  `<c>` inline code is always safe.

### The angle-bracket rule (important)

Doc comments must be **well-formed XML**. FIO's central type is written
`FIO<'A,'E>`, and a raw `<` starts a tag the XML parser won't understand. Two
ways out, in order of preference:

1. **Rephrase.** Say "an effect", "a fiber", "a new fiber" instead of writing the
   type. This is almost always cleaner and is the house style.
2. **Escape.** If you must name the type in prose, write `FIO&lt;'A,'E&gt;`.
   Escape `&` as `&amp;` too.

```fsharp
// ✗ malformed XML — '<' opens a tag
/// Returns a FIO<Fiber<'A,'E>,'E1> that runs this effect concurrently.

// ✓ rephrased (house style)
/// Returns an effect that runs this effect on a new fiber.
```

### Make the build enforce it

The F# "invalid XML doc" check (warning **3390**) is **off by default**, so a
malformed comment would otherwise fail silently. It is enabled on each of the four
library projects so the build validates every comment:

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

/// Returns an effect that runs this effect on a new fiber, yielding its handle.
member this.Fork<'E1> () : FIO<Fiber<'A, 'E>, 'E1> = ...

/// Returns an effect that recovers from this effect's error with the given handler.
member this.CatchAll<'E1> (onError: 'E -> FIO<'A, 'E1>) : FIO<'A, 'E1> = ...

/// Returns an effect that runs the given finalizer on success, error, and interruption.
member this.Ensuring (finalizer: FIO<unit, 'E>) : FIO<'A, 'E> = ...
```

### Operator

Document the behaviour in one line and point to the named method with `<c>`.

```fsharp
/// Sequentially composes two effects, passing the first's success value into the
/// function. Operator form of <c>FlatMap</c>.
let inline (>>=) ...

/// Runs two effects concurrently and pairs their success values. Operator form of <c>ZipPar</c>.
let inline (<&>) ...

/// Returns the first effect, or the second if the first fails. Operator form of <c>OrElse</c>.
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
/// Tunable limits and timeouts for a WebSocket connection.
type WebSocketConfig =
    {
        /// Buffer size, in bytes, for receiving message frames.
        ReceiveBufferSize: int
        /// Buffer size, in bytes, for sending message frames.
        SendBufferSize: int
        /// Largest message, in bytes, that may be received before the connection is closed.
        MaxMessageSize: int64
        /// Send timeout, in milliseconds.
        SendTimeout: int
        /// Receive timeout, in milliseconds.
        ReceiveTimeout: int
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

/// Base class for a FIO application. Override `effect`; optionally override the rest.
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
/// Builds effects with `let!`, `do!`, `return`, `for`, `while`, `use`, and `try/with`/`try/finally`.
let fio = FIOBuilder()
```

---

## 5. Wording cheat-sheet

| Use | For |
|-----|-----|
| `Creates an effect that …` | factory functions (`succeed`, `fail`, `attempt`) |
| `Returns an effect that …` | combinators on an existing effect (`Map`, `CatchAll`) |
| `Runs …` / `Runs this effect …` | execution / forking |
| `Maps …` | value/error transformations |
| `Races …` / `Retries …` / `Folds …` | the matching combinator |

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
- **`<param>`:** avoid it — fold any needed clarification into the summary. If you do
  use one, you must document *every* parameter (param-completeness, §3), so it's all
  or none.
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
// ✗ four lines that narrate
// A receive the fiber gave up on, through an interruption or a caller's token that is not the fiber's, still
// reads into the rented array and holds the lock, so the clean-up waits for it instead of handing the array
// to the next renter under a pending read.

// ✓ one line that says why
// A receive the fiber abandoned still writes into the array and holds the lock: clean up once it ends.
```

---

## For AI agents & contributors

When you touch the public API, follow these rules:

1. **Add/update a `///` summary on any public member you add or change**, in the
   verb-first "this effect" voice above. Keep it to one line unless more is
   genuinely needed.
2. **Never document internal or private items**, and never document the
   `FIOBuilder` methods. If it isn't callable from a referencing package, leave
   it bare.
3. **Never restate the signature.** Prefer a summary with no `<param>` tags; if a
   parameter needs explaining, work it into the summary. Adding one `<param>` forces
   documenting all of them (§3).
4. **Keep comments in sync with behaviour.** If you change what an effect does,
   update its summary in the same change.
5. **Keep the XML well-formed.** Rephrase types out of prose ("an effect"), or
   escape (`FIO&lt;'A,'E&gt;`, `&amp;`). Cross-reference with `<c>Name</c>`, not
   `<see cref>`. `dotnet build` must stay green.
6. **Don't bulk-add `///` API docs to internals, tests, or examples**, and don't
   "tidy" by adding comments the existing code deliberately omits. Tests carry no
   comments; examples one line per app and example function; benchmarks one line
   per class and per workload actor (§2). Internals: `//` only for an invisible
   why, one line, two at most (§7). Before finishing, run the two checks in §3.
7. **Explain in the commit message, not the code.** A fix that needed a paragraph
   to justify gets that paragraph in its commit; the code gets one line naming the
   constraint.
