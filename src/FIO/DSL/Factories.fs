/// <summary>Provides factory functions for creating FIO effects.</summary>
[<AutoOpen>]
module FIO.DSL.Factories

open System
open System.Threading
open System.Threading.Tasks

/// <summary>Provides factory functions for creating FIO effects, accessed as <c>FIO.succeed</c>, <c>FIO.fail</c>, and so on.</summary>
[<RequireQualifiedAccess>]
module FIO =

    /// <summary>Lifts unit into an effect that completes successfully with no value.</summary>
    /// <returns>An effect that completes immediately with unit on the success channel.</returns>
    let unit<'E>() : FIO<unit, 'E> =
        Success()

    /// <summary>Lifts a value into an effect that completes with that value as its success result.</summary>
    /// <param name="res">The value to wrap as a successful result.</param>
    /// <returns>An effect that completes immediately with <paramref name="res"/> on the success channel.</returns>
    let succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        Success res

    /// <summary>Lifts a typed error into an effect that fails with that error.</summary>
    /// <param name="err">The error value to fail with.</param>
    /// <returns>An effect that fails immediately with <paramref name="err"/> on the error channel.</returns>
    let fail<'R, 'E> (err: 'E) : FIO<'R, 'E> =
        Failure err

    /// <summary>Creates an effect that interrupts the current fiber when interpreted.</summary>
    /// <param name="cause">The reason for the interruption.</param>
    /// <param name="msg">A human-readable description of the interruption.</param>
    /// <returns>An effect that interrupts the current fiber when evaluated.</returns>
    let interrupt<'R, 'E> (cause: InterruptionCause) (msg: string) : FIO<'R, 'E> =
        Interrupt(cause, msg)

    /// <summary>Lifts a side-effecting thunk into an effect that runs the thunk when interpreted.</summary>
    /// <param name="func">A thunk that produces the success value when invoked at evaluation time.</param>
    /// <param name="onError">A function that maps any exception thrown by <paramref name="func"/> to the typed error.</param>
    /// <returns>An effect that invokes <paramref name="func"/> when evaluated and either succeeds with its result or fails through <paramref name="onError"/>.</returns>
    let attempt<'R, 'E> (func: unit -> 'R) (onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.Action(func, onError)

    /// <summary>Lifts a <c>Result</c> into an effect, succeeding for <c>Ok</c> and failing for <c>Error</c>.</summary>
    /// <param name="res">The result value to convert.</param>
    /// <returns>An effect that completes with the <c>Ok</c> value or fails with the <c>Error</c> value.</returns>
    let inline fromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> succeed res
        | Error err -> fail err

    /// <summary>Lifts an <c>Option</c> into an effect, failing through a supplied function when the option is <c>None</c>.</summary>
    /// <param name="opt">The option value to convert.</param>
    /// <param name="onNone">A function producing the typed error when <paramref name="opt"/> is <c>None</c>.</param>
    /// <returns>An effect that completes with the <c>Some</c> value or fails with the result of <paramref name="onNone"/>.</returns>
    let inline fromOption<'R, 'E> (opt: Option<'R>) (onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> succeed res
        | None -> fail (onNone ())

    /// <summary>Lifts a <c>Choice</c> into an effect, succeeding for <c>Choice1Of2</c> and failing for <c>Choice2Of2</c>.</summary>
    /// <param name="choice">The choice value to convert.</param>
    /// <returns>An effect that completes with <c>Choice1Of2</c> or fails with <c>Choice2Of2</c>.</returns>
    let inline fromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> succeed res
        | Choice2Of2 err -> fail err

    /// <summary>Builds an effect whose construction is deferred until the effect is interpreted.</summary>
    /// <param name="eff">A factory function producing the effect to evaluate.</param>
    /// <returns>An effect that calls <paramref name="eff"/> at evaluation time and runs the effect it produces.</returns>
    /// <remarks>Use to wrap construction-time side effects so they only run when the resulting effect is interpreted.</remarks>
    let suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> =
        unit().FlatMap(fun () -> eff())

    /// <summary>Builds an effect that produces the running fiber's <c>CancellationToken</c>.</summary>
    /// <returns>An effect that completes with the cancellation token of the fiber evaluating it; falls back to <c>CancellationToken.None</c> when no fiber context is available.</returns>
    /// <remarks>The returned token is cancelled when the running fiber is interrupted. Pass it to .NET cancellation-aware APIs to make I/O cooperate with FIO interruption.</remarks>
    let cancellationToken<'E>() : FIO<CancellationToken, 'E> =
        FiberCancellationToken

    /// <summary>Builds an effect that awaits a <c>Task</c> that returns no value and completes with unit.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with unit when the task finishes successfully or fails through <paramref name="onError"/>.</returns>
    let awaitUnitTask<'E> (task: Task) (onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTask(wrapVoidTask task, onError)

    /// <summary>Builds an effect that awaits a <c>Task&lt;'R&gt;</c> and completes with its result.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with the task's result or fails through <paramref name="onError"/>.</returns>
    let awaitTask<'R, 'E> (task: Task<'R>) (onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitTask(upcastTask task, onError)

    /// <summary>Builds an effect that awaits an <c>Async</c> computation and completes with its result.</summary>
    /// <param name="async">The async workflow to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the async workflow to the typed error.</param>
    /// <returns>An effect that starts the async workflow at evaluation time and completes with its result.</returns>
    let inline awaitAsync<'R, 'E> (async: Async<'R>) (onError: exn -> 'E) : FIO<'R, 'E> =
        cancellationToken().FlatMap(fun ct ->
            awaitTask (Async.StartAsTask(async, cancellationToken = ct)) onError)

    /// <summary>Forks a new fiber that runs a lazily-evaluated <c>Task</c> that returns no value concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let forkUnitTask<'E> (taskFactory: unit -> Task) (onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        suspend (fun () ->
            (awaitUnitTask (taskFactory ()) onError).Fork())

    /// <summary>Forks a new fiber that runs a lazily-evaluated <c>Task&lt;'R&gt;</c> concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let forkTask<'R, 'E> (taskFactory: unit -> Task<'R>) (onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        suspend (fun () ->
            (awaitTask (taskFactory ()) onError).Fork())

    /// <summary>Lifts a callback-based asynchronous operation into an effect by exposing a completion callback to the supplied registration function.</summary>
    /// <param name="register">A function that receives a completion callback; it must arrange for the callback to be invoked with the operation's outcome. A synchronous exception thrown by <paramref name="register"/> is routed through <paramref name="onError"/> instead of deadlocking the fiber.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying completion source to the typed error.</param>
    /// <returns>An effect that suspends until the registered callback fires, then completes with its result.</returns>
    /// <remarks>The canonical adapter for callback-based APIs (event handlers, native interop). The registration callback uses <c>TrySetResult</c> internally and is safe to invoke multiple times — only the first call takes effect. When the running fiber is interrupted while awaiting the callback, the await is cancelled and the fiber surfaces as interrupted; the user-side subscription installed by <paramref name="register"/> is not torn down automatically — callers that allocate long-lived resources should observe <c>cancellationToken()</c> themselves to release them.</remarks>
    let async<'R, 'E> (register: (Result<'R, 'E> -> unit) -> unit) (onError: exn -> 'E) : FIO<'R, 'E> =
        cancellationToken().FlatMap(fun ct ->
            suspend (fun () ->
                let tcs = TaskCompletionSource<Result<'R, 'E>>()
                let registration = ct.Register(fun () -> tcs.TrySetCanceled ct |> ignore)
                try
                    register (fun result ->
                        if tcs.TrySetResult result then
                            registration.Dispose())
                with ex ->
                    tcs.TrySetException ex |> ignore
                    registration.Dispose()
                (awaitTask tcs.Task onError).FlatMap fromResult))

    /// <summary>Builds an effect that completes with unit after the specified delay.</summary>
    /// <param name="duration">The amount of time to wait before completing.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with unit once <paramref name="duration"/> has elapsed.</returns>
    let sleep<'E> (duration: TimeSpan) (onError: exn -> 'E) : FIO<unit, 'E> =
        cancellationToken().FlatMap(fun ct ->
            awaitUnitTask (Task.Delay(duration, ct)) onError)

    /// <summary>Builds an effect that hints to the scheduler to re-check the run queue, yielding cooperatively to other ready fibers.</summary>
    /// <param name="onError">A function that maps an exception thrown by the underlying yield to the typed error.</param>
    /// <returns>An effect that completes with unit after yielding.</returns>
    /// <remarks>Useful in tight CPU-bound loops on <c>ConcurrentRuntime</c> to prevent peer fibers from being starved between scheduler pre-emption points.</remarks>
    let yieldNow<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        sleep TimeSpan.Zero onError

    /// <summary>Creates an effect that never completes on its own.</summary>
    /// <returns>An effect that suspends the fiber indefinitely; the fiber must be interrupted to terminate.</returns>
    let never<'R, 'E>() : FIO<'R, 'E> =
        suspend (fun () ->
            let chan = new Channel<'R>()
            chan.Receive())

    /// <summary>Builds a resource-managed effect that acquires, uses, and releases a resource with cleanup guaranteed on every outcome.</summary>
    /// <param name="acquire">An effect that produces the resource.</param>
    /// <param name="release">A function from the acquired resource to a cleanup effect that runs on success, failure, and interruption.</param>
    /// <param name="useResource">A function from the acquired resource to the use-effect whose result is propagated to the caller.</param>
    /// <returns>An effect that completes with the use-effect's result and always runs the cleanup effect afterwards.</returns>
    /// <remarks>The cleanup is wired through <c>Ensuring</c>, so it cannot be skipped by failure or interruption of the use-effect.</remarks>
    let inline acquireRelease<'A, 'R, 'E> (acquire: FIO<'A, 'E>) (release: 'A -> FIO<unit, 'E>) (useResource: 'A -> FIO<'R, 'E>) : FIO<'R, 'E> =
        acquire.FlatMap(fun resource ->
            (useResource resource)
                .Ensuring(release resource))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each in order and collects the results in source order.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence yields an empty result list without invoking <paramref name="func"/>.</param>
    /// <param name="func">A function from each input to the effect that produces its result; the next effect runs only after the previous one succeeds.</param>
    /// <returns>An effect that completes with the list of per-input results in the order their inputs appeared in <paramref name="items"/>, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the traversal and no further inputs are processed.</remarks>
    let forEach<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray items
            let rec loop i acc =
                if i >= arr.Length then
                    succeed (List.rev acc)
                else
                    (func arr.[i]).FlatMap(fun res ->
                        suspend (fun () -> loop (i + 1) (res :: acc)))
            loop 0 [])

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each in order and discards the results.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence completes with unit without invoking <paramref name="func"/>.</param>
    /// <param name="func">A function from each input to the effect that processes it; the next effect runs only after the previous one succeeds.</param>
    /// <returns>An effect that completes with unit once every input has been processed, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the traversal and no further inputs are processed. Prefer this over <c>forEach</c> when the per-input results are not needed.</remarks>
    let forEachDiscard<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<unit, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray items
            let rec loop i =
                if i >= arr.Length then
                    unit ()
                else
                    (func arr.[i]).FlatMap(fun _ ->
                        suspend (fun () -> loop (i + 1)))
            loop 0)

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each concurrently and collects the results in source order.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence yields an empty result list without forking.</param>
    /// <param name="func">A function from each input to the effect that produces its result; each invocation runs on its own fiber.</param>
    /// <returns>An effect that completes with the list of per-input results in the order their inputs appeared in <paramref name="items"/>, or fails with the first error observed among the forked fibers.</returns>
    /// <remarks>Fail-fast: when any forked fiber fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates.</remarks>
    let forEachPar<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray items

            let rec forkAll i (forked: Fiber<'R, 'E> list) : FIO<Fiber<'R, 'E> list, 'E> =
                if i >= arr.Length then
                    succeed (List.rev forked)
                else
                    (func arr.[i]).Fork().FlatMap(fun fiber ->
                        suspend (fun () -> forkAll (i + 1) (fiber :: forked)))

            let inline interruptOne (fiber: Fiber<'R, 'E>) : FIO<unit, 'E> =
                fiber.Interrupt(ExplicitInterrupt, "forEachPar peer failed")
                    .CatchAll(fun _ -> unit ())

            let rec interruptAll (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E> =
                match fibers with
                | [] -> unit ()
                | fiber :: rest ->
                    (interruptOne fiber).FlatMap(fun () ->
                        suspend (fun () -> interruptAll rest))

            let rec joinAll (fibers: Fiber<'R, 'E> list) (acc: 'R list) : FIO<'R list, 'E> =
                match fibers with
                | [] -> succeed (List.rev acc)
                | fiber :: rest ->
                    (fiber.Join().FlatMap(fun res -> suspend (fun () ->
                        joinAll rest (res :: acc))))
                        .CatchAll(fun err ->
                            (interruptAll rest).FlatMap(fun () -> fail err))

            (forkAll 0 []).FlatMap(fun fibers -> joinAll fibers []))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each concurrently and discards the results.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence completes with unit without forking.</param>
    /// <param name="func">A function from each input to the effect that processes it; each invocation runs on its own fiber.</param>
    /// <returns>An effect that completes with unit once every forked fiber has finished, or fails with the first error observed among them.</returns>
    /// <remarks>Fail-fast: when any forked fiber fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates. Prefer this over <c>forEachPar</c> when the per-input results are not needed.</remarks>
    let forEachParDiscard<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<unit, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray items

            let rec forkAll i (forked: Fiber<'R, 'E> list) : FIO<Fiber<'R, 'E> list, 'E> =
                if i >= arr.Length then
                    succeed (List.rev forked)
                else
                    (func arr.[i]).Fork().FlatMap(fun fiber ->
                        suspend (fun () -> forkAll (i + 1) (fiber :: forked)))

            let inline interruptOne (fiber: Fiber<'R, 'E>) : FIO<unit, 'E> =
                fiber.Interrupt(ExplicitInterrupt, "forEachParDiscard peer failed")
                    .CatchAll(fun _ -> unit ())

            let rec interruptAll (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E> =
                match fibers with
                | [] -> unit ()
                | fiber :: rest ->
                    (interruptOne fiber).FlatMap(fun () ->
                        suspend (fun () -> interruptAll rest))

            let rec joinAll (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E> =
                match fibers with
                | [] -> unit ()
                | fiber :: rest ->
                    (fiber.Join().FlatMap(fun _ ->
                        suspend (fun () -> joinAll rest)))
                        .CatchAll(fun err ->
                            (interruptAll rest).FlatMap(fun () -> fail err))

            (forkAll 0 []).FlatMap joinAll)

    /// <summary>Combines a sequence of effects into one effect that runs them in order and collects their results in source order.</summary>
    /// <param name="effects">The effects to run sequentially; an empty sequence yields an empty result list.</param>
    /// <returns>An effect that completes with the list of results in the order their effects appeared in <paramref name="effects"/>, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the traversal and no further effects are evaluated.</remarks>
    let collectAll<'R, 'E> (effects: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        forEach effects id

    /// <summary>Combines a sequence of effects into one effect that runs them in order and discards their results.</summary>
    /// <param name="effects">The effects to run sequentially; an empty sequence completes with unit.</param>
    /// <returns>An effect that completes with unit once every effect has finished, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the traversal and no further effects are evaluated. Prefer this over <c>collectAll</c> when the results are not needed.</remarks>
    let collectAllDiscard<'R, 'E> (effects: seq<FIO<'R, 'E>>) : FIO<unit, 'E> =
        forEachDiscard effects id

    /// <summary>Combines a sequence of effects into one effect that runs them concurrently and collects their results in source order.</summary>
    /// <param name="effects">The effects to run in parallel; an empty sequence yields an empty result list without forking.</param>
    /// <returns>An effect that completes with the list of results in the order their effects appeared in <paramref name="effects"/>, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates.</remarks>
    let collectAllPar<'R, 'E> (effects: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        forEachPar effects id

    /// <summary>Combines a sequence of effects into one effect that runs them concurrently and discards their results.</summary>
    /// <param name="effects">The effects to run in parallel; an empty sequence completes with unit without forking.</param>
    /// <returns>An effect that completes with unit once every forked fiber has finished, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates. Prefer this over <c>collectAllPar</c> when the results are not needed.</remarks>
    let collectAllParDiscard<'R, 'E> (effects: seq<FIO<'R, 'E>>) : FIO<unit, 'E> =
        forEachParDiscard effects id

    /// <summary>Builds an effect that runs the given effect a fixed number of times in order and collects the results in iteration order.</summary>
    /// <param name="n">The number of repetitions; values less than or equal to zero yield an empty result list without evaluating <paramref name="eff"/>.</param>
    /// <param name="eff">The effect to repeat; the next iteration runs only after the previous one succeeds.</param>
    /// <returns>An effect that completes with the list of per-iteration results, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the repetition and no further iterations are evaluated. Use <c>RepeatN</c> when only the final result is needed.</remarks>
    let replicate<'R, 'E> (n: int) (eff: FIO<'R, 'E>) : FIO<'R list, 'E> =
        if n <= 0 then
            succeed []
        else
            forEach (seq { 1 .. n }) (fun _ -> eff)

    /// <summary>Builds an effect that runs the given effect a fixed number of times in order and discards each result.</summary>
    /// <param name="n">The number of repetitions; values less than or equal to zero complete with unit without evaluating <paramref name="eff"/>.</param>
    /// <param name="eff">The effect to repeat; the next iteration runs only after the previous one succeeds.</param>
    /// <returns>An effect that completes with unit once every iteration has finished, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the repetition and no further iterations are evaluated. Prefer this over <c>replicate</c> when the per-iteration results are not needed.</remarks>
    let replicateDiscard<'R, 'E> (n: int) (eff: FIO<'R, 'E>) : FIO<unit, 'E> =
        if n <= 0 then
            unit ()
        else
            forEachDiscard (seq { 1 .. n }) (fun _ -> eff)

    /// <summary>Builds an effect that repeatedly runs a body over an evolving state, collecting each iteration's result in order.</summary>
    /// <typeparam name="'S">The loop state type.</typeparam>
    /// <param name="initial">The initial loop state.</param>
    /// <param name="cont">A predicate evaluated at the start of each iteration; the loop runs while it returns <c>true</c>.</param>
    /// <param name="inc">A function from the current state to the next state, applied after each iteration's body succeeds.</param>
    /// <param name="body">A function from the current state to the effect that produces this iteration's result.</param>
    /// <returns>An effect that completes with the list of per-iteration results in iteration order, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the loop and no further iterations are evaluated. <paramref name="cont"/> is evaluated before <paramref name="body"/> on each iteration, so a predicate that returns <c>false</c> on the initial state yields an empty result list without invoking <paramref name="body"/>.</remarks>
    let loop<'S, 'R, 'E> (initial: 'S) (cont: 'S -> bool) (inc: 'S -> 'S) (body: 'S -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        suspend (fun () ->
            let rec go state acc =
                if cont state then
                    (body state).FlatMap(fun res ->
                        suspend (fun () -> go (inc state) (res :: acc)))
                else
                    succeed (List.rev acc)
            go initial [])

    /// <summary>Builds an effect that repeatedly runs a body over an evolving state and discards each iteration's result.</summary>
    /// <typeparam name="'S">The loop state type.</typeparam>
    /// <param name="initial">The initial loop state.</param>
    /// <param name="cont">A predicate evaluated at the start of each iteration; the loop runs while it returns <c>true</c>.</param>
    /// <param name="inc">A function from the current state to the next state, applied after each iteration's body succeeds.</param>
    /// <param name="body">A function from the current state to the effect that processes this iteration.</param>
    /// <returns>An effect that completes with unit once the loop finishes, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the loop and no further iterations are evaluated. Prefer this over <c>loop</c> when the per-iteration results are not needed.</remarks>
    let loopDiscard<'S, 'R, 'E> (initial: 'S) (cont: 'S -> bool) (inc: 'S -> 'S) (body: 'S -> FIO<'R, 'E>) : FIO<unit, 'E> =
        suspend (fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap(fun _ ->
                        suspend (fun () -> go (inc state)))
                else
                    unit ()
            go initial)

    /// <summary>Builds an effect that repeatedly runs a body whose result is the next loop state, returning the final state once the predicate stops the loop.</summary>
    /// <typeparam name="'S">The loop state type, also the body's success type.</typeparam>
    /// <param name="initial">The initial loop state.</param>
    /// <param name="cont">A predicate evaluated at the start of each iteration; the loop runs while it returns <c>true</c>.</param>
    /// <param name="body">A function from the current state to the effect that produces the next state.</param>
    /// <returns>An effect that completes with the most recent state for which <paramref name="cont"/> returned <c>false</c>, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the loop. Use this when the body itself drives state progression. If <paramref name="cont"/> returns <c>false</c> on <paramref name="initial"/>, the result is <paramref name="initial"/> and <paramref name="body"/> is never invoked.</remarks>
    let iterate<'S, 'E> (initial: 'S) (cont: 'S -> bool) (body: 'S -> FIO<'S, 'E>) : FIO<'S, 'E> =
        suspend (fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap(fun next ->
                        suspend (fun () -> go next))
                else
                    succeed state
            go initial)

    /// <summary>Combines a sequence of effects into one effect that runs them in order and folds each successful result into an accumulator without materialising the intermediate list.</summary>
    /// <param name="effects">The effects to run sequentially; an empty sequence yields <paramref name="zero"/> without invoking <paramref name="func"/>.</param>
    /// <param name="zero">The initial accumulator value.</param>
    /// <param name="func">The folding function applied left-to-right as each effect completes successfully.</param>
    /// <returns>An effect that completes with the final accumulator, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; the first failure short-circuits the fold and no further effects are evaluated. Stack-safe via internal trampolining.</remarks>
    let mergeAll<'A, 'B, 'E> (effects: seq<FIO<'A, 'E>>) (zero: 'B) (func: 'B -> 'A -> 'B) : FIO<'B, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray effects
            let rec go i acc =
                if i >= arr.Length then
                    succeed acc
                else
                    arr.[i].FlatMap(fun x ->
                        suspend (fun () -> go (i + 1) (func acc x)))
            go 0 zero)

    /// <summary>Combines a sequence of effects into one effect that runs them concurrently and folds each successful result into an accumulator.</summary>
    /// <param name="effects">The effects to run in parallel; an empty sequence yields <paramref name="zero"/> without forking.</param>
    /// <param name="zero">The initial accumulator value.</param>
    /// <param name="func">The folding function applied to each successful result in source order after every effect has completed.</param>
    /// <returns>An effect that completes with the final accumulator, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates. The fold itself is applied sequentially in source order once all effects have completed; the user-supplied <paramref name="func"/> should be associative if the caller wants the result to be independent of the relative completion order of effects.</remarks>
    let mergeAllPar<'A, 'B, 'E> (effects: seq<FIO<'A, 'E>>) (zero: 'B) (func: 'B -> 'A -> 'B) : FIO<'B, 'E> =
        // TODO: incremental pairwise reduce as fibers complete (like ZIO) instead of collect-then-fold.
        (collectAllPar effects).FlatMap(fun xs ->
            succeed (List.fold func zero xs))

    /// <summary>Combines a non-empty group of effects into one effect that runs them in order and reduces each successful result into the first one using <paramref name="func"/>.</summary>
    /// <param name="head">The first effect; mandatory and runs before <paramref name="tail"/>. Its successful result seeds the reduction.</param>
    /// <param name="tail">The remaining effects to fold; an empty sequence yields <paramref name="head"/>'s result.</param>
    /// <param name="func">The reducing function applied left-to-right as each tail effect completes successfully.</param>
    /// <returns>An effect that completes with the reduced value, or fails with the first error observed.</returns>
    /// <remarks>Sequential evaluation; failure of <paramref name="head"/> short-circuits before <paramref name="tail"/> is consulted. Failure inside <paramref name="tail"/> short-circuits the reduction. The non-empty invariant is enforced by the mandatory <paramref name="head"/> parameter.</remarks>
    let reduceAll<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        head.FlatMap(fun h -> mergeAll tail h func)

    /// <summary>Combines a non-empty group of effects into one effect that runs them concurrently and reduces each successful result using <paramref name="func"/>.</summary>
    /// <param name="head">The first effect; mandatory and runs alongside <paramref name="tail"/> on its own fiber. Its successful result is the first element of the reduction.</param>
    /// <param name="tail">The remaining effects to fold; an empty sequence yields <paramref name="head"/>'s result.</param>
    /// <param name="func">The reducing function applied left-to-right over the source-order list of results after every effect has completed.</param>
    /// <returns>An effect that completes with the reduced value, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c> before the failure propagates. The reduction itself is applied sequentially in source order once all effects have completed; the user-supplied <paramref name="func"/> should be associative if the caller wants the result to be independent of the relative completion order of effects.</remarks>
    let reduceAllPar<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        // TODO: incremental pairwise reduce as fibers complete (like ZIO) instead of collect-then-fold.
        let all = Seq.append (Seq.singleton head) tail
        (collectAllPar all).FlatMap(fun xs ->
            succeed (List.reduce func xs))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each in order, never failing — per-input failures and successes are gathered into two lists in source order.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because per-input failures are moved into the success channel.</typeparam>
    /// <param name="items">The inputs to traverse; an empty sequence yields <c>([], [])</c> without invoking <paramref name="func"/>.</param>
    /// <param name="func">A function from each input to the effect that produces its result; per-input failures are collected rather than short-circuiting the traversal.</param>
    /// <returns>An effect that completes with a tuple <c>(errors, successes)</c> where both lists preserve the relative order of <paramref name="items"/>.</returns>
    /// <remarks>Sequential evaluation; every input is processed regardless of per-input failures. Stack safety is delegated to <c>forEach</c>.</remarks>
    let partition<'A, 'R, 'E, 'E1> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'E list * 'R list, 'E1> =
        let resultOf a = (func a).FlatMap(fun x -> succeed (Ok x)).CatchAll(fun e -> succeed (Error e))
        (forEach items resultOf).FlatMap(fun results ->
            let folder (errs, oks) res =
                match res with
                | Ok x -> errs, x :: oks
                | Error e -> e :: errs, oks
            let errs, oks = List.fold folder ([], []) results
            succeed (List.rev errs, List.rev oks))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each concurrently, never failing — per-input failures and successes are gathered into two lists in source order.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because per-input failures are moved into the success channel.</typeparam>
    /// <param name="items">The inputs to traverse; an empty sequence yields <c>([], [])</c> without forking.</param>
    /// <param name="func">A function from each input to the effect that produces its result; per-input failures are collected rather than interrupting siblings.</param>
    /// <returns>An effect that completes with a tuple <c>(errors, successes)</c> where both lists preserve the relative order of <paramref name="items"/>.</returns>
    /// <remarks>Each input runs on its own fiber. Unlike <c>forEachPar</c>, a per-input failure does not interrupt sibling fibers — every input is allowed to complete and its outcome is collected. Stack safety is delegated to <c>forEachPar</c>.</remarks>
    let partitionPar<'A, 'R, 'E, 'E1> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'E list * 'R list, 'E1> =
        let resultOf a = (func a).FlatMap(fun x -> succeed (Ok x)).CatchAll(fun e -> succeed (Error e))
        (forEachPar items resultOf).FlatMap(fun results ->
            let folder (errs, oks) res =
                match res with
                | Ok x -> errs, x :: oks
                | Error e -> e :: errs, oks
            let errs, oks = List.fold folder ([], []) results
            succeed (List.rev errs, List.rev oks))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each in order, collecting every error rather than short-circuiting and only succeeding when every input succeeds.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence yields an empty result list without invoking <paramref name="func"/>.</param>
    /// <param name="func">A function from each input to the effect that produces its result; per-input failures are accumulated rather than short-circuiting the traversal.</param>
    /// <returns>An effect that completes with the list of per-input results in source order when every input succeeds, or fails with the source-ordered list of every error observed.</returns>
    /// <remarks>Sequential evaluation; every input is processed regardless of per-input failures. The error-channel list is non-empty when the effect fails. Stack safety is delegated to <c>forEach</c>.</remarks>
    let validate<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E list> =
        let resultOf a = (func a).FlatMap(fun x -> succeed (Ok x)).CatchAll(fun e -> succeed (Error e))
        (forEach items resultOf).FlatMap(fun results ->
            let folder (errs, oks) res =
                match res with
                | Ok x -> errs, x :: oks
                | Error e -> e :: errs, oks
            let errs, oks = List.fold folder ([], []) results
            if List.isEmpty errs then
                succeed (List.rev oks)
            else
                fail (List.rev errs))

    /// <summary>Combines a sequence of inputs into one effect that applies an effectful function to each concurrently, collecting every error rather than interrupting siblings and only succeeding when every input succeeds.</summary>
    /// <param name="items">The inputs to traverse; an empty sequence yields an empty result list without forking.</param>
    /// <param name="func">A function from each input to the effect that produces its result; per-input failures are accumulated rather than interrupting siblings.</param>
    /// <returns>An effect that completes with the list of per-input results in source order when every input succeeds, or fails with the source-ordered list of every error observed.</returns>
    /// <remarks>Each input runs on its own fiber. Unlike <c>forEachPar</c>, a per-input failure does not interrupt sibling fibers — every input is allowed to complete and its outcome is collected. The error-channel list is non-empty when the effect fails. Stack safety is delegated to <c>forEachPar</c>.</remarks>
    let validatePar<'A, 'R, 'E> (items: seq<'A>) (func: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E list> =
        let resultOf a = (func a).FlatMap(fun x -> succeed (Ok x)).CatchAll(fun e -> succeed (Error e))
        (forEachPar items resultOf).FlatMap(fun results ->
            let folder (errs, oks) res =
                match res with
                | Ok x -> errs, x :: oks
                | Error e -> e :: errs, oks
            let errs, oks = List.fold folder ([], []) results
            if List.isEmpty errs then
                succeed (List.rev oks)
            else
                fail (List.rev errs))

    /// <summary>Combines a sequence of effects into one effect that runs them in order and collects only the successful results, silently dropping failures.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because per-effect failures are discarded.</typeparam>
    /// <param name="effects">The effects to run sequentially; an empty sequence yields an empty result list without evaluating anything.</param>
    /// <returns>An effect that completes with the list of successful results in source order; failed effects contribute nothing and do not propagate.</returns>
    /// <remarks>Sequential evaluation; every effect is evaluated regardless of per-effect failures. Stack safety is delegated to <c>forEach</c>.</remarks>
    let collectAllSuccesses<'R, 'E, 'E1> (effects: seq<FIO<'R, 'E>>) : FIO<'R list, 'E1> =
        let optionOf (eff: FIO<'R, 'E>) = 
            eff.FlatMap(fun x -> succeed (Some x)).CatchAll(fun _ -> succeed None)
        (forEach effects optionOf).FlatMap(fun results ->
            succeed (results |> List.choose id))

    /// <summary>Builds an effect that evaluates an effectful predicate and continues with one of two branches based on its result.</summary>
    /// <param name="predicate">The effect producing the boolean to branch on.</param>
    /// <param name="onTrue">The effect to evaluate when <paramref name="predicate"/> succeeds with <c>true</c>.</param>
    /// <param name="onFalse">The effect to evaluate when <paramref name="predicate"/> succeeds with <c>false</c>.</param>
    /// <returns>An effect that completes with the result of the selected branch, or fails with the first error observed in <paramref name="predicate"/> or the selected branch.</returns>
    /// <remarks>Only the selected branch is evaluated. A failure in <paramref name="predicate"/> short-circuits before either branch is consulted.</remarks>
    let ifFIO<'R, 'E> (predicate: FIO<bool, 'E>) (onTrue: FIO<'R, 'E>) (onFalse: FIO<'R, 'E>) : FIO<'R, 'E> =
        predicate.FlatMap(fun b -> if b then onTrue else onFalse)

    /// <summary>Extracts the value from an effect producing <c>Option</c>, failing with a supplied error when the option is <c>None</c>.</summary>
    /// <param name="error">The typed error to fail with when the option is <c>None</c>.</param>
    /// <param name="eff">The effect whose success value is the <c>Option</c> to unwrap.</param>
    /// <returns>An effect that completes with the <c>Some</c> value or fails with <paramref name="error"/> when the option is <c>None</c>.</returns>
    /// <remarks>Exposed as a factory because F# extension methods cannot constrain the success type to <c>'R option</c>; use with <c>|></c> for chainability.</remarks>
    let someOrFail<'R, 'E> (error: 'E) (eff: FIO<'R option, 'E>) : FIO<'R, 'E> =
        eff.FlatMap(function
            | Some res -> succeed res
            | None -> fail error)

    /// <summary>Extracts the value from an effect producing <c>Option</c>, substituting a default value when the option is <c>None</c>.</summary>
    /// <param name="defaultValue">The value to produce when the option is <c>None</c>.</param>
    /// <param name="eff">The effect whose success value is the <c>Option</c> to unwrap.</param>
    /// <returns>An effect that completes with the <c>Some</c> value or with <paramref name="defaultValue"/> when the option is <c>None</c>.</returns>
    /// <remarks>Exposed as a factory because F# extension methods cannot constrain the success type to <c>'R option</c>; use with <c>|></c> for chainability.</remarks>
    let someOrElse<'R, 'E> (defaultValue: 'R) (eff: FIO<'R option, 'E>) : FIO<'R, 'E> =
        eff.FlatMap(function
            | Some res -> succeed res
            | None -> succeed defaultValue)

    /// <summary>Extracts the value from an effect producing <c>Option</c>, evaluating a fallback effect when the option is <c>None</c>.</summary>
    /// <param name="defaultEffect">The effect to evaluate when the option is <c>None</c>; its outcome is propagated.</param>
    /// <param name="eff">The effect whose success value is the <c>Option</c> to unwrap.</param>
    /// <returns>An effect that completes with the <c>Some</c> value or with the outcome of <paramref name="defaultEffect"/> when the option is <c>None</c>.</returns>
    /// <remarks>Exposed as a factory because F# extension methods cannot constrain the success type to <c>'R option</c>; use with <c>|></c> for chainability.</remarks>
    let someOrElseFIO<'R, 'E> (defaultEffect: FIO<'R, 'E>) (eff: FIO<'R option, 'E>) : FIO<'R, 'E> =
        eff.FlatMap(function
            | Some res -> succeed res
            | None -> defaultEffect)

    /// <summary>Builds an effect that returns the first success from a non-empty sequence of effects, evaluating them in order until one succeeds.</summary>
    /// <param name="head">The first effect to attempt; mandatory and runs before any tail effect.</param>
    /// <param name="tail">The remaining effects to attempt in order when prior effects fail; an empty sequence yields <paramref name="head"/>'s outcome.</param>
    /// <returns>An effect that completes with the first observed success, or fails with the final effect's error when every attempt fails.</returns>
    /// <remarks>Sequential evaluation; each effect is evaluated only after the previous one has failed. Stack-safe via <c>CatchAll</c>'s iterative flattening. The non-empty invariant is enforced by the mandatory <paramref name="head"/> parameter.</remarks>
    let firstSuccessOf<'R, 'E> (head: FIO<'R, 'E>) (tail: seq<FIO<'R, 'E>>) : FIO<'R, 'E> =
        tail |> Seq.fold (fun acc eff -> acc.CatchAll(fun _ -> eff)) head

    /// <summary>Builds an effect that races a non-empty sequence of effects concurrently using first-to-succeed semantics; failing racers are retired without winning.</summary>
    /// <param name="effects">The effects to race in parallel; an empty sequence causes the resulting fiber to interrupt with <c>InvalidArgument</c>.</param>
    /// <returns>An effect that completes with the first racer's success value, or fails with the most-recently-observed error when every racer fails.</returns>
    /// <remarks>Each effect runs on its own fiber. When a racer succeeds, every other fiber is interrupted with <c>ExplicitInterrupt</c> before the success propagates. A racer's failure does not win the race; the receiver continues waiting until either a success arrives or every racer has failed. Single-element sequences are propagated directly without forking.</remarks>
    let raceAll<'R, 'E> (effects: seq<FIO<'R, 'E>>) : FIO<'R, 'E> =
        suspend (fun () ->
            let arr = Seq.toArray effects

            if arr.Length = 0 then
                interrupt (InvalidArgument("effects", "sequence must not be empty")) "Cannot race an empty sequence"
            
            elif arr.Length = 1 then
                arr.[0]
            else
                let resultChan = Channel<Result<'R * int, 'E>>()

                let signal (idx: int) (fiber: Fiber<'R, 'E>) : FIO<unit, 'E> =
                    fiber.Await().FlatMap(fun result ->
                        match result with
                        | Succeeded res -> resultChan.Send(Ok(res, idx)).FlatMap(fun _ -> unit ())
                        | Failed err -> resultChan.Send(Error err).FlatMap(fun _ -> unit ())
                        | Interrupted _ -> unit ())

                let rec forkAll i (forked: Fiber<'R, 'E> list) : FIO<Fiber<'R, 'E> list, 'E> =
                    if i >= arr.Length then
                        succeed (List.rev forked)
                    else
                        arr.[i].Fork().FlatMap(fun fiber ->
                            suspend (fun () -> forkAll (i + 1) (fiber :: forked)))

                let rec startSignals (fibers: Fiber<'R, 'E> list) (idx: int) : FIO<unit, 'E> =
                    match fibers with
                    | [] -> unit ()
                    | fiber :: rest ->
                        (signal idx fiber).Fork().FlatMap(fun _ ->
                            suspend (fun () -> startSignals rest (idx + 1)))

                let interruptOthers (fiberArr: Fiber<'R, 'E> array) (winnerIdx: int) : FIO<unit, 'E> =
                    let rec loop i =
                        if i >= fiberArr.Length then
                            unit ()
                        elif i = winnerIdx then
                            loop (i + 1)
                        else
                            fiberArr.[i]
                                .Interrupt(ExplicitInterrupt, "Lost race")
                                .CatchAll(fun _ -> unit ())
                                .FlatMap(fun () -> loop (i + 1))
                    loop 0

                let rec receive (received: int) (fiberArr: Fiber<'R, 'E> array) : FIO<'R, 'E> =
                    resultChan.Receive().FlatMap(fun result ->
                        match result with
                        | Ok(res, winnerIdx) ->
                            (interruptOthers fiberArr winnerIdx).FlatMap(fun () -> succeed res)
                        | Error err ->
                            if received + 1 >= arr.Length then
                                fail err
                            else
                                receive (received + 1) fiberArr)

                (forkAll 0 []).FlatMap(fun fibers ->
                    let fiberArr = List.toArray fibers
                    (startSignals fibers 0).FlatMap(fun () -> receive 0 fiberArr)))
