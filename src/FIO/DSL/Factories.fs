/// <summary>Provides factory functions for creating FIO effects.</summary>
[<AutoOpen>]
module FIO.DSL.Factories

open System
open System.Threading.Tasks

/// <summary>Provides factory functions for creating FIO effects, accessed as <c>FIO.succeed</c>, <c>FIO.fail</c>, and so on.</summary>
[<RequireQualifiedAccess>]
module FIO =

    /// <summary>Lifts unit into an effect that completes successfully with no value.</summary>
    /// <returns>An effect that completes immediately with unit on the success channel.</returns>
    let unit<'E> () : FIO<unit, 'E> = Success()

    /// <summary>Lifts a value into an effect that completes with that value as its success result.</summary>
    /// <param name="res">The value to wrap as a successful result.</param>
    /// <returns>An effect that completes immediately with <paramref name="res"/> on the success channel.</returns>
    let succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> = Success res

    /// <summary>Lifts a typed error into an effect that fails with that error.</summary>
    /// <param name="err">The error value to fail with.</param>
    /// <returns>An effect that fails immediately with <paramref name="err"/> on the error channel.</returns>
    let fail<'R, 'E> (err: 'E) : FIO<'R, 'E> = Failure err

    /// <summary>Creates an effect that interrupts the current fiber when interpreted.</summary>
    /// <param name="cause">The reason for the interruption.</param>
    /// <param name="msg">A human-readable description of the interruption.</param>
    /// <returns>An effect that interrupts the current fiber when evaluated.</returns>
    let interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> = InterruptSelf(cause, msg)

    /// <summary>Lifts a side-effecting thunk into an effect that runs the thunk when interpreted.</summary>
    /// <param name="func">A thunk that produces the success value when invoked at evaluation time.</param>
    /// <param name="onError">A function that maps any exception thrown by <paramref name="func"/> to the typed error.</param>
    /// <returns>An effect that invokes <paramref name="func"/> when evaluated and either succeeds with its result or fails through <paramref name="onError"/>.</returns>
    let attempt<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> = FIO.Action(func, onError)

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
    let inline fromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
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

    /// <summary>Builds an effect that awaits a non-generic <c>Task</c> and completes with unit.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with unit when the task finishes successfully or fails through <paramref name="onError"/>.</returns>
    let awaitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> = AwaitTask(wrapVoidTask task, onError)

    /// <summary>Builds an effect that awaits a generic <c>Task</c> and completes with its result.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with the task's result or fails through <paramref name="onError"/>.</returns>
    let awaitGenericTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitTask(upcastTask task, onError)

    /// <summary>Builds an effect whose construction is deferred until the effect is interpreted.</summary>
    /// <param name="eff">A factory function producing the effect to evaluate.</param>
    /// <returns>An effect that calls <paramref name="eff"/> at evaluation time and runs the effect it produces.</returns>
    /// <remarks>Use to wrap construction-time side effects so they only run when the resulting effect is interpreted.</remarks>
    let suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> = (unit ()).FlatMap(fun () -> eff ())

    /// <summary>Builds an effect that awaits an <c>Async</c> computation and completes with its result.</summary>
    /// <param name="async">The async workflow to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the async workflow to the typed error.</param>
    /// <returns>An effect that starts the async workflow at evaluation time and completes with its result.</returns>
    let inline awaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        suspend (fun () -> awaitGenericTask (Async.StartAsTask async, onError))

    /// <summary>Creates a new fiber that runs a lazily-evaluated non-generic <c>Task</c> concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let fromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        suspend (fun () ->
            let fiber = new Fiber<unit, 'E>()
            ForkTask((fun () -> wrapVoidTask (taskFactory ())), onError, fiber, fiber.Context))

    /// <summary>Creates a new fiber that runs a lazily-evaluated generic <c>Task</c> concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let fromGenericTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        suspend (fun () ->
            let fiber = new Fiber<'R, 'E>()
            ForkTask((fun () -> upcastTask (taskFactory ())), onError, fiber, fiber.Context))

    /// <summary>Builds an effect that completes with unit after the specified delay.</summary>
    /// <param name="duration">The amount of time to wait before completing.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with unit once <paramref name="duration"/> has elapsed.</returns>
    let sleep<'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<unit, 'E> =
        suspend (fun () -> awaitTask (Task.Delay duration, onError))

    /// <summary>Creates an effect that never completes on its own.</summary>
    /// <returns>An effect that suspends the fiber indefinitely; the fiber must be interrupted to terminate.</returns>
    let never<'R, 'E> () : FIO<'R, 'E> =
        suspend (fun () ->
            let chan = new Channel<'R>()
            chan.Receive())

    /// <summary>Builds a resource-managed effect that acquires, uses, and releases a resource with cleanup guaranteed on every outcome.</summary>
    /// <param name="acquire">An effect that produces the resource.</param>
    /// <param name="release">A function from the acquired resource to a cleanup effect that runs on success, failure, and interruption.</param>
    /// <param name="useResource">A function from the acquired resource to the use-effect whose result is propagated to the caller.</param>
    /// <returns>An effect that completes with the use-effect's result and always runs the cleanup effect afterwards.</returns>
    /// <remarks>The cleanup is wired through <c>Ensuring</c>, so it cannot be skipped by failure or interruption of the use-effect.</remarks>
    let inline acquireRelease<'A, 'R, 'E>
        (acquire: FIO<'A, 'E>, release: 'A -> FIO<unit, 'E>, useResource: 'A -> FIO<'R, 'E>)
        : FIO<'R, 'E> =
        acquire.FlatMap(fun resource -> (useResource resource).Ensuring(release resource))

    /// <summary>Combines a sequence of effects into one that runs them sequentially and collects their results in order.</summary>
    /// <param name="effSeq">The effects to run in order; an empty sequence yields an empty result list.</param>
    /// <returns>An effect that completes with the list of results in source order, or fails on the first error encountered.</returns>
    let rec collectAll<'R, 'E> (effSeq: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        suspend (fun () ->
            let effArray = Seq.toArray effSeq

            match effArray.Length with
            | 0 -> succeed []
            | _ ->
                let results = Array.zeroCreate<'R> effArray.Length

                let rec loop (index: int) : FIO<'R list, 'E> =
                    if index >= effArray.Length then
                        succeed (Array.toList results)
                    else
                        effArray.[index]
                            .FlatMap(fun res ->
                                results.[index] <- res
                                loop (index + 1))

                loop 0)

    /// <summary>Combines a sequence of effects into one that runs them concurrently and collects their results in order.</summary>
    /// <param name="effSeq">The effects to run in parallel; an empty sequence yields an empty result list without forking.</param>
    /// <returns>An effect that completes with the list of results in the order their effects appeared in <paramref name="effSeq"/>, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c>.</remarks>
    let collectAllPar<'R, 'E> (effSeq: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        suspend (fun () ->
            let effArray = Seq.toArray effSeq

            match effArray.Length with
            | 0 -> succeed []
            | 1 -> effArray.[0].FlatMap(fun r -> succeed [ r ])
            | _ ->
                let forkEffects = Array.zeroCreate<FIO<Fiber<'R, 'E>, 'E>> effArray.Length

                for i = 0 to effArray.Length - 1 do
                    forkEffects.[i] <- effArray.[i].Fork()

                collectAll forkEffects
                |> fun forkEff ->
                    forkEff.FlatMap(fun fibers ->
                        let fibersArray = List.toArray fibers
                        let results = Array.zeroCreate<'R> fibersArray.Length

                        let rec interruptFrom (i: int) =
                            if i >= fibersArray.Length then
                                unit ()
                            else
                                fibersArray.[i]
                                    .Interrupt(ExplicitInterrupt, "collectAllPar sibling failed")
                                    .FlatMap(fun () -> interruptFrom (i + 1))

                        let rec loop (index: int) : FIO<'R list, 'E> =
                            if index >= fibersArray.Length then
                                succeed (Array.toList results)
                            else
                                fibersArray.[index]
                                    .Join()
                                    .FlatMap(fun res ->
                                        results.[index] <- res
                                        loop (index + 1))
                                    .CatchAll(fun err -> (interruptFrom (index + 1)).FlatMap(fun () -> fail err))

                        loop 0))

    /// <summary>Transforms a sequence of items into a sequential effect that runs the supplied function on each and collects the results.</summary>
    /// <param name="items">The items to process in order.</param>
    /// <param name="f">A function from each item to the effect that processes it.</param>
    /// <returns>An effect that completes with the list of per-item results in source order, or fails on the first error encountered.</returns>
    let forEach<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> = items |> Seq.map f |> collectAll

    /// <summary>Transforms a sequence of items into a parallel effect that runs the supplied function on each and collects the results.</summary>
    /// <param name="items">The items to process concurrently.</param>
    /// <param name="f">A function from each item to the effect that processes it.</param>
    /// <returns>An effect that completes with the list of per-item results in source order, or fails with the first error observed.</returns>
    /// <remarks>Fail-fast: when any per-item effect fails, the remaining sibling fibers are interrupted with <c>ExplicitInterrupt</c>.</remarks>
    let forEachPar<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        items |> Seq.map f |> collectAllPar
