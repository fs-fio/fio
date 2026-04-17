/// Provides factory functions for creating FIO effects.
[<AutoOpen>]
module FIO.DSL.Factories

open System
open System.Threading.Tasks

/// Factory functions for creating FIO effects.
/// Functions accepting `onError: exn -> 'E` use it to map exceptions to the effect's error type.
[<RequireQualifiedAccess>]
module FIO =

    /// Succeeds immediately with unit.
    /// <returns>An effect that succeeds with unit.</returns>
    let unit<'E> () : FIO<unit, 'E> = Success()

    /// Succeeds with the given value.
    /// <param name="res">The value to succeed with.</param>
    /// <returns>An effect that succeeds with the given value.</returns>
    let succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> = Success res

    /// Fails with the given error.
    /// <param name="err">The error to fail with.</param>
    /// <returns>An effect that fails with the given error.</returns>
    let fail<'R, 'E> (err: 'E) : FIO<'R, 'E> = Failure err

    /// Interrupts the current fiber.
    /// <param name="cause">The reason for the interruption.</param>
    /// <param name="msg">A descriptive message for the interruption.</param>
    /// <returns>An effect that interrupts the current fiber when interpreted.</returns>
    let interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> = InterruptSelf(cause, msg)

    /// Converts a side-effecting function into an effect.
    /// <param name="func">Function to execute when the effect is interpreted.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that executes the function when interpreted.</returns>
    let attempt<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> = FIO.Action(func, onError)

    /// Converts a Result into an effect.
    /// <param name="res">The Result value to convert.</param>
    /// <returns>An effect that succeeds with Ok or fails with Error.</returns>
    let inline fromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> succeed res
        | Error err -> fail err

    /// Converts an Option into an effect, failing with onNone if None.
    /// <param name="opt">The Option value to convert.</param>
    /// <param name="onNone">Produces an error when the option is None.</param>
    /// <returns>An effect that succeeds with Some or fails with the result of onNone.</returns>
    let inline fromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> succeed res
        | None -> fail (onNone ())

    /// Converts a Choice into an effect.
    /// <param name="choice">The Choice value to convert.</param>
    /// <returns>An effect that succeeds with Choice1Of2 or fails with Choice2Of2.</returns>
    let inline fromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> succeed res
        | Choice2Of2 err -> fail err

    /// Awaits a Task, returning unit on completion.
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that completes with unit when the task finishes.</returns>
    let awaitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> = AwaitTask(wrapVoidTask task, onError)

    /// Awaits a Task and returns its result.
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that completes with the task's result.</returns>
    let awaitGenericTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitTask(upcastTask task, onError)

    /// Awaits an Async computation and returns its result.
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that completes with the async computation's result.</returns>
    let inline awaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        awaitGenericTask (Async.StartAsTask async, onError)

    /// Forks a lazily-evaluated Task into a Fiber.
    /// <param name="taskFactory">Factory function that creates the Task to fork.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces a Fiber running the task.</returns>
    let fromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E>()
        ForkTask((fun () -> wrapVoidTask (taskFactory ())), onError, fiber, fiber.Context)

    /// Forks a lazily-evaluated generic Task into a Fiber.
    /// <param name="taskFactory">Factory function that creates the Task to fork.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces a Fiber running the task.</returns>
    let fromGenericTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E>()
        ForkTask((fun () -> upcastTask (taskFactory ())), onError, fiber, fiber.Context)

    /// Defers effect construction until execution.
    /// <param name="eff">Factory function that produces the effect to execute.</param>
    /// <returns>An effect whose construction is deferred until interpretation.</returns>
    let suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> = (unit ()).FlatMap(fun () -> eff ())

    /// Delays execution for the specified duration.
    /// <param name="duration">The time span to delay.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that completes with unit after the specified duration.</returns>
    let sleep<'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<unit, 'E> =
        suspend (fun () -> awaitTask (Task.Delay duration, onError))

    /// Creates an effect that never completes.
    /// <remarks>Blocks forever by waiting on an empty channel. Must be interrupted to terminate.</remarks>
    /// <returns>An effect that never completes.</returns>
    let never<'R, 'E> () : FIO<'R, 'E> =
        suspend (fun () ->
            let chan = new Channel<'R>()
            chan.Receive())

    /// Acquires a resource, uses it, and guarantees release.
    /// <remarks>Release runs on success, failure, and interruption via Ensuring.</remarks>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">Produces the effect to release the resource.</param>
    /// <param name="useResource">Produces the effect to use the resource.</param>
    /// <returns>An effect that acquires, uses, and releases the resource.</returns>
    let inline acquireRelease<'A, 'R, 'E>
        (acquire: FIO<'A, 'E>, release: 'A -> FIO<unit, 'E>, useResource: 'A -> FIO<'R, 'E>)
        : FIO<'R, 'E> =
        acquire.FlatMap(fun resource -> (useResource resource).Ensuring(release resource))

    /// Sequences effects, collecting results into a list.
    /// <param name="effSeq">The sequence of effects to execute.</param>
    /// <returns>An effect that produces a list of all results in order.</returns>
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

    /// Executes effects in parallel, collecting results into a list.
    /// <remarks>All forked fibers are interrupted if any effect fails (fail-fast semantics).</remarks>
    /// <param name="effSeq">The sequence of effects to execute in parallel.</param>
    /// <returns>An effect that produces a list of all results in order.</returns>
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

    /// Maps each item to an effect and sequences them, collecting results.
    /// <param name="items">The sequence of items to process.</param>
    /// <param name="f">Function that maps each item to an effect.</param>
    /// <returns>An effect that produces a list of all results in order.</returns>
    let forEach<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> = items |> Seq.map f |> collectAll

    /// Maps each item to an effect and executes them in parallel, collecting results.
    /// <remarks>All forked fibers are interrupted if any effect fails (fail-fast semantics).</remarks>
    /// <param name="items">The sequence of items to process in parallel.</param>
    /// <param name="f">Function that maps each item to an effect.</param>
    /// <returns>An effect that produces a list of all results in order.</returns>
    let forEachPar<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        items |> Seq.map f |> collectAllPar
