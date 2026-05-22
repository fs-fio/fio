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
    let interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> = Interrupt(cause, msg)

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

    /// <summary>Builds an effect that awaits a <c>Task</c> that returns no value and completes with unit.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with unit when the task finishes successfully or fails through <paramref name="onError"/>.</returns>
    let awaitUnitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> = AwaitTask(wrapVoidTask task, onError)

    /// <summary>Builds an effect that awaits a <c>Task&lt;'R&gt;</c> and completes with its result.</summary>
    /// <param name="task">The task to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with the task's result or fails through <paramref name="onError"/>.</returns>
    let awaitTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitTask(upcastTask task, onError)

    /// <summary>Builds an effect whose construction is deferred until the effect is interpreted.</summary>
    /// <param name="eff">A factory function producing the effect to evaluate.</param>
    /// <returns>An effect that calls <paramref name="eff"/> at evaluation time and runs the effect it produces.</returns>
    /// <remarks>Use to wrap construction-time side effects so they only run when the resulting effect is interpreted.</remarks>
    let suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> = (unit ()).FlatMap(fun () -> eff ())

    /// <summary>Builds an effect that produces the running fiber's <c>CancellationToken</c>.</summary>
    /// <returns>An effect that completes with the cancellation token of the fiber evaluating it; falls back to <c>CancellationToken.None</c> when no fiber context is available.</returns>
    /// <remarks>The returned token is cancelled when the running fiber is interrupted. Pass it to .NET cancellation-aware APIs to make I/O cooperate with FIO interruption.</remarks>
    let cancellationToken<'E> () : FIO<CancellationToken, 'E> = GetFiberCancellationToken

    /// <summary>Builds an effect that awaits an <c>Async</c> computation and completes with its result.</summary>
    /// <param name="async">The async workflow to await.</param>
    /// <param name="onError">A function that maps an exception thrown by the async workflow to the typed error.</param>
    /// <returns>An effect that starts the async workflow at evaluation time and completes with its result.</returns>
    let inline awaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        suspend (fun () -> awaitTask (Async.StartAsTask async, onError))

    /// <summary>Forks a new fiber that runs a lazily-evaluated <c>Task</c> that returns no value concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let forkUnitTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        suspend (fun () -> (awaitUnitTask (taskFactory (), onError)).Fork())

    /// <summary>Forks a new fiber that runs a lazily-evaluated <c>Task&lt;'R&gt;</c> concurrently with the caller.</summary>
    /// <param name="taskFactory">A factory function that creates the task to fork; called at evaluation time.</param>
    /// <param name="onError">A function that maps an exception thrown by the task to the typed error.</param>
    /// <returns>An effect that completes with a <c>Fiber</c> handle to the running task.</returns>
    /// <remarks>Fiber allocation is deferred until the effect is interpreted.</remarks>
    let forkTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        suspend (fun () -> (awaitTask (taskFactory (), onError)).Fork())

    /// <summary>Builds an effect that completes with unit after the specified delay.</summary>
    /// <param name="duration">The amount of time to wait before completing.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with unit once <paramref name="duration"/> has elapsed.</returns>
    let sleep<'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<unit, 'E> =
        suspend (fun () -> awaitUnitTask (Task.Delay duration, onError))

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
