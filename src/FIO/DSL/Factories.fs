/// <summary>
/// Provides factory functions for creating FIO effects from various sources.
/// </summary>
[<AutoOpen>]
module FIO.DSL.Factories

open System
open System.Threading.Tasks

/// <summary>
/// Factory functions for creating FIO effects.
/// </summary>
[<RequireQualifiedAccess>]
module FIO =

    /// <summary>
    /// Succeeds immediately with unit.
    /// </summary>
    let unit<'E> () : FIO<unit, 'E> = Success()

    /// <summary>
    /// Succeeds immediately with the provided result value.
    /// </summary>
    /// <param name="res">The result value.</param>
    let succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> = Success res

    /// <summary>
    /// Fails immediately with the provided error value.
    /// </summary>
    /// <param name="err">The error value.</param>
    let fail<'R, 'E> (err: 'E) : FIO<'R, 'E> = Failure err

    /// <summary>
    /// Interrupts the effect with the provided cause and message.
    /// </summary>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    let interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> = InterruptSelf(cause, msg)

    /// <summary>
    /// Converts a side-effecting function into an effect.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let attempt<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> = FIO.Action(func, onError)

    /// <summary>
    /// Converts a side-effecting function into an effect with exceptions as errors.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    let inline attemptExn<'R> (func: unit -> 'R) : FIO<'R, exn> = attempt (func, id)

    /// <summary>
    /// Converts a Result value into an effect.
    /// </summary>
    /// <param name="res">The Result value to convert.</param>
    let inline fromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> succeed res
        | Error err -> fail err

    /// <summary>
    /// Converts an Option value into an effect.
    /// </summary>
    /// <param name="opt">The Option value to convert.</param>
    /// <param name="onNone">A function to produce an error if the option is None.</param>
    let inline fromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> succeed res
        | None -> fail (onNone ())

    /// <summary>
    /// Converts an Option value into an effect with exceptions as errors.
    /// </summary>
    /// <param name="opt">The Option value to convert.</param>
    let inline fromOptionExn<'R> (opt: Option<'R>) : FIO<'R, exn> =
        match opt with
        | Some res -> succeed res
        | None -> fail (ArgumentException "Option was None.")

    /// <summary>
    /// Converts a Choice value into an effect.
    /// </summary>
    /// <param name="choice">The Choice value to convert.</param>
    let inline fromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> succeed res
        | Choice2Of2 err -> fail err

    /// <summary>
    /// Awaits a Task and returns unit on completion.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let awaitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> = AwaitTPLTask(task, onError)

    /// <summary>
    /// Awaits a Task with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    let inline awaitTaskExn (task: Task) : FIO<unit, exn> = awaitTask (task, id)

    /// <summary>
    /// Awaits a Task and returns its result.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let awaitGenericTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitGenericTPLTask(upcastTask task, onError)

    /// <summary>
    /// Awaits a Task and returns its result with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    let inline awaitGenericTaskExn<'R> (task: Task<'R>) : FIO<'R, exn> = awaitGenericTask (task, id)

    /// <summary>
    /// Awaits an Async computation and returns its result.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let inline awaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        awaitGenericTask (Async.StartAsTask async, onError)

    /// <summary>
    /// Awaits an Async computation with exceptions as errors.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    let inline awaitAsyncExn<'R> (async: Async<'R>) : FIO<'R, exn> = awaitAsync (async, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let fromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E>()
        ForkTPLTask(taskFactory, onError, fiber, fiber.Context)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    let inline fromTaskExn (taskFactory: unit -> Task) : FIO<Fiber<unit, exn>, exn> = fromTask (taskFactory, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let fromGenericTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E>()
        ForkGenericTPLTask((fun () -> upcastTask (taskFactory ())), onError, fiber, fiber.Context)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    let inline fromGenericTaskExn<'R> (taskFactory: unit -> Task<'R>) : FIO<Fiber<'R, exn>, exn> =
        fromGenericTask (taskFactory, id)

    /// <summary>
    /// Lazily constructs an effect, deferring its creation until execution.
    /// </summary>
    /// <param name="eff">A function that produces the effect.</param>
    let suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> = (unit ()).FlatMap(fun () -> eff ())

    /// <summary>
    /// Delays execution for the specified duration.
    /// </summary>
    /// <param name="duration">The delay duration.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    let sleep<'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<unit, 'E> =
        suspend (fun () -> awaitTask (Task.Delay duration, onError))

    /// <summary>
    /// Delays execution for the specified duration with exceptions as errors.
    /// </summary>
    /// <param name="duration">The delay duration.</param>
    let inline sleepExn (duration: TimeSpan) : FIO<unit, exn> = sleep (duration, id)

    /// <summary>
    /// Creates an effect that never completes (runs forever).
    /// Uses a channel receive on an empty channel that cooperates with
    /// the runtime's blocking mechanism instead of pinning evaluation workers.
    /// </summary>
    let never<'R, 'E> () : FIO<'R, 'E> =
        suspend (fun () ->
            let chan = new Channel<'R>()
            chan.Receive())

    /// <summary>
    /// Implements the acquire-release pattern for safe resource management.
    /// </summary>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">A function that produces the effect to release the resource.</param>
    /// <param name="useResource">A function that produces the effect to use the resource.</param>
    let inline acquireRelease<'A, 'R, 'E>
        (acquire: FIO<'A, 'E>, release: 'A -> FIO<unit, 'E>, useResource: 'A -> FIO<'R, 'E>)
        : FIO<'R, 'E> =
        acquire.FlatMap(fun resource -> (useResource resource).Ensuring(release resource))

    /// <summary>
    /// Sequences a collection of effects, collecting their results into a list.
    /// Effects are executed sequentially in the order they appear in the collection.
    /// </summary>
    /// <param name="effSeq">The sequence of effects to execute.</param>
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

    /// <summary>
    /// Executes a collection of effects in parallel, collecting their results into a list.
    /// The results maintain the same order as the input effects.
    /// All effects are forked simultaneously, then awaited together.
    /// If any fiber fails, the remaining unjoined fibers are interrupted before propagating the error.
    /// </summary>
    /// <param name="effSeq">The sequence of effects to execute in parallel.</param>
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

    /// <summary>
    /// Maps each item in a collection to an effect, then sequences all effects sequentially.
    /// Collects all results into a list maintaining the order of the input collection.
    /// </summary>
    /// <param name="items">The collection of items to process.</param>
    /// <param name="f">The function to map each item to an effect.</param>
    let forEach<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> = items |> Seq.map f |> collectAll

    /// <summary>
    /// Maps each item in a collection to an effect, then executes all effects in parallel.
    /// Collects all results into a list maintaining the order of the input collection.
    /// </summary>
    /// <param name="items">The collection of items to process.</param>
    /// <param name="f">The function to map each item to an effect.</param>
    let forEachPar<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        items |> Seq.map f |> collectAllPar
