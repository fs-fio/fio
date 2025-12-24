(**************************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(**************************************************************************************************)

/// <summary>
/// Provides factory functions for creating FIO effects from various sources.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Factories

open System
open System.Threading.Tasks

/// <summary>
/// Module-level singleton TaskCompletionSource for FIO.Never to avoid memory leaks.
/// </summary>
let private neverTcs = TaskCompletionSource<obj> TaskCreationOptions.RunContinuationsAsynchronously

type FIO<'R, 'E> with

    /// <summary>
    /// Succeeds immediately with unit.
    /// </summary>
    static member Unit<'E> () : FIO<unit, 'E> =
        Success ()

    /// <summary>
    /// Succeeds immediately with the provided result value.
    /// </summary>
    /// <param name="res">The result value.</param>
    static member Succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        Success res

    /// <summary>
    /// Fails immediately with the provided error value.
    /// </summary>
    /// <param name="err">The error value.</param>
    static member Fail<'R, 'E> (err: 'E) : FIO<'R, 'E> =
        Failure err

    /// <summary>
    /// Interrupts the effect with the provided cause and message.
    /// </summary>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    static member Interrupt<'R, 'E> (?cause: InterruptionCause, ?msg: string)  : FIO<'R, 'E> =
        let cause = defaultArg cause ExplicitInterrupt
        let msg = defaultArg msg "Fiber was interrupted."
        InterruptSelf(cause, msg)

    /// <summary>
    /// Converts a side-effecting function into an effect.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Attempt<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.Action(func, onError)

    /// <summary>
    /// Converts a side-effecting function into an effect with exceptions as errors.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    static member inline Attempt<'R> (func: unit -> 'R) : FIO<'R, exn> =
        FIO.Attempt(func, id)

    /// <summary>
    /// Converts a Result value into an effect.
    /// </summary>
    /// <param name="res">The Result value to convert.</param>
    static member inline FromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> FIO.Succeed res
        | Error err -> FIO.Fail err

    /// <summary>
    /// Converts an Option value into an effect.
    /// </summary>
    /// <param name="opt">The Option value to convert.</param>
    /// <param name="onNone">A function to produce an error if the option is None.</param>
    static member inline FromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> FIO.Succeed res
        | None -> FIO.Fail(onNone())

    /// <summary>
    /// Converts an Option value into an effect with exceptions as errors.
    /// </summary>
    /// <param name="opt">The Option value to convert.</param>
    static member inline FromOption<'R> (opt: Option<'R>) : FIO<'R, exn> =
        match opt with
        | Some res -> FIO.Succeed res
        | None -> FIO.Fail(ArgumentException "Option was None.")

    /// <summary>
    /// Converts a Choice value into an effect.
    /// </summary>
    /// <param name="choice">The Choice value to convert.</param>
    static member inline FromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> FIO.Succeed res
        | Choice2Of2 err -> FIO.Fail err

    /// <summary>
    /// Awaits a Task and returns unit on completion.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member AwaitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTPLTask(task, onError)

    /// <summary>
    /// Awaits a Task with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    static member inline AwaitTask (task: Task) : FIO<unit, exn> =
        FIO.AwaitTask(task, id)

    /// <summary>
    /// Awaits a Task and returns its result.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member AwaitTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitGenericTPLTask(upcastTask task, onError)

    /// <summary>
    /// Awaits a Task and returns its result with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    static member inline AwaitTask<'R> (task: Task<'R>) : FIO<'R, exn> =
        FIO.AwaitTask(task, id)

    /// <summary>
    /// Awaits an Async computation and returns its result.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member inline AwaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> = 
        FIO.AwaitTask(Async.StartAsTask async, onError)

    /// <summary>
    /// Awaits an Async computation with exceptions as errors.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    static member inline AwaitAsync<'R> (async: Async<'R>) : FIO<'R, exn> =
        FIO.AwaitAsync(async, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member FromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E>()
        ForkTPLTask(taskFactory, onError, fiber, fiber.Internal)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    static member inline FromTask (taskFactory: unit -> Task) : FIO<Fiber<unit, exn>, exn> =
        FIO.FromTask(taskFactory, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member FromTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E>()
        ForkGenericTPLTask(taskFactory >> upcastTask, onError, fiber, fiber.Internal)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    static member inline FromTask<'R> (taskFactory: unit -> Task<'R>) : FIO<Fiber<'R, exn>, exn> =
        FIO.FromTask(taskFactory, id)

    /// <summary>
    /// Lazily constructs an effect, deferring its creation until execution.
    /// </summary>
    /// <param name="eff">A function that produces the effect.</param>
    static member Suspend<'R, 'E> (eff: unit -> FIO<'R, 'E>) : FIO<'R, 'E> =
        FIO.Unit().FlatMap(fun () -> eff())

    /// <summary>
    /// Delays execution for the specified duration.
    /// </summary>
    /// <param name="duration">The delay duration.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Sleep<'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.Suspend(fun () -> FIO.AwaitTask(Task.Delay duration, onError))

    /// <summary>
    /// Delays execution for the specified duration with exceptions as errors.
    /// </summary>
    /// <param name="duration">The delay duration.</param>
    static member inline Sleep (duration: TimeSpan) : FIO<unit, exn> =
        FIO.Sleep(duration, id)

    /// <summary>
    /// Creates an effect that never completes (runs forever).
    /// Uses a singleton TaskCompletionSource to avoid memory leaks.
    /// </summary>
    static member Never<'R, 'E> () : FIO<'R, 'E> =
        FIO.AwaitTask(neverTcs.Task, fun _ -> Unchecked.defaultof<'E>)
            .FlatMap(fun obj -> FIO.Succeed(obj :?> 'R))

    /// <summary>
    /// Implements the acquire-release pattern for safe resource management.
    /// </summary>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">A function that produces the effect to release the resource.</param>
    /// <param name="useResource">A function that produces the effect to use the resource.</param>
    static member inline AcquireRelease<'A, 'R, 'E> (acquire: FIO<'A, 'E>, release: 'A -> FIO<unit, 'E>, useResource: 'A -> FIO<'R, 'E>) : FIO<'R, 'E> =
        acquire.FlatMap(fun resource -> (useResource resource).Ensuring(release resource))

    /// <summary>
    /// Sequences a collection of effects, collecting their results into a list.
    /// Effects are executed sequentially in the order they appear in the collection.
    /// </summary>
    /// <param name="effSeq">The sequence of effects to execute.</param>
    static member CollectAll<'R, 'E> (effSeq: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        FIO.Suspend(fun () ->
            let effList = Seq.toList effSeq
            match effList with
            | [] -> FIO.Succeed []
            | _ ->
                let rec loop (remaining: FIO<'R, 'E> list) (acc: 'R list) : FIO<'R list, 'E> =
                    match remaining with
                    | [] -> FIO.Succeed(List.rev acc)
                    | eff :: rest ->
                        eff.FlatMap(fun res -> loop rest (res :: acc))
                loop effList [])

    /// <summary>
    /// Executes a collection of effects in parallel, collecting their results into a list.
    /// The results maintain the same order as the input effects.
    /// All effects are forked simultaneously, then awaited together.
    /// </summary>
    /// <param name="effSeq">The sequence of effects to execute in parallel.</param>
    static member CollectAllPar<'R, 'E> (effSeq: seq<FIO<'R, 'E>>) : FIO<'R list, 'E> =
        FIO.Suspend(fun () ->
            let effList = Seq.toList effSeq
            match effList with
            | [] -> FIO.Succeed []
            | _ ->
                // Fork all effects in parallel first
                let forkAll =
                    effList
                    |> List.map (fun eff -> eff.Fork())
                    |> FIO.CollectAll

                // Then await all fibers in parallel
                forkAll.FlatMap(fun fibers ->
                    fibers
                    |> List.map (fun fiber -> fiber.Join())
                    |> FIO.CollectAll))

    /// <summary>
    /// Maps each item in a collection to an effect, then sequences all effects sequentially.
    /// Collects all results into a list maintaining the order of the input collection.
    /// </summary>
    /// <param name="items">The collection of items to process.</param>
    /// <param name="f">The function to map each item to an effect.</param>
    static member ForEach<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        items |> Seq.map f |> FIO.CollectAll

    /// <summary>
    /// Maps each item in a collection to an effect, then executes all effects in parallel.
    /// Collects all results into a list maintaining the order of the input collection.
    /// </summary>
    /// <param name="items">The collection of items to process.</param>
    /// <param name="f">The function to map each item to an effect.</param>
    static member ForEachPar<'A, 'R, 'E> (items: seq<'A>, f: 'A -> FIO<'R, 'E>) : FIO<'R list, 'E> =
        items |> Seq.map f |> FIO.CollectAllPar
