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

open System.Threading.Tasks

type FIO<'R, 'E> with

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
    static member Interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> =
        Interruption (cause, msg)

    /// <summary>
    /// Converts a function into an effect.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member FromFunc<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        Action (func, onError)

    /// <summary>
    /// Converts a function into an effect with exceptions as errors.
    /// </summary>
    /// <param name="func">The function to execute.</param>
    static member inline FromFunc<'R> (func: unit -> 'R) : FIO<'R, exn> =
        FIO.FromFunc (func, id)

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
        | None -> FIO.Fail <| onNone ()

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
        AwaitTPLTask (task, onError)

    /// <summary>
    /// Awaits a Task with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    static member inline AwaitTask (task: Task) : FIO<unit, exn> =
        FIO.AwaitTask (task, id)

    /// <summary>
    /// Awaits a Task and returns its result.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member AwaitTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitGenericTPLTask (upcastTask task, onError)

    /// <summary>
    /// Awaits a Task and returns its result with exceptions as errors.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    static member inline AwaitTask<'R> (task: Task<'R>) : FIO<'R, exn> =
        FIO.AwaitTask (task, id)

    /// <summary>
    /// Awaits an Async computation and returns its result.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member inline AwaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> = 
        FIO.AwaitTask (Async.StartAsTask async, onError)

    /// <summary>
    /// Awaits an Async computation with exceptions as errors.
    /// </summary>
    /// <param name="async">The Async computation to await.</param>
    static member inline AwaitAsync<'R> (async: Async<'R>) : FIO<'R, exn> =
        FIO.AwaitAsync (async, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member FromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E> ()
        ConcurrentTPLTask (taskFactory, onError, fiber, fiber.Internal)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    static member inline FromTask (taskFactory: unit -> Task) : FIO<Fiber<unit, exn>, exn> =
        FIO.FromTask (taskFactory, id)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member FromTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E> ()
        ConcurrentGenericTPLTask (taskFactory >> upcastTask, onError, fiber, fiber.Internal)

    /// <summary>
    /// Forks a lazily-evaluated Task into a Fiber with exceptions as errors.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task.</param>
    static member inline FromTask<'R> (taskFactory: unit -> Task<'R>) : FIO<Fiber<'R, exn>, exn> =
        FIO.FromTask (taskFactory, id)

    /// <summary>
    /// Implements the acquire-release pattern for safe resource management.
    /// </summary>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">A function that produces the effect to release the resource.</param>
    /// <param name="useResource">A function that produces the effect to use the resource.</param>
    static member inline AcquireRelease<'A, 'R, 'E> (acquire: FIO<'A, 'E>) (release: 'A -> FIO<unit, 'E>) (useResource: 'A -> FIO<'R, 'E>) : FIO<'R, 'E> =
        acquire.FlatMap <| fun resource ->
            (useResource resource).Ensuring(release resource)
