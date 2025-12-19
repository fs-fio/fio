(**************************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(**************************************************************************************************)

/// <summary>
/// Provides factory functions for creating FIO effects from various sources.
/// This module is auto-opened and provides static member factories on the FIO type.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Factories

open System.Threading.Tasks

type FIO<'R, 'E> with

    /// <summary>
    /// Succeeds immediately with the provided result value.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="res">The result value to succeed with.</param>
    /// <returns>An FIO effect that succeeds with the given result.</returns>
    static member Succeed<'R, 'E> (res: 'R) : FIO<'R, 'E> =
        Success res

    /// <summary>
    /// Fails immediately with the provided error value.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="err">The error value to fail with.</param>
    /// <returns>An FIO effect that fails with the given error.</returns>
    static member Fail<'R, 'E> (err: 'E) : FIO<'R, 'E> =
        Failure err

    /// <summary>
    /// Interrupts the effect with the provided cause and message.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cause">The interruption cause.</param>
    /// <param name="msg">The interruption message.</param>
    /// <returns>An FIO effect that is interrupted with the given cause and message.</returns>
    static member Interrupt<'R, 'E> (cause: InterruptionCause, msg: string) : FIO<'R, 'E> =
        Interruption (cause, msg)

    /// <summary>
    /// Converts a function into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="func">The function to execute.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that executes the function and returns its result or an error.</returns>
    static member FromFunc<'R, 'E> (func: unit -> 'R, onError: exn -> 'E) : FIO<'R, 'E> =
        Action (func, onError)

    /// <summary>
    /// Converts a function into an effect with a default error handler that passes exceptions through.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="func">The function to execute.</param>
    /// <returns>An FIO effect that executes the function and returns its result, or fails with an exception.</returns>
    static member inline FromFunc<'R> (func: unit -> 'R) : FIO<'R, exn> =
        FIO.FromFunc<'R, exn> (func, id)

    /// <summary>
    /// Converts a Result value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="res">The Result value to convert.</param>
    /// <returns>An FIO effect that succeeds with the Ok value or fails with the Error value.</returns>
    static member inline FromResult<'R, 'E> (res: Result<'R, 'E>) : FIO<'R, 'E> =
        match res with
        | Ok res -> FIO.Succeed<'R, 'E> res
        | Error err -> FIO.Fail<'R, 'E> err

    /// <summary>
    /// Converts an Option value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="opt">The Option value to convert.</param>
    /// <param name="onNone">A function to produce an error if the option is None.</param>
    /// <returns>An FIO effect that succeeds with the Some value or fails with the error from onNone.</returns>
    static member inline FromOption<'R, 'E> (opt: Option<'R>, onNone: unit -> 'E) : FIO<'R, 'E> =
        match opt with
        | Some res -> FIO.Succeed<'R, 'E> res
        | None -> FIO.Fail<'R, 'E> <| onNone ()

    /// <summary>
    /// Converts a Choice value into an effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="choice">The Choice value to convert.</param>
    /// <returns>An FIO effect that succeeds with the Choice1Of2 value or fails with the Choice2Of2 value.</returns>
    static member inline FromChoice<'R, 'E> (choice: Choice<'R, 'E>) : FIO<'R, 'E> =
        match choice with
        | Choice1Of2 res -> FIO.Succeed<'R, 'E> res
        | Choice2Of2 err -> FIO.Fail<'R, 'E> err

    /// <summary>
    /// Converts a Task into an effect that awaits its completion.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes when the Task completes, or fails with a mapped error.</returns>
    static member AwaitTask<'E> (task: Task, onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTPLTask (task, onError)

    /// <summary>
    /// Converts a Task into an effect that awaits its completion with a default error handler.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <returns>An FIO effect that completes when the Task completes, or fails with an exception.</returns>
    static member inline AwaitTask (task: Task) : FIO<unit, exn> =
        FIO.AwaitTask<exn> (task, id)

    /// <summary>
    /// Converts a generic Task into an effect that awaits its completion and returns the result.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes with the Task result, or fails with a mapped error.</returns>
    static member AwaitTask<'R, 'E> (task: Task<'R>, onError: exn -> 'E) : FIO<'R, 'E> =
        AwaitGenericTPLTask (upcastTask task, onError)

    /// <summary>
    /// Converts a generic Task into an effect that awaits its completion with a default error handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="task">The Task to await.</param>
    /// <returns>An FIO effect that completes with the Task result, or fails with an exception.</returns>
    static member inline AwaitTask<'R> (task: Task<'R>) : FIO<'R, exn> =
        FIO.AwaitTask<'R, exn> (task, id)

    /// <summary>
    /// Converts an Async computation into an effect that awaits its completion and returns the result.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="async">The Async computation to await.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that completes with the Async result, or fails with a mapped error.</returns>
    static member inline AwaitAsync<'R, 'E> (async: Async<'R>, onError: exn -> 'E) : FIO<'R, 'E> = 
        FIO.AwaitTask<'R, 'E> (Async.StartAsTask async, onError)

    /// <summary>
    /// Converts an Async computation into an effect that awaits its completion with a default error handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="async">The Async computation to await.</param>
    /// <returns>An FIO effect that completes with the Async result, or fails with an exception.</returns>
    static member inline AwaitAsync<'R> (async: Async<'R>) : FIO<'R, exn> =
        FIO.AwaitAsync<'R, exn> (async, id)

    /// <summary>
    /// Converts a lazily-evaluated Task into a Fiber for concurrent execution.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="taskFactory">A function that produces the Task to run concurrently.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that starts the Task in a new Fiber and immediately returns the Fiber handle.</returns>
    static member FromTask<'E> (taskFactory: unit -> Task, onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E> =
        let fiber = new Fiber<unit, 'E> ()
        ConcurrentTPLTask ((fun () -> taskFactory ()), onError, fiber, fiber.Internal)

    /// <summary>
    /// Converts a lazily-evaluated Task into a Fiber for concurrent execution with a default error handler.
    /// </summary>
    /// <param name="taskFactory">A function that produces the Task to run concurrently.</param>
    /// <returns>An FIO effect that starts the Task in a new Fiber and immediately returns the Fiber handle.</returns>
    static member inline FromTask (taskFactory: unit -> Task) : FIO<Fiber<unit, exn>, exn> =
        FIO.FromTask<exn> (taskFactory, id)

    /// <summary>
    /// Converts a lazily-evaluated generic Task into a Fiber for concurrent execution.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="taskFactory">A function that produces the Task to run concurrently.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>An FIO effect that starts the Task in a new Fiber and immediately returns the Fiber handle.</returns>
    static member FromTask<'R, 'E> (taskFactory: unit -> Task<'R>, onError: exn -> 'E) : FIO<Fiber<'R, 'E>, 'E> =
        let fiber = new Fiber<'R, 'E> ()
        ConcurrentGenericTPLTask ((fun () -> upcastTask (taskFactory ())), onError, fiber, fiber.Internal)

    /// <summary>
    /// Converts a lazily-evaluated generic Task into a Fiber for concurrent execution with a default error handler.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <param name="taskFactory">A function that produces the Task to run concurrently.</param>
    /// <returns>An FIO effect that starts the Task in a new Fiber and immediately returns the Fiber handle.</returns>
    static member inline FromTask<'R> (taskFactory: unit -> Task<'R>) : FIO<Fiber<'R, exn>, exn> =
        FIO.FromTask<'R, exn> (taskFactory, id)

    /// <summary>
    /// Implements the acquire-release pattern for safe resource management.
    /// Guarantees that the release effect runs after the use effect completes, even if an error or interruption occurs.
    /// This is the fundamental resource management primitive for ensuring cleanup in all scenarios.
    /// </summary>
    /// <typeparam name="A">The resource type.</typeparam>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="acquire">The effect that acquires the resource.</param>
    /// <param name="release">A function that produces the effect to release the resource.</param>
    /// <param name="useResource">A function that produces the effect to use the resource.</param>
    /// <returns>An FIO effect that safely acquires, uses, and releases the resource with guaranteed cleanup.</returns>
    static member inline AcquireRelease<'A, 'R, 'E> (acquire: FIO<'A, 'E>) (release: 'A -> FIO<unit, 'E>) (useResource: 'A -> FIO<'R, 'E>) : FIO<'R, 'E> =
        acquire.Bind(fun resource ->
            (useResource resource)
                .Bind(fun res -> (release resource).Bind(fun _ -> FIO.Succeed<'R, 'E> res))
                .BindError(fun err -> (release resource).Bind(fun _ -> FIO.Fail<'R, 'E> err)))
