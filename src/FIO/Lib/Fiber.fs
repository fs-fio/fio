/// <summary>Provides static functions for creating and combining fibers.</summary>
[<RequireQualifiedAccess>]
module FIO.Lib.Fiber

open FIO.DSL.Core
open FIO.DSL.Factories
open FIO.DSL.Exceptions

/// <summary>Creates a fiber that is already in the succeeded state with the given value.</summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The typed error type.</typeparam>
/// <param name="value">The success value to assign to the fiber.</param>
/// <returns>A fiber whose terminal state is success carrying <paramref name="value"/>.</returns>
let succeed<'R, 'E> (value: 'R) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Ok(value :> obj))
    fiber

/// <summary>Creates a fiber that is already in the failed state with the given typed error.</summary>
/// <typeparam name="'R">The success result type.</typeparam>
/// <typeparam name="'E">The typed error type.</typeparam>
/// <param name="err">The typed error to assign to the fiber.</param>
/// <returns>A fiber whose terminal state is failure carrying <paramref name="err"/>.</returns>
let fail<'R, 'E> (err: 'E) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Error(err :> obj))
    fiber

/// <summary>Combines a list of fibers into an effect that joins each in order and collects their success values.</summary>
/// <typeparam name="'R">The success result type produced by every fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fibers may fail with.</typeparam>
/// <param name="fibers">The fibers to join.</param>
/// <returns>An effect that completes with the list of success values, or fails with the first error or interruption observed.</returns>
let joinAll<'R, 'E> (fibers: Fiber<'R, 'E> list) : FIO<'R list, 'E> =
    fibers |> List.map (fun f -> f.Join()) |> FIO.collectAll

/// <summary>Combines a list of fibers into an effect that awaits each in order and collects their terminal states.</summary>
/// <typeparam name="'R">The success result type produced by every fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fibers may fail with.</typeparam>
/// <typeparam name="'E1">The error type of the resulting effect; never produced because awaiting never fails.</typeparam>
/// <param name="fibers">The fibers to await.</param>
/// <returns>An effect that completes with the list of <c>FiberResult</c> values, exposing successes, failures, and interruptions.</returns>
let awaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    fibers |> List.map (fun f -> f.Await()) |> FIO.collectAll

/// <summary>Creates an effect that requests interruption of every fiber in the list without awaiting them.</summary>
/// <typeparam name="'R">The success result type produced by every fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fibers may fail with.</typeparam>
/// <typeparam name="'E1">The error type of the resulting effect; never produced because requesting interruption does not fail.</typeparam>
/// <param name="fibers">The fibers to interrupt.</param>
/// <returns>An effect that completes with unit once interruption has been requested for every fiber.</returns>
let interruptAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E1> =
    Action(
        (fun () ->
            for f in fibers do
                f.Context.Interrupt(ExplicitInterrupt, "Fiber was interrupted.")),
        fun ex -> raise ex
    )

/// <summary>Combines fiber interruption with await, requesting interruption of every fiber and then collecting their terminal states.</summary>
/// <typeparam name="'R">The success result type produced by every fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fibers may fail with.</typeparam>
/// <typeparam name="'E1">The error type of the resulting effect; never produced because the await never fails.</typeparam>
/// <param name="fibers">The fibers to interrupt and await.</param>
/// <returns>An effect that completes with the list of <c>FiberResult</c> values after every fiber has reached a terminal state.</returns>
let interruptAwaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    (interruptAll fibers).FlatMap(fun () -> awaitAll fibers)

/// <summary>Combines a list of fibers into a non-blocking poll, returning the terminal state of each fiber that has finished.</summary>
/// <typeparam name="'R">The success result type produced by every fiber.</typeparam>
/// <typeparam name="'E">The typed error type the fibers may fail with.</typeparam>
/// <typeparam name="'E1">The error type of the resulting effect; never produced because polling does not fail.</typeparam>
/// <param name="fibers">The fibers to poll.</param>
/// <returns>An effect that completes with a list aligned with <paramref name="fibers"/>, where each entry is <c>Some</c> for terminated fibers and <c>None</c> for still-running fibers.</returns>
let pollAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> option list, 'E1> =
    fibers |> List.map (fun f -> f.Poll()) |> FIO.collectAll
