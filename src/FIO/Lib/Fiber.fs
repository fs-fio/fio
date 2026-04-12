/// <summary>
/// Static functions for creating and combining fibers.
/// </summary>
[<RequireQualifiedAccess>]
module FIO.Lib.Fiber

open FIO.DSL.Core
open FIO.DSL.Exceptions
open FIO.DSL.Factories

/// <summary>
/// Creates a fiber that is already completed with a success value.
/// </summary>
/// <param name="value">The success value.</param>
let succeed<'R, 'E> (value: 'R) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Ok(value :> obj))
    fiber

/// <summary>
/// Creates a fiber that is already completed with an error value.
/// </summary>
/// <param name="err">The error value.</param>
let fail<'R, 'E> (err: 'E) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Error(err :> obj))
    fiber

/// <summary>
/// Joins all fibers sequentially, collecting their results into a list.
/// Fails on the first error encountered.
/// </summary>
/// <param name="fibers">The fibers to join.</param>
let joinAll<'R, 'E> (fibers: Fiber<'R, 'E> list) : FIO<'R list, 'E> =
    fibers |> List.map (fun f -> f.Join()) |> FIO.collectAll

/// <summary>
/// Awaits all fibers, collecting their FiberResults without re-raising errors.
/// </summary>
/// <param name="fibers">The fibers to await.</param>
let awaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    fibers |> List.map (fun f -> f.Await()) |> FIO.collectAll

/// <summary>
/// Interrupts all fibers (fire-and-forget).
/// </summary>
/// <param name="fibers">The fibers to interrupt.</param>
let interruptAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E1> =
    FIO.Action(
        (fun () ->
            for f in fibers do
                f.Context.Interrupt(ExplicitInterrupt, "Fiber was interrupted.")),
        fun ex -> raise ex)

/// <summary>
/// Interrupts all fibers and waits for all to reach a terminal state, collecting their FiberResults.
/// </summary>
/// <param name="fibers">The fibers to interrupt and await.</param>
let interruptAwaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    (interruptAll fibers).FlatMap(fun () -> awaitAll fibers)

/// <summary>
/// Non-blocking poll of all fibers, collecting their results as options.
/// </summary>
/// <param name="fibers">The fibers to poll.</param>
let pollAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> option list, 'E1> =
    fibers |> List.map (fun f -> f.Poll()) |> FIO.collectAll
