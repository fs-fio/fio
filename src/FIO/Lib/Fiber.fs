/// Static functions for creating and combining fibers.
[<RequireQualifiedAccess>]
module FIO.Lib.Fiber

open FIO.DSL.Core
open FIO.DSL.Factories
open FIO.DSL.Exceptions

/// Creates a fiber already completed with a success value.
let succeed<'R, 'E> (value: 'R) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Ok(value :> obj))
    fiber

/// Creates a fiber already completed with an error value.
let fail<'R, 'E> (err: 'E) =
    let fiber = new Fiber<'R, 'E>()
    fiber.Context.Complete(Error(err :> obj))
    fiber

/// Joins all fibers, collecting success values into a list. Fails on the first error encountered by re-raising it in the caller's fiber.
let joinAll<'R, 'E> (fibers: Fiber<'R, 'E> list) : FIO<'R list, 'E> =
    fibers |> List.map (fun f -> f.Join()) |> FIO.collectAll

/// Awaits all fibers, collecting their terminal states without re-raising errors. Use instead of joinAll when you need to inspect individual outcomes.
let awaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    fibers |> List.map (fun f -> f.Await()) |> FIO.collectAll

/// Interrupts all fibers without waiting for them to terminate.
let interruptAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<unit, 'E1> =
    Action(
        (fun () ->
            for f in fibers do
                f.Context.Interrupt(ExplicitInterrupt, "Fiber was interrupted.")),
        fun ex -> raise ex
    )

/// Interrupts all fibers and waits for each to reach a terminal state.
let interruptAwaitAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> list, 'E1> =
    (interruptAll fibers).FlatMap(fun () -> awaitAll fibers)

/// Non-blocking poll of all fibers. Returns Some result for completed fibers, None for those still running.
let pollAll<'R, 'E, 'E1> (fibers: Fiber<'R, 'E> list) : FIO<FiberResult<'R, 'E> option list, 'E1> =
    fibers |> List.map (fun f -> f.Poll()) |> FIO.collectAll
