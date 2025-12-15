(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional operators and aliases for FIO effects, enabling idiomatic and expressive functional programming with FIO.
/// Includes combinators for sequencing, mapping, error handling, concurrency, and channel operations.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Ops

open System.Threading.Tasks

/// <summary>
/// Succeeds immediately with the given result. Alias for <c>FIO.Succeed</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="res">The result value to succeed with.</param>
/// <returns>An FIO effect that succeeds with the given result.</returns>
let inline ( !+ ) (res: 'R) : FIO<'R, 'E> =
    FIO.Succeed res

/// <summary>
/// Fails immediately with the given error. Alias for <c>FIO.Fail</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="err">The error value to fail with.</param>
/// <returns>An FIO effect that fails with the given error.</returns>
let inline ( !- ) (err: 'E) : FIO<'R, 'E> =
    FIO.Fail err

/// <summary>
/// Succeeds with the result of a function and applies error handling if necessary. Alias for <c>FIO.FromFunc</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="func">The function to execute.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that executes the function and returns its result or an error.</returns>
let inline ( !<<< ) (func: unit -> 'R) (onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.FromFunc<'R, 'E> (func, onError)
    
/// <summary>
/// Succeeds with the result of a function and fails with Exception. Alias for <c>FIO.FromFunc</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <param name="func">The function to execute.</param>
/// <returns>An FIO effect that executes the function and returns its result or an exception.</returns>
let inline ( !<< ) (func: unit -> 'R) : FIO<'R, exn> =
    FIO.FromFunc<'R, exn> func

/// <summary>
/// Sends a message to the channel and succeeds with the message. Alias for <c>Channel.Send</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="msg">The message to send.</param>
/// <param name="chan">The channel to send to.</param>
/// <returns>An FIO effect that sends the message and returns it, or an error.</returns>
let inline ( --> ) (msg: 'R) (chan: Channel<'R>) : FIO<'R, 'E> =
    chan.Send msg

/// <summary>
/// Sends a message to the channel and succeeds with the message. Alias for <c>Channel.Send</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to send to.</param>
/// <param name="msg">The message to send.</param>
/// <returns>An FIO effect that sends the message and returns it, or an error.</returns>
let inline ( <-- ) (chan: Channel<'R>) (msg: 'R) : FIO<'R, 'E> =
    chan.Send msg

/// <summary>
/// Sends a message to the channel and succeeds with unit. Alias for <c>Channel.Send</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="msg">The message to send.</param>
/// <param name="chan">The channel to send to.</param>
/// <returns>An FIO effect that sends the message and returns unit, or an error.</returns>
let inline ( --!> ) (msg: 'R) (chan: Channel<'R>) : FIO<unit, 'E> =
    chan.Send msg |> _.Then <| FIO.Succeed ()

/// <summary>
/// Sends a message to the channel and succeeds with unit. Alias for <c>Channel.Send</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to send to.</param>
/// <param name="msg">The message to send.</param>
/// <returns>An FIO effect that sends the message and returns unit, or an error.</returns>
let inline ( <!-- ) (chan: Channel<'R>) (msg: 'R) : FIO<unit, 'E> =
    chan.Send msg |> _.Then <| FIO.Succeed ()

/// <summary>
/// Receives a message from the channel and succeeds with it. Alias for <c>Channel.Receive</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to receive from.</param>
/// <returns>An FIO effect that receives a message and returns it, or an error.</returns>
let inline ( !--> ) (chan: Channel<'R>) : FIO<'R, 'E> =
    chan.Receive()

/// <summary>
/// Receives a message from the channel and succeeds with it. Alias for <c>Channel.Receive</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to receive from.</param>
/// <returns>An FIO effect that receives a message and returns it, or an error.</returns>
let inline ( !<-- ) (chan: Channel<'R>) : FIO<'R, 'E> =
    chan.Receive()

/// <summary>
/// Receives a message from the channel and succeeds with unit. Alias for <c>Channel.Receive</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to receive from.</param>
/// <returns>An FIO effect that receives a message and returns unit, or an error.</returns>
let inline ( !--!> ) (chan: Channel<'R>) : FIO<unit, 'E> =
    chan.Receive().Then <| FIO.Succeed ()

/// <summary>
/// Receives a message from the channel and succeeds with unit. Alias for <c>Channel.Receive</c>.
/// </summary>
/// <typeparam name="R">The message type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="chan">The channel to receive from.</param>
/// <returns>An FIO effect that receives a message and returns unit, or an error.</returns>
let inline ( !<!-- ) (chan: Channel<'R>) : FIO<unit, 'E> =
    chan.Receive().Then <| FIO.Succeed ()

/// <summary>
/// Interprets an effect concurrently and returns the fiber that is interpreting it.
/// The fiber can be awaited for the result of the effect. Alias for <c>FIO.Fork</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to fork.</param>
/// <returns>An FIO effect that interprets the given effect concurrently and returns the fiber.</returns>
let inline ( !<~ ) (eff: FIO<'R, 'E>) : FIO<Fiber<'R, 'E>, 'E1> =
    eff.Fork()

/// <summary>
/// Interprets an effect concurrently and returns the fiber that is interpreting it.
/// The fiber can be awaited for the result of the effect. Alias for <c>FIO.Fork</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to fork.</param>
/// <returns>An FIO effect that interprets the given effect concurrently and returns the fiber.</returns>
let inline ( !~> ) (eff: FIO<'R, 'E>) : FIO<Fiber<'R, 'E>, 'E1> =
    eff.Fork()

/// <summary>
/// Interprets an effect concurrently and returns `unit` when interpreted. Alias for <c>FIO.Fork</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to fork.</param>
/// <returns>An FIO effect that interprets the given effect concurrently and returns `unit`.</returns>
let inline ( !!<~ ) (eff: FIO<'R, 'E>) : FIO<unit, 'E1> =
    eff.Fork().Then <| FIO.Succeed ()

/// <summary>
/// Interprets an effect concurrently and returns `unit` when interpreted. Alias for <c>FIO.Fork</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to fork.</param>
/// <returns>An FIO effect that interprets the given effect concurrently and returns `unit`.</returns>
let inline ( !!~> ) (eff: FIO<'R, 'E>) : FIO<unit, 'E1> =
    eff.Fork().Then <| FIO.Succeed ()

/// <summary>
/// Interprets two effects concurrently and succeeds with a tuple of their results when both complete.
/// If either effect fails, the error is immediately returned. Alias for <c>FIO.Parallel</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the second effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that interprets both effects concurrently and returns a tuple of their results.</returns>
let inline ( <!> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
    eff.Parallel eff'

/// <summary>
/// Interprets two effects concurrently and succeeds with `unit` when completed.
/// If either effect fails, the error is immediately returned. Alias for <c>FIO.Parallel</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the second effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that interprets both effects concurrently and returns `unit`.</returns>
let inline ( <~> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<unit, 'E> =
    eff.Parallel eff' |> _.Then <| FIO.Succeed ()

/// <summary>
/// Interprets two effects concurrently and succeeds with a tuple of their errors when both fail.
/// Alias for <c>FIO.ParallelError</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="E">The error type of the first effect.</typeparam>
/// <typeparam name="E1">The error type of the second effect.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that interprets both effects concurrently and returns a tuple of their errors.</returns>
let inline ( <!!> ) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
    eff.ParallelError eff'

/// <summary>
/// Waits for the result of the given fiber and succeeds with it. Alias for <c>Fiber.Await</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="fiber">The fiber to await.</param>
/// <returns>An FIO effect that awaits the given fiber and returns its result.</returns>
let inline ( !<~~ ) (fiber: Fiber<'R, 'E>) : FIO<'R, 'E> =
    fiber.Await()

/// <summary>
/// Waits for the result of the given fiber and succeeds with it. Alias for <c>Fiber.Await</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="fiber">The fiber to await.</param>
/// <returns>An FIO effect that awaits the given fiber and returns its result.</returns>
let inline ( !~~> ) (fiber: Fiber<'R, 'E>) : FIO<'R, 'E> =
    fiber.Await()

/// <summary>
/// Waits for the completion of the fiber and returns `unit`. Alias for <c>Fiber.Await</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="fiber">The fiber to await.</param>
/// <returns>An FIO effect that awaits the given fiber and returns `unit`.</returns>
let inline ( !!<~~ ) (fiber: Fiber<'R, 'E>) : FIO<unit, 'E> =
    fiber.Await().Then <| FIO.Succeed ()

/// <summary>
/// Waits for the completion of the fiber and returns `unit`. Alias for <c>Fiber.Await</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="fiber">The fiber to await.</param>
/// <returns>An FIO effect that awaits the given fiber and returns `unit`.</returns>
let inline ( !!~~> ) (fiber: Fiber<'R, 'E>) : FIO<unit, 'E> =
    fiber.Await().Then <| FIO.Succeed ()

/// <summary>
/// Awaits a Task and turns it into an effect, applying error handling if necessary. Alias for <c>FIO.AwaitTask</c>.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="task">The Task to await.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that completes when the Task completes or fails with an error.</returns>
let inline ( !<<~ ) (task: Task) (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.AwaitTask<unit, 'E> (task, onError)

/// <summary>
/// Awaits a Task and turns it into an effect. Alias for <c>FIO.AwaitTask</c>.
/// </summary>
/// <param name="task">The Task to await.</param>
/// <returns>An FIO effect that completes when the Task completes or fails with an exception.</returns>
let inline ( !!<<~ ) (task: Task) : FIO<unit, exn> =
    FIO.AwaitTask<unit, exn> task

/// <summary>
/// Awaits a generic Task and turns it into an effect, applying error handling if necessary. Alias for <c>FIO.AwaitGenericTask</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="task">The Task to await.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that completes with the Task result or fails with an error.</returns>
let inline ( !<<~~ ) (task: Task<'R>) (onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.AwaitGenericTask<'R, 'E> (task, onError)

/// <summary>
/// Awaits a generic Task and turns it into an effect. Alias for <c>FIO.AwaitGenericTask</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <param name="task">The Task to await.</param>
/// <returns>An FIO effect that completes with the Task result or fails with an exception.</returns>
let inline ( !!<<~~ ) (task: Task<'R>) : FIO<'R, exn> =
    FIO.AwaitGenericTask<'R, exn> task

/// <summary>
/// Awaits an Async computation and turns it into an effect with a default onError. Alias for <c>FIO.AwaitAsync</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <param name="async">The Async computation to await.</param>
/// <returns>An FIO effect that completes with the Async result or fails with an exception.</returns>
let inline ( !<<<~ ) (async: Async<'R>) : FIO<'R, exn> =
    FIO.AwaitAsync<'R, exn> async

/// <summary>
/// Awaits an Async computation and turns it into an effect, applying error handling if necessary. Alias for <c>FIO.AwaitAsync</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="async">The Async computation to await.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that completes with the Async result or fails with an error.</returns>
let inline ( !!<<<~ ) (async: Async<'R>) (onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.AwaitAsync<'R, 'E> (async, onError)

/// <summary>
/// Chains the success result of the effect to the continuation function. Alias for <c>FIO.Bind</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="R1">The result type of the continuation.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to bind.</param>
/// <param name="cont">The continuation function.</param>
/// <returns>An FIO effect that chains the success result of the given effect to the continuation function.</returns>
let inline ( >>= ) (eff: FIO<'R, 'E>) (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
    eff.Bind cont

/// <summary>
/// Chains the success result of the effect to the continuation function. Alias for <c>FIO.Bind</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="R1">The result type of the continuation.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="cont">The continuation function.</param>
/// <param name="eff">The effect to bind.</param>
/// <returns>An FIO effect that chains the success result of the given effect to the continuation function.</returns>
let inline ( =<< ) (cont: 'R -> FIO<'R1, 'E>) (eff: FIO<'R, 'E>)  : FIO<'R1, 'E> =
    eff.Bind cont

/// <summary>
/// Chains the error result of the effect to the continuation function. Alias for <c>FIO.BindError</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="E">The error type of the effect.</typeparam>
/// <typeparam name="E1">The error type of the continuation.</typeparam>
/// <param name="eff">The effect to bind.</param>
/// <param name="cont">The continuation function.</param>
/// <returns>An FIO effect that chains the error result of the given effect to the continuation function.</returns>
let inline ( >>=? ) (eff: FIO<'R, 'E>) (cont: 'E -> FIO<'R, 'E1>) : FIO<'R, 'E1> =
    eff.BindError cont

/// <summary>
/// Chains the error result of the effect to the continuation function. Alias for <c>FIO.BindError</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="E">The error type of the effect.</typeparam>
/// <typeparam name="E1">The error type of the continuation.</typeparam>
/// <param name="cont">The continuation function.</param>
/// <param name="eff">The effect to bind.</param>
/// <returns>An FIO effect that chains the error result of the given effect to the continuation function.</returns>
let inline ( ?=<< ) (cont: 'E -> FIO<'R, 'E1>) (eff: FIO<'R, 'E>) : FIO<'R, 'E1> =
    eff.BindError cont

/// <summary>
/// Maps a function over the result of an effect. Alias for <c>FIO.Map</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="R1">The result type of the mapped function.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect to map.</param>
/// <param name="cont">The function to apply.</param>
/// <returns>An FIO effect that maps the given function over the result of the effect.</returns>
let inline ( *> ) (eff: FIO<'R, 'E>) (cont: 'R -> 'R1) : FIO<'R1, 'E> =
    eff.Map cont

/// <summary>
/// Maps a function over the result of an effect. Alias for <c>FIO.Map</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="R1">The result type of the mapped function.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="cont">The function to apply.</param>
/// <param name="eff">The effect to map.</param>
/// <returns>An FIO effect that maps the given function over the result of the effect.</returns>
let inline ( <* ) (cont: 'R -> 'R1) (eff: FIO<'R, 'E>) : FIO<'R1, 'E> =
    eff.Map cont

/// <summary>
/// Maps a function over the error of an effect. Alias for <c>FIO.MapError</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="E">The error type of the effect.</typeparam>
/// <typeparam name="E1">The error type of the mapped function.</typeparam>
/// <param name="eff">The effect to map.</param>
/// <param name="cont">The function to apply.</param>
/// <returns>An FIO effect that maps the given function over the error of the effect.</returns>
let inline ( *>? ) (eff: FIO<'R, 'E>) (cont: 'E -> 'E1) : FIO<'R, 'E1> =
    eff.MapError cont

/// <summary>
/// Maps a function over the error of an effect. Alias for <c>FIO.MapError</c>.
/// </summary>
/// <typeparam name="R">The result type of the effect.</typeparam>
/// <typeparam name="E">The error type of the effect.</typeparam>
/// <typeparam name="E1">The error type of the mapped function.</typeparam>
/// <param name="cont">The function to apply.</param>
/// <param name="eff">The effect to map.</param>
/// <returns>An FIO effect that maps the given function over the error of the effect.</returns>
let inline ( ?<* ) (cont: 'E -> 'E1) (eff: FIO<'R, 'E>) : FIO<'R, 'E1> =
    eff.MapError cont

/// <summary>
/// Sequences two effects, ignoring the result of the first one. Alias for <c>FIO.Then</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the second effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that sequences the given effects, ignoring the result of the first one.</returns>
let inline ( >> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> =
    eff.Then eff'

/// <summary>
/// Sequences two effects, ignoring the result of the first one. Alias for <c>FIO.Then</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the second effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that sequences the given effects, ignoring the result of the first one.</returns>
let inline ( << ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> =
    eff'.Then eff

/// <summary>
/// Sequences two effects, ignoring the error of the first one. Alias for <c>FIO.ThenError</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="E">The error type of the first effect.</typeparam>
/// <typeparam name="E1">The error type of the second effect.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that sequences the given effects, ignoring the error of the first one.</returns>
let inline ( >>? ) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E1> =
    eff.ThenError eff'

/// <summary>
/// Sequences two effects, ignoring the error of the first one. Alias for <c>FIO.ThenError</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="E">The error type of the first effect.</typeparam>
/// <typeparam name="E1">The error type of the second effect.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that sequences the given effects, ignoring the error of the first one.</returns>
let inline ( ?<< ) (eff: FIO<'R, 'E1>) (eff': FIO<'R, 'E>) : FIO<'R, 'E1> =
    eff'.ThenError eff

/// <summary>
/// Combines two effects: one producing a function and the other a value, 
/// and applies the function to the value. Alias for <c>FIO.Apply</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the function.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The effect producing the function.</param>
/// <param name="eff'">The effect producing the value.</param>
/// <returns>An FIO effect that combines the given effects and applies the function to the value.</returns>
let inline ( <*> ) (eff: FIO<'R, 'E>) (eff': FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
    eff.Apply eff'

/// <summary>
/// Combines two effects: one producing a function and the other a value, 
/// and applies the function to the value. Alias for <c>FIO.ApplyError</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <typeparam name="E1">The error type of the function.</typeparam>
/// <param name="eff">The effect producing the function.</param>
/// <param name="eff'">The effect producing the value.</param>
/// <returns>An FIO effect that combines the given effects and applies the function to the value.</returns>
let inline ( <**> ) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
    eff.ApplyError eff'

/// <summary>
/// Combines the results of two effects into a tuple when both succeed.
/// If either effect fails, the error is immediately returned. Alias for <c>FIO.Zip</c>.
/// </summary>
/// <typeparam name="R">The result type of the first effect.</typeparam>
/// <typeparam name="R1">The result type of the second effect.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that combines the results of the given effects into a tuple.</returns>
let inline ( <^> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
    eff.Zip eff'

/// <summary>
/// Combines the errors of two effects into a tuple when both fail.
/// Alias for <c>FIO.ZipError</c>.
/// </summary>
/// <typeparam name="R">The result type.</typeparam>
/// <typeparam name="E">The error type of the first effect.</typeparam>
/// <typeparam name="E1">The error type of the second effect.</typeparam>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
/// <returns>An FIO effect that combines the errors of the given effects into a tuple.</returns>
let inline ( <^^> ) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
    eff.ZipError eff'
