/// <summary>Provides a one-shot synchronization primitive for coordinating between fibers.</summary>
[<AutoOpen>]
module FIO.Promise

open FIO.DSL

open System.Threading.Tasks

/// <summary>Represents a one-shot synchronization primitive that may be completed once with success or failure and observed by multiple waiters.</summary>
/// <typeparam name="'R">The success result type the promise may complete with.</typeparam>
/// <typeparam name="'E">The typed error type the promise may complete with.</typeparam>
[<Sealed>]
type Promise<'R, 'E> internal () =
    /// <summary>Represents the underlying completion source that holds the promise's result.</summary>
    let tcs =
        TaskCompletionSource<Result<'R, 'E>> TaskCreationOptions.RunContinuationsAsynchronously

    /// <summary>Creates an effect that attempts to complete this promise with a success value.</summary>
    /// <param name="res">The success value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during completion to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when this call set the result, or <c>false</c> when the promise was already completed.</returns>
    member _.Succeed(res: 'R, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult(Ok res)), onError)

    /// <summary>Creates an effect that attempts to complete this promise with a typed error.</summary>
    /// <param name="err">The typed error to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during completion to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when this call set the result, or <c>false</c> when the promise was already completed.</returns>
    member _.Fail(err: 'E, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult(Error err)), onError)

    /// <summary>Creates an effect that attempts to complete this promise with a <c>Result</c> value.</summary>
    /// <param name="res">The result value to publish.</param>
    /// <param name="onError">A function that maps an exception thrown during completion to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when this call set the result, or <c>false</c> when the promise was already completed.</returns>
    member _.Complete(res: Result<'R, 'E>, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult res), onError)

    /// <summary>Creates an effect that suspends until this promise is completed and surfaces the result.</summary>
    /// <param name="onError">A function that maps an exception thrown while awaiting to the typed error.</param>
    /// <returns>An effect that completes with the promise's success value, or fails with the promise's typed error.</returns>
    member _.Await(onError: exn -> 'E) : FIO<'R, 'E> =
        FIO
            .awaitGenericTask(tcs.Task, onError)
            .FlatMap(fun res ->
                match res with
                | Ok res -> FIO.succeed res
                | Error err -> FIO.fail err)

    /// <summary>Returns the current state of this promise without blocking.</summary>
    /// <param name="onError">A function that maps an exception thrown during the poll to the typed error.</param>
    /// <returns>An effect that completes with <c>Some</c> wrapping the result when the promise is completed, or <c>None</c> when it is still pending.</returns>
    member _.Poll(onError: exn -> 'E) : FIO<Result<'R, 'E> option, 'E> =
        FIO.attempt ((fun () -> if tcs.Task.IsCompleted then Some tcs.Task.Result else None), onError)

    /// <summary>Returns whether this promise has reached a completed state.</summary>
    /// <returns>An effect that completes with <c>true</c> when the promise has been resolved; <c>false</c> while it is still pending.</returns>
    member _.IsDone() : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.Task.IsCompleted), fun ex -> raise ex)

/// <summary>Provides factory functions for creating and completing promises.</summary>
module Promise =
    /// <summary>Creates a new uncompleted promise.</summary>
    /// <typeparam name="'R">The success result type the promise may complete with.</typeparam>
    /// <typeparam name="'E">The typed error type the promise may complete with.</typeparam>
    /// <returns>An effect that completes with a fresh, pending promise.</returns>
    let make<'R, 'E> () : FIO<Promise<'R, 'E>, 'E> =
        FIO.attempt ((fun () -> Promise<'R, 'E>()), fun ex -> raise ex)

    /// <summary>Creates a promise that is already in the succeeded state with the given value.</summary>
    /// <typeparam name="'R">The success result type the promise carries.</typeparam>
    /// <typeparam name="'E">The typed error type the promise may complete with.</typeparam>
    /// <param name="res">The success value to publish.</param>
    /// <param name="onError">A function invoked if the promise has already been completed.</param>
    /// <returns>An effect that completes with a promise pre-resolved to <paramref name="res"/>.</returns>
    let succeed<'R, 'E> (res: 'R, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Succeed(res, onError)
            return promise
        }

    /// <summary>Creates a promise that is already in the failed state with the given typed error.</summary>
    /// <typeparam name="'R">The success result type the promise may complete with.</typeparam>
    /// <typeparam name="'E">The typed error type the promise carries.</typeparam>
    /// <param name="err">The typed error to publish.</param>
    /// <param name="onError">A function invoked if the promise has already been completed.</param>
    /// <returns>An effect that completes with a promise pre-resolved to <paramref name="err"/>.</returns>
    let fail<'R, 'E> (err: 'E, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Fail(err, onError)
            return promise
        }

    /// <summary>Combines a promise with an effect, completing the promise with the effect's outcome.</summary>
    /// <typeparam name="'R">The success result type produced by <paramref name="eff"/> and held by <paramref name="promise"/>.</typeparam>
    /// <typeparam name="'E">The typed error type produced by <paramref name="eff"/> and held by <paramref name="promise"/>.</typeparam>
    /// <param name="promise">The promise to complete.</param>
    /// <param name="eff">The effect whose outcome resolves the promise.</param>
    /// <param name="onError">A function that maps an exception thrown by completion to the typed error.</param>
    /// <returns>An effect that completes with unit once the promise has been resolved with <paramref name="eff"/>'s outcome.</returns>
    let completeWith<'R, 'E> (promise: Promise<'R, 'E>, eff: FIO<'R, 'E>, onError: exn -> 'E) : FIO<unit, 'E> =
        eff
            .FlatMap(fun value -> promise.Succeed(value, onError).Map(fun _ -> ()))
            .CatchAll(fun error -> promise.Fail(error, onError).Map(fun _ -> ()))
