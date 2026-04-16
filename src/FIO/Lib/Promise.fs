/// Promise for one-shot synchronization between fibers.
[<AutoOpen>]
module FIO.Promise

open FIO.DSL

open System.Threading.Tasks

/// A one-shot synchronization primitive that can be completed once with success or failure, allowing multiple waiters.
[<Sealed>]
type Promise<'R, 'E> internal () =
    /// Underlying TCS for one-shot completion.
    let tcs =
        TaskCompletionSource<Result<'R, 'E>> TaskCreationOptions.RunContinuationsAsynchronously

    /// Completes the promise with a success value.
    /// <param name="res">The success value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing <c>true</c> if the promise was completed, <c>false</c> if already completed.</returns>
    member _.Succeed(res: 'R, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult(Ok res)), onError)

    /// Completes the promise with an error.
    /// <param name="err">The error value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing <c>true</c> if the promise was completed, <c>false</c> if already completed.</returns>
    member _.Fail(err: 'E, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult(Error err)), onError)

    /// Completes the promise with a Result value.
    /// <param name="res">The result value to complete the promise with.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing <c>true</c> if the promise was completed, <c>false</c> if already completed.</returns>
    member _.Complete(res: Result<'R, 'E>, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.TrySetResult res), onError)

    /// Semantically blocks until the promise is completed, then surfaces the result.
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the promise's success value or fails with the promise's error.</returns>
    member _.Await(onError: exn -> 'E) : FIO<'R, 'E> =
        FIO
            .awaitGenericTask(tcs.Task, onError)
            .FlatMap(fun res ->
                match res with
                | Ok res -> FIO.succeed res
                | Error err -> FIO.fail err)

    /// Non-blocking check for the promise result.
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces <c>Some result</c> if the promise is completed, <c>None</c> otherwise.</returns>
    member _.Poll(onError: exn -> 'E) : FIO<Result<'R, 'E> option, 'E> =
        FIO.attempt ((fun () -> if tcs.Task.IsCompleted then Some tcs.Task.Result else None), onError)

    /// Checks whether the promise has been completed.
    /// <returns>An effect producing <c>true</c> if the promise has been completed, <c>false</c> otherwise.</returns>
    member _.IsDone() : FIO<bool, 'E> =
        FIO.attempt ((fun () -> tcs.Task.IsCompleted), fun ex -> raise ex)

/// Factory functions for creating Promise instances.
module Promise =
    /// Creates a new empty (uncompleted) promise.
    /// <returns>An effect that produces a new uncompleted promise.</returns>
    let make<'R, 'E> () : FIO<Promise<'R, 'E>, 'E> =
        FIO.attempt ((fun () -> Promise<'R, 'E>()), fun ex -> raise ex)

    /// Creates a promise already completed with a success value.
    /// <param name="res">The success value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces a promise already completed with the success value.</returns>
    let succeed<'R, 'E> (res: 'R, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Succeed(res, onError)
            return promise
        }

    /// Creates a promise already completed with an error.
    /// <param name="err">The error value.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces a promise already completed with the error value.</returns>
    let fail<'R, 'E> (err: 'E, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Fail(err, onError)
            return promise
        }

    /// Runs an effect and completes the promise with its outcome (success or caught error).
    /// <param name="promise">The promise to complete.</param>
    /// <param name="eff">The effect whose result completes the promise.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that completes the promise with the outcome of the given effect.</returns>
    let completeWith<'R, 'E> (promise: Promise<'R, 'E>, eff: FIO<'R, 'E>, onError: exn -> 'E) : FIO<unit, 'E> =
        eff
            .FlatMap(fun value -> promise.Succeed(value, onError).Map(fun _ -> ()))
            .CatchAll(fun error -> promise.Fail(error, onError).Map(fun _ -> ()))
