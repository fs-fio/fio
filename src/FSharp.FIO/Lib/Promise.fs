/// <summary>
/// Promise for one-shot synchronization between fibers.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.Promise

open FSharp.FIO.DSL

open System.Threading.Tasks

/// <summary>
/// Promise that can be completed once with success or failure, allowing multiple waiters.
/// </summary>
[<Sealed>]
type Promise<'R, 'E> internal () =
    let tcs = TaskCompletionSource<Result<'R, 'E>> TaskCreationOptions.RunContinuationsAsynchronously

    /// <summary>Completes the promise with a success value, returning true if successful or false if already completed.</summary>
    /// <param name="res">Success value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Succeed (res: 'R, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt((fun () -> tcs.TrySetResult(Ok res)), onError)

    /// <summary>Completes the promise with a success value (for Promise with exn error type).</summary>
    /// <param name="res">Success value.</param>
    member _.SucceedExn (res: 'R) : FIO<bool, exn> =
        FIO.attemptExn(fun () -> tcs.TrySetResult(Ok res))

    /// <summary>Completes the promise with an error, returning true if successful or false if already completed.</summary>
    /// <param name="err">Error value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Fail (err: 'E, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt((fun () -> tcs.TrySetResult(Error err)), onError)

    /// <summary>Completes the promise with an error (for Promise with exn error type).</summary>
    /// <param name="err">Error value.</param>
    member _.FailExn (err: 'E) : FIO<bool, exn> =
        FIO.attemptExn(fun () -> tcs.TrySetResult(Error err))

    /// <summary>Completes the promise with a Result value, returning true if successful or false if already completed.</summary>
    /// <param name="res">Result value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Complete (res: Result<'R, 'E>, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.attempt((fun () -> tcs.TrySetResult res), onError)

    /// <summary>Completes the promise with a Result value (for Promise with exn error type).</summary>
    /// <param name="res">Result value.</param>
    member _.CompleteExn (res: Result<'R, 'E>) : FIO<bool, exn> =
        FIO.attemptExn(fun () -> tcs.TrySetResult res)

    /// <summary>Waits for the promise to be completed and returns the result.</summary>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Await (onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.awaitGenericTask(tcs.Task, onError)
            .FlatMap(fun res ->
                match res with
                | Ok res -> FIO.succeed res
                | Error err -> FIO.fail err)

    /// <summary>Waits for the promise to be completed (for Promise with exn error type).</summary>
    member _.AwaitExn () : FIO<'R, exn> =
        FIO.awaitGenericTaskExn(tcs.Task)
            .FlatMap(fun res ->
                match res with
                | Ok res -> FIO.succeed res
                | Error err ->
                    match box err with
                    | :? exn as e -> FIO.fail e
                    | _ -> FIO.fail (System.Exception $"Promise failed: {err}"))

    /// <summary>Tries to get the result if completed, returning None if not yet completed.</summary>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Poll (onError: exn -> 'E) : FIO<Result<'R, 'E> option, 'E> =
        FIO.attempt((fun () -> if tcs.Task.IsCompleted then Some tcs.Task.Result else None), onError)

    /// <summary>Tries to get the result if completed (for Promise with exn error type).</summary>
    member _.PollExn () : FIO<Result<'R, 'E> option, exn> =
        FIO.attemptExn(fun () -> if tcs.Task.IsCompleted then Some tcs.Task.Result else None)

    /// <summary>Checks if the promise has been completed.</summary>
    member _.IsDone () : FIO<bool, 'E> =
        FIO.succeed tcs.Task.IsCompleted

/// <summary>Factory functions for creating Promise instances.</summary>
module Promise =
    /// <summary>Creates a new empty promise.</summary>
    let make<'R, 'E> () : FIO<Promise<'R, 'E>, 'E> =
        FIO.succeed(Promise<'R, 'E>())

    /// <summary>Creates a promise already completed with a success value.</summary>
    /// <param name="res">Success value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    let succeed<'R, 'E> (res: 'R, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Succeed(res, onError)
            return promise
        }

    /// <summary>Creates a promise already completed with a success value (for exn error type).</summary>
    /// <param name="res">Success value.</param>
    let inline succeedExn<'R> (res: 'R) : FIO<Promise<'R, exn>, exn> =
        succeed<'R, exn>(res, id)

    /// <summary>Creates a promise already completed with an error.</summary>
    /// <param name="err">Error value.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    let fail<'R, 'E> (err: 'E, onError: exn -> 'E) : FIO<Promise<'R, 'E>, 'E> =
        fio {
            let! promise = make<'R, 'E> ()
            let! _ = promise.Fail(err, onError)
            return promise
        }

    /// <summary>Creates a promise already completed with an error (for exn error type).</summary>
    /// <param name="err">Error value.</param>
    let inline failExn<'R> (err: exn) : FIO<Promise<'R, exn>, exn> =
        fail<'R, exn>(err, id)

    /// <summary>Completes the promise from the result of an effect.</summary>
    /// <param name="promise">Promise to complete.</param>
    /// <param name="eff">Effect whose result completes the promise.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    let completeWith<'R, 'E> (promise: Promise<'R, 'E>, eff: FIO<'R, 'E>, onError: exn -> 'E) : FIO<unit, 'E> =
        eff.FlatMap(fun value -> promise.Succeed(value, onError).Map(fun _ -> ()))
           .CatchAll(fun error -> promise.Fail(error, onError).Map(fun _ -> ()))

    /// <summary>Completes the promise from the result of an effect (for exn error type).</summary>
    /// <param name="promise">Promise to complete.</param>
    /// <param name="eff">Effect whose result completes the promise.</param>
    let inline completeWithExn<'R> (promise: Promise<'R, exn>, eff: FIO<'R, exn>) : FIO<unit, exn> =
        completeWith<'R, exn>(promise, eff, id)
