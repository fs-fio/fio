namespace FIO.DSL

open System.Threading.Tasks

/// A variable that is completed once, with a value or an error, and that any number of fibers can await.
[<Sealed>]
type Promise<'A, 'E>() =

    // Never faulted or cancelled: a typed error is stored as a value, so an unawaited one raises no task exception.
    let completion = TaskCompletionSource<obj> TaskCreationOptions.RunContinuationsAsynchronously

    /// Returns an effect that completes this promise with the given value, yielding whether this call completed it.
    member _.Succeed<'E1> (value: 'A) : FIO<bool, 'E1> =
        FIO.succeedWith (fun () -> completion.TrySetResult(box (Ok value : Result<'A, 'E>)))

    /// Returns an effect that completes this promise with the given error, yielding whether this call completed it.
    member _.Fail<'E1> (error: 'E) : FIO<bool, 'E1> =
        FIO.succeedWith (fun () -> completion.TrySetResult(box (Error error : Result<'A, 'E>)))

    /// Returns an effect that waits for this promise, succeeding with its value or failing with its error.
    member _.Await () : FIO<'A, 'E> =
        AwaitTaskNode<obj, 'E>(completion.Task, Rethrow<_>.Instance).FlatMap(fun outcome ->
            FIO.fromResult (outcome :?> Result<'A, 'E>))

    /// Returns an effect that yields this promise's outcome if it has been completed, or None if it has not.
    member _.Poll<'E1> () : FIO<Result<'A, 'E> option, 'E1> =
        FIO.succeedWith (fun () ->
            if completion.Task.IsCompleted then Some(completion.Task.Result :?> Result<'A, 'E>) else None)
