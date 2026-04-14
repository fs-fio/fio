/// <summary>
/// Internal utilities for type casting and conversion used across the FIO DSL.
/// </summary>
[<AutoOpen>]
module private FIO.DSL.Utilities

open System
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Internal utilities for type upcasting to obj.
/// </summary>
[<AutoOpen>]
module internal Casting =

    /// <summary>
    /// Upcasts an error mapping function to work with obj types.
    /// </summary>
    /// <param name="onError">The error mapping function.</param>
    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) = fun (ex: exn) -> onError ex :> obj

    /// <summary>
    /// Upcasts a function's return type to obj.
    /// </summary>
    /// <param name="func">The function to upcast.</param>
    let inline upcastFunc (func: unit -> 'R) : unit -> obj = fun () -> func () :> obj

    /// <summary>
    /// Upcasts a generic Task's result type to obj.
    /// </summary>
    /// <param name="genericTask">The task to upcast.</param>
    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        if genericTask.IsCompletedSuccessfully then
            Task.FromResult(box genericTask.Result)
        elif genericTask.IsFaulted then
            Task.FromException<obj>(genericTask.Exception.GetBaseException())
        elif genericTask.IsCanceled then
            Task.FromCanceled<obj>(CancellationToken true)
        else
            let tcs =
                TaskCompletionSource<obj> TaskCreationOptions.RunContinuationsAsynchronously

            genericTask.ContinueWith(
                Action<Task<'R>>(fun completedTask ->
                    if completedTask.IsCanceled then
                        tcs.TrySetCanceled() |> ignore
                    elif completedTask.IsFaulted then
                        tcs.TrySetException completedTask.Exception.InnerExceptions |> ignore
                    else
                        tcs.TrySetResult(box completedTask.Result) |> ignore),
                CancellationToken.None,
                TaskContinuationOptions.ExecuteSynchronously,
                TaskScheduler.Default
            )
            |> ignore

            tcs.Task
