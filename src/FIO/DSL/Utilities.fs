/// Internal type casting and conversion utilities.
[<AutoOpen>]
module private FIO.DSL.Utilities

open System
open System.Threading
open System.Threading.Tasks

/// Internal type upcasting utilities.
[<AutoOpen>]
module internal Casting =

    /// Upcasts an error mapping function to work with obj types.
    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) = fun (ex: exn) -> onError ex :> obj

    /// Upcasts a function's return type to obj.
    let inline upcastFunc (func: unit -> 'R) : unit -> obj = fun () -> func () :> obj

    /// Wraps a Task as a Task<obj>, boxing the result via boxResult on completion.
    let inline private wrapTaskCore (boxResult: unit -> obj) (task: Task) : Task<obj> =
        if task.IsCompletedSuccessfully then
            Task.FromResult(boxResult ())
        elif task.IsFaulted then
            Task.FromException<obj>(task.Exception.GetBaseException())
        elif task.IsCanceled then
            Task.FromCanceled<obj>(CancellationToken true)
        else
            let tcs =
                TaskCompletionSource<obj> TaskCreationOptions.RunContinuationsAsynchronously

            task.ContinueWith(
                Action<Task>(fun completedTask ->
                    if completedTask.IsCanceled then
                        tcs.TrySetCanceled() |> ignore
                    elif completedTask.IsFaulted then
                        tcs.TrySetException completedTask.Exception.InnerExceptions |> ignore
                    else
                        tcs.TrySetResult(boxResult ()) |> ignore),
                CancellationToken.None,
                TaskContinuationOptions.ExecuteSynchronously,
                TaskScheduler.Default
            )
            |> ignore

            tcs.Task

    /// Upcasts a generic Task's result type to obj.
    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        wrapTaskCore (fun () -> box genericTask.Result) (genericTask :> Task)

    /// Wraps a void Task as a Task&lt;obj&gt; returning boxed unit.
    let inline wrapVoidTask (task: Task) : Task<obj> =
        wrapTaskCore (fun () -> box ()) task
