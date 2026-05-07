/// <summary>Provides private utility modules for the FIO DSL.</summary>
[<AutoOpen>]
module private FIO.DSL.Utilities

open System
open System.Threading
open System.Threading.Tasks

/// <summary>Provides type-casting helpers that erase generic type parameters to <c>obj</c> for internal effect representation.</summary>
[<AutoOpen>]
module internal Casting =

    /// <summary>Transforms a typed error handler into one that returns <c>obj</c>.</summary>
    /// <param name="onError">The typed error handler to upcast.</param>
    /// <returns>An error handler whose return type is erased to <c>obj</c>.</returns>
    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) = fun (ex: exn) -> onError ex :> obj

    /// <summary>Transforms a typed thunk into one that returns <c>obj</c>.</summary>
    /// <param name="func">The typed thunk to upcast.</param>
    /// <returns>A thunk whose return type is erased to <c>obj</c>.</returns>
    let inline upcastFunc (func: unit -> 'R) : unit -> obj = fun () -> func () :> obj

    /// <summary>Transforms a completed or pending <c>Task</c> into a <c>Task&lt;obj&gt;</c> using the supplied boxing function.</summary>
    /// <param name="boxResult">A function that boxes the completed task's result into <c>obj</c>.</param>
    /// <param name="task">The task to wrap.</param>
    /// <returns>A <c>Task&lt;obj&gt;</c> that mirrors the original task's outcome with the result boxed.</returns>
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

    /// <summary>Transforms a generic <c>Task&lt;'R&gt;</c> into a <c>Task&lt;obj&gt;</c> by boxing its result.</summary>
    /// <param name="genericTask">The typed task to upcast.</param>
    /// <returns>A <c>Task&lt;obj&gt;</c> that completes with the boxed result of the original task.</returns>
    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        wrapTaskCore (fun () -> box genericTask.Result) (genericTask :> Task)

    /// <summary>Transforms a non-generic <c>Task</c> into a <c>Task&lt;obj&gt;</c> that completes with boxed unit.</summary>
    /// <param name="task">The void task to wrap.</param>
    /// <returns>A <c>Task&lt;obj&gt;</c> that completes with boxed unit when the original task finishes.</returns>
    let inline wrapVoidTask (task: Task) : Task<obj> = wrapTaskCore (fun () -> box ()) task
