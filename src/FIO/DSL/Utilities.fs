[<AutoOpen>]
module internal FIO.DSL.Utilities

open System
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module internal Casting =

    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) =
        fun (ex: exn) -> onError ex :> obj

    let inline upcastFunc (func: unit -> 'A) : unit -> obj =
        fun () -> func () :> obj

    let inline private wrapTaskCore (boxResult: unit -> obj) (task: Task) : Task<obj> =
        if task.IsCompletedSuccessfully then
            Task.FromResult(boxResult ())
        elif task.IsFaulted then
            Task.FromException<obj>(task.Exception.GetBaseException())
        elif task.IsCanceled then
            Task.FromCanceled<obj>(CancellationToken true)
        else
            let tcs =
                TaskCompletionSource<obj>
                    TaskCreationOptions.RunContinuationsAsynchronously

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
            ) |> ignore

            tcs.Task

    let inline upcastTask (task: Task<'A>) : Task<obj> =
        wrapTaskCore (fun () -> box task.Result) (task :> Task)

    let inline wrapVoidTask (task: Task) : Task<obj> =
        wrapTaskCore (fun () -> box ()) task

[<AutoOpen>]
module internal Atomics =

    let inline tryClaim (flag: byref<int>) : bool =
        Interlocked.CompareExchange(&flag, 1, 0) = 0

    let inline tryTransition (field: byref<int>) (expected: int) (next: int) : bool =
        Interlocked.CompareExchange(&field, next, expected) = expected

    let inline transitionFrom (field: byref<int>) (expected: int) (next: int) : int =
        Interlocked.CompareExchange(&field, next, expected)

    let inline initIfNull< ^T when ^T : not struct and ^T : null> (field: byref< ^T >) (factory: unit -> ^T) : ^T =
        let existing = Volatile.Read &field
        if not (isNull existing) then
            existing
        else
            let created = factory ()
            let prev = Interlocked.CompareExchange(&field, created, null)
            if isNull prev then created
            else prev
