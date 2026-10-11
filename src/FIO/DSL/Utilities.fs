namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module internal Utilities =

    [<AutoOpen>]
    module internal Casting =

        let inline boxOnError (onError: exn -> 'E) =
            fun (ex: exn) -> onError ex :> obj

        let inline boxFunc (compute: unit -> 'A) =
            fun () -> compute () :> obj

        let inline private boxTaskCore (boxResult: unit -> obj) (task: Task) =
            if task.IsCompletedSuccessfully then
                Task.FromResult <| boxResult ()
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

        let inline boxTask (task: Task<'A>) =
            boxTaskCore (fun () -> box task.Result) (task :> Task)

        let inline boxVoidTask (task: Task) =
            boxTaskCore (fun () -> box ()) task

    [<AutoOpen>]
    module internal Atomics =

        let inline tryClaim (flag: byref<int>) =
            Interlocked.CompareExchange(&flag, 1, 0) = 0

        let inline tryTransition (field: byref<int>) (expected: int) (next: int) =
            Interlocked.CompareExchange(&field, next, expected) = expected

        let inline transitionFrom (field: byref<int>) (expected: int) (next: int) =
            Interlocked.CompareExchange(&field, next, expected)

        let inline initIfNull< ^T when ^T : not struct and ^T : null> (field: byref< ^T >) (factory: unit -> ^T) =
            let existing = Volatile.Read &field
            if not (isNull existing) then
                existing
            else
                let created = factory ()
                let prev = Interlocked.CompareExchange(&field, created, null)
                if isNull prev then created
                else prev

    [<AutoOpen>]
    module internal Timeouts =

        let validateTimeout (paramName: string) (timeout: TimeSpan) =
            if timeout <> Timeout.InfiniteTimeSpan
               && (timeout < TimeSpan.Zero || timeout.TotalMilliseconds > float Int32.MaxValue) then
                raise (
                    ArgumentOutOfRangeException(
                        paramName,
                        timeout,
                        "The timeout must be between zero and Int32.MaxValue milliseconds, or Timeout.InfiniteTimeSpan."))
