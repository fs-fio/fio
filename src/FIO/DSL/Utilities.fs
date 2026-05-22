/// <summary>Provides utility modules for the FIO DSL: type-casting helpers and lock-free atomic primitives.</summary>
[<AutoOpen>]
module internal FIO.DSL.Utilities

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

/// <summary>Provides intent-revealing wrappers around <c>System.Threading.Interlocked</c> primitives used to implement one-shot flags, state-machine transitions, and lazy double-checked initialization of nullable reference fields.</summary>
/// <remarks>Every helper is <c>inline</c> so the call site expands to the same IL as a direct <c>Interlocked</c> call — no allocation, no virtual dispatch, no measurable overhead.</remarks>
[<AutoOpen>]
module internal Atomics =

    /// <summary>Atomically transitions a one-shot integer flag from <c>0</c> to <c>1</c>.</summary>
    /// <param name="flag">The flag field to claim, passed by reference.</param>
    /// <returns><c>true</c> when this caller flipped the flag from <c>0</c> to <c>1</c>; <c>false</c> when another thread already claimed it.</returns>
    let inline tryClaim (flag: byref<int>) : bool =
        Interlocked.CompareExchange(&flag, 1, 0) = 0

    /// <summary>Atomically transitions an integer field from <paramref name="expected"/> to <paramref name="next"/>.</summary>
    /// <param name="field">The field to transition, passed by reference.</param>
    /// <param name="expected">The value that must currently be in <paramref name="field"/> for the transition to succeed.</param>
    /// <param name="next">The value to install when <paramref name="field"/> equals <paramref name="expected"/>.</param>
    /// <returns><c>true</c> when this caller drove the transition; <c>false</c> when the field already held some other value.</returns>
    let inline tryTransition (field: byref<int>) (expected: int) (next: int) : bool =
        Interlocked.CompareExchange(&field, next, expected) = expected

    /// <summary>Atomically attempts to transition an integer field from <paramref name="expected"/> to <paramref name="next"/> and returns the value that was observed before the attempt.</summary>
    /// <param name="field">The field to transition, passed by reference.</param>
    /// <param name="expected">The value required for the transition to succeed.</param>
    /// <param name="next">The value installed on success.</param>
    /// <returns>The prior value of <paramref name="field"/>. Equal to <paramref name="expected"/> on success; some other value on failure.</returns>
    let inline transitionFrom (field: byref<int>) (expected: int) (next: int) : int =
        Interlocked.CompareExchange(&field, next, expected)

    /// <summary>Performs a lock-free double-checked initialization of a nullable reference field. If the field is already non-null, the existing value is returned; otherwise the factory is invoked and its result is installed via a single <c>Interlocked.CompareExchange</c>.</summary>
    /// <param name="field">The field to initialize, passed by reference.</param>
    /// <param name="factory">A factory invoked at most once per losing race to produce a candidate value; the candidate may be discarded if another thread wins.</param>
    /// <returns>The instance now stored in <paramref name="field"/> — either the existing one, the freshly created one, or the one installed by the winning concurrent caller.</returns>
    let inline initIfNull< ^T when ^T : not struct and ^T : null>
        (field: byref< ^T >) (factory: unit -> ^T) : ^T =
        let existing = Volatile.Read &field
        if not (isNull existing) then
            existing
        else
            let created = factory ()
            let prev = Interlocked.CompareExchange(&field, created, null)
            if isNull prev then created else prev
