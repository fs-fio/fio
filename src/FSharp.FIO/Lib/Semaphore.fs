/// <summary>
/// Semaphore for controlling concurrent access to resources.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.Semaphore

open System
open System.Threading

open FSharp.FIO.DSL

/// <summary>
/// Counting semaphore that limits concurrent access using SemaphoreSlim.
/// </summary>
[<Sealed>]
type Semaphore internal (permits: int) =
    let sem = new SemaphoreSlim(permits, permits)
    let mutable disposed = 0 // 0 = false, 1 = true (for atomic CAS)

    /// <summary>Acquires a permit, blocking until one is available.</summary>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Acquire (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.awaitTask(sem.WaitAsync(), onError)

    /// <summary>Acquires a permit, blocking until one is available.</summary>
    member _.AcquireExn () : FIO<unit, exn> =
        FIO.awaitTaskExn(sem.WaitAsync())

    /// <summary>Acquires a permit with a timeout, returning true if acquired or false if timed out.</summary>
    /// <param name="timeout">Maximum wait time.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.TryAcquire (timeout: TimeSpan, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.awaitGenericTask(sem.WaitAsync(timeout), onError)

    /// <summary>Acquires a permit with a timeout, returning true if acquired or false if timed out.</summary>
    /// <param name="timeout">Maximum wait time.</param>
    member _.TryAcquireExn (timeout: TimeSpan) : FIO<bool, exn> =
        FIO.awaitGenericTaskExn(sem.WaitAsync(timeout))

    /// <summary>Releases a permit back to the semaphore.</summary>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.Release (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt((fun () -> sem.Release() |> ignore), onError)

    /// <summary>Releases a permit back to the semaphore.</summary>
    member _.ReleaseExn () : FIO<unit, exn> =
        FIO.attemptExn(fun () -> sem.Release() |> ignore)

    /// <summary>Releases multiple permits back to the semaphore.</summary>
    /// <param name="count">Number of permits.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member _.ReleaseMany (count: int, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt((fun () -> sem.Release count |> ignore), onError)

    /// <summary>Releases multiple permits back to the semaphore.</summary>
    /// <param name="count">Number of permits.</param>
    member _.ReleaseManyExn(count: int) : FIO<unit, exn> =
        FIO.attemptExn(fun () -> sem.Release count |> ignore)

    /// <summary>Gets the current number of available permits.</summary>
    member _.Available<'E> () : FIO<int, 'E> =
        FIO.succeed sem.CurrentCount

    /// <summary>Executes an effect while holding a permit, automatically releasing when complete.</summary>
    /// <param name="effect">Effect to execute.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member this.WithPermit (effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.acquireRelease(
            this.Acquire onError,
            (fun () -> this.Release onError),
            fun () -> effect)

    /// <summary>Executes an effect while holding a permit, automatically releasing when complete.</summary>
    /// <param name="effect">Effect to execute.</param>
    member this.WithPermitExn (effect: FIO<'R, exn>) : FIO<'R, exn> =
        FIO.acquireRelease(
            this.AcquireExn(),
            (fun () -> this.ReleaseExn()),
            fun () -> effect)

    /// <summary>Executes an effect while holding multiple permits, automatically releasing when complete.</summary>
    /// <param name="count">Number of permits.</param>
    /// <param name="effect">Effect to execute.</param>
    /// <param name="onError">Maps exceptions to error type.</param>
    member this.WithPermits (count: int, effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
        let acquireMany =
            FIO.collectAll(List.replicate count (this.Acquire onError))
                .Map(fun _ -> ())
        FIO.acquireRelease(
            acquireMany,
            (fun () -> this.ReleaseMany(count, onError)),
            fun () -> effect)

    /// <summary>Executes an effect while holding multiple permits, automatically releasing when complete.</summary>
    /// <param name="count">Number of permits.</param>
    /// <param name="effect">Effect to execute.</param>
    member this.WithPermitsExn (count: int, effect: FIO<'R, exn>) : FIO<'R, exn> =
        let acquireMany =
            FIO.collectAll(List.replicate count (this.AcquireExn()))
                .Map(fun _ -> ())
        FIO.acquireRelease(
            acquireMany,
            (fun () -> this.ReleaseManyExn count),
            fun () -> effect)

    member private _.Dispose (disposing: bool) =
        if Interlocked.CompareExchange(&disposed, 1, 0) = 0 then
            if disposing then
                sem.Dispose()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize () =
        this.Dispose false

/// <summary>Factory functions for creating Semaphore instances.</summary>
module Semaphore =
    /// <summary>Creates a new semaphore with the specified number of permits.</summary>
    /// <param name="permits">Initial number of permits (must be >= 1).</param>
    let make<'E> (permits: int) : FIO<Semaphore, 'E> =
        if permits < 1 then
            FIO.interrupt(InvalidArgument("permits", "must be >= 1"), "Invalid argument: permits must be >= 1")
        else
            FIO.succeed(new Semaphore(permits))

    /// <summary>Creates a binary semaphore with 1 permit.</summary>
    let binary<'E> () : FIO<Semaphore, 'E> =
        make 1
