/// Semaphore for controlling concurrent access to resources.
[<AutoOpen>]
module FIO.Semaphore

open System
open System.Threading

open FIO.DSL

/// Counting semaphore that limits concurrent access using SemaphoreSlim.
[<Sealed>]
type Semaphore internal (permits: int) =
    let sem = new SemaphoreSlim(permits, permits)
    let mutable disposed = 0

    /// Acquires a permit, semantically blocking until one is available.
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that acquires a permit.</returns>
    member _.Acquire(onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.awaitTask (sem.WaitAsync(), onError)

    /// Tries to acquire a permit within a timeout.
    /// <param name="timeout">The maximum time to wait for a permit.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect producing <c>true</c> if the permit was acquired, <c>false</c> on timeout.</returns>
    member _.TryAcquire(timeout: TimeSpan, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.awaitGenericTask (sem.WaitAsync(timeout), onError)

    /// Releases a single permit back to the semaphore.
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that releases a permit.</returns>
    member _.Release(onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> sem.Release() |> ignore), onError)

    /// Releases multiple permits back to the semaphore.
    /// <param name="count">Number of permits to release.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that releases the specified number of permits.</returns>
    member _.ReleaseMany(count: int, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> sem.Release count |> ignore), onError)

    /// Gets the current number of available permits.
    /// <returns>An effect that produces the number of available permits.</returns>
    member _.Available<'E>() : FIO<int, 'E> =
        FIO.attempt ((fun () -> sem.CurrentCount), fun ex -> raise ex)

    /// Runs an effect while holding a permit, releasing it on completion, error, or interruption.
    /// <param name="effect">The effect to run under the permit.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the result of the given effect while holding a permit.</returns>
    member this.WithPermit(effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.acquireRelease (this.Acquire onError, (fun () -> this.Release onError), fun () -> effect)

    /// Runs an effect while holding multiple permits, releasing them on completion, error, or interruption.
    /// <param name="count">Number of permits to acquire.</param>
    /// <param name="effect">The effect to run under the permits.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>An effect that produces the result of the given effect while holding the permits.</returns>
    member this.WithPermits(count: int, effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
        // Nest acquireRelease per permit so each successful acquire independently sets up its own release.
        // If acquire k+1 fails, only the k already-acquired permits are released — not the full count.
        FIO.suspend (fun () ->
            let rec loop remaining =
                if remaining <= 0 then
                    effect
                else
                    FIO.acquireRelease (
                        this.Acquire onError,
                        (fun () -> this.Release onError),
                        fun () -> loop (remaining - 1)
                    )

            loop count)

    /// Disposes the underlying SemaphoreSlim if not already disposed.
    member private _.Dispose(disposing: bool) =
        if Interlocked.CompareExchange(&disposed, 1, 0) = 0 then
            if disposing then
                sem.Dispose()

    interface IDisposable with
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize() = this.Dispose false

/// Factory functions for creating Semaphore instances.
module Semaphore =
    /// Creates a new semaphore with the specified number of permits.
    /// <param name="permits">Initial number of permits (must be >= 1).</param>
    /// <returns>Interrupts with InvalidArgument if permits is less than 1.</returns>
    let make<'E> (permits: int) : FIO<Semaphore, 'E> =
        if permits < 1 then
            FIO.interrupt (InvalidArgument("permits", "must be >= 1"), "Invalid argument: permits must be >= 1")
        else
            FIO.attempt ((fun () -> new Semaphore(permits)), fun ex -> raise ex)

    /// Creates a binary semaphore (mutex) with 1 permit.
    /// <returns>An effect that produces a new semaphore with 1 permit.</returns>
    let binary<'E> () : FIO<Semaphore, 'E> = make 1
