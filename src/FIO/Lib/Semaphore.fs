/// <summary>Provides semaphores for controlling concurrent access to limited resources.</summary>
[<AutoOpen>]
module FIO.Semaphore

open System
open System.Threading

open FIO.DSL

/// <summary>Represents a counting semaphore that bounds the number of fibers holding a permit at any moment.</summary>
[<Sealed>]
type Semaphore internal (permits: int) =
    /// <summary>Represents the underlying synchronization primitive.</summary>
    let sem = new SemaphoreSlim(permits, permits)
    /// <summary>Represents the disposed flag; non-zero means the semaphore has been disposed.</summary>
    let mutable disposed = 0

    /// <summary>Creates an effect that acquires a single permit, suspending until one becomes available.</summary>
    /// <param name="onError">A function that maps an exception thrown during the acquire to the typed error.</param>
    /// <returns>An effect that completes with unit once a permit has been acquired.</returns>
    member _.Acquire(onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.awaitTask (sem.WaitAsync(), onError)

    /// <summary>Creates an effect that attempts to acquire a single permit within a deadline.</summary>
    /// <param name="timeout">The maximum time to wait for a permit.</param>
    /// <param name="onError">A function that maps an exception thrown during the acquire to the typed error.</param>
    /// <returns>An effect that completes with <c>true</c> when a permit was acquired, or <c>false</c> when the timeout fired first.</returns>
    member _.TryAcquire(timeout: TimeSpan, onError: exn -> 'E) : FIO<bool, 'E> =
        FIO.awaitGenericTask (sem.WaitAsync(timeout), onError)

    /// <summary>Creates an effect that releases a single permit back to this semaphore.</summary>
    /// <param name="onError">A function that maps an exception thrown during the release to the typed error.</param>
    /// <returns>An effect that completes with unit once the permit has been returned.</returns>
    member _.Release(onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> sem.Release() |> ignore), onError)

    /// <summary>Creates an effect that releases several permits back to this semaphore at once.</summary>
    /// <param name="count">The number of permits to return; must be positive.</param>
    /// <param name="onError">A function that maps an exception thrown during the release to the typed error.</param>
    /// <returns>An effect that completes with unit once <paramref name="count"/> permits have been returned.</returns>
    member _.ReleaseMany(count: int, onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt ((fun () -> sem.Release count |> ignore), onError)

    /// <summary>Returns the number of permits currently available in this semaphore.</summary>
    /// <returns>An effect that completes with the unused permit count.</returns>
    member _.Available<'E>() : FIO<int, 'E> =
        FIO.attempt ((fun () -> sem.CurrentCount), fun ex -> raise ex)

    /// <summary>Builds a resource-managed effect that holds a permit while running the supplied effect.</summary>
    /// <typeparam name="'R">The success result type produced by <paramref name="effect"/>.</typeparam>
    /// <param name="effect">The effect to evaluate while holding the permit.</param>
    /// <param name="onError">A function that maps an exception thrown during acquire or release to the typed error.</param>
    /// <returns>An effect that completes with <paramref name="effect"/>'s result and always releases the permit afterwards.</returns>
    member this.WithPermit(effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
        FIO.acquireRelease (this.Acquire onError, (fun () -> this.Release onError), fun () -> effect)

    /// <summary>Builds a resource-managed effect that holds several permits while running the supplied effect.</summary>
    /// <typeparam name="'R">The success result type produced by <paramref name="effect"/>.</typeparam>
    /// <param name="count">The number of permits to acquire; must be positive.</param>
    /// <param name="effect">The effect to evaluate while holding the permits.</param>
    /// <param name="onError">A function that maps an exception thrown during acquire or release to the typed error.</param>
    /// <returns>An effect that completes with <paramref name="effect"/>'s result and releases every successfully acquired permit afterwards.</returns>
    /// <remarks>If acquiring the kth permit fails, only the k-1 permits already held are released — partial acquisition is unwound safely.</remarks>
    member this.WithPermits(count: int, effect: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
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

    /// <summary>Transforms the semaphore by releasing all managed and unmanaged resources.</summary>
    /// <param name="disposing">When <c>true</c>, managed resources are released; when <c>false</c>, only unmanaged resources are released.</param>
    member private _.Dispose(disposing: bool) =
        if Interlocked.CompareExchange(&disposed, 1, 0) = 0 then
            if disposing then
                sem.Dispose()

    interface IDisposable with

        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize() = this.Dispose false

/// <summary>Provides factory functions for creating counting semaphores.</summary>
module Semaphore =
    /// <summary>Creates a new counting semaphore with the specified number of permits.</summary>
    /// <param name="permits">The initial permit count; must be at least 1.</param>
    /// <returns>An effect that completes with the new semaphore, or interrupts the current fiber when <paramref name="permits"/> is invalid.</returns>
    let make<'E> (permits: int) : FIO<Semaphore, 'E> =
        if permits < 1 then
            FIO.interrupt (InvalidArgument("permits", "must be >= 1"), "Invalid argument: permits must be >= 1")
        else
            FIO.attempt ((fun () -> new Semaphore(permits)), fun ex -> raise ex)

    /// <summary>Creates a new binary semaphore that admits a single holder at a time.</summary>
    /// <returns>An effect that completes with a semaphore initialized with one permit.</returns>
    let binary<'E> () : FIO<Semaphore, 'E> = make 1
