namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

/// <summary>A fixed number of permits that bounds how many effects run at once; fewer than 1 permit throws. With
/// <c>System.Threading</c> opened after <c>FIO.DSL</c>, the name <c>Semaphore</c> means that namespace's type.</summary>
[<Sealed>]
type Semaphore(permits: int) =

    do
        if permits < 1 then
            raise (ArgumentOutOfRangeException(nameof permits, permits, "A semaphore needs at least 1 permit."))

    let gate = new SemaphoreSlim(permits, permits)

    // A wait the fiber gave up on can still be granted, so the permit goes back exactly when the wait succeeds. The
    // continuation is attached whatever the wait's state: one that completes during the check still decides itself.
    let returnWhenGranted (wait: Task) =
        if wait.IsCompletedSuccessfully then
            gate.Release() |> ignore
        else
            wait.ContinueWith(
                (fun (granted: Task) -> if granted.IsCompletedSuccessfully then gate.Release() |> ignore),
                CancellationToken.None,
                TaskContinuationOptions.ExecuteSynchronously,
                TaskScheduler.Default)
            |> ignore

    /// The number of permits no fiber holds.
    member _.Available : int =
        gate.CurrentCount

    /// Returns an effect that waits for a permit, interruptibly unless the caller is uninterruptible, runs the given
    /// effect holding it, and returns the permit however the effect ends.
    member _.WithPermit<'A, 'E> (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.cancellationToken().FlatMap <| fun cancellationToken ->
            FIO.acquireReleaseWith
                (FIO.succeedWith (fun () -> gate.WaitAsync cancellationToken))
                (fun wait -> FIO.succeedWith (fun () -> returnWhenGranted wait))
                (fun wait -> (FIO.awaitUnitTask wait Rethrow<_>.Instance).FlatMap(fun () -> effect))
