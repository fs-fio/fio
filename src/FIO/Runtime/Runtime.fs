namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

module internal ContStackLimits =
    [<Literal>]
    let DefaultCapacity = 32

    [<Literal>]
    let MaxPool = 256

    [<Literal>]
    let MaxDepth = 4096

[<AbstractClass>]
type FIORuntime internal () =

    // Root and daemon fibers not yet unwound; a fiber leaves when it unwinds, so nothing finished stays reachable. Each
    // stripe keeps the capacity of its peak, which is bounded by the peak number of live fibers.
    let stripes = Array.init 16 (fun _ -> HashSet<FiberContext> HashIdentity.Reference)

    [<VolatileField>]
    let mutable disposed = 0

    // Set once Shutdown has interrupted the fibers it found: from then on the last fiber to leave completes the wait.
    [<VolatileField>]
    let mutable draining = 0

    let unwound =
        TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let stopped =
        TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let disposedMessage = "The runtime was disposed."

    let stripeOf (fiberContext: FiberContext) =
        stripes[RuntimeHelpers.GetHashCode fiberContext &&& (stripes.Length - 1)]

    let snapshot () =
        stripes
        |> Array.collect (fun stripe -> lock stripe (fun () -> Seq.toArray stripe))

    let trackedCount () =
        let mutable count = 0

        for stripe in stripes do
            count <- count + lock stripe (fun () -> stripe.Count)

        count

    // One delegate per runtime, installed on every tracked fiber. The flag is read under the stripe's lock, which
    // Shutdown takes for its count only after setting the flag: a fiber the count saw sees the flag when it leaves.
    let onUnwound =
        Action<FiberContext>(fun fiberContext ->
            let stripe = stripeOf fiberContext

            let awaited =
                lock stripe (fun () ->
                    stripe.Remove fiberContext |> ignore
                    Volatile.Read &draining = 1)

            if awaited && trackedCount () = 0 then
                unwound.TrySetResult() |> ignore)

    /// This runtime's name.
    abstract member Name: string

    /// Schedules the given effect on a new fiber and returns its handle at once; it never waits for, interrupts, or
    /// discards a fiber already running, so call it as often as you like.
    abstract member Run<'A, 'E> : FIO<'A, 'E> -> Fiber<'A, 'E>

    override this.ToString() : string = this.Name

    member val internal StopWorkers: unit -> unit = ignore with get, set

    // The flag is read under the stripe's lock, which Shutdown takes for its snapshot only after claiming the flag:
    // a Run the snapshot missed sees the flag and interrupts itself.
    member private _.Watch(fiberContext: FiberContext) =
        let stripe = stripeOf fiberContext

        let late =
            lock stripe (fun () ->
                stripe.Add fiberContext |> ignore
                Volatile.Read &disposed = 1)

        fiberContext.SetOnUnwound onUnwound

        if late then
            fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)

    member internal this.Track(fiberContext: FiberContext) =
        if Volatile.Read &disposed = 1 then
            raise (ObjectDisposedException(this.Name, disposedMessage))

        this.Watch fiberContext

    member internal this.TrackDaemon(fiberContext: FiberContext) = this.Watch fiberContext

    member internal _.TrackedCount = trackedCount ()

    /// Interrupts every fiber still running, waits up to the given time for them to unwind, then stops the workers and
    /// abandons what is left; a later call waits for the first. Do not call it from one of this runtime's fibers.
    member this.Shutdown(timeout: TimeSpan) : unit =
        validateTimeout (nameof timeout) timeout

        if tryClaim &disposed then
            // A fiber whose handle was disposed, or whose cancellation callback throws, must not stop the rest.
            try
                for fiberContext in snapshot () do
                    try
                        fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)
                    with _ ->
                        ()

                Volatile.Write(&draining, 1)

                if trackedCount () > 0 && not (unwound.Task.Wait timeout) then
                    for fiberContext in snapshot () do
                        fiberContext.Abandon()
            finally
                try
                    this.StopWorkers()
                finally
                    stopped.TrySetResult() |> ignore
        else
            stopped.Task.Wait timeout |> ignore

    interface IDisposable with

        member this.Dispose() : unit =
            this.Shutdown(TimeSpan.FromSeconds 10.0)

/// Worker counts and scheduling parameters for a worker-based runtime.
type WorkerConfig =
    {
        /// Number of workers that evaluate effects.
        EvaluationWorkers: int
        /// Number of evaluation steps a work item runs before being rescheduled.
        EvaluationSteps: int
    }

    /// The default configuration, sized to the current machine.
    static member Default: WorkerConfig =
        {
            EvaluationWorkers = max 2 (Environment.ProcessorCount - 1)
            EvaluationSteps = 200
        }
