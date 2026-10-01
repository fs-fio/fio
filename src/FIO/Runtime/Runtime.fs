namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

module internal WorkerRuntimeDefaults =
    let ProcessorReserve = 1

    let MinimumEvaluationWorkerCount = 2

    let EvaluationWorkerSteps = 200

    let BlockingWorkerCount = 1

    let ComputeEvaluationWorkerCount () =
        let availableWorkers = Environment.ProcessorCount - ProcessorReserve

        if availableWorkers >= MinimumEvaluationWorkerCount then
            availableWorkers
        else
            MinimumEvaluationWorkerCount

type internal ContStackPool private () =
    static let DefaultStackCapacity = 32
    static let MaxPoolSize = 256
    static let MaxReturnedStackDepth = 4096

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<Stack<Cont>>

    static member inline Rent () =
        let mutable pool = ContStackPool.pool
        if isNull pool then
            pool <- Stack<_>()
            ContStackPool.pool <- pool

        if pool.Count > 0 then
            let stack = pool.Pop()
            stack.Clear()
            stack
        else
            Stack<Cont> DefaultStackCapacity

    static member inline Return (stack: Stack<Cont>) =
        let mutable pool = ContStackPool.pool
        if isNull pool then
            pool <- Stack<_>()
            ContStackPool.pool <- pool

        if pool.Count < MaxPoolSize && stack.Count <= MaxReturnedStackDepth then
            stack.Clear()
            pool.Push stack

type internal WorkItemPool private () =
    static let MaxPoolSize = 512

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<WorkItem>

    static member inline Rent (effect: FIO<obj, obj>, fiberContext: FiberContext, contStack: Stack<Cont>) =
        let mutable pool = WorkItemPool.pool
        if isNull pool then
            pool <- Stack<WorkItem>()
            WorkItemPool.pool <- pool

        if pool.Count > 0 then
            let workItem = pool.Pop()
            workItem.Effect <- effect
            workItem.FiberContext <- fiberContext
            workItem.ContStack <- contStack
            workItem.InterruptionSuppressed <- 0
            workItem
        else
            {
                Effect = effect
                FiberContext = fiberContext
                ContStack = contStack
                InterruptionSuppressed = 0
            }

    static member inline Return (workItem: WorkItem) =
        let mutable pool = WorkItemPool.pool

        if isNull pool then
            pool <- Stack<WorkItem>()
            WorkItemPool.pool <- pool

        if pool.Count < MaxPoolSize then
            workItem.Effect <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.ContStack <- Unchecked.defaultof<_>
            workItem.InterruptionSuppressed <- 0
            pool.Push workItem

type internal WorkStealingDeque(initialCapacity: int) =
    let mutable items = Array.zeroCreate initialCapacity

    let mutable mask = initialCapacity - 1

    let mutable bottom = 0

    let mutable top = 0

    let gate = obj ()

    member _.IsEmpty =
        Monitor.Enter gate
        try
            bottom = top
        finally
            Monitor.Exit gate

    member _.IsEmptyApprox =
        bottom = top

    member _.PushBottom (workItem: WorkItem) =
        Monitor.Enter gate
        try
            if bottom - top >= items.Length then
                let count = bottom - top
                let grown = Array.zeroCreate (items.Length * 2)
                for i in 0 .. count - 1 do
                    grown.[i] <- items.[(top + i) &&& mask]
                items <- grown
                mask <- grown.Length - 1
                top <- 0
                bottom <- count
            items.[bottom &&& mask] <- workItem
            bottom <- bottom + 1
        finally
            Monitor.Exit gate

    member _.TryPopBottom (workItem: byref<WorkItem>) =
        Monitor.Enter gate
        try
            if bottom = top then
                false
            else
                bottom <- bottom - 1
                workItem <- items.[bottom &&& mask]
                items.[bottom &&& mask] <- Unchecked.defaultof<_>
                true
        finally
            Monitor.Exit gate

    member _.TrySteal (workItem: byref<WorkItem>) =
        Monitor.Enter gate
        try
            if bottom = top then
                false
            else
                workItem <- items.[top &&& mask]
                items.[top &&& mask] <- Unchecked.defaultof<_>
                top <- top + 1
                true
        finally
            Monitor.Exit gate

// One thread's root fibers: Add by that thread only, Snapshot by Shutdown, so the lock is uncontended until then.
// Pruned of unwound fibers each time it doubles, so a fiber is examined a bounded number of times.
type internal TrackedFibers () =
    let fibers = ResizeArray<FiberContext>()
    let mutable pruneAt = 64

    member _.Add (fiberContext: FiberContext, isDisposed: Func<bool>) =
        lock fibers (fun () ->
            fibers.Add fiberContext

            if fibers.Count >= pruneAt then
                fibers.RemoveAll(fun tracked -> tracked.HasUnwound) |> ignore
                pruneAt <- max 64 (fibers.Count * 2)

            isDisposed.Invoke())

    member _.Snapshot () =
        lock fibers (fun () -> fibers.ToArray())

/// Base class for a FIO runtime that runs effects into fibers.
[<AbstractClass>]
type FIORuntime internal () =

    // Root and daemon fibers not yet unwound, in a list per thread that ran them: a Run touches only its own
    // thread's list and unwinding costs nothing. Shutdown snapshots every list.
    let lists = ResizeArray<TrackedFibers>()

    let local =
        new ThreadLocal<TrackedFibers>(fun () ->
            let tracked = TrackedFibers()
            lock lists (fun () -> lists.Add tracked)
            tracked)

    [<VolatileField>]
    let mutable disposed = 0

    let isDisposed = Func<bool>(fun () -> Volatile.Read &disposed = 1)

    // The fibers Shutdown found live and has not yet seen unwind, plus one while it is still counting.
    let mutable awaiting = 0

    // Set when the last of those has unwound, and when the first Shutdown has finished.
    let unwound = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let stopped = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let disposedMessage = "The runtime was disposed."

    // One delegate per runtime; Shutdown installs it on the fibers it found live.
    let onUnwound =
        Action<FiberContext>(fun _ ->
            if Interlocked.Decrement &awaiting = 0 then
                unwound.TrySetResult() |> ignore)

    /// The runtime's name.
    abstract member Name: string

    /// A display string describing the runtime and its configuration.
    abstract member ConfigString: string

    default this.ConfigString : string =
        this.Name

    /// Schedules the given effect on a new fiber and returns its handle at once; it never waits for, interrupts, or
    /// discards a fiber already running, so call it as often as you like.
    abstract member Run<'A, 'E> : FIO<'A, 'E> -> Fiber<'A, 'E>

    /// Returns a filesystem-safe form of this runtime's configuration string.
    member this.ToFileString () : string =
        this.ToString()
            .ToLowerInvariant()
            .Replace("(", "")
            .Replace(")", "")
            .Replace(":", "")
            .Replace(' ', '-')

    override this.ToString () : string =
        this.ConfigString

    member val internal StopWorkers: unit -> unit = ignore with get, set

    // The flag is read under the list's lock, which Shutdown takes for its snapshot only after claiming the flag:
    // a Run the snapshot missed sees the flag and interrupts itself.
    member private _.Watch (fiberContext: FiberContext) =
        if local.Value.Add(fiberContext, isDisposed) then
            fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)

    member internal this.Track (fiberContext: FiberContext) =
        if Volatile.Read &disposed = 1 then
            raise (ObjectDisposedException(this.Name, disposedMessage))

        this.Watch fiberContext

    member internal this.TrackDaemon (fiberContext: FiberContext) =
        this.Watch fiberContext

    /// Interrupts every fiber still running, waits up to the given time for them to unwind, then stops the workers; a
    /// later call waits for the first, and running an effect afterwards throws. Do not call it from one of its fibers.
    member this.Shutdown (timeout: TimeSpan) : unit =
        if timeout <> Timeout.InfiniteTimeSpan
           && (timeout < TimeSpan.Zero || timeout.TotalMilliseconds > float Int32.MaxValue) then
            raise (
                ArgumentOutOfRangeException(
                    nameof timeout,
                    timeout,
                    "The timeout must be between zero and Int32.MaxValue milliseconds, or Timeout.InfiniteTimeSpan."))

        if tryClaim &disposed then
            // A fiber whose handle was disposed, or whose cancellation callback throws, must not stop the rest.
            try
                let live =
                    lock lists (fun () -> lists.ToArray())
                    |> Array.collect (fun tracked -> tracked.Snapshot())
                    |> Array.filter (fun fiberContext -> not fiberContext.HasUnwound)

                // One more than the fibers, released after the loop: a hook that fires early cannot complete the count.
                awaiting <- live.Length + 1

                for fiberContext in live do
                    try
                        fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)
                    with _ ->
                        ()

                    fiberContext.SetOnUnwound onUnwound

                if Interlocked.Decrement &awaiting > 0 then
                    unwound.Task.Wait timeout |> ignore
            finally
                try
                    this.StopWorkers()
                finally
                    stopped.TrySetResult() |> ignore
        else
            stopped.Task.Wait timeout |> ignore

    interface IDisposable with

        /// Shuts the runtime down, giving its fibers up to ten seconds to unwind.
        member this.Dispose () : unit =
            this.Shutdown(TimeSpan.FromSeconds 10.0)

/// Worker counts and scheduling parameters for a worker-based runtime.
type WorkerConfig =
    {
        /// Number of workers that evaluate effects.
        EvaluationWorkers: int
        /// Number of evaluation steps a work item runs before being rescheduled.
        EvaluationSteps: int
        /// Number of workers that handle blocking operations.
        BlockingWorkers: int
    }

    /// The default configuration, sized to the current machine.
    static member Default : WorkerConfig =
        {
            EvaluationWorkers = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EvaluationSteps = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BlockingWorkers = WorkerRuntimeDefaults.BlockingWorkerCount
        }
