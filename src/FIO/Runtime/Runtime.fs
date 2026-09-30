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
    let mutable items: WorkItem[] = Array.zeroCreate initialCapacity

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
                let grown: WorkItem[] = Array.zeroCreate (items.Length * 2)
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

/// Base class for a FIO runtime that runs effects into fibers.
// The fibers one thread has run on a runtime. Add is called by that thread only and Snapshot by Shutdown,
// so the lock is uncontended until then. The list is pruned of unwound fibers when it has doubled since the
// last prune, so a fiber is examined a bounded number of times however many stay live.
type internal TrackedFibers () =
    let fibers = ResizeArray<FiberContext>()
    let mutable pruneAt = 64

    member _.Add (fiberContext: FiberContext) =
        lock fibers (fun () ->
            fibers.Add fiberContext

            if fibers.Count >= pruneAt then
                fibers.RemoveAll(fun tracked -> tracked.HasUnwound) |> ignore
                pruneAt <- max 64 (fibers.Count * 2))

    member _.Snapshot () =
        lock fibers (fun () -> fibers.ToArray())

[<AbstractClass>]
type FIORuntime internal () =

    // Root and daemon fibers that have not fully unwound, in a list per thread that ran them; scoped children
    // unwind with their roots. A Run touches only its own thread's list, so no cache line is shared with the
    // workers that unwind fibers, and unwinding costs nothing. Shutdown snapshots every list, so each is
    // registered once, when its thread first runs a fiber here.
    let lists = ResizeArray<TrackedFibers>()

    let local =
        new ThreadLocal<TrackedFibers>(fun () ->
            let tracked = TrackedFibers()
            lock lists (fun () -> lists.Add tracked)
            tracked)

    [<VolatileField>]
    let mutable disposed = 0

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

    default this.ConfigString =
        this.Name

    /// Schedules the given effect on a new fiber and returns immediately with a handle to it. Safe to
    /// call concurrently and as often as you like — for example once per request in a server — because
    /// it never waits for, interrupts, or discards any fiber already running on this runtime.
    abstract member Run<'A, 'E> : FIO<'A, 'E> -> Fiber<'A, 'E>

    /// Returns a filesystem-safe form of this runtime's configuration string.
    member this.ToFileString () =
        this.ToString()
            .ToLowerInvariant()
            .Replace("(", "")
            .Replace(")", "")
            .Replace(":", "")
            .Replace(' ', '-')

    override this.ToString () =
        this.ConfigString

    member val internal StopWorkers: unit -> unit = ignore with get, set

    // Adding before reading the flag pairs with Shutdown setting the flag before taking its snapshots: either
    // Shutdown sees the fiber, or the fiber sees the flag and is interrupted here. The barrier orders the
    // add before the read; Shutdown's claim of the flag is a full fence before its snapshots.
    member private _.Watch (fiberContext: FiberContext) =
        local.Value.Add fiberContext
        Interlocked.MemoryBarrier()

        if Volatile.Read &disposed = 1 then
            fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)

    member internal this.Track (fiberContext: FiberContext) =
        if Volatile.Read &disposed = 1 then
            raise (ObjectDisposedException(this.Name, disposedMessage))

        this.Watch fiberContext

    member internal this.TrackDaemon (fiberContext: FiberContext) =
        this.Watch fiberContext

    /// Interrupts every fiber still running on this runtime, waits up to the given time for them to unwind, then stops the runtime's workers.
    /// A concurrent or later call waits for the first one; running an effect afterwards throws. Do not call it from one of this runtime's own fibers.
    member this.Shutdown (timeout: TimeSpan) =
        if timeout <> Timeout.InfiniteTimeSpan
           && (timeout < TimeSpan.Zero || timeout.TotalMilliseconds > float Int32.MaxValue) then
            raise (
                ArgumentOutOfRangeException(
                    nameof timeout,
                    timeout,
                    "The timeout must be between zero and Int32.MaxValue milliseconds, or Timeout.InfiniteTimeSpan."))

        if tryClaim &disposed then
            // Interrupting a fiber can throw, when its handle was disposed or a cancellation callback of its own
            // did; that must not keep the other fibers from being interrupted or the workers from being stopped.
            try
                let live =
                    lock lists (fun () -> lists.ToArray())
                    |> Array.collect (fun tracked -> tracked.Snapshot())
                    |> Array.filter (fun fiberContext -> not fiberContext.HasUnwound)

                // One more than the fibers, held until every hook is installed: a fiber that unwinds in the
                // meantime fires its hook at once, and the count must not reach zero before the loop ends.
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
        member this.Dispose () =
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
    static member Default =
        {
            EvaluationWorkers = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EvaluationSteps = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BlockingWorkers = WorkerRuntimeDefaults.BlockingWorkerCount
        }
