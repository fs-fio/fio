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
[<AbstractClass>]
type FIORuntime internal () =

    // Root and daemon fibers that have not fully unwound; scoped children unwind with their roots. Striped
    // by identity so concurrent Runs rarely share a lock, and a HashSet so tracking a fiber allocates nothing.
    let live = Array.init 16 (fun _ -> HashSet<FiberContext> HashIdentity.Reference)

    [<VolatileField>]
    let mutable liveCount = 0

    [<VolatileField>]
    let mutable disposed = 0

    // Set when the last live fiber unwinds after disposal began, and when the first Shutdown has finished.
    let unwound = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let stopped = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let disposedMessage = "The runtime was disposed."

    let stripeOf (fiberContext: FiberContext) =
        live.[RuntimeHelpers.GetHashCode fiberContext &&& 15]

    // One delegate per runtime, so watching a fiber allocates no closure.
    let onUnwound =
        Action<FiberContext>(fun fiberContext ->
            let stripe = stripeOf fiberContext
            lock stripe (fun () -> stripe.Remove fiberContext |> ignore)

            if Interlocked.Decrement &liveCount = 0 && Volatile.Read &disposed = 1 then
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

    // Watching before reading the flag pairs with Shutdown setting the flag before reading live: either
    // Shutdown sees the fiber, or the fiber sees the flag and is interrupted here.
    member private _.Watch (fiberContext: FiberContext) =
        let stripe = stripeOf fiberContext
        Interlocked.Increment &liveCount |> ignore
        lock stripe (fun () -> stripe.Add fiberContext |> ignore)
        fiberContext.SetOnUnwound onUnwound

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
        if tryClaim &disposed then
            for stripe in live do
                for fiberContext in lock stripe (fun () -> Seq.toArray stripe) do
                    fiberContext.Interrupt(ExplicitInterrupt, disposedMessage)

            if Volatile.Read &liveCount > 0 then
                unwound.Task.Wait timeout |> ignore

            this.StopWorkers()
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
