namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Collections.Generic

/// Shared defaults for worker-based runtimes.
module internal WorkerRuntimeDefaults =
    /// Number of processors reserved for the system, excluded from worker allocation.
    let ProcessorReserve = 1

    /// Minimum number of evaluation workers regardless of processor count.
    let MinimumEvaluationWorkerCount = 2

    /// Default number of evaluation steps per work item before rescheduling.
    let EvaluationWorkerSteps = 200

    /// Default number of blocking workers.
    let BlockingWorkerCount = 1

    /// Computes the evaluation worker count based on available processors.
    let ComputeEvaluationWorkerCount () =
        let availableWorkers = Environment.ProcessorCount - ProcessorReserve

        if availableWorkers >= MinimumEvaluationWorkerCount then
            availableWorkers
        else
            MinimumEvaluationWorkerCount

/// Object pool for continuation stacks. Thread-local to avoid synchronization.
type internal ContStackPool private () =
    static let DefaultStackCapacity = 32
    static let MaxPoolSize = 256
    static let MaxReturnedStackDepth = 4096

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<Stack<ContStackFrame>>

    /// Rents a continuation stack from the pool or creates a new one.
    /// <returns>A continuation stack from the pool or newly created.</returns>
    static member inline Rent() =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count > 0 then
            let stack = ContStackPool.pool.Pop()
            stack.Clear()
            stack
        else
            Stack<ContStackFrame> DefaultStackCapacity

    /// Returns a continuation stack to the pool for reuse.
    /// <param name="stack">The stack to return to the pool.</param>
    static member inline Return(stack: Stack<ContStackFrame>) =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count < MaxPoolSize && stack.Count <= MaxReturnedStackDepth then
            stack.Clear()
            ContStackPool.pool.Push stack

/// Object pool for WorkItems. Thread-local to avoid synchronization.
type internal WorkItemPool private () =
    static let MaxPoolSize = 512

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<WorkItem>

    /// Rents a WorkItem from the pool or creates a new one.
    /// <param name="eff">The effect to evaluate.</param>
    /// <param name="fiberContext">The fiber context for execution.</param>
    /// <param name="stack">The continuation stack.</param>
    /// <returns>A work item from the pool or newly created.</returns>
    static member inline Rent(eff: FIO<obj, obj>, fiberContext: FiberContext, stack: ContStack) : WorkItem =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        if WorkItemPool.pool.Count > 0 then
            let workItem = WorkItemPool.pool.Pop()
            workItem.Eff <- eff
            workItem.FiberContext <- fiberContext
            workItem.Stack <- stack
            workItem.InterruptionSuppressed <- 0
            workItem
        else
            {
                Eff = eff
                FiberContext = fiberContext
                Stack = stack
                InterruptionSuppressed = 0
            }

    /// Returns a WorkItem to the pool for reuse.
    /// <param name="workItem">The work item to return to the pool.</param>
    static member inline Return(workItem: WorkItem) =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        if WorkItemPool.pool.Count < MaxPoolSize then
            workItem.Eff <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.Stack <- Unchecked.defaultof<_>
            workItem.InterruptionSuppressed <- 0
            WorkItemPool.pool.Push workItem

/// Runtime for interpreting FIO effects.
[<AbstractClass>]
type FIORuntime internal () =
    /// Gets the name of this runtime.
    /// <returns>The runtime's name.</returns>
    abstract member Name: string

    /// Gets a string describing this runtime's configuration.
    /// <returns>A description of the runtime configuration.</returns>
    abstract member ConfigString: string

    override this.ConfigString = this.Name

    /// Runs an FIO effect and returns a fiber representing its execution.
    /// <param name="eff">The effect to execute.</param>
    /// <returns>A fiber representing the execution.</returns>
    abstract member Run<'R, 'E> : FIO<'R, 'E> -> Fiber<'R, 'E>

    /// Returns a lowercase, file-friendly string representation of the runtime.
    /// <returns>A lowercase file-friendly runtime name.</returns>
    member this.ToFileString() =
        this.ToString().ToLowerInvariant().Replace("(", "").Replace(")", "").Replace(":", "").Replace(' ', '-')

    override this.ToString() = this.ConfigString

/// Configuration for a worker runtime.
type WorkerConfig =
    {
        /// Evaluation worker count.
        EWC: int
        /// Evaluation worker steps per work item before rescheduling.
        EWS: int
        /// Blocking worker count.
        BWC: int
        /// Maximum number of concurrently active fibers. None = unbounded (default).
        MaxFibers: int option
    }

    /// Default worker configuration based on system resources.
    /// <returns>Default configuration with system-appropriate processor allocation.</returns>
    static member Default =
        {
            EWC = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EWS = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BWC = WorkerRuntimeDefaults.BlockingWorkerCount
            MaxFibers = None
        }

/// Fiber admission control abstraction for backpressure.
/// <remarks>Thread-safe. Implementations must be safe for concurrent TryAcquire/Release calls.</remarks>
type internal IFiberAdmission =
    /// Attempts to acquire a permit without blocking.
    /// <returns>true if a permit was acquired; false if at capacity.</returns>
    abstract TryAcquire: unit -> bool
    /// Releases a permit, indicating a fiber has terminated.
    abstract Release: unit -> unit
    /// Resets admission state to full capacity.
    abstract Reset: unit -> unit

/// No-op admission control — always admits. Used when MaxFibers is None.
[<Sealed>]
type internal UnboundedAdmission private () =
    static let instance = UnboundedAdmission()

    /// Gets the singleton instance.
    static member Instance = instance :> IFiberAdmission

    interface IFiberAdmission with
        member _.TryAcquire() = true
        member _.Release() = ()
        member _.Reset() = ()

/// Bounded admission control — limits concurrent active fibers using a semaphore.
/// <param name="maxFibers">Maximum number of concurrently active fibers.</param>
/// <remarks>
/// Uses SemaphoreSlim for non-blocking TryAcquire via Wait(0).
/// Release guards against SemaphoreFullException from stale fibers after Reset.
/// </remarks>
[<Sealed>]
type internal BoundedAdmission(maxFibers: int) =
    let sem = new SemaphoreSlim(maxFibers, maxFibers)

    interface IFiberAdmission with
        /// Attempts to acquire a fiber permit without blocking.
        /// <returns>true if a permit was acquired; false if at capacity.</returns>
        member _.TryAcquire() = sem.Wait(0)

        /// Releases a fiber permit. Guards against over-release after Reset.
        member _.Release() =
            try
                sem.Release() |> ignore
            with :? SemaphoreFullException ->
                ()

        /// Resets the semaphore to full capacity for a new Run cycle.
        member _.Reset() =
            while sem.Wait(0) do
                ()

            if maxFibers > 0 then
                sem.Release maxFibers |> ignore

    interface IDisposable with
        member _.Dispose() = sem.Dispose()

/// Creates the appropriate admission control based on configuration.
module internal FiberAdmission =
    /// Creates an IFiberAdmission instance from a WorkerConfig.
    /// <param name="config">The worker configuration.</param>
    /// <returns>An admission control instance.</returns>
    let fromConfig (config: WorkerConfig) : IFiberAdmission =
        match config.MaxFibers with
        | None -> UnboundedAdmission.Instance
        | Some maxFibers ->
            if maxFibers <= 0 then
                invalidArg "config" $"MaxFibers must be positive, got {maxFibers}."

            new BoundedAdmission(maxFibers)
