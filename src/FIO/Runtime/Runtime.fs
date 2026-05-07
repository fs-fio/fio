namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Collections.Generic

/// <summary>Represents default configuration constants for worker-based FIO runtimes.</summary>
module internal WorkerRuntimeDefaults =
    /// <summary>Returns the number of processors reserved for the host process and OS.</summary>
    let ProcessorReserve = 1

    /// <summary>Returns the minimum number of evaluation workers regardless of processor count.</summary>
    let MinimumEvaluationWorkerCount = 2

    /// <summary>Returns the default number of evaluation steps each worker runs per work item before rescheduling.</summary>
    let EvaluationWorkerSteps = 200

    /// <summary>Returns the default number of blocking workers.</summary>
    let BlockingWorkerCount = 1

    /// <summary>Returns the evaluation worker count derived from the available processor count minus the processor reserve.</summary>
    /// <returns>The computed worker count, clamped to at least <c>MinimumEvaluationWorkerCount</c>.</returns>
    let ComputeEvaluationWorkerCount () =
        let availableWorkers = Environment.ProcessorCount - ProcessorReserve

        if availableWorkers >= MinimumEvaluationWorkerCount then
            availableWorkers
        else
            MinimumEvaluationWorkerCount

/// <summary>Represents a per-thread pool of continuation stacks that reduces allocation pressure.</summary>
type internal ContStackPool private () =
    /// <summary>Returns the initial capacity allocated for each new continuation stack.</summary>
    static let DefaultStackCapacity = 32
    /// <summary>Returns the maximum number of stacks retained per thread.</summary>
    static let MaxPoolSize = 256
    /// <summary>Returns the maximum depth a stack may have and still be returned to the pool.</summary>
    static let MaxReturnedStackDepth = 4096

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<Stack<ContStackFrame>>

    /// <summary>Returns a cleared continuation stack, reusing a pooled instance when available.</summary>
    /// <returns>A ready-to-use continuation stack.</returns>
    static member inline Rent() =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count > 0 then
            let stack = ContStackPool.pool.Pop()
            stack.Clear()
            stack
        else
            Stack<ContStackFrame> DefaultStackCapacity

    /// <summary>Returns the given continuation stack to the pool for later reuse if pool and stack limits allow.</summary>
    /// <param name="stack">The continuation stack to return.</param>
    static member inline Return(stack: Stack<ContStackFrame>) =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count < MaxPoolSize && stack.Count <= MaxReturnedStackDepth then
            stack.Clear()
            ContStackPool.pool.Push stack

/// <summary>Represents a per-thread pool of work items that reduces allocation pressure.</summary>
type internal WorkItemPool private () =
    /// <summary>Returns the maximum number of work items retained per thread.</summary>
    static let MaxPoolSize = 512

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<WorkItem>

    /// <summary>Returns a work item initialized with the given effect, fiber context, and continuation stack, reusing a pooled instance when available.</summary>
    /// <param name="eff">The effect to evaluate.</param>
    /// <param name="fiberContext">The fiber context owning this work item.</param>
    /// <param name="stack">The continuation stack for this work item.</param>
    /// <returns>A ready-to-use work item.</returns>
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

    /// <summary>Returns the given work item to the pool for later reuse if pool capacity allows.</summary>
    /// <param name="workItem">The work item to return.</param>
    static member inline Return(workItem: WorkItem) =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        if WorkItemPool.pool.Count < MaxPoolSize then
            workItem.Eff <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.Stack <- Unchecked.defaultof<_>
            workItem.InterruptionSuppressed <- 0
            WorkItemPool.pool.Push workItem

/// <summary>Represents the abstract base for FIO runtimes that interpret effects into running fibers.</summary>
[<AbstractClass>]
type FIORuntime internal () =
    /// <summary>Returns the human-readable name of this runtime.</summary>
    /// <returns>The runtime's name.</returns>
    abstract member Name: string

    /// <summary>Returns a string describing this runtime's full configuration.</summary>
    /// <returns>A description of the runtime including its configuration parameters; the default returns <c>Name</c>.</returns>
    abstract member ConfigString: string

    default this.ConfigString = this.Name

    /// <summary>Creates a new fiber that interprets the given effect under this runtime.</summary>
    /// <typeparam name="'R">The success result type produced by the effect.</typeparam>
    /// <typeparam name="'E">The typed error type the effect may fail with.</typeparam>
    /// <param name="eff">The effect to evaluate.</param>
    /// <returns>A fiber that runs <paramref name="eff"/> and exposes its terminal state.</returns>
    abstract member Run<'R, 'E> : FIO<'R, 'E> -> Fiber<'R, 'E>

    /// <summary>Returns a lowercase, file-friendly form of this runtime's description.</summary>
    /// <returns>A string suitable for embedding in filenames, derived from <c>ConfigString</c>.</returns>
    member this.ToFileString() =
        this.ToString().ToLowerInvariant().Replace("(", "").Replace(")", "").Replace(":", "").Replace(' ', '-')

    /// <summary>Returns a string representation of this runtime including its configuration.</summary>
    /// <returns>The value of <c>ConfigString</c>.</returns>
    override this.ToString() = this.ConfigString

/// <summary>Represents the configuration of a worker-based FIO runtime.</summary>
type WorkerConfig =
    {
        /// <summary>Represents the number of evaluation workers that interpret runnable fibers.</summary>
        EWC: int
        /// <summary>Represents the number of evaluation steps each worker runs on a single work item before rescheduling.</summary>
        EWS: int
        /// <summary>Represents the number of blocking workers that handle fibers waiting on channels or other fibers.</summary>
        BWC: int
        /// <summary>Represents the maximum number of concurrently active fibers; <c>None</c> means unbounded.</summary>
        MaxFibers: int option
    }

    /// <summary>Returns a default worker configuration sized to the available processors.</summary>
    /// <returns>A configuration with <c>EWC</c> derived from the processor count and standard defaults for the remaining fields.</returns>
    static member Default =
        {
            EWC = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EWS = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BWC = WorkerRuntimeDefaults.BlockingWorkerCount
            MaxFibers = None
        }

/// <summary>Represents an admission control strategy that decides whether a new fiber may be started.</summary>
type internal IFiberAdmission =
    /// <summary>Returns whether a permit was acquired, allowing a new fiber to be started.</summary>
    /// <returns><c>true</c> if a permit was acquired; <c>false</c> if the fiber limit has been reached.</returns>
    abstract TryAcquire: unit -> bool
    /// <summary>Releases a previously acquired permit, allowing another fiber to start.</summary>
    abstract Release: unit -> unit
    /// <summary>Resets the admission controller to its initial state with all permits available.</summary>
    abstract Reset: unit -> unit

/// <summary>Represents an admission controller that permits an unlimited number of concurrent fibers.</summary>
[<Sealed>]
type internal UnboundedAdmission private () =
    /// <summary>Returns the singleton instance of the unbounded admission controller.</summary>
    static let instance = UnboundedAdmission()

    /// <summary>Returns the singleton <c>UnboundedAdmission</c> as an <c>IFiberAdmission</c>.</summary>
    /// <returns>The shared unbounded admission instance.</returns>
    static member Instance = instance :> IFiberAdmission

    /// <summary>Provides the unbounded admission control implementation.</summary>
    interface IFiberAdmission with
        /// <summary>Returns whether a permit was acquired, always succeeding for unbounded admission.</summary>
        /// <returns><c>true</c> unconditionally.</returns>
        member _.TryAcquire() = true
        /// <summary>Transforms the admission state by releasing a permit; has no effect for unbounded admission.</summary>
        member _.Release() = ()
        /// <summary>Transforms the admission state by resetting all permits; has no effect for unbounded admission.</summary>
        member _.Reset() = ()

/// <summary>Represents an admission controller that limits the number of concurrent fibers to a fixed maximum.</summary>
/// <param name="maxFibers">The maximum number of fibers that may run concurrently.</param>
[<Sealed>]
type internal BoundedAdmission(maxFibers: int) =
    /// <summary>Represents the semaphore that tracks available fiber permits.</summary>
    let sem = new SemaphoreSlim(maxFibers, maxFibers)

    /// <summary>Provides the bounded admission control implementation.</summary>
    interface IFiberAdmission with
        /// <summary>Returns whether a permit was acquired without blocking.</summary>
        /// <returns><c>true</c> if a permit was available and acquired; <c>false</c> if the fiber limit has been reached.</returns>
        member _.TryAcquire() = sem.Wait(0)

        /// <summary>Transforms the admission state by releasing one previously acquired permit.</summary>
        member _.Release() =
            try
                sem.Release() |> ignore
            with :? SemaphoreFullException ->
                ()

        /// <summary>Transforms the admission state by draining and restoring all permits to the initial count.</summary>
        member _.Reset() =
            while sem.Wait(0) do
                ()

            if maxFibers > 0 then
                sem.Release maxFibers |> ignore

    /// <summary>Provides resource cleanup for the bounded admission controller.</summary>
    interface IDisposable with
        /// <summary>Transforms the admission controller by releasing its underlying resources.</summary>
        member _.Dispose() = sem.Dispose()

/// <summary>Represents factory functions for creating fiber admission controllers from a worker configuration.</summary>
module internal FiberAdmission =
    /// <summary>Creates an <c>IFiberAdmission</c> matching the <c>MaxFibers</c> setting in the given configuration.</summary>
    /// <param name="config">The worker configuration whose <c>MaxFibers</c> field determines the admission strategy.</param>
    /// <returns>An unbounded admission when <c>MaxFibers</c> is <c>None</c>, or a bounded admission capped at the specified count.</returns>
    let fromConfig (config: WorkerConfig) : IFiberAdmission =
        match config.MaxFibers with
        | None -> UnboundedAdmission.Instance
        | Some maxFibers ->
            if maxFibers <= 0 then
                invalidArg "config" $"MaxFibers must be positive, got {maxFibers}."

            new BoundedAdmission(maxFibers)
