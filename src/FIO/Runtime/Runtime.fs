namespace FIO.Runtime

open FIO.DSL

open System
open System.Globalization
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
    }

    /// Default worker configuration based on system resources.
    /// <returns>Default configuration with system-appropriate processor allocation.</returns>
    static member Default =
        {
            EWC = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EWS = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BWC = WorkerRuntimeDefaults.BlockingWorkerCount
        }

/// Base class for worker-based FIO runtimes.
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 || config.EWS <= 0 || config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    member _.WorkerConfig = config

    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    override this.ToString() = $"{this.Name} ({this.ConfigString})"
