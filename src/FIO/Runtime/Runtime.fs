namespace FIO.Runtime

open FIO.DSL

open System
open System.Globalization
open System.Collections.Generic

/// <summary>
/// Object pool for continuation stacks to reduce GC pressure.
/// Thread-local pooling avoids synchronization overhead.
/// </summary>
type internal ContStackPool private () =
    // Configuration constants
    static let DefaultStackCapacity = 32      // Initial capacity for new stacks
    static let MaxPoolSize = 100              // Maximum stacks per thread pool

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<Stack<ContStackFrame>>

    /// <summary>
    /// Rents a continuation stack from the pool or creates a new one.
    /// </summary>
    /// <returns>A continuation stack ready for use.</returns>
    static member inline Rent () =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count > 0 then
            let stack = ContStackPool.pool.Pop()
            stack.Clear()
            stack
        else
            Stack<ContStackFrame> DefaultStackCapacity

    /// <summary>
    /// Returns a continuation stack to the pool for reuse.
    /// </summary>
    /// <param name="stack">The stack to return to the pool.</param>
    static member inline Return (stack: Stack<ContStackFrame>) =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        // Only pool if not at capacity (Stack<T> doesn't expose Capacity, so just check pool size)
        if ContStackPool.pool.Count < MaxPoolSize then
            stack.Clear()
            ContStackPool.pool.Push stack

/// <summary>
/// Object pool for WorkItems to reduce GC pressure.
/// Thread-local pooling avoids synchronization overhead.
/// </summary>
type internal WorkItemPool private () =
    // Configuration constants
    static let MaxPoolSize = 200  // Maximum WorkItems per thread pool

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<WorkItem>

    /// <summary>
    /// Rents a WorkItem from the pool or creates a new one.
    /// </summary>
    /// <param name="eff">The effect to execute.</param>
    /// <param name="fiberContext">The fiber context for execution.</param>
    /// <param name="stack">The continuation stack.</param>
    /// <returns>A WorkItem ready for use.</returns>
    static member inline Rent (eff: FIO<obj, obj>, fiberContext: FiberContext, stack: ContStack) : WorkItem =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        if WorkItemPool.pool.Count > 0 then
            let workItem = WorkItemPool.pool.Pop()
            workItem.Eff <- eff
            workItem.FiberContext <- fiberContext
            workItem.Stack <- stack
            workItem
        else
            { Eff = eff; FiberContext = fiberContext; Stack = stack }

    /// <summary>
    /// Returns a WorkItem to the pool for reuse.
    /// </summary>
    static member inline Return (workItem: WorkItem) =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        // Only pool if not at capacity
        if WorkItemPool.pool.Count < MaxPoolSize then
            workItem.Eff <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.Stack <- Unchecked.defaultof<_>
            WorkItemPool.pool.Push workItem

/// <summary>
/// Represents a functional runtime for interpreting FIO effects.
/// </summary>
[<AbstractClass>]
type FIORuntime internal () =
    /// <summary>
    /// Gets the name of the runtime.
    /// </summary>
    abstract member Name : string

    /// <summary>
    /// Gets the configuration string for the runtime.
    /// </summary>
    abstract member ConfigString : string

    override this.ConfigString =
        this.Name

    /// <summary>
    /// Runs an FIO effect and returns a fiber representing its execution.
    /// </summary>
    /// <param name="eff">The FIO effect to run.</param>
    abstract member Run<'R, 'E> : FIO<'R, 'E> -> Fiber<'R, 'E>

    /// <summary>
    /// Gets a file-friendly string representation of the runtime.
    /// </summary>
    /// <returns>A lowercase string suitable for file names.</returns>
    member this.ToFileString () =
        this.ToString()
            .ToLowerInvariant()
            .Replace("(", "")
            .Replace(")", "")
            .Replace(":", "")
            .Replace(' ', '-')
    
    override this.ToString () =
        this.ConfigString

/// <summary>
/// Represents the configuration for a worker runtime.
/// </summary>
type WorkerConfig =
    { /// <summary>
      /// Evaluation worker count.
      /// </summary>
      EWC: int
      /// <summary>
      /// Evaluation worker steps per work item before rescheduling.
      /// </summary>
      EWS: int
      /// <summary>
      /// Blocking worker count.
      /// </summary>
      BWC: int }

/// <summary>
/// Represents a functional worker runtime for interpreting FIO effects.
/// </summary>
/// <param name="config">The worker configuration.</param>
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime ()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 ||
           config.EWS <= 0 ||
           config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString ()}"

    do validateWorkerConfiguration ()

    /// <summary>
    /// Gets the worker configuration.
    /// </summary>
    /// <returns>The worker configuration.</returns>
    member _.GetWorkerConfiguration () =
        config

    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"
