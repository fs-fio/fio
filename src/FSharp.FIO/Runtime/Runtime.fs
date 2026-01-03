namespace FSharp.FIO.Runtime

open FSharp.FIO.DSL

open System
open System.Globalization
open System.Collections.Generic

[<AutoOpen>]
module private Utils =
    
    let inline pop (contStack: ResizeArray<ContStackFrame>) =
        let lastIndex = contStack.Count - 1
        let stackFrame = contStack[lastIndex]
        contStack.RemoveAt lastIndex
        stackFrame

/// <summary>
/// Object pool for continuation stacks to reduce GC pressure.
/// Thread-local pooling avoids synchronization overhead.
/// </summary>
type internal ContStackPool private () =
    // Configuration constants
    static let DefaultStackCapacity = 32      // Initial capacity for new stacks
    static let MaxStackCapacity = 1024        // Maximum stack capacity to pool
    static let MaxPoolSize = 100              // Maximum stacks per thread pool

    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<ResizeArray<ContStackFrame>>

    /// <summary>
    /// Rents a continuation stack from the pool or creates a new one.
    /// </summary>
    static member inline Rent () =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        if ContStackPool.pool.Count > 0 then
            let stack = ContStackPool.pool.Pop()
            stack.Clear()
            stack
        else
            ResizeArray<ContStackFrame> DefaultStackCapacity

    /// <summary>
    /// Returns a continuation stack to the pool for reuse.
    /// </summary>
    /// <param name="stack">The stack to return to the pool.</param>
    static member inline Return (stack: ResizeArray<ContStackFrame>) =
        if isNull ContStackPool.pool then
            ContStackPool.pool <- Stack<_>()

        // Only pool stacks that aren't too large and if pool isn't at capacity
        if stack.Capacity <= MaxStackCapacity && ContStackPool.pool.Count < MaxPoolSize then
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
            // Create new if pool empty
            { Eff = eff; FiberContext = fiberContext; Stack = stack }

    /// <summary>
    /// Returns a WorkItem to the pool for reuse.
    /// </summary>
    static member inline Return (workItem: WorkItem) =
        if isNull WorkItemPool.pool then
            WorkItemPool.pool <- Stack<WorkItem>()

        // Only pool if not at capacity
        if WorkItemPool.pool.Count < MaxPoolSize then
            // Clear references to help GC (allow collected objects to be freed)
            let clearedWorkItem =
                { workItem with
                    Eff = Unchecked.defaultof<_>
                    FiberContext = Unchecked.defaultof<_>
                    Stack = Unchecked.defaultof<_> }
            WorkItemPool.pool.Push clearedWorkItem

/// <summary>
/// Represents a functional runtime for interpreting FIO effects.
/// </summary>
[<AbstractClass>]
type FRuntime internal () =
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
    { EWC: int
      EWS: int
      BWC: int }

/// <summary>
/// Represents a functional worker runtime for interpreting FIO effects.
/// </summary>
[<AbstractClass>]
type FWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FRuntime ()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 ||
           config.EWS <= 0 ||
           config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString ()}"

    do validateWorkerConfiguration ()

    /// <summary>
    /// Gets the worker configuration.
    /// </summary>
    member _.GetWorkerConfiguration () =
        config

    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"
