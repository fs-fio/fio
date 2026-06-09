namespace FIO.Runtime

open FIO.DSL

open System
open System.Collections.Generic

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

    static member inline Rent() =
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

    static member inline Return(stack: Stack<Cont>) =
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

    static member inline Rent(eff: FIO<obj, obj>, fiberContext: FiberContext, stack: ContStack) : WorkItem =
        let mutable pool = WorkItemPool.pool
        if isNull pool then
            pool <- Stack<WorkItem>()
            WorkItemPool.pool <- pool

        if pool.Count > 0 then
            let workItem = pool.Pop()
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

    static member inline Return(workItem: WorkItem) =
        let mutable pool = WorkItemPool.pool
        if isNull pool then
            pool <- Stack<WorkItem>()
            WorkItemPool.pool <- pool

        if pool.Count < MaxPoolSize then
            workItem.Eff <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.Stack <- Unchecked.defaultof<_>
            workItem.InterruptionSuppressed <- 0
            pool.Push workItem

[<AbstractClass>]
type FIORuntime internal () =

    abstract member Name: string

    abstract member ConfigString: string

    default this.ConfigString =
        this.Name

    abstract member Run<'A, 'E> : FIO<'A, 'E> -> Fiber<'A, 'E>

    member this.ToFileString () =
        this.ToString().ToLowerInvariant().Replace("(", "").Replace(")", "").Replace(":", "").Replace(' ', '-')

    override this.ToString () =
        this.ConfigString

type WorkerConfig =
    {
        EWC: int
        EWS: int
        BWC: int
    }

    static member Default =
        {
            EWC = WorkerRuntimeDefaults.ComputeEvaluationWorkerCount()
            EWS = WorkerRuntimeDefaults.EvaluationWorkerSteps
            BWC = WorkerRuntimeDefaults.BlockingWorkerCount
        }
