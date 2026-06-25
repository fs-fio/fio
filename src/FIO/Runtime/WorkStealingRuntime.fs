module FIO.Runtime.WorkStealing

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

[<Literal>]
let private SpinAttempts = 32

[<Literal>]
let private SpinWaitIterations = 48

[<Literal>]
let private BackstopMs = 1000

[<Literal>]
let private GlobalCheckInterval = 61

[<Literal>]
let private DequeCapacity = 256

[<Literal>]
let private ContStackDefaultCapacity = 32

[<Literal>]
let private ContStackMaxPool = 256

[<Literal>]
let private ContStackMaxDepth = 4096

[<Literal>]
let private WorkItemMaxPool = 512

type internal Scheduler(workerCount: int) =
    let globalQueue = MailboxQueue<WorkItem>()
    let runNext: WorkItem[] = Array.zeroCreate workerCount
    let deques: WorkStealingDeque[] = Array.init workerCount (fun _ -> WorkStealingDeque(DequeCapacity))
    let tick: int[] = Array.zeroCreate workerCount
    let contStackPools: Stack<Stack<Cont>>[] = Array.init workerCount (fun _ -> Stack<Stack<Cont>>())
    let workItemPools: Stack<WorkItem>[] = Array.init workerCount (fun _ -> Stack<WorkItem>())
    let workGate = new SemaphoreSlim(0)
    let mutable waitingWorkers = 0
    let mutable numSearching = 0

    let hasAnyWorkApprox () =
        if globalQueue.Count > 0 then
            true
        else
            let mutable i = 0
            let mutable found = false
            while not found && i < workerCount do
                if not (obj.ReferenceEquals(Volatile.Read &runNext.[i], null)) || not deques.[i].IsEmptyApprox then
                    found <- true
                i <- i + 1
            found

    let releaseOne () =
        if Volatile.Read &waitingWorkers > 0 then
            workGate.Release() |> ignore

    let signalWork () =
        if Volatile.Read &numSearching = 0 then
            releaseOne ()

    let cascadeWake () =
        if Volatile.Read &numSearching = 0 && hasAnyWorkApprox () then
            releaseOne ()

    member _.GlobalQueue =
        globalQueue

    member _.SignalWork () =
        signalWork ()

    member _.ScheduleLocal (workerId: int, workItem: WorkItem) =
        let previous = Interlocked.Exchange(&runNext.[workerId], workItem)
        if not (obj.ReferenceEquals(previous, null)) then
            deques.[workerId].PushBottom previous
        signalWork ()

    member _.TryGetLocal (workerId: int, workItem: byref<WorkItem>) : bool =
        tick.[workerId] <- tick.[workerId] + 1
        if tick.[workerId] % GlobalCheckInterval = 0 && globalQueue.TryRead &workItem then
            true
        else
            let next = Interlocked.Exchange(&runNext.[workerId], Unchecked.defaultof<_>)
            if not (obj.ReferenceEquals(next, null)) then
                workItem <- next
                true
            elif deques.[workerId].TryPopBottom &workItem then
                true
            else
                globalQueue.TryRead &workItem

    member _.TrySteal (workerId: int, workItem: byref<WorkItem>) : bool =
        let start = Random.Shared.Next workerCount
        let mutable offset = 0
        let mutable found = false
        while not found && offset < workerCount do
            let victimId = (start + offset) % workerCount
            if victimId <> workerId then
                if not (obj.ReferenceEquals(Volatile.Read &runNext.[victimId], null)) then
                    let stolen = Interlocked.Exchange(&runNext.[victimId], Unchecked.defaultof<_>)
                    if not (obj.ReferenceEquals(stolen, null)) then
                        workItem <- stolen
                        found <- true
                if not found && not deques.[victimId].IsEmptyApprox && deques.[victimId].TrySteal &workItem then
                    found <- true
            offset <- offset + 1
        found

    member _.BeginSearch () =
        Interlocked.Increment &numSearching |> ignore

    member _.EndSearch () =
        Interlocked.Decrement &numSearching |> ignore

    member _.CascadeWake () =
        cascadeWake ()

    member _.HasOtherWork (workerId: int) =
        not deques.[workerId].IsEmpty || globalQueue.Count > 0

    member _.RegisterWaiter () =
        Interlocked.Increment &waitingWorkers |> ignore

    member _.UnregisterWaiter () =
        Interlocked.Decrement &waitingWorkers |> ignore

    member _.WaitForWork (cancelToken: CancellationToken) : Task =
        task {
            let! _ = workGate.WaitAsync(BackstopMs, cancelToken)
            ()
        }

    member _.Reset () =
        globalQueue.Clear()
        for i in 0 .. workerCount - 1 do
            Interlocked.Exchange(&runNext.[i], Unchecked.defaultof<_>) |> ignore
            let mutable drained = Unchecked.defaultof<WorkItem>
            while deques.[i].TryPopBottom &drained do
                ()

    member _.Dispose () =
        workGate.Dispose()

    member _.RentContStack (workerId: int) : Stack<Cont> =
        let pool = contStackPools.[workerId]
        if pool.Count > 0 then
            let stack = pool.Pop()
            stack.Clear()
            stack
        else
            Stack<Cont> ContStackDefaultCapacity

    member _.ReturnContStack (workerId: int, stack: Stack<Cont>) =
        let pool = contStackPools.[workerId]
        if pool.Count < ContStackMaxPool && stack.Count <= ContStackMaxDepth then
            stack.Clear()
            pool.Push stack

    member _.RentWorkItem (workerId: int, effect: FIO<obj, obj>, fiberContext: FiberContext, contStack: Stack<Cont>) : WorkItem =
        let pool = workItemPools.[workerId]
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

    member _.ReturnWorkItem (workerId: int, workItem: WorkItem) =
        let pool = workItemPools.[workerId]
        if pool.Count < WorkItemMaxPool then
            workItem.Effect <- Unchecked.defaultof<_>
            workItem.FiberContext <- Unchecked.defaultof<_>
            workItem.ContStack <- Unchecked.defaultof<_>
            workItem.InterruptionSuppressed <- 0
            pool.Push workItem

type private EvaluationWorkerConfig =
    {
        Scheduler: Scheduler
        Runtime: WorkStealingRuntime
        WorkerId: int
        EvaluationSteps: int
    }

and [<Struct>] private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

and private Worker(config: EvaluationWorkerConfig) =

    let scheduler = config.Scheduler
    let runtime = config.Runtime
    let workerId = config.WorkerId

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"Worker-{workerId}" <| fun cancelToken ->
            task {
                while not cancelToken.IsCancellationRequested do
                    let mutable workItem = Unchecked.defaultof<WorkItem>
                    let mutable hasWork = false
                    if scheduler.TryGetLocal(workerId, &workItem) then
                        hasWork <- true
                    else
                        scheduler.BeginSearch()
                        let mutable got = false
                        let mutable spins = 0
                        while not got && spins < SpinAttempts && not cancelToken.IsCancellationRequested do
                            got <- scheduler.TrySteal(workerId, &workItem) || scheduler.TryGetLocal(workerId, &workItem)
                            if not got then
                                Thread.SpinWait SpinWaitIterations
                                spins <- spins + 1
                        if got then
                            scheduler.EndSearch()
                            scheduler.CascadeWake()
                            hasWork <- true
                        elif not cancelToken.IsCancellationRequested then
                            scheduler.RegisterWaiter()
                            scheduler.EndSearch()
                            if scheduler.TryGetLocal(workerId, &workItem) || scheduler.TrySteal(workerId, &workItem) then
                                scheduler.UnregisterWaiter()
                                scheduler.CascadeWake()
                                hasWork <- true
                            else
                                do! scheduler.WaitForWork cancelToken
                                scheduler.UnregisterWaiter()
                        else
                            scheduler.EndSearch()

                    // Completed (success/failure) fibers have nothing left to run, but an interrupted
                    // fiber may still need to unwind finalizers, so it must not be gated out here.
                    if hasWork && not (workItem.FiberContext.IsCompleted()) then
                        try
                            do! runtime.InterpretAsync workItem config.EvaluationSteps workerId
                        with exn ->
                            try
                                do! workItem.FiberContext.CompleteAndReschedule(Error(exn :> obj), scheduler.GlobalQueue)
                                scheduler.SignalWork()
                            with _ ->
                                ()
                            raise exn
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

/// A multi-threaded, work-stealing runtime with custom fibers. The default runtime.
and WorkStealingRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let workerCount = config.EvaluationWorkers
    let scheduler = Scheduler(workerCount)

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let workers =
        List.init workerCount (fun i ->
            new Worker(
                {
                    Scheduler = scheduler
                    Runtime = this
                    WorkerId = i
                    EvaluationSteps = config.EvaluationSteps
                }))

    override _.Name =
        "WorkStealingRuntime"

    interface IDisposable with

        member _.Dispose () =
            workers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            scheduler.Dispose()

    /// Creates the runtime with the default worker configuration.
    new() = new WorkStealingRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal _.InterpretAsync
        (workItem: WorkItem)
        (evaluationSteps: int)
        (workerId: int) =
        let mutable state =
            InterpreterState(workItem.Effect, workItem.ContStack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEvaluationSteps = evaluationSteps
        let currentFiberContext = workItem.FiberContext
        let mutable completionAction = NoCompletion

        let inline onSuccessComplete value =
            scheduler.ReturnContStack(workerId, state.ContStack)
            completionAction <- CompleteSuccess value

        let inline onErrorComplete error =
            scheduler.ReturnContStack(workerId, state.ContStack)
            completionAction <- CompleteFailure error

        task {
            try
                while not state.Completed do
                    if state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ ->
                            raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error error ->
                            processOutcome
                                &state
                                onSuccessComplete
                                onErrorComplete
                                (OutcomeInterrupted error)
                    elif currentEvaluationSteps = 0 then
                        if scheduler.HasOtherWork workerId then
                            let newWorkItem = scheduler.RentWorkItem(workerId, state.Effect, currentFiberContext, state.ContStack)
                            newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                            scheduler.ScheduleLocal(workerId, newWorkItem)
                            state.Completed <- true
                        else
                            currentEvaluationSteps <- evaluationSteps
                    else
                        currentEvaluationSteps <- currentEvaluationSteps - 1
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleWriteChan(message, channel) ->
                                let writeTask = channel.WriteAsync message
                                if not writeTask.IsCompletedSuccessfully then
                                    do! writeTask
                                if channel.BlockingWorkItemCount > 0 then
                                    let mutable blockedReader = Unchecked.defaultof<WorkItem>
                                    if channel.TryDequeueBlockingWorkItem &blockedReader then
                                        scheduler.ScheduleLocal(workerId, blockedReader)
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded message)
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead(&value) then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                else
                                    let newWorkItem =
                                        scheduler.RentWorkItem(workerId, state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! channel.AddBlockingWorkItem newWorkItem
                                    if channel.Count > 0 then
                                        let mutable blockedReader = Unchecked.defaultof<WorkItem>
                                        if channel.TryDequeueBlockingWorkItem &blockedReader then
                                            scheduler.ScheduleLocal(workerId, blockedReader)
                                    state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext) ->
                                let _ = setupForkRegistration currentFiberContext fiberContext
                                let forkedWorkItem = scheduler.RentWorkItem(workerId, effect, fiberContext, scheduler.RentContStack workerId)
                                scheduler.ScheduleLocal(workerId, forkedWorkItem)
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded fiber)
                            | HandleJoinFiber fiberContext ->
                                if fiberContext.IsTerminal() then
                                    let! value = fiberContext.Task
                                    processResult
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        value
                                else
                                    let newWorkItem =
                                        scheduler.RentWorkItem(workerId, state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! fiberContext.AddBlockingWorkItem newWorkItem
                                    let! rescheduled = fiberContext.TryRescheduleBlockingWorkItems scheduler.GlobalQueue
                                    if rescheduled then
                                        scheduler.SignalWork()
                                    state.Completed <- true
                            | HandleAwaitTask(awaited, onError) ->
                                let waited =
                                    if state.InterruptionSuppressed > 0 then
                                        awaited
                                    else
                                        awaited.WaitAsync currentFiberContext.CancellationToken

                                if waited.IsCompletedSuccessfully then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded waited.Result)
                                else
                                    // Park asynchronously rather than blocking this worker for the
                                    // duration of the task: when the task completes, reschedule the
                                    // fiber onto the global queue. The continuation may run on a
                                    // foreign thread, so it must not touch the per-worker pools or
                                    // deque (only the thread-safe global queue and wake signal).
                                    let fiberContext = currentFiberContext
                                    let suppressed = state.InterruptionSuppressed
                                    let contStack = state.ContStack

                                    let resume () =
                                        let resumeEffect =
                                            if waited.IsCompletedSuccessfully then
                                                Success waited.Result
                                            elif waited.IsCanceled
                                                 && fiberContext.CancellationToken.IsCancellationRequested then
                                                Interrupt(ExplicitInterrupt, "Task has been cancelled.")
                                            else
                                                let exn =
                                                    match waited.Exception with
                                                    | null -> OperationCanceledException() :> exn
                                                    | aggregate ->
                                                        match aggregate.InnerException with
                                                        | null -> aggregate :> exn
                                                        | inner -> inner
                                                Failure(onError exn)

                                        let resumeWorkItem =
                                            {
                                                Effect = resumeEffect
                                                FiberContext = fiberContext
                                                ContStack = contStack
                                                InterruptionSuppressed = suppressed
                                            }

                                        scheduler.GlobalQueue.WriteAsync resumeWorkItem |> ignore
                                        scheduler.SignalWork()

                                    waited.GetAwaiter().OnCompleted(Action resume)
                                    state.Completed <- true

                match completionAction with
                | CompleteSuccess value ->
                    do! currentFiberContext.CompleteAndReschedule(Ok value, scheduler.GlobalQueue)
                    scheduler.SignalWork()
                | CompleteFailure error ->
                    do! currentFiberContext.CompleteAndReschedule(Error error, scheduler.GlobalQueue)
                    scheduler.SignalWork()
                | NoCompletion -> ()

                return ()
            finally
                if not state.Completed then
                    scheduler.ReturnContStack(workerId, state.ContStack)

                scheduler.ReturnWorkItem(workerId, workItem)
        }

    member private _.Reset () =
        scheduler.Reset()

    override _.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task.GetAwaiter().GetResult() |> ignore
            | _ -> ()

            match currentFiber with
            | Some fiberContext -> fiberContext.Cancel()
            | None -> ()

            this.Reset()
            let fiber = new Fiber<'A, 'E>()
            currentFiber <- Some fiber.Context

            let workItem =
                WorkItemPool.Rent(effect.UpcastBoth(), fiber.Context, ContStackPool.Rent())

            scheduler.GlobalQueue.WriteAsync workItem |> ignore
            scheduler.SignalWork()

            fiber)
