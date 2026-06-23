module FIO.Runtime.Signaling

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks

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

// The work-stealing scheduler state, kept as a standalone object that is fully constructed before
// any worker starts. Workers therefore touch only this (already-initialized) object on their hot
// path, never the half-initialized SignalingRuntime — which avoids the F# recursive-initialization
// trap when worker threads launch from inside the runtime's constructor.
type internal Scheduler(workerCount: int) =
    let globalQueue = MailboxQueue<WorkItem>()
    let runNext: WorkItem[] = Array.zeroCreate workerCount
    let deques: WorkStealingDeque[] = Array.init workerCount (fun _ -> WorkStealingDeque(DequeCapacity))
    let tick: int[] = Array.zeroCreate workerCount
    let workGate = new SemaphoreSlim(0)
    let mutable waitingWorkers = 0

    // Wakes one parked worker, but only when one is actually parked. Gating on the waiter count
    // keeps the (unbounded) semaphore's permit count bounded and avoids SemaphoreFullException as
    // control flow on the hot signalling path.
    let signalWork () =
        if Volatile.Read &waitingWorkers > 0 then
            workGate.Release() |> ignore

    member _.GlobalQueue =
        globalQueue

    member _.SignalWork () =
        signalWork ()

    // Schedules a work item onto the running worker's local structures: the freshest item goes to
    // the runNext slot, evicting any previous occupant to the deque. A parked worker is woken so it
    // can steal the item if the owner stays busy.
    member _.ScheduleLocal (workerId: int, workItem: WorkItem) =
        let previous = Interlocked.Exchange(&runNext.[workerId], workItem)
        if not (obj.ReferenceEquals(previous, null)) then
            deques.[workerId].PushBottom previous
        signalWork ()

    // Finds the next work item: own runNext, own deque, the global queue, then steals from a random
    // peer's runNext/deque. A periodic global-first check keeps the global queue from starving.
    member _.TryGetWork (workerId: int, workItem: byref<WorkItem>) : bool =
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
            elif globalQueue.TryRead &workItem then
                true
            else
                let start = Random.Shared.Next workerCount
                let mutable offset = 0
                let mutable found = false
                while not found && offset < workerCount do
                    let victimId = (start + offset) % workerCount
                    if victimId <> workerId then
                        // Cheap lock-free probes first, so idle peers are skipped without an atomic
                        // exchange or a deque lock — this keeps stealing cheap on low-parallelism
                        // workloads where only one peer ever has work.
                        if not (obj.ReferenceEquals(Volatile.Read &runNext.[victimId], null)) then
                            let stolen = Interlocked.Exchange(&runNext.[victimId], Unchecked.defaultof<_>)
                            if not (obj.ReferenceEquals(stolen, null)) then
                                workItem <- stolen
                                found <- true
                        if not found && not deques.[victimId].IsEmptyApprox && deques.[victimId].TrySteal &workItem then
                            found <- true
                    offset <- offset + 1
                found

    member _.HasOtherWork (workerId: int) =
        not deques.[workerId].IsEmpty || globalQueue.Count > 0

    member _.RegisterWaiter () =
        Interlocked.Increment &waitingWorkers |> ignore

    member _.UnregisterWaiter () =
        Interlocked.Decrement &waitingWorkers |> ignore

    // Blocks until signalWork releases a permit, or a long backstop elapses. The register-then-
    // recheck protocol in the worker makes a missed signal impossible in normal operation, so the
    // backstop is pure insurance; idle workers otherwise sleep at zero CPU.
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

type private EvaluationWorkerConfig =
    {
        Scheduler: Scheduler
        Runtime: SignalingRuntime
        WorkerId: int
        EvaluationSteps: int
    }

and [<Struct>] private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

// A unified work-stealing worker. It evaluates work items from its own runNext slot and deque first
// (so a just-unblocked rendezvous partner stays hot and is reclaimed without a wakeup), then the
// global queue, then steals from peers. There is no dedicated blocking worker: channel unblocks are
// scheduled like any other work and either reclaimed by the signalling worker or stolen by an idle
// peer — the choice self-adapts to whether the signalling fiber blocks next.
and private Worker(config: EvaluationWorkerConfig) =

    let scheduler = config.Scheduler
    let runtime = config.Runtime
    let workerId = config.WorkerId

    let runItem (workItem: WorkItem) =
        task {
            if not (workItem.FiberContext.IsTerminal()) then
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

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"Worker-{workerId}" <| fun cancelToken ->
            task {
                while not cancelToken.IsCancellationRequested do
                    let mutable workItem = Unchecked.defaultof<WorkItem>
                    if scheduler.TryGetWork(workerId, &workItem) then
                        do! runItem workItem
                    else
                        let mutable got = false
                        let mutable spins = 0
                        while not got && spins < SpinAttempts && not cancelToken.IsCancellationRequested do
                            Thread.SpinWait SpinWaitIterations
                            got <- scheduler.TryGetWork(workerId, &workItem)
                            spins <- spins + 1
                        if got then
                            do! runItem workItem
                        elif not cancelToken.IsCancellationRequested then
                            // Publish as a waiter, then re-check (so a producer's signalWork cannot
                            // be missed), then block until signalled. Parked workers use no CPU.
                            scheduler.RegisterWaiter()
                            if scheduler.TryGetWork(workerId, &workItem) then
                                scheduler.UnregisterWaiter()
                                do! runItem workItem
                            else
                                do! scheduler.WaitForWork cancelToken
                                scheduler.UnregisterWaiter()
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

/// A multi-threaded, work-stealing runtime with custom fibers. The default runtime.
and SignalingRuntime(config: WorkerConfig) as this =
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
        "SignalingRuntime"

    interface IDisposable with

        member _.Dispose () =
            workers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            scheduler.Dispose()

    /// Creates the runtime with the default worker configuration.
    new() = new SignalingRuntime(WorkerConfig.Default)

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
            ContStackPool.Return state.ContStack
            completionAction <- CompleteSuccess value

        let inline onErrorComplete error =
            ContStackPool.Return state.ContStack
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
                            let newWorkItem = WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
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
                                // Direct hand-off: reschedule a blocked reader onto this worker's
                                // local structures. The signalling worker reclaims it if it blocks
                                // next (latency-bound chains); otherwise an idle peer steals it
                                // (throughput-bound producers). Guard with the cheap blocked-count
                                // read so a producer racing ahead of its consumer pays nothing per
                                // write when no reader is parked.
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
                                        WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! channel.AddBlockingWorkItem newWorkItem
                                    // Close the lost-wakeup window: a message may have arrived
                                    // between the failed TryRead and parking.
                                    if channel.Count > 0 then
                                        let mutable blockedReader = Unchecked.defaultof<WorkItem>
                                        if channel.TryDequeueBlockingWorkItem &blockedReader then
                                            scheduler.ScheduleLocal(workerId, blockedReader)
                                    state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext) ->
                                let _ = setupForkRegistration currentFiberContext fiberContext
                                let forkedWorkItem = WorkItemPool.Rent(effect, fiberContext, ContStackPool.Rent())
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
                                        WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! fiberContext.AddBlockingWorkItem newWorkItem
                                    let! rescheduled = fiberContext.TryRescheduleBlockingWorkItems scheduler.GlobalQueue
                                    if rescheduled then
                                        scheduler.SignalWork()
                                    state.Completed <- true
                            | HandleAwaitTask(awaited, onError) ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            awaited
                                        else
                                            awaited.WaitAsync currentFiberContext.CancellationToken
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupted (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Task has been cancelled."
                                        )))
                                | exn ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeFailed (onError exn))

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
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
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
