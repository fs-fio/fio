/// <summary>
/// Provides the cooperative (work-stealing) runtime for interpreting FIO effects, enabling concurrent and asynchronous execution across multiple workers.
/// </summary>
module FIO.Runtime.Cooperative

open FIO.DSL

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

module private CooperativePolling =
    // Blocking worker polling/backoff tuning knobs.
    let BatchSize = 256
    let PendingQueueCapacityMultiplier = 2

    let ChannelSpinWaitIterations = 256
    let ChannelSpinMissThreshold = 4_096
    let ChannelYieldMissThreshold = 65_536

    let FiberSpinWaitIterations = 128
    let FiberSpinMissThreshold = 512
    let FiberYieldMissThreshold = 8_192
    let FiberDelayStep1MissThreshold = 16_384
    let FiberDelayStep2MissThreshold = 32_768
    let FiberDelayStep1Ms = 1
    let FiberDelayStep2Ms = 2
    let FiberDelayStep3Ms = 4

/// <summary>
/// Configuration for an evaluation worker.
/// </summary>
type private EvaluationWorkerConfig =
    { /// <summary>
      /// The runtime instance.
      /// </summary>
      Runtime: CooperativeRuntime
      /// <summary>
      /// Channel for active work items.
      /// </summary>
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      /// <summary>
      /// The blocking worker for handling blocked effects.
      /// </summary>
      BlockingWorker: BlockingWorker
      /// <summary>
      /// Number of evaluation steps per work item.
      /// </summary>
      EWSteps: int }

/// <summary>
/// Configuration for a blocking worker.
/// </summary>
and internal BlockingWorkerConfig =
    { /// <summary>
      /// Channel for active work items.
      /// </summary>
      ActiveWorkItemChan: UnboundedChannel<WorkItem>
      /// <summary>
      /// Channel for blocking items awaiting resources.
      /// </summary>
      ActiveBlockingItemChan: UnboundedChannel<BlockingEntry> }

/// <summary>
/// Blocking item metadata used by cooperative polling workers.
/// </summary>
and [<Struct>] internal BlockingEntry =
    { Item: BlockingItem
      MissCount: int }

/// <summary>
/// Worker that evaluates FIO effects.
/// </summary>
/// <param name="config">The evaluation worker configuration.</param>
and private EvaluationWorker (config: EvaluationWorkerConfig) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan, config.BlockingWorker)

    let cts = new CancellationTokenSource()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cts.Token.IsCancellationRequested do
                    try
                        let! hasWorkItem = config.ActiveWorkItemChan.WaitToTakeAsync()
                        if not hasWorkItem || cts.Token.IsCancellationRequested then
                            loop <- false
                        else
                            let! workItem = config.ActiveWorkItemChan.TakeAsync()
                            if not (workItem.FiberContext.IsTerminal()) then
                                do! processWorkItem workItem
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker()

    interface IDisposable with
        member _.Dispose () =
            cts.Cancel()
            cts.Dispose()

/// <summary>
/// Worker that handles blocked effects waiting for resources.
/// </summary>
/// <param name="config">The blocking worker configuration.</param>
and internal BlockingWorker (config: BlockingWorkerConfig) =
    let batchSize = CooperativePolling.BatchSize
    let pendingQueueCapacity = batchSize * CooperativePolling.PendingQueueCapacityMultiplier
    let mutable preferChannelFirst = true
    let channelPending = Queue<BlockingEntry>(pendingQueueCapacity)
    let fiberPending = Queue<BlockingEntry>(pendingQueueCapacity)

    let hasPending () =
        channelPending.Count > 0 || fiberPending.Count > 0

    let enqueuePending (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel _ -> channelPending.Enqueue entry
        | BlockingFiber _ -> fiberPending.Enqueue entry

    let tryTakePending (preferChannel: bool, entry: byref<BlockingEntry>) =
        if preferChannel then
            if channelPending.Count > 0 then
                entry <- channelPending.Dequeue()
                true
            elif fiberPending.Count > 0 then
                entry <- fiberPending.Dequeue()
                true
            else
                false
        else
            if fiberPending.Count > 0 then
                entry <- fiberPending.Dequeue()
                true
            elif channelPending.Count > 0 then
                entry <- channelPending.Dequeue()
                true
            else
                false

    let isReady (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel (chan, _) -> chan.Count > 0
        | BlockingFiber (fiberContext, _) -> fiberContext.IsTerminal()

    let getWorkItem (entry: BlockingEntry) =
        match entry.Item with
        | BlockingChannel (_, workItem) -> workItem
        | BlockingFiber (_, workItem) -> workItem

    let queueActive (workItem: WorkItem) =
        task {
            let addVt = config.ActiveWorkItemChan.AddAsync workItem
            if not addVt.IsCompletedSuccessfully then
                do! addVt.AsTask()
        }

    let applyBackoff (maxChannelMiss: int, maxFiberMiss: int) =
        task {
            if maxChannelMiss > 0 then
                if maxChannelMiss < CooperativePolling.ChannelSpinMissThreshold then
                    Thread.SpinWait CooperativePolling.ChannelSpinWaitIterations
                elif maxChannelMiss < CooperativePolling.ChannelYieldMissThreshold then
                    Thread.Yield() |> ignore
                else
                    do! Task.Yield()
            elif maxFiberMiss > 0 then
                if maxFiberMiss < CooperativePolling.FiberSpinMissThreshold then
                    Thread.SpinWait CooperativePolling.FiberSpinWaitIterations
                elif maxFiberMiss < CooperativePolling.FiberYieldMissThreshold then
                    do! Task.Yield()
                else
                    let delayMs =
                        if maxFiberMiss < CooperativePolling.FiberDelayStep1MissThreshold then CooperativePolling.FiberDelayStep1Ms
                        elif maxFiberMiss < CooperativePolling.FiberDelayStep2MissThreshold then CooperativePolling.FiberDelayStep2Ms
                        else CooperativePolling.FiberDelayStep3Ms
                    do! Task.Delay delayMs
        }

    let processBatch () =
        task {
            let mutable processed = 0
            let mutable maxChannelMiss = 0
            let mutable maxFiberMiss = 0
            let mutable entry = Unchecked.defaultof<BlockingEntry>
            let mutable hasEntry = true

            while processed < batchSize && hasEntry do
                hasEntry <- tryTakePending(preferChannelFirst, &entry)
                if hasEntry then
                    processed <- processed + 1
                    if isReady entry then
                        do! queueActive (getWorkItem entry)
                    else
                        let missed = { entry with MissCount = entry.MissCount + 1 }
                        enqueuePending missed
                        match missed.Item with
                        | BlockingChannel _ ->
                            if missed.MissCount > maxChannelMiss then
                                maxChannelMiss <- missed.MissCount
                        | BlockingFiber _ ->
                            if missed.MissCount > maxFiberMiss then
                                maxFiberMiss <- missed.MissCount

            preferChannelFirst <- not preferChannelFirst

            if maxChannelMiss > 0 || maxFiberMiss > 0 then
                do! applyBackoff (maxChannelMiss, maxFiberMiss)
        }

    let tryDrainIncoming () =
        let mutable drained = 0
        let mutable entry = Unchecked.defaultof<BlockingEntry>
        while drained < batchSize && config.ActiveBlockingItemChan.TryTake(&entry) do
            enqueuePending entry
            drained <- drained + 1

    let waitForFirstIfNeeded (ct: CancellationToken) =
        task {
            if hasPending() then
                return true
            else
                let! hasBlockingItem = config.ActiveBlockingItemChan.WaitToTakeAsync()
                if not hasBlockingItem || ct.IsCancellationRequested then
                    return false
                else
                    let! blockingEntry = config.ActiveBlockingItemChan.TakeAsync()
                    enqueuePending blockingEntry
                    return true
        }
    
    let cancellationTokenSource = new CancellationTokenSource ()

    let startWorker () =
        (new Task((fun () ->
            task {
                let mutable loop = true
                while loop && not cancellationTokenSource.Token.IsCancellationRequested do
                    try
                        let! hasItemToProcess = waitForFirstIfNeeded cancellationTokenSource.Token
                        if not hasItemToProcess || cancellationTokenSource.Token.IsCancellationRequested then
                            loop <- false
                        else
                            tryDrainIncoming()
                            do! processBatch()
                    with
                    | :? OperationCanceledException ->
                        loop <- false
            } |> ignore), TaskCreationOptions.LongRunning))
            .Start TaskScheduler.Default

    do startWorker ()

    interface IDisposable with
        member _.Dispose () =
            cancellationTokenSource.Cancel()
            cancellationTokenSource.Dispose()

    /// <summary>
    /// Reschedules a work item that is blocked on a resource.
    /// </summary>
    /// <param name="blockingItem">The blocking item for the work item.</param>
    member internal _.RescheduleForBlocking blockingItem =
        config.ActiveBlockingItemChan.AddAsync {
            Item = blockingItem
            MissCount = 0
        }

/// <summary>
/// The cooperative runtime for FIO, interpreting effects concurrently using work-stealing.
/// </summary>
/// <param name="config">The worker configuration.</param>
and CooperativeRuntime (config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()
    let activeBlockingItemChan = UnboundedChannel<BlockingEntry>()
    let mutable currentFiber: FiberContext option = None
    let runLock = obj()

    let blockingWorkers =
        List.init config.BWC <| fun _ ->
            new BlockingWorker({
                ActiveWorkItemChan = activeWorkItemChan
                ActiveBlockingItemChan = activeBlockingItemChan
            })

    let evaluationWorkers =
        List.init config.EWC <| fun i ->
            let blockingWorker = blockingWorkers.[i % blockingWorkers.Length]
            new EvaluationWorker({
                Runtime = this
                ActiveWorkItemChan = activeWorkItemChan
                BlockingWorker = blockingWorker
                EWSteps = config.EWS
            })

    override _.Name =
        "CooperativeRuntime"

    interface IDisposable with
        member _.Dispose () =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    /// <summary>
    /// Creates a new CooperativeRuntime with default configuration.
    /// </summary>
    new() =
        new CooperativeRuntime(WorkerConfig.Default)

    /// <summary>
    /// Interprets an FIO effect asynchronously with step-limited evaluation.
    /// </summary>
    /// <param name="workItem">The work item containing the effect to interpret.</param>
    /// <param name="evalSteps">The maximum number of evaluation steps before rescheduling.</param>
    /// <param name="activeQueue">The active work item queue for rescheduling.</param>
    /// <param name="blockingWorker">The blocking worker for handling blocked effects.</param>
    [<TailCall>]
    member internal _.InterpretAsync (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>, blockingWorker: BlockingWorker) =
        let mutable currentEff = workItem.Eff
        let mutable currentContStack = workItem.Stack
        let mutable currentEWSteps = evalSteps
        let mutable currentFiberContext = workItem.FiberContext
        let mutable completed = false

        let inline processSuccess res =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    currentFiberContext.Complete(Ok res)
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()
                    match stackFrame.Cont with
                    | SuccessCont cont ->
                        currentEff <- cont res
                        loop <- false
                    | FailureCont _ ->
                        ()

        let inline processError err =
            let mutable loop = true
            while loop do
                if currentContStack.Count = 0 then
                    ContStackPool.Return currentContStack
                    currentFiberContext.Complete(Error err)
                    completed <- true
                    loop <- false
                else
                    let stackFrame = currentContStack.Pop()
                    match stackFrame.Cont with
                    | SuccessCont _ ->
                        ()
                    | FailureCont cont ->
                        currentEff <- cont err
                        loop <- false

        let inline processInterruptError err =
            ContStackPool.Return currentContStack
            currentFiberContext.Complete(Error err)
            completed <- true

        let inline processResult res =
            match res with
            | Ok res ->
                processSuccess res
            | Error err ->
                processError err

        task {
            let mutable workItemOwnership = true
            try
                while not completed do
                    if currentFiberContext.CancellationToken.IsCancellationRequested then
                        match! currentFiberContext.Task with
                        | Ok _ ->
                            raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error err ->
                            processInterruptError err
                    elif currentEWSteps = 0 then
                        let newWorkItem = WorkItemPool.Rent(currentEff, currentFiberContext, currentContStack)
                        do! activeWorkItemChan.AddAsync newWorkItem
                        workItemOwnership <- false
                        completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1
                        match currentEff with
                        | Success res ->
                            processSuccess res
                        | Failure err ->
                            processError err
                        | InterruptFiber(cause, msg, fiberContext) ->
                            fiberContext.Interrupt(cause, msg)
                            processSuccess()
                        | InterruptSelf(cause, msg) ->
                            currentFiberContext.Interrupt(cause, msg)
                            processSuccess()
                        | Action(func, onError) ->
                            try
                                let res = func()
                                processSuccess res
                            with exn ->
                                processError (onError exn)
                        | SendChan(msg, chan) ->
                            do! chan.SendAsync msg
                            processSuccess msg
                        | ReceiveChan chan ->
                            let mutable res = Unchecked.defaultof<_>
                            if chan.UnboundedChannel.TryTake(&res) then
                                processSuccess res
                            else
                                let newWorkItem = WorkItemPool.Rent(ReceiveChan chan, currentFiberContext, currentContStack)
                                do! blockingWorker.RescheduleForBlocking
                                    <| BlockingChannel (chan, newWorkItem)
                                workItemOwnership <- false
                                completed <- true
                        | ForkEffect(eff, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt(ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                            do! activeWorkItemChan.AddAsync workItem
                            processSuccess fiber
                        | ForkTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt (ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        try
                                            do! t.WaitAsync fiberContext.CancellationToken
                                            fiberContext.Complete(Ok ())
                                        with
                                        | :? OperationCanceledException ->
                                            fiberContext.Complete(Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                        | exn ->
                                            fiberContext.Complete(Error (onError exn))
                                    finally
                                        registration.Dispose()
                                } :> Task)
                            processSuccess fiber
                        | ForkGenericTPLTask(taskFactory, onError, fiber, fiberContext) ->
                            let registration = currentFiberContext.CancellationToken.Register(fun () ->
                                fiberContext.Interrupt (ParentInterrupted currentFiberContext.Id, "Parent fiber was interrupted."))
                            fiberContext.AddRegistration registration
                            do! Task.Run(fun () ->
                                task {
                                    let t = taskFactory()
                                    try
                                        try
                                            let! result = t.WaitAsync fiberContext.CancellationToken
                                            fiberContext.Complete (Ok result)
                                        with
                                        | :? OperationCanceledException ->
                                            fiberContext.Complete (Error (onError (FiberInterruptedException (fiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))))
                                        | exn ->
                                            fiberContext.Complete (Error (onError exn))
                                    finally
                                        registration.Dispose()
                                } :> Task)
                            processSuccess fiber
                        | JoinFiber fiberContext ->
                            if fiberContext.IsTerminal() then
                                let! res = fiberContext.Task
                                processResult res
                            else
                                let newWorkItem = WorkItemPool.Rent(JoinFiber fiberContext, currentFiberContext, currentContStack)
                                do! blockingWorker.RescheduleForBlocking
                                    <| BlockingFiber (fiberContext, newWorkItem)
                                workItemOwnership <- false
                                completed <- true
                        | AwaitTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with
                            | :? OperationCanceledException when currentFiberContext.CancellationToken.IsCancellationRequested ->
                                processInterruptError (FiberInterruptedException (currentFiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))
                            | exn ->
                                processError (onError exn)
                        | AwaitGenericTPLTask(task, onError) ->
                            try
                                let! res = task.WaitAsync currentFiberContext.CancellationToken
                                processSuccess res
                            with
                            | :? OperationCanceledException when currentFiberContext.CancellationToken.IsCancellationRequested ->
                                processInterruptError (FiberInterruptedException (currentFiberContext.Id, ExplicitInterrupt, "Task has been cancelled."))
                            | exn ->
                                processError (onError exn)
                        | ChainSuccess(eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame (SuccessCont cont))
                        | ChainError (eff, cont) ->
                            currentEff <- eff
                            currentContStack.Push(ContStackFrame (FailureCont cont))
                return ()
            finally
                if not completed then
                    ContStackPool.Return currentContStack
                WorkItemPool.Return workItem
        }

    /// <summary>
    /// Resets the runtime state by clearing work item channels.
    /// </summary>
    member private _.Reset () =
        activeWorkItemChan.Clear()
        activeBlockingItemChan.Clear()

    /// <summary>
    /// Runs an FIO effect and returns a fiber representing its execution.
    /// </summary>
    /// <param name="eff">The FIO effect to run.</param>
    /// <returns>A fiber representing the running effect.</returns>
    override _.Run<'R, 'E> (eff: FIO<'R, 'E>) : Fiber<'R, 'E> =
        lock runLock (fun () ->
            match currentFiber with
            | Some fiberContext when not (fiberContext.IsTerminal()) ->
                fiberContext.Task
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> ignore
            | _ -> ()

            this.Reset()
            let fiber = new Fiber<'R, 'E>()
            currentFiber <- Some fiber.Internal

            let workItem = WorkItemPool.Rent(eff.UpcastBoth(), fiber.Internal, ContStackPool.Rent())
            activeWorkItemChan.AddAsync workItem
            |> ignore

            fiber)
