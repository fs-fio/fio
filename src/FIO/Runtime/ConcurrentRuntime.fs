module FIO.Runtime.Concurrent

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading.Tasks

type private EvaluationWorkerConfig =
    {
        Runtime: ConcurrentRuntime
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        BlockingWorker: BlockingWorker
        EWSteps: int
    }

and private BlockingWorkerConfig =
    {
        ActiveWorkItemChan: UnboundedChannel<WorkItem>
        ActiveBlockingEventChan: UnboundedChannel<Channel<obj>>
    }

and [<Struct>] private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem (workItem: WorkItem) =
        config.Runtime.InterpretAsync(workItem, config.EWSteps, config.ActiveWorkItemChan)

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}" <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemChan.WaitToReadAsync token

                    if not hasWorkItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemChan.ReadAsync()

                        if not (workItem.FiberContext.IsTerminal()) then
                            try
                                do! processWorkItem workItem
                            with exn ->
                                try
                                    do!
                                        workItem.FiberContext.CompleteAndReschedule(
                                            Error(exn :> obj),
                                            config.ActiveWorkItemChan)
                                with _ ->
                                    ()

                                raise exn
            }

    interface IDisposable with

        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

and private BlockingWorker(config: BlockingWorkerConfig, workerId: int) =

    let processBlockingChannel (blockingChan: Channel<obj>) =
        task {
            try
                let mutable keepProcessing = true
                while keepProcessing do
                    let! rescheduled =
                        blockingChan.TryRescheduleNextBlockingWorkItem config.ActiveWorkItemChan
                    let hasPendingMessages = blockingChan.Count > 0
                    let hasBlockedWorkItems = blockingChan.BlockingWorkItemCount > 0
                    keepProcessing <- rescheduled && hasPendingMessages && hasBlockedWorkItems
            finally
                blockingChan.EndSignalProcessing()

            if
                blockingChan.Count > 0
                && blockingChan.BlockingWorkItemCount > 0
                && blockingChan.TryBeginSignalProcessing()
            then
                do! config.ActiveBlockingEventChan.WriteAsync blockingChan
        }

    let struct (cts, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}" <| fun token ->
            task {
                let mutable loop = true

                while loop && not token.IsCancellationRequested do
                    let! hasBlockingItem = config.ActiveBlockingEventChan.WaitToReadAsync token

                    if not hasBlockingItem || token.IsCancellationRequested then
                        loop <- false
                    else
                        let! blockingChanEvent = config.ActiveBlockingEventChan.ReadAsync()
                        do! processBlockingChannel blockingChanEvent
            }

    interface IDisposable with

        member _.Dispose() =
            cts.Cancel()
            cts.Dispose()

and ConcurrentRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemChan = UnboundedChannel<WorkItem>()

    let activeBlockingEventChan = UnboundedChannel<Channel<obj>>()

    let signalBlockingWorkerIfPending (chan: Channel<obj>) : ValueTask =
        if
            chan.Count > 0
            && chan.BlockingWorkItemCount > 0
            && chan.TryBeginSignalProcessing()
        then
            activeBlockingEventChan.WriteAsync chan
        else
            ValueTask.CompletedTask

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let struct (blockingWorkers, evaluationWorkers) =
        WorkerBuilders.buildPairedWorkers
            config.BWC
            config.EWC
            (fun i ->
                new BlockingWorker(
                    {
                        ActiveWorkItemChan = activeWorkItemChan
                        ActiveBlockingEventChan = activeBlockingEventChan
                    },
                    i
                ))
            (fun i blockingWorker ->
                new EvaluationWorker(
                    {
                        Runtime = this
                        ActiveWorkItemChan = activeWorkItemChan
                        BlockingWorker = blockingWorker
                        EWSteps = config.EWS
                    },
                    i
                ))

    override _.Name =
        "ConcurrentRuntime"

    interface IDisposable with

        member _.Dispose() =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    new() = new ConcurrentRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal _.InterpretAsync (workItem: WorkItem, evalSteps: int, activeWorkItemChan: UnboundedChannel<WorkItem>) =
        let mutable state =
            InterpreterState(workItem.Eff, workItem.Stack, workItem.FiberContext, workItem.InterruptionSuppressed)

        let mutable currentEWSteps = evalSteps
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
                            processOutcome &state onSuccessComplete onErrorComplete (OutcomeInterrupt error)
                    elif currentEWSteps = 0 then
                        let newWorkItem = WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)
                        newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                        do! activeWorkItemChan.WriteAsync newWorkItem
                        state.Completed <- true
                    else
                        currentEWSteps <- currentEWSteps - 1
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleSendChan(msg, chan) ->
                                do! chan.WriteAsync msg
                                do! signalBlockingWorkerIfPending chan
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess msg)
                            | HandleReceiveChan chan ->
                                let mutable value = Unchecked.defaultof<_>

                                if chan.UnboundedChannel.TryRead(&value) then
                                    processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! chan.AddBlockingWorkItem newWorkItem
                                    do! signalBlockingWorkerIfPending chan
                                    state.Completed <- true
                            | HandleForkEffect(eff, fiber, fiberContext) ->
                                let _registration = setupForkRegistration currentFiberContext fiberContext
                                let workItem = WorkItemPool.Rent(eff, fiberContext, ContStackPool.Rent())
                                do! activeWorkItemChan.WriteAsync workItem
                                processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess fiber)
                            | HandleJoinFiber fiberContext ->
                                if fiberContext.IsTerminal() then
                                    let! value = fiberContext.Task
                                    processResult &state onSuccessComplete onErrorComplete value
                                else
                                    let newWorkItem =
                                        WorkItemPool.Rent(state.Eff, currentFiberContext, state.ContStack)

                                    newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                                    do! fiberContext.AddBlockingWorkItem newWorkItem
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemChan
                                    state.Completed <- true
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            task
                                        else
                                            task.WaitAsync currentFiberContext.CancellationToken

                                    processOutcome &state onSuccessComplete onErrorComplete (OutcomeSuccess value)
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested
                                    ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupt (FiberInterruptedException(
                                            currentFiberContext.Id,
                                            ExplicitInterrupt,
                                            "Task has been cancelled."
                                        )))
                                | exn -> processOutcome &state onSuccessComplete onErrorComplete (OutcomeError (onError exn))

                match completionAction with
                | CompleteSuccess value ->
                    do! currentFiberContext.CompleteAndReschedule(Ok value, activeWorkItemChan)
                | CompleteFailure error ->
                    do! currentFiberContext.CompleteAndReschedule(Error error, activeWorkItemChan)
                | NoCompletion -> ()

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
        }

    member private _.Reset () =
        activeWorkItemChan.Clear()
        activeBlockingEventChan.Clear()

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

            activeWorkItemChan.WriteAsync workItem |> ignore

            fiber)
