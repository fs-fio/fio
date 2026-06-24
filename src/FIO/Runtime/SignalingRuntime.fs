module FIO.Runtime.Signaling

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading.Tasks

type private EvaluationWorkerConfig =
    {
        Runtime: SignalingRuntime
        ActiveWorkItemQueue: MailboxQueue<WorkItem>
        BlockingWorker: BlockingWorker
        EvaluationSteps: int
    }

and private BlockingWorkerConfig =
    {
        ActiveWorkItemQueue: MailboxQueue<WorkItem>
        BlockingSignalQueue: MailboxQueue<Channel<obj>>
    }

and [<Struct>] private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem workItem =
        config.Runtime.InterpretAsync workItem config.EvaluationSteps config.ActiveWorkItemQueue

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}" <| fun cancelToken ->
            task {
                let mutable loop = true
                while loop && not cancelToken.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemQueue.WaitToReadAsync cancelToken
                    if not hasWorkItem || cancelToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemQueue.ReadAsync()
                        if not (workItem.FiberContext.IsTerminal()) then
                            try
                                do! processWorkItem workItem
                            with exn ->
                                try
                                    do!
                                        workItem.FiberContext.CompleteAndReschedule(
                                            Error(exn :> obj),
                                            config.ActiveWorkItemQueue)
                                with _ ->
                                    ()
                                raise exn
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

and private BlockingWorker(config: BlockingWorkerConfig, workerId: int) =

    let processBlockingChannel (blockingChannel: Channel<obj>) =
        task {
            try
                let mutable keepProcessing = true
                while keepProcessing do
                    let! rescheduled =
                        blockingChannel.TryRescheduleNextBlockingWorkItem config.ActiveWorkItemQueue
                    let hasPendingMessages = blockingChannel.Count > 0
                    let hasBlockedWorkItems = blockingChannel.BlockingWorkItemCount > 0
                    keepProcessing <- rescheduled && hasPendingMessages && hasBlockedWorkItems
            finally
                blockingChannel.EndSignalProcessing()

            if
                blockingChannel.Count > 0
                && blockingChannel.BlockingWorkItemCount > 0
                && blockingChannel.TryBeginSignalProcessing()
            then
                do! config.BlockingSignalQueue.WriteAsync blockingChannel
        }

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"BlockingWorker-{workerId}" <| fun cancelToken ->
            task {
                let mutable loop = true
                while loop && not cancelToken.IsCancellationRequested do
                    let! hasBlockingItem = config.BlockingSignalQueue.WaitToReadAsync cancelToken
                    if not hasBlockingItem || cancelToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! blockingSignal = config.BlockingSignalQueue.ReadAsync()
                        do! processBlockingChannel blockingSignal
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

/// A multi-threaded, event-driven runtime with custom fibers and constant-time handling of blocked fibers.
and SignalingRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemQueue = MailboxQueue<WorkItem>()
    let blockingSignalQueue = MailboxQueue<Channel<obj>>()

    let signalBlockingWorkerIfPending (channel: Channel<obj>) : ValueTask =
        if
            channel.Count > 0
            && channel.BlockingWorkItemCount > 0
            && channel.TryBeginSignalProcessing()
        then
            blockingSignalQueue.WriteAsync channel
        else
            ValueTask.CompletedTask

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

    let struct (blockingWorkers, evaluationWorkers) =
        WorkerBuilders.buildPairedWorkers
            config.BlockingWorkers
            config.EvaluationWorkers
            (fun i ->
                new BlockingWorker(
                    {
                        ActiveWorkItemQueue = activeWorkItemQueue
                        BlockingSignalQueue = blockingSignalQueue
                    },
                    i
                ))
            (fun i blockingWorker ->
                new EvaluationWorker(
                    {
                        Runtime = this
                        ActiveWorkItemQueue = activeWorkItemQueue
                        BlockingWorker = blockingWorker
                        EvaluationSteps = config.EvaluationSteps
                    },
                    i
                ))

    override _.Name =
        "SignalingRuntime"

    interface IDisposable with

        member _.Dispose () =
            blockingWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())
            evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    /// Creates the runtime with the default worker configuration.
    new() = new SignalingRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal _.InterpretAsync
        (workItem: WorkItem)
        (evaluationSteps: int)
        (activeWorkItemQueue: MailboxQueue<WorkItem>) =
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
                        if activeWorkItemQueue.Count > 0 then
                            let newWorkItem = WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack)
                            newWorkItem.InterruptionSuppressed <- state.InterruptionSuppressed
                            do! activeWorkItemQueue.WriteAsync newWorkItem
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
                                do! signalBlockingWorkerIfPending channel
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
                                    do! signalBlockingWorkerIfPending channel
                                    state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext) ->
                                let _ = setupForkRegistration currentFiberContext fiberContext
                                let workItem = WorkItemPool.Rent(effect, fiberContext, ContStackPool.Rent())
                                do! activeWorkItemQueue.WriteAsync workItem
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
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemQueue
                                    state.Completed <- true
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! value =
                                        if state.InterruptionSuppressed > 0 then
                                            task
                                        else
                                            task.WaitAsync currentFiberContext.CancellationToken
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
                    do! currentFiberContext.CompleteAndReschedule(Ok value, activeWorkItemQueue)
                | CompleteFailure error ->
                    do! currentFiberContext.CompleteAndReschedule(Error error, activeWorkItemQueue)
                | NoCompletion -> ()

                return ()
            finally
                if not state.Completed then
                    ContStackPool.Return state.ContStack

                WorkItemPool.Return workItem
        }

    member private _.Reset () =
        activeWorkItemQueue.Clear()
        blockingSignalQueue.Clear()

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

            activeWorkItemQueue.WriteAsync workItem |> ignore

            fiber)
