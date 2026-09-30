module FIO.Runtime.Signaling

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading

type private EvaluationWorkerConfig =
    {
        Runtime: SignalingRuntime
        ActiveWorkItemQueue: MailboxQueue<WorkItem>
        EvaluationSteps: int
    }

and [<Struct>] private CompletionAction =
    | NoCompletion
    | CompleteSuccess of successValue: obj
    | CompleteFailure of failureError: obj

and private EvaluationWorker(config: EvaluationWorkerConfig, workerId: int) =

    let processWorkItem workItem =
        config.Runtime.InterpretAsync workItem config.EvaluationSteps config.ActiveWorkItemQueue

    let struct (cancelSource, _workerTask) =
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}" <| fun cancellationToken ->
            task {
                let mutable loop = true
                while loop && not cancellationToken.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemQueue.WaitToReadAsync cancellationToken
                    if not hasWorkItem || cancellationToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemQueue.ReadAsync()

                        if not (workItem.FiberContext.IsCompleted()) then
                            let fiberContext = workItem.FiberContext
                            try
                                do! processWorkItem workItem
                            with ex ->
                                try
                                    do!
                                        fiberContext.CompleteAndReschedule(
                                            Error(defectError fiberContext ex),
                                            config.ActiveWorkItemQueue)
                                with _ ->
                                    ()

                                Console.Error.WriteLine
                                    $"FIO EvaluationWorker-{workerId} recovered from an unhandled effect exception: {ex}"
            }

    interface IDisposable with

        member _.Dispose () =
            cancelSource.Cancel()
            cancelSource.Dispose()

/// A multi-threaded, event-driven runtime with custom fibers. Blocked channel reads async-park on the
/// channel's native wait; blocked fiber joins park until the joined fiber completes.
and SignalingRuntime(config: WorkerConfig) as this =
    inherit FIOWorkerRuntime(config)

    let activeWorkItemQueue = MailboxQueue<WorkItem>()

    let evaluationWorkers =
        List.init config.EvaluationWorkers (fun i ->
            new EvaluationWorker(
                {
                    Runtime = this
                    ActiveWorkItemQueue = activeWorkItemQueue
                    EvaluationSteps = config.EvaluationSteps
                },
                i
            ))

    do this.StopWorkers <- fun () ->
        evaluationWorkers |> List.iter (fun w -> (w :> IDisposable).Dispose())

    override _.Name =
        "SignalingRuntime"

    /// Creates the runtime with the default worker configuration.
    new() = new SignalingRuntime(WorkerConfig.Default)

    [<TailCall>]
    member internal runtime.InterpretAsync
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
                            let newWorkItem =
                                resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
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
                            | HandleWriteChan(message, channel, reportAccepted) ->
                                match tryWriteChannel channel message reportAccepted with
                                | Written result ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded result)
                                | MustWait ->
                                    parkUntilWritable
                                        channel
                                        state.Effect
                                        currentFiberContext
                                        state.ContStack
                                        state.InterruptionSuppressed
                                        (fun workItem -> activeWorkItemQueue.WriteAsync workItem |> ignore)
                                    state.Completed <- true
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead(&value) then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                else
                                    let waited =
                                        if state.InterruptionSuppressed > 0 then
                                            channel.Queue.WaitToReadAsync CancellationToken.None
                                        else
                                            channel.Queue.WaitToReadAsync currentFiberContext.CancellationToken

                                    if waited.IsCompletedSuccessfully then
                                        waited.GetAwaiter().GetResult() |> ignore
                                    else
                                        let fiberContext = currentFiberContext
                                        let suppressed = state.InterruptionSuppressed
                                        let contStack = state.ContStack
                                        let readEffect = state.Effect

                                        let resume () =
                                            try
                                                let resumeEffect =
                                                    if suppressed = 0 && fiberContext.CancellationToken.IsCancellationRequested then
                                                        match interruptionFor fiberContext "Fiber was interrupted while blocked on a channel read." with
                                                        | :? FiberInterruptedException as interruption ->
                                                            Interrupt(interruption.cause, interruption.message)
                                                        | _ ->
                                                            Interrupt(ExplicitInterrupt, "Fiber was interrupted while blocked on a channel read.")
                                                    else
                                                        readEffect
                                                let resumeWorkItem =
                                                    {
                                                        Effect = resumeEffect
                                                        FiberContext = fiberContext
                                                        ContStack = contStack
                                                        InterruptionSuppressed = suppressed
                                                    }
                                                activeWorkItemQueue.WriteAsync resumeWorkItem |> ignore
                                            with _ ->
                                                ()

                                        waited.GetAwaiter().OnCompleted(Action resume)
                                        state.Completed <- true
                            | HandleForkEffect(effect, fiber, fiberContext, daemon) ->
                                attachFork currentFiberContext fiberContext daemon (state.InterruptionSuppressed > 0)
                                if daemon then runtime.TrackDaemon fiberContext
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
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    let waiter =
                                        parkBlockingWaiter currentFiberContext state.InterruptionSuppressed newWorkItem (fun wi ->
                                            activeWorkItemQueue.WriteAsync wi |> ignore)
                                    do! fiberContext.AddBlockingWorkItem waiter
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemQueue
                                    state.Completed <- true
                            | HandleJoinFirst fiberContexts ->
                                match tryFindTerminalIndex fiberContexts with
                                | index when index >= 0 ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box index))
                                | _ ->
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    parkJoinFirstOnQueue fiberContexts currentFiberContext state.InterruptionSuppressed newWorkItem activeWorkItemQueue
                                    state.Completed <- true
                            | HandleJoinAllFailFast fiberContexts ->
                                match tryCompleteJoinAll fiberContexts with
                                | ValueSome outcome ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box outcome))
                                | ValueNone ->
                                    let newWorkItem =
                                        resumeWith &state (WorkItemPool.Rent(state.Effect, currentFiberContext, state.ContStack))
                                    parkJoinAllFailFastOnQueue fiberContexts currentFiberContext state.InterruptionSuppressed newWorkItem activeWorkItemQueue
                                    state.Completed <- true
                            | HandleAwaitTask(awaited, onError) ->
                                let waited = awaitedTask awaited state.InterruptionSuppressed currentFiberContext

                                if waited.IsCompletedSuccessfully then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded waited.Result)
                                else
                                    parkOnTask
                                        waited
                                        currentFiberContext
                                        state.ContStack
                                        state.InterruptionSuppressed
                                        onError
                                        (fun workItem -> activeWorkItemQueue.WriteAsync workItem |> ignore)

                                    state.Completed <- true

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

    /// Schedules the given effect on a new fiber and returns immediately with a handle to it. Safe to
    /// call concurrently and as often as you like: it never waits for, interrupts, or discards any
    /// fiber already running on this runtime.
    override this.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        let fiber = new Fiber<'A, 'E>()
        this.Track fiber.Context

        let workItem =
            WorkItemPool.Rent(effect.UpcastBoth(), fiber.Context, ContStackPool.Rent())

        activeWorkItemQueue.WriteAsync workItem |> ignore

        fiber
