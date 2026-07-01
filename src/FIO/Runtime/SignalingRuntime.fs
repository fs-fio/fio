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
        WorkerLifecycle.startWorker $"EvaluationWorker-{workerId}" <| fun cancelToken ->
            task {
                let mutable loop = true
                while loop && not cancelToken.IsCancellationRequested do
                    let! hasWorkItem = config.ActiveWorkItemQueue.WaitToReadAsync cancelToken
                    if not hasWorkItem || cancelToken.IsCancellationRequested then
                        loop <- false
                    else
                        let! workItem = config.ActiveWorkItemQueue.ReadAsync()

                        if not (workItem.FiberContext.IsCompleted()) then
                            try
                                do! processWorkItem workItem
                            with ex ->
                                try
                                    do!
                                        workItem.FiberContext.CompleteAndReschedule(
                                            Error(ex :> obj),
                                            config.ActiveWorkItemQueue)
                                with _ ->
                                    ()
                                raise ex
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

    let mutable currentFiber: FiberContext option = None

    let runLock = obj ()

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

    override _.Name =
        "SignalingRuntime"

    interface IDisposable with

        member _.Dispose () =
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
                                    let waiter =
                                        parkBlockingWaiter currentFiberContext state.InterruptionSuppressed newWorkItem (fun wi ->
                                            activeWorkItemQueue.WriteAsync wi |> ignore)
                                    do! fiberContext.AddBlockingWorkItem waiter
                                    let! _ = fiberContext.TryRescheduleBlockingWorkItems activeWorkItemQueue
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
                                                let ex =
                                                    match waited.Exception with
                                                    | null -> OperationCanceledException() :> exn
                                                    | aggregate ->
                                                        match aggregate.InnerException with
                                                        | null -> aggregate :> exn
                                                        | inner -> inner
                                                let error =
                                                    try onError ex
                                                    with _ -> ex :> obj
                                                Failure error

                                        let resumeWorkItem =
                                            {
                                                Effect = resumeEffect
                                                FiberContext = fiberContext
                                                ContStack = contStack
                                                InterruptionSuppressed = suppressed
                                            }

                                        try
                                            activeWorkItemQueue.WriteAsync resumeWorkItem |> ignore
                                        with _ ->
                                            ()

                                    waited.GetAwaiter().OnCompleted(Action resume)
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

    member private _.Reset () =
        activeWorkItemQueue.Clear()

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
