module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks

/// A runtime with no scheduler of its own: each fiber is a .NET task on the thread pool and a blocked fiber awaits.
/// Handy for tests and as the baseline the other runtimes are measured against.
type DirectRuntime() =
    inherit FIORuntime()



    override _.Name : string = "DirectRuntime"

    [<TailCall>]
    member private this.InterpretAsync effect (currentFiberContext: FiberContext) =
        let mutable state =
            InterpreterState(effect, ContStackPool.Rent(), currentFiberContext, 0)

        let mutable result = ValueNone

        let inline onSuccessComplete value =
            result <- ValueSome <| Ok value

        let inline onErrorComplete error =
            result <- ValueSome <| Error error

        task {
            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
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
                    else
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone ->
                            ()
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
                                    try
                                        let cancellationToken =
                                            if state.InterruptionSuppressed > 0 then
                                                CancellationToken.None
                                            else
                                                currentFiberContext.CancellationToken

                                        do! channel.Queue.WriteAsync(message, cancellationToken)

                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            (OutcomeSucceeded(if reportAccepted then box true else message))
                                    with
                                    | :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            (OutcomeInterrupted (interruptionFor currentFiberContext "Fiber was interrupted while blocked on a channel write."))
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead &value then
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded value)
                                else
                                    try
                                        let mutable waiting = true
                                        while waiting do
                                            let! _ =
                                                if state.InterruptionSuppressed > 0 then
                                                    channel.Queue.WaitToReadAsync CancellationToken.None
                                                else
                                                    channel.Queue.WaitToReadAsync currentFiberContext.CancellationToken
                                            if channel.Queue.TryRead &value then
                                                waiting <- false
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
                                            (OutcomeInterrupted (interruptionFor currentFiberContext "Fiber was interrupted while blocked on a channel read."))
                            | HandleForkEffect(effect, fiber, fiberContext, daemon) ->
                                attachFork currentFiberContext fiberContext daemon (state.InterruptionSuppressed > 0)
                                if daemon then this.TrackDaemon fiberContext
                                Task.Run(fun () -> this.RunFiber effect fiberContext :> Task) |> ignore
                                processOutcome
                                    &state
                                    onSuccessComplete
                                    onErrorComplete
                                    (OutcomeSucceeded fiber)
                            | HandleJoinFiber fiberContext ->
                                try
                                    let! value =
                                        awaitedTask fiberContext.Task state.InterruptionSuppressed currentFiberContext
                                    processResult
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        value
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupted (interruptionFor currentFiberContext "Fiber was interrupted while blocked on a fiber join."))
                            | HandleJoinFirst fiberContexts ->
                                let contexts = List.toArray fiberContexts
                                try
                                    let whenAny =
                                        Task.WhenAny(contexts |> Array.map (fun fiberContext -> fiberContext.Task :> Task))
                                    let! _ =
                                        if state.InterruptionSuppressed > 0 then
                                            whenAny
                                        else
                                            whenAny.WaitAsync currentFiberContext.CancellationToken
                                    let index = contexts |> Array.findIndex (fun fiberContext -> fiberContext.IsTerminal())
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box index))
                                with
                                | :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeInterrupted (interruptionFor currentFiberContext "Fiber was interrupted while blocked on a fiber join."))
                            | HandleJoinAllFailFast fiberContexts ->
                                match tryCompleteJoinAll fiberContexts with
                                | ValueSome outcome ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (OutcomeSucceeded(box outcome))
                                | ValueNone ->
                                    try
                                        let! outcome =
                                            awaitJoinAllSettled
                                                fiberContexts
                                                (state.InterruptionSuppressed > 0)
                                                currentFiberContext
                                                currentFiberContext.CancellationToken
                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            outcome
                                    with
                                    | :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        processOutcome
                                            &state
                                            onSuccessComplete
                                            onErrorComplete
                                            (OutcomeInterrupted (interruptionFor currentFiberContext "Fiber was interrupted while blocked on a fiber join."))
                            | HandleAwaitTask(task, onError) ->
                                try
                                    let! value =
                                        awaitedTask task state.InterruptionSuppressed currentFiberContext
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
                                        (OutcomeInterrupted (interruptionFor currentFiberContext "Task has been cancelled."))
                                | ex ->
                                    processOutcome
                                        &state
                                        onSuccessComplete
                                        onErrorComplete
                                        (awaitTaskFailureOutcome currentFiberContext onError ex)
                return result.Value
            finally
                ContStackPool.Return state.ContStack
        }

    // The only call to InterpretAsync from outside itself. A fork is a concurrent hand-off, so it
    // can never be a tail call; keeping it here is what lets [<TailCall>] guard InterpretAsync.
    member private this.RunFiber (effect: FIO<obj, obj>) (fiberContext: FiberContext) =
        task {
            try
                let! value = this.InterpretAsync effect fiberContext
                fiberContext.Complete value
            with ex ->
                fiberContext.Complete <| Error(defectError fiberContext ex)
        }

    /// Schedules the given effect on a new fiber and returns its handle at once; it never waits for, interrupts, or
    /// discards a fiber already running, so call it as often as you like.
    override this.Run<'A, 'E> (effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        let fiber = new Fiber<'A, 'E>()
        this.Track fiber.Context
        Task.Run(fun () -> this.RunFiber (effect.UpcastBoth()) fiber.Context :> Task) |> ignore
        fiber
