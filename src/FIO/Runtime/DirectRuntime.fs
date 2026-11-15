module FIO.Runtime.Direct

open FIO.DSL
open FIO.Runtime.InterpreterCore

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

type internal ContStackPool private () =
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
            Stack<Cont> ContStackLimits.DefaultCapacity

    static member inline Return(stack: Stack<Cont>) =
        let mutable pool = ContStackPool.pool
        if isNull pool then
            pool <- Stack<_>()
            ContStackPool.pool <- pool

        if pool.Count < ContStackLimits.MaxPool && stack.Count <= ContStackLimits.MaxDepth then
            stack.Clear()
            pool.Push stack

/// A runtime with no scheduler of its own: each fiber is a .NET task on the thread pool and a blocked fiber awaits.
type DirectRuntime() =
    inherit FIORuntime()

    /// The runtime's name, "DirectRuntime".
    override _.Name: string = "DirectRuntime"

    [<TailCall>]
    member private this.InterpretAsync effect (currentFiberContext: FiberContext) =
        let mutable state =
            InterpreterState(effect, ContStackPool.Rent(), currentFiberContext, 0)

        let mutable result = ValueNone

        let inline onSuccessComplete value = result <- ValueSome <| Ok value

        let inline onErrorComplete error = result <- ValueSome <| Error error

        let inline settle outcome =
            processOutcome &state onSuccessComplete onErrorComplete outcome

        let inline interrupted message =
            settle (OutcomeInterrupted(interruptionFor currentFiberContext message))

        task {
            try
                while not state.Completed do
                    if
                        state.InterruptionSuppressed = 0
                        && currentFiberContext.CancellationToken.IsCancellationRequested
                    then
                        match! currentFiberContext.Task with
                        | Ok _ -> raise (InvalidOperationException "Fiber was cancelled but completed successfully.")
                        | Error error -> settle (OutcomeInterrupted error)
                    else
                        match handleSharedCase &state onSuccessComplete onErrorComplete with
                        | ValueNone -> ()
                        | ValueSome runtimeCase ->
                            match runtimeCase with
                            | HandleWriteChan(message, channel, reportAccepted) ->
                                match tryWriteChannel channel message reportAccepted with
                                | Written result -> settle (OutcomeSucceeded result)
                                | MustWait ->
                                    try
                                        let cancellationToken =
                                            if state.InterruptionSuppressed > 0 then
                                                CancellationToken.None
                                            else
                                                currentFiberContext.CancellationToken

                                        do! channel.Queue.WriteAsync(message, cancellationToken)

                                        settle (OutcomeSucceeded message)
                                    with :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        interrupted "Fiber was interrupted while blocked on a channel write."
                            | HandleReadChan channel ->
                                let mutable value = Unchecked.defaultof<_>
                                if channel.Queue.TryRead &value then
                                    settle (OutcomeSucceeded value)
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
                                        settle (OutcomeSucceeded value)
                                    with :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        interrupted "Fiber was interrupted while blocked on a channel read."
                            | HandleForkEffect(effect, fiber, fiberContext, daemon) ->
                                attachFork currentFiberContext fiberContext daemon (state.InterruptionSuppressed > 0)
                                if daemon then
                                    this.TrackDaemon fiberContext
                                Task.Run(fun () -> this.RunFiber effect fiberContext :> Task) |> ignore
                                settle (OutcomeSucceeded fiber)
                            | HandleJoinFiber fiberContext ->
                                try
                                    let! value =
                                        awaitedTask fiberContext.Task state.InterruptionSuppressed currentFiberContext
                                    processResult &state onSuccessComplete onErrorComplete value
                                with :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    interrupted "Fiber was interrupted while blocked on a fiber join."
                            | HandleJoinFirst fiberContexts ->
                                let contexts = List.toArray fiberContexts
                                try
                                    let whenAny =
                                        Task.WhenAny(
                                            contexts |> Array.map (fun fiberContext -> fiberContext.Task :> Task)
                                        )
                                    let! _ =
                                        if state.InterruptionSuppressed > 0 then
                                            whenAny
                                        else
                                            whenAny.WaitAsync currentFiberContext.CancellationToken
                                    let index =
                                        contexts |> Array.findIndex (fun fiberContext -> fiberContext.IsTerminal())
                                    settle (OutcomeSucceeded(box index))
                                with :? OperationCanceledException when
                                    currentFiberContext.CancellationToken.IsCancellationRequested ->
                                    interrupted "Fiber was interrupted while blocked on a fiber join."
                            | HandleJoinAllFailFast fiberContexts ->
                                match tryCompleteJoinAll fiberContexts with
                                | ValueSome outcome -> settle (OutcomeSucceeded(box outcome))
                                | ValueNone ->
                                    try
                                        let! outcome =
                                            awaitJoinAllSettled
                                                fiberContexts
                                                (state.InterruptionSuppressed > 0)
                                                currentFiberContext
                                                currentFiberContext.CancellationToken
                                        settle outcome
                                    with :? OperationCanceledException when
                                        currentFiberContext.CancellationToken.IsCancellationRequested ->
                                        interrupted "Fiber was interrupted while blocked on a fiber join."
                            | HandleAwaitTask(task, node) ->
                                try
                                    let! value = awaitedTask task state.InterruptionSuppressed currentFiberContext
                                    settle (OutcomeSucceeded value)
                                with
                                | :? TaskCanceledException when
                                    state.InterruptionSuppressed = 0
                                    && currentFiberContext.CancellationToken.IsCancellationRequested
                                    ->
                                    interrupted "Task has been cancelled."
                                | ex -> settle (awaitTaskFailureOutcome currentFiberContext node ex)
                return result.Value
            finally
                ContStackPool.Return state.ContStack
        }

    // The only call to InterpretAsync from outside itself. A fork is a concurrent hand-off, so it
    // can never be a tail call; keeping it here is what lets [<TailCall>] guard InterpretAsync.
    member private this.RunFiber (effect: FIOBase) (fiberContext: FiberContext) =
        task {
            try
                let! value = this.InterpretAsync effect fiberContext
                fiberContext.Complete value
            with ex ->
                fiberContext.Complete <| Error(defectError fiberContext ex)
        }

    /// Schedules the given effect on a new fiber and returns its handle at once; it never waits for, interrupts, or
    /// discards a fiber already running, so call it as often as you like.
    override this.Run<'A, 'E>(effect: FIO<'A, 'E>) : Fiber<'A, 'E> =
        let fiber = new Fiber<'A, 'E>()
        this.Track fiber.Context
        Task.Run(fun () -> this.RunFiber (effect :> FIOBase) fiber.Context :> Task)
        |> ignore
        fiber
