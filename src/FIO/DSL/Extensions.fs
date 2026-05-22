/// <summary>Provides instance combinator methods for the FIO effect type.</summary>
[<AutoOpen>]
module FIO.DSL.Extensions

open System
open System.Diagnostics

type FIO<'R, 'E> with

    /// <summary>Transforms the success value of this effect with a pure function.</summary>
    /// <typeparam name="'R1">The success result type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's success value to the new success value.</param>
    /// <returns>An effect that completes with <paramref name="mapper"/> applied to the original success value.</returns>
    member inline this.Map<'R, 'R1, 'E>(mapper: 'R -> 'R1) : FIO<'R1, 'E> =
        this.FlatMap(fun res -> FIO.succeed (mapper res))

    /// <summary>Transforms the typed error of this effect with a pure function.</summary>
    /// <typeparam name="'E1">The error type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's typed error to the new typed error.</param>
    /// <returns>An effect that propagates the original success value, or fails with <paramref name="mapper"/> applied to the original error.</returns>
    member inline this.MapError<'R, 'E, 'E1>(mapper: 'E -> 'E1) : FIO<'R, 'E1> =
        this.CatchAll(fun err -> FIO.fail (mapper err))

    /// <summary>Transforms both the success and error channels of this effect with pure functions.</summary>
    /// <typeparam name="'R1">The success result type produced by <paramref name="successMapper"/>.</typeparam>
    /// <typeparam name="'E1">The error type produced by <paramref name="errorMapper"/>.</typeparam>
    /// <param name="successMapper">A pure function from the original success value to the new success value.</param>
    /// <param name="errorMapper">A pure function from the original typed error to the new typed error.</param>
    /// <returns>An effect with both channels transformed by their respective mappers.</returns>
    member inline this.MapBoth<'R, 'R1, 'E, 'E1>(successMapper: 'R -> 'R1, errorMapper: 'E -> 'E1) : FIO<'R1, 'E1> =
        this.Map(successMapper).MapError errorMapper

    /// <summary>Transforms this effect's success value into unit, discarding the original result.</summary>
    /// <returns>An effect that completes successfully with unit.</returns>
    member inline this.Unit<'R, 'E>() : FIO<unit, 'E> = this.Map(fun _ -> ())

    /// <summary>Transforms this effect's success value into a constant.</summary>
    /// <typeparam name="'R1">The result type of the constant value.</typeparam>
    /// <param name="res">The constant value to produce on success.</param>
    /// <returns>An effect that completes with <paramref name="res"/> on success, replacing the original result.</returns>
    member inline this.As<'R, 'R1, 'E>(res: 'R1) : FIO<'R1, 'E> = this.Map(fun _ -> res)

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Result</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because the original error is moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Ok</c> on success or <c>Error</c> on failure.</returns>
    member inline this.Result<'R, 'E, 'E1>() : FIO<Result<'R, 'E>, 'E1> =
        this.Map(Ok).CatchAll(fun err -> FIO.succeed (Error err))

    /// <summary>Transforms this effect into an infallible effect whose success channel carries an <c>Option</c>, discarding the error.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are mapped to <c>None</c>.</typeparam>
    /// <returns>An effect that completes with <c>Some</c> on success or <c>None</c> on failure.</returns>
    member inline this.Option<'R, 'E, 'E1>() : FIO<'R option, 'E1> =
        this.Map(Some).CatchAll(fun _ -> FIO.succeed None)

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Choice</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Choice1Of2</c> on success or <c>Choice2Of2</c> on failure.</returns>
    member inline this.Choice<'R, 'E, 'E1>() : FIO<Choice<'R, 'E>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun err -> FIO.succeed (Choice2Of2 err))

    /// <summary>Combines this effect with a boolean guard, running it only when the guard is true.</summary>
    /// <param name="cond">The condition controlling whether this effect runs.</param>
    /// <returns>An effect that runs this effect (discarding its result) when <paramref name="cond"/> is true, or completes with unit otherwise.</returns>
    member inline this.When<'R, 'E>(cond: bool) : FIO<unit, 'E> =
        if cond then this.Unit() else FIO.unit ()

    /// <summary>Combines this effect with a boolean guard, running it only when the guard is false.</summary>
    /// <param name="cond">The condition controlling whether this effect is suppressed.</param>
    /// <returns>An effect that runs this effect (discarding its result) when <paramref name="cond"/> is false, or completes with unit otherwise.</returns>
    member inline this.Unless<'R, 'E>(cond: bool) : FIO<unit, 'E> = this.When(not cond)

    /// <summary>Combines this effect with a side-effecting function that observes the success value while preserving it.</summary>
    /// <typeparam name="'R1">The result type produced by the side-effecting function; discarded.</typeparam>
    /// <param name="effOnSuccess">A function from the success value to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs <paramref name="effOnSuccess"/> on success and then completes with the original success value.</returns>
    member inline this.Tap<'R, 'R1, 'E>(effOnSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> effOnSuccess(res).Map(fun _ -> res))

    /// <summary>Combines this effect with a side-effecting function that observes the typed error while preserving it.</summary>
    /// <typeparam name="'R1">The result type produced by the side-effecting function; discarded.</typeparam>
    /// <param name="effOnError">A function from the typed error to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs <paramref name="effOnError"/> on failure and then re-fails with the original typed error.</returns>
    member inline this.TapError<'R, 'R1, 'E>(effOnError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.CatchAll(fun err -> effOnError(err).FlatMap(fun _ -> FIO.fail err))

    /// <summary>Combines this effect with side-effecting observers on both channels while preserving the outcome.</summary>
    /// <typeparam name="'R1">The result type produced by the error observer; discarded.</typeparam>
    /// <typeparam name="'R2">The result type produced by the success observer; discarded.</typeparam>
    /// <param name="onSuccess">A function from the success value to a side-effecting effect whose result is discarded.</param>
    /// <param name="onError">A function from the typed error to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs the matching observer and then completes with the original outcome unchanged.</returns>
    member inline this.TapBoth<'R, 'R1, 'R2, 'E>
        (onSuccess: 'R -> FIO<'R2, 'E>, onError: 'E -> FIO<'R1, 'E>)
        : FIO<'R, 'E> =
        this.Tap(onSuccess).TapError onError

    /// <summary>Combines this effect with a best-effort console print of its success value for debugging.</summary>
    /// <param name="message">The prefix to print before the success value; defaults to <c>"Debug"</c>.</param>
    /// <returns>An effect that prints the success value and completes with it unchanged; printing failures are swallowed.</returns>
    member inline this.Debug<'R, 'E>(?message: string) : FIO<'R, 'E> =
        let message = defaultArg message "Debug"

        this.Tap(fun res ->
            FIO
                .attempt((fun () -> printfn "%s: %A" message res), id)
                .CatchAll(fun ex ->
                    FIO
                        .attempt(
                            (fun () -> eprintfn "Debug print failed. Message: %s, Exception: %s" message ex.Message),
                            id
                        )
                        .CatchAll(fun _ -> FIO.unit ())))

    /// <summary>Combines this effect with a best-effort console print of its typed error for debugging.</summary>
    /// <param name="message">The prefix to print before the error value; defaults to <c>"Debug Error"</c>.</param>
    /// <returns>An effect that prints the typed error and re-fails with it unchanged; printing failures are swallowed.</returns>
    member inline this.DebugError<'R, 'E>(?message: string) : FIO<'R, 'E> =
        let message = defaultArg message "Debug Error"

        this.TapError(fun err ->
            FIO
                .attempt((fun () -> printfn "%s: %A" message err), id)
                .CatchAll(fun ex ->
                    FIO
                        .attempt(
                            (fun () ->
                                eprintfn "Debug Error print failed. Message: %s, Exception: %s" message ex.Message),
                            id
                        )
                        .CatchAll(fun _ -> FIO.unit ())))

    /// <summary>Combines this effect with a fallback effect that runs when this effect fails.</summary>
    /// <typeparam name="'E1">The error type of the fallback effect.</typeparam>
    /// <param name="eff">The fallback effect to evaluate on failure.</param>
    /// <returns>An effect that completes with this effect's success value, or with the fallback effect's outcome on failure.</returns>
    member inline this.OrElse<'R, 'E, 'E1>(eff: FIO<'R, 'E1>) : FIO<'R, 'E1> = this.CatchAll(fun _ -> eff)

    /// <summary>Combines a function-producing effect with this effect, applying the produced function to the success value.</summary>
    /// <typeparam name="'R1">The success result type produced by the function.</typeparam>
    /// <param name="eff">An effect whose success value is a function applied to this effect's success value.</param>
    /// <returns>An effect that completes with the produced function applied to this effect's success value.</returns>
    member inline this.Apply<'R, 'R1, 'E>(eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap this.Map

    /// <summary>Combines a function-producing effect with this effect, applying the produced function to the typed error.</summary>
    /// <typeparam name="'E1">The error type produced by the function.</typeparam>
    /// <param name="eff">An effect whose error channel produces a function applied to this effect's typed error.</param>
    /// <returns>An effect that propagates this effect's success value, or fails with the produced function applied to the original error.</returns>
    member inline this.ApplyError<'R, 'E, 'E1>(eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> = eff.CatchAll this.MapError

    /// <summary>Combines this effect with a second effect sequentially and returns both results as a tuple.</summary>
    /// <typeparam name="'R1">The success result type of the second effect.</typeparam>
    /// <param name="eff">The effect to evaluate after this one.</param>
    /// <returns>An effect that runs both in order and completes with a pair of their results.</returns>
    member inline this.Zip<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.FlatMap(fun res -> eff.Map(fun res' -> res, res'))

    /// <summary>Combines this effect with a second effect, failing with a tuple of both errors when both fail.</summary>
    /// <typeparam name="'E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to evaluate when this effect fails.</param>
    /// <returns>An effect that completes with this effect's success value when it succeeds, or fails with a pair of both errors when both fail.</returns>
    member inline this.ZipError<'R, 'E, 'E1>(eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.CatchAll(fun err -> eff.MapError(fun err' -> err, err'))

    /// <summary>Combines this effect with a second effect sequentially and returns the second result.</summary>
    /// <typeparam name="'R1">The success result type of the second effect; propagated.</typeparam>
    /// <param name="eff">The effect to evaluate after this one; its result is propagated.</param>
    /// <returns>An effect that runs both in order and completes with the second result.</returns>
    member inline this.ZipRight<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R1, 'E> = this.FlatMap(fun _ -> eff)

    /// <summary>Combines this effect with a second effect sequentially and returns the first result.</summary>
    /// <typeparam name="'R1">The success result type of the second effect; discarded.</typeparam>
    /// <param name="eff">The effect to evaluate after this one; its result is discarded.</param>
    /// <returns>An effect that runs both in order and completes with this effect's result.</returns>
    member inline this.ZipLeft<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> eff.Map(fun _ -> res))

    /// <summary>Combines this effect with a second effect concurrently and returns both results as a tuple.</summary>
    /// <typeparam name="'R1">The success result type of the second effect.</typeparam>
    /// <param name="eff">The effect to fork alongside this one.</param>
    /// <returns>An effect that runs both concurrently and completes with a pair of their results.</returns>
    /// <remarks>Both effects run on separate fibers in fiber runtimes.</remarks>
    member inline this.ZipPar<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().FlatMap(fun fiber -> this.FlatMap(fun res -> fiber.Join().Map(fun res' -> res, res')))

    /// <summary>Combines this effect with a second effect concurrently, failing with a tuple of both errors when both fail.</summary>
    /// <param name="eff">The effect to fork alongside this one.</param>
    /// <returns>An effect that completes with this effect's success value when it succeeds, or fails with a pair of both errors when both fail.</returns>
    member inline this.ZipParError<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E * 'E> =
        eff
            .Fork()
            .FlatMap(fun fiber ->
                this
                    .FlatMap(fun res1 -> fiber.Join().Map(fun _ -> res1))
                    .CatchAll(fun err1 ->
                        fiber.Join().FlatMap(fun res2 -> FIO.succeed res2).CatchAll(fun err2 -> FIO.fail (err1, err2))))

    /// <summary>Combines this effect with a second effect concurrently and returns the second result.</summary>
    /// <typeparam name="'R1">The success result type of the second effect; propagated.</typeparam>
    /// <param name="eff">The effect to fork alongside this one; its result is propagated.</param>
    /// <returns>An effect that runs both concurrently and completes with the second result.</returns>
    member inline this.ZipParRight<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.ZipPar(eff).Map(fun (_, res) -> res)

    /// <summary>Combines this effect with a second effect concurrently and returns the first result.</summary>
    /// <typeparam name="'R1">The success result type of the second effect; discarded.</typeparam>
    /// <param name="eff">The effect to fork alongside this one; its result is discarded.</param>
    /// <returns>An effect that runs both concurrently and completes with this effect's result.</returns>
    member inline this.ZipParLeft<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.ZipPar(eff).Map(fun (res, _) -> res)

    /// <summary>Combines this effect's outcome with two pure folds, producing an infallible effect.</summary>
    /// <typeparam name="'R1">The result type produced by both folds.</typeparam>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because both branches succeed.</typeparam>
    /// <param name="onError">A pure function from the typed error to the result value.</param>
    /// <param name="onSuccess">A pure function from the success value to the result value.</param>
    /// <returns>An infallible effect that completes with the appropriate fold applied to the original outcome.</returns>
    member inline this.Fold<'R, 'R1, 'E, 'E1>(onError: 'E -> 'R1, onSuccess: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.Map(onSuccess).CatchAll(fun err -> FIO.succeed (onError err))

    /// <summary>Combines this effect's outcome with two effectful folds, producing the effect from whichever branch matches.</summary>
    /// <typeparam name="'R1">The success result type produced by both folds.</typeparam>
    /// <param name="onError">A function from the typed error to the next effect.</param>
    /// <param name="onSuccess">A function from the success value to the next effect.</param>
    /// <returns>An effect that runs the matching fold and propagates its outcome.</returns>
    member inline this.FoldFIO<'R, 'R1, 'E>(onError: 'E -> FIO<'R1, 'E>, onSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// <summary>Builds an effect that retries this effect on failure up to a maximum number of attempts.</summary>
    /// <param name="maxAttempts">The total number of attempts permitted; must be at least 1.</param>
    /// <param name="onEachRetry">An optional callback receiving the error, the attempt index, and the maximum, and producing an effect to run before each retry.</param>
    /// <returns>An effect that completes with the first success, or fails with the final error after exhausting <paramref name="maxAttempts"/>.</returns>
    member inline this.Retry<'R, 'E>(maxAttempts: int, ?onEachRetry: 'E * int * int -> FIO<unit, 'E>) : FIO<'R, 'E> =
        if maxAttempts < 1 then
            FIO.interrupt (InvalidArgument("maxAttempts", "must be >= 1"), "Invalid argument: maxAttempts must be >= 1")
        else
            FIO.suspend (fun () ->
                let rec loop attemptNumber =
                    this.CatchAll(fun err ->
                        if attemptNumber < maxAttempts then
                            match onEachRetry with
                            | Some callback ->
                                callback(err, attemptNumber, maxAttempts)
                                    .FlatMap(fun () -> FIO.suspend (fun () -> loop (attemptNumber + 1)))
                            | None -> FIO.suspend (fun () -> loop (attemptNumber + 1))
                        else
                            FIO.fail err)

                loop 1)

    /// <summary>Builds an effect that retries this effect on failure and falls back to another effect once attempts are exhausted.</summary>
    /// <typeparam name="'E1">The error type of the fallback effect and resulting effect.</typeparam>
    /// <param name="maxAttempts">The total number of attempts permitted; must be at least 1.</param>
    /// <param name="orElse">A function from the final typed error to the fallback effect.</param>
    /// <param name="onEachRetry">An optional callback receiving the error, the attempt index, and the maximum, and producing an effect to run before each retry.</param>
    /// <returns>An effect that completes with the first success, or with the fallback effect's outcome once retries are exhausted.</returns>
    member inline this.RetryOrElse<'R, 'E, 'E1>
        (maxAttempts: int, orElse: 'E -> FIO<'R, 'E1>, ?onEachRetry: 'E * int * int -> FIO<unit, 'E1>)
        : FIO<'R, 'E1> =
        if maxAttempts < 1 then
            FIO.interrupt (InvalidArgument("maxAttempts", "must be >= 1"), "Invalid argument: maxAttempts must be >= 1")
        else
            FIO.suspend (fun () ->
                let rec loop attempt =
                    if attempt >= maxAttempts then
                        this.CatchAll orElse
                    else
                        this.CatchAll(fun err ->
                            match onEachRetry with
                            | Some callback ->
                                callback(err, attempt, maxAttempts)
                                    .FlatMap(fun () -> FIO.suspend (fun () -> loop (attempt + 1)))
                            | None -> FIO.suspend (fun () -> loop (attempt + 1)))

                loop 1)

    /// <summary>Combines this effect with a timeout, returning <c>None</c> when the deadline is reached before completion.</summary>
    /// <param name="duration">The maximum time to wait for completion.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with <c>Some</c> when this effect finishes in time, or <c>None</c> when the timeout fires first.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes.</remarks>
    member this.Timeout<'R, 'E>(duration: TimeSpan, onError: exn -> 'E) : FIO<'R option, 'E> =
        let timeoutEff = FIO.sleep(duration, onError).FlatMap(fun () -> FIO.succeed None)
        this.Map(Some).Race timeoutEff

    /// <summary>Combines this effect with another concurrently and completes with whichever finishes first.</summary>
    /// <param name="eff">The effect to race against this one.</param>
    /// <returns>An effect that completes with the result of the first racer to terminate.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes.</remarks>
    member this.Race<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        FIO.suspend (fun () ->
            this
                .Fork()
                .FlatMap(fun fiber1 ->
                    eff
                        .Fork()
                        .FlatMap(fun fiber2 ->
                            let resultChan = new Channel<Choice<Result<'R, 'E>, Result<'R, 'E>>>()

                            let inline sendUnit choice : FIO<unit, 'E> =
                                resultChan.Send(choice).Map(fun _ -> ())

                            let waiter1: FIO<unit, 'E> =
                                fiber1
                                    .Join()
                                    .FlatMap(fun r -> sendUnit (Choice1Of2(Ok r)))
                                    .CatchAll(fun e -> sendUnit (Choice1Of2(Error e)))
                                    .CatchAll(fun _ -> FIO.succeed ())

                            let waiter2: FIO<unit, 'E> =
                                fiber2
                                    .Join()
                                    .FlatMap(fun r -> sendUnit (Choice2Of2(Ok r)))
                                    .CatchAll(fun e -> sendUnit (Choice2Of2(Error e)))
                                    .CatchAll(fun _ -> FIO.succeed ())

                            waiter1
                                .Fork()
                                .FlatMap(fun _ ->
                                    waiter2
                                        .Fork()
                                        .FlatMap(fun _ ->
                                            resultChan
                                                .Receive()
                                                .FlatMap(fun choice ->
                                                    let interruptLoser =
                                                        match choice with
                                                        | Choice1Of2 _ ->
                                                            FIO
                                                                .attempt(
                                                                    (fun () ->
                                                                        fiber2.Context.Interrupt(
                                                                            ExplicitInterrupt,
                                                                            "Lost race"
                                                                        )),
                                                                    id
                                                                )
                                                                .CatchAll(fun _ -> FIO.unit ())
                                                        | Choice2Of2 _ ->
                                                            FIO
                                                                .attempt(
                                                                    (fun () ->
                                                                        fiber1.Context.Interrupt(
                                                                            ExplicitInterrupt,
                                                                            "Lost race"
                                                                        )),
                                                                    id
                                                                )
                                                                .CatchAll(fun _ -> FIO.unit ())

                                                    interruptLoser.FlatMap(fun () ->
                                                        let result =
                                                            match choice with
                                                            | Choice1Of2 res -> res
                                                            | Choice2Of2 res -> res

                                                        match result with
                                                        | Ok res -> FIO.succeed res
                                                        | Error err -> FIO.fail err)))))))

    /// <summary>Combines this effect with timing instrumentation, producing both the elapsed duration and the result.</summary>
    /// <param name="onError">A function that maps an exception thrown by the stopwatch start to the typed error.</param>
    /// <returns>An effect that completes with a tuple of the elapsed time and the original success value.</returns>
    member inline this.Timed<'R, 'E>(onError: exn -> 'E) : FIO<TimeSpan * 'R, 'E> =
        FIO
            .attempt(Stopwatch.StartNew, onError)
            .FlatMap(fun sw ->
                this
                    .Ensuring(
                        FIO
                            .unit()
                            .FlatMap(fun () ->
                                sw.Stop()
                                FIO.unit ())
                    )
                    .Map(fun res -> sw.Elapsed, res))

    /// <summary>Builds an effect that runs this effect a fixed number of times and returns the last result.</summary>
    /// <param name="n">The number of repetitions; values less than or equal to 1 evaluate this effect once.</param>
    /// <returns>An effect that completes with the result of the final iteration.</returns>
    member this.RepeatN<'R, 'E>(n: int) : FIO<'R, 'E> =
        if n <= 1 then
            this
        else
            this.FlatMap(fun _ -> FIO.suspend (fun () -> this.RepeatN(n - 1)))

    /// <summary>Combines this effect with a partial recovery function, recovering matched errors and re-raising the rest.</summary>
    /// <param name="pf">A function returning <c>Some</c> recovery effect for handled errors, or <c>None</c> to propagate the original error.</param>
    /// <returns>An effect that recovers from matched failures and re-fails with unmatched errors.</returns>
    member inline this.CatchSome<'R, 'E>(pf: 'E -> FIO<'R, 'E> option) : FIO<'R, 'E> =
        this.CatchAll(fun err ->
            match pf err with
            | Some recovery -> recovery
            | None -> FIO.fail err)

    /// <summary>Transforms every typed error of this effect into a fiber interruption, producing an infallible effect.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures interrupt the fiber.</typeparam>
    /// <param name="toMessage">A function from the typed error to the interruption message.</param>
    /// <returns>An infallible effect that completes with the original success value, or interrupts the fiber on failure.</returns>
    member inline this.OrInterrupt<'R, 'E, 'E1>(toMessage: 'E -> string) : FIO<'R, 'E1> =
        this.CatchAll(fun err ->
            FIO.interrupt (ResourceExhaustion(toMessage err), "Fiber interrupted due to unrecoverable error"))
