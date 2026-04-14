/// Instance combinator methods for the FIO effect type.
[<AutoOpen>]
module FIO.DSL.Extensions

open System
open System.Diagnostics

type FIO<'R, 'E> with

    /// Maps a function over the result.
    member inline this.Map<'R, 'R1, 'E>(mapper: 'R -> 'R1) : FIO<'R1, 'E> =
        this.FlatMap(fun res -> FIO.succeed (mapper res))

    /// Maps a function over the error.
    member inline this.MapError<'R, 'E, 'E1>(mapper: 'E -> 'E1) : FIO<'R, 'E1> =
        this.CatchAll(fun err -> FIO.fail (mapper err))

    /// Transforms both success and error channels.
    member inline this.MapBoth<'R, 'R1, 'E, 'E1>(successMapper: 'R -> 'R1, errorMapper: 'E -> 'E1) : FIO<'R1, 'E1> =
        this.Map(successMapper).MapError errorMapper

    /// Discards the result, returning unit.
    member inline this.Unit<'R, 'E>() : FIO<unit, 'E> = this.Map(fun _ -> ())

    /// Maps the result to a constant value.
    member inline this.As<'R, 'R1, 'E>(res: 'R1) : FIO<'R1, 'E> = this.Map(fun _ -> res)

    /// Converts the error channel into a Result, making the effect infallible.
    member inline this.Result<'R, 'E, 'E1>() : FIO<Result<'R, 'E>, 'E1> =
        this.Map(Ok).CatchAll(fun err -> FIO.succeed (Error err))

    /// Returns Some on success, None on failure.
    member inline this.Option<'R, 'E, 'E1>() : FIO<'R option, 'E1> =
        this.Map(Some).CatchAll(fun _ -> FIO.succeed None)

    /// Returns Choice1Of2 on success, Choice2Of2 on failure.
    member inline this.Choice<'R, 'E, 'E1>() : FIO<Choice<'R, 'E>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun err -> FIO.succeed (Choice2Of2 err))

    /// Executes only if the condition is true, otherwise succeeds with unit.
    member inline this.When<'R, 'E>(cond: bool) : FIO<unit, 'E> =
        if cond then this.Unit() else FIO.unit ()

    /// Executes only if the condition is false, otherwise succeeds with unit.
    member inline this.Unless<'R, 'E>(cond: bool) : FIO<unit, 'E> = this.When(not cond)

    /// Executes a side effect on success, preserving the original value.
    member inline this.Tap<'R, 'R1, 'E>(effOnSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> effOnSuccess(res).Map(fun _ -> res))

    /// Executes a side effect on error, preserving the original error.
    member inline this.TapError<'R, 'R1, 'E>(effOnError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.CatchAll(fun err -> effOnError(err).FlatMap(fun _ -> FIO.fail err))

    /// Executes side effects on both success and error.
    member inline this.TapBoth<'R, 'R1, 'R2, 'E>
        (onSuccess: 'R -> FIO<'R2, 'E>, onError: 'E -> FIO<'R1, 'E>)
        : FIO<'R, 'E> =
        this.Tap(onSuccess).TapError onError

    /// Prints the success value to stdout for debugging (best-effort).
    /// <param name="message">The message prefix to print before the value.</param>
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

    /// Prints the error value to stdout for debugging (best-effort).
    /// <param name="message">The message prefix to print before the error.</param>
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

    /// Falls back to another effect on failure.
    member inline this.OrElse<'R, 'E, 'E1>(eff: FIO<'R, 'E1>) : FIO<'R, 'E1> = this.CatchAll(fun _ -> eff)

    /// Applies a function-producing effect to this effect's result.
    member inline this.Apply<'R, 'R1, 'E>(eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap this.Map

    /// Applies a function-producing effect to this effect's error.
    member inline this.ApplyError<'R, 'E, 'E1>(eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> = eff.CatchAll this.MapError

    /// Sequences two effects, returning a tuple of results.
    member inline this.Zip<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.FlatMap(fun res -> eff.Map(fun res' -> res, res'))

    /// Sequences two effects, failing with a tuple of errors when both fail.
    member inline this.ZipError<'R, 'E, 'E1>(eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.CatchAll(fun err -> eff.MapError(fun err' -> err, err'))

    /// Sequences two effects, returning the second result.
    member inline this.ZipRight<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R1, 'E> = this.FlatMap(fun _ -> eff)

    /// Sequences two effects, returning the first result.
    member inline this.ZipLeft<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> eff.Map(fun _ -> res))

    /// Executes two effects in parallel, returning a tuple of results.
    member inline this.ZipPar<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().FlatMap(fun fiber -> this.FlatMap(fun res -> fiber.Join().Map(fun res' -> res, res')))

    /// Executes two effects in parallel, failing with a tuple of errors when both fail.
    member inline this.ZipParError<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E * 'E> =
        eff
            .Fork()
            .FlatMap(fun fiber ->
                this
                    .FlatMap(fun res1 -> fiber.Join().Map(fun _ -> res1))
                    .CatchAll(fun err1 ->
                        fiber.Join().FlatMap(fun res2 -> FIO.succeed res2).CatchAll(fun err2 -> FIO.fail (err1, err2))))

    /// Executes two effects in parallel, returning the second result.
    member inline this.ZipParRight<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.ZipPar(eff).Map(fun (_, res) -> res)

    /// Executes two effects in parallel, returning the first result.
    member inline this.ZipParLeft<'R, 'R1, 'E>(eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.ZipPar(eff).Map(fun (res, _) -> res)

    /// Folds over both channels with pure functions, producing an infallible result.
    member inline this.Fold<'R, 'R1, 'E, 'E1>(onError: 'E -> 'R1, onSuccess: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.Map(onSuccess).CatchAll(fun err -> FIO.succeed (onError err))

    /// Folds over both channels with effectful functions.
    member inline this.FoldFIO<'R, 'R1, 'E>(onError: 'E -> FIO<'R1, 'E>, onSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// Retries on failure up to the specified number of attempts.
    /// <param name="maxAttempts">The maximum total number of attempts (must be >= 1).</param>
    /// <param name="onEachRetry">Optional callback executed after each failure before retrying.</param>
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

    /// Retries on failure, falling back to another effect on exhaustion.
    /// <param name="maxAttempts">The maximum total number of attempts (must be >= 1).</param>
    /// <param name="orElse">The fallback effect to run with the last error if all retries fail.</param>
    /// <param name="onEachRetry">Optional callback executed after each failure before retrying.</param>
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

    /// Returns None if the effect doesn't complete within the duration, Some on success.
    /// <param name="duration">The maximum duration to wait.</param>
    member this.Timeout<'R, 'E>(duration: TimeSpan, onError: exn -> 'E) : FIO<'R option, 'E> =
        let timeoutEff = FIO.sleep(duration, onError).FlatMap(fun () -> FIO.succeed None)
        this.Map(Some).Race timeoutEff

    /// Races against another effect, returning whichever completes first.
    member this.Race<'R, 'E>(eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        FIO.suspend (fun () ->
            this
                .Fork()
                .FlatMap(fun fiber1 ->
                    eff
                        .Fork()
                        .FlatMap(fun fiber2 ->
                            let resultChan = new Channel<Choice<Result<'R, 'E>, Result<'R, 'E>>>()

                            let waiter1: FIO<Choice<Result<'R, 'E>, Result<'R, 'E>>, 'E> =
                                fiber1
                                    .Join()
                                    .FlatMap(fun r -> resultChan.Send(Choice1Of2(Ok r)))
                                    .CatchAll(fun e -> resultChan.Send(Choice1Of2(Error e)))
                                    .CatchAll(fun _ -> FIO.succeed Unchecked.defaultof<_>)

                            let waiter2: FIO<Choice<Result<'R, 'E>, Result<'R, 'E>>, 'E> =
                                fiber2
                                    .Join()
                                    .FlatMap(fun r -> resultChan.Send(Choice2Of2(Ok r)))
                                    .CatchAll(fun e -> resultChan.Send(Choice2Of2(Error e)))
                                    .CatchAll(fun _ -> FIO.succeed Unchecked.defaultof<_>)

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

    /// Measures execution time, returning the duration and result.
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

    /// Repeats N times, returning the last result.
    member this.RepeatN<'R, 'E>(n: int) : FIO<'R, 'E> =
        if n <= 1 then
            this
        else
            this.FlatMap(fun _ -> FIO.suspend (fun () -> this.RepeatN(n - 1)))

    /// Recovers from specific errors using a partial function.
    /// <param name="pf">Returns Some effect for recoverable errors, None to re-raise.</param>
    member inline this.CatchSome<'R, 'E>(pf: 'E -> FIO<'R, 'E> option) : FIO<'R, 'E> =
        this.CatchAll(fun err ->
            match pf err with
            | Some recovery -> recovery
            | None -> FIO.fail err)

    /// Converts all errors to interruptions, making the effect infallible.
    /// <param name="toMessage">Converts the error to an interruption message.</param>
    member inline this.OrInterrupt<'R, 'E, 'E1>(toMessage: 'E -> string) : FIO<'R, 'E1> =
        this.CatchAll(fun err ->
            FIO.interrupt (ResourceExhaustion(toMessage err), "Fiber interrupted due to unrecoverable error"))
