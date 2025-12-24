(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides instance combinator methods for the FIO effect type.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Extensions

open System
open System.Diagnostics

type FIO<'R, 'E> with

    /// <summary>
    /// Maps a function over the result of this effect.
    /// </summary>
    /// <param name="mapper">The function to apply to the result.</param>
    member inline this.Map<'R, 'R1, 'E> (mapper: 'R -> 'R1) : FIO<'R1, 'E> =
        this.FlatMap(fun res -> FIO.Succeed(mapper res))

    /// <summary>
    /// Maps a function over the error of this effect.
    /// </summary>
    /// <param name="mapper">The function to apply to the error.</param>
    member inline this.MapError<'R, 'E, 'E1> (mapper: 'E -> 'E1) : FIO<'R, 'E1> =
        this.CatchAll(fun err -> FIO.Fail(mapper err))

    /// <summary>
    /// Transforms both the success and error channels of this effect simultaneously.
    /// </summary>
    /// <param name="successMapper">The function to apply to the result.</param>
    /// <param name="errorMapper">The function to apply to the error.</param>
    member inline this.MapBoth<'R, 'R1, 'E, 'E1> (successMapper: 'R -> 'R1, errorMapper: 'E -> 'E1) : FIO<'R1, 'E1> =
        this.Map(successMapper).MapError errorMapper

    /// <summary>
    /// Discards the result of this effect, returning unit instead.
    /// </summary>
    member inline this.Unit<'R, 'E> () : FIO<unit, 'E> =
        this.Map(fun _ -> ())

    /// <summary>
    /// Maps this effect's result to a constant value.
    /// </summary>
    /// <param name="res">The constant value to return.</param>
    member inline this.As<'R, 'R1, 'E> (res: 'R1) : FIO<'R1, 'E> =
        this.Map(fun _ -> res)

    /// <summary>
    /// Converts this effect's error channel into a Result, making the effect infallible.
    /// </summary>
    member inline this.Result<'R, 'E, 'E1> () : FIO<Result<'R, 'E>, 'E1> =
        this.Map(Ok).CatchAll(fun err -> FIO.Succeed(Error err))

    /// <summary>
    /// Converts this effect's error into None, returning Some on success.
    /// </summary>
    member inline this.Option<'R, 'E, 'E1> () : FIO<'R option, 'E1> =
        this.Map(Some).CatchAll(fun _ -> FIO.Succeed None)

    /// <summary>
    /// Converts this effect into a Choice, with Choice1Of2 on success and Choice2Of2 on error.
    /// </summary>
    member inline this.Choice<'R, 'E, 'E1> () : FIO<Choice<'R, 'E>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun err -> FIO.Succeed(Choice2Of2 err))

    /// <summary>
    /// Executes this effect only if the condition is true, otherwise succeeds with unit.
    /// </summary>
    /// <param name="cond">The condition to check.</param>
    member inline this.When<'R, 'E> (cond: bool) : FIO<unit, 'E> =
        if cond then this.Unit()
        else FIO.Unit()

    /// <summary>
    /// Executes this effect only if the condition is false, otherwise succeeds with unit.
    /// </summary>
    /// <param name="cond">The condition to check.</param>
    member inline this.Unless<'R, 'E> (cond: bool) : FIO<unit, 'E> =
        this.When(not cond)

    /// <summary>
    /// Executes an effect on success, discarding its result and preserving the original value.
    /// </summary>
    /// <param name="effOnSuccess">The effect to execute on the success value.</param>
    member inline this.Tap<'R, 'R1, 'E> (effOnSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> effOnSuccess(res).Map(fun _ -> res))

    /// <summary>
    /// Executes an effect on error, discarding its result and preserving the original error.
    /// </summary>
    /// <param name="effOnError">The effect to execute on the error value.</param>
    member inline this.TapError<'R, 'R1, 'E> (effOnError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.CatchAll(fun err -> effOnError(err).Map(fun _ -> err).FlatMap FIO.Fail)

    /// <summary>
    /// Executes an effect on both success and error, for side effects.
    /// </summary>
    /// <param name="onSuccess">The effect to execute on success.</param>
    /// <param name="onError">The effect to execute on error.</param>
    member inline this.TapBoth<'R, 'R1, 'R2, 'E> (onSuccess: 'R -> FIO<'R2, 'E>, onError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.Tap(onSuccess).TapError onError

    /// <summary>
    /// Prints the success value to stdout for debugging, preserving the original value.
    /// The value is printed with the prefix "Debug: " per default.
    /// Any errors from printing are printed to stderr but do not fail the effect (best-effort output).
    /// </summary>
    /// <param name="message">The message prefix to print before the value.</param>
    member inline this.Debug<'R, 'E> (?message: string) : FIO<'R, 'E> =
        let message = defaultArg message "Debug"
        this.Tap(fun res ->
            FIO.Attempt((fun () -> printfn "%s: %A" message res))
                .CatchAll(fun exn ->
                    eprintfn "Debug print failed. Message: %s, Exception: %s" message (exn.Message); FIO.Unit()))

    /// <summary>
    /// Prints the error value to stdout for debugging, preserving the original error.
    /// The error is printed with the prefix "Debug Error: " per default.
    /// Any errors from printing are printed to stderr but do not fail the effect (best-effort output).
    /// </summary>
    /// <param name="message">The message prefix to print before the error.</param>
    member inline this.DebugError<'R, 'E> (?message: string) : FIO<'R, 'E> =
        let message = defaultArg message "Debug Error"
        this.TapError(fun err ->
            FIO.Attempt((fun () -> printfn "%s: %A" message err), id)
                .CatchAll(fun exn ->
                    eprintfn "Debug Error print failed. Message: %s, Exception: %s" message (exn.Message); FIO.Unit()))

    /// <summary>
    /// Falls back to another effect if this effect fails, ignoring the original error.
    /// </summary>
    /// <param name="eff">The fallback effect to run if this effect fails.</param>
    member inline this.OrElse<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E1> =
        this.CatchAll(fun _ -> eff)

    /// <summary>
    /// Applies a function-producing effect to this value-producing effect.
    /// </summary>
    /// <param name="eff">The effect producing a function to apply to this effect's result.</param>
    member inline this.Apply<'R, 'R1, 'E> (eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
        eff.FlatMap this.Map

    /// <summary>
    /// Applies a function-producing effect to this error-producing effect.
    /// </summary>
    /// <param name="eff">The effect producing a function to apply to this effect's error.</param>
    member inline this.ApplyError<'R, 'E, 'E1> (eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll this.MapError
    
    /// <summary>
    /// Sequences two effects and succeeds with a tuple of their results.
    /// </summary>
    /// <param name="eff">The effect to zip with this effect.</param>
    member inline this.Zip<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.FlatMap(fun res -> eff.Map(fun res' -> res, res'))

    /// <summary>
    /// Sequences two effects and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <param name="eff">The effect to zip errors with this effect.</param>
    member inline this.ZipError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.CatchAll(fun err -> eff.MapError(fun err' -> err, err'))

    /// <summary>
    /// Sequences two effects, ignoring the result of the first effect (zipRight).
    /// </summary>
    /// <param name="eff">The effect to run after this effect succeeds.</param>
    member inline this.ZipRight<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(fun _ -> eff)

    /// <summary>
    /// Sequences two effects, ignoring the result of the second effect.
    /// </summary>
    /// <param name="eff">The second effect to run.</param>
    member inline this.ZipLeft<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap(fun res -> eff.Map(fun _ -> res))

    /// <summary>
    /// Executes two effects in parallel, returning a tuple of their results.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipPar<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().FlatMap(fun fiber ->
            this.FlatMap(fun res ->
                fiber.Join().Map(fun res' ->
                    res, res')))

    /// <summary>
    /// Executes two effects concurrently and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipParError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        eff.Fork().FlatMap(fun fiber ->
            this.CatchAll(fun err ->
                fiber.Join().MapError(fun err' ->
                    err, err')))

    /// <summary>
    /// Executes two effects in parallel, returning the result of the second.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipParRight<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.ZipPar(eff).Map(fun (_, res) -> res)

    /// <summary>
    /// Executes two effects in parallel, returning the result of the first.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipParLeft<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.ZipPar(eff).Map(fun (res, _) -> res)

    /// <summary>
    /// Folds over both channels using pure functions, producing an infallible result.
    /// </summary>
    /// <param name="onError">The function to apply to the error.</param>
    /// <param name="onSuccess">The function to apply to the success value.</param>
    member inline this.Fold<'R, 'R1, 'E, 'E1> (onError: 'E -> 'R1, onSuccess: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.Map(onSuccess).CatchAll(fun err -> FIO.Succeed(onError err))

    /// <summary>
    /// Folds over both channels using effectful functions with recovery semantics.
    /// </summary>
    /// <param name="onError">The function to apply to any error, producing a recovery effect.</param>
    /// <param name="onSuccess">The function to apply to the success value, producing an effect.</param>
    member inline this.FoldFIO<'R, 'R1, 'E> (onError: 'E -> FIO<'R1, 'E>, onSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// <summary>
    /// Retries this effect up to N total attempts on failure.
    /// For example, RetryN(3) will attempt the effect up to 3 times total.
    /// </summary>
    /// <param name="maxAttempts">Maximum total number of attempts (must be >= 1).</param>
    member inline this.RetryN<'R, 'E> (maxAttempts: int): FIO<'R, 'E> =
        if maxAttempts < 1 then
            FIO.Fail(ArgumentException "maxAttempts must be >= 1" :> obj :?> 'E)
        else
            FIO.Suspend(fun () ->
                let rec loop attempt =
                    if attempt >= maxAttempts then
                        this
                    else
                        this.CatchAll(fun _ ->
                            FIO.Suspend(fun () -> loop(attempt + 1)))
                loop 1)

    /// <summary>
    /// Times out this effect if it doesn't complete within the specified duration.
    /// Returns None on timeout, Some on success.
    /// </summary>
    /// <param name="duration">The maximum duration to wait.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    member this.Timeout<'R, 'E> (duration: TimeSpan, onError: exn -> 'E) : FIO<'R option, 'E> =
        let timeoutEff =
            FIO.Sleep(duration, onError)
                .FlatMap(fun () -> FIO.Succeed None)
        this.Map(Some)
            .Race timeoutEff

    /// <summary>
    /// Races this effect against another, returning the result of whichever completes first.
    /// The losing fiber is automatically interrupted.
    /// </summary>
    /// <param name="eff">The effect to race against.</param>
    member this.Race<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R, 'E> =
        FIO.Suspend(fun () ->
            this.Fork().FlatMap(fun fiber1 ->
                eff.Fork().FlatMap(fun fiber2 ->
                    let resultChan = new Channel<Choice<Result<'R, 'E>, Result<'R, 'E>>>()

                    let sendResult choice =
                        resultChan.Send(choice).CatchAll(fun _ -> FIO.Succeed choice).Unit()

                    let waiter1 =
                        fiber1.Join()
                            .Map(fun r -> Ok r)
                            .CatchAll(fun e -> FIO.Succeed(Error e))
                            .FlatMap(fun result -> sendResult(Choice1Of2 result))
                            .Fork()

                    let waiter2 =
                        fiber2.Join()
                            .Map(fun r -> Ok r)
                            .CatchAll(fun e -> FIO.Succeed(Error e))
                            .FlatMap(fun result -> sendResult(Choice2Of2 result))
                            .Fork()

                    waiter1.FlatMap(fun _ ->
                        waiter2.FlatMap(fun _ ->
                            resultChan.Receive().FlatMap(fun choice ->
                                let interruptLoser =
                                    match choice with
                                    | Choice1Of2 _ ->
                                        FIO.Attempt(fun () -> fiber2.Internal.Interrupt(ExplicitInterrupt, "Lost race"))
                                            .CatchAll(fun _ -> FIO.Unit())
                                    | Choice2Of2 _ ->
                                        FIO.Attempt(fun () -> fiber1.Internal.Interrupt(ExplicitInterrupt, "Lost race"))
                                            .CatchAll(fun _ -> FIO.Unit())

                                interruptLoser.FlatMap(fun () ->
                                    let result =
                                        match choice with
                                        | Choice1Of2 res -> res
                                        | Choice2Of2 res -> res

                                    match result with
                                    | Ok res -> FIO.Succeed res
                                    | Error err -> FIO.Fail err)))))))

    /// <summary>
    /// Measures the execution time of this effect, returning the duration and result.
    /// </summary>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    member inline this.Timed<'R, 'E> (onError: exn -> 'E) : FIO<TimeSpan * 'R, 'E> =
        FIO.Attempt(Stopwatch.StartNew, onError)
            .FlatMap(fun sw ->
                this.Ensuring(FIO.Unit().FlatMap(fun () -> sw.Stop(); FIO.Unit()))
                    .Map(fun res -> sw.Elapsed, res))

    /// <summary>
    /// Repeats this effect N times, returning the result of the last execution.
    /// </summary>
    /// <param name="n">The number of times to repeat.</param>
    member inline this.RepeatN<'R, 'E> (n: int) : FIO<'R, 'E> =
        let rec loop count =
            if count <= 1 then this
            else this.ZipRight(loop(count - 1))
        loop(n)

    /// <summary>
    /// Recovers from specific errors using a partial function.
    /// </summary>
    /// <param name="pf">A function that returns Some effect for recoverable errors, None otherwise.</param>
    member inline this.CatchSome<'R, 'E> (pf: 'E -> FIO<'R, 'E> option) : FIO<'R, 'E> =
        this.CatchAll(fun err ->
            match pf err with
            | Some recovery -> recovery
            | None -> FIO.Fail err)

    /// <summary>
    /// Converts all errors to defects (interruptions), making the effect infallible.
    /// </summary>
    /// <param name="toMessage">A function to convert the error to an interruption message.</param>
    // member inline this.OrInterrupt<'R, 'E, 'E1> (toMessage: 'E -> string) : FIO<'R, 'E1> =
    //     this.CatchAll(fun err ->
    //         FIO.Interrupt(ResourceExhaustion (toMessage err), "Fiber interrupted due to unrecoverable error"))
