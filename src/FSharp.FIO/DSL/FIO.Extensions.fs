(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides instance combinator methods for the FIO effect type, including Map, Zip, Parallel, Retry, and Timeout operations.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Extensions

open System
open System.Threading.Tasks

type FIO<'R, 'E> with

    /// <summary>
    /// Maps a function over the result of this effect.
    /// </summary>
    /// <typeparam name="R">The original result type.</typeparam>
    /// <typeparam name="R1">The mapped result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="mapper">The function to apply to the result.</param>
    /// <returns>An FIO effect that applies the function to the result, or propagates an error.</returns>
    member inline this.Map<'R, 'R1, 'E> (mapper: 'R -> 'R1) : FIO<'R1, 'E> =
        this.FlatMap <| fun res ->
            FIO.Succeed <| mapper res

    /// <summary>
    /// Maps a function over the error of this effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The original error type.</typeparam>
    /// <typeparam name="E1">The mapped error type.</typeparam>
    /// <param name="mapper">The function to apply to the error.</param>
    /// <returns>An FIO effect that applies the function to the error, or propagates the result.</returns>
    member inline this.MapError<'R, 'E, 'E1> (mapper: 'E -> 'E1) : FIO<'R, 'E1> =
        this.CatchAll <| fun err ->
            FIO.Fail <| mapper err

    /// <summary>
    /// Transforms both the error and success channels of this effect simultaneously.
    /// Applies errorMapper to transform the error type and successMapper to transform the result type.
    /// </summary>
    /// <typeparam name="R">The original result type.</typeparam>
    /// <typeparam name="R1">The transformed result type.</typeparam>
    /// <typeparam name="E">The original error type.</typeparam>
    /// <typeparam name="E1">The transformed error type.</typeparam>
    /// <param name="errorMapper">The function to apply to the error.</param>
    /// <param name="successMapper">The function to apply to the result.</param>
    /// <returns>An FIO effect with both channels transformed.</returns>
    member inline this.Bimap<'R, 'R1, 'E, 'E1> (errorMapper: 'E -> 'E1) (successMapper: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.MapError(errorMapper).Map successMapper

    /// <summary>
    /// Sequences two effects, ignoring the result of the first effect. If the first effect fails, the error is propagated immediately.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to run after this effect succeeds.</param>
    /// <returns>An FIO effect that sequences both effects, returning the result of the second effect or propagating an error.</returns>
    member inline this.Then<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap <| fun _ -> eff

    /// <summary>
    /// Sequences two effects, ignoring the error of the first effect. If the first effect succeeds, the result is propagated immediately.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to run after this effect fails.</param>
    /// <returns>An FIO effect that sequences both effects on error, returning the result or the error of the second effect.</returns>
    member inline this.OrElse<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E1> =
        this.CatchAll <| fun _ -> eff

    /// <summary>
    /// Applies a function-producing effect to this value-producing effect. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The input result type.</typeparam>
    /// <typeparam name="R1">The output result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply to this effect's result.</param>
    /// <returns>An FIO effect that applies the function to the result, or propagates an error.</returns>
    member inline this.Apply<'R, 'R1, 'E> (eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
        eff.FlatMap <| this.Map

    /// <summary>
    /// Applies a function-producing effect to this error-producing effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The input error type.</typeparam>
    /// <typeparam name="E1">The output error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply to this effect's error.</param>
    /// <returns>An FIO effect that applies the function to the error, or propagates the result.</returns>
    member inline this.ApplyError<'R, 'E, 'E1> (eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll <| this.MapError

    /// <summary>
    /// Executes an effect based on the success value of this effect, discarding the effect's result
    /// and preserving the original success value. If the tap effect fails, the failure propagates.
    /// This is useful for side effects like logging or metrics that should not alter the result.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="R1">The result type of the tap effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="effOnSuccess">The effect to execute on the success value.</param>
    /// <returns>An FIO effect that executes the tap effect and returns the original success value, or propagates an error.</returns>
    member inline this.Tap<'R, 'R1, 'E> (effOnSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap <| fun res ->
            (effOnSuccess res).FlatMap <| fun _ ->
                FIO.Succeed res

    /// <summary>
    /// Executes an effect based on the error value of this effect, discarding the effect's result
    /// and preserving the original error value. If the tap effect fails, that failure propagates.
    /// This is useful for error-handling side effects like error logging or notifications.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="R1">The result type of the tap effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="effOnError">The effect to execute on the error value.</param>
    /// <returns>An FIO effect that executes the tap effect and returns the original error, or propagates the tap effect's failure.</returns>
    member inline this.TapError<'R, 'R1, 'E> (effOnError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.CatchAll <| fun err ->
            (effOnError err).FlatMap <| fun _ ->
                FIO.Fail err

    /// <summary>
    /// Sequences two effects and succeeds with a tuple of their results when both complete. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to zip with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both results, or propagates an error.</returns>
    member inline this.Zip<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.FlatMap <| fun res ->
            eff.FlatMap <| fun res' ->
                FIO.Succeed (res, res')

    /// <summary>
    /// Sequences two effects and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to zip errors with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both errors, or propagates the result.</returns>
    member inline this.ZipError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.CatchAll <| fun err ->
            eff.CatchAll <| fun err' ->
                FIO.Fail (err, err')

    /// <summary>
    /// Executes two effects concurrently in parallel and succeeds with a tuple of their results when both complete. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both results, or propagates an error.</returns>
    member inline this.ZipPar<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().FlatMap <| fun fiber ->
            this.FlatMap <| fun res ->
                fiber.Join().FlatMap <| fun res' ->
                     FIO.Succeed (res, res')

    /// <summary>
    /// Executes two effects concurrently in parallel and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both errors, or propagates the result.</returns>
    member inline this.ZipParError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        eff.Fork().FlatMap <| fun fiber ->
            this.CatchAll <| fun err ->
                fiber.Join().CatchAll <| fun err' ->
                    FIO.Fail (err, err')

    /// <summary>
    /// Folds over both the error and success channels using pure functions, producing an infallible result.
    /// Both error and success paths are transformed to the same result type, guaranteeing the effect never fails at runtime.
    /// The error type parameter 'E1 is phantom and unconstrained - the runtime can instantiate it as needed, but it will never contain an error value.
    /// This is useful for handling both cases and normalizing them to a single result type.
    /// </summary>
    /// <typeparam name="R">The original result type.</typeparam>
    /// <typeparam name="R1">The folded result type.</typeparam>
    /// <typeparam name="E">The original error type.</typeparam>
    /// <typeparam name="E1">Phantom error type parameter (never used at runtime, as this effect cannot fail).</typeparam>
    /// <param name="onError">The function to apply to the error, converting it to the result type.</param>
    /// <param name="onSuccess">The function to apply to the success value, converting it to the result type.</param>
    /// <returns>An FIO effect that handles both channels and produces an infallible result.</returns>
    member inline this.Fold<'R, 'R1, 'E, 'E1> (onError: 'E -> 'R1) (onSuccess: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.FlatMap(fun res -> FIO.Succeed (onSuccess res))
            .CatchAll(fun err -> FIO.Succeed (onError err))

    /// <summary>
    /// Folds over both the error and success channels using effectful functions with recovery semantics.
    /// This method provides a way to handle both success and error cases with effects, where the error handler
    /// serves as a fallback for ANY failure (both from the original effect and from the success handler).
    ///
    /// Execution semantics:
    /// - If the original effect succeeds: Runs onSuccess with the result
    ///   - If onSuccess succeeds: Returns that result
    ///   - If onSuccess FAILS: The error handler catches it and runs onError with that error
    /// - If the original effect fails: Runs onError with the error
    ///   - If onError succeeds: Returns that result
    ///   - If onError fails: That error propagates
    ///
    /// Note: This differs from ZIO's foldZIO where errors from the success handler are NOT caught by the error handler.
    /// FIO's FoldFIO provides "recovery" semantics where onError serves as a universal error handler.
    /// </summary>
    /// <typeparam name="R">The original result type.</typeparam>
    /// <typeparam name="R1">The folded result type.</typeparam>
    /// <typeparam name="E">The error type (remains unchanged).</typeparam>
    /// <param name="onError">The function to apply to any error (from original effect OR from success handler), producing a recovery effect.</param>
    /// <param name="onSuccess">The function to apply to the success value, producing an effect.</param>
    /// <returns>An FIO effect that handles both channels with effectful transformations and recovery semantics.</returns>
    member inline this.FoldFIO<'R, 'R1, 'E> (onError: 'E -> FIO<'R1, 'E>) (onSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// <summary>
    /// Retries this effect up to maxRetries times with exponential backoff on failure.
    /// Executes onEachRetry callback before each retry attempt (not before the initial attempt).
    /// The delay doubles after each failed attempt, starting from the initial delay.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="delayBetweenRetriesMillis">Initial delay in milliseconds before the first retry.</param>
    /// <param name="maxRetries">Maximum number of retry attempts.</param>
    /// <param name="onEachRetry">Callback executed before each retry with current retry number (0-indexed) and maxRetries.</param>
    /// <returns>An FIO effect that retries on failure with exponential backoff, or returns the final error if all retries are exhausted.</returns>
    member inline this.Retry (delayBetweenRetriesMillis: float) (maxRetries: int) (onEachRetry: int -> int -> FIO<unit, 'E>): FIO<'R, 'E> =
        let rec loop retry (delayBetweenRetriesMillis: float) =
            this.CatchAll <| fun err ->
                if retry >= maxRetries then
                    FIO.Fail err
                else
                    let nextDelay = delayBetweenRetriesMillis * 2.0
                    let delayEff = FIO.AwaitTask(Task.Delay(TimeSpan.FromMilliseconds delayBetweenRetriesMillis), fun _ -> err)
                    (onEachRetry retry maxRetries)
                        .Then(delayEff
                            .Then (loop (retry + 1) nextDelay))
        loop 0 delayBetweenRetriesMillis

    /// <summary>
    /// Applies a timeout to this effect, failing with the specified error if the duration is exceeded.
    /// Uses Task.WhenAny to race the effect against a timeout timer - whichever completes first wins.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="durationMillis">Maximum duration in milliseconds before timing out.</param>
    /// <param name="onTimeout">Function that produces the error if timeout occurs.</param>
    /// <returns>An FIO effect that returns the result if completed within duration, or fails with the timeout error.</returns>
    member inline this.Timeout (durationMillis: float) (onTimeout: unit -> 'E) : FIO<'R, 'E> =
        this.Fork().FlatMap <| fun originalFiber ->
            // Race the original effect against a timeout using Task.WhenAny
            FIO.AwaitTask(
                task {
                   let originalTask = originalFiber.Task()
                   let timeoutTask = task {
                       do! Task.Delay(TimeSpan.FromMilliseconds durationMillis)
                       return Error (onTimeout ())
                   }
                   let! completedTask = Task.WhenAny(originalTask, timeoutTask)
                   let! result = completedTask
                   return box result
               },
                fun _ -> onTimeout()
            ).FlatMap(fun (result: obj) ->
                match result :?> Result<'R, 'E> with
                | Ok res -> FIO.Succeed res
                | Error err -> FIO.Fail err)
