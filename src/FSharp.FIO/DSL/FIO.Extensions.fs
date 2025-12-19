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
    /// <param name="cont">The function to apply to the result.</param>
    /// <returns>An FIO effect that applies the function to the result, or propagates an error.</returns>
    member inline this.Map<'R, 'R1, 'E> (cont: 'R -> 'R1) : FIO<'R1, 'E> =
        this.Bind <| fun res ->
            FIO.Succeed <| cont res

    /// <summary>
    /// Maps a function over the error of this effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The original error type.</typeparam>
    /// <typeparam name="E1">The mapped error type.</typeparam>
    /// <param name="cont">The function to apply to the error.</param>
    /// <returns>An FIO effect that applies the function to the error, or propagates the result.</returns>
    member inline this.MapError<'R, 'E, 'E1> (cont: 'E -> 'E1) : FIO<'R, 'E1> =
        this.BindError <| fun err ->
            FIO.Fail <| cont err

    /// <summary>
    /// Sequences two effects, ignoring the result of the first effect. If the first effect fails, the error is propagated immediately.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to run after this effect succeeds.</param>
    /// <returns>An FIO effect that sequences both effects, returning the result of the second effect or propagating an error.</returns>
    member inline this.Then<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.Bind <| fun _ -> eff

    /// <summary>
    /// Sequences two effects, ignoring the error of the first effect. If the first effect succeeds, the result is propagated immediately.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to run after this effect fails.</param>
    /// <returns>An FIO effect that sequences both effects on error, returning the result or the error of the second effect.</returns>
    member inline this.OrElse<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E1> =
        this.BindError <| fun _ -> eff

    /// <summary>
    /// Applies a function-producing effect to this value-producing effect. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The input result type.</typeparam>
    /// <typeparam name="R1">The output result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply to this effect's result.</param>
    /// <returns>An FIO effect that applies the function to the result, or propagates an error.</returns>
    member inline this.Apply<'R, 'R1, 'E> (eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
        eff.Bind <| this.Map

    /// <summary>
    /// Applies a function-producing effect to this error-producing effect.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The input error type.</typeparam>
    /// <typeparam name="E1">The output error type.</typeparam>
    /// <param name="eff">The effect producing a function to apply to this effect's error.</param>
    /// <returns>An FIO effect that applies the function to the error, or propagates the result.</returns>
    member inline this.ApplyError<'R, 'E, 'E1> (eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
        eff.BindError <| this.MapError

    /// <summary>
    /// Sequences two effects and succeeds with a tuple of their results when both complete. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to zip with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both results, or propagates an error.</returns>
    member inline this.Zip<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.Bind <| fun res ->
            eff.Bind <| fun res' ->
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
        this.BindError <| fun err ->
            eff.BindError <| fun err' ->
                FIO.Fail (err, err')

    /// <summary>
    /// Executes two effects concurrently and succeeds with a tuple of their results when both complete. Errors are propagated immediately if any effect fails.
    /// </summary>
    /// <typeparam name="R">The result type of the first effect.</typeparam>
    /// <typeparam name="R1">The result type of the second effect.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both results, or propagates an error.</returns>
    member inline this.Parallel<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().Bind <| fun fiber ->
            this.Bind <| fun res ->
                fiber.Await().Bind <| fun res' ->
                     FIO.Succeed (res, res')

    /// <summary>
    /// Executes two effects concurrently and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type of the first effect.</typeparam>
    /// <typeparam name="E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    /// <returns>An FIO effect that returns a tuple of both errors, or propagates the result.</returns>
    member inline this.ParallelError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        eff.Fork().Bind <| fun fiber ->
            this.BindError <| fun err ->
                fiber.Await().BindError <| fun err' ->
                    FIO.Fail (err, err')
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
            this.BindError <| fun err ->
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
        this.Fork().Bind <| fun originalFiber ->
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
            ).Bind(fun (result: obj) ->
                match result :?> Result<'R, 'E> with
                | Ok res -> FIO.Succeed res
                | Error err -> FIO.Fail err)
