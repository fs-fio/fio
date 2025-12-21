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
open System.Threading.Tasks

type FIO<'R, 'E> with

    /// <summary>
    /// Maps a function over the result of this effect.
    /// </summary>
    /// <param name="mapper">The function to apply to the result.</param>
    member inline this.Map<'R, 'R1, 'E> (mapper: 'R -> 'R1) : FIO<'R1, 'E> =
        this.FlatMap <| fun res ->
            FIO.Succeed <| mapper res

    /// <summary>
    /// Maps a function over the error of this effect.
    /// </summary>
    /// <param name="mapper">The function to apply to the error.</param>
    member inline this.MapError<'R, 'E, 'E1> (mapper: 'E -> 'E1) : FIO<'R, 'E1> =
        this.CatchAll <| fun err ->
            FIO.Fail <| mapper err

    /// <summary>
    /// Transforms both the error and success channels of this effect simultaneously.
    /// </summary>
    /// <param name="errorMapper">The function to apply to the error.</param>
    /// <param name="successMapper">The function to apply to the result.</param>
    member inline this.Bimap<'R, 'R1, 'E, 'E1> (errorMapper: 'E -> 'E1) (successMapper: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.MapError(errorMapper)
            .Map successMapper

    /// <summary>
    /// Sequences two effects, ignoring the result of the first effect.
    /// </summary>
    /// <param name="eff">The effect to run after this effect succeeds.</param>
    member inline this.Then<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap <| fun _ -> eff

    /// <summary>
    /// Sequences two effects, ignoring the error of the first effect.
    /// </summary>
    /// <param name="eff">The effect to run after this effect fails.</param>
    member inline this.OrElse<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E1> =
        this.CatchAll <| fun _ -> eff

    /// <summary>
    /// Applies a function-producing effect to this value-producing effect.
    /// </summary>
    /// <param name="eff">The effect producing a function to apply to this effect's result.</param>
    member inline this.Apply<'R, 'R1, 'E> (eff: FIO<'R -> 'R1, 'E>) : FIO<'R1, 'E> =
        eff.FlatMap <| this.Map

    /// <summary>
    /// Applies a function-producing effect to this error-producing effect.
    /// </summary>
    /// <param name="eff">The effect producing a function to apply to this effect's error.</param>
    member inline this.ApplyError<'R, 'E, 'E1> (eff: FIO<'R, 'E -> 'E1>) : FIO<'R, 'E1> =
        eff.CatchAll <| this.MapError

    /// <summary>
    /// Executes an effect on success, discarding its result and preserving the original value.
    /// </summary>
    /// <param name="effOnSuccess">The effect to execute on the success value.</param>
    member inline this.Tap<'R, 'R1, 'E> (effOnSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.FlatMap <| fun res ->
            (effOnSuccess res).Map <| fun _ -> res

    /// <summary>
    /// Executes an effect on error, discarding its result and preserving the original error.
    /// </summary>
    /// <param name="effOnError">The effect to execute on the error value.</param>
    member inline this.TapError<'R, 'R1, 'E> (effOnError: 'E -> FIO<'R1, 'E>) : FIO<'R, 'E> =
        this.CatchAll <| fun err ->
            (effOnError err).Map <| fun _ -> err
                |> fun eff -> eff.FlatMap FIO.Fail

    /// <summary>
    /// Sequences two effects and succeeds with a tuple of their results.
    /// </summary>
    /// <param name="eff">The effect to zip with this effect.</param>
    member inline this.Zip<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        this.FlatMap <| fun res ->
            eff.Map <| fun res' ->
                res, res'

    /// <summary>
    /// Sequences two effects and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <param name="eff">The effect to zip errors with this effect.</param>
    member inline this.ZipError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        this.CatchAll <| fun err ->
            eff.MapError <| fun err' ->
                err, err'

    /// <summary>
    /// Executes two effects concurrently and succeeds with a tuple of their results.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipPar<'R, 'R1, 'E> (eff: FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
        eff.Fork().FlatMap <| fun fiber ->
            this.FlatMap <| fun res ->
                fiber.Join().Map <| fun res' ->
                    res, res'

    /// <summary>
    /// Executes two effects concurrently and fails with a tuple of their errors when both fail.
    /// </summary>
    /// <param name="eff">The effect to run concurrently with this effect.</param>
    member inline this.ZipParError<'R, 'E, 'E1> (eff: FIO<'R, 'E1>) : FIO<'R, 'E * 'E1> =
        eff.Fork().FlatMap <| fun fiber ->
            this.CatchAll <| fun err ->
                fiber.Join().MapError <| fun err' ->
                    err, err'

    /// <summary>
    /// Folds over both channels using pure functions, producing an infallible result.
    /// </summary>
    /// <param name="onError">The function to apply to the error.</param>
    /// <param name="onSuccess">The function to apply to the success value.</param>
    member inline this.Fold<'R, 'R1, 'E, 'E1> (onError: 'E -> 'R1) (onSuccess: 'R -> 'R1) : FIO<'R1, 'E1> =
        this.Map(onSuccess)
            .CatchAll(fun err -> FIO.Succeed (onError err))

    /// <summary>
    /// Folds over both channels using effectful functions with recovery semantics.
    /// </summary>
    /// <param name="onError">The function to apply to any error, producing a recovery effect.</param>
    /// <param name="onSuccess">The function to apply to the success value, producing an effect.</param>
    member inline this.FoldFIO<'R, 'R1, 'E> (onError: 'E -> FIO<'R1, 'E>) (onSuccess: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// <summary>
    /// Retries this effect up to maxRetries times with exponential backoff on failure.
    /// </summary>
    /// <param name="delayBetweenRetriesMillis">Initial delay in milliseconds before the first retry.</param>
    /// <param name="maxRetries">Maximum number of retry attempts.</param>
    /// <param name="onEachRetry">Callback executed before each retry with current retry number and maxRetries.</param>
    member inline this.Retry (delayBetweenRetriesMillis: float) (maxRetries: int) (onEachRetry: int -> int -> FIO<unit, 'E>): FIO<'R, 'E> =
        let rec loop retry (delayMs: float) =
            this.CatchAll(fun err ->
                if retry >= maxRetries then
                    FIO.Fail err
                else
                    (onEachRetry retry maxRetries)
                        .Then(FIO.AwaitTask(Task.Delay(TimeSpan.FromMilliseconds delayMs), fun _ -> err))
                        .Then(loop (retry + 1) (delayMs * 2.0)))
        loop 0 delayBetweenRetriesMillis
