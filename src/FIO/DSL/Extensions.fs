/// <summary>Provides instance combinator methods for the FIO effect type.</summary>
[<AutoOpen>]
module FIO.DSL.Extensions

open System
open System.Diagnostics

type FIO<'A, 'E> with

    /// <summary>Transforms the success value of this effect with a pure function.</summary>
    /// <typeparam name="'A1">The success result type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's success value to the new success value.</param>
    /// <returns>An effect that completes with <paramref name="mapper"/> applied to the original success value.</returns>
    member inline this.Map<'A1> (mapper: 'A -> 'A1) : FIO<'A1, 'E> =
        this.FlatMap <| fun value -> FIO.succeed <| mapper value

    /// <summary>Transforms the typed error of this effect with a pure function.</summary>
    /// <typeparam name="'E1">The error type produced by the mapper.</typeparam>
    /// <param name="mapper">A pure function from this effect's typed error to the new typed error.</param>
    /// <returns>An effect that propagates the original success value, or fails with <paramref name="mapper"/> applied to the original error.</returns>
    member inline this.MapError<'E1> (mapper: 'E -> 'E1) : FIO<'A, 'E1> =
        this.CatchAll <| fun error -> FIO.fail <| mapper error

    /// <summary>Transforms both the success and error channels of this effect with pure functions.</summary>
    /// <typeparam name="'A1">The success result type produced by <paramref name="successMapper"/>.</typeparam>
    /// <typeparam name="'E1">The error type produced by <paramref name="errorMapper"/>.</typeparam>
    /// <param name="successMapper">A pure function from the original success value to the new success value.</param>
    /// <param name="errorMapper">A pure function from the original typed error to the new typed error.</param>
    /// <returns>An effect with both channels transformed by their respective mappers.</returns>
    member inline this.MapBoth<'A1, 'E1> (successMapper: 'A -> 'A1) (errorMapper: 'E -> 'E1) : FIO<'A1, 'E1> =
        this.Map(successMapper).MapError errorMapper

    /// <summary>Transforms the success value of this effect with a function that may throw, routing any exception into the typed error channel.</summary>
    /// <typeparam name="'A1">The success result type produced by the mapper.</typeparam>
    /// <param name="mapper">A function from this effect's success value to the new success value; may throw.</param>
    /// <param name="onError">A function that maps an exception thrown by <paramref name="mapper"/> to the typed error.</param>
    /// <returns>An effect that completes with <paramref name="mapper"/> applied to the original success value, or fails via <paramref name="onError"/> if <paramref name="mapper"/> throws.</returns>
    /// <remarks>Useful for mapping through throwing APIs such as parsing or deserialisation.</remarks>
    member inline this.MapAttempt<'A1> (mapper: 'A -> 'A1) (onError: exn -> 'E) : FIO<'A1, 'E> =
        this.FlatMap <| fun value -> FIO.attempt (fun () -> mapper value) onError

    /// <summary>Transforms this effect's success value into unit, discarding the original result.</summary>
    /// <returns>An effect that completes successfully with unit.</returns>
    member inline this.Unit () : FIO<unit, 'E> =
        this.Map <| fun _ -> ()

    /// <summary>Transforms this effect's success value into a constant.</summary>
    /// <typeparam name="'A1">The result type of the constant value.</typeparam>
    /// <param name="value">The constant value to produce on success.</param>
    /// <returns>An effect that completes with <paramref name="value"/> on success, replacing the original result.</returns>
    member inline this.As<'A1> (value: 'A1) : FIO<'A1, 'E> =
        this.Map <| fun _ -> value

    /// <summary>Wraps this effect's success value in <c>Choice1Of2</c>.</summary>
    /// <typeparam name="'B">The phantom right-branch type of the resulting choice; never inhabited.</typeparam>
    /// <returns>An effect that completes with <c>Choice1Of2</c> applied to the original success value.</returns>
    member inline this.AsLeft<'B> () : FIO<Choice<'A, 'B>, 'E> =
        this.Map Choice1Of2

    /// <summary>Wraps this effect's success value in <c>Choice2Of2</c>.</summary>
    /// <typeparam name="'M">The phantom left-branch type of the resulting choice; never inhabited.</typeparam>
    /// <returns>An effect that completes with <c>Choice2Of2</c> applied to the original success value.</returns>
    member inline this.AsRight<'M> () : FIO<Choice<'M, 'A>, 'E> =
        this.Map Choice2Of2

    /// <summary>Wraps this effect's success value in <c>Some</c>.</summary>
    /// <returns>An effect that completes with <c>Some</c> applied to the original success value.</returns>
    member inline this.AsSome () : FIO<'A option, 'E> =
        this.Map Some

    /// <summary>Wraps this effect's typed error in <c>Choice1Of2</c>.</summary>
    /// <typeparam name="'B">The phantom right-branch type of the resulting choice; never inhabited.</typeparam>
    /// <returns>An effect that propagates the original success value, or fails with <c>Choice1Of2</c> applied to the original error.</returns>
    member inline this.AsLeftError<'B> () : FIO<'A, Choice<'E, 'B>> =
        this.MapError Choice1Of2

    /// <summary>Wraps this effect's typed error in <c>Choice2Of2</c>.</summary>
    /// <typeparam name="'M">The phantom left-branch type of the resulting choice; never inhabited.</typeparam>
    /// <returns>An effect that propagates the original success value, or fails with <c>Choice2Of2</c> applied to the original error.</returns>
    member inline this.AsRightError<'M> () : FIO<'A, Choice<'M, 'E>> =
        this.MapError Choice2Of2

    /// <summary>Wraps this effect's typed error in <c>Some</c>.</summary>
    /// <returns>An effect that propagates the original success value, or fails with <c>Some</c> applied to the original error.</returns>
    member inline this.AsSomeError<'A, 'E> () : FIO<'A, 'E option> =
        this.MapError Some

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Result</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because the original error is moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Ok</c> on success or <c>Error</c> on failure.</returns>
    member inline this.Result<'E1> () : FIO<Result<'A, 'E>, 'E1> =
        this.Map(Ok).CatchAll <| fun error -> FIO.succeed (Error error)

    /// <summary>Transforms this effect into an infallible effect whose success channel carries an <c>Option</c>, discarding the error.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are mapped to <c>None</c>.</typeparam>
    /// <returns>An effect that completes with <c>Some</c> on success or <c>None</c> on failure.</returns>
    member inline this.Option<'E1> () : FIO<'A option, 'E1> =
        this.Map(Some).CatchAll <| fun _ -> FIO.succeed None

    /// <summary>Transforms this effect into an infallible effect whose success channel carries a <c>Choice</c>.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are moved into the success channel.</typeparam>
    /// <returns>An effect that completes with <c>Choice1Of2</c> on success or <c>Choice2Of2</c> on failure.</returns>
    member inline this.Choice<'E1> () : FIO<Choice<'A, 'E>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun error ->
            FIO.succeed (Choice2Of2 error))

    /// <summary>Builds an effect that swaps the success and error channels of this effect.</summary>
    /// <returns>An effect whose success carries the original typed error, and whose error carries the original success value.</returns>
    /// <remarks><c>Flip().Flip()</c> is equivalent to the original effect.</remarks>
    member inline this.Flip () : FIO<'E, 'A> =
        this.Result().FlatMap(fun result ->
            match result with
            | Ok value -> FIO.fail value
            | Error error -> FIO.succeed error)

    /// <summary>Builds an infallible effect that discards both the success and error channels.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because both branches map to unit.</typeparam>
    /// <returns>An effect that completes with unit regardless of whether this effect succeeded or failed.</returns>
    /// <remarks>Unlike <c>Unit</c>, which preserves the error channel, <c>Ignore</c> swallows failures and is suitable for fire-and-forget effects whose outcome is intentionally disregarded.</remarks>
    member inline this.Ignore<'E1> () : FIO<unit, 'E1> =
        this.Map(fun _ -> ()).CatchAll(fun _ ->
            FIO.succeed ())

    /// <summary>Builds an infallible effect that reports whether this effect succeeded.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because both branches succeed.</typeparam>
    /// <returns>An effect that completes with <c>true</c> when this effect succeeded, or <c>false</c> when it failed.</returns>
    member inline this.IsSuccess<'E1> () : FIO<bool, 'E1> =
        this.Map(fun _ -> true).CatchAll(fun _ ->
            FIO.succeed false)

    /// <summary>Builds an infallible effect that reports whether this effect failed.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because both branches succeed.</typeparam>
    /// <returns>An effect that completes with <c>true</c> when this effect failed, or <c>false</c> when it succeeded.</returns>
    member inline this.IsFailure<'E1> () : FIO<bool, 'E1> =
        this.Map(fun _ -> false).CatchAll(fun _ ->
            FIO.succeed true)

    /// <summary>Combines this effect with a boolean guard, running it only when the guard is true.</summary>
    /// <param name="cond">The condition controlling whether this effect runs.</param>
    /// <returns>An effect that runs this effect (discarding its result) when <paramref name="cond"/> is true, or completes with unit otherwise.</returns>
    member inline this.When (cond: bool) : FIO<unit, 'E> =
        if cond then this.Unit() else FIO.unit ()

    /// <summary>Combines this effect with a boolean guard, running it only when the guard is false.</summary>
    /// <param name="cond">The condition controlling whether this effect is suppressed.</param>
    /// <returns>An effect that runs this effect (discarding its result) when <paramref name="cond"/> is false, or completes with unit otherwise.</returns>
    member inline this.Unless (cond: bool) : FIO<unit, 'E> =
        this.When(not cond)

    /// <summary>Combines this effect with a side-effecting function that observes the success value while preserving it.</summary>
    /// <typeparam name="'A1">The result type produced by the side-effecting function; discarded.</typeparam>
    /// <param name="effOnSuccess">A function from the success value to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs <paramref name="effOnSuccess"/> on success and then completes with the original success value.</returns>
    member inline this.Tap<'A1> (effOnSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.FlatMap(fun value -> effOnSuccess(value).Map(fun _ -> value))

    /// <summary>Combines this effect with a side-effecting function that observes the typed error while preserving it.</summary>
    /// <typeparam name="'A1">The result type produced by the side-effecting function; discarded.</typeparam>
    /// <param name="effOnError">A function from the typed error to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs <paramref name="effOnError"/> on failure and then re-fails with the original typed error.</returns>
    member inline this.TapError<'A1> (effOnError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.CatchAll(fun error -> effOnError(error).FlatMap(fun _ -> FIO.fail error))

    /// <summary>Combines this effect with side-effecting observers on both channels while preserving the outcome.</summary>
    /// <typeparam name="'A1">The result type produced by the error observer; discarded.</typeparam>
    /// <typeparam name="'A2">The result type produced by the success observer; discarded.</typeparam>
    /// <param name="onSuccess">A function from the success value to a side-effecting effect whose result is discarded.</param>
    /// <param name="onError">A function from the typed error to a side-effecting effect whose result is discarded.</param>
    /// <returns>An effect that runs the matching observer and then completes with the original outcome unchanged.</returns>
    member inline this.TapBoth<'A1, 'A2> (onSuccess: 'A -> FIO<'A2, 'E>) (onError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.Tap(onSuccess).TapError onError

    /// <summary>Combines this effect with a best-effort console print of its success value for debugging.</summary>
    /// <param name="message">The prefix to print before the success value; defaults to <c>"Debug"</c>.</param>
    /// <returns>An effect that prints the success value and completes with it unchanged; printing failures are swallowed.</returns>
    member inline this.Debug (?message: string) : FIO<'A, 'E> =
        let message = defaultArg message "Debug"
        this.Tap(fun value ->
            (FIO.attempt (fun () -> printfn "%s: %A" message value) id)
                .CatchAll(fun ex ->
                    (FIO.attempt (fun () -> eprintfn "Debug print failed. Message: %s, Exception: %s" message ex.Message) id)
                        .Ignore()))

    /// <summary>Combines this effect with a best-effort console print of its typed error for debugging.</summary>
    /// <param name="message">The prefix to print before the error value; defaults to <c>"Debug Error"</c>.</param>
    /// <returns>An effect that prints the typed error and re-fails with it unchanged; printing failures are swallowed.</returns>
    member inline this.DebugError (?message: string) : FIO<'A, 'E> =
        let message = defaultArg message "Debug Error"
        this.TapError(fun error ->
            (FIO.attempt(fun () -> printfn "%s: %A" message error) id)
                .CatchAll(fun ex ->
                    (FIO.attempt(fun () -> eprintfn "Debug Error print failed. Message: %s, Exception: %s" message ex.Message) id)
                        .Ignore()))

    /// <summary>Combines this effect with a fallback effect that runs when this effect fails.</summary>
    /// <typeparam name="'E1">The error type of the fallback effect.</typeparam>
    /// <param name="eff">The fallback effect to evaluate on failure.</param>
    /// <returns>An effect that completes with this effect's success value, or with the fallback effect's outcome on failure.</returns>
    member inline this.OrElse<'E1> (eff: FIO<'A, 'E1>) : FIO<'A, 'E1> =
        this.CatchAll(fun _ -> eff)

    /// <summary>Combines this effect with a constant success fallback that replaces any typed error.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are swallowed.</typeparam>
    /// <param name="value">The success value to produce when this effect fails.</param>
    /// <returns>An effect that completes with this effect's success value, or with <paramref name="value"/> on failure.</returns>
    member inline this.OrElseSucceed<'E1> (result: 'A) : FIO<'A, 'E1> =
        this.CatchAll(fun _ -> FIO.succeed result)

    /// <summary>Combines this effect with a constant typed error that replaces any failure of this effect.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect.</typeparam>
    /// <param name="error">The typed error to fail with when this effect fails.</param>
    /// <returns>An effect that completes with this effect's success value, or fails with <paramref name="error"/> on failure.</returns>
    member inline this.OrElseFail<'E1> (error: 'E1) : FIO<'A, 'E1> =
        this.CatchAll(fun _ -> FIO.fail error)

    /// <summary>Combines this effect with a fallback effect, distinguishing which branch produced the result via a <c>Choice</c>.</summary>
    /// <typeparam name="'A1">The success result type of the fallback effect.</typeparam>
    /// <typeparam name="'E1">The error type of the fallback effect and resulting effect.</typeparam>
    /// <param name="eff">The fallback effect to evaluate on failure.</param>
    /// <returns>An effect that completes with <c>Choice1Of2</c> when this effect succeeds, or with <c>Choice2Of2</c> when the fallback succeeds; fails with the fallback's error when both fail.</returns>
    member inline this.OrElseEither<'A1, 'E1> (eff: FIO<'A1, 'E1>) : FIO<Choice<'A, 'A1>, 'E1> =
        this.Map(Choice1Of2).CatchAll(fun _ -> eff.Map Choice2Of2)

    /// <summary>Combines this effect with a partial recovery function, recovering matched errors and re-raising the rest.</summary>
    /// <param name="func">A function returning <c>Some</c> recovery effect for handled errors, or <c>None</c> to propagate the original error.</param>
    /// <returns>An effect that recovers from matched failures and re-fails with unmatched errors.</returns>
    member inline this.CatchSome (func: 'E -> FIO<'A, 'E> option) : FIO<'A, 'E> =
        this.CatchAll(fun error ->
            match func error with
            | Some recovery -> recovery
            | None -> FIO.fail error)

    /// <summary>Transforms every typed error of this effect into a fiber interruption, producing an infallible effect.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures interrupt the fiber.</typeparam>
    /// <param name="toMessage">A function from the typed error to the interruption message.</param>
    /// <returns>An infallible effect that completes with the original success value, or interrupts the fiber on failure.</returns>
    member inline this.OrInterrupt<'E1> (toMessage: 'E -> string) : FIO<'A, 'E1> =
        this.CatchAll(fun error ->
            FIO.interrupt(ResourceExhaustion(toMessage error)) "Fiber interrupted due to unrecoverable error")

    /// <summary>Combines this effect with a predicate over its success value, failing with a supplied error when the predicate rejects it.</summary>
    /// <param name="predicate">A predicate evaluated against this effect's success value; the effect passes through when it returns <c>true</c>.</param>
    /// <param name="error">The typed error to fail with when <paramref name="predicate"/> returns <c>false</c>.</param>
    /// <returns>An effect that completes with this effect's success value when <paramref name="predicate"/> accepts it, or fails with <paramref name="error"/> otherwise.</returns>
    member inline this.FilterOrFail (predicate: 'A -> bool) (error: 'E) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            if predicate value then FIO.succeed value else FIO.fail error)

    /// <summary>Combines this effect with a predicate over its success value, evaluating a fallback effect when the predicate rejects it.</summary>
    /// <param name="predicate">A predicate evaluated against this effect's success value; the effect passes through when it returns <c>true</c>.</param>
    /// <param name="fallback">The fallback effect to evaluate when <paramref name="predicate"/> returns <c>false</c>; its outcome is propagated.</param>
    /// <returns>An effect that completes with this effect's success value when <paramref name="predicate"/> accepts it, or with the outcome of <paramref name="fallback"/> otherwise.</returns>
    member inline this.FilterOrElse (predicate: 'A -> bool) (fallback: FIO<'A, 'E>) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            if predicate value then FIO.succeed value else fallback)

    /// <summary>Combines this effect with a predicate over its success value, deriving a fallback effect from the rejected value.</summary>
    /// <param name="predicate">A predicate evaluated against this effect's success value; the effect passes through when it returns <c>true</c>.</param>
    /// <param name="fallback">A function from the rejected success value to the fallback effect to evaluate; its outcome is propagated.</param>
    /// <returns>An effect that completes with this effect's success value when <paramref name="predicate"/> accepts it, or with the outcome of <paramref name="fallback"/> applied to the rejected value otherwise.</returns>
    member inline this.FilterOrElseWith (predicate: 'A -> bool) (fallback: 'A -> FIO<'A, 'E>) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            if predicate value then FIO.succeed value else fallback value)

    /// <summary>Combines this effect with a predicate over its success value, interrupting the fiber when the predicate rejects it.</summary>
    /// <param name="predicate">A predicate evaluated against this effect's success value; the effect passes through when it returns <c>true</c>.</param>
    /// <param name="message">The human-readable description attached to the interruption.</param>
    /// <returns>An effect that completes with this effect's success value when <paramref name="predicate"/> accepts it, or interrupts the fiber with <c>ResourceExhaustion</c> otherwise.</returns>
    /// <remarks>Mirrors the convention used by <c>OrInterrupt</c>: filter failures surface as fiber interruptions rather than typed errors.</remarks>
    member inline this.FilterOrInterrupt (predicate: 'A -> bool) (message: string) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            if predicate value then FIO.succeed value
            else FIO.interrupt (ResourceExhaustion message) message)

    /// <summary>Combines this effect with a partial function over its success value, failing with the matched error and passing through otherwise.</summary>
    /// <param name="func">A partial function from the success value to a typed error; <c>Some</c> rejects with that error, <c>None</c> passes through.</param>
    /// <returns>An effect that fails with the rejection error when <paramref name="func"/> matches, or completes with this effect's success value otherwise.</returns>
    /// <remarks>Dual to <c>FilterOrFail</c>: <c>Reject</c> fails on match, <c>FilterOrFail</c> fails on predicate-false.</remarks>
    member inline this.Reject (func: 'A -> 'E option) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            match func value with
            | Some error -> FIO.fail error
            | None -> FIO.succeed value)

    /// <summary>Combines this effect with a partial function over its success value, failing with the result of an effectful computation when the partial function matches.</summary>
    /// <param name="func">A partial function from the success value to an effect producing the typed error; <c>Some</c> rejects by running the effect, <c>None</c> passes through.</param>
    /// <returns>An effect that fails with the rejection error when <paramref name="func"/> matches, or completes with this effect's success value otherwise.</returns>
    /// <remarks>The error-computing effect has the same success and error type; if it fails during computation, that failure is the rejection error.</remarks>
    member inline this.RejectFIO (func: 'A -> FIO<'E, 'E> option) : FIO<'A, 'E> =
        this.FlatMap(fun value ->
            match func value with
            | Some errEff -> errEff.FlatMap FIO.fail
            | None -> FIO.succeed value)

    /// <summary>Combines this effect with a partial function over its success value, extracting matched values and failing with a supplied error otherwise.</summary>
    /// <typeparam name="'A1">The extracted success result type produced by the partial function.</typeparam>
    /// <param name="error">The typed error to fail with when <paramref name="pf"/> does not match.</param>
    /// <param name="pf">A partial function from the success value to the extracted result; <c>Some</c> keeps the extracted value, <c>None</c> rejects.</param>
    /// <returns>An effect that completes with the extracted value when <paramref name="pf"/> matches, or fails with <paramref name="error"/> otherwise.</returns>
    /// <remarks>Combines filter and extract in one step; equivalent to <c>this.Map(pf) |> FIO.someOrFail error</c> with a single pass.</remarks>
    member inline this.Collect<'A1> (error: 'E) (func: 'A -> 'A1 option) : FIO<'A1, 'E> =
        this.FlatMap(fun value ->
            match func value with
            | Some out -> FIO.succeed out
            | None -> FIO.fail error)

    /// <summary>Combines this effect with a partial function over its success value producing an effect, extracting matched values and failing with a supplied error otherwise.</summary>
    /// <typeparam name="'A1">The extracted success result type produced by the partial function's effect.</typeparam>
    /// <param name="error">The typed error to fail with when <paramref name="pf"/> does not match.</param>
    /// <param name="func">A partial function from the success value to an effect producing the extracted result; <c>Some</c> runs the effect, <c>None</c> rejects.</param>
    /// <returns>An effect that propagates the matched effect's outcome when <paramref name="func"/> matches, or fails with <paramref name="error"/> otherwise.</returns>
    member inline this.CollectFIO<'A1> (error: 'E) (func: 'A -> FIO<'A1, 'E> option) : FIO<'A1, 'E> =
        this.FlatMap(fun value ->
            match func value with
            | Some eff -> eff
            | None -> FIO.fail error)

    /// <summary>Combines a function-producing effect with this effect, applying the produced function to the success value.</summary>
    /// <typeparam name="'A1">The success result type produced by the function.</typeparam>
    /// <param name="eff">An effect whose success value is a function applied to this effect's success value.</param>
    /// <returns>An effect that completes with the produced function applied to this effect's success value.</returns>
    member inline this.Apply<'A1> (eff: FIO<'A -> 'A1, 'E>) : FIO<'A1, 'E> =
        eff.FlatMap this.Map

    /// <summary>Combines a function-producing effect with this effect, applying the produced function to the typed error.</summary>
    /// <typeparam name="'E1">The error type produced by the function.</typeparam>
    /// <param name="eff">An effect whose error channel produces a function applied to this effect's typed error.</param>
    /// <returns>An effect that propagates this effect's success value, or fails with the produced function applied to the original error.</returns>
    member inline this.ApplyError<'E1> (eff: FIO<'A, 'E -> 'E1>) : FIO<'A, 'E1> =
        eff.CatchAll this.MapError

    /// <summary>Combines this effect with a second effect sequentially and returns both results as a tuple.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <param name="eff">The effect to evaluate after this one.</param>
    /// <returns>An effect that runs both in order and completes with a pair of their results.</returns>
    member inline this.Zip<'A1> (eff: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        this.FlatMap(fun value -> eff.Map(fun value' -> value, value'))

    /// <summary>Combines this effect with a second effect, failing with a tuple of both errors when both fail.</summary>
    /// <typeparam name="'E1">The error type of the second effect.</typeparam>
    /// <param name="eff">The effect to evaluate when this effect fails.</param>
    /// <returns>An effect that completes with this effect's success value when it succeeds, or fails with a pair of both errors when both fail.</returns>
    member inline this.ZipError<'E1> (eff: FIO<'A, 'E1>) : FIO<'A, 'E * 'E1> =
        this.CatchAll(fun error -> eff.MapError(fun error' -> error, error'))

    /// <summary>Combines this effect with a second effect sequentially and returns the second result.</summary>
    /// <typeparam name="'A1">The success result type of the second effect; propagated.</typeparam>
    /// <param name="eff">The effect to evaluate after this one; its result is propagated.</param>
    /// <returns>An effect that runs both in order and completes with the second result.</returns>
    member inline this.ZipRight<'A1> (eff: FIO<'A1, 'E>) : FIO<'A1, 'E> =
        this.FlatMap(fun _ -> eff)

    /// <summary>Combines this effect with a second effect sequentially and returns the first result.</summary>
    /// <typeparam name="'A1">The success result type of the second effect; discarded.</typeparam>
    /// <param name="eff">The effect to evaluate after this one; its result is discarded.</param>
    /// <returns>An effect that runs both in order and completes with this effect's result.</returns>
    member inline this.ZipLeft<'A1> (eff: FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.FlatMap(fun value -> eff.Map(fun _ -> value))

    /// <summary>Combines this effect with a second effect concurrently and returns both results as a tuple.</summary>
    /// <typeparam name="'A1">The success result type of the second effect.</typeparam>
    /// <param name="eff">The effect to fork alongside this one.</param>
    /// <returns>An effect that runs both concurrently and completes with a pair of their results.</returns>
    /// <remarks>Both effects run on separate fibers in fiber runtimes.</remarks>
    member inline this.ZipPar<'A1> (eff: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        eff.Fork().FlatMap(fun fiber -> this.FlatMap(fun value -> fiber.Join().Map(fun value' -> value, value')))

    /// <summary>Combines this effect with a second effect concurrently, failing with a tuple of both errors when both fail.</summary>
    /// <param name="eff">The effect to fork alongside this one.</param>
    /// <returns>An effect that completes with this effect's success value when it succeeds, or fails with a tuple of both errors when both fail.</returns>
    member inline this.ZipParError (eff: FIO<'A, 'E>) : FIO<'A, 'E * 'E> =
        eff.Fork().FlatMap(fun fiber ->
            this.FlatMap(fun res1 -> fiber.Join().Map(fun _ -> res1))
                .CatchAll(fun err1 -> 
                    fiber.Join().FlatMap(fun res2 -> FIO.succeed res2)
                        .CatchAll(fun err2 -> FIO.fail (err1, err2))))

    /// <summary>Combines this effect with a second effect concurrently and returns the second result.</summary>
    /// <typeparam name="'A1">The success result type of the second effect; propagated.</typeparam>
    /// <param name="eff">The effect to fork alongside this one; its result is propagated.</param>
    /// <returns>An effect that runs both concurrently and completes with the second result.</returns>
    member inline this.ZipParRight<'A1> (eff: FIO<'A1, 'E>) : FIO<'A1, 'E> =
        this.ZipPar(eff).Map(fun (_, value) -> value)

    /// <summary>Combines this effect with a second effect concurrently and returns the first result.</summary>
    /// <typeparam name="'A1">The success result type of the second effect; discarded.</typeparam>
    /// <param name="eff">The effect to fork alongside this one; its result is discarded.</param>
    /// <returns>An effect that runs both concurrently and completes with this effect's result.</returns>
    member inline this.ZipParLeft<'A1> (eff: FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.ZipPar(eff).Map(fun (value, _) -> value)

    /// <summary>Combines this effect's outcome with two pure folds, producing an infallible effect.</summary>
    /// <typeparam name="'A1">The result type produced by both folds.</typeparam>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because both branches succeed.</typeparam>
    /// <param name="onError">A pure function from the typed error to the result value.</param>
    /// <param name="onSuccess">A pure function from the success value to the result value.</param>
    /// <returns>An infallible effect that completes with the appropriate fold applied to the original outcome.</returns>
    member inline this.Fold<'A1, 'E1> (onError: 'E -> 'A1) (onSuccess: 'A -> 'A1) : FIO<'A1, 'E1> =
        this.Map(onSuccess).CatchAll(fun error -> FIO.succeed(onError error))

    /// <summary>Combines this effect's outcome with two effectful folds, producing the effect from whichever branch matches.</summary>
    /// <typeparam name="'A1">The success result type produced by both folds.</typeparam>
    /// <param name="onError">A function from the typed error to the next effect.</param>
    /// <param name="onSuccess">A function from the success value to the next effect.</param>
    /// <returns>An effect that runs the matching fold and propagates its outcome.</returns>
    member inline this.FoldFIO<'A1> (onError: 'E -> FIO<'A1, 'E>) (onSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        this.FlatMap(onSuccess).CatchAll onError

    /// <summary>Combines this effect with fire-and-forget handlers on both channels, producing a <c>unit</c>-effect that swallows the original outcome.</summary>
    /// <param name="onError">A function from the typed error to the handler effect; its result is discarded.</param>
    /// <param name="onSuccess">A function from the success value to the handler effect; its result is discarded.</param>
    /// <returns>An effect that runs the matching handler and completes with unit; failures of either handler propagate as the typed error.</returns>
    /// <remarks>The two handlers are isolated — a failure in <paramref name="onSuccess"/> propagates directly and does not flow into <paramref name="onError"/>. Use when the original outcome should be observed but not propagated, e.g. logging the result of a forked task without changing its shape.</remarks>
    member inline this.OnDone (onError: 'E -> FIO<unit, 'E>) (onSuccess: 'A -> FIO<unit, 'E>) : FIO<unit, 'E> =
        this.Result().FlatMap(function
            | Ok value -> onSuccess value
            | Error error -> onError error)

    /// <summary>Builds an effect that retries this effect on failure and falls back to another effect once attempts are exhausted.</summary>
    /// <typeparam name="'E1">The error type of the fallback effect and resulting effect.</typeparam>
    /// <param name="maxAttempts">The total number of attempts permitted; must be at least 1.</param>
    /// <param name="orElse">A function from the final typed error to the fallback effect.</param>
    /// <param name="onEachRetry">An optional callback receiving the error, the attempt index, and the maximum, and producing an effect to run before each retry.</param>
    /// <returns>An effect that completes with the first success, or with the fallback effect's outcome once retries are exhausted.</returns>
    member inline this.RetryOrElse<'E1> (maxAttempts: int) (orElse: 'E -> FIO<'A, 'E1>) (onEachRetry: 'E * int * int -> FIO<unit, 'E1>) : FIO<'A, 'E1> =
        if maxAttempts < 1 then
            FIO.interrupt (InvalidArgument("maxAttempts", "must be >= 1")) "Invalid argument: maxAttempts must be >= 1"
        else
            let rec loop attempt =
                this.CatchAll(fun error ->
                    if attempt >= maxAttempts then
                        orElse error
                    else
                        onEachRetry(error, attempt, maxAttempts)
                            .FlatMap(fun () -> loop (attempt + 1)))
            loop 1

    /// <summary>Builds an effect that retries this effect on failure up to a maximum number of attempts.</summary>
    /// <param name="maxAttempts">The total number of attempts permitted; must be at least 1.</param>
    /// <param name="onEachRetry">An optional callback receiving the error, the attempt index, and the maximum, and producing an effect to run before each retry.</param>
    /// <returns>An effect that completes with the first success, or fails with the final error after exhausting <paramref name="maxAttempts"/>.</returns>
    member inline this.Retry (maxAttempts: int) (onEachRetry: 'E * int * int -> FIO<unit, 'E>) : FIO<'A, 'E> =
        this.RetryOrElse maxAttempts FIO.fail onEachRetry

    /// <summary>Builds an effect that retries this effect on failure until a pure predicate examining the error returns <c>true</c>.</summary>
    /// <param name="predicate">A pure predicate evaluated against the latest error after each failure; the loop stops once it returns <c>true</c>.</param>
    /// <returns>An effect that completes with the first success, or fails with the error for which <paramref name="predicate"/> returned <c>true</c>.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed.</remarks>
    member inline this.RetryUntil (predicate: 'E -> bool) : FIO<'A, 'E> =
        this.RetryUntilFIO(fun error -> FIO.succeed (predicate error))

    /// <summary>Builds an effect that retries this effect on failure until the error equals a given value.</summary>
    /// <param name="error">The target error; the loop stops once the error equals it.</param>
    /// <returns>An effect that completes with the first success, or fails with <paramref name="error"/> once the error matches.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed. Equality uses <c>Unchecked.equals</c> so no <c>equality</c> constraint is required.</remarks>
    member inline this.RetryUntilEquals (error: 'E) : FIO<'A, 'E> =
        this.RetryUntil(fun err -> Unchecked.equals err error)

    /// <summary>Builds an effect that retries this effect on failure until an effectful predicate examining the error returns <c>true</c>.</summary>
    /// <param name="predicate">An effectful predicate evaluated against the latest error after each failure; the loop stops once it returns <c>true</c>.</param>
    /// <returns>An effect that completes with the first success, or fails with the error for which <paramref name="predicate"/> returned <c>true</c>.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed. Failures from <paramref name="predicate"/> propagate as the effect's error.</remarks>
    member this.RetryUntilFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.CatchAll(fun error -> (predicate error).FlatMap(fun stop ->
                if stop then FIO.fail error else loop ()))
        loop ()

    /// <summary>Builds an effect that retries this effect on failure while a pure predicate examining the error returns <c>true</c>.</summary>
    /// <param name="predicate">A pure predicate evaluated against the latest error after each failure; the loop stops once it returns <c>false</c>.</param>
    /// <returns>An effect that completes with the first success, or fails with the error for which <paramref name="predicate"/> returned <c>false</c>.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed.</remarks>
    member inline this.RetryWhile (predicate: 'E -> bool) : FIO<'A, 'E> =
        this.RetryWhileFIO(fun error -> FIO.succeed (predicate error))

    /// <summary>Builds an effect that retries this effect on failure while the error equals a given value.</summary>
    /// <param name="error">The target error; the loop stops once the error differs from it.</param>
    /// <returns>An effect that completes with the first success, or fails with the first error that differs from <paramref name="error"/>.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed. Equality uses <c>Unchecked.equals</c> so no <c>equality</c> constraint is required.</remarks>
    member inline this.RetryWhileEquals (error: 'E) : FIO<'A, 'E> =
        this.RetryWhile(fun error -> Unchecked.equals error error)

    /// <summary>Builds an effect that retries this effect on failure while an effectful predicate examining the error returns <c>true</c>.</summary>
    /// <param name="predicate">An effectful predicate evaluated against the latest error after each failure; the loop stops once it returns <c>false</c>.</param>
    /// <returns>An effect that completes with the first success, or fails with the error for which <paramref name="predicate"/> returned <c>false</c>.</returns>
    /// <remarks>The loop is unbounded; combining with <c>Retry(maxAttempts)</c> or <c>RetryOrElse</c> is redundant because the cap is swallowed. Pair with <c>Timeout</c> when a wall-clock bound is needed. Failures from <paramref name="predicate"/> propagate as the effect's error.</remarks>
    member this.RetryWhileFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.CatchAll(fun error -> (predicate error).FlatMap(fun cont ->
                if cont then loop () else FIO.fail error))
        loop ()

    /// <summary>Builds an effect that retries this effect on failure indefinitely, succeeding once it succeeds.</summary>
    /// <typeparam name="'E1">The error type of the resulting effect; never produced because failures are swallowed and retried.</typeparam>
    /// <returns>An effect that completes with the first success of this effect.</returns>
    /// <remarks>The loop is unbounded; if this effect fails permanently the fiber loops forever. Pair with <c>Timeout</c> when a wall-clock bound is needed. The error type is free because failures are never observed by the caller.</remarks>
    member this.Eventually<'E1> () : FIO<'A, 'E1> =
        let rec loop () =
            this.CatchAll(fun _ -> loop ())
        loop ()

    /// <summary>Builds an effect that runs this effect a fixed number of times and returns the last result.</summary>
    /// <param name="n">The number of repetitions; values less than or equal to 1 evaluate this effect once.</param>
    /// <returns>An effect that completes with the result of the final iteration.</returns>
    member this.RepeatN (n: int) : FIO<'A, 'E> =
        if n <= 1 then
            this
        else
            this.FlatMap(fun _ -> FIO.suspend (fun () -> this.RepeatN(n - 1)))

    /// <summary>Builds an effect that re-runs this effect until a pure predicate examining the success value returns <c>true</c>.</summary>
    /// <param name="predicate">A pure predicate evaluated against the latest success value after each run; the loop stops once it returns <c>true</c>.</param>
    /// <returns>An effect that completes with the first success value for which <paramref name="predicate"/> returns <c>true</c>.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed.</remarks>
    member inline this.RepeatUntil (predicate: 'A -> bool) : FIO<'A, 'E> =
        this.RepeatUntilFIO(fun value -> FIO.succeed (predicate value))

    /// <summary>Builds an effect that re-runs this effect until the success value equals a given value.</summary>
    /// <param name="value">The target value; the loop stops once the success value equals it.</param>
    /// <returns>An effect that completes with <paramref name="value"/> once the success value matches.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed. Equality uses <c>Unchecked.equals</c> so no <c>equality</c> constraint is required.</remarks>
    member inline this.RepeatUntilEquals (result: 'A) : FIO<'A, 'E> =
        this.RepeatUntil(fun value -> Unchecked.equals value result)

    /// <summary>Builds an effect that re-runs this effect until an effectful predicate examining the success value returns <c>true</c>.</summary>
    /// <param name="predicate">An effectful predicate evaluated against the latest success value after each run; the loop stops once it returns <c>true</c>.</param>
    /// <returns>An effect that completes with the first success value for which <paramref name="predicate"/> returns <c>true</c>.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed. Failures from <paramref name="predicate"/> propagate as the effect's error.</remarks>
    member this.RepeatUntilFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap(fun value -> (predicate value).FlatMap(fun stop ->
                if stop then FIO.succeed value else loop ()))
        loop ()

    /// <summary>Builds an effect that re-runs this effect while a pure predicate examining the success value returns <c>true</c>.</summary>
    /// <param name="predicate">A pure predicate evaluated against the latest success value after each run; the loop stops once it returns <c>false</c>.</param>
    /// <returns>An effect that completes with the first success value for which <paramref name="predicate"/> returns <c>false</c>.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed.</remarks>
    member inline this.RepeatWhile (predicate: 'A -> bool) : FIO<'A, 'E> =
        this.RepeatWhileFIO(fun value -> FIO.succeed (predicate value))

    /// <summary>Builds an effect that re-runs this effect while the success value equals a given value.</summary>
    /// <param name="value">The target value; the loop stops once the success value differs from it.</param>
    /// <returns>An effect that completes with the first success value that differs from <paramref name="value"/>.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed. Equality uses <c>Unchecked.equals</c> so no <c>equality</c> constraint is required.</remarks>
    member inline this.RepeatWhileEquals (value: 'A) : FIO<'A, 'E> =
        this.RepeatWhile(fun value -> Unchecked.equals value value)

    /// <summary>Builds an effect that re-runs this effect while an effectful predicate examining the success value returns <c>true</c>.</summary>
    /// <param name="predicate">An effectful predicate evaluated against the latest success value after each run; the loop stops once it returns <c>false</c>.</param>
    /// <returns>An effect that completes with the first success value for which <paramref name="predicate"/> returns <c>false</c>.</returns>
    /// <remarks>This effect always runs at least once. The loop is unbounded; pair with <c>Timeout</c> when a cap is needed. Failures from <paramref name="predicate"/> propagate as the effect's error.</remarks>
    member this.RepeatWhileFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap(fun value -> (predicate value).FlatMap(fun cont ->
                if cont then loop () else FIO.succeed value))
        loop ()

    /// <summary>Builds an effect that runs this effect indefinitely, never producing a success value.</summary>
    /// <returns>An effect that loops forever; the success type is nominal because no value is ever produced. Failures from this effect propagate immediately.</returns>
    /// <remarks>The only way to terminate this effect is fiber interruption (e.g. via <c>Timeout</c> or <c>InterruptFiber</c>). Use <c>Eventually</c> instead when failures should be swallowed and retried.</remarks>
    member this.Forever () : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap(fun _ -> loop ())
        loop ()

    /// <summary>Combines this effect with a leading delay, sleeping for the given duration before running.</summary>
    /// <param name="duration">The amount of time to sleep before running this effect.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with this effect's outcome after the delay elapses.</returns>
    /// <remarks>This is a leading delay applied once, not an inter-iteration delay. Combining with <c>Retry</c> or <c>RepeatN</c> sleeps once before the first attempt, not between iterations.</remarks>
    member inline this.Delay (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
        (FIO.sleep duration onError).FlatMap(fun () -> this)

    /// <summary>Combines this effect with a timeout, returning <c>None</c> when the deadline is reached before completion.</summary>
    /// <param name="duration">The maximum time to wait for completion.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with <c>Some</c> when this effect finishes in time, or <c>None</c> when the timeout fires first.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes.</remarks>
    member this.Timeout (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A option, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap(fun () -> FIO.succeed None)
        this.Map(Some).Race timeoutEff

    /// <summary>Combines this effect with a timeout, failing with a supplied typed error when the deadline is reached before completion.</summary>
    /// <param name="timeoutError">The typed error to fail with when the timeout fires before this effect completes.</param>
    /// <param name="duration">The maximum time to wait for completion.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with this effect's success value when it finishes in time, or fails with <paramref name="timeoutError"/> when the timeout fires first.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes. Prefer this over <c>Timeout</c> when callers want a typed failure rather than an <c>Option</c>.</remarks>
    member this.TimeoutFail (timeoutError: 'E) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap(fun () -> FIO.fail timeoutError)
        this.Race timeoutEff

    /// <summary>Combines this effect with a timeout, applying a success mapper or substituting a default when the deadline is reached.</summary>
    /// <typeparam name="'A1">The result type produced by both the success mapper and the default value.</typeparam>
    /// <param name="defaultValue">The value to produce when the timeout fires before this effect completes.</param>
    /// <param name="onSuccess">A function from this effect's success value to the resulting value.</param>
    /// <param name="duration">The maximum time to wait for completion.</param>
    /// <param name="onError">A function that maps an exception thrown by the underlying delay to the typed error.</param>
    /// <returns>An effect that completes with <paramref name="onSuccess"/> applied to this effect's success value when it finishes in time, or with <paramref name="defaultValue"/> when the timeout fires first.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes. Generalises <c>Timeout</c> by letting the caller choose both branches' shape.</remarks>
    member this.TimeoutTo<'A1> (defaultValue: 'A1) (onSuccess: 'A -> 'A1) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A1, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap(fun () -> FIO.succeed defaultValue)
        this.Map(onSuccess).Race timeoutEff

    /// <summary>Combines this effect with timing instrumentation, producing both the elapsed duration and the result.</summary>
    /// <param name="onError">A function that maps an exception thrown by the stopwatch start to the typed error.</param>
    /// <returns>An effect that completes with a tuple of the elapsed time and the original success value.</returns>
    member inline this.Timed (onError: exn -> 'E) : FIO<TimeSpan * 'A, 'E> =
        (FIO.attempt Stopwatch.StartNew onError).FlatMap(fun sw ->
            this.Ensuring(FIO.unit().FlatMap(fun () ->
                sw.Stop()
                FIO.unit ()))
                    .Map(fun value -> sw.Elapsed, value))

    /// <summary>Combines this effect with another concurrently and completes with whichever finishes first.</summary>
    /// <param name="eff">The effect to race against this one.</param>
    /// <returns>An effect that completes with the result of the first racer to terminate.</returns>
    /// <remarks>The losing fiber is interrupted once the winner completes.</remarks>
    member this.Race (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.suspend <| fun () ->
            let resultChan = Channel<bool>()

            let signal (won: bool) (fiber: Fiber<'A, 'E>) =
                fiber.Await().FlatMap(fun result ->
                    match result with
                    | Succeeded _
                    | Failed _ -> resultChan.Write(won).Map(fun _ -> ())
                    | Interrupted _ -> FIO.unit ())

            this.Fork().FlatMap(fun fiber1 ->
                eff.Fork().FlatMap(fun fiber2 ->
                    (signal true fiber1).Fork().FlatMap(fun _ ->
                        (signal false fiber2).Fork().FlatMap(fun _ ->
                            resultChan.Read().FlatMap(fun winnerIsFirst ->
                                let winner, loser =
                                    if winnerIsFirst then fiber1, fiber2 else fiber2, fiber1

                                (loser.Interrupt ExplicitInterrupt "Lost race")
                                    .CatchAll(fun _ -> FIO.unit ())
                                    .FlatMap <| fun () -> winner.Join())))))

    /// <summary>Combines this effect with another concurrently, returning a <c>Choice</c> tagged with which racer terminated first.</summary>
    /// <typeparam name="'A1">The success result type of the right-hand racer.</typeparam>
    /// <param name="eff">The effect to race against this one.</param>
    /// <returns>An effect that completes with <c>Choice1Of2</c> when this effect wins, or <c>Choice2Of2</c> when <paramref name="eff"/> wins.</returns>
    /// <remarks>First-to-complete semantics, mirroring <c>Race</c>; the losing fiber is interrupted once the winner terminates. Use this when the racers have different success types.</remarks>
    member this.RaceEither<'A1> (eff: FIO<'A1, 'E>) : FIO<Choice<'A, 'A1>, 'E> =
        this.Map(Choice1Of2).Race(eff.Map Choice2Of2)

    /// <summary>Combines this effect with another concurrently using first-to-succeed semantics; a failure does not win the race.</summary>
    /// <param name="eff">The effect to race against this one.</param>
    /// <returns>An effect that completes with the first racer's success value, or fails with the most-recently-observed error when both racers fail.</returns>
    /// <remarks>Distinct from <c>Race</c> (first-to-complete): a failing racer is retired without winning, and the receiver continues waiting for the peer. Use this for failover / replication patterns where any successful source should win. The losing fiber is interrupted once a winner is determined.</remarks>
    member this.RaceFirstSuccess (eff: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.suspend <| fun () ->
            let resultChan = Channel<Result<'A * bool, 'E>>()

            let signal (isFirst: bool) (fiber: Fiber<'A, 'E>) =
                fiber.Await().FlatMap(fun result ->
                    match result with
                    | Succeeded value -> resultChan.Write(Ok(value, isFirst)).Map(fun _ -> ())
                    | Failed error -> resultChan.Write(Error error).Map(fun _ -> ())
                    | Interrupted _ -> FIO.unit ())

            this.Fork().FlatMap(fun fiber1 ->
                eff.Fork().FlatMap(fun fiber2 ->
                    (signal true fiber1).Fork().FlatMap(fun _ ->
                        (signal false fiber2).Fork().FlatMap(fun _ ->
                            resultChan.Read().FlatMap(fun first ->
                                match first with
                                | Ok(value, isFirst) ->
                                    let loser = if isFirst then fiber2 else fiber1

                                    (loser.Interrupt ExplicitInterrupt "Lost race")
                                        .CatchAll(fun _ -> FIO.unit ())
                                        .FlatMap(fun () -> FIO.succeed value)
                                | Error _ ->
                                    resultChan.Read().FlatMap(fun second ->
                                        match second with
                                        | Ok(value, _) -> FIO.succeed value
                                        | Error error -> FIO.fail error))))))
