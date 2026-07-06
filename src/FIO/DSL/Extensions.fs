namespace FIO.DSL

open System
open System.Diagnostics

[<AutoOpen>]
module Extensions =

    type FIO<'A, 'E> with

        /// Returns an effect that applies a possibly-throwing function to this effect's success value, mapping any exception to a typed error.
        member inline this.MapAttempt<'A1> (mapper: 'A -> 'A1) (onError: exn -> 'E) : FIO<'A1, 'E> =
            this.FlatMap <| fun value ->
                FIO.attempt (fun () ->
                    mapper value) onError

        /// Returns an effect that discards this effect's success value, yielding unit.
        member inline this.Unit () : FIO<unit, 'E> =
            this.Map <| fun _ -> ()

        /// Returns an effect that replaces this effect's success value with the given value.
        member inline this.As<'A1> (value: 'A1) : FIO<'A1, 'E> =
            this.Map <| fun _ -> value

        /// Returns an effect that wraps this effect's success value in Choice1Of2.
        member inline this.AsLeft<'A1> () : FIO<Choice<'A, 'A1>, 'E> =
            this.Map Choice1Of2

        /// Returns an effect that wraps this effect's success value in Choice2Of2.
        member inline this.AsRight<'A1> () : FIO<Choice<'A1, 'A>, 'E> =
            this.Map Choice2Of2

        /// Returns an effect that wraps this effect's success value in Some.
        member inline this.AsSome () : FIO<'A option, 'E> =
            this.Map Some

        /// Returns an effect that wraps this effect's error in Choice1Of2.
        member inline this.AsLeftError<'E1> () : FIO<'A, Choice<'E, 'E1>> =
            this.MapError Choice1Of2

        /// Returns an effect that wraps this effect's error in Choice2Of2.
        member inline this.AsRightError<'E1> () : FIO<'A, Choice<'E1, 'E>> =
            this.MapError Choice2Of2

        /// Returns an effect that wraps this effect's error in Some.
        member inline this.AsSomeError<'A, 'E> () : FIO<'A, 'E option> =
            this.MapError Some

        /// Returns an effect that swaps this effect's success and error channels.
        member inline this.Flip () : FIO<'E, 'A> =
            this.Result().FlatMap <| function
            | Ok value -> FIO.fail value
            | Error error -> FIO.succeed error

        /// Returns an effect that discards this effect's outcome, succeeding with unit regardless.
        member inline this.Ignore<'E1> () : FIO<unit, 'E1> =
            this.Fold (fun _ -> ()) (fun _ -> ())

        /// Returns an effect that yields true if this effect succeeds and false if it fails.
        member inline this.IsSuccess<'E1> () : FIO<bool, 'E1> =
            this.Fold (fun _ -> false) (fun _ -> true)

        /// Returns an effect that yields true if this effect fails and false if it succeeds.
        member inline this.IsFailure<'E1> () : FIO<bool, 'E1> =
            this.Fold (fun _ -> true) (fun _ -> false)

        /// Returns an effect that runs this effect only when the condition is true, otherwise yielding unit.
        member inline this.When (cond: bool) : FIO<unit, 'E> =
            if cond then this.Unit() else FIO.unit ()

        /// Returns an effect that runs this effect only when the condition is false, otherwise yielding unit.
        member inline this.Unless (cond: bool) : FIO<unit, 'E> =
            this.When(not cond)

        /// Returns an effect that runs a side effect on this effect's success value, keeping the original value.
        member inline this.Tap<'A1> (onSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                (onSuccess value).As value

        /// Returns an effect that runs a side effect on this effect's error, keeping the original error.
        member inline this.TapError<'A1> (onError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
            this.CatchAll <| fun error ->
                (onError error).FlatMap <| fun _ ->
                    FIO.fail error

        /// Returns an effect that runs a side effect on this effect's success value or error, keeping the original outcome.
        member this.TapBoth<'A1, 'A2> (onSuccess: 'A -> FIO<'A2, 'E>) (onError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
            ChainBoth(
                this.UpcastBoth(),
                (fun value ->
                    let value = value :?> 'A
                    (onSuccess value).As value),
                (fun error ->
                    let error = error :?> 'E
                    (onError error).FlatMap(fun _ -> FIO.fail error)))

        /// Returns an effect that prints this effect's success value, keeping the original value.
        member inline this.Debug () : FIO<'A, 'E> =
            this.Debug "Debug"

        /// Returns an effect that prints this effect's success value with the given label, keeping the original value.
        member this.Debug (message: string) : FIO<'A, 'E> =
            this.Tap <| fun value ->
                (FIO.attempt (fun () ->
                    try printfn "%s: %A" message value
                    with ex ->
                        try eprintfn "Debug print failed. Message: %s, Exception: %s" message ex.Message
                        with _ -> ()
                ) id).Ignore()

        /// Returns an effect that prints this effect's error, keeping the original error.
        member inline this.DebugError () : FIO<'A, 'E> =
            this.DebugError "Debug Error"

        /// Returns an effect that prints this effect's error with the given label, keeping the original error.
        member this.DebugError (message: string) : FIO<'A, 'E> =
            this.TapError <| fun error ->
                (FIO.attempt (fun () ->
                    try printfn "%s: %A" message error
                    with ex ->
                        try eprintfn "Debug Error print failed. Message: %s, Exception: %s" message ex.Message
                        with _ -> ()
                ) id).Ignore()

        /// Returns an effect that falls back to the given effect if this effect fails.
        member inline this.OrElse<'E1> (effect: FIO<'A, 'E1>) : FIO<'A, 'E1> =
            this.CatchAll <| fun _ -> effect

        /// Returns an effect that falls back to succeeding with the given value if this effect fails.
        member inline this.OrElseSucceed<'E1> (value: 'A) : FIO<'A, 'E1> =
            this.CatchAll <| fun _ -> FIO.succeed value

        /// Returns an effect that falls back to failing with the given error if this effect fails.
        member inline this.OrElseFail<'E1> (error: 'E1) : FIO<'A, 'E1> =
            this.CatchAll <| fun _ -> FIO.fail error

        /// Returns an effect yielding this effect's success in Choice1Of2, or the fallback effect's success in Choice2Of2 if this effect fails.
        member this.OrElseEither<'A1, 'E1> (effect: FIO<'A1, 'E1>) : FIO<Choice<'A, 'A1>, 'E1> =
            ChainBoth(
                this.UpcastBoth(),
                (fun value -> Success (Choice1Of2 (value :?> 'A))),
                (fun _ -> effect.Map Choice2Of2))

        /// Returns an effect that recovers from some errors with the given handler, re-failing the rest.
        member inline this.CatchSome (func: 'E -> FIO<'A, 'E> option) : FIO<'A, 'E> =
            this.CatchAll <| fun error ->
                match func error with
                | Some recovery -> recovery
                | None -> FIO.fail error

        /// Returns an effect that interrupts the current fiber if this effect fails, using a message derived from the error.
        member inline this.OrInterrupt<'E1> (toMessage: 'E -> string) : FIO<'A, 'E1> =
            this.CatchAll <| fun error ->
                FIO.interrupt ExplicitInterrupt (toMessage error)

        /// Returns an effect that fails with the given error unless this effect's success value satisfies the predicate.
        member inline this.FilterOrFail (predicate: 'A -> bool) (error: 'E) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                if predicate value then FIO.succeed value
                else FIO.fail error

        /// Returns an effect that falls back to the given effect unless this effect's success value satisfies the predicate.
        member inline this.FilterOrElse (predicate: 'A -> bool) (fallback: FIO<'A, 'E>) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                if predicate value then FIO.succeed value
                else fallback

        /// Returns an effect that falls back to the given function unless this effect's success value satisfies the predicate.
        member inline this.FilterOrElseWith (predicate: 'A -> bool) (fallback: 'A -> FIO<'A, 'E>) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                if predicate value then FIO.succeed value
                else fallback value

        /// Returns an effect that interrupts the current fiber unless this effect's success value satisfies the predicate.
        member inline this.FilterOrInterrupt (predicate: 'A -> bool) (message: string) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                if predicate value then FIO.succeed value
                else FIO.interrupt ExplicitInterrupt message

        /// Returns an effect that fails when the given function produces an error for this effect's success value.
        member inline this.Reject (func: 'A -> 'E option) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                match func value with
                | Some error -> FIO.fail error
                | None -> FIO.succeed value

        /// Returns an effect that fails when the given effectful function produces an error for this effect's success value.
        member inline this.RejectFIO (func: 'A -> FIO<'E, 'E> option) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                match func value with
                | Some errorEffect -> errorEffect.FlatMap FIO.fail
                | None -> FIO.succeed value

        /// Returns an effect that maps this effect's success value through the given partial function, failing with the given error when it yields None.
        member inline this.Collect<'A1> (error: 'E) (func: 'A -> 'A1 option) : FIO<'A1, 'E> =
            this.FlatMap <| fun value ->
                match func value with
                | Some outValue -> FIO.succeed outValue
                | None -> FIO.fail error

        /// Returns an effect that continues with the given partial function's effect, failing with the given error when it yields None.
        member inline this.CollectFIO<'A1> (error: 'E) (func: 'A -> FIO<'A1, 'E> option) : FIO<'A1, 'E> =
            this.FlatMap <| fun value ->
                match func value with
                | Some effect -> effect
                | None -> FIO.fail error

        /// Returns an effect that applies a function produced by the given effect to this effect's success value.
        member inline this.Apply<'A1> (effect: FIO<'A -> 'A1, 'E>) : FIO<'A1, 'E> =
            effect.FlatMap this.Map

        /// Returns an effect that applies a function produced by the given effect to this effect's error.
        member inline this.ApplyError<'E1> (effect: FIO<'A, 'E -> 'E1>) : FIO<'A, 'E1> =
            effect.CatchAll this.MapError

        /// Returns an effect that runs this effect then the given effect, pairing their success values.
        member inline this.Zip<'A1> (effect: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
            this.FlatMap <| fun value ->
                effect.Map <| fun value' ->
                    value, value'

        /// Returns an effect that pairs this effect's error with the given effect's error.
        member inline this.ZipError<'E1> (effect: FIO<'A, 'E1>) : FIO<'A, 'E * 'E1> =
            this.CatchAll <| fun error ->
                effect.MapError <| fun error' ->
                    error, error'

        /// Returns an effect that runs this effect then the given effect, keeping only the second value.
        member inline this.ZipRight<'A1> (effect: FIO<'A1, 'E>) : FIO<'A1, 'E> =
            this.FlatMap <| fun _ -> effect

        /// Returns an effect that runs this effect then the given effect, keeping only the first value.
        member inline this.ZipLeft<'A1> (effect: FIO<'A1, 'E>) : FIO<'A, 'E> =
            this.FlatMap <| fun value ->
                effect.Map <| fun _ ->
                    value

        /// Returns an effect that runs this effect and the given effect concurrently, pairing their success values, failing fast by interrupting the other side on the first failure. When both sides fail, the surfaced error is whichever settled first.
        member this.ZipPar<'A1> (effect: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
            FIO.suspend <| fun () ->
                this.Fork().FlatMap <| fun fiber1 ->
                    effect.Fork().FlatMap <| fun fiber2 ->
                        let interruptBoth () =
                            (fiber1.Interrupt ExplicitInterrupt "ZipPar sibling failed").Ignore()
                                .FlatMap <| fun () ->
                                    (fiber2.Interrupt ExplicitInterrupt "ZipPar sibling failed").Ignore()

                        (FIO.joinFirst [fiber1.Context; fiber2.Context]).FlatMap <| fun index ->
                            if index = 0 then
                                fiber1.Await().FlatMap <| fun result ->
                                    match result with
                                    | Succeeded value1 ->
                                        fiber2.Join().Map <| fun value2 ->
                                            value1, value2
                                    | Failed error ->
                                        (interruptBoth ()).FlatMap <| fun () -> FIO.fail error
                                    | Interrupted ex ->
                                        (interruptBoth ()).FlatMap <| fun () -> FIO.interrupt ex.cause ex.message
                            else
                                fiber2.Await().FlatMap <| fun result ->
                                    match result with
                                    | Succeeded value2 ->
                                        fiber1.Join().Map <| fun value1 ->
                                            value1, value2
                                    | Failed error ->
                                        (interruptBoth ()).FlatMap <| fun () -> FIO.fail error
                                    | Interrupted ex ->
                                        (interruptBoth ()).FlatMap <| fun () -> FIO.interrupt ex.cause ex.message

        /// Returns an effect that runs this effect and the given effect concurrently, pairing their errors. Succeeds fast: the first side to succeed interrupts the other. The error pair is surfaced only if both fail; when both succeed, the surfaced value is whichever settled first (unspecified).
        member this.ZipParError (effect: FIO<'A, 'E>) : FIO<'A, 'E * 'E> =
            FIO.suspend <| fun () ->
                this.Fork().FlatMap <| fun fiber1 ->
                    effect.Fork().FlatMap <| fun fiber2 ->
                        let interruptBoth () =
                            (fiber1.Interrupt ExplicitInterrupt "ZipParError sibling succeeded").Ignore()
                                .FlatMap <| fun () ->
                                    (fiber2.Interrupt ExplicitInterrupt "ZipParError sibling succeeded").Ignore()

                        (FIO.joinFirst [fiber1.Context; fiber2.Context]).FlatMap <| fun index ->
                            let winner, loser = if index = 0 then fiber1, fiber2 else fiber2, fiber1
                            winner.Await().FlatMap <| fun winnerResult ->
                                match winnerResult with
                                | Succeeded value ->
                                    (interruptBoth ()).FlatMap <| fun () -> FIO.succeed value
                                | Failed winnerError ->
                                    loser.Await().FlatMap <| fun loserResult ->
                                        match loserResult with
                                        | Succeeded value ->
                                            (interruptBoth ()).FlatMap <| fun () -> FIO.succeed value
                                        | Failed loserError ->
                                            let thisError, effectError =
                                                if index = 0 then winnerError, loserError
                                                else loserError, winnerError
                                            FIO.fail (thisError, effectError)
                                        | Interrupted ex ->
                                            FIO.interrupt ex.cause ex.message
                                | Interrupted ex ->
                                    (interruptBoth ()).FlatMap <| fun () -> FIO.interrupt ex.cause ex.message

        /// Returns an effect that runs this effect and the given effect concurrently, keeping only the second value.
        member inline this.ZipParRight<'A1> (effect: FIO<'A1, 'E>) : FIO<'A1, 'E> =
            this.ZipPar(effect).Map snd

        /// Returns an effect that runs this effect and the given effect concurrently, keeping only the first value.
        member inline this.ZipParLeft<'A1> (effect: FIO<'A1, 'E>) : FIO<'A, 'E> =
            this.ZipPar(effect).Map fst

        /// Returns an effect that always succeeds, mapping this effect's success or error to a single value.
        member this.Fold<'A1, 'E1> (onError: 'E -> 'A1) (onSuccess: 'A -> 'A1) : FIO<'A1, 'E1> =
            ChainBoth(
                this.UpcastBoth(),
                (fun value -> Success (onSuccess (value :?> 'A))),
                (fun error -> Success (onError (error :?> 'E))))

        /// Returns an effect that continues with the matching effectful handler for this effect's success or error.
        member this.FoldFIO<'A1> (onError: 'E -> FIO<'A1, 'E>) (onSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
            ChainBoth(
                this.UpcastBoth(),
                (fun value -> onSuccess (value :?> 'A)),
                (fun error -> onError (error :?> 'E)))

        /// Returns an effect that runs the matching finalizing effect for this effect's success or error.
        member inline this.OnDone (onError: 'E -> FIO<unit, 'E>) (onSuccess: 'A -> FIO<unit, 'E>) : FIO<unit, 'E> =
            this.Result().FlatMap <| function
                | Ok value -> onSuccess value
                | Error error -> onError error

        /// Returns an effect that retries this effect up to the given number of attempts, falling back to the given handler when they are exhausted.
        member inline this.RetryOrElse<'E1>
            (maxAttempts: int)
            (orElse: 'E -> FIO<'A, 'E1>)
            (onEachRetry: 'E * int * int -> FIO<unit, 'E1>)
            : FIO<'A, 'E1> =
            if maxAttempts < 1 then
                FIO.interrupt (InvalidArgument("maxAttempts", "must be >= 1")) "Invalid argument: maxAttempts must be >= 1"
            else
                let rec loop attempt =
                    this.CatchAll <| fun error ->
                        if attempt >= maxAttempts then
                            orElse error
                        else
                            onEachRetry(error, attempt, maxAttempts)
                                .FlatMap <| fun () -> loop (attempt + 1)
                loop 1

        /// Returns an effect that retries this effect up to the given number of attempts before failing.
        member inline this.Retry (maxAttempts: int) (onEachRetry: 'E * int * int -> FIO<unit, 'E>) : FIO<'A, 'E> =
            this.RetryOrElse maxAttempts FIO.fail onEachRetry

        /// Returns an effect that retries this effect until its error satisfies the predicate.
        member inline this.RetryUntil (predicate: 'E -> bool) : FIO<'A, 'E> =
            this.RetryUntilFIO <| fun error ->
                FIO.succeed (predicate error)

        /// Returns an effect that retries this effect until it fails with the given error.
        member inline this.RetryUntilEquals (error: 'E) : FIO<'A, 'E> =
            this.RetryUntil <| fun error' ->
                Unchecked.equals error' error

        /// Returns an effect that retries this effect until its error satisfies the effectful predicate.
        member this.RetryUntilFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
            let rec loop () =
                this.CatchAll <| fun error ->
                    (predicate error).FlatMap <| fun stop ->
                        if stop then FIO.fail error
                        else loop ()
            loop ()

        /// Returns an effect that retries this effect while its error satisfies the predicate.
        member inline this.RetryWhile (predicate: 'E -> bool) : FIO<'A, 'E> =
            this.RetryWhileFIO <| fun error ->
                FIO.succeed (predicate error)

        /// Returns an effect that retries this effect while it fails with the given error.
        member inline this.RetryWhileEquals (error: 'E) : FIO<'A, 'E> =
            this.RetryWhile <| fun error' ->
                Unchecked.equals error' error

        /// Returns an effect that retries this effect while its error satisfies the effectful predicate.
        member this.RetryWhileFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
            let rec loop () =
                this.CatchAll <| fun error ->
                    (predicate error).FlatMap <| fun cont ->
                        if cont then loop ()
                        else FIO.fail error
            loop ()

        /// Returns an effect that retries this effect until it succeeds.
        member inline this.Eventually<'E1> () : FIO<'A, 'E1> =
            let rec loop () =
                this.CatchAll <| fun _ -> loop ()
            loop ()

        /// Returns an effect that runs this effect n times, yielding the last result. Interrupts the fiber when n is less than 1.
        member this.RepeatN (n: int) : FIO<'A, 'E> =
            if n < 1 then
                FIO.interrupt (InvalidArgument("n", "must be >= 1")) "Invalid argument: n must be >= 1"
            elif n = 1 then
                this
            else
                this.FlatMap <|fun _ ->
                    FIO.suspend <| fun () ->
                        this.RepeatN (n - 1)

        /// Returns an effect that repeats this effect until its success value satisfies the predicate.
        member inline this.RepeatUntil (predicate: 'A -> bool) : FIO<'A, 'E> =
            this.RepeatUntilFIO <| fun value ->
                FIO.succeed <| predicate value

        /// Returns an effect that repeats this effect until it succeeds with the given value.
        member inline this.RepeatUntilEquals (value: 'A) : FIO<'A, 'E> =
            this.RepeatUntil <| fun value' ->
                Unchecked.equals value' value

        /// Returns an effect that repeats this effect until its success value satisfies the effectful predicate.
        member this.RepeatUntilFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
            let rec loop () =
                this.FlatMap <| fun value ->
                    (predicate value).FlatMap <| fun stop ->
                        if stop then FIO.succeed value
                        else loop ()
            loop ()

        /// Returns an effect that repeats this effect while its success value satisfies the predicate.
        member inline this.RepeatWhile (predicate: 'A -> bool) : FIO<'A, 'E> =
            this.RepeatWhileFIO <| fun value ->
                FIO.succeed <| predicate value

        /// Returns an effect that repeats this effect while it succeeds with the given value.
        member inline this.RepeatWhileEquals (value: 'A) : FIO<'A, 'E> =
            this.RepeatWhile <| fun value' ->
                Unchecked.equals value' value

        /// Returns an effect that repeats this effect while its success value satisfies the effectful predicate.
        member this.RepeatWhileFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
            let rec loop () =
                this.FlatMap <| fun value ->
                    (predicate value).FlatMap <| fun cont ->
                        if cont then loop ()
                        else FIO.succeed value
            loop ()

        /// Returns an effect that runs this effect repeatedly, forever.
        member inline this.Forever () : FIO<'A, 'E> =
            let rec loop () =
                this.FlatMap <| fun _ -> loop ()
            loop ()

        /// Returns an effect that waits the given duration before running this effect.
        member inline this.Delay (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
            (FIO.sleep duration onError).FlatMap <| fun () -> this

        /// Returns an effect that yields Some result if this effect completes within the duration, or None if it times out.
        member inline this.Timeout (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A option, 'E> =
            let timeoutEff =
                (FIO.sleep duration onError)
                    .FlatMap <| fun () ->
                        FIO.succeed None
            this.Map(Some).RaceFirst timeoutEff

        /// Returns an effect that fails with the given error if this effect does not complete within the duration.
        member inline this.TimeoutFail (timeoutError: 'E) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
            let timeoutEff =
                (FIO.sleep duration onError)
                    .FlatMap <| fun () ->
                        FIO.fail timeoutError
            this.RaceFirst timeoutEff

        /// Returns an effect that maps this effect's result, or yields the default value if it does not complete within the duration.
        member inline this.TimeoutTo<'A1> (defaultValue: 'A1) (onSuccess: 'A -> 'A1) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A1, 'E> =
            let timeoutEff =
                (FIO.sleep duration onError)
                    .FlatMap <| fun () ->
                        FIO.succeed defaultValue
            this.Map(onSuccess).RaceFirst timeoutEff

        /// Returns an effect that pairs this effect's success value with the time it took to run.
        member inline this.Timed (onError: exn -> 'E) : FIO<TimeSpan * 'A, 'E> =
            (FIO.attempt Stopwatch.StartNew onError).FlatMap <| fun stopwatch ->
                this.Ensuring(FIO.suspend (fun () -> stopwatch.Stop(); FIO.unit ()))
                    .Map <| fun value -> stopwatch.Elapsed, value

        /// Returns an effect that runs this effect and the given effect concurrently, yielding the first to settle and interrupting the loser.
        member this.RaceFirst (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            FIO.suspend <| fun () ->
                this.Fork().FlatMap <| fun fiber1 ->
                    effect.Fork().FlatMap <| fun fiber2 ->
                        (FIO.joinFirst [fiber1.Context; fiber2.Context]).FlatMap <| fun index ->
                            let winner, loser = if index = 0 then fiber1, fiber2 else fiber2, fiber1
                            (loser.Interrupt ExplicitInterrupt "Lost race")
                                .Ignore()
                                .FlatMap <| fun () -> winner.Join()

        /// Returns an effect that races this effect against the given effect, yielding the winner as a Choice.
        member inline this.RaceEither<'A1> (effect: FIO<'A1, 'E>) : FIO<Choice<'A, 'A1>, 'E> =
            this.Map(Choice1Of2).Race <| effect.Map Choice2Of2

        /// Returns an effect that runs this effect and the given effect concurrently, yielding the first to succeed and interrupting the loser.
        member this.Race (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
            FIO.suspend <| fun () ->
                this.Fork().FlatMap <| fun fiber1 ->
                    effect.Fork().FlatMap <| fun fiber2 ->
                        (FIO.joinFirst [fiber1.Context; fiber2.Context]).FlatMap <| fun index ->
                            let winner, loser = if index = 0 then fiber1, fiber2 else fiber2, fiber1
                            winner.Await().FlatMap <| fun result ->
                                match result with
                                | Succeeded value ->
                                    (loser.Interrupt ExplicitInterrupt "Lost race")
                                        .Ignore()
                                        .As value
                                | Failed _
                                | Interrupted _ ->
                                    loser.Join()
