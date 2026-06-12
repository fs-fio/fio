[<AutoOpen>]
module FIO.DSL.Extensions

open System
open System.Diagnostics

type FIO<'A, 'E> with

    member inline this.MapAttempt<'A1> (mapper: 'A -> 'A1) (onError: exn -> 'E) : FIO<'A1, 'E> =
        this.FlatMap <| fun value ->
            FIO.attempt (fun () ->
                mapper value) onError

    member inline this.Unit () : FIO<unit, 'E> =
        this.Map <| fun _ -> ()

    member inline this.As<'A1> (value: 'A1) : FIO<'A1, 'E> =
        this.Map <| fun _ -> value

    member inline this.AsLeft<'A1> () : FIO<Choice<'A, 'A1>, 'E> =
        this.Map Choice1Of2

    member inline this.AsRight<'A1> () : FIO<Choice<'A1, 'A>, 'E> =
        this.Map Choice2Of2

    member inline this.AsSome () : FIO<'A option, 'E> =
        this.Map Some

    member inline this.AsLeftError<'E1> () : FIO<'A, Choice<'E, 'E1>> =
        this.MapError Choice1Of2

    member inline this.AsRightError<'E1> () : FIO<'A, Choice<'E1, 'E>> =
        this.MapError Choice2Of2

    member inline this.AsSomeError<'A, 'E> () : FIO<'A, 'E option> =
        this.MapError Some

    member inline this.Flip () : FIO<'E, 'A> =
        this.Result().FlatMap <| function
        | Ok value -> FIO.fail value
        | Error error -> FIO.succeed error

    member inline this.Ignore<'E1> () : FIO<unit, 'E1> =
        this.Fold (fun _ -> ()) (fun _ -> ())

    member inline this.IsSuccess<'E1> () : FIO<bool, 'E1> =
        this.Fold (fun _ -> false) (fun _ -> true)

    member inline this.IsFailure<'E1> () : FIO<bool, 'E1> =
        this.Fold (fun _ -> true) (fun _ -> false)

    member inline this.When (cond: bool) : FIO<unit, 'E> =
        if cond then this.Unit() else FIO.unit ()

    member inline this.Unless (cond: bool) : FIO<unit, 'E> =
        this.When(not cond)

    member inline this.Tap<'A1> (onSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            (onSuccess value).As value

    member inline this.TapError<'A1> (onError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.CatchAll <| fun error ->
            (onError error).FlatMap <| fun _ ->
                FIO.fail error

    member this.TapBoth<'A1, 'A2> (onSuccess: 'A -> FIO<'A2, 'E>) (onError: 'E -> FIO<'A1, 'E>) : FIO<'A, 'E> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value ->
                let value = value :?> 'A
                (onSuccess value).As value),
            (fun error ->
                let error = error :?> 'E
                (onError error).FlatMap(fun _ -> FIO.fail error)))

    member inline this.Debug () : FIO<'A, 'E> =
        this.Debug "Debug"

    member this.Debug (message: string) : FIO<'A, 'E> =
        this.Tap <| fun value ->
            (FIO.attempt (fun () ->
                try printfn "%s: %A" message value
                with ex ->
                    try eprintfn "Debug print failed. Message: %s, Exception: %s" message ex.Message
                    with _ -> ()
            ) id).Ignore()

    member inline this.DebugError () : FIO<'A, 'E> =
        this.DebugError "Debug Error"

    member this.DebugError (message: string) : FIO<'A, 'E> =
        this.TapError <| fun error ->
            (FIO.attempt (fun () ->
                try printfn "%s: %A" message error
                with ex ->
                    try eprintfn "Debug Error print failed. Message: %s, Exception: %s" message ex.Message
                    with _ -> ()
            ) id).Ignore()

    member inline this.OrElse<'E1> (effect: FIO<'A, 'E1>) : FIO<'A, 'E1> =
        this.CatchAll <| fun _ -> effect

    member inline this.OrElseSucceed<'E1> (value: 'A) : FIO<'A, 'E1> =
        this.CatchAll <| fun _ -> FIO.succeed value

    member inline this.OrElseFail<'E1> (error: 'E1) : FIO<'A, 'E1> =
        this.CatchAll <| fun _ -> FIO.fail error

    member this.OrElseEither<'A1, 'E1> (effect: FIO<'A1, 'E1>) : FIO<Choice<'A, 'A1>, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (Choice1Of2 (value :?> 'A))),
            (fun _ -> effect.Map Choice2Of2))

    member inline this.CatchSome (func: 'E -> FIO<'A, 'E> option) : FIO<'A, 'E> =
        this.CatchAll <| fun error ->
            match func error with
            | Some recovery -> recovery
            | None -> FIO.fail error

    member inline this.OrInterrupt<'E1> (toMessage: 'E -> string) : FIO<'A, 'E1> =
        this.CatchAll <| fun error ->
            FIO.interrupt(
                ResourceExhaustion(toMessage error))
                "Fiber interrupted due to unrecoverable error"

    member inline this.FilterOrFail (predicate: 'A -> bool) (error: 'E) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            if predicate value then FIO.succeed value
            else FIO.fail error

    member inline this.FilterOrElse (predicate: 'A -> bool) (fallback: FIO<'A, 'E>) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            if predicate value then FIO.succeed value
            else fallback

    member inline this.FilterOrElseWith (predicate: 'A -> bool) (fallback: 'A -> FIO<'A, 'E>) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            if predicate value then FIO.succeed value
            else fallback value

    member inline this.FilterOrInterrupt (predicate: 'A -> bool) (message: string) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            if predicate value then FIO.succeed value
            else FIO.interrupt (ResourceExhaustion message) message

    member inline this.Reject (func: 'A -> 'E option) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            match func value with
            | Some error -> FIO.fail error
            | None -> FIO.succeed value

    member inline this.RejectFIO (func: 'A -> FIO<'E, 'E> option) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            match func value with
            | Some errorEffect -> errorEffect.FlatMap FIO.fail
            | None -> FIO.succeed value

    member inline this.Collect<'A1> (error: 'E) (func: 'A -> 'A1 option) : FIO<'A1, 'E> =
        this.FlatMap <| fun value ->
            match func value with
            | Some outValue -> FIO.succeed outValue
            | None -> FIO.fail error

    member inline this.CollectFIO<'A1> (error: 'E) (func: 'A -> FIO<'A1, 'E> option) : FIO<'A1, 'E> =
        this.FlatMap <| fun value ->
            match func value with
            | Some effect -> effect
            | None -> FIO.fail error

    member inline this.Apply<'A1> (effect: FIO<'A -> 'A1, 'E>) : FIO<'A1, 'E> =
        effect.FlatMap this.Map

    member inline this.ApplyError<'E1> (effect: FIO<'A, 'E -> 'E1>) : FIO<'A, 'E1> =
        effect.CatchAll this.MapError

    member inline this.Zip<'A1> (effect: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        this.FlatMap <| fun value ->
            effect.Map <| fun value' ->
                value, value'

    member inline this.ZipError<'E1> (effect: FIO<'A, 'E1>) : FIO<'A, 'E * 'E1> =
        this.CatchAll <| fun error ->
            effect.MapError <| fun error' ->
                error, error'

    member inline this.ZipRight<'A1> (effect: FIO<'A1, 'E>) : FIO<'A1, 'E> =
        this.FlatMap <| fun _ -> effect

    member inline this.ZipLeft<'A1> (effect: FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.FlatMap <| fun value ->
            effect.Map <| fun _ ->
                value

    member inline this.ZipPar<'A1> (effect: FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        effect.Fork().FlatMap <| fun fiber ->
            this.FlatMap <| fun value ->
                fiber.Join().Map <| fun value' ->
                    value, value'

    member inline this.ZipParError (effect: FIO<'A, 'E>) : FIO<'A, 'E * 'E> =
        effect.Fork().FlatMap(fun fiber ->
            this.FlatMap(fun value ->
                fiber.Join().As value).CatchAll <| fun error ->
                    fiber.Join().CatchAll <| fun error' ->
                        FIO.fail (error, error'))
    member inline this.ZipParRight<'A1> (effect: FIO<'A1, 'E>) : FIO<'A1, 'E> =
        this.ZipPar(effect).Map snd

    member inline this.ZipParLeft<'A1> (effect: FIO<'A1, 'E>) : FIO<'A, 'E> =
        this.ZipPar(effect).Map fst

    member this.Fold<'A1, 'E1> (onError: 'E -> 'A1) (onSuccess: 'A -> 'A1) : FIO<'A1, 'E1> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> Success (onSuccess (value :?> 'A))),
            (fun error -> Success (onError (error :?> 'E))))

    member this.FoldFIO<'A1> (onError: 'E -> FIO<'A1, 'E>) (onSuccess: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        ChainBoth(
            this.UpcastBoth(),
            (fun value -> onSuccess (value :?> 'A)),
            (fun error -> onError (error :?> 'E)))

    member inline this.OnDone (onError: 'E -> FIO<unit, 'E>) (onSuccess: 'A -> FIO<unit, 'E>) : FIO<unit, 'E> =
        this.Result().FlatMap <| function
            | Ok value -> onSuccess value
            | Error error -> onError error

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

    member inline this.Retry (maxAttempts: int) (onEachRetry: 'E * int * int -> FIO<unit, 'E>) : FIO<'A, 'E> =
        this.RetryOrElse maxAttempts FIO.fail onEachRetry

    member inline this.RetryUntil (predicate: 'E -> bool) : FIO<'A, 'E> =
        this.RetryUntilFIO <| fun error ->
            FIO.succeed (predicate error)

    member inline this.RetryUntilEquals (error: 'E) : FIO<'A, 'E> =
        this.RetryUntil <| fun error' ->
            Unchecked.equals error' error

    member this.RetryUntilFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.CatchAll <| fun error ->
                (predicate error).FlatMap <| fun stop ->
                    if stop then FIO.fail error
                    else loop ()
        loop ()

    member inline this.RetryWhile (predicate: 'E -> bool) : FIO<'A, 'E> =
        this.RetryWhileFIO <| fun error ->
            FIO.succeed (predicate error)

    member inline this.RetryWhileEquals (error: 'E) : FIO<'A, 'E> =
        this.RetryWhile <| fun error' ->
            Unchecked.equals error' error

    member this.RetryWhileFIO (predicate: 'E -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.CatchAll <| fun error ->
                (predicate error).FlatMap <| fun cont ->
                    if cont then loop ()
                    else FIO.fail error
        loop ()

    member inline this.Eventually<'E1> () : FIO<'A, 'E1> =
        let rec loop () =
            this.CatchAll <| fun _ -> loop ()
        loop ()

    member this.RepeatN (n: int) : FIO<'A, 'E> =
        if n <= 1 then
            this
        else
            this.FlatMap <|fun _ ->
                FIO.suspend <| fun () ->
                    this.RepeatN (n - 1)

    member inline this.RepeatUntil (predicate: 'A -> bool) : FIO<'A, 'E> =
        this.RepeatUntilFIO <| fun value ->
            FIO.succeed <| predicate value

    member inline this.RepeatUntilEquals (value: 'A) : FIO<'A, 'E> =
        this.RepeatUntil <| fun value' ->
            Unchecked.equals value' value

    member this.RepeatUntilFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap <| fun value ->
                (predicate value).FlatMap <| fun stop ->
                    if stop then FIO.succeed value
                    else loop ()
        loop ()

    member inline this.RepeatWhile (predicate: 'A -> bool) : FIO<'A, 'E> =
        this.RepeatWhileFIO <| fun value ->
            FIO.succeed <| predicate value

    member inline this.RepeatWhileEquals (value: 'A) : FIO<'A, 'E> =
        this.RepeatWhile <| fun value' ->
            Unchecked.equals value' value

    member this.RepeatWhileFIO (predicate: 'A -> FIO<bool, 'E>) : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap <| fun value ->
                (predicate value).FlatMap <| fun cont ->
                    if cont then loop ()
                    else FIO.succeed value
        loop ()

    member inline this.Forever () : FIO<'A, 'E> =
        let rec loop () =
            this.FlatMap <| fun _ -> loop ()
        loop ()

    member inline this.Delay (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
        (FIO.sleep duration onError).FlatMap <| fun () -> this

    member inline this.Timeout (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A option, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap <| fun () ->
                    FIO.succeed None
        this.Map(Some).RaceFirst timeoutEff

    member inline this.TimeoutFail (timeoutError: 'E) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap <| fun () ->
                    FIO.fail timeoutError
        this.RaceFirst timeoutEff

    member inline this.TimeoutTo<'A1> (defaultValue: 'A1) (onSuccess: 'A -> 'A1) (duration: TimeSpan) (onError: exn -> 'E) : FIO<'A1, 'E> =
        let timeoutEff =
            (FIO.sleep duration onError)
                .FlatMap <| fun () ->
                    FIO.succeed defaultValue
        this.Map(onSuccess).RaceFirst timeoutEff

    member inline this.Timed (onError: exn -> 'E) : FIO<TimeSpan * 'A, 'E> =
        (FIO.attempt Stopwatch.StartNew onError).FlatMap <| fun stopwatch ->
            this.Ensuring(FIO.suspend (fun () -> stopwatch.Stop(); FIO.unit ()))
                .Map <| fun value -> stopwatch.Elapsed, value

    member this.RaceFirst (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.suspend <| fun () ->
            let channel = Channel<bool>()

            let signal (won: bool) (fiber: Fiber<'A, 'E>) =
                fiber.Await().FlatMap <| fun result ->
                    match result with
                    | Succeeded _
                    | Failed _ -> (channel.Write won).Unit()
                    | Interrupted _ -> FIO.unit ()

            this.Fork().FlatMap <| fun fiber1 ->
                effect.Fork().FlatMap <| fun fiber2 ->
                    (signal true fiber1).Fork().FlatMap <| fun _ ->
                        (signal false fiber2).Fork().FlatMap <| fun _ ->
                            channel.Read().FlatMap <| fun winnerIsFirst ->
                                let winner, loser =
                                    if winnerIsFirst then fiber1, fiber2 else fiber2, fiber1
                                (loser.Interrupt ExplicitInterrupt "Lost race")
                                    .Ignore()
                                    .FlatMap <| fun () -> winner.Join()

    member inline this.RaceEither<'A1> (effect: FIO<'A1, 'E>) : FIO<Choice<'A, 'A1>, 'E> =
        this.Map(Choice1Of2).Race <| effect.Map Choice2Of2

    member this.Race (effect: FIO<'A, 'E>) : FIO<'A, 'E> =
        FIO.suspend <| fun () ->
            let channel = Channel<Result<'A * bool, 'E>>()

            let signal (isFirst: bool) (fiber: Fiber<'A, 'E>) =
                fiber.Await().FlatMap <| fun result ->
                    match result with
                    | Succeeded value -> (channel.Write(Ok(value, isFirst))).Unit()
                    | Failed error -> (channel.Write(Error error)).Unit()
                    | Interrupted _ -> FIO.unit ()

            this.Fork().FlatMap <| fun fiber1 ->
                effect.Fork().FlatMap <| fun fiber2 ->
                    (signal true fiber1).Fork().FlatMap <| fun _ ->
                        (signal false fiber2).Fork().FlatMap <| fun _ ->
                            channel.Read().FlatMap <| fun first ->
                                match first with
                                | Ok(value, isFirst) ->
                                    let loser = if isFirst then fiber2 else fiber1
                                    (loser.Interrupt ExplicitInterrupt "Lost race")
                                        .Ignore()
                                        .As value
                                | Error _ ->
                                    channel.Read().FlatMap <| fun second ->
                                        match second with
                                        | Ok(value, _) -> FIO.succeed value
                                        | Error error -> FIO.fail error
