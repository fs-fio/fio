namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module FIO =

    /// Creates an effect that succeeds with unit.
    let unit<'E> () : FIO<unit, 'E> =
        Success()

    /// Creates an effect that always succeeds with the given value.
    let succeed<'A, 'E> (value: 'A) : FIO<'A, 'E> =
        Success value

    /// Creates an effect that always fails with the given error.
    let fail<'A, 'E> (error: 'E) : FIO<'A, 'E> =
        Failure error

    /// Creates an effect that interrupts the current fiber with the given cause and message.
    let interrupt<'A, 'E> (cause: InterruptionCause) (message: string) : FIO<'A, 'E> =
        Interrupt(cause, message)

    /// Creates an effect that interrupts the current fiber with an explicit-interrupt cause.
    let inline interruptNow<'A, 'E> () : FIO<'A, 'E> =
        interrupt ExplicitInterrupt "Fiber interrupted"

    /// Creates an effect that runs a side-effecting function, mapping any thrown exception to a typed error via onError.
    let attempt<'A, 'E> (func: unit -> 'A) (onError: exn -> 'E) : FIO<'A, 'E> =
        FIO.Action(func, onError)

    /// Creates an effect that succeeds or fails according to the given Result.
    let inline fromResult<'A, 'E> : Result<'A, 'E> -> FIO<'A, 'E> = function
        | Ok value -> succeed value
        | Error error -> fail error

    /// Creates an effect that succeeds with the option's value, or fails using the given function when it is None.
    let inline fromOption<'A, 'E> (option: Option<'A>) (onNone: unit -> 'E) : FIO<'A, 'E> =
        match option with
        | Some value -> succeed value
        | None -> fail (onNone ())

    /// Creates an effect that succeeds or fails according to the given Choice.
    let inline fromChoice<'A, 'E> (choice: Choice<'A, 'E>) : FIO<'A, 'E> =
        match choice with
        | Choice1Of2 value -> succeed value
        | Choice2Of2 error -> fail error

    /// Defers construction of an effect until it is run.
    let suspend<'A, 'E> (effect: unit -> FIO<'A, 'E>) : FIO<'A, 'E> =
        Suspend effect

    /// Creates an effect that yields the current fiber's cancellation token.
    let cancellationToken<'E> () : FIO<CancellationToken, 'E> =
        FiberCancellationToken

    /// Creates an effect that awaits a non-generic task, mapping any exception to a typed error.
    let awaitUnitTask<'E> (task: Task) (onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTask(boxVoidTask task, onError)

    /// Creates an effect that awaits a task and yields its result, mapping any exception to a typed error.
    let awaitTask<'A, 'E> (task: Task<'A>) (onError: exn -> 'E) : FIO<'A, 'E> =
        AwaitTask(boxTask task, onError)

    /// Creates an effect that runs an F# async and yields its result, mapping any exception to a typed error.
    let inline awaitAsync<'A, 'E> (async: Async<'A>) (onError: exn -> 'E) : FIO<'A, 'E> =
        cancellationToken().FlatMap <| fun cancelToken ->
            awaitTask (Async.StartAsTask(async, cancellationToken = cancelToken)) onError

    /// Creates an effect that starts a non-generic task on a new fiber, mapping any exception to a typed error.
    let inline forkUnitTask<'E, 'E1> (taskFactory: unit -> Task) (onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E1> =
        suspend <| fun () ->
            let task =
                try taskFactory ()
                with ex -> Task.FromException ex
            (awaitUnitTask task onError).Fork()

    /// Creates an effect that starts a task on a new fiber, mapping any exception to a typed error.
    let inline forkTask<'A, 'E, 'E1> (taskFactory: unit -> Task<'A>) (onError: exn -> 'E) : FIO<Fiber<'A, 'E>, 'E1> =
        suspend <| fun () ->
            let task =
                try taskFactory ()
                with ex -> Task.FromException<'A> ex
            (awaitTask task onError).Fork()

    /// Creates an effect from a callback-based asynchronous operation, completed by invoking the registered callback.
    let inline async<'A, 'E> (register: (Result<'A, 'E> -> unit) -> unit) (onError: exn -> 'E) : FIO<'A, 'E> =
        cancellationToken().FlatMap <| fun cancelToken ->
            let resultSource = TaskCompletionSource<Result<'A, 'E>>()
            let registration = cancelToken.Register(fun () ->
                resultSource.TrySetCanceled cancelToken |> ignore)
            try
                register <| fun result ->
                    if resultSource.TrySetResult result then
                        registration.Dispose()
            with ex ->
                resultSource.TrySetException ex |> ignore
                registration.Dispose()
            (awaitTask resultSource.Task onError).FlatMap fromResult

    /// Creates an effect that suspends the current fiber for the given duration.
    let inline sleep<'E> (duration: TimeSpan) (onError: exn -> 'E) : FIO<unit, 'E> =
        cancellationToken().FlatMap <| fun cancelToken ->
            awaitUnitTask (Task.Delay(duration, cancelToken)) onError

    /// Creates an effect that yields control, letting other fibers run before continuing.
    let inline yieldNow<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        cancellationToken().FlatMap <| fun _ ->
            awaitUnitTask (Task.Run(fun () -> ())) onError

    type private NeverEffect<'A, 'E>() =
        static let channel: Channel<'A> = Channel<'A>()
        static let effect: FIO<'A, 'E> = channel.Read()
        static member Effect = effect

    /// An effect that never completes.
    let never<'A, 'E> () : FIO<'A, 'E> =
        NeverEffect<'A, 'E>.Effect

    /// Acquires a resource, uses it, and releases it afterwards on success, failure, or interruption.
    let inline acquireReleaseWith<'A, 'A1, 'E>
        (acquire: FIO<'A1, 'E>)
        (release: 'A1 -> FIO<unit, 'E>)
        (useResource: 'A1 -> FIO<'A, 'E>)
        : FIO<'A, 'E> =
        acquire.FlatMap <| fun resource ->
            (useResource resource)
                .Ensuring(release resource)

    /// Runs the given effect for each item in sequence, collecting the results into a list.
    let inline forEach<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E> =
        suspend <| fun () ->
            let arr = Seq.toArray items
            let results = ResizeArray<'A> arr.Length
            let rec loop i =
                if i >= arr.Length then
                    succeed (List.ofSeq results)
                else
                    (func arr[i]).FlatMap <| fun value ->
                        results.Add value
                        loop (i + 1)
            loop 0

    /// Runs the given effect for each item in sequence, discarding the results.
    let inline forEachDiscard<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<unit, 'E> =
        suspend <| fun () ->
            let arr = Seq.toArray items
            let rec loop i =
                if i >= arr.Length then
                    unit ()
                else
                    (func arr[i]).FlatMap <| fun _ ->
                        loop (i + 1)
            loop 0

    /// Runs the given effect for each item concurrently, collecting the results into a list.
    let inline forEachPar<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E> =
        suspend <| fun () ->
            let arr = Seq.toArray items
            let forked = ResizeArray<Fiber<'A, 'E>> arr.Length
            let results = ResizeArray<'A> arr.Length

            let rec forkAll i : FIO<Fiber<'A, 'E> list, 'E> =
                if i >= arr.Length then
                    succeed (List.ofSeq forked)
                else
                    (func arr[i]).Fork().FlatMap <| fun fiber ->
                        forked.Add fiber
                        forkAll (i + 1)

            let inline interruptOne (fiber: Fiber<'A, 'E>) : FIO<unit, 'E> =
                (fiber.Interrupt ExplicitInterrupt "forEachPar peer failed")
                    .CatchAll(fun _ -> unit ())

            let rec interruptAll (fibers: Fiber<'A, 'E> list) : FIO<unit, 'E> =
                match fibers with
                | [] -> unit ()
                | fiber :: rest ->
                    (interruptOne fiber).FlatMap <| fun () ->
                        interruptAll rest

            let rec joinAll (fibers: Fiber<'A, 'E> list) : FIO<'A list, 'E> =
                match fibers with
                | [] -> succeed (List.ofSeq results)
                | fiber :: rest ->
                    (fiber.Join().FlatMap <| fun value ->
                        results.Add value
                        joinAll rest).CatchAll(fun error ->
                            (interruptAll rest).FlatMap <| fun () ->
                                fail error)

            (forkAll 0).FlatMap <| fun fibers ->
                joinAll fibers

    /// Runs the given effect for each item concurrently, discarding the results.
    let inline forEachParDiscard<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<unit, 'E> =
        (forEachPar items func).Map(fun _ -> ())

    /// Runs the given effects in sequence, collecting their results into a list.
    let inline collectAll<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E> =
        forEach effects id

    /// Runs the given effects in sequence, discarding their results.
    let inline collectAllDiscard<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<unit, 'E> =
        forEachDiscard effects id

    /// Runs the given effects concurrently, collecting their results into a list.
    let inline collectAllPar<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E> =
        forEachPar effects id

    /// Runs the given effects concurrently, discarding their results.
    let inline collectAllParDiscard<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<unit, 'E> =
        forEachParDiscard effects id

    /// Runs the given effect n times in sequence, collecting the results into a list.
    let inline replicateFIO<'A, 'E> (n: int) (effect: FIO<'A, 'E>) : FIO<'A list, 'E> =
        if n <= 0 then
            succeed []
        else
            forEach (seq { 1 .. n }) (fun _ -> effect)

    /// Runs the given effect n times in sequence, discarding the results.
    let inline replicateFIODiscard<'A, 'E> (n: int) (effect: FIO<'A, 'E>) : FIO<unit, 'E> =
        if n <= 0 then
            unit ()
        else
            forEachDiscard (seq { 1 .. n }) (fun _ -> effect)

    /// Repeatedly runs the body while the condition holds, stepping the state each time and collecting the results.
    let inline loop<'S, 'A, 'E> (initial: 'S) (cont: 'S -> bool) (inc: 'S -> 'S) (body: 'S -> FIO<'A, 'E>) : FIO<'A list, 'E> =
        suspend <| fun () ->
            let results = ResizeArray<'A>()
            let rec go state =
                if cont state then
                    (body state).FlatMap <| fun value ->
                        results.Add value
                        go (inc state)
                else
                    succeed (List.ofSeq results)
            go initial

    /// Repeatedly runs the body while the condition holds, stepping the state each time and discarding the results.
    let inline loopDiscard<'S, 'A, 'E> (initial: 'S) (cont: 'S -> bool) (inc: 'S -> 'S) (body: 'S -> FIO<'A, 'E>) : FIO<unit, 'E> =
        suspend <| fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap <| fun _ ->
                        go (inc state)
                else
                    unit ()
            go initial

    /// Repeatedly runs the body while the condition holds, threading the state through each step and yielding the final state.
    let inline iterate<'S, 'E> (initial: 'S) (cont: 'S -> bool) (body: 'S -> FIO<'S, 'E>) : FIO<'S, 'E> =
        suspend <| fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap <| fun next ->
                        go next
                else
                    succeed state
            go initial

    /// Runs the given effects in sequence, folding their results into a single value.
    let inline mergeAll<'A, 'A1, 'E> (effects: seq<FIO<'A1, 'E>>) (zero: 'A) (func: 'A -> 'A1 -> 'A) : FIO<'A, 'E> =
        suspend <| fun () ->
            let arr = Seq.toArray effects
            let rec go i acc =
                if i >= arr.Length then
                    succeed acc
                else
                    arr[i].FlatMap <| fun x ->
                        go (i + 1) (func acc x)
            go 0 zero

    /// Runs the given effects concurrently, folding their results into a single value.
    let inline mergeAllPar<'A, 'A1, 'E> (effects: seq<FIO<'A1, 'E>>) (zero: 'A) (func: 'A -> 'A1 -> 'A) : FIO<'A, 'E> =
        (collectAllPar effects).Map(List.fold func zero)

    /// Runs the given effects in sequence, reducing their results into a single value.
    let inline reduceAll<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        head.FlatMap <| fun head -> mergeAll tail head func

    /// Runs the given effects concurrently, reducing their results into a single value.
    let inline reduceAllPar<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        let all = Seq.append (Seq.singleton head) tail
        (collectAllPar all).Map(List.reduce func)

    let inline private splitResults<'A, 'E> (results: Result<'A, 'E> list) : 'E list * 'A list =
        let folder (errors, values) = function
            | Ok value -> errors, value :: values
            | Error error -> error :: errors, values
        let errors, values = List.fold folder ([], []) results
        List.rev errors, List.rev values

    /// Runs the given effect for each item in sequence, partitioning the outcomes into errors and successes.
    let inline partition<'A, 'A1, 'E, 'E1> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'E list * 'A list, 'E1> =
        (forEach items (fun value -> (func value).Result())).Map splitResults

    /// Runs the given effect for each item concurrently, partitioning the outcomes into errors and successes.
    let inline partitionPar<'A, 'A1, 'E, 'E1> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'E list * 'A list, 'E1> =
        (forEachPar items (fun value -> (func value).Result())).Map splitResults

    /// Runs the given effect for each item in sequence, succeeding with all values or failing with every collected error.
    let inline validate<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E list> =
        (forEach items (fun value -> (func value).Result())).FlatMap <| fun results ->
            let errors, values = splitResults results
            if List.isEmpty errors then succeed values
            else fail errors

    /// Runs the given effect for each item concurrently, succeeding with all values or failing with every collected error.
    let inline validatePar<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E list> =
        (forEachPar items (fun value -> (func value).Result())).FlatMap <| fun results ->
            let errors, values = splitResults results
            if List.isEmpty errors then succeed values
            else fail errors

    /// Runs the given effects in sequence, keeping only the successful results.
    let inline collectAllSuccesses<'A, 'E, 'E1> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E1> =
        (forEach effects (fun effect ->
            effect.Option())).Map(List.choose id)

    /// Runs onTrue or onFalse depending on the result of the predicate effect.
    let inline ifFIO<'A, 'E> (predicate: FIO<bool, 'E>) (onTrue: FIO<'A, 'E>) (onFalse: FIO<'A, 'E>) : FIO<'A, 'E> =
        predicate.FlatMap <| fun bool ->
            if bool then onTrue
            else onFalse

    /// Unwraps an optional success, failing with the given error when it is None.
    let inline someOrFail<'A, 'E> (error: 'E) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.FlatMap <| function
            | Some value -> succeed value
            | None -> fail error

    /// Unwraps an optional success, falling back to the given value when it is None.
    let inline someOrElse<'A, 'E> (defaultResult: 'A) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.Map(Option.defaultValue defaultResult)

    /// Unwraps an optional success, falling back to the given effect when it is None.
    let inline someOrElseFIO<'A, 'E> (defaultEffect: FIO<'A, 'E>) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.FlatMap <| function
            | Some value -> succeed value
            | None -> defaultEffect

    /// Returns the first effect to succeed, trying each in turn.
    let inline firstSuccessOf<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) : FIO<'A, 'E> =
        tail |> Seq.fold (fun acc effect ->
            acc.CatchAll <| fun _ ->
                effect) head

    /// Runs the given effects concurrently and returns the first to succeed, interrupting the rest.
    let inline raceAll<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<'A, 'E> =
        suspend <| fun () ->
            let arr = Seq.toArray effects

            if arr.Length = 0 then
                interrupt (InvalidArgument("effects", "sequence must not be empty")) "Cannot race an empty sequence"

            elif arr.Length = 1 then
                arr[0]
            else
                let resultChannel = Channel<Result<'A * int, 'E>>()

                let signal (idx: int) (fiber: Fiber<'A, 'E>) : FIO<unit, 'E> =
                    fiber.Await().FlatMap <| fun result ->
                        match result with
                        | Succeeded value ->
                            resultChannel.Write(Ok(value, idx)).FlatMap <| fun _ -> unit ()
                        | Failed error ->
                            resultChannel.Write(Error error).FlatMap <| fun _ -> unit ()
                        | Interrupted _ -> unit ()

                let interruptOthers (fiberArr: Fiber<'A, 'E> array) (winnerIdx: int) : FIO<unit, 'E> =
                    let rec loop i =
                        if i >= fiberArr.Length then
                            unit ()
                        elif i = winnerIdx then
                            loop (i + 1)
                        else
                            (fiberArr[i].Interrupt ExplicitInterrupt "Lost race")
                                .CatchAll(fun _ -> unit ())
                                .FlatMap <| fun () -> loop (i + 1)
                    loop 0

                let rec receive (received: int) (fiberArr: Fiber<'A, 'E> array) : FIO<'A, 'E> =
                    resultChannel.Read().FlatMap <| fun result ->
                        match result with
                        | Ok(value, winnerIdx) ->
                            (interruptOthers fiberArr winnerIdx).FlatMap <| fun () -> succeed value
                        | Error error ->
                            if received + 1 >= arr.Length then
                                fail error
                            else
                                receive (received + 1) fiberArr

                let forkAllEff = collectAll (arr |> Array.map _.Fork())

                forkAllEff.FlatMap <| fun fibers ->
                    let fiberArr = List.toArray fibers
                    let startSignalsEff =
                        fibers
                        |> List.mapi (fun i fiber -> (signal i fiber).Fork().Map(fun _ -> ()))
                        |> collectAllDiscard
                    startSignalsEff.FlatMap <| fun () -> receive 0 fiberArr
