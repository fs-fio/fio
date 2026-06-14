namespace FIO.DSL

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module FIO =

    let unit<'E> () : FIO<unit, 'E> =
        Success()

    let succeed<'A, 'E> (value: 'A) : FIO<'A, 'E> =
        Success value

    let fail<'A, 'E> (error: 'E) : FIO<'A, 'E> =
        Failure error

    let interrupt<'A, 'E> (cause: InterruptionCause) (message: string) : FIO<'A, 'E> =
        Interrupt(cause, message)

    let inline interruptNow<'A, 'E> () : FIO<'A, 'E> =
        interrupt ExplicitInterrupt "Fiber interrupted"

    let attempt<'A, 'E> (func: unit -> 'A) (onError: exn -> 'E) : FIO<'A, 'E> =
        FIO.Action(func, onError)

    let inline fromResult<'A, 'E> : Result<'A, 'E> -> FIO<'A, 'E> = function
        | Ok value -> succeed value
        | Error error -> fail error

    let inline fromOption<'A, 'E> (option: Option<'A>) (onNone: unit -> 'E) : FIO<'A, 'E> =
        match option with
        | Some value -> succeed value
        | None -> fail (onNone ())

    let inline fromChoice<'A, 'E> (choice: Choice<'A, 'E>) : FIO<'A, 'E> =
        match choice with
        | Choice1Of2 value -> succeed value
        | Choice2Of2 error -> fail error

    let suspend<'A, 'E> (effect: unit -> FIO<'A, 'E>) : FIO<'A, 'E> =
        Suspend effect

    let cancellationToken<'E> () : FIO<CancellationToken, 'E> =
        FiberCancellationToken

    let awaitUnitTask<'E> (task: Task) (onError: exn -> 'E) : FIO<unit, 'E> =
        AwaitTask(boxVoidTask task, onError)

    let awaitTask<'A, 'E> (task: Task<'A>) (onError: exn -> 'E) : FIO<'A, 'E> =
        AwaitTask(boxTask task, onError)

    let inline awaitAsync<'A, 'E> (async: Async<'A>) (onError: exn -> 'E) : FIO<'A, 'E> =
        cancellationToken().FlatMap <| fun cancelToken ->
            awaitTask (Async.StartAsTask(async, cancellationToken = cancelToken)) onError

    let inline forkUnitTask<'E, 'E1> (taskFactory: unit -> Task) (onError: exn -> 'E) : FIO<Fiber<unit, 'E>, 'E1> =
        suspend <| fun () ->
            let task =
                try taskFactory ()
                with ex -> Task.FromException ex
            (awaitUnitTask task onError).Fork()

    let inline forkTask<'A, 'E, 'E1> (taskFactory: unit -> Task<'A>) (onError: exn -> 'E) : FIO<Fiber<'A, 'E>, 'E1> =
        suspend <| fun () ->
            let task =
                try taskFactory ()
                with ex -> Task.FromException<'A> ex
            (awaitTask task onError).Fork()

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

    let inline sleep<'E> (duration: TimeSpan) (onError: exn -> 'E) : FIO<unit, 'E> =
        cancellationToken().FlatMap <| fun cancelToken ->
            awaitUnitTask (Task.Delay(duration, cancelToken)) onError

    let inline yieldNow<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        cancellationToken().FlatMap <| fun _ ->
            awaitUnitTask (Task.Run(fun () -> ())) onError

    type private NeverEffect<'A, 'E>() =
        static let channel: Channel<'A> = Channel<'A>()
        static let effect: FIO<'A, 'E> = channel.Read()
        static member Effect = effect

    let never<'A, 'E> () : FIO<'A, 'E> =
        NeverEffect<'A, 'E>.Effect

    let inline acquireReleaseWith<'A, 'A1, 'E>
        (acquire: FIO<'A1, 'E>)
        (release: 'A1 -> FIO<unit, 'E>)
        (useResource: 'A1 -> FIO<'A, 'E>)
        : FIO<'A, 'E> =
        acquire.FlatMap <| fun resource ->
            (useResource resource)
                .Ensuring(release resource)

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

    let inline forEachParDiscard<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<unit, 'E> =
        (forEachPar items func).Map(fun _ -> ())

    let inline collectAll<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E> =
        forEach effects id

    let inline collectAllDiscard<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<unit, 'E> =
        forEachDiscard effects id

    let inline collectAllPar<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E> =
        forEachPar effects id

    let inline collectAllParDiscard<'A, 'E> (effects: seq<FIO<'A, 'E>>) : FIO<unit, 'E> =
        forEachParDiscard effects id

    let inline replicateFIO<'A, 'E> (n: int) (effect: FIO<'A, 'E>) : FIO<'A list, 'E> =
        if n <= 0 then
            succeed []
        else
            forEach (seq { 1 .. n }) (fun _ -> effect)

    let inline replicateFIODiscard<'A, 'E> (n: int) (effect: FIO<'A, 'E>) : FIO<unit, 'E> =
        if n <= 0 then
            unit ()
        else
            forEachDiscard (seq { 1 .. n }) (fun _ -> effect)

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

    let inline loopDiscard<'S, 'A, 'E> (initial: 'S) (cont: 'S -> bool) (inc: 'S -> 'S) (body: 'S -> FIO<'A, 'E>) : FIO<unit, 'E> =
        suspend <| fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap <| fun _ ->
                        go (inc state)
                else
                    unit ()
            go initial

    let inline iterate<'S, 'E> (initial: 'S) (cont: 'S -> bool) (body: 'S -> FIO<'S, 'E>) : FIO<'S, 'E> =
        suspend <| fun () ->
            let rec go state =
                if cont state then
                    (body state).FlatMap <| fun next ->
                        go next
                else
                    succeed state
            go initial

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

    let inline mergeAllPar<'A, 'A1, 'E> (effects: seq<FIO<'A1, 'E>>) (zero: 'A) (func: 'A -> 'A1 -> 'A) : FIO<'A, 'E> =
        (collectAllPar effects).Map(List.fold func zero)

    let inline reduceAll<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        head.FlatMap <| fun head -> mergeAll tail head func

    let inline reduceAllPar<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) (func: 'A -> 'A -> 'A) : FIO<'A, 'E> =
        let all = Seq.append (Seq.singleton head) tail
        (collectAllPar all).Map(List.reduce func)

    let inline private splitResults<'A, 'E> (results: Result<'A, 'E> list) : 'E list * 'A list =
        let folder (errors, values) = function
            | Ok value -> errors, value :: values
            | Error error -> error :: errors, values
        let errors, values = List.fold folder ([], []) results
        List.rev errors, List.rev values

    let inline partition<'A, 'A1, 'E, 'E1> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'E list * 'A list, 'E1> =
        (forEach items (fun value -> (func value).Result())).Map splitResults

    let inline partitionPar<'A, 'A1, 'E, 'E1> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'E list * 'A list, 'E1> =
        (forEachPar items (fun value -> (func value).Result())).Map splitResults

    let inline validate<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E list> =
        (forEach items (fun value -> (func value).Result())).FlatMap <| fun results ->
            let errors, values = splitResults results
            if List.isEmpty errors then succeed values
            else fail errors

    let inline validatePar<'A, 'A1, 'E> (items: seq<'A1>) (func: 'A1 -> FIO<'A, 'E>) : FIO<'A list, 'E list> =
        (forEachPar items (fun value -> (func value).Result())).FlatMap <| fun results ->
            let errors, values = splitResults results
            if List.isEmpty errors then succeed values
            else fail errors

    let inline collectAllSuccesses<'A, 'E, 'E1> (effects: seq<FIO<'A, 'E>>) : FIO<'A list, 'E1> =
        (forEach effects (fun effect ->
            effect.Option())).Map(List.choose id)

    let inline ifFIO<'A, 'E> (predicate: FIO<bool, 'E>) (onTrue: FIO<'A, 'E>) (onFalse: FIO<'A, 'E>) : FIO<'A, 'E> =
        predicate.FlatMap <| fun bool ->
            if bool then onTrue
            else onFalse

    let inline someOrFail<'A, 'E> (error: 'E) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.FlatMap <| function
            | Some value -> succeed value
            | None -> fail error

    let inline someOrElse<'A, 'E> (defaultResult: 'A) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.Map(Option.defaultValue defaultResult)

    let inline someOrElseFIO<'A, 'E> (defaultEffect: FIO<'A, 'E>) (effect: FIO<'A option, 'E>) : FIO<'A, 'E> =
        effect.FlatMap <| function
            | Some value -> succeed value
            | None -> defaultEffect

    let inline firstSuccessOf<'A, 'E> (head: FIO<'A, 'E>) (tail: seq<FIO<'A, 'E>>) : FIO<'A, 'E> =
        tail |> Seq.fold (fun acc effect ->
            acc.CatchAll <| fun _ ->
                effect) head

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
