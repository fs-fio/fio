module FIO.Tests.ConformanceTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Polling

open Expecto

open System
open System.Threading
open System.Threading.Tasks

let private expectDefect (runtime: FIORuntime) (label: string) (effect: FIO<int, string>) =
    let name = runtime.GetType().Name

    let result =
        try
            runtime.Run(effect).UnsafeResult()
        with :? InvalidCastException as ex ->
            failtest
                $"{name}: {label} put a non-'E value in the error channel — observing it raised {ex.GetType().Name}"

    match result with
    | Interrupted ex ->
        match ex.cause with
        | Defect _ -> ()
        | other -> failtest $"{name}: {label} expected a Defect cause but got {other}"
    | other -> failtest $"{name}: {label} expected Interrupted but got {other}"

[<Tests>]
let conformanceTests =
    testList
        "Runtime conformance"
        [
            testList
                "Defect paths at 'E <> exn"
                [
                    testAllRuntimes "Defect - throwing Suspend thunk"
                    <| fun runtime ->
                        expectDefect runtime "Suspend" (FIO.suspend (fun () -> failwith "suspend threw"))

                    testAllRuntimes "Defect - throwing FlatMap continuation"
                    <| fun runtime ->
                        expectDefect runtime "FlatMap" (FIO.succeed(42).FlatMap(fun (_: int) -> failwith "continuation threw"))

                    testAllRuntimes "Defect - throwing CatchAll handler"
                    <| fun runtime ->
                        expectDefect runtime "CatchAll" (FIO.fail("typed").CatchAll(fun (_: string) -> failwith "handler threw"))

                    testAllRuntimes "Defect - throwing onError on attempt"
                    <| fun runtime ->
                        expectDefect
                            runtime
                            "attempt onError"
                            (FIO.attempt (fun () -> raise (InvalidOperationException "boom")) (fun _ -> failwith "onError threw"))

                    testAllRuntimes "Defect - throwing onError on awaitTask"
                    <| fun runtime ->
                        let faulting = Task.FromException<int>(Exception "task boom")
                        expectDefect runtime "awaitTask onError" (FIO.awaitTask faulting (fun _ -> failwith "onError threw"))
                ]

            testList
                "The typed error channel stays typed"
                [
                    testAllRuntimes "Typed error - a genuine failure surfaces as Failed, not Interrupted"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.fail "typed failure"

                        match runtime.Run(effect).UnsafeResult() with
                        | Failed error -> Expect.equal error "typed failure" "The typed error must survive intact"
                        | other -> failtest $"{runtime.GetType().Name}: expected Failed but got {other}"

                    testAllRuntimes "Typed error - success surfaces as Succeeded"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.succeed 42

                        match runtime.Run(effect).UnsafeResult() with
                        | Succeeded value -> Expect.equal value 42 "The success value must survive intact"
                        | other -> failtest $"{runtime.GetType().Name}: expected Succeeded but got {other}"
                ]

            testList
                "Await / Poll / UnsafeResult agree"
                [
                    testAllRuntimes "Observation - Await and UnsafeResult agree on a defected fiber"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.suspend (fun () -> failwith "boom")

                        let viaAwait =
                            let observer: FIO<FiberResult<int, string>, string> =
                                effect.Fork().FlatMap(fun fiber -> fiber.Await())
                            runtime.Run(observer).UnsafeSuccess()

                        let viaResult = runtime.Run(effect).UnsafeResult()

                        match viaAwait, viaResult with
                        | Interrupted a, Interrupted b ->
                            Expect.equal (a.cause.ToString()) (b.cause.ToString()) "Await and UnsafeResult must report the same cause"
                        | a, b -> failtest $"{runtime.GetType().Name}: Await gave {a} but UnsafeResult gave {b}"
                ]

            testList
                "Finalizers may use the fiber's cancellation token"
                [
                    let interruptAndAwaitFinalizer (runtime: FIORuntime) (finalizer: bool ref -> FIO<unit, string>) =
                        let finished = ref false

                        let child: FIO<unit, string> =
                            (FIO.never ()).Ensuring(finalizer finished)

                        let effect: FIO<unit, string> =
                            child.ForkDaemon().FlatMap(fun (fiber: Fiber<unit, string>) ->
                                (FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                    .FlatMap(fun () -> fiber.InterruptNow())
                                    .FlatMap(fun () -> (fiber.Await()).Unit()))

                        runtime.Run(effect).UnsafeSuccess()
                        waitForFlag finished

                    testAllRuntimes "Finalizer - FIO.sleep runs to completion after interruption"
                    <| fun runtime ->
                        Expect.isTrue
                            (interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.sleep (TimeSpan.FromMilliseconds 150.0))
                                    .FlatMap(fun () -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message))))
                            $"{runtime.GetType().Name}: a finalizer must be able to sleep after its fiber was interrupted"

                    testAllRuntimes "Finalizer - FIO.async runs to completion after interruption"
                    <| fun runtime ->
                        Expect.isTrue
                            (interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.async
                                    (fun complete -> complete (Ok 1))
                                    (fun ex -> ex.Message): FIO<int, string>)
                                    .FlatMap(fun _ -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message))))
                            $"{runtime.GetType().Name}: a finalizer must be able to use FIO.async after interruption"

                    testAllRuntimes "Finalizer - FIO.awaitAsync runs to completion after interruption"
                    <| fun runtime ->
                        Expect.isTrue
                            (interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.awaitAsync (async { return 1 }) (fun ex -> ex.Message): FIO<int, string>)
                                    .FlatMap(fun _ -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message))))
                            $"{runtime.GetType().Name}: a finalizer must be able to use FIO.awaitAsync after interruption"
                ]

            testList
                "Structured concurrency"
                [
                    testAllRuntimes "Fork - a parent does not publish its result until its children have unwound"
                    <| fun runtime ->
                        let finalized = ref false

                        let slowFinalizer: FIO<unit, string> =
                            (FIO.sleep (TimeSpan.FromMilliseconds 200.0))
                                .FlatMap(fun () -> FIO.attempt (fun () -> finalized.Value <- true) (fun ex -> ex.Message))

                        let child: FIO<unit, string> =
                            (FIO.never ()).Ensuring slowFinalizer

                        let parent: FIO<unit, string> =
                            child.Fork().FlatMap(fun (_: Fiber<unit, string>) -> FIO.unit ())

                        runtime.Run(parent).UnsafeSuccess()

                        Expect.isTrue
                            finalized.Value
                            $"{runtime.GetType().Name}: the child's finalizer must have completed before the parent's result became observable"

                    testAllRuntimes "ForkDaemon - a daemon child survives its parent completing"
                    <| fun runtime ->
                        let started = new ManualResetEventSlim false

                        let child: FIO<unit, string> =
                            (FIO.attempt (fun () -> started.Set()) (fun ex -> ex.Message))
                                .FlatMap(fun () -> FIO.never ())

                        let parent: FIO<obj, string> =
                            child.ForkDaemon().FlatMap(fun (fiber: Fiber<unit, string>) ->
                                (FIO.attempt (fun () -> started.Wait(TimeSpan.FromSeconds 5.0) |> ignore) (fun ex -> ex.Message))
                                    .FlatMap(fun () -> FIO.succeed (fiber :> obj)))

                        let daemon = runtime.Run(parent).UnsafeSuccess() :?> Fiber<unit, string>

                        Expect.isFalse
                            (daemon.IsTerminal())
                            $"{runtime.GetType().Name}: a daemon fiber must outlive its parent"

                        runtime.Run(daemon.InterruptNow()).UnsafeSuccess()
                        started.Dispose()

                    testAllRuntimes "Fork - re-running one effect value forks again and yields fresh results"
                    <| fun runtime ->
                        let runs = 3
                        let forkCount = ref 0

                        let effect: FIO<int, string> =
                            (FIO.suspend (fun () ->
                                Interlocked.Increment forkCount |> ignore
                                FIO.succeed 1))
                                .Fork()
                                .FlatMap(fun (fiber: Fiber<int, string>) -> fiber.Join())

                        let bounded: FIO<int, string> =
                            effect.TimeoutFail "re-run blocked: the fork was skipped" (TimeSpan.FromSeconds 10.0)

                        let results = [ for _ in 1..runs -> runtime.Run(bounded).UnsafeSuccess() ]

                        Expect.equal
                            results
                            (List.replicate runs 1)
                            $"{runtime.GetType().Name}: every run of a forked effect must produce its own result"

                        Expect.equal
                            forkCount.Value
                            runs
                            $"{runtime.GetType().Name}: the forked child must actually run once per Run, not be skipped after the first"

                    testAllRuntimes "Fork - a re-run effect observes state written by that run only"
                    <| fun runtime ->
                        let effect: FIO<int, string> =
                            FIO.suspend (fun () ->

                                let cell = ref 0
                                (FIO.attempt (fun () -> Interlocked.Increment cell) (fun ex -> ex.Message))
                                    .Fork()
                                    .FlatMap(fun (fiber: Fiber<int, string>) -> fiber.Join()))

                        let bounded: FIO<int, string> =
                            effect.TimeoutFail "re-run blocked: the fork was skipped" (TimeSpan.FromSeconds 10.0)

                        for attempt in 1..3 do
                            Expect.equal
                                (runtime.Run(bounded).UnsafeSuccess())
                                1
                                $"{runtime.GetType().Name}: run {attempt} must see its own state, not a cached earlier result"

                    testAllRuntimes "ForkDaemon - re-running one effect value forks a fresh daemon each time"
                    <| fun runtime ->
                        let started = ref 0

                        let effect: FIO<unit, string> =
                            (FIO.attempt (fun () -> Interlocked.Increment started |> ignore) (fun ex -> ex.Message))
                                .ForkDaemon()
                                .FlatMap(fun (_: Fiber<unit, string>) -> FIO.unit ())

                        let bounded: FIO<unit, string> =
                            (effect.Timeout (TimeSpan.FromSeconds 10.0)).Unit()

                        for _ in 1..3 do
                            runtime.Run(bounded).UnsafeSuccess()

                        let deadline = DateTime.UtcNow.AddSeconds 5.0
                        while started.Value < 3 && DateTime.UtcNow < deadline do
                            Thread.Sleep 1

                        Expect.equal
                            started.Value
                            3
                            $"{runtime.GetType().Name}: each Run must fork a fresh daemon child"
                ]

            testList
                "Interruption cause survives the wake-up path"
                [
                    testAllRuntimes "Interrupt - a custom cause survives interruption while blocked on a channel"
                    <| fun runtime ->
                        let causes: InterruptionCause list =
                            [ ResourceExhaustion "out of memory"
                              InvalidArgument("count", "must be positive")
                              ParentInterrupted(Guid.NewGuid())
                              Defect(exn "boom")
                              ExplicitInterrupt ]

                        for expected in causes do
                            let effect: FIO<FiberResult<int, string>, string> =
                                fio {
                                    let! fiber = FIO.never<int, string>().Fork()
                                    return! fiber.InterruptAwait expected "cause under test"
                                }

                            match runtime.Run(effect).UnsafeSuccess() with
                            | Interrupted ex ->
                                Expect.equal
                                    ex.cause
                                    expected
                                    $"{runtime.GetType().Name}: the cause supplied to InterruptAwait must survive the wake-up"
                            | other ->
                                failtest $"{runtime.GetType().Name}: expected Interrupted but got {other}"

                    testAllRuntimes "Interrupt - a custom cause survives interruption while blocked on a join"
                    <| fun runtime ->
                        let expected = ResourceExhaustion "join under pressure"

                        let effect: FIO<FiberResult<int, string>, string> =
                            fio {
                                let! blocker = FIO.never<int, string>().Fork()
                                let! waiter = (blocker.Join()).Fork()
                                return! waiter.InterruptAwait expected "cause under test"
                            }

                        match runtime.Run(effect).UnsafeSuccess() with
                        | Interrupted ex ->
                            Expect.equal
                                ex.cause
                                expected
                                $"{runtime.GetType().Name}: the cause must survive a join wake-up too"
                        | other ->
                            failtest $"{runtime.GetType().Name}: expected Interrupted but got {other}"
                ]

            testList
                "Run is a scheduler, not a lifecycle"
                [
                    testAllRuntimes "Run - schedules an effect and returns its value"
                    <| fun runtime ->
                        match runtime.Run(FIO.succeed 7: FIO<int, string>).UnsafeResult() with
                        | Succeeded value -> Expect.equal value 7 "Run must produce the effect's value"
                        | other -> failtest $"{runtime.GetType().Name}: expected Succeeded but got {other}"

                    testAllRuntimes "Run - orphaned children still run their finalizers after a later Run"
                    <| fun runtime ->
                        let childCount = 400
                        let finalized = ref 0

                        let child: FIO<unit, string> =
                            FIO.succeed()
                                .Unit()
                                .Ensuring(
                                    FIO.attempt
                                        (fun () -> Interlocked.Increment finalized |> ignore)
                                        (fun ex -> ex.Message))

                        let parent: FIO<unit, string> =
                            FIO.forEach [ 1..childCount ] (fun _ -> child.Fork()) |> fun forked -> forked.Unit()

                        runtime.Run(parent).UnsafeSuccess() |> ignore
                        runtime.Run(FIO.succeed 1: FIO<int, string>).UnsafeSuccess() |> ignore

                        let deadline = DateTime.UtcNow.AddSeconds 5.0
                        while finalized.Value < childCount && DateTime.UtcNow < deadline do
                            Thread.Sleep 1

                        Expect.equal
                            finalized.Value
                            childCount
                            $"{runtime.GetType().Name}: every orphaned child must run its finalizer, none may be discarded"

                    testAllRuntimes "Run - a channel-blocked orphan still runs its finalizer after a later Run"
                    <| fun runtime ->
                        let finalized = ref false
                        let channel = Channel<int>()

                        let child: FIO<unit, string> =
                            channel
                                .Read()
                                .Unit()
                                .Ensuring(FIO.attempt (fun () -> finalized.Value <- true) (fun ex -> ex.Message))

                        let parent: FIO<unit, string> =
                            child.Fork().FlatMap(fun (_: Fiber<unit, string>) -> FIO.unit ())

                        runtime.Run(parent).UnsafeSuccess() |> ignore
                        runtime.Run(FIO.succeed 1: FIO<int, string>).UnsafeSuccess() |> ignore

                        runtime.Run(channel.Write(1).Unit(): FIO<unit, string>).UnsafeSuccess() |> ignore

                        Expect.isTrue
                            (waitForFlag finalized)
                            $"{runtime.GetType().Name}: a channel-blocked orphan must survive a later Run"

                    testAllRuntimes "Run - does not interrupt a previously started fiber"
                    <| fun runtime ->
                        let slow: FIO<int, string> =
                            (FIO.sleep (TimeSpan.FromMilliseconds 300.0))
                                .FlatMap(fun () -> FIO.succeed 1)

                        let first = runtime.Run slow
                        let second = runtime.Run (FIO.succeed 2: FIO<int, string>)

                        match second.UnsafeResult() with
                        | Succeeded value -> Expect.equal value 2 "The second fiber must complete"
                        | other -> failtest $"{runtime.GetType().Name}: second fiber expected Succeeded but got {other}"

                        match first.UnsafeResult() with
                        | Succeeded value -> Expect.equal value 1 "The first fiber must survive a later Run"
                        | other -> failtest $"{runtime.GetType().Name}: first fiber expected Succeeded but got {other}"

                    testAllRuntimes "Run - returns without waiting for the effect to finish"
                    <| fun runtime ->
                        let slow: FIO<int, string> =
                            (FIO.sleep (TimeSpan.FromMilliseconds 500.0))
                                .FlatMap(fun () -> FIO.succeed 1)

                        let clock = Diagnostics.Stopwatch.StartNew()
                        let fiber = runtime.Run slow
                        clock.Stop()

                        Expect.isLessThan
                            clock.ElapsedMilliseconds
                            250L
                            $"{runtime.GetType().Name}: Run must schedule and return, not run the effect on the caller's thread"

                        fiber.UnsafeResult() |> ignore

                    testSequenced (
                        stressTestCase "PollingRuntime - an idle blocked fiber does not burn CPU"
                        <| fun () ->
                            let lowestOf n =
                                List.init n (fun _ ->
                                    let before = Diagnostics.Process.GetCurrentProcess().TotalProcessorTime
                                    Thread.Sleep 400
                                    (Diagnostics.Process.GetCurrentProcess().TotalProcessorTime - before).TotalSeconds)
                                |> List.min

                            use runtime = new PollingRuntime(testConfig)
                            let channel = Channel<int>()

                            let ambient = lowestOf 3

                            runtime.Run(channel.Read().Unit(): FIO<unit, string>) |> ignore
                            Thread.Sleep 300

                            let added = lowestOf 3 - ambient

                            Expect.isLessThan
                                added
                                0.4
                                $"A blocked fiber must not keep a PollingRuntime worker spinning (added {added:F2} CPU-seconds/s over {ambient:F2} ambient)")

                    testAllRuntimes "Run - several fibers started at once all complete"
                    <| fun runtime ->
                        let fibers = [ for i in 1..8 -> runtime.Run(FIO.succeed i: FIO<int, string>) ]

                        let results =
                            fibers
                            |> List.map (fun fiber ->
                                match fiber.UnsafeResult() with
                                | Succeeded value -> value
                                | other -> failtest $"{runtime.GetType().Name}: expected Succeeded but got {other}")

                        Expect.equal (List.sort results) [ 1..8 ] "Every concurrently started fiber must complete with its own value"
                ]
        ]
