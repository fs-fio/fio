module FIO.Tests.ConformanceTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks
open System.Collections.Concurrent

let private testFreshRuntimes name (test: FIORuntime -> unit) =
    testList
        name
        [
            for runtimeName, make in
                [
                    "DirectRuntime", (fun () -> new DirectRuntime() :> FIORuntime)
                    "PollingRuntime", (fun () -> new PollingRuntime(testConfig) :> FIORuntime)
                    "SignalingRuntime", (fun () -> new SignalingRuntime(testConfig) :> FIORuntime)
                    "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(testConfig) :> FIORuntime)
                ] ->
                testCase runtimeName (fun () -> test (make ()))
        ]

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

                        let child =
                            (FIO.never ()).Ensuring(finalizer finished)

                        let effect =
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

                        let slowFinalizer =
                            (FIO.sleep (TimeSpan.FromMilliseconds 200.0))
                                .FlatMap(fun () -> FIO.attempt (fun () -> finalized.Value <- true) (fun ex -> ex.Message))

                        let child =
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

                        let child =
                            (FIO.attempt (fun () -> started.Set()) (fun ex -> ex.Message))
                                .FlatMap(fun () -> FIO.never ())

                        let parent =
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

                        let effect =
                            (FIO.suspend (fun () ->
                                Interlocked.Increment forkCount |> ignore
                                FIO.succeed 1))
                                .Fork()
                                .FlatMap(fun (fiber: Fiber<int, string>) -> fiber.Join())

                        let bounded =
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
                        let effect =
                            FIO.suspend (fun () ->

                                let cell = ref 0
                                (FIO.attempt (fun () -> Interlocked.Increment cell) (fun ex -> ex.Message))
                                    .Fork()
                                    .FlatMap(fun (fiber: Fiber<int, string>) -> fiber.Join()))

                        let bounded =
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
                        let causes =
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

                        let child =
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

                        let child =
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

                    testAllRuntimes "Run - a later Run leaves an existing fiber alone; the caller interrupts it"
                    <| fun runtime ->
                        let childStarted = new ManualResetEventSlim false

                        let childEffect: FIO<unit, string> =
                            fio {
                                do! FIO.attempt (fun () -> childStarted.Set()) (fun ex -> ex.Message)
                                return! FIO.never ()
                            }

                        let parentEffect: FIO<obj, string> =
                            fio {
                                let! fiber = childEffect.ForkDaemon()

                                do!
                                    FIO.attempt
                                        (fun () -> childStarted.Wait(TimeSpan.FromSeconds 5.0) |> ignore)
                                        (fun ex -> ex.Message)

                                return fiber :> obj
                            }

                        let fiber1 = runtime.Run parentEffect
                        let childFiber = fiber1.UnsafeSuccess() :?> Fiber<unit, string>

                        let fiber2 = runtime.Run(FIO.succeed 99: FIO<int, string>)
                        Expect.equal (fiber2.UnsafeSuccess()) 99 "The second fiber must complete"

                        Expect.isFalse
                            (childFiber.IsTerminal())
                            $"{runtime.GetType().Name}: a later Run must leave a fiber that is already running untouched"

                        runtime.Run(childFiber.InterruptNow()).UnsafeSuccess()

                        match childFiber.UnsafeResult() with
                        | Interrupted _ -> ()
                        | other -> failtest $"{runtime.GetType().Name}: expected the child fiber to be Interrupted once asked, got {other}"

                        childStarted.Dispose()

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

            testList
                "Runtime disposal"
                [
                    testFreshRuntimes "Dispose - interrupts a running fiber and returns after its finalizer ran" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let finalized = ref false

                        let effect : FIO<unit, string> =
                            FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())

                        let fiber = runtime.Run(effect.Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true)))
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"
                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue finalized.Value "Dispose should return only once the fiber's finalizer has run"

                        match fiber.UnsafeResult() with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testFreshRuntimes "Dispose - interrupts daemon fibers and runs their finalizers" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let finalized = ref false

                        let daemon : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true))

                        runtime.Run(daemon.ForkDaemon<string>()).UnsafeResult() |> ignore
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The daemon should start"
                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue finalized.Value "Dispose should interrupt the daemon and wait for its finalizer")

                    testFreshRuntimes "Dispose - waits for scoped children to unwind" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let childFinalized = ref false

                        let child : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.sleep(TimeSpan.FromMilliseconds 50.0).FlatMap(fun () ->
                                    FIO.succeedWith (fun () -> childFinalized.Value <- true)))

                        runtime.Run(child.Fork().FlatMap(fun fiber -> fiber.Join())) |> ignore
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The child should start"
                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue childFinalized.Value "Dispose should wait for a scoped child's finalizer too")

                    testFreshRuntimes "Dispose - a fiber parked on a task completes as interrupted" (fun runtime ->
                        let gate = TaskCompletionSource<int>()
                        let parked = runtime.Run(FIO.awaitTask gate.Task (fun ex -> ex.Message))
                        Thread.Sleep 50
                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue (parked.Task().Wait(TimeSpan.FromSeconds 5.0)) "The parked fiber should complete"

                        match parked.Task().Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testFreshRuntimes "Dispose - running an effect afterwards throws ObjectDisposedException" (fun runtime ->
                        (runtime :> IDisposable).Dispose()

                        Expect.throwsT<ObjectDisposedException>
                            (fun () -> runtime.Run(FIO.unit<string> ()) |> ignore)
                            "Run after Dispose should throw")

                    testFreshRuntimes "Dispose - a second call is harmless" (fun runtime ->
                        (runtime :> IDisposable).Dispose()
                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "Dispose - a fiber whose interruption throws does not keep the others from unwinding" (fun runtime ->
                        let bystanders = 32
                        let started = new CountdownEvent(bystanders + 1)
                        let finalized = ref 0

                        let hostile : FIO<unit, string> =
                            FIO.cancellationToken<string>().FlatMap(fun token ->
                                token.Register(fun () -> failwith "a cancellation callback threw") |> ignore
                                started.Signal() |> ignore
                                FIO.never ())

                        let bystander : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Signal() |> ignore).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.succeedWith (fun () -> Interlocked.Increment finalized |> ignore))

                        runtime.Run hostile |> ignore

                        for _ in 1..bystanders do
                            runtime.Run bystander |> ignore

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Every fiber should start"
                        (runtime :> IDisposable).Dispose()

                        Expect.equal finalized.Value bystanders "Dispose should interrupt every fiber and wait for its finalizer"

                        let second = Stopwatch.StartNew()
                        (runtime :> IDisposable).Dispose()
                        Expect.isLessThan second.ElapsedMilliseconds 2_000L "The first Dispose should have stopped the workers")

                    testFreshRuntimes "Shutdown - rejects a timeout it cannot wait for, and leaves the runtime running" (fun runtime ->
                        Expect.throwsT<ArgumentOutOfRangeException>
                            (fun () -> runtime.Shutdown TimeSpan.MaxValue)
                            "A timeout beyond what a wait accepts should be rejected"

                        Expect.throwsT<ArgumentOutOfRangeException>
                            (fun () -> runtime.Shutdown(TimeSpan.FromSeconds -5.0))
                            "A negative timeout should be rejected"

                        Expect.equal (runtime.Run(FIO.succeed 7 : FIO<int, string>).UnsafeSuccess()) 7 "A rejected Shutdown should not have disposed the runtime"
                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "Shutdown - gives up after its timeout when a finalizer never ends" (fun runtime ->
                        let started = new ManualResetEventSlim(false)

                        let effect : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())).Ensuring(FIO.never ())

                        runtime.Run effect |> ignore
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"
                        let stopwatch = Stopwatch.StartNew()
                        runtime.Shutdown(TimeSpan.FromMilliseconds 300.0)

                        Expect.isLessThan stopwatch.ElapsedMilliseconds 3_000L "Shutdown should stop waiting at its timeout")

                    testFreshRuntimes "Shutdown - a concurrent second call waits for the first to finish" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let finalized = ref false

                        let effect : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.sleep(TimeSpan.FromMilliseconds 300.0).FlatMap(fun () ->
                                    FIO.succeedWith (fun () -> finalized.Value <- true)))

                        runtime.Run effect |> ignore
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"
                        let first = Task.Run(fun () -> runtime.Shutdown(TimeSpan.FromSeconds 5.0))
                        Thread.Sleep 50
                        runtime.Shutdown(TimeSpan.FromSeconds 5.0)

                        Expect.isTrue finalized.Value "Either call should return only once the fiber's finalizer has run"
                        first.Wait())

                    testList
                        "Stress - Run racing Dispose never loses a fiber"
                        [
                            for runtimeName, make in
                                [
                                    "DirectRuntime", (fun () -> new DirectRuntime() :> FIORuntime)
                                    "PollingRuntime", (fun () -> new PollingRuntime(testConfig) :> FIORuntime)
                                    "SignalingRuntime", (fun () -> new SignalingRuntime(testConfig) :> FIORuntime)
                                    "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(testConfig) :> FIORuntime)
                                ] ->
                                stressTestCase runtimeName (fun () ->
                                    let deadline = Stopwatch.StartNew()
                                    let mutable lost = 0

                                    while deadline.ElapsedMilliseconds < 3_000L && lost = 0 do
                                        let runtime = make ()
                                        let fibers = ConcurrentBag<Fiber<int, string>>()
                                        use go = new ManualResetEventSlim false

                                        let spammers =
                                            [|
                                                for _ in 1..4 ->
                                                    Task.Run(fun () ->
                                                        go.Wait()
                                                        let mutable running = true
                                                        while running do
                                                            try
                                                                fibers.Add(runtime.Run(FIO.succeed 1))
                                                            with :? ObjectDisposedException ->
                                                                running <- false)
                                            |]

                                        go.Set()
                                        Thread.SpinWait(Random.Shared.Next(0, 20_000))
                                        (runtime :> IDisposable).Dispose()
                                        Task.WaitAll spammers

                                        for fiber in fibers do
                                            if not (fiber.Task().Wait(TimeSpan.FromSeconds 2.0)) then
                                                lost <- lost + 1

                                    Expect.equal lost 0 "Every fiber a successful Run returned should complete")
                        ]
                ]
        ]
