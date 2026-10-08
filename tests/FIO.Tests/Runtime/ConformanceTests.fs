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

let private lowestCpuSecondsOf (samples: int) =
    List.init samples (fun _ ->
        let before = Process.GetCurrentProcess().TotalProcessorTime
        Thread.Sleep 400
        (Process.GetCurrentProcess().TotalProcessorTime - before).TotalSeconds)
    |> List.min

let private cpuAddedByInterruptedWait (runtime: FIORuntime) (wait: FIO<unit, string>) =
    let started = new ManualResetEventSlim(false)
    let waiting = FIO.uninterruptible (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> wait))
    let ambient = lowestCpuSecondsOf 3
    let fiber = runtime.Run waiting
    started.Wait(TimeSpan.FromSeconds 5.0) |> ignore
    runtime.Run(fiber.InterruptNow(): FIO<unit, string>).UnsafeResult() |> ignore
    Thread.Sleep 300
    lowestCpuSecondsOf 3 - ambient

let private workerRuntimes : (string * (unit -> FIORuntime)) list =
    [
        "PollingRuntime", (fun () -> new PollingRuntime(testConfig) :> FIORuntime)
        "SignalingRuntime", (fun () -> new SignalingRuntime(testConfig) :> FIORuntime)
        "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(testConfig) :> FIORuntime)
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

                    testAllRuntimes "Defect - throwing release function in acquireReleaseWith"
                    <| fun runtime ->
                        expectDefect
                            runtime
                            "acquireReleaseWith release"
                            (FIO.acquireReleaseWith (FIO.succeed 1) (fun (_: int) -> failwith "release threw") (fun n -> FIO.succeed n))

                    testAllRuntimes "Defect - throwing uninterruptibleMask body"
                    <| fun runtime ->
                        expectDefect runtime "uninterruptibleMask" (FIO.uninterruptibleMask (fun _ -> failwith "body threw"))
                ]

            testList
                "The typed error channel stays typed"
                [
                    testAllRuntimes "Typed error - a genuine failure surfaces as Failed, not Interrupted"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.fail "typed failure"

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Failed error -> Expect.equal error "typed failure" "The typed error must survive intact"
                        | other -> failtest $"{runtime.GetType().Name}: expected Failed but got {other}"

                    testAllRuntimes "Typed error - success surfaces as Succeeded"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.succeed 42

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded value -> Expect.equal value 42 "The success value must survive intact"
                        | other -> failtest $"{runtime.GetType().Name}: expected Succeeded but got {other}"
                ]

            testList
                "Await / Poll / UnsafeResult agree"
                [
                    testAllRuntimes "Observation - Await and UnsafeResult agree on a defected fiber"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.suspend (fun () -> failwith "boom")
                        let observer: FIO<FiberResult<int, string>, string> =
                            effect.Fork().FlatMap(fun fiber -> fiber.Await())

                        let viaAwait = runtime.Run(observer).UnsafeSuccess()
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
                        let finalizerFinished =
                            interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.sleep (TimeSpan.FromMilliseconds 150.0))
                                    .FlatMap(fun () -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message)))

                        Expect.isTrue
                            finalizerFinished
                            $"{runtime.GetType().Name}: a finalizer must be able to sleep after its fiber was interrupted"

                    testAllRuntimes "Finalizer - FIO.async runs to completion after interruption"
                    <| fun runtime ->
                        let finalizerFinished =
                            interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.async
                                    (fun complete -> complete (Ok 1))
                                    (fun ex -> ex.Message): FIO<int, string>)
                                    .FlatMap(fun _ -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message)))

                        Expect.isTrue
                            finalizerFinished
                            $"{runtime.GetType().Name}: a finalizer must be able to use FIO.async after interruption"

                    testAllRuntimes "Finalizer - FIO.awaitAsync runs to completion after interruption"
                    <| fun runtime ->
                        let finalizerFinished =
                            interruptAndAwaitFinalizer runtime (fun finished ->
                                (FIO.awaitAsync (async { return 1 }) (fun ex -> ex.Message): FIO<int, string>)
                                    .FlatMap(fun _ -> FIO.attempt (fun () -> finished.Value <- true) (fun ex -> ex.Message)))

                        Expect.isTrue
                            finalizerFinished
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
                            let result = runtime.Run(bounded).UnsafeSuccess()

                            Expect.equal
                                result
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

                            let result = runtime.Run(effect).UnsafeSuccess()

                            match result with
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

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
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
                        let result = runtime.Run(FIO.succeed 7: FIO<int, string>).UnsafeResult()

                        match result with
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
                        let secondResult = second.UnsafeResult()

                        match secondResult with
                        | Succeeded value -> Expect.equal value 2 "The second fiber must complete"
                        | other -> failtest $"{runtime.GetType().Name}: second fiber expected Succeeded but got {other}"

                        let firstResult = first.UnsafeResult()

                        match firstResult with
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
                        let second = fiber2.UnsafeSuccess()

                        Expect.equal second 99 "The second fiber must complete"
                        Expect.isFalse
                            (childFiber.IsTerminal())
                            $"{runtime.GetType().Name}: a later Run must leave a fiber that is already running untouched"

                        runtime.Run(childFiber.InterruptNow()).UnsafeSuccess()
                        let childResult = childFiber.UnsafeResult()

                        match childResult with
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
                            use runtime = new PollingRuntime(testConfig)
                            let channel = Channel<int>()

                            let ambient = lowestCpuSecondsOf 3
                            runtime.Run(channel.Read().Unit(): FIO<unit, string>) |> ignore
                            Thread.Sleep 300
                            let added = lowestCpuSecondsOf 3 - ambient

                            Expect.isLessThan
                                added
                                0.4
                                $"A blocked fiber must not keep a PollingRuntime worker spinning (added {added:F2} CPU-seconds/s over {ambient:F2} ambient)")

                    testSequenced (
                        testList
                            "PollingRuntime - an interrupted fiber waiting uninterruptibly does not burn CPU"
                            [
                                stressTestCase "on a channel read" (fun () ->
                                    use runtime = new PollingRuntime(testConfig)
                                    let channel = Channel<int>()

                                    let added = cpuAddedByInterruptedWait runtime (channel.Read().Unit())
                                    runtime.Run(channel.Write 1: FIO<int, string>).UnsafeResult() |> ignore

                                    Expect.isLessThan added 0.4 $"An interruption the fiber cannot act on yet must not make the workers spin (added {added:F2} CPU-seconds per 0.4 s)")

                                stressTestCase "on a join" (fun () ->
                                    use runtime = new PollingRuntime(testConfig)
                                    let gate = TaskCompletionSource<int>()
                                    let target = runtime.Run(FIO.awaitTask gate.Task (fun ex -> ex.Message))

                                    let added = cpuAddedByInterruptedWait runtime (target.Join().Unit())
                                    gate.SetResult 1

                                    Expect.isLessThan added 0.4 $"An interruption the fiber cannot act on yet must not make the workers spin (added {added:F2} CPU-seconds per 0.4 s)")
                            ])

                    testSequenced (
                        testList
                            "Channel - a writer parked on a full channel inside a finalizer does not burn CPU"
                            [
                                for runtimeName, make in workerRuntimes ->
                                    stressTestCase runtimeName (fun () ->
                                        use runtime = make ()
                                        let channel = Channel<int>.Bounded 1
                                        let started = new ManualResetEventSlim(false)
                                        runtime.Run(channel.Write 1: FIO<int, string>).UnsafeResult() |> ignore
                                        let effect: FIO<unit, string> =
                                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                                .Ensuring(channel.Write(2).Unit())

                                        let ambient = lowestCpuSecondsOf 3
                                        let fiber = runtime.Run effect
                                        let running = started.Wait(TimeSpan.FromSeconds 5.0)
                                        runtime.Run(fiber.InterruptNow(): FIO<unit, string>).UnsafeResult() |> ignore
                                        Thread.Sleep 300
                                        let added = lowestCpuSecondsOf 3 - ambient
                                        runtime.Run(channel.Read(): FIO<int, string>).UnsafeResult() |> ignore

                                        Expect.isTrue running "The fiber should start"
                                        Expect.isLessThan added 0.4 $"A writer that waits in a finalizer must not spin on its fiber's cancelled token (added {added:F2} CPU-seconds per 0.4 s)")
                            ])

                    testAllRuntimes "Run - several fibers started at once all complete"
                    <| fun runtime ->
                        let fibers = [ for i in 1..8 -> runtime.Run(FIO.succeed i: FIO<int, string>) ]
                        let outcomes = fibers |> List.map (fun fiber -> fiber.UnsafeResult())

                        let results =
                            outcomes
                            |> List.map (fun outcome ->
                                match outcome with
                                | Succeeded value -> value
                                | other -> failtest $"{runtime.GetType().Name}: expected Succeeded but got {other}")
                        Expect.equal (List.sort results) [ 1..8 ] "Every concurrently started fiber must complete with its own value"
                ]

            testList
                "Awaited task cancellation"
                [
                    testFreshRuntimes "awaitTask - in a finalizer of an interrupted fiber, a task cancelled by another token fails through onError" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let awaiting = new ManualResetEventSlim false
                        let foreign = TaskCompletionSource<int>()
                        let seen = TaskCompletionSource<string>()
                        let finalizer: FIO<unit, string> =
                            FIO.succeedWith(fun () -> awaiting.Set())
                                .FlatMap(fun () -> FIO.awaitTask foreign.Task (fun ex -> ex.GetType().Name))
                                .CatchAll(fun name ->
                                    FIO.succeedWith (fun () ->
                                        seen.TrySetResult name |> ignore
                                        0))
                                .Unit()
                        let effect: FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())).Ensuring finalizer
                        let fiber = runtime.Run effect
                        let running = started.Wait(TimeSpan.FromSeconds 5.0)

                        runtime.Run(fiber.InterruptNow(): FIO<unit, string>).UnsafeResult() |> ignore
                        let parked = awaiting.Wait(TimeSpan.FromSeconds 5.0)
                        Thread.Sleep 50
                        foreign.SetCanceled()
                        let handled = seen.Task.Wait(TimeSpan.FromSeconds 5.0)

                        Expect.isTrue (running && parked) "The fiber should reach its finalizer"
                        Expect.isTrue handled "The finalizer's CatchAll should see the task's cancellation as a typed error"
                        Expect.equal seen.Task.Result "TaskCanceledException" "onError should receive the cancellation"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "awaitTask - in an uninterruptible region of an interrupted fiber, a task cancelled by another token fails through onError" (fun runtime ->
                        let awaiting = new ManualResetEventSlim false
                        let foreign = TaskCompletionSource<int>()
                        let seen = TaskCompletionSource<string>()
                        let region: FIO<unit, string> =
                            FIO.succeedWith(fun () -> awaiting.Set())
                                .FlatMap(fun () -> FIO.awaitTask foreign.Task (fun ex -> ex.GetType().Name))
                                .CatchAll(fun name ->
                                    FIO.succeedWith (fun () ->
                                        seen.TrySetResult name |> ignore
                                        0))
                                .Unit()
                        let fiber = runtime.Run(FIO.uninterruptible region)
                        let parked = awaiting.Wait(TimeSpan.FromSeconds 5.0)

                        runtime.Run(fiber.InterruptNow(): FIO<unit, string>).UnsafeResult() |> ignore
                        Thread.Sleep 50
                        foreign.SetCanceled()
                        let handled = seen.Task.Wait(TimeSpan.FromSeconds 5.0)

                        Expect.isTrue parked "The fiber should reach its await"
                        Expect.isTrue handled "The region's CatchAll should see the task's cancellation as a typed error"
                        Expect.equal seen.Task.Result "TaskCanceledException" "onError should receive the cancellation"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "awaitTask - a task faulted with OperationCanceledException fails through onError, also on an interrupted fiber" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let seen = TaskCompletionSource<string>()
                        let faulted = Task.FromException<int>(OperationCanceledException "the task's own failure")
                        let finalizer: FIO<unit, string> =
                            (FIO.awaitTask faulted (fun ex -> ex.Message))
                                .CatchAll(fun message ->
                                    FIO.succeedWith (fun () ->
                                        seen.TrySetResult message |> ignore
                                        0))
                                .Unit()
                        let effect: FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())).Ensuring finalizer
                        let fiber = runtime.Run effect
                        let running = started.Wait(TimeSpan.FromSeconds 5.0)

                        runtime.Run(fiber.InterruptNow(): FIO<unit, string>).UnsafeResult() |> ignore
                        let handled = seen.Task.Wait(TimeSpan.FromSeconds 5.0)

                        Expect.isTrue running "The fiber should start"
                        Expect.isTrue handled "A faulted task is a typed error, whatever exception it holds"
                        Expect.equal seen.Task.Result "the task's own failure" "onError should receive the task's exception"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "awaitTask - a cancelled task hands onError a TaskCanceledException" (fun runtime ->
                        let cancelled = Task.FromCanceled<int>(CancellationToken true)

                        let result = runtime.Run(FIO.awaitTask cancelled (fun ex -> ex.GetType().Name)).UnsafeResult()

                        Expect.equal result (Failed "TaskCanceledException") "Every runtime should hand onError the same exception type"

                        (runtime :> IDisposable).Dispose())
                ]

            testList
                "Workers after shutdown"
                [
                    for runtimeName, make in workerRuntimes ->
                        testCase $"Shutdown - a fiber abandoned at the timeout runs no further step.{runtimeName}" (fun () ->
                            let runtime = make ()
                            let gate = TaskCompletionSource()
                            let parked = new ManualResetEventSlim false
                            let steps = ref 0
                            let abandoned: FIO<unit, string> =
                                FIO.uninterruptible (
                                    FIO.succeedWith(fun () -> parked.Set())
                                        .FlatMap(fun () -> FIO.awaitUnitTask gate.Task (fun ex -> ex.Message))
                                        .FlatMap(fun () -> FIO.succeedWith (fun () -> Interlocked.Increment steps |> ignore)))
                            Thread.Sleep 100
                            runtime.Run abandoned |> ignore
                            let waiting = parked.Wait(TimeSpan.FromSeconds 5.0)
                            Thread.Sleep 50

                            runtime.Shutdown(TimeSpan.FromMilliseconds 50.0)
                            gate.SetResult()
                            Thread.Sleep 300

                            Expect.isTrue waiting "The fiber should reach its await"
                            Expect.equal steps.Value 0 "A worker that was stopped should not pick up the abandoned fiber again")

                    for runtimeName, make in workerRuntimes ->
                        testCase $"Shutdown - a fiber still running steps leaves its stopped worker.{runtimeName}" (fun () ->
                            let runtime = make ()
                            let steps = ref 0L
                            let rec spin () : FIO<unit, string> =
                                FIO.succeedWith(fun () -> Interlocked.Increment steps |> ignore).FlatMap(fun () -> spin ())
                            runtime.Run(FIO.uninterruptible (spin ())) |> ignore
                            let running = SpinWait.SpinUntil((fun () -> Interlocked.Read &steps.contents > 1_000L), 5_000)

                            runtime.Shutdown(TimeSpan.FromMilliseconds 100.0)
                            Thread.Sleep 50
                            let afterShutdown = Interlocked.Read &steps.contents
                            Thread.Sleep 300
                            let later = Interlocked.Read &steps.contents

                            Expect.isTrue running "The fiber should be running"
                            Expect.equal later afterShutdown "No step should run once the runtime has shut down")
                ]

            testList
                "Joining an interrupted fiber"
                [
                    testFreshRuntimes "Join - resumes when its target is interrupted, without waiting for the target's finalizer" (fun runtime ->
                        let started = TaskCompletionSource()
                        let clock = Stopwatch()
                        let resumedAt = ref -1L
                        let target: FIO<int, string> =
                            (FIO.succeedWith(fun () -> started.SetResult()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.sleep (TimeSpan.FromMilliseconds 700.0))
                        let effect =
                            target.Fork().FlatMap(fun fiber ->
                                let joiner =
                                    fiber.Join().Ensuring(FIO.succeedWith (fun () -> resumedAt.Value <- clock.ElapsedMilliseconds))
                                joiner.Fork().FlatMap(fun joining ->
                                    (FIO.awaitUnitTask started.Task (fun ex -> ex.Message))
                                        .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                        .FlatMap(fun () -> FIO.succeedWith (fun () -> clock.Start()))
                                        .FlatMap(fun () -> fiber.Interrupt ExplicitInterrupt "stop")
                                        .FlatMap(fun () -> joining.Await())))

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded(Interrupted _) -> ()
                        | other -> failtest $"{runtime.GetType().Name}: expected the joiner to be interrupted, got {other}"
                        Expect.isLessThan resumedAt.Value 300L "The joiner should resume at the interruption, not after the 700 ms finalizer"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "JoinFirst - resumes when a target is interrupted, without waiting for the target's finalizer" (fun runtime ->
                        let started = TaskCompletionSource()
                        let clock = Stopwatch()
                        let resumedAt = ref -1L
                        let target: FIO<int, string> =
                            (FIO.succeedWith(fun () -> started.SetResult()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.sleep (TimeSpan.FromMilliseconds 700.0))
                        let effect =
                            target.Fork().FlatMap(fun fiber ->
                                let joiner =
                                    (FIO.joinFirst [ fiber.Context ]: FIO<int, string>)
                                        .FlatMap(fun index -> FIO.succeedWith (fun () ->
                                            resumedAt.Value <- clock.ElapsedMilliseconds
                                            index))
                                joiner.Fork().FlatMap(fun joining ->
                                    (FIO.awaitUnitTask started.Task (fun ex -> ex.Message))
                                        .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                        .FlatMap(fun () -> FIO.succeedWith (fun () -> clock.Start()))
                                        .FlatMap(fun () -> fiber.Interrupt ExplicitInterrupt "stop")
                                        .FlatMap(fun () -> joining.Await())))

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.equal result (Succeeded(Succeeded 0)) "The joiner should yield the index of the interrupted fiber"
                        Expect.isLessThan resumedAt.Value 300L "The joiner should resume at the interruption, not after the 700 ms finalizer"

                        (runtime :> IDisposable).Dispose())
                ]

            testList
                "Runtime disposal"
                [
                    testFreshRuntimes "Dispose - interrupts a running fiber and returns after its finalizer ran" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let finalized = ref false
                        let effect : FIO<unit, string> =
                            FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())

                        let fiber = runtime.Run(effect.Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true)))

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"

                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue finalized.Value "Dispose should return only once the fiber's finalizer has run"

                        let result = fiber.UnsafeResult()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testFreshRuntimes "Dispose - interrupts daemon fibers and runs their finalizers" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let finalized = ref false
                        let daemon : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true))

                        runtime.Run(daemon.ForkDaemon<string>()).UnsafeResult() |> ignore

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The daemon should start"

                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue finalized.Value "Dispose should interrupt the daemon and wait for its finalizer")

                    testFreshRuntimes "Dispose - a daemon forked by a finalizer during disposal is interrupted at once" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let late = TaskCompletionSource<FiberResult<unit, string>>()
                        let lateDaemon: FIO<unit, string> = FIO.never ()
                        let root: FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(
                                    lateDaemon.ForkDaemon<string>().FlatMap(fun fiber ->
                                        fiber.Await().FlatMap(fun result -> FIO.succeedWith (fun () -> late.TrySetResult result |> ignore)))
                                )

                        runtime.Run root |> ignore

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The root should start"

                        (runtime :> IDisposable).Dispose()

                        Expect.isTrue (late.Task.Wait(TimeSpan.FromSeconds 5.0)) "The late daemon should end at once"
                        match late.Task.Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected the late daemon to be interrupted, got {other}")

                    testFreshRuntimes "Dispose - waits for scoped children to unwind" (fun runtime ->
                        let started = new ManualResetEventSlim false
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
                        let result = parked.Task()

                        Expect.isTrue (result.Wait(TimeSpan.FromSeconds 5.0)) "The parked fiber should complete"
                        match result.Result with
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

                        let result = runtime.Run(FIO.succeed 7 : FIO<int, string>).UnsafeSuccess()

                        Expect.equal result 7 "A rejected Shutdown should not have disposed the runtime"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "Shutdown - accepts Timeout.InfiniteTimeSpan" (fun runtime ->
                        runtime.Shutdown Timeout.InfiniteTimeSpan

                        Expect.throwsT<ObjectDisposedException>
                            (fun () -> runtime.Run(FIO.succeed 7 : FIO<int, string>) |> ignore)
                            "A runtime shut down with no time limit should be disposed")

                    testFreshRuntimes "Shutdown - gives up after its timeout when a finalizer never ends" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let effect : FIO<unit, string> =
                            (FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())).Ensuring(FIO.never ())

                        runtime.Run effect |> ignore

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"

                        let stopwatch = Stopwatch.StartNew()
                        runtime.Shutdown(TimeSpan.FromMilliseconds 300.0)

                        Expect.isLessThan stopwatch.ElapsedMilliseconds 3_000L "Shutdown should stop waiting at its timeout")

                    testFreshRuntimes "Shutdown - a fiber that only waits for its child gets its result when the timeout abandons it" (fun runtime ->
                        let childStarted = TaskCompletionSource()
                        let child: FIO<unit, string> =
                            (FIO.succeedWith(fun () -> childStarted.SetResult()).FlatMap(fun () -> FIO.never ()))
                                .Ensuring(FIO.sleep (TimeSpan.FromSeconds 3.0))
                        let root =
                            child.Fork().FlatMap(fun _ -> FIO.awaitUnitTask childStarted.Task (fun ex -> ex.Message)).FlatMap(fun () -> FIO.succeed 1)
                        let fiber = runtime.Run root
                        let started = childStarted.Task.Wait(TimeSpan.FromSeconds 5.0)
                        Thread.Sleep 100

                        runtime.Shutdown(TimeSpan.FromMilliseconds 500.0)
                        let published = fiber.Task().Wait(TimeSpan.FromSeconds 1.0)

                        Expect.isTrue started "The child should start"
                        Expect.isTrue published "A root that had finished its effect should not be left without a result"
                        Expect.equal (fiber.Task().Result) (Succeeded 1) "The root should publish the result it had")

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

                    testFreshRuntimes "Run - releases a root fiber once it has unwound, whichever thread ran it" (fun runtime ->
                        let threads =
                            [|
                                for i in 1..8 ->
                                    Thread(fun () -> runtime.Run(FIO.succeed i: FIO<int, string>).UnsafeResult() |> ignore)
                            |]

                        for thread in threads do
                            thread.Start()
                        for thread in threads do
                            thread.Join()
                        let released = SpinWait.SpinUntil((fun () -> runtime.TrackedCount = 0), 5_000)

                        Expect.isTrue released $"A finished fiber should not stay tracked, but {runtime.TrackedCount} did"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "Run - releases every fiber of a burst" (fun runtime ->
                        let burst = 2_000

                        let fibers = [| for i in 1..burst -> runtime.Run(FIO.succeed i: FIO<int, string>) |]
                        for fiber in fibers do
                            fiber.UnsafeResult() |> ignore
                        let released = SpinWait.SpinUntil((fun () -> runtime.TrackedCount = 0), 5_000)

                        Expect.isTrue released $"A finished fiber should not stay tracked, but {runtime.TrackedCount} did"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "ForkDaemon - releases a daemon fiber once it has unwound" (fun runtime ->
                        let daemon: FIO<int, string> = FIO.succeed 1

                        runtime.Run(daemon.ForkDaemon<string>().FlatMap(fun fiber -> fiber.Await())).UnsafeResult() |> ignore
                        let released = SpinWait.SpinUntil((fun () -> runtime.TrackedCount = 0), 5_000)

                        Expect.isTrue released $"A finished daemon should not stay tracked, but {runtime.TrackedCount} did"

                        (runtime :> IDisposable).Dispose())

                    testFreshRuntimes "Run - keeps a running fiber tracked until it unwinds" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let effect: FIO<unit, string> =
                            FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())

                        runtime.Run effect |> ignore
                        let running = started.Wait(TimeSpan.FromSeconds 5.0)
                        let tracked = runtime.TrackedCount

                        Expect.isTrue running "The fiber should start"
                        Expect.equal tracked 1 "A running fiber should be tracked"

                        (runtime :> IDisposable).Dispose()
                        let afterDispose = runtime.TrackedCount

                        Expect.equal afterDispose 0 "Dispose should leave nothing tracked")

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
                                    let mutable pending = 0

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
                                            if not (fiber.Task().IsCompleted) then
                                                pending <- pending + 1
                                            if not (fiber.Task().Wait(TimeSpan.FromSeconds 2.0)) then
                                                lost <- lost + 1

                                    Expect.equal lost 0 "Every fiber a successful Run returned should complete"
                                    Expect.equal pending 0 "Dispose should return only once every fiber has its result")
                        ]
                ]
        ]
