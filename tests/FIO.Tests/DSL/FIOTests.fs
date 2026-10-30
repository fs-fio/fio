module FIO.Tests.FIOTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks
open Microsoft.FSharp.Reflection

let private kindName (node: FIOBase) =
    (fst (FSharpValue.GetUnionFields(box node.Kind, typeof<NodeKind>, true))).Name

let private allKindNames =
    FSharpType.GetUnionCases(typeof<NodeKind>, true) |> Array.map (fun case -> case.Name) |> Set.ofArray

let private nodeOps (node: FIOBase) =
    node.B :?> NodeOps

let private nodeCase (expected: string) (node: FIOBase) (holds: FIOBase -> bool) =
    expected, node, holds

let private nodePair (kind: string) (first: FIOBase) (second: FIOBase) =
    kind, first, second

// Only the Nodes tests use it, so no test running in parallel creates those cached instances first, at the same time.
type private SlotError = SlotError of string

let private throwsNullReference (build: unit -> FIOBase) =
    try
        build () |> ignore
        false
    with :? NullReferenceException ->
        true

[<Tests>]
let fioTests =
    testList
        "FIO"
        [
            testList
                "FlatMap"
                [
                    testPropertyWithConfig fsCheckConfig "FlatMap - chains success continuation"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed(value).FlatMap(fun x -> FIO.succeed (x + 1))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (value + 1) "FlatMap should chain the continuation on success"

                    testPropertyWithConfig fsCheckConfig "FlatMap - short-circuits on error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let mutable contRan = false
                        let effect =
                            FIO.fail(error).FlatMap(fun (_: int) ->
                                contRan <- true
                                FIO.succeed 0)

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "FlatMap should propagate the error"
                        Expect.isFalse contRan "FlatMap continuation should not run on error"

                    testPropertyWithConfig fsCheckConfig "FlatMap - can change result type"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            FIO.succeed(value)
                                .FlatMap(fun x -> FIO.succeed (x.ToString()))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (value.ToString()) "FlatMap should allow changing the result type"

                    testAllRuntimes "FlatMap - chains multiple operations" (fun runtime ->
                        let effect =
                            FIO.succeed(1)
                                .FlatMap(fun x -> FIO.succeed (x + 10))
                                .FlatMap(fun x -> FIO.succeed (x * 2))
                                .FlatMap(fun x -> FIO.succeed (x.ToString()))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "22" "FlatMap chain should compose correctly")

                    testPropertyWithConfig fsCheckConfig "FlatMap - error in continuation propagates"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed(value).FlatMap(fun _ -> FIO.fail "boom")

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result "boom" "Error in FlatMap continuation should propagate"
                ]

            testList
                "CatchAll"
                [
                    testPropertyWithConfig fsCheckConfig "CatchAll - recovers from error"
                    <| fun (runtime: FIORuntime, error: int, recovery: int) ->
                        let effect = FIO.fail(error).CatchAll(fun _ -> FIO.succeed recovery)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result recovery "CatchAll should recover from the error"

                    testPropertyWithConfig fsCheckConfig "CatchAll - does not affect success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable handlerRan = false
                        let effect =
                            FIO.succeed(value)
                                .CatchAll(fun (_: string) ->
                                    handlerRan <- true
                                    FIO.succeed 0)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "CatchAll should not affect success"
                        Expect.isFalse handlerRan "CatchAll handler should not run on success"

                    testPropertyWithConfig fsCheckConfig "CatchAll - can change error type"
                    <| fun (runtime: FIORuntime, error: int) ->
                        let effect = FIO.fail(error).CatchAll(fun e -> FIO.fail (e.ToString()))

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result (error.ToString()) "CatchAll should allow changing the error type"

                    testPropertyWithConfig fsCheckConfig "CatchAll - recovery can itself fail"
                    <| fun (runtime: FIORuntime, error: int) ->
                        let effect = FIO.fail(error).CatchAll(fun _ -> FIO.fail "recovery failed")

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result "recovery failed" "CatchAll recovery failure should propagate"
                ]

            testList
                "Ensuring"
                [
                    testAllRuntimes "Ensuring - runs finalizer on success" (fun runtime ->
                        let mutable waitForFlag = false
                        let finalizer = FIO.attempt (fun () -> waitForFlag <- true) (fun _ -> "")
                        let effect = FIO.succeed(42).Ensuring finalizer

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue waitForFlag "Finalizer should run on success")

                    testAllRuntimes "Ensuring - runs finalizer on error" (fun runtime ->
                        let mutable waitForFlag = false
                        let finalizer = FIO.attempt (fun () -> waitForFlag <- true) (fun _ -> "")
                        let effect = FIO.fail("boom").Ensuring finalizer

                        runtime.Run(effect).UnsafeError() |> ignore

                        Expect.isTrue waitForFlag "Finalizer should run on error")

                    testPropertyWithConfig fsCheckConfig "Ensuring - preserves success result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed(value).Ensuring(FIO.unit ())

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "Ensuring should preserve the success result"

                    testPropertyWithConfig fsCheckConfig "Ensuring - preserves error when finalizer succeeds"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fail(error).Ensuring(FIO.unit ())

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "Ensuring should preserve the error"

                    testPropertyWithConfig
                        fsCheckConfig
                        "Ensuring - returns finalizer error when main succeeds but finalizer fails"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed(value).Ensuring(FIO.fail "finalizer failed")

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result "finalizer failed" "Finalizer error should be returned when main succeeds"

                    testAllRuntimes "Ensuring - finalizer runs on self-interruption"
                    <| fun (runtime: FIORuntime) ->
                        let flag = ref false
                        let effect =
                            fio {
                                let! fiber =
                                    (FIO.interruptNow ())
                                        .Ensuring(FIO.attempt (fun () -> flag.Value <- true) id)
                                        .Fork()
                                do! (fiber.Await()).Unit()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue (waitForFlag flag) "Finalizer should run on self-interruption"

                    testAllRuntimes "Ensuring - finalizer runs on external interruption"
                    <| fun (runtime: FIORuntime) ->
                        let flag = ref false
                        let effect =
                            fio {
                                let! fiber =
                                    (FIO.sleep (TimeSpan.FromSeconds 60.0))
                                        .Ensuring(FIO.attempt (fun () -> flag.Value <- true) id)
                                        .Fork()

                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! fiber.InterruptNow ()
                                do! (fiber.Await()).Unit()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue (waitForFlag flag) "Finalizer should run on external interruption"

                    testAllRuntimes "Ensuring - nested finalizers both run on interrupt"
                    <| fun (runtime: FIORuntime) ->
                        let flag1 = ref false
                        let flag2 = ref false
                        let effect =
                            fio {
                                let! fiber =
                                    (FIO.sleep (TimeSpan.FromSeconds 60.0))
                                        .Ensuring(FIO.attempt (fun () -> flag1.Value <- true) id)
                                        .Ensuring(FIO.attempt (fun () -> flag2.Value <- true) id)
                                        .Fork()

                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! fiber.InterruptNow ()
                                do! (fiber.Await()).Unit()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue (waitForFlag flag1) "Inner finalizer should run on interrupt"
                        Expect.isTrue (waitForFlag flag2) "Outer finalizer should run on interrupt"

                    testAllRuntimes "Ensuring - finalizer runs on interruption while blocked on channel read"
                    <| fun (runtime: FIORuntime) ->
                        let flag = ref false
                        let channel = Channel<int>()
                        let effect =
                            fio {
                                let! fiber =
                                    channel
                                        .Read()
                                        .Unit()
                                        .Ensuring(FIO.attempt (fun () -> flag.Value <- true) id)
                                        .Fork()

                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! fiber.InterruptNow ()
                                do! (fiber.Await()).Unit()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue (waitForFlag flag) "Finalizer should run when interrupted while blocked on a channel read"

                    testAllRuntimes "Ensuring - finalizer runs on interruption while blocked on fiber join"
                    <| fun (runtime: FIORuntime) ->
                        let flag = ref false
                        let effect =
                            fio {
                                let! target = (FIO.sleep (TimeSpan.FromSeconds 60.0)).Fork()

                                let! joiner =
                                    target
                                        .Join()
                                        .Ensuring(FIO.attempt (fun () -> flag.Value <- true) id)
                                        .Fork()

                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! joiner.InterruptNow ()
                                do! (joiner.Await()).Unit()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isTrue (waitForFlag flag) "Finalizer should run when interrupted while blocked on a fiber join"

                    testPropertyWithConfig fsCheckConfig "Ensuring - main error preserved when finalizer also fails"
                    <| fun (runtime: FIORuntime, mainErr: string, finalizerErr: string) ->
                        let effect = FIO.fail mainErr
                        let finalizer = FIO.fail finalizerErr

                        let result =
                            runtime.Run(effect.Ensuring finalizer).UnsafeError()

                        Expect.equal result mainErr "Ensuring should preserve main error, suppressing finalizer error"

                    testAllRuntimes "Ensuring - result is still interrupted when finalizer fails"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            fio {
                                let! fiber =
                                    (FIO.sleep (TimeSpan.FromSeconds 60.0))
                                        .Ensuring(FIO.fail (exn "finalizer error")).Fork()

                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! fiber.InterruptNow ()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}"

                    testAllRuntimes "Ensuring - runs the finalizer when the fiber is interrupted just before entering the effect"
                    <| fun (runtime: FIORuntime) ->
                        let self = TaskCompletionSource<Fiber<unit, string>>()
                        let finalized = new ManualResetEventSlim false
                        let effect =
                            (FIO.awaitTask self.Task (fun ex -> ex.Message)).FlatMap(fun fiber ->
                                fiber.InterruptNow().FlatMap(fun () -> FIO.unit().Ensuring(FIO.succeedWith (fun () -> finalized.Set()))))
                        let fiber = runtime.Run effect
                        self.SetResult fiber

                        let result = fiber.UnsafeResult()

                        Expect.isTrue (finalized.Wait(TimeSpan.FromSeconds 5.0)) "The finalizer should run though its effect was never entered"
                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}"

                    testAllRuntimes "Ensuring - a finalizer that is an OnExit effect hands its own finalizer the finalizer's outcome"
                    <| fun (runtime: FIORuntime) ->
                        let seen = ref None
                        let finalizer : FIO<unit, string> = FIO.unit().OnExit(fun outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                        let effect : FIO<int, string> = FIO.fail("effect failed").Ensuring finalizer

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.equal result (Failed "effect failed") "The effect's failure should survive its finalizer"
                        Expect.equal seen.Value (Some(Succeeded ())) "The OnExit inside the finalizer should see the finalizer's success, not the effect's failure"

                    testCase "Ensuring - a null finalizer throws when the effect is built"
                    <| fun () ->
                        let effect : FIO<int, string> = FIO.succeed 1

                        Expect.throwsT<NullReferenceException>
                            (fun () -> effect.Ensuring Unchecked.defaultof<FIO<unit, string>> |> ignore)
                            "A null finalizer should be rejected where the effect is built"
                ]

            testList
                "OnExit"
                [
                    testPropertyWithConfig fsCheckConfig "OnExit - hands the finalizer the success value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let seen = ref None
                        let effect : FIO<int, string> = FIO.succeed(value).OnExit(fun outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "OnExit should keep the success value"
                        Expect.equal seen.Value (Some(Succeeded value)) "The finalizer should see the success"

                    testPropertyWithConfig fsCheckConfig "OnExit - hands the finalizer the typed error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let seen = ref None
                        let effect : FIO<int, string> = FIO.fail(error).OnExit(fun outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "OnExit should keep the error"
                        Expect.equal seen.Value (Some(Failed error)) "The finalizer should see the typed error"

                    testAllRuntimes "OnExit - hands the finalizer the interruption and its cause" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let seen = ref None
                        let record outcome = FIO.succeedWith (fun () -> seen.Value <- Some outcome)
                        let fiber = runtime.Run(FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never<unit, string> ()).OnExit(record))

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should start"

                        runtime.Run(fiber.Interrupt (ResourceExhaustion "out of slots") "stopped" : FIO<unit, string>).Task().Wait()

                        Expect.isTrue (waitUntil (fun () -> seen.Value.IsSome)) "The finalizer should run"
                        match seen.Value with
                        | Some(Interrupted ex) ->
                            Expect.equal ex.cause (ResourceExhaustion "out of slots") "The finalizer should see the interruption's cause"
                            Expect.equal ex.message "stopped" "The finalizer should see the interruption's message"
                        | other -> failtest $"Expected the finalizer to see the interruption but got %A{other}")

                    testAllRuntimes "OnExit - hands the finalizer a Defect when the effect throws" (fun runtime ->
                        let thrown = InvalidOperationException "effect threw"
                        let seen = ref None
                        let effect : FIO<FiberResult<unit, string>, string> =
                            fio {
                                let! fiber = FIO.suspend(fun () -> raise thrown).OnExit(fun outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome)).Fork()
                                return! fiber.Await()
                            }

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        match seen.Value with
                        | Some(Interrupted ex) ->
                            match ex.cause with
                            | Defect inner -> Expect.isTrue (obj.ReferenceEquals(inner, thrown)) "The finalizer should see the thrown exception itself"
                            | cause -> failtest $"Expected a Defect but got %A{cause}"
                        | other -> failtest $"Expected the finalizer to see a defect but got %A{other}")

                    testAllRuntimes "OnExit - a failing finalizer replaces a success, not a failure" (fun runtime ->
                        let afterSuccess : FIO<int, string> = FIO.succeed(1).OnExit(fun _ -> FIO.fail "finalizer failed")
                        let afterFailure : FIO<int, string> = FIO.fail("effect failed").OnExit(fun _ -> FIO.fail "finalizer failed")

                        let fromSuccess = runtime.Run(afterSuccess).UnsafeError()

                        Expect.equal fromSuccess "finalizer failed" "A failing finalizer should replace a success"

                        let fromFailure = runtime.Run(afterFailure).UnsafeError()

                        Expect.equal fromFailure "effect failed" "A failing finalizer should not replace an earlier failure")

                    testAllRuntimes "OnExit - calls the finalizer only when the effect runs" (fun runtime ->
                        let calls = ref 0
                        let effect : FIO<int, string> =
                            FIO.succeed(1).OnExit(fun _ ->
                                calls.Value <- calls.Value + 1
                                FIO.unit ())
                        let callsWhenBuilt = calls.Value

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.equal callsWhenBuilt 0 "Building the effect should not call the finalizer"
                        Expect.equal calls.Value 1 "Running it should call the finalizer once")

                    testAllRuntimes "OnExit - nested finalizers run innermost first and see the same outcome" (fun runtime ->
                        let seen = Collections.Generic.List<string * FiberResult<int, string>>()
                        let record label = fun outcome -> FIO.succeedWith (fun () -> seen.Add((label, outcome)))
                        let effect : FIO<int, string> = FIO.succeed(7).OnExit(record "inner").OnExit(record "outer")

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.equal (List.ofSeq seen) [ "inner", Succeeded 7; "outer", Succeeded 7 ] "The inner finalizer should run first, both with the effect's outcome")

                    testAllRuntimes "OnExit - hands the finalizer the interruption when the fiber is interrupted just before entering the effect" (fun runtime ->
                        let self = TaskCompletionSource<Fiber<unit, string>>()
                        let seen = ref None
                        let finalized = new ManualResetEventSlim false
                        let record outcome = FIO.succeedWith (fun () -> seen.Value <- Some outcome; finalized.Set())
                        let effect =
                            (FIO.awaitTask self.Task (fun ex -> ex.Message)).FlatMap(fun fiber ->
                                fiber.InterruptNow().FlatMap(fun () -> FIO.unit().OnExit record))
                        let fiber = runtime.Run effect
                        self.SetResult fiber

                        let result = fiber.UnsafeResult()

                        Expect.isTrue (finalized.Wait(TimeSpan.FromSeconds 5.0)) "The finalizer should run though its effect was never entered"
                        match result, seen.Value with
                        | Interrupted _, Some(Interrupted ex) -> Expect.equal ex.cause ExplicitInterrupt "The finalizer should see the interruption's cause"
                        | other -> failtest $"Expected an interrupted fiber whose finalizer saw the interruption but got %A{other}")
                ]

            testList
                "Fork"
                [
                    testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with success value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! result = fiber.Join()
                                return result
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "Forked success effect should complete with the value"

                    testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                let! _ = fiber.Await()
                                return fiber
                            }

                        let fiber =
                            runtime.Run(effect).UnsafeSuccess()
                        let result = fiber.UnsafeResult()

                        match result with
                        | Failed e -> Expect.equal e error "Forked error effect should fail with the error"
                        | other -> failtest $"Expected Failed but got: {other}"

                    testAllRuntimes "Fork - forked effect executes concurrently" (fun runtime ->
                        let effect =
                            fio {
                                let chan = Channel<int>()
                                let! fiber = (chan.Read()).Fork()
                                do! chan.Write(42).Unit()
                                let! result = fiber.Join()
                                return result
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 42 "Forked receiver should get message sent after fork")

                    testAllRuntimes "Fork - multiple forks execute independently" (fun runtime ->
                        let effect =
                            fio {
                                let! f1 = FIO.succeed(1).Fork()
                                let! f2 = FIO.succeed(2).Fork()
                                let! f3 = FIO.succeed(3).Fork()
                                let! r1 = f1.Join()
                                let! r2 = f2.Join()
                                let! r3 = f3.Join()
                                return [ r1; r2; r3 ]
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [ 1; 2; 3 ] "Each forked fiber should complete independently")
                ]

            testList
                "Throw-in-handler (E-1 / E-2)"
                [
                    testAllRuntimes "Action - throwing onError surfaces the original exception as a defect (E-1)" (fun runtime ->
                        let originalExn = InvalidOperationException "original"
                        let effect: FIO<int, exn> =
                            FIO.attempt
                                (fun () -> raise originalExn)
                                (fun _ -> failwith "onError also throws")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect ->
                                Expect.isTrue
                                    (obj.ReferenceEquals(defect, originalExn))
                                    "When onError throws, the original exception should surface as the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "FlatMap - throwing continuation produces a defect (E-2)" (fun runtime ->
                        let effect: FIO<int, exn> =
                            FIO.succeed(42)
                                .FlatMap(fun (_: int) -> failwith "continuation throws")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect ->
                                Expect.equal defect.Message "continuation throws" "Thrown exception should become the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "CatchAll - throwing error handler produces a defect (E-2)" (fun runtime ->
                        let effect: FIO<int, exn> =
                            FIO.fail(InvalidOperationException "typed error")
                                .CatchAll(fun _ -> failwith "handler throws")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect ->
                                Expect.equal
                                    defect.Message
                                    "handler throws"
                                    "Thrown exception in CatchAll handler should become the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")
                ]

            testList
                "Stack safety"
                [
                    testAllRuntimes "Stack safety - deep left-chained FlatMap via Fork and CatchAll"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- effect.FlatMap(fun n -> FIO.succeed (n + 1))
                        let forked = effect.Fork()
                        let caught = effect.CatchAll(fun _ -> FIO.succeed -1)
                        let joined =
                            fio {
                                let! fiber = forked
                                let! fromFork = fiber.Join()
                                let! fromCatch = caught
                                return fromFork, fromCatch
                            }

                        let fromFork, fromCatch =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal fromFork depth $"Forked deep FlatMap chain should yield {depth}"
                        Expect.equal fromCatch depth $"Caught deep FlatMap chain should yield {depth}"

                    testAllRuntimes "Stack safety - deep left-chained CatchAll via Fork and CatchAll"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect = FIO.fail (exn "seed")
                        for _ in 1..depth do
                            effect <- effect.CatchAll(fun _ -> FIO.fail (exn "next"))
                        let recovered = effect.CatchAll(fun _ -> FIO.succeed 42)
                        let joined =
                            fio {
                                let! fiber = recovered.Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal result 42 "Deep CatchAll chain should recover to 42"

                    testAllRuntimes "Stack safety - deep left-chained Ensuring via Fork"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- effect.Ensuring(FIO.unit ())
                        let joined =
                            fio {
                                let! fiber = effect.Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal result 0 "Deep Ensuring chain should yield the seed value"

                    testAllRuntimes "Stack safety - deep left-chained OnExit via Fork"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let calls = ref 0
                        let mutable effect = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- effect.OnExit(fun _ -> FIO.succeedWith (fun () -> calls.Value <- calls.Value + 1))
                        let joined =
                            fio {
                                let! fiber = effect.Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal result 0 "Deep OnExit chain should yield the seed value"
                        Expect.equal calls.Value depth "Every finalizer in the chain should run once"

                    testAllRuntimes "Stack safety - deep left-chained MapBoth via Fork"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect : FIO<int, string> = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- effect.MapBoth (fun n -> n + 1) id
                        let joined =
                            fio {
                                let! fiber = effect.Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal result depth $"Deep MapBoth chain should yield {depth}"

                    testAllRuntimes "Stack safety - deep left-chained Map"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect : FIO<int, string> = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- effect.Map(fun n -> n + 1)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result depth $"Deep Map chain should yield {depth}"

                    testAllRuntimes "Stack safety - deep left-chained TapBoth and Fold via Fork"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect : FIO<int, string> = FIO.succeed 0
                        for _ in 1..depth do
                            effect <- (effect.TapBoth (fun _ -> FIO.unit ()) (fun _ -> FIO.unit ())).Fold (fun _ -> -1) (fun n -> n + 1)
                        let joined =
                            fio {
                                let! fiber = effect.Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(joined).UnsafeSuccess()

                        Expect.equal result depth $"Deep TapBoth and Fold chain should yield {depth}"

                    testAllRuntimes "Stack safety - deep left-chained Unit and As"
                    <| fun (runtime: FIORuntime) ->
                        let depth = 10000
                        let mutable effect : FIO<int, string> = FIO.succeed 0
                        for i in 1..depth do
                            effect <- effect.Unit().As i

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result depth $"Deep Unit and As chain should yield {depth}"

                    testAllRuntimesSequenced "Stack safety - interrupting the root of a 10,000-deep chain of nested forks"
                    <| fun (runtime: FIORuntime) ->
                        let leafStarted = new ManualResetEventSlim false
                        let leafFinalized = new ManualResetEventSlim false
                        let rec chain (level: int) : FIO<unit, string> =
                            if level = 0 then
                                (FIO.succeedWith(fun () -> leafStarted.Set()).FlatMap(fun () -> FIO.never ()))
                                    .Ensuring(FIO.succeedWith (fun () -> leafFinalized.Set()))
                            else
                                FIO.suspend(fun () -> chain (level - 1)).Fork().FlatMap(fun _ -> FIO.never ())

                        let root = runtime.Run(chain 10000)

                        Expect.isTrue (leafStarted.Wait(TimeSpan.FromSeconds 60.0)) "Every level should have forked (sequenced: 10,000 fibers stall DirectRuntime tests running alongside)"

                        runtime.Run(root.InterruptNow()).UnsafeSuccess()

                        Expect.isTrue (leafFinalized.Wait(TimeSpan.FromSeconds 60.0)) "The interruption should reach the deepest fork"

                    testAllRuntimes "Stack safety - completing a 10,000-deep chain of nested forks"
                    <| fun (runtime: FIORuntime) ->
                        let gate = TaskCompletionSource()
                        let leafReached = new ManualResetEventSlim false
                        let rec chain (level: int) : FIO<unit, string> =
                            if level = 0 then
                                FIO.succeedWith(fun () -> leafReached.Set()).FlatMap(fun () -> FIO.never ())
                            else
                                FIO.suspend(fun () -> chain (level - 1)).Fork().FlatMap(fun _ -> FIO.awaitUnitTask gate.Task (fun ex -> ex.Message))

                        let root = runtime.Run(chain 10000)

                        Expect.isTrue (leafReached.Wait(TimeSpan.FromSeconds 60.0)) "Every level should have forked and be waiting on the gate"

                        gate.SetResult()
                        let result = root.Task()

                        Expect.isTrue (result.Wait(TimeSpan.FromSeconds 60.0)) "The root should publish once the leaf's interruption has unwound back up the chain"
                        Expect.equal result.Result (Succeeded ()) "The chain should complete"

                    testAllRuntimesSequenced "Join - resumes promptly when its target publishes at the end of a 10,000-deep chain of nested forks"
                    <| fun (runtime: FIORuntime) ->
                        let gate = TaskCompletionSource()
                        let leafReached = new ManualResetEventSlim false
                        let rec chain (level: int) : FIO<unit, string> =
                            if level = 0 then
                                FIO.succeedWith(fun () -> leafReached.Set()).FlatMap(fun () -> FIO.never ())
                            else
                                FIO.suspend(fun () -> chain (level - 1)).Fork().FlatMap(fun _ -> FIO.awaitUnitTask gate.Task (fun ex -> ex.Message))
                        let joiner = runtime.Run((chain 10000).Fork().FlatMap(fun top -> top.Join()))
                        let reached = leafReached.Wait(TimeSpan.FromSeconds 60.0)
                        let clock = Stopwatch.StartNew()

                        gate.SetResult()
                        let joined = joiner.Task().Wait(TimeSpan.FromSeconds 60.0)
                        let elapsed = clock.ElapsedMilliseconds

                        Expect.isTrue reached "Every level should have forked and be waiting on the gate"
                        Expect.isTrue joined "The joiner should resume"
                        Expect.isLessThan elapsed 800L "The joiner should be rescheduled when the chain publishes, not at a worker's idle timeout"
                ]

            testList
                "Scheduler edge cases"
                [
                    testAllRuntimes "AddRegistration - fast-completing forked effects do not wedge under load"
                    <| fun (runtime: FIORuntime) ->
                        let counter = ref 0
                        let effect =
                            fio {
                                for _ in 1..200 do
                                    let! fiber = FIO.succeed(1).Fork()
                                    let! _ = fiber.Join()
                                    counter.Value <- counter.Value + 1

                                return counter.Value
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 200 "All fast-completing forks should be observed"
                ]

            testList
                "Nodes"
                [
                    testCase "Nodes - every node kind holds its payload in the slots the interpreter reads"
                    <| fun () ->
                        let fiber = new Fiber<int, string>()
                        let channel = Channel<int>()
                        let inner : FIO<int, string> = FIO.succeed 1
                        let finalizer : FIO<unit, string> = FIO.unit ()
                        let func = fun () -> 2
                        let thunk = fun () -> inner
                        let contexts = [ fiber.Context ]
                        let contextArray = [| fiber.Context |]
                        let force (node: FIOBase) = (nodeOps node).Force node
                        let wrapsInner (node: FIOBase) = obj.ReferenceEquals(node.A, inner) && node.B :? NodeOps
                        let forksInner (node: FIOBase) =
                            let struct (target, context) = (nodeOps node).ForkTarget node
                            obj.ReferenceEquals(node.A, inner) && obj.ReferenceEquals((target :?> Fiber<int, string>).Context, context)
                        let cases =
                            [
                                nodeCase "Success" (FIO.succeed<int, string> 1) (fun node -> node.A = box 1 && isNull node.B)
                                nodeCase "Failure" (FIO.fail<int, string> "error") (fun node -> node.A = box "error" && isNull node.B)
                                nodeCase "Interrupt" (FIO.interrupt<int, string> ExplicitInterrupt "stop") (fun node -> node.A = box ExplicitInterrupt && node.B = box "stop")
                                nodeCase "FiberCancellationToken" (FIO.cancellationToken<SlotError> ()) (fun node -> isNull node.A && isNull node.B && obj.ReferenceEquals(node, FIO.cancellationToken<SlotError> ()))
                                nodeCase "Action" (FIO.attempt func (fun ex -> ex.Message)) (fun node -> obj.ReferenceEquals(node.A, func) && (nodeOps node).Invoke node = box 2 && (nodeOps node).OnError(node, exn "boom") = box "boom")
                                nodeCase "Bind" (inner.FlatMap(fun n -> FIO.succeed (n + 1))) (fun node -> wrapsInner node && (nodeOps node).Continue(node, box 1).A = box 2)
                                nodeCase "Catch" (inner.CatchAll(fun (error: string) -> FIO.succeed error.Length)) (fun node -> wrapsInner node && (nodeOps node).Fail(node, box "ab").A = box 2)
                                nodeCase "Fold" (inner.MapBoth (fun n -> n + 1) (fun (error: string) -> error.Length)) (fun node -> wrapsInner node && (nodeOps node).Continue(node, box 1).A = box 2 && (nodeOps node).Fail(node, box "abc").A = box 3)
                                nodeCase "Map" (inner.Map(fun n -> n + 1)) (fun node -> wrapsInner node && (nodeOps node).Apply(node, box 1) = box 2)
                                nodeCase "Map" (inner.Unit()) (fun node -> wrapsInner node && isNull ((nodeOps node).Apply(node, box 1)))
                                nodeCase "Map" (inner.As "as") (fun node -> wrapsInner node && (nodeOps node).Apply(node, box 1) = box "as")
                                nodeCase "OnFinalize" (inner.Ensuring finalizer) (fun node -> obj.ReferenceEquals(node.A, inner) && obj.ReferenceEquals(node.B, finalizer))
                                nodeCase "OnExit" (inner.OnExit(fun _ -> finalizer)) (fun node -> wrapsInner node && obj.ReferenceEquals((nodeOps node).RunExit(node, PostFinalizerSucceeded(box 1)), finalizer))
                                nodeCase "Suspend" (FIO.suspend thunk) (fun node -> obj.ReferenceEquals(node.A, thunk) && obj.ReferenceEquals((nodeOps node).Force node, inner))
                                nodeCase "WithSuppression" (FIO.uninterruptible inner) (fun node -> (node.A :?> int -> int) 0 = 1 && obj.ReferenceEquals((nodeOps node).Body(node, 0), inner))
                                nodeCase "AcquireRelease" (FIO.acquireReleaseWith inner (fun _ -> finalizer) FIO.succeed) (fun node -> wrapsInner node && obj.ReferenceEquals((nodeOps node).OnAcquired(node, box 1).B, finalizer))
                                nodeCase "WriteChan" (channel.Write<string>(5)) (fun node -> node.A = box 5 && obj.ReferenceEquals(node.B, channel.Upcast()))
                                nodeCase "TryWriteChan" (channel.TryWrite<string>(5)) (fun node -> node.A = box 5 && obj.ReferenceEquals(node.B, channel.Upcast()))
                                nodeCase "ReadChan" (channel.Read<string>()) (fun node -> obj.ReferenceEquals(node.A, channel.Upcast()) && isNull node.B)
                                nodeCase "ForkEffect" (force (inner.Fork<string>())) forksInner
                                nodeCase "ForkDaemonEffect" (force (inner.ForkDaemon<string>())) forksInner
                                nodeCase "JoinFiber" (fiber.Join()) (fun node -> obj.ReferenceEquals(node.A, fiber.Context) && isNull node.B)
                                nodeCase "JoinFirst" (FIO.joinFirst<string> contexts) (fun node -> obj.ReferenceEquals(node.A, contexts) && isNull node.B)
                                nodeCase "JoinAllFailFast" (FIO.joinAllFailFast<string> contextArray) (fun node -> obj.ReferenceEquals(node.A, contextArray) && isNull node.B)
                                nodeCase "AwaitTask" (FIO.awaitTask (Task.FromResult 3) (fun ex -> ex.Message)) (fun node -> (node.A :?> Task<obj>).Result = box 3 && (nodeOps node).OnError(node, exn "boom") = box "boom")
                            ]

                        let readings = [ for expected, node, holds in cases -> expected, kindName node, holds node ]

                        Expect.isEmpty [ for expected, kind, held in readings do if kind <> expected || not held then expected ] "These node kinds should hold their payloads in the slots the interpreter reads"
                        Expect.equal (readings |> List.map (fun (_, kind, _) -> kind) |> Set.ofList) allKindNames "Every node kind should be built"

                    testCase "Nodes - two nodes of a typed kind with the same type arguments share one ops instance"
                    <| fun () ->
                        let isolated : FIO<int, SlotError> = FIO.succeed 1
                        let pairs =
                            [
                                nodePair "Bind" (isolated.FlatMap(fun n -> FIO.succeed (n + 1))) (isolated.FlatMap(fun n -> FIO.succeed (n + 2)))
                                nodePair "Catch" (isolated.CatchAll<SlotError>(fun _ -> FIO.succeed 1)) (isolated.CatchAll<SlotError>(fun _ -> FIO.succeed 0))
                                nodePair "Fold" (isolated.MapBoth (fun n -> n + 1) (fun (_: SlotError) -> 1)) (isolated.MapBoth (fun n -> n + 2) (fun (_: SlotError) -> 0))
                                nodePair "Map" (isolated.Map(fun n -> n + 1)) (isolated.Map(fun n -> n + 2))
                                nodePair "Map" (isolated.Unit()) (isolated.Unit())
                                nodePair "Map" (isolated.As "as") (isolated.As "other")
                            ]

                        let unshared = [ for kind, first, second in pairs do if kindName first <> kind || not (obj.ReferenceEquals(first.B, second.B)) then kind ]

                        Expect.isEmpty unshared "Two nodes of these kinds with the same type arguments should share one ops instance"

                    testCase "Nodes - every builder that builds its node at once rejects a null effect where the effect is built"
                    <| fun () ->
                        let none : FIO<int, string> = Unchecked.defaultof<_>
                        let other : FIO<int, string> = FIO.succeed 1
                        let builders : (string * (unit -> FIOBase)) list =
                            [
                                "FlatMap", fun () -> none.FlatMap(fun n -> FIO.succeed n) :> FIOBase
                                "CatchAll", fun () -> none.CatchAll(fun (_: string) -> FIO.succeed 0) :> FIOBase
                                "Ensuring", fun () -> none.Ensuring(FIO.unit ()) :> FIOBase
                                "OnExit", fun () -> none.OnExit(fun _ -> FIO.unit ()) :> FIOBase
                                "Map", fun () -> none.Map(fun n -> n + 1) :> FIOBase
                                "MapError", fun () -> none.MapError(fun (error: string) -> error.Length) :> FIOBase
                                "MapBoth", fun () -> none.MapBoth (fun n -> n + 1) (fun (error: string) -> error.Length) :> FIOBase
                                "Result", fun () -> none.Result<string>() :> FIOBase
                                "Option", fun () -> none.Option<string>() :> FIOBase
                                "Choice", fun () -> none.Choice<string>() :> FIOBase
                                "Unit", fun () -> none.Unit() :> FIOBase
                                "As", fun () -> none.As "as" :> FIOBase
                                "TapBoth", fun () -> none.TapBoth (fun _ -> FIO.unit ()) (fun _ -> FIO.unit ()) :> FIOBase
                                "OrElseEither", fun () -> none.OrElseEither other :> FIOBase
                                "ZipRight", fun () -> none.ZipRight other :> FIOBase
                                "Fold", fun () -> none.Fold<int, string> (fun _ -> 0) (fun n -> n) :> FIOBase
                                "FoldFIO", fun () -> none.FoldFIO (fun _ -> FIO.succeed 0) (fun n -> FIO.succeed n) :> FIOBase
                            ]

                        let accepted = [ for name, build in builders do if not (throwsNullReference build) then name ]

                        Expect.isEmpty accepted "These builders should reject a null effect where the effect is built"
                ]
        ]
