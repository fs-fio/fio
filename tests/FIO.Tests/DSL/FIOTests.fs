module FIO.Tests.FIOTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling

open Expecto

open System
open System.Threading

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime() :> FIORuntime
        new SignalingRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let fioTests =
    testList
        "FIO"
        [
            // ─── Fork ─────────────────────────────────────────

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
                        return fiber
                    }

                let fiber =
                    runtime.Run(effect).UnsafeSuccess()

                match fiber.UnsafeResult() with
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

            // ─── FlatMap ─────────────────────────────────────────

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

            // ─── CatchAll ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "CatchAll - recovers from error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).CatchAll(fun e -> FIO.succeed (e * 2))

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (error * 2) "CatchAll should recover from the error"

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

            // ─── Ensuring ─────────────────────────────────────────

            testAllRuntimes "Ensuring - runs finalizer on success" (fun runtime ->
                let mutable finalizerRan = false
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun _ -> "")
                let effect = FIO.succeed(42).Ensuring finalizer

                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue finalizerRan "Finalizer should run on success")

            testAllRuntimes "Ensuring - runs finalizer on error" (fun runtime ->
                let mutable finalizerRan = false
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun _ -> "")
                let effect = FIO.fail("boom").Ensuring finalizer

                runtime.Run(effect).UnsafeError() |> ignore

                Expect.isTrue finalizerRan "Finalizer should run on error")

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
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                    }

                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue flag.Value "Finalizer should run on self-interruption"

            testAllRuntimes "Ensuring - finalizer runs on external interruption"
            <| fun (runtime: FIORuntime) ->
                let flag = ref false

                let effect =
                    fio {
                        let! fiber =
                            (FIO.sleep (TimeSpan.FromSeconds 60.0) id)
                                .Ensuring(FIO.attempt (fun () -> flag.Value <- true) id)
                                .Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                        do! fiber.InterruptNow ()
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                    }

                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue flag.Value "Finalizer should run on external interruption"

            testAllRuntimes "Ensuring - nested finalizers both run on interrupt"
            <| fun (runtime: FIORuntime) ->
                let flag1 = ref false
                let flag2 = ref false

                let effect =
                    fio {
                        let! fiber =
                            (FIO.sleep(TimeSpan.FromSeconds 60.0) id)
                                .Ensuring(FIO.attempt (fun () -> flag1.Value <- true) id)
                                .Ensuring(FIO.attempt (fun () -> flag2.Value <- true) id)
                                .Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                        do! fiber.InterruptNow ()
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                    }

                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue flag1.Value "Inner finalizer should run on interrupt"
                Expect.isTrue flag2.Value "Outer finalizer should run on interrupt"

            testAllRuntimes "Ensuring - result is still interrupted when finalizer fails"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let! fiber =
                            (FIO.sleep(TimeSpan.FromSeconds 60.0) id)
                                .Ensuring(FIO.fail (exn "finalizer error")).Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) id
                        do! fiber.InterruptNow ()
                        return! fiber.Join()
                    }

                let result =
                    runtime.Run(effect).UnsafeResult()

                match result with
                | Interrupted _ -> ()
                | other -> failtest $"Expected Interrupted but got {other}"

            // ─── Stack safety ─────────────────────────────────────────

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

            // ─── Scheduler edge cases ─────────────────────────────────────────

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

            // ─── Throw-in-handler (E-1 / E-2) ─────────────────────────────────────────

            testAllRuntimes "Action - throwing onError falls back to original exception (E-1)" (fun runtime ->
                let originalExn = InvalidOperationException "original"

                let effect =
                    FIO.attempt
                        (fun () -> raise originalExn)
                        (fun _ -> failwith "onError also throws")

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.isTrue
                    (obj.ReferenceEquals(result, originalExn))
                    "When onError throws, the original exception should be used as the error")

            testAllRuntimes "FlatMap - throwing continuation produces error (E-2)" (fun runtime ->
                let effect: FIO<int, exn> =
                    FIO.succeed(42)
                        .FlatMap(fun (_: int) -> failwith "continuation throws")

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result.Message "continuation throws" "Thrown exception should become the error")

            testAllRuntimes "CatchAll - throwing error handler produces error (E-2)" (fun runtime ->
                let effect: FIO<int, exn> =
                    FIO.fail(InvalidOperationException "typed error")
                        .CatchAll(fun _ -> failwith "handler throws")

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal
                    result.Message
                    "handler throws"
                    "Thrown exception in CatchAll handler should become the error")

            // ─── Run lifecycle ─────────────────────────────────────────

            testAllRuntimes "Run - orphaned child fibers are interrupted on re-Run" (fun runtime ->
                let childStarted = new ManualResetEventSlim false

                let childEffect: FIO<unit, exn> =
                    fio {
                        do! FIO.attempt
                                (fun () -> childStarted.Set())
                                id
                        return! FIO.never ()
                    }

                let parentEffect: FIO<obj, exn> =
                    fio {
                        let! fiber = childEffect.Fork()
                        do! FIO.attempt
                                (fun () -> childStarted.Wait(TimeSpan.FromSeconds 5.0) |> ignore)
                                id
                        return fiber :> obj
                    }

                let fiber1 = runtime.Run parentEffect
                let childFiber = fiber1.UnsafeSuccess() :?> Fiber<unit, exn>

                let fiber2 = runtime.Run(FIO.succeed 99)
                Expect.equal (fiber2.UnsafeSuccess()) 99 "Second run should succeed"

                let childResult = childFiber.UnsafeResult()

                match childResult with
                | Interrupted _ -> ()
                | other -> failtestf "Expected child fiber to be Interrupted, got %A" other)

            testAllRuntimes "Run - second Run produces correct result after first completes" (fun runtime ->
                let fiber1 = runtime.Run(FIO.succeed 1)
                Expect.equal (fiber1.UnsafeSuccess()) 1 "First run"

                let fiber2 = runtime.Run(FIO.succeed 2)
                Expect.equal (fiber2.UnsafeSuccess()) 2 "Second run")
        ]
