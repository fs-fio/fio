module FIO.Tests.FIOTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

let private runtimes () = [
    new DirectRuntime() :> FIORuntime
    new CooperativeRuntime() :> FIORuntime
    new ConcurrentRuntime() :> FIORuntime
]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [
        for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> f rt)
    ]

[<Tests>]
let fioTests =
    testList "FIO" [

        testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with success value"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                let! fiber = FIO.succeed(value).Fork()
                let! result = fiber.Join()
                return result
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Forked success effect should complete with the value"

        testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff = fio {
                let! fiber = FIO.fail(err).Fork()
                return fiber
            }

            let fiber = runtime.Run(eff).UnsafeSuccess()

            match fiber.UnsafeResult() with
            | Failed e -> Expect.equal e err "Forked error effect should fail with the error"
            | other -> failtest $"Expected Failed but got: {other}"

        testAllRuntimes "Fork - forked effect executes concurrently" (fun runtime ->
            let eff = fio {
                let chan = Channel<int>()
                let! fiber = (chan.Receive()).Fork()
                do! chan.Send(42).Unit()
                let! result = fiber.Join()
                return result
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 42 "Forked receiver should get message sent after fork")

        testAllRuntimes "Fork - multiple forks execute independently" (fun runtime ->
            let eff = fio {
                let! f1 = FIO.succeed(1).Fork()
                let! f2 = FIO.succeed(2).Fork()
                let! f3 = FIO.succeed(3).Fork()
                let! r1 = f1.Join()
                let! r2 = f2.Join()
                let! r3 = f3.Join()
                return [r1; r2; r3]
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "Each forked fiber should complete independently")

        testPropertyWithConfig fsCheckConfig "FlatMap - chains success continuation"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.succeed(value).FlatMap(fun x -> FIO.succeed(x + 1))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (value + 1) "FlatMap should chain the continuation on success"

        testPropertyWithConfig fsCheckConfig "FlatMap - short-circuits on error"
        <| fun (runtime: FIORuntime, err: string) ->
            let mutable contRan = false
            let eff = FIO.fail(err).FlatMap(fun (_: int) ->
                contRan <- true
                FIO.succeed 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "FlatMap should propagate the error"
            Expect.isFalse contRan "FlatMap continuation should not run on error"

        testPropertyWithConfig fsCheckConfig "FlatMap - can change result type"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.succeed(value).FlatMap(fun x -> FIO.succeed(x.ToString()))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (value.ToString()) "FlatMap should allow changing the result type"

        testAllRuntimes "FlatMap - chains multiple operations" (fun runtime ->
            let eff =
                FIO.succeed(1)
                    .FlatMap(fun x -> FIO.succeed(x + 10))
                    .FlatMap(fun x -> FIO.succeed(x * 2))
                    .FlatMap(fun x -> FIO.succeed(x.ToString()))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "22" "FlatMap chain should compose correctly")

        testPropertyWithConfig fsCheckConfig "FlatMap - error in continuation propagates"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.succeed(value).FlatMap(fun _ -> FIO.fail "boom")

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result "boom" "Error in FlatMap continuation should propagate"

        testPropertyWithConfig fsCheckConfig "CatchAll - recovers from error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail(err).CatchAll(fun e -> FIO.succeed (e * 2))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (err * 2) "CatchAll should recover from the error"

        testPropertyWithConfig fsCheckConfig "CatchAll - does not affect success"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable handlerRan = false
            let eff = FIO.succeed(value).CatchAll(fun (_: string) ->
                handlerRan <- true
                FIO.succeed 0)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "CatchAll should not affect success"
            Expect.isFalse handlerRan "CatchAll handler should not run on success"

        testPropertyWithConfig fsCheckConfig "CatchAll - can change error type"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff =
                FIO.fail(err)
                    .CatchAll(fun e -> FIO.fail(e.ToString()))

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result (err.ToString()) "CatchAll should allow changing the error type"

        testPropertyWithConfig fsCheckConfig "CatchAll - recovery can itself fail"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff =
                FIO.fail(err)
                    .CatchAll(fun _ -> FIO.fail "recovery failed")

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result "recovery failed" "CatchAll recovery failure should propagate"

        testAllRuntimes "Ensuring - runs finalizer on success" (fun runtime ->
            let mutable finalizerRan = false
            let finalizer = FIO.attempt((fun () -> finalizerRan <- true), fun _ -> "")
            let eff = FIO.succeed(42).Ensuring finalizer

            runtime.Run(eff).UnsafeSuccess() |> ignore

            Expect.isTrue finalizerRan "Finalizer should run on success")

        testAllRuntimes "Ensuring - runs finalizer on error" (fun runtime ->
            let mutable finalizerRan = false
            let finalizer = FIO.attempt((fun () -> finalizerRan <- true), fun _ -> "")
            let eff = FIO.fail("boom").Ensuring finalizer

            runtime.Run(eff).UnsafeError() |> ignore

            Expect.isTrue finalizerRan "Finalizer should run on error")

        testPropertyWithConfig fsCheckConfig "Ensuring - preserves success result"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff =
                FIO.succeed(value)
                    .Ensuring(FIO.unit())

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Ensuring should preserve the success result"

        testPropertyWithConfig fsCheckConfig "Ensuring - preserves error when finalizer succeeds"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff =
                FIO.fail(err)
                    .Ensuring(FIO.unit())

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Ensuring should preserve the error"

        testPropertyWithConfig fsCheckConfig "Ensuring - returns finalizer error when main succeeds but finalizer fails"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff =
                FIO.succeed(value)
                    .Ensuring(FIO.fail "finalizer failed")

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result "finalizer failed" "Finalizer error should be returned when main succeeds"
    ]
