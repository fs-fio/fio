module FIO.Tests.FiberTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

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
let fiberTests =
    testList "Fiber" [

        testList "Fiber - Id" [

            testPropertyWithConfig fsCheckConfig "Each fiber has a unique identifier"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio {
                    let! f1 = FIO.succeed(value).Fork()
                    let! f2 = FIO.succeed(value).Fork()
                    let! f3 = FIO.succeed(value).Fork()
                    return f1.Id, f2.Id, f3.Id
                }

                let id1, id2, id3 = runtime.Run(eff).UnsafeSuccess()

                Expect.notEqual id1 id2 "Fiber IDs should be distinct (1 vs 2)"
                Expect.notEqual id2 id3 "Fiber IDs should be distinct (2 vs 3)"
                Expect.notEqual id1 id3 "Fiber IDs should be distinct (1 vs 3)"

            testAllRuntimes "Fiber ID is a non-empty GUID" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)
                let _result = fiber.UnsafeSuccess()

                Expect.notEqual fiber.Id Guid.Empty "Fiber ID should not be empty GUID")
        ]

        testList "Fiber - CancellationToken" [

            testAllRuntimes "Token is not cancelled for active fiber" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.succeed(42).Fork()
                    let! _result = fiber.Join()
                    return fiber.CancellationToken.IsCancellationRequested
                }

                let cancelled = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse cancelled "CancellationToken should not be cancelled for completed fiber")

            testAllRuntimes "Token is cancelled after interruption" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    do! fiber.Interrupt()
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "err")
                    return fiber.CancellationToken.IsCancellationRequested
                }

                let cancelled = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue cancelled "CancellationToken should be cancelled after interruption")
        ]

        testList "Fiber - Completed" [

            testPropertyWithConfig fsCheckConfig "Returns true after fiber finishes"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio {
                    let! fiber = FIO.succeed(value).Fork()
                    let! _result = fiber.Join()
                    return fiber.Completed()
                }

                let completed = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "Completed should be true after fiber finishes"

            testAllRuntimes "Returns true after fiber fails" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.fail("boom").Fork()
                    // Join and catch the error to ensure the fiber has completed
                    let! _result = fiber.Join().CatchAll(fun (_err: string) -> FIO.succeed 0)
                    return fiber.Completed()
                }

                let completed = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "Completed should be true after fiber fails")
        ]

        testList "Fiber - Interrupted" [

            testPropertyWithConfig fsCheckConfig "Returns false for successful fiber"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio {
                    let! fiber = FIO.succeed(value).Fork()
                    let! _result = fiber.Join()
                    return fiber.Interrupted
                }

                let interrupted = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse interrupted "Interrupted should be false for successful fiber"

            testAllRuntimes "Returns true after Interrupt effect" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    do! fiber.Interrupt()
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "err")
                    return fiber.Interrupted
                }

                let interrupted = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue interrupted "Interrupted should be true after Interrupt effect")
        ]

        testList "Fiber - Join" [

            testPropertyWithConfig fsCheckConfig "Awaits and returns successful result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio {
                    let! fiber = FIO.succeed(value).Fork()
                    return! fiber.Join()
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Join should return the fiber's successful result"

            testPropertyWithConfig fsCheckConfig "Propagates failure from forked fiber"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = fio {
                    let! fiber = FIO.fail(err).Fork()
                    return! fiber.Join()
                }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Join should propagate the fiber's error"

            testPropertyWithConfig fsCheckConfig "Fork then join equals identity"
            <| fun (runtime: FIORuntime, value: int) ->
                let direct = runtime.Run(FIO.succeed value).UnsafeSuccess()

                let eff = fio {
                    let! fiber = FIO.succeed(value).Fork()
                    return! fiber.Join()
                }
                let viaFork = runtime.Run(eff).UnsafeSuccess()

                Expect.equal viaFork direct "Fork then Join should equal identity"
        ]

        testList "Fiber - Task" [

            testPropertyWithConfig fsCheckConfig "Returns Succeeded for successful fiber"
            <| fun (runtime: FIORuntime, value: int) ->
                let fiber = runtime.Run(FIO.succeed value)
                let result = fiber.Task().Result

                match result with
                | Succeeded r -> Expect.equal r value "Task should return Succeeded with value"
                | Failed _ -> failtest "Expected Succeeded but got Failed"
                | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

            testPropertyWithConfig fsCheckConfig "Returns Failed for failed fiber"
            <| fun (runtime: FIORuntime, err: string) ->
                let fiber = runtime.Run(FIO.fail err)
                let result = fiber.Task().Result

                match result with
                | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                | Failed e -> Expect.equal e err "Task should return Failed with error"
                | Interrupted _ -> failtest "Expected Failed but got Interrupted"
        ]

        testList "Fiber - UnsafeResult" [

            testPropertyWithConfig fsCheckConfig "Returns Succeeded for success"
            <| fun (runtime: FIORuntime, value: int) ->
                let fiber = runtime.Run(FIO.succeed value)
                let result = fiber.UnsafeResult()

                match result with
                | Succeeded r -> Expect.equal r value "UnsafeResult should return Succeeded"
                | _ -> failtest $"Expected Succeeded, got {result}"

            testPropertyWithConfig fsCheckConfig "Returns Failed for failure"
            <| fun (runtime: FIORuntime, err: string) ->
                let fiber = runtime.Run(FIO.fail err)
                let result = fiber.UnsafeResult()

                match result with
                | Failed e -> Expect.equal e err "UnsafeResult should return Failed"
                | _ -> failtest $"Expected Failed, got {result}"

            testAllRuntimes "Returns Interrupted for interrupted fiber" (fun runtime ->
                let fiber = runtime.Run(FIO.interrupt(ExplicitInterrupt, "test"))
                let result = fiber.UnsafeResult()

                match result with
                | Interrupted ex ->
                    Expect.equal ex.cause ExplicitInterrupt "UnsafeResult should return Interrupted with cause"
                | _ -> failtest $"Expected Interrupted, got {result}")
        ]

        testList "Fiber - UnsafeSuccess" [

            testPropertyWithConfig fsCheckConfig "Returns value on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let fiber = runtime.Run(FIO.succeed value)
                let result = fiber.UnsafeSuccess()

                Expect.equal result value "UnsafeSuccess should return the success value"

            testAllRuntimes "Throws InvalidOperationException on failure" (fun runtime ->
                let fiber = runtime.Run(FIO.fail "boom")

                Expect.throwsT<InvalidOperationException>
                    (fun () -> fiber.UnsafeSuccess() |> ignore)
                    "UnsafeSuccess should throw on failure")
        ]

        testList "Fiber - UnsafeError" [

            testPropertyWithConfig fsCheckConfig "Returns error on failure"
            <| fun (runtime: FIORuntime, err: string) ->
                let fiber = runtime.Run(FIO.fail err)
                let result = fiber.UnsafeError()

                Expect.equal result err "UnsafeError should return the error value"

            testAllRuntimes "Throws InvalidOperationException on success" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)

                Expect.throwsT<InvalidOperationException>
                    (fun () -> fiber.UnsafeError() |> ignore)
                    "UnsafeError should throw on success")
        ]

        testList "Fiber - UnsafePrintResult" [

            testAllRuntimes "Does not throw on success" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)

                // UnsafePrintResult should complete without throwing
                fiber.UnsafePrintResult()

                Expect.isTrue (fiber.Completed()) "Fiber should be completed after UnsafePrintResult")
        ]

        testList "Fiber - UnsafeComplete" [

            testPropertyWithConfig fsCheckConfig "Completes fiber with Ok result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    return fiber
                }

                let parentFiber = runtime.Run eff
                let childFiber = parentFiber.UnsafeSuccess()
                childFiber.UnsafeComplete(Ok value)
                let result = childFiber.UnsafeResult()

                match result with
                | Succeeded r -> Expect.equal r value "UnsafeComplete Ok should set Succeeded result"
                | _ -> failtest $"Expected Succeeded, got {result}"

            testAllRuntimes "Completes fiber with Error result" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    return fiber
                }

                let parentFiber = runtime.Run eff
                let childFiber = parentFiber.UnsafeSuccess()
                childFiber.UnsafeComplete(Error "completed-error")
                let result = childFiber.UnsafeResult()

                match result with
                | Failed e -> Expect.equal e "completed-error" "UnsafeComplete Error should set Failed result"
                | _ -> failtest $"Expected Failed, got {result}")
        ]

        testList "Fiber - UnsafeInterrupt" [

            testAllRuntimes "Interrupts fiber from outside effect system" (fun runtime ->
                let fiber = runtime.Run(FIO.never())
                fiber.UnsafeInterrupt()
                let result = fiber.UnsafeResult()

                match result with
                | Interrupted ex ->
                    Expect.equal ex.cause ExplicitInterrupt "Default cause should be ExplicitInterrupt"
                | _ -> failtest $"Expected Interrupted, got {result}")

            testAllRuntimes "Accepts custom cause and message" (fun runtime ->
                let fiber = runtime.Run(FIO.never())
                fiber.UnsafeInterrupt(cause = Timeout 5000.0, msg = "timed out")
                let result = fiber.UnsafeResult()

                match result with
                | Interrupted ex ->
                    match ex.cause with
                    | Timeout ms -> Expect.equal ms 5000.0 "Timeout duration should match"
                    | other -> failtest $"Expected Timeout cause, got {other}"
                | _ -> failtest $"Expected Interrupted, got {result}")
        ]

        testList "Fiber - Interrupt" [

            testAllRuntimes "Effect-based interruption marks fiber as interrupted" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    do! fiber.Interrupt()
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "err")
                    return fiber.Interrupted
                }

                let interrupted = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue interrupted "Interrupt effect should mark fiber as interrupted")

            testAllRuntimes "Custom cause propagates cause to FiberResult" (fun runtime ->
                let eff = fio {
                    let! fiber = FIO.never().Fork()
                    do! fiber.Interrupt(cause = ResourceExhaustion "out of memory")
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "err")
                    return fiber
                }

                let parentFiber = runtime.Run eff
                let childFiber = parentFiber.UnsafeSuccess()
                let result = childFiber.UnsafeResult()

                match result with
                | Interrupted ex ->
                    match ex.cause with
                    | ResourceExhaustion reason ->
                        Expect.equal reason "out of memory" "Custom cause should propagate"
                    | other -> failtest $"Expected ResourceExhaustion, got {other}"
                | _ -> failtest $"Expected Interrupted, got {result}")
        ]

        testList "Fiber - ToString" [

            testAllRuntimes "Returns string representation of fiber ID" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)
                let _result = fiber.UnsafeSuccess()

                let str = fiber.ToString()

                Expect.equal str (fiber.Id.ToString()) "ToString should return the fiber's ID as string")
        ]

        testList "Fiber - IDisposable" [

            testAllRuntimes "Fiber can be disposed after completion" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)
                let _result = fiber.UnsafeSuccess()

                (fiber :> IDisposable).Dispose()

                // Disposing should not throw; fiber is already completed
                Expect.isTrue true "Dispose should not throw on completed fiber")

            testAllRuntimes "Double dispose does not throw" (fun runtime ->
                let fiber = runtime.Run(FIO.succeed 42)
                let _result = fiber.UnsafeSuccess()

                (fiber :> IDisposable).Dispose()
                (fiber :> IDisposable).Dispose()

                Expect.isTrue true "Double dispose should not throw")
        ]

        testList "FiberResult - Pattern Matching" [

            testPropertyWithConfig fsCheckConfig "Succeeded - pattern matches success result"
            <| fun (runtime: FIORuntime, value: int) ->
                let fiber = runtime.Run(FIO.succeed value)
                let result = fiber.UnsafeResult()

                match result with
                | Succeeded r -> Expect.equal r value "Succeeded should contain the result"
                | Failed _ -> failtest "Expected Succeeded but got Failed"
                | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

            testPropertyWithConfig fsCheckConfig "Failed - pattern matches failure result"
            <| fun (runtime: FIORuntime, err: string) ->
                let fiber = runtime.Run(FIO.fail err)
                let result = fiber.UnsafeResult()

                match result with
                | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                | Failed e -> Expect.equal e err "Failed should contain the error"
                | Interrupted _ -> failtest "Expected Failed but got Interrupted"

            testAllRuntimes "Interrupted - pattern matches interrupted result with cause" (fun runtime ->
                let fiber = runtime.Run(FIO.interrupt(ExplicitInterrupt, "test interruption"))
                let result = fiber.UnsafeResult()

                match result with
                | Succeeded _ -> failtest "Expected Interrupted but got Succeeded"
                | Failed _ -> failtest "Expected Interrupted but got Failed"
                | Interrupted ex ->
                    Expect.equal ex.cause ExplicitInterrupt "Interrupted should contain ExplicitInterrupt cause")

            testAllRuntimes "InterruptionCause - all cause variants can be matched" (fun runtime ->
                let causes = [
                    Timeout 1000.0
                    ExplicitInterrupt
                    InvalidArgument("arg", "bad value")
                    ResourceExhaustion "out of memory"
                ]

                for cause in causes do
                    let fiber = runtime.Run(FIO.interrupt(cause, "test"))
                    let result = fiber.UnsafeResult()

                    match result with
                    | Interrupted ex -> Expect.equal ex.cause cause $"Cause {cause} should round-trip"
                    | _ -> failtest $"Expected Interrupted for cause {cause}")
        ]
    ]
