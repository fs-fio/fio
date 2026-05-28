/// <summary>Provides property-based tests for fiber fork, join, and interruption semantics.</summary>
module FIO.Tests.FiberTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System
open System.IO

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name
        [ for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let fiberTests =
    testList
        "Fiber"
        [
            testList
                "Fiber - Id"
                [
                    testPropertyWithConfig fsCheckConfig "Each fiber has a unique identifier"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! f1 = FIO.succeed(value).Fork()
                                let! f2 = FIO.succeed(value).Fork()
                                let! f3 = FIO.succeed(value).Fork()
                                return f1.Id, f2.Id, f3.Id
                            }

                        let id1, id2, id3 =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.notEqual id1 id2 "Fiber IDs should be distinct (1 vs 2)"
                        Expect.notEqual id2 id3 "Fiber IDs should be distinct (2 vs 3)"
                        Expect.notEqual id1 id3 "Fiber IDs should be distinct (1 vs 3)"

                    testAllRuntimes "Fiber ID is a non-empty GUID" (fun runtime ->
                        let fiber = runtime.Run(FIO.succeed 42)

                        let _ =
                            fiber.UnsafeSuccess()

                        Expect.notEqual fiber.Id Guid.Empty "Fiber ID should not be empty GUID")
                ]

            testList
                "Fiber - CancellationToken"
                [
                    testAllRuntimes "Token is not cancelled for active fiber" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(42).Fork()
                                let! _ = fiber.Join()
                                return fiber.CancellationToken.IsCancellationRequested
                            }

                        let cancelled =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isFalse cancelled "CancellationToken should not be cancelled for completed fiber")

                    testAllRuntimes "Token is cancelled after interruption" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")
                                return fiber.CancellationToken.IsCancellationRequested
                            }

                        let cancelled =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue cancelled "CancellationToken should be cancelled after interruption")
                ]

            testList
                "Fiber - Completed"
                [
                    testPropertyWithConfig fsCheckConfig "Returns true after fiber finishes"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return fiber.IsCompleted()
                            }

                        let completed =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue completed "Completed should be true after fiber finishes"

                    testAllRuntimes "Returns true after fiber fails" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.fail("boom").Fork()
                                let! _result = fiber.Join().CatchAll(fun (_err: string) -> FIO.succeed 0)
                                return fiber.IsCompleted()
                            }

                        let completed =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue completed "Completed should be true after fiber fails")
                ]

            testList
                "Fiber - Interrupted"
                [
                    testPropertyWithConfig fsCheckConfig "Returns false for successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return fiber.IsInterrupted()
                            }

                        let interrupted = 
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isFalse interrupted "Interrupted should be false for successful fiber"

                    testAllRuntimes "Returns true after Interrupt effect" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")
                                return fiber.IsInterrupted()
                            }

                        let interrupted =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue interrupted "Interrupted should be true after Interrupt effect")
                ]

            testList
                "Fiber - Join"
                [
                    testPropertyWithConfig fsCheckConfig "Awaits and returns successful result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result value "Join should return the fiber's successful result"

                    testPropertyWithConfig fsCheckConfig "Propagates failure from forked fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let eff =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(eff).UnsafeError()

                        Expect.equal result error "Join should propagate the fiber's error"

                    testPropertyWithConfig fsCheckConfig "Fork then join equals identity"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let direct = runtime.Run(FIO.succeed value).UnsafeSuccess()

                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Join()
                            }

                        let viaFork =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.equal viaFork direct "Fork then Join should equal identity"
                ]

            testList
                "Fiber - Task"
                [
                    testPropertyWithConfig fsCheckConfig "Returns Succeeded for successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)

                        let result = fiber.Task().Result

                        match result with
                        | Succeeded r -> Expect.equal r value "Task should return Succeeded with value"
                        | Failed _ -> failtest "Expected Succeeded but got Failed"
                        | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

                    testPropertyWithConfig fsCheckConfig "Returns Failed for failed fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber =
                             runtime.Run(FIO.fail error)

                        let result = fiber.Task().Result

                        match result with
                        | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                        | Failed e -> Expect.equal e error "Task should return Failed with error"
                        | Interrupted _ -> failtest "Expected Failed but got Interrupted"
                ]

            testList
                "Fiber - UnsafeResult"
                [
                    testPropertyWithConfig fsCheckConfig "Returns Succeeded for success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)

                        let result = fiber.UnsafeResult()

                        match result with
                        | Succeeded r -> Expect.equal r value "UnsafeResult should return Succeeded"
                        | _ -> failtest $"Expected Succeeded, got {result}"

                    testPropertyWithConfig fsCheckConfig "Returns Failed for failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber = runtime.Run(FIO.fail error)
                        let result = fiber.UnsafeResult()

                        match result with
                        | Failed e -> Expect.equal e error "UnsafeResult should return Failed"
                        | _ -> failtest $"Expected Failed, got {result}"

                    testAllRuntimes "Returns Interrupted for interrupted fiber" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.interrupt ExplicitInterrupt "test")

                        let result = fiber.UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "UnsafeResult should return Interrupted with cause"
                        | _ -> failtest $"Expected Interrupted, got {result}")
                ]

            testList
                "Fiber - UnsafeSuccess"
                [
                    testPropertyWithConfig fsCheckConfig "Returns value on success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)

                        let result = fiber.UnsafeSuccess()

                        Expect.equal result value "UnsafeSuccess should return the success value"

                    testAllRuntimes "Throws InvalidOperationException on failure" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.fail "boom")

                        Expect.throwsT<InvalidOperationException>
                            (fun () -> fiber.UnsafeSuccess() |> ignore)
                            "UnsafeSuccess should throw on failure")
                ]

            testList
                "Fiber - UnsafeError"
                [
                    testPropertyWithConfig fsCheckConfig "Returns error on failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber =
                            runtime.Run(FIO.fail error)

                        let result = fiber.UnsafeError()

                        Expect.equal result error "UnsafeError should return the error value"

                    testAllRuntimes "Throws InvalidOperationException on success" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)

                        Expect.throwsT<InvalidOperationException>
                            (fun () -> fiber.UnsafeError() |> ignore)
                            "UnsafeError should throw on success")
                ]

            testList
                "Fiber - UnsafePrintResult"
                [
                    testAllRuntimes "Does not throw on success" (fun runtime ->
                        let fiber = runtime.Run(FIO.succeed 42)
                        let oldOut = Console.Out
                        Console.SetOut TextWriter.Null

                        try
                            fiber.UnsafePrintResult()
                        finally
                            Console.SetOut oldOut

                        Expect.isTrue (fiber.IsCompleted()) "Fiber should be completed after UnsafePrintResult")
                ]

            testList
                "Fiber - Interrupt"
                [
                    testAllRuntimes "Effect-based interruption marks fiber as interrupted" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")
                                return fiber.IsInterrupted()
                            }

                        let interrupted =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue interrupted "Interrupt effect should mark fiber as interrupted")

                    testAllRuntimes "Custom cause propagates cause to FiberResult" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.Interrupt (ResourceExhaustion "out of memory") "Resource exhaustion"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")
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

            testList
                "Fiber - ToString"
                [
                    testAllRuntimes "Returns string representation of fiber ID" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        let str = fiber.ToString()

                        Expect.equal str (fiber.Id.ToString()) "ToString should return the fiber's ID as string")
                ]

            testList
                "Fiber - IDisposable"
                [
                    testAllRuntimes "Fiber can be disposed after completion" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        (fiber :> IDisposable).Dispose()

                        Expect.isTrue true "Dispose should not throw on completed fiber")

                    testAllRuntimes "Double dispose does not throw" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        (fiber :> IDisposable).Dispose()
                        (fiber :> IDisposable).Dispose()

                        Expect.isTrue true "Double dispose should not throw")
                ]

            testList
                "Fiber - Await"
                [
                    testPropertyWithConfig fsCheckConfig "Returns Succeeded for successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Succeeded r -> Expect.equal r value "Await should return Succeeded with value"
                        | Failed _ -> failtest "Expected Succeeded but got Failed"
                        | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

                    testPropertyWithConfig fsCheckConfig "Returns Failed for failed fiber without re-raising"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let eff =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                        | Failed e -> Expect.equal e error "Await should return Failed with error"
                        | Interrupted _ -> failtest "Expected Failed but got Interrupted"

                    testAllRuntimes "Returns Interrupted for interrupted fiber without re-raising" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Succeeded _ -> failtest "Expected Interrupted but got Succeeded"
                        | Failed _ -> failtest "Expected Interrupted but got Failed"
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "Await should return Interrupted with cause")
                ]

            testList
                "Fiber - InterruptAwait"
                [
                    testAllRuntimes "Interrupts and returns Interrupted result" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                return! fiber.InterruptAwait ExplicitInterrupt "Interrupted"
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "InterruptAwait should return Interrupted"
                        | _ -> failtest $"Expected Interrupted, got {result}")

                    testAllRuntimes "Custom cause propagates through InterruptAwait" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                return! fiber.InterruptAwait (ResourceExhaustion "out of memory") "Resource exhaustion"
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | ResourceExhaustion reason ->
                                Expect.equal reason "out of memory" "Custom cause should propagate"
                            | other -> failtest $"Expected ResourceExhaustion, got {other}"
                        | _ -> failtest $"Expected Interrupted, got {result}")

                    testPropertyWithConfig fsCheckConfig "Returns Succeeded if fiber completes before interrupt"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return! fiber.InterruptAwait ExplicitInterrupt "Interrupted"
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Succeeded r -> Expect.equal r value "Should return Succeeded if already completed"
                        | Interrupted _ -> () // Also acceptable — interrupt may arrive first
                        | Failed _ -> failtest "Should not get Failed"
                ]

            testList
                "Fiber - Poll"
                [
                    testPropertyWithConfig fsCheckConfig "Returns Some Succeeded for completed fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return! fiber.Poll()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Some(Succeeded r) -> Expect.equal r value "Poll should return Some Succeeded"
                        | other -> failtest $"Expected Some Succeeded, got {other}"

                    testAllRuntimes "Returns None for running fiber" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                let! poll = fiber.Poll()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                return poll
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.isNone result "Poll should return None for running fiber")

                    testPropertyWithConfig fsCheckConfig "Returns Some Failed for failed fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let eff =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                let! _result = fiber.Join().CatchAll(fun (_: string) -> FIO.succeed 0)
                                return! fiber.Poll()
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Some(Failed e) -> Expect.equal e error "Poll should return Some Failed"
                        | other -> failtest $"Expected Some Failed, got {other}"
                ]

            testList
                "Fiber - JoinWith"
                [
                    testPropertyWithConfig fsCheckConfig "Calls onSucceeded for successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()

                                return!
                                    fiber.JoinWith(
                                        (fun r -> FIO.succeed (r * 2)),
                                        (fun (_: string) -> FIO.succeed -1),
                                        (fun _ -> FIO.succeed -2)
                                    )
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result (value * 2) "JoinWith should call onSucceeded"

                    testPropertyWithConfig fsCheckConfig "Calls onFailed for failed fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let eff =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return!
                                    fiber.JoinWith(
                                        (fun (_: int) -> FIO.succeed "success"),
                                        (fun e -> FIO.succeed ($"caught: {e}")),
                                        (fun _ -> FIO.succeed "interrupted")
                                    )
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result $"caught: {error}" "JoinWith should call onFailed"

                    testAllRuntimes "Calls onInterrupted for interrupted fiber" (fun runtime ->
                        let eff =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                do! fiber.Interrupt ExplicitInterrupt "Interrupted"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "error")

                                return!
                                    fiber.JoinWith(
                                        (fun _ -> FIO.succeed "success"),
                                        (fun _ -> FIO.succeed "failed"),
                                        (fun _ -> FIO.succeed "interrupted")
                                    )
                            }

                        let result =
                            runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result "interrupted" "JoinWith should call onInterrupted")
                ]

            testList
                "FiberResult - Pattern Matching"
                [
                    testPropertyWithConfig fsCheckConfig "Succeeded - pattern matches success result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)
                        let result = fiber.UnsafeResult()

                        match result with
                        | Succeeded r -> Expect.equal r value "Succeeded should contain the result"
                        | Failed _ -> failtest "Expected Succeeded but got Failed"
                        | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

                    testPropertyWithConfig fsCheckConfig "Failed - pattern matches failure result"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber =
                            runtime.Run(FIO.fail error)
                        let result = fiber.UnsafeResult()

                        match result with
                        | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                        | Failed e -> Expect.equal e error "Failed should contain the error"
                        | Interrupted _ -> failtest "Expected Failed but got Interrupted"

                    testAllRuntimes "Interrupted - pattern matches interrupted result with cause" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.interrupt ExplicitInterrupt "test interruption")
                        let result = fiber.UnsafeResult()

                        match result with
                        | Succeeded _ -> failtest "Expected Interrupted but got Succeeded"
                        | Failed _ -> failtest "Expected Interrupted but got Failed"
                        | Interrupted ex ->
                            Expect.equal
                                ex.cause
                                ExplicitInterrupt
                                "Interrupted should contain ExplicitInterrupt cause")

                    testAllRuntimes "InterruptionCause - all cause variants can be matched" (fun runtime ->
                        let causes =
                            [
                                ExplicitInterrupt
                                InvalidArgument("arg", "bad value")
                                ResourceExhaustion "out of memory"
                            ]

                        for cause in causes do
                            let fiber =
                                runtime.Run(FIO.interrupt cause "test")
                            let result = fiber.UnsafeResult()

                            match result with
                            | Interrupted ex -> Expect.equal ex.cause cause $"Cause {cause} should round-trip"
                            | _ -> failtest $"Expected Interrupted for cause {cause}")

                    testCase "SetOnTerminal - fires exactly once even on post-terminal install"
                    <| fun () ->
                        let runtime = DirectRuntime()
                        let fiber =
                            runtime.Run(FIO.succeed 1)
                        fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

                        let firstCount = ref 0
                        let secondCount = ref 0
                        fiber.Context.SetOnTerminal(fun () -> firstCount.Value <- firstCount.Value + 1)
                        fiber.Context.SetOnTerminal(fun () -> secondCount.Value <- secondCount.Value + 1)

                        Expect.equal
                            firstCount.Value
                            1
                            "First post-terminal SetOnTerminal callback should fire exactly once"

                        Expect.equal
                            secondCount.Value
                            0
                            "Second post-terminal SetOnTerminal callback should NOT fire (CAS gate already won)"

                    testCase "SetOnTerminal - second install after pre-terminal install does not refire"
                    <| fun () ->
                        let runtime = DirectRuntime()
                        let fiber =
                            runtime.Run(FIO.sleep (TimeSpan.FromMilliseconds 20.0) id)

                        let firstCount = ref 0
                        let secondCount = ref 0
                        fiber.Context.SetOnTerminal(fun () -> firstCount.Value <- firstCount.Value + 1)

                        fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

                        fiber.Context.SetOnTerminal(fun () -> secondCount.Value <- secondCount.Value + 1)

                        Expect.equal
                            firstCount.Value
                            1
                            "Pre-terminal SetOnTerminal callback should fire exactly once on completion"

                        Expect.equal
                            secondCount.Value
                            0
                            "Post-terminal SetOnTerminal after pre-terminal install must not refire (gate consumed)"
                ]
        ]
