module FIO.Tests.FiberTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct

open Expecto

open System
open System.IO

[<Tests>]
let fiberTests =
    testList
        "Fiber"
        [
            testList
                "Fiber - Id"
                [
                    testPropertyWithConfig fsCheckConfig "Id - is unique per fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! f1 = FIO.succeed(value).Fork()
                                let! f2 = FIO.succeed(value).Fork()
                                let! f3 = FIO.succeed(value).Fork()
                                return f1.Id, f2.Id, f3.Id
                            }

                        let id1, id2, id3 =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.notEqual id1 id2 "Fiber IDs should be distinct (1 vs 2)"
                        Expect.notEqual id2 id3 "Fiber IDs should be distinct (2 vs 3)"
                        Expect.notEqual id1 id3 "Fiber IDs should be distinct (1 vs 3)"

                    testAllRuntimes "Id - is a non-empty GUID" (fun runtime ->
                        let fiber = runtime.Run(FIO.succeed 42)
                        let _ =
                            fiber.UnsafeSuccess()

                        Expect.notEqual fiber.Id Guid.Empty "Fiber ID should not be empty GUID")
                ]

            testList
                "Fiber - CancellationToken"
                [
                    testAllRuntimes "CancellationToken - is not cancelled for a fiber that completed normally" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(42).Fork()
                                let! _ = fiber.Join()
                                return fiber.CancellationToken.IsCancellationRequested
                            }

                        let cancelled =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isFalse cancelled "CancellationToken should not be cancelled for completed fiber")

                    testAllRuntimes "CancellationToken - is cancelled after interruption" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.InterruptNow ()
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")
                                return fiber.CancellationToken.IsCancellationRequested
                            }

                        let cancelled =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue cancelled "CancellationToken should be cancelled after interruption")
                ]

            testList
                "Fiber - Task"
                [
                    testPropertyWithConfig fsCheckConfig "Task - returns Succeeded for a successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)
                        let result = fiber.Task().Result

                        match result with
                        | Succeeded r -> Expect.equal r value "Task should return Succeeded with value"
                        | Failed _ -> failtest "Expected Succeeded but got Failed"
                        | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

                    testPropertyWithConfig fsCheckConfig "Task - returns Failed for a failed fiber"
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
                "Fiber - Join"
                [
                    testPropertyWithConfig fsCheckConfig "Join - awaits and returns the successful result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "Join should return the fiber's successful result"

                    testPropertyWithConfig fsCheckConfig "Join - propagates the failure of the forked fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return! fiber.Join()
                            }

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "Join should propagate the fiber's error"

                    testPropertyWithConfig fsCheckConfig "Join - fork then join equals identity"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Join()
                            }

                        let direct = runtime.Run(FIO.succeed value).UnsafeSuccess()
                        let viaFork =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal viaFork direct "Fork then Join should equal identity"
                ]

            testList
                "Fiber - Interrupt"
                [
                    testAllRuntimes "Interrupt - marks the fiber as interrupted" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.InterruptNow ()
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")
                                return fiber.IsInterrupted()
                            }

                        let interrupted =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue interrupted "Interrupt effect should mark fiber as interrupted")

                    testAllRuntimes "Interrupt - propagates a custom cause to FiberResult" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.Interrupt (ResourceExhaustion "out of memory") "Resource exhaustion"
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")
                                return fiber
                            }

                        let parentFiber = runtime.Run effect
                        let childFiber = parentFiber.UnsafeSuccess()
                        let result = childFiber.UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | ResourceExhaustion reason ->
                                Expect.equal reason "out of memory" "Custom cause should propagate"
                            | other -> failtest $"Expected ResourceExhaustion, got {other}"
                        | _ -> failtest $"Expected Interrupted, got {result}")

                    testAllRuntimes "Interrupt - succeeds and still interrupts the fiber's children when a cancellation callback throws" (fun runtime ->
                        let childFinalized = new Threading.ManualResetEventSlim(false)
                        let registered = new Threading.ManualResetEventSlim(false)
                        let parent =
                            fio {
                                let! _ = (FIO.never<unit, string>().Ensuring(FIO.succeedWith (fun () -> childFinalized.Set()))).Fork()
                                let! token = FIO.cancellationToken ()

                                do! FIO.succeedWith (fun () ->
                                        token.Register(fun () -> failwith "A cancellation callback threw.") |> ignore
                                        registered.Set())

                                return! FIO.never<unit, string> ()
                            }

                        let fiber = runtime.Run parent

                        Expect.isTrue (registered.Wait(TimeSpan.FromSeconds 5.0)) "The parent should register its callback"

                        let interruption = runtime.Run(fiber.InterruptNow()).UnsafeResult()

                        match interruption with
                        | Succeeded () -> ()
                        | other -> failtest $"Interrupting must succeed even when a callback throws, got {other}"
                        Expect.isTrue (childFinalized.Wait(TimeSpan.FromSeconds 5.0)) "The forked child must be interrupted and finalized"
                        Expect.isTrue (fiber.IsInterrupted()) "The parent must be interrupted")
                ]

            testList
                "Fiber - Await"
                [
                    testPropertyWithConfig fsCheckConfig "Await - returns Succeeded for a successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Succeeded r -> Expect.equal r value "Await should return Succeeded with value"
                        | Failed _ -> failtest "Expected Succeeded but got Failed"
                        | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

                    testPropertyWithConfig fsCheckConfig "Await - returns Failed for a failed fiber without re-raising"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Succeeded _ -> failtest "Expected Failed but got Succeeded"
                        | Failed e -> Expect.equal e error "Await should return Failed with error"
                        | Interrupted _ -> failtest "Expected Failed but got Interrupted"

                    testAllRuntimes "Await - returns Interrupted for an interrupted fiber without re-raising" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                do! fiber.InterruptNow ()
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")
                                return! fiber.Await()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Succeeded _ -> failtest "Expected Interrupted but got Succeeded"
                        | Failed _ -> failtest "Expected Interrupted but got Failed"
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "Await should return Interrupted with cause")
                ]

            testList
                "Fiber - InterruptAwait"
                [
                    testAllRuntimes "InterruptAwait - interrupts and returns the Interrupted result" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                return! fiber.InterruptAwaitNow ()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "InterruptAwait should return Interrupted"
                        | _ -> failtest $"Expected Interrupted, got {result}")

                    testAllRuntimes "InterruptAwait - propagates a custom cause" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                return! fiber.InterruptAwait (ResourceExhaustion "out of memory") "Resource exhaustion"
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | ResourceExhaustion reason ->
                                Expect.equal reason "out of memory" "Custom cause should propagate"
                            | other -> failtest $"Expected ResourceExhaustion, got {other}"
                        | _ -> failtest $"Expected Interrupted, got {result}")

                    testPropertyWithConfig fsCheckConfig "InterruptAwait - returns Succeeded when the fiber completes before the interrupt"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return! fiber.InterruptAwaitNow ()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Succeeded r -> Expect.equal r value "Should return Succeeded if already completed"
                        | Interrupted _ -> () // Also acceptable — interrupt may arrive first
                        | Failed _ -> failtest "Should not get Failed"
                ]

            testList
                "Fiber - Poll"
                [
                    testPropertyWithConfig fsCheckConfig "Poll - returns Some Succeeded for a completed fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return! fiber.Poll()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Some(Succeeded r) -> Expect.equal r value "Poll should return Some Succeeded"
                        | other -> failtest $"Expected Some Succeeded, got {other}"

                    testAllRuntimes "Poll - returns None for a running fiber" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                let! poll = fiber.Poll()
                                do! fiber.InterruptNow ()
                                return poll
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isNone result "Poll should return None for running fiber")

                    testPropertyWithConfig fsCheckConfig "Poll - returns Some Failed for a failed fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                let! _result = fiber.Join().CatchAll(fun (_: string) -> FIO.succeed 0)
                                return! fiber.Poll()
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Some(Failed e) -> Expect.equal e error "Poll should return Some Failed"
                        | other -> failtest $"Expected Some Failed, got {other}"
                ]

            testList
                "Fiber - JoinWith"
                [
                    testPropertyWithConfig fsCheckConfig "JoinWith - calls onSucceeded for a successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()

                                return!
                                    fiber.JoinWith
                                        (fun r -> FIO.succeed (r * 2))
                                        (fun (_: string) -> FIO.succeed -1)
                                        (fun _ -> FIO.succeed -2)
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (value * 2) "JoinWith should call onSucceeded"

                    testPropertyWithConfig fsCheckConfig "JoinWith - calls onFailed for a failed fiber"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail(error).Fork()
                                return!
                                    fiber.JoinWith
                                        (fun (_: int) -> FIO.succeed "success")
                                        (fun e -> FIO.succeed ($"caught: {e}"))
                                        (fun _ -> FIO.succeed "interrupted")
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result $"caught: {error}" "JoinWith should call onFailed"

                    testAllRuntimes "JoinWith - calls onInterrupted for an interrupted fiber" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never<int, string>().Fork()
                                do! fiber.InterruptNow ()
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")

                                return!
                                    fiber.JoinWith
                                        (fun _ -> FIO.succeed "success")
                                        (fun _ -> FIO.succeed "failed")
                                        (fun _ -> FIO.succeed "interrupted")
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "interrupted" "JoinWith should call onInterrupted")
                ]

            testList
                "Fiber - Completed"
                [
                    testPropertyWithConfig fsCheckConfig "Completed - returns true after the fiber finishes"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return fiber.IsCompleted()
                            }

                        let completed =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue completed "Completed should be true after fiber finishes"

                    testAllRuntimes "Completed - returns true after the fiber fails" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.fail("boom").Fork()
                                let! _result = fiber.Join().CatchAll(fun (_err: string) -> FIO.succeed 0)
                                return fiber.IsCompleted()
                            }

                        let completed =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue completed "Completed should be true after fiber fails")
                ]

            testList
                "Fiber - Interrupted"
                [
                    testPropertyWithConfig fsCheckConfig "Interrupted - returns false for a successful fiber"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            fio {
                                let! fiber = FIO.succeed(value).Fork()
                                let! _result = fiber.Join()
                                return fiber.IsInterrupted()
                            }

                        let interrupted =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isFalse interrupted "Interrupted should be false for successful fiber"

                    testAllRuntimes "Interrupted - returns true after the Interrupt effect" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.never().Fork()
                                do! fiber.InterruptNow ()
                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "error")
                                return fiber.IsInterrupted()
                            }

                        let interrupted =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue interrupted "Interrupted should be true after Interrupt effect")
                ]

            testList
                "Fiber - UnsafeResult"
                [
                    testPropertyWithConfig fsCheckConfig "UnsafeResult - returns Succeeded for a success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)
                        let result = fiber.UnsafeResult()

                        match result with
                        | Succeeded r -> Expect.equal r value "UnsafeResult should return Succeeded"
                        | _ -> failtest $"Expected Succeeded, got {result}"

                    testPropertyWithConfig fsCheckConfig "UnsafeResult - returns Failed for a failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber = runtime.Run(FIO.fail error)
                        let result = fiber.UnsafeResult()

                        match result with
                        | Failed e -> Expect.equal e error "UnsafeResult should return Failed"
                        | _ -> failtest $"Expected Failed, got {result}"

                    testAllRuntimes "UnsafeResult - returns Interrupted for an interrupted fiber" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.interruptNow ())
                        let result = fiber.UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "UnsafeResult should return Interrupted with cause"
                        | _ -> failtest $"Expected Interrupted, got {result}")
                ]

            testList
                "Fiber - UnsafeSuccess"
                [
                    testPropertyWithConfig fsCheckConfig "UnsafeSuccess - returns the value on success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber =
                            runtime.Run(FIO.succeed value)
                        let result = fiber.UnsafeSuccess()

                        Expect.equal result value "UnsafeSuccess should return the success value"

                    testAllRuntimes "UnsafeSuccess - throws InvalidOperationException on failure" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.fail "boom")

                        Expect.throwsT<InvalidOperationException>
                            (fun () -> fiber.UnsafeSuccess() |> ignore)
                            "UnsafeSuccess should throw on failure")

                    testAllRuntimes "UnsafeSuccess - throws InvalidOperationException for an interrupted fiber" (fun runtime ->
                        let fiber = runtime.Run(FIO.interrupt<int, string> ExplicitInterrupt "stopped")
                        let thrown = (try fiber.UnsafeSuccess() |> ignore; None with ex -> Some ex)

                        match thrown with
                        | Some(:? InvalidOperationException as ex) -> Expect.stringContains ex.Message "Fiber was interrupted" "The message should say why"
                        | other -> failtest $"Expected InvalidOperationException, got {other}")
                ]

            testList
                "Fiber - UnsafeError"
                [
                    testPropertyWithConfig fsCheckConfig "UnsafeError - returns the error on failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let fiber =
                            runtime.Run(FIO.fail error)
                        let result = fiber.UnsafeError()

                        Expect.equal result error "UnsafeError should return the error value"

                    testAllRuntimes "UnsafeError - throws InvalidOperationException on success" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)

                        Expect.throwsT<InvalidOperationException>
                            (fun () -> fiber.UnsafeError() |> ignore)
                            "UnsafeError should throw on success")

                    testAllRuntimes "UnsafeError - throws InvalidOperationException for an interrupted fiber" (fun runtime ->
                        let fiber = runtime.Run(FIO.interrupt<int, string> ExplicitInterrupt "stopped")
                        let thrown = try fiber.UnsafeError() |> ignore; None with ex -> Some ex

                        match thrown with
                        | Some(:? InvalidOperationException as ex) -> Expect.stringContains ex.Message "Fiber was interrupted" "The message should say why"
                        | other -> failtest $"Expected InvalidOperationException, got {other}")
                ]

            testList
                "Fiber - UnsafePrintResult"
                [
                    testAllRuntimes "UnsafePrintResult - does not throw on success" (fun runtime ->
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
                "Fiber - ToString"
                [
                    testAllRuntimes "ToString - returns the fiber ID as a string" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        let str = fiber.ToString()

                        Expect.equal str (fiber.Id.ToString()) "ToString should return the fiber's ID as string")
                ]

            testList
                "Fiber - IDisposable"
                [
                    testAllRuntimes "Dispose - succeeds after completion" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        (fiber :> IDisposable).Dispose()

                        Expect.isTrue true "Dispose should not throw on completed fiber")

                    testAllRuntimes "Dispose - a second call does not throw" (fun runtime ->
                        let fiber =
                            runtime.Run(FIO.succeed 42)
                        let _result = fiber.UnsafeSuccess()

                        (fiber :> IDisposable).Dispose()
                        (fiber :> IDisposable).Dispose()

                        Expect.isTrue true "Double dispose should not throw")

                    testAllRuntimes "Dispose - disposing a running fiber's handle ends it as a defect and leaves the runtime usable" (fun runtime ->
                        let fiber = runtime.Run((FIO.sleep (TimeSpan.FromMilliseconds 200.0)).FlatMap(fun () -> FIO.succeed 1))
                        (fiber :> IDisposable).Dispose()
                        let result = fiber.Task()

                        Expect.isTrue (result.Wait(TimeSpan.FromSeconds 10.0)) "A disposed fiber must still end"
                        match result.Result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect _ -> ()
                            | other -> failtest $"Expected a Defect, got {other}"
                        | other -> failtest $"Expected the disposed fiber to end as a defect, got {other}"

                        let rerun = runtime.Run(FIO.succeed 7).UnsafeSuccess()

                        Expect.equal rerun 7 "The runtime must keep running effects")

                    testList
                        "Dispose - disposing a fiber parked on a task or a full channel does not crash the process"
                        [
                            for name, scenario in [ "parked on a task", "dispose-parked-on-task"; "parked on a full channel", "dispose-parked-writer" ] ->
                                testCase name (fun () ->
                                    use child = new FIO.Tests.ChildProcess.ChildProcess(scenario)

                                    let exitCode = child.WaitForExit(TimeSpan.FromSeconds 60.0)

                                    Expect.equal exitCode (Some 0) $"The process must survive; output: {child.Output}")
                        ]
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
                            runtime.Run(FIO.interruptNow ())
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
                        let runtime = new DirectRuntime()
                        let firstCount = ref 0
                        let secondCount = ref 0

                        let fiber =
                            runtime.Run(FIO.succeed 1)
                        fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
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
                        let runtime = new DirectRuntime()
                        let firstCount = ref 0
                        let secondCount = ref 0

                        let fiber =
                            runtime.Run(FIO.sleep (TimeSpan.FromMilliseconds 20.0))
                        fiber.Context.SetOnTerminal(fun () -> firstCount.Value <- firstCount.Value + 1)
                        fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
                        let deadline = DateTime.UtcNow.AddSeconds 5.0
                        while firstCount.Value = 0 && DateTime.UtcNow < deadline do
                            Threading.Thread.Sleep 1

                        Expect.equal
                            firstCount.Value
                            1
                            "Pre-terminal SetOnTerminal callback should fire exactly once on completion"

                        fiber.Context.SetOnTerminal(fun () -> secondCount.Value <- secondCount.Value + 1)

                        Expect.equal
                            secondCount.Value
                            0
                            "Post-terminal SetOnTerminal after pre-terminal install must not refire (gate consumed)"
                ]
        ]
