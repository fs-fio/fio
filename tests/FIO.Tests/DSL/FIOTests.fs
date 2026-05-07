/// <summary>Provides property-based tests for core FIO effects including succeed, fail, action, and stack safety.</summary>
module FIO.Tests.FIOTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Ref
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Threading

open Expecto

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let fioTests =
    testList
        "FIO"
        [

            testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with success value"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff =
                    fio {
                        let! fiber = FIO.succeed(value).Fork()
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Forked success effect should complete with the value"

            testPropertyWithConfig fsCheckConfig "Fork - returns a fiber that completes with error"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff =
                    fio {
                        let! fiber = FIO.fail(err).Fork()
                        return fiber
                    }

                let fiber = runtime.Run(eff).UnsafeSuccess()

                match fiber.UnsafeResult() with
                | Failed e -> Expect.equal e err "Forked error effect should fail with the error"
                | other -> failtest $"Expected Failed but got: {other}"

            testAllRuntimes "Fork - forked effect executes concurrently" (fun runtime ->
                let eff =
                    fio {
                        let chan = Channel<int>()
                        let! fiber = (chan.Receive()).Fork()
                        do! chan.Send(42).Unit()
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 42 "Forked receiver should get message sent after fork")

            testAllRuntimes "Fork - multiple forks execute independently" (fun runtime ->
                let eff =
                    fio {
                        let! f1 = FIO.succeed(1).Fork()
                        let! f2 = FIO.succeed(2).Fork()
                        let! f3 = FIO.succeed(3).Fork()
                        let! r1 = f1.Join()
                        let! r2 = f2.Join()
                        let! r3 = f3.Join()
                        return [ r1; r2; r3 ]
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "Each forked fiber should complete independently")

            testPropertyWithConfig fsCheckConfig "FlatMap - chains success continuation"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).FlatMap(fun x -> FIO.succeed (x + 1))

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (value + 1) "FlatMap should chain the continuation on success"

            testPropertyWithConfig fsCheckConfig "FlatMap - short-circuits on error"
            <| fun (runtime: FIORuntime, err: string) ->
                let mutable contRan = false

                let eff =
                    FIO
                        .fail(err)
                        .FlatMap(fun (_: int) ->
                            contRan <- true
                            FIO.succeed 0)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "FlatMap should propagate the error"
                Expect.isFalse contRan "FlatMap continuation should not run on error"

            testPropertyWithConfig fsCheckConfig "FlatMap - can change result type"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).FlatMap(fun x -> FIO.succeed (x.ToString()))

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (value.ToString()) "FlatMap should allow changing the result type"

            testAllRuntimes "FlatMap - chains multiple operations" (fun runtime ->
                let eff =
                    FIO
                        .succeed(1)
                        .FlatMap(fun x -> FIO.succeed (x + 10))
                        .FlatMap(fun x -> FIO.succeed (x * 2))
                        .FlatMap(fun x -> FIO.succeed (x.ToString()))

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

                let eff =
                    FIO
                        .succeed(value)
                        .CatchAll(fun (_: string) ->
                            handlerRan <- true
                            FIO.succeed 0)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "CatchAll should not affect success"
                Expect.isFalse handlerRan "CatchAll handler should not run on success"

            testPropertyWithConfig fsCheckConfig "CatchAll - can change error type"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).CatchAll(fun e -> FIO.fail (e.ToString()))

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result (err.ToString()) "CatchAll should allow changing the error type"

            testPropertyWithConfig fsCheckConfig "CatchAll - recovery can itself fail"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).CatchAll(fun _ -> FIO.fail "recovery failed")

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "recovery failed" "CatchAll recovery failure should propagate"

            testAllRuntimes "Ensuring - runs finalizer on success" (fun runtime ->
                let mutable finalizerRan = false
                let finalizer = FIO.attempt ((fun () -> finalizerRan <- true), fun _ -> "")
                let eff = FIO.succeed(42).Ensuring finalizer

                runtime.Run(eff).UnsafeSuccess() |> ignore

                Expect.isTrue finalizerRan "Finalizer should run on success")

            testAllRuntimes "Ensuring - runs finalizer on error" (fun runtime ->
                let mutable finalizerRan = false
                let finalizer = FIO.attempt ((fun () -> finalizerRan <- true), fun _ -> "")
                let eff = FIO.fail("boom").Ensuring finalizer

                runtime.Run(eff).UnsafeError() |> ignore

                Expect.isTrue finalizerRan "Finalizer should run on error")

            testPropertyWithConfig fsCheckConfig "Ensuring - preserves success result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).Ensuring(FIO.unit ())

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Ensuring should preserve the success result"

            testPropertyWithConfig fsCheckConfig "Ensuring - preserves error when finalizer succeeds"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail(err).Ensuring(FIO.unit ())

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Ensuring should preserve the error"

            testPropertyWithConfig
                fsCheckConfig
                "Ensuring - returns finalizer error when main succeeds but finalizer fails"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).Ensuring(FIO.fail "finalizer failed")

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "finalizer failed" "Finalizer error should be returned when main succeeds"

            testAllRuntimes "Ensuring - finalizer runs on self-interruption"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue false

                        let! fiber =
                            FIO.interrupt(ExplicitInterrupt, "self-interrupt").Ensuring(ref.Set(true, id)).Fork()

                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        return! ref.Get()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "Finalizer should run on self-interruption"

            testAllRuntimes "Ensuring - finalizer runs on external interruption"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue false

                        let! fiber =
                            FIO.sleep(TimeSpan.FromSeconds 60.0, id).Ensuring(ref.Set(true, id)).Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        do! fiber.Interrupt()
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        return! ref.Get()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "Finalizer should run on external interruption"

            testAllRuntimes "Ensuring - nested finalizers both run on interrupt"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! ref1 = Ref.makeValue false
                        let! ref2 = Ref.makeValue false

                        let! fiber =
                            FIO
                                .sleep(TimeSpan.FromSeconds 60.0, id)
                                .Ensuring(ref1.Set(true, id))
                                .Ensuring(ref2.Set(true, id))
                                .Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        do! fiber.Interrupt()
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        let! v1 = ref1.Get()
                        let! v2 = ref2.Get()
                        return v1, v2
                    }

                let v1, v2 = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue v1 "Inner finalizer should run on interrupt"
                Expect.isTrue v2 "Outer finalizer should run on interrupt"

            testAllRuntimes "Ensuring - result is still interrupted when finalizer fails"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.sleep(TimeSpan.FromSeconds 60.0, id).Ensuring(FIO.fail (exn "finalizer error")).Fork()

                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        do! fiber.Interrupt()
                        return! fiber.Join()
                    }

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Interrupted _ -> ()
                | other -> failtest $"Expected Interrupted but got {other}"

            testAllRuntimes "Stack safety - deep left-chained FlatMap via Fork and CatchAll"
            <| fun (runtime: FIORuntime) ->
                let depth = 10000
                let mutable eff = FIO.succeed 0

                for _ in 1..depth do
                    eff <- eff.FlatMap(fun n -> FIO.succeed (n + 1))

                let forked = eff.Fork()
                let caught = eff.CatchAll(fun _ -> FIO.succeed -1)

                let joined =
                    fio {
                        let! fiber = forked
                        let! fromFork = fiber.Join()
                        let! fromCatch = caught
                        return fromFork, fromCatch
                    }

                let fromFork, fromCatch = runtime.Run(joined).UnsafeSuccess()
                Expect.equal fromFork depth $"Forked deep FlatMap chain should yield {depth}"
                Expect.equal fromCatch depth $"Caught deep FlatMap chain should yield {depth}"

            testAllRuntimes "Stack safety - deep left-chained CatchAll via Fork and CatchAll"
            <| fun (runtime: FIORuntime) ->
                let depth = 10000
                let mutable eff = FIO.fail (exn "seed")

                for _ in 1..depth do
                    eff <- eff.CatchAll(fun _ -> FIO.fail (exn "next"))

                let recovered = eff.CatchAll(fun _ -> FIO.succeed 42)

                let joined =
                    fio {
                        let! fiber = recovered.Fork()
                        return! fiber.Join()
                    }

                let result = runtime.Run(joined).UnsafeSuccess()
                Expect.equal result 42 "Deep CatchAll chain should recover to 42"

            testAllRuntimes "Stack safety - deep left-chained Ensuring via Fork"
            <| fun (runtime: FIORuntime) ->
                let depth = 10000
                let mutable eff = FIO.succeed 0

                for _ in 1..depth do
                    eff <- eff.Ensuring(FIO.unit ())

                let joined =
                    fio {
                        let! fiber = eff.Fork()
                        return! fiber.Join()
                    }

                let result = runtime.Run(joined).UnsafeSuccess()
                Expect.equal result 0 "Deep Ensuring chain should yield the seed value"

            testAllRuntimes "AddRegistration - fast-completing forked effects do not wedge under load"
            <| fun (runtime: FIORuntime) ->
                let counter = ref 0

                let eff =
                    fio {
                        for _ in 1..200 do
                            let! fiber = FIO.succeed(1).Fork()
                            let! _ = fiber.Join()
                            counter.Value <- counter.Value + 1

                        return counter.Value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.equal result 200 "All fast-completing forks should be observed"

            testAllRuntimes "Action - throwing onError falls back to original exception (E-1)" (fun runtime ->
                let originalExn = InvalidOperationException("original")

                let eff: FIO<int, exn> =
                    FIO.attempt ((fun () -> raise originalExn), (fun _ -> failwith "onError also throws"))

                let result = runtime.Run(eff).UnsafeError()

                Expect.isTrue
                    (obj.ReferenceEquals(result, originalExn))
                    "When onError throws, the original exception should be used as the error")

            testAllRuntimes "FlatMap - throwing continuation produces error (E-2)" (fun runtime ->
                let eff: FIO<int, exn> =
                    FIO.succeed(42).FlatMap(fun (_: int) -> failwith "continuation throws": FIO<int, exn>)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result.Message "continuation throws" "Thrown exception should become the error")

            testAllRuntimes "CatchAll - throwing error handler produces error (E-2)" (fun runtime ->
                let eff: FIO<int, exn> =
                    FIO
                        .fail(InvalidOperationException "typed error" :> exn)
                        .CatchAll(fun _ -> failwith "handler throws": FIO<int, exn>)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal
                    result.Message
                    "handler throws"
                    "Thrown exception in CatchAll handler should become the error")

            testAllRuntimes "Run - orphaned child fibers are interrupted on re-Run (R-8)" (fun runtime ->
                let childStarted = new ManualResetEventSlim(false)

                let childEffect: FIO<unit, exn> =
                    fio {
                        do! FIO.attempt ((fun () -> childStarted.Set()), id)
                        return! FIO.never ()
                    }

                let parentEffect: FIO<obj, exn> =
                    fio {
                        let! fiber = childEffect.Fork()
                        do! FIO.attempt ((fun () -> childStarted.Wait(TimeSpan.FromSeconds 5.0) |> ignore), id)
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

            testCase "WorkerHealthMonitor - Healthy by default" (fun () ->
                let monitor = WorkerHealthMonitor(4)
                Expect.equal monitor.State Healthy "Should start Healthy")

            testCase "WorkerHealthMonitor - Degraded on EW permanent fault" (fun () ->
                let monitor = WorkerHealthMonitor(4)
                monitor.ReportPermanentFault(Evaluation, "test fault")
                Expect.equal monitor.State (Degraded 1) "Should be Degraded(1)")

            testCase "WorkerHealthMonitor - Faulted when all EWs fault" (fun () ->
                let monitor = WorkerHealthMonitor(2)
                monitor.ReportPermanentFault(Evaluation, "fault 1")
                monitor.ReportPermanentFault(Evaluation, "fault 2")

                match monitor.State with
                | Faulted _ -> ()
                | other -> failtestf "Expected Faulted, got %A" other)

            testCase "WorkerHealthMonitor - Faulted on BW permanent fault" (fun () ->
                let monitor = WorkerHealthMonitor(4)
                monitor.ReportPermanentFault(Blocking, "bw fault")

                match monitor.State with
                | Faulted _ -> ()
                | other -> failtestf "Expected Faulted, got %A" other)

            testCase "WorkerHealthMonitor - NotifyFault invokes callback" (fun () ->
                let monitor = WorkerHealthMonitor(4)
                let mutable received = false
                monitor.OnFault(Action<WorkerFaultEvent>(fun _ -> received <- true))

                let origErr = Console.Error
                Console.SetError TextWriter.Null

                try
                    monitor.NotifyFault
                        {
                            WorkerName = "test"
                            WorkerKind = Evaluation
                            FaultCount = 1
                            Exception = exn "test"
                            Timestamp = DateTimeOffset.UtcNow
                            WillRestart = true
                        }
                finally
                    Console.SetError origErr

                Expect.isTrue received "Callback should have been invoked")

            testCase "RuntimeFaultedException - Run throws on Faulted runtime (ConcurrentRuntime)" (fun () ->
                let runtime = new ConcurrentRuntime()
                runtime.Monitor.ReportPermanentFault(Blocking, "test fault")

                Expect.throws
                    (fun () -> runtime.Run(FIO.succeed 1) |> ignore)
                    "Run should throw RuntimeFaultedException"

                (runtime :> IDisposable).Dispose())

            testCase "RuntimeFaultedException - Run throws on Faulted runtime (CooperativeRuntime)" (fun () ->
                let runtime = new CooperativeRuntime()
                runtime.Monitor.ReportPermanentFault(Blocking, "test fault")

                Expect.throws
                    (fun () -> runtime.Run(FIO.succeed 1) |> ignore)
                    "Run should throw RuntimeFaultedException"

                (runtime :> IDisposable).Dispose())

            testCase "HealthState - Degraded allows Run (ConcurrentRuntime)" (fun () ->
                let runtime = new ConcurrentRuntime()
                runtime.Monitor.ReportPermanentFault(Evaluation, "one ew down")
                let fiber = runtime.Run(FIO.succeed 42)
                Expect.equal (fiber.UnsafeSuccess()) 42 "Should still work when Degraded"
                (runtime :> IDisposable).Dispose())

            testCase "HealthState - Degraded allows Run (CooperativeRuntime)" (fun () ->
                let runtime = new CooperativeRuntime()
                runtime.Monitor.ReportPermanentFault(Evaluation, "one ew down")
                let fiber = runtime.Run(FIO.succeed 42)
                Expect.equal (fiber.UnsafeSuccess()) 42 "Should still work when Degraded"
                (runtime :> IDisposable).Dispose())

            testCase "OnWorkerFault - callback accessible from FIOWorkerRuntime" (fun () ->
                let runtime = new ConcurrentRuntime()
                let mutable received = false

                runtime.OnWorkerFault(Action<WorkerFaultEvent>(fun _ -> received <- true))

                let origErr = Console.Error
                Console.SetError TextWriter.Null

                try
                    runtime.Monitor.NotifyFault
                        {
                            WorkerName = "test"
                            WorkerKind = Evaluation
                            FaultCount = 1
                            Exception = exn "test"
                            Timestamp = DateTimeOffset.UtcNow
                            WillRestart = true
                        }
                finally
                    Console.SetError origErr

                Expect.isTrue received "OnWorkerFault callback should work"
                (runtime :> IDisposable).Dispose())
        ]
