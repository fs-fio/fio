module FIO.Tests.FiberModuleTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Lib
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let fiberModuleTests =
    testList
        "Fiber Module"
        [

            testList
                "Fiber.succeed"
                [

                    testPropertyWithConfig fsCheckConfig "Creates a completed fiber with success value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let fiber = Fiber.succeed<int, string> value
                        let eff = fiber.Join()

                        let result = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result value "Fiber.succeed should create a fiber with the given value"

                    testAllRuntimes "Fiber is immediately terminal" (fun _runtime ->
                        let fiber = Fiber.succeed<int, string> 42

                        Expect.isTrue (fiber.IsCompleted()) "Fiber.succeed should be immediately completed"
                        Expect.isTrue (fiber.IsTerminal()) "Fiber.succeed should be immediately terminal")
                ]

            testList
                "Fiber.fail"
                [

                    testPropertyWithConfig fsCheckConfig "Creates a completed fiber with error value"
                    <| fun (runtime: FIORuntime, err: string) ->
                        let fiber = Fiber.fail<int, string> err
                        let eff = fiber.Await()

                        let result = runtime.Run(eff).UnsafeSuccess()

                        match result with
                        | Failed e -> Expect.equal e err "Fiber.fail should create a fiber with the given error"
                        | other -> failtest $"Expected Failed, got {other}"

                    testAllRuntimes "Fiber is immediately terminal" (fun _runtime ->
                        let fiber = Fiber.fail<int, string> "boom"

                        Expect.isTrue (fiber.IsCompleted()) "Fiber.fail should be immediately completed"
                        Expect.isTrue (fiber.IsTerminal()) "Fiber.fail should be immediately terminal")
                ]

            testList
                "Fiber.joinAll"
                [

                    testPropertyWithConfig fsCheckConfig "Joins all fibers and collects results"
                    <| fun (runtime: FIORuntime, values: int list) ->
                        let eff =
                            fio {
                                let! fibers = values |> List.map (fun v -> FIO.succeed(v).Fork()) |> FIO.collectAll
                                return! Fiber.joinAll fibers
                            }

                        let result = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal result values "Fiber.joinAll should collect all results"

                    testAllRuntimes "Fails on first error" (fun runtime ->
                        let eff =
                            fio {
                                let! f1 = FIO.succeed(1).Fork()
                                let! f2 = FIO.fail("boom").Fork()
                                let! f3 = FIO.succeed(3).Fork()
                                return! Fiber.joinAll [ f1; f2; f3 ]
                            }

                        let result = runtime.Run(eff).UnsafeError()

                        Expect.equal result "boom" "Fiber.joinAll should fail on first error")
                ]

            testList
                "Fiber.awaitAll"
                [

                    testPropertyWithConfig fsCheckConfig "Awaits all fibers without re-raising"
                    <| fun (runtime: FIORuntime, values: int list) ->
                        let eff =
                            fio {
                                let! fibers = values |> List.map (fun v -> FIO.succeed(v).Fork()) |> FIO.collectAll
                                return! Fiber.awaitAll fibers
                            }

                        let results = runtime.Run(eff).UnsafeSuccess()

                        let extracted =
                            results
                            |> List.map (fun r ->
                                match r with
                                | Succeeded v -> v
                                | _ -> failtest "Expected all Succeeded")

                        Expect.equal extracted values "Fiber.awaitAll should collect all FiberResults"

                    testAllRuntimes "Collects mixed results without failing" (fun runtime ->
                        let eff =
                            fio {
                                let! f1 = FIO.succeed(1).Fork()
                                let! f2 = FIO.fail("boom").Fork()
                                let! _ = f1.Join()
                                let! _ = f2.Join().CatchAll(fun (_: string) -> FIO.succeed 0)
                                return! Fiber.awaitAll [ f1; f2 ]
                            }

                        let results = runtime.Run(eff).UnsafeSuccess()

                        match results with
                        | [ Succeeded 1; Failed "boom" ] -> ()
                        | other -> failtest $"Expected [Succeeded 1; Failed boom], got {other}")
                ]

            testList
                "Fiber.interruptAll"
                [

                    testAllRuntimes "Interrupts all fibers" (fun runtime ->
                        let eff =
                            fio {
                                let! f1 = FIO.never<int, string>().Fork()
                                let! f2 = FIO.never<int, string>().Fork()
                                let! f3 = FIO.never<int, string>().Fork()
                                do! Fiber.interruptAll [ f1; f2; f3 ]
                                do! FIO.sleep(TimeSpan.FromMilliseconds 50.0, id).MapError(fun _ -> "err")
                                return f1.IsInterrupted(), f2.IsInterrupted(), f3.IsInterrupted()
                            }

                        let i1, i2, i3 = runtime.Run(eff).UnsafeSuccess()

                        Expect.isTrue i1 "First fiber should be interrupted"
                        Expect.isTrue i2 "Second fiber should be interrupted"
                        Expect.isTrue i3 "Third fiber should be interrupted")
                ]

            testList
                "Fiber.interruptAwaitAll"
                [

                    testAllRuntimes "Interrupts all fibers and returns their results" (fun runtime ->
                        let eff =
                            fio {
                                let! f1 = FIO.never<int, string>().Fork()
                                let! f2 = FIO.never<int, string>().Fork()
                                return! Fiber.interruptAwaitAll [ f1; f2 ]
                            }

                        let results = runtime.Run(eff).UnsafeSuccess()

                        Expect.equal (List.length results) 2 "Should have results for both fibers"

                        for result in results do
                            match result with
                            | Interrupted _ -> ()
                            | other -> failtest $"Expected Interrupted, got {other}")
                ]

            testList
                "Fiber.pollAll"
                [

                    testPropertyWithConfig fsCheckConfig "Returns Some for all completed fibers"
                    <| fun (runtime: FIORuntime, values: int list) ->
                        let eff =
                            fio {
                                let! fibers = values |> List.map (fun v -> FIO.succeed(v).Fork()) |> FIO.collectAll
                                let! _ = Fiber.joinAll fibers
                                return! Fiber.pollAll fibers
                            }

                        let results = runtime.Run(eff).UnsafeSuccess()

                        let extracted =
                            results
                            |> List.map (fun r ->
                                match r with
                                | Some(Succeeded v) -> v
                                | other -> failtest $"Expected Some Succeeded, got {other}")

                        Expect.equal extracted values "Fiber.pollAll should return all results"

                    testAllRuntimes "Returns None for running fibers" (fun runtime ->
                        let eff =
                            fio {
                                let! f1 = FIO.never<int, string>().Fork()
                                let! f2 = FIO.never<int, string>().Fork()
                                let! polls = Fiber.pollAll [ f1; f2 ]
                                do! Fiber.interruptAll [ f1; f2 ]
                                return polls
                            }

                        let results = runtime.Run(eff).UnsafeSuccess()

                        for result in results do
                            Expect.isNone result "Poll should return None for running fibers")
                ]
        ]
