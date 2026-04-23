module FIO.Tests.AdmissionControlTests

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System
open System.Threading

let private workerRuntimes maxFibers =
    let config = { WorkerConfig.Default with MaxFibers = Some maxFibers }

    [
        new CooperativeRuntime(config) :> FIORuntime
        new ConcurrentRuntime(config) :> FIORuntime
    ]

let private testAllBoundedRuntimes name maxFibers (f: FIORuntime -> unit) =
    testList
        name
        [
            for rt in workerRuntimes maxFibers -> testCase (rt.GetType().Name) (fun () -> f rt)
        ]

[<Tests>]
let admissionControlTests =
    testList
        "Admission Control"
        [

            testList
                "Backward Compatibility"
                [

                    testCase "Default config (MaxFibers=None) behaves as unbounded"
                    <| fun () ->
                        use runtime = new ConcurrentRuntime()
                        let eff = FIO.collectAllPar [ for i in 1..500 -> FIO.succeed i ]
                        let result = runtime.Run(eff).UnsafeSuccess()
                        Expect.hasLength result 500 "Unbounded mode should work as before"

                    testCase "Default CooperativeRuntime (MaxFibers=None) behaves as unbounded"
                    <| fun () ->
                        use runtime = new CooperativeRuntime()
                        let eff = FIO.collectAllPar [ for i in 1..500 -> FIO.succeed i ]
                        let result = runtime.Run(eff).UnsafeSuccess()
                        Expect.hasLength result 500 "Unbounded mode should work as before"

                ]

            testList
                "Normal Workload"
                [

                    testAllBoundedRuntimes "High limit does not regress collectAllPar" 10_000
                    <| fun runtime ->
                        let eff = FIO.collectAllPar [ for i in 1..1000 -> FIO.succeed i ]
                        let result = runtime.Run(eff).UnsafeSuccess()
                        Expect.hasLength result 1000 "All results should be collected"

                    // Fork-join needs 2 permits per effect (outer + inner fork).
                    // With 100 fork-joins, we need MaxFibers >> 200 to avoid permit starvation.
                    testAllBoundedRuntimes "Fork-join works under admission control" 500
                    <| fun runtime ->
                        let forkJoin () =
                            fio {
                                let! fiber = (FIO.succeed 42).Fork()
                                return! fiber.Join()
                            }

                        let eff = FIO.collectAllPar [ for _ in 1..100 -> forkJoin () ]
                        let result = runtime.Run(eff).UnsafeSuccess()
                        Expect.hasLength result 100 "All fork-joins should complete"
                        Expect.allEqual result 42 "All results should be 42"

                ]

            testList
                "Deadlock Prevention"
                [

                    // Non-nested forks: each effect is a simple FIO.succeed, so only
                    // collectAllPar's forks consume permits. No nested fork dependency.
                    testAllBoundedRuntimes
                        "Tight MaxFibers does not deadlock on simple parallel work"
                        (WorkerConfig.Default.EWC + 2)
                    <| fun runtime ->
                        let eff = FIO.collectAllPar [ for i in 1..50 -> FIO.succeed i ]
                        let result = runtime.Run(eff).UnsafeSuccess()
                        Expect.hasLength result 50 "All effects should complete"

                ]

            testList
                "Fork Bomb Protection"
                [

                    testAllBoundedRuntimes "Fork bomb with admission control stays bounded" 100
                    <| fun runtime ->
                        let forkCount = ref 0

                        let rec forkBomb (depth: int) =
                            if depth <= 0 then
                                FIO.succeed ()
                            else
                                fio {
                                    Interlocked.Increment forkCount |> ignore
                                    let! _ = (forkBomb (depth - 1)).Fork()
                                    let! _ = (forkBomb (depth - 1)).Fork()
                                    return ()
                                }

                        // Depth 5 creates up to 2^5 - 1 = 31 forks, within the 100 limit.
                        let fiber = runtime.Run(forkBomb 5)
                        let result = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match result with
                        | Succeeded() -> ()
                        | Failed e -> failtestf "Should not fail: %A" e
                        | Interrupted e -> failtestf "Should not be interrupted: %A" e

                ]

            testList
                "Configuration Validation"
                [

                    testCase "MaxFibers = Some 0 is rejected"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                let config = { WorkerConfig.Default with MaxFibers = Some 0 }
                                use _ = new ConcurrentRuntime(config)
                                ())
                            "MaxFibers = 0 should be rejected"

                    testCase "MaxFibers = Some -1 is rejected"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                let config = { WorkerConfig.Default with MaxFibers = Some -1 }
                                use _ = new ConcurrentRuntime(config)
                                ())
                            "Negative MaxFibers should be rejected"

                ]

            testList
                "Permit Accounting"
                [

                    testAllBoundedRuntimes "Multiple Run cycles work correctly" 500
                    <| fun runtime ->
                        for _ in 1..5 do
                            let eff = FIO.collectAllPar [ for i in 1..20 -> FIO.succeed i ]
                            let result = runtime.Run(eff).UnsafeSuccess()
                            Expect.hasLength result 20 "Each Run cycle should complete"

                ]

        ]
