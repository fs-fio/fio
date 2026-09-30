module FIO.Tests.Factories.TraversalTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.Threading

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Sequential traversals"
                [
                    testPropertyWithConfig fsCheckConfig "forEach - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.forEach [] FIO.succeed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "forEach over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "forEach - round-trip identity over list of ints"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.forEach xs FIO.succeed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "forEach with FIO.succeed should preserve input"

                    testPropertyWithConfig fsCheckConfig "forEach - applies f to each input in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.forEach xs (fun i -> FIO.succeed (i + 1))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.map (fun i -> i + 1) xs) "forEach should apply f to each input"

                    testPropertyWithConfig fsCheckConfig "forEach - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0

                        let f i =
                            (FIO.attempt
                                (fun () -> Interlocked.Increment(&callCount) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () ->
                                if i = 2 then FIO.fail "boom"
                                else FIO.succeed i)

                        let effect = FIO.forEach [ 1; 2; 3; 4 ] f

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result "boom" "forEach should fail with the first error"
                        Expect.equal callCount 2 "forEach should not invoke f after failure"

                    testCase "forEach - stack-safe over 10000 items"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let xs = [ 1 .. 10000 ]
                            let effect = FIO.forEach xs FIO.succeed

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length result) 10000 $"forEach on {runtime.GetType().Name} should handle 10000 items"

                    testPropertyWithConfig fsCheckConfig "forEachDiscard - empty input completes with unit"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.forEachDiscard [] FIO.succeed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "forEachDiscard over an empty seq should yield ()"

                    testPropertyWithConfig fsCheckConfig "forEachDiscard - applies f to each input without collecting"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let mutable sum = 0

                        let f i =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Add(&sum, i) |> ignore)
                                id

                        let effect = FIO.forEachDiscard xs f

                        let _ =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal sum (List.sum xs) "forEachDiscard should invoke f for every input"

                    testPropertyWithConfig fsCheckConfig "forEachDiscard - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0

                        let f i =
                            (FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () ->
                                if i = 2 then FIO.fail "boom"
                                else FIO.succeed ())

                        let effect = FIO.forEachDiscard [ 1; 2; 3; 4 ] f

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result "boom" "forEachDiscard should fail with the first error"
                        Expect.equal callCount 2 "forEachDiscard should not invoke f after failure"

                    testCase "forEachDiscard - stack-safe over 10000 items"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let xs = [ 1 .. 10000 ]
                            let effect = FIO.forEachDiscard xs (fun _ -> FIO.unit ())

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result () $"forEachDiscard on {runtime.GetType().Name} should handle 10000 items"
                ]

            testList
                "Parallel traversals"
                [
                    testPropertyWithConfig fsCheckConfig "forEachPar - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.forEachPar [] FIO.succeed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "forEachPar over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "forEachPar - preserves input order despite parallel execution"
                    <| fun (runtime: FIORuntime) ->
                        let rnd = Random()
                        let xs = [ 1 .. 50 ]

                        let f i =
                            FIO.attempt
                                (fun () ->
                                    Thread.Sleep(rnd.Next(0, 3))
                                    i)
                                id

                        let effect = FIO.forEachPar xs f

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "forEachPar should preserve input order"

                    testCase "forEachPar - fails with one of the errors and interrupts peers"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let mutable peerCompleted = 0
                            let started = new ManualResetEventSlim(false)
                            let failureItems = 5

                            let f i =
                                if i = 0 then
                                    (FIO.attempt
                                        (fun () -> started.Wait(TimeSpan.FromSeconds 1.0) |> ignore)
                                        (fun ex -> ex.Message)
                                    ).FlatMap(fun () -> FIO.fail "boom")
                                else
                                    (FIO.attempt
                                        (fun () -> started.Set())
                                        (fun ex -> ex.Message))
                                        .FlatMap(fun () ->
                                            FIO.sleep (TimeSpan.FromMilliseconds 200.0))
                                        .FlatMap(fun () ->
                                            FIO.attempt
                                                (fun () -> Interlocked.Increment(&peerCompleted) |> ignore)
                                                (fun ex -> ex.Message))

                            let effect = FIO.forEachPar [ 0 .. failureItems ] f

                            let error =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal error "boom" $"forEachPar on {runtime.GetType().Name} should propagate the failure"
                            Expect.isLessThan peerCompleted failureItems $"forEachPar on {runtime.GetType().Name} should interrupt at least one peer"

                    testCase "forEachPar - fails fast even when an earlier peer never terminates"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let sentinel = -1

                            let effect =
                                (FIO.forEachPar [ 0; 1 ] (fun i ->
                                    if i = 0 then FIO.never<int, int>() else FIO.fail 99))
                                    .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                            let error =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal error 99 $"forEachPar on {runtime.GetType().Name} should observe the late failure without hanging on the never-terminating earlier peer"

                    testPropertyWithConfig fsCheckConfig "forEachParDiscard - applies f to each input without collecting"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let mutable sum = 0

                        let f i =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Add(&sum, i) |> ignore)
                                id

                        let effect = FIO.forEachParDiscard xs f

                        let _ =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal sum (List.sum xs) "forEachParDiscard should invoke f for every input"
                ]

            testList
                "Collect aliases (over seq<FIO>)"
                [
                    testPropertyWithConfig fsCheckConfig "collectAll - mirrors forEach with id"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed

                        let effect = FIO.collectAll effects

                        let result
                            = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "collectAll should collect successes in order"

                    testPropertyWithConfig fsCheckConfig "collectAllDiscard - completes with unit"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed

                        let effect = FIO.collectAllDiscard effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "collectAllDiscard should complete with unit"

                    testPropertyWithConfig fsCheckConfig "collectAllPar - mirrors forEachPar with id"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed

                        let effect = FIO.collectAllPar effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "collectAllPar should collect successes in source order"

                    testPropertyWithConfig fsCheckConfig "collectAllParDiscard - completes with unit"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed

                        let effect = FIO.collectAllParDiscard effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "collectAllParDiscard should complete with unit"
                ]
        ]
