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
                            use _ = runtime
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
                            use _ = runtime
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
                            use _ = runtime
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
                            use _ = runtime
                            let sentinel = -1
                            let effect =
                                (FIO.forEachPar [ 0; 1 ] (fun i ->
                                    if i = 0 then FIO.never<int, int>() else FIO.fail 99))
                                    .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                            let error =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal error 99 $"forEachPar on {runtime.GetType().Name} should observe the late failure without hanging on the never-terminating earlier peer"

                    testAllRuntimes "forEachPar - an interrupted element interrupts its peers and propagates its cause" (fun runtime ->
                        let peerFinalized = ref false
                        let peerStarted = new ManualResetEventSlim(false)
                        let element (i: int) : FIO<int, string> =
                            if i = 0 then
                                (FIO.attempt (fun () -> peerStarted.Wait(TimeSpan.FromSeconds 5.0) |> ignore) (fun ex -> ex.Message))
                                    .FlatMap(fun () -> FIO.interrupt (ResourceExhaustion "element ran out") "element stopped")
                            else
                                (FIO.succeedWith(fun () -> peerStarted.Set()).FlatMap(fun () -> FIO.never ()))
                                    .Ensuring(FIO.succeedWith (fun () -> peerFinalized.Value <- true))
                        let parent: FIO<FiberResult<int list, string>, string> =
                            fio {
                                let! fiber = (FIO.forEachPar [ 0; 1 ] element).Fork()
                                return! fiber.Await()
                            }

                        let result = runtime.Run(parent).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | ResourceExhaustion "element ran out" -> Expect.equal ex.message "element stopped" "The element's message should propagate"
                            | cause -> failtest $"Expected the element's cause, got %A{cause}"
                        | other -> failtest $"Expected the interruption to propagate, got %A{other}"
                        Expect.isTrue (waitForFlag peerFinalized) "The peer should be interrupted and finalized")

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

                    testPropertyWithConfig fsCheckConfig "forEachParN - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.forEachParN 3 [] FIO.succeed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "forEachParN over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "forEachParN - preserves input order"
                    <| fun (runtime: FIORuntime, xs: int list, seed: int) ->
                        let parallelism = abs (seed % 8) + 1
                        let effect = FIO.forEachParN parallelism xs (fun x -> FIO.succeed (x * 2))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (xs |> List.map (fun x -> x * 2)) "forEachParN should collect the results in input order"

                    testAllRuntimes "forEachParN - never runs more than parallelism effects at once" (fun runtime ->
                        let inFlight = Ref 0
                        let peak = Ref 0
                        let item (_: int) : FIO<unit, string> =
                            fio {
                                let! now = inFlight.UpdateAndGet((+) 1)
                                do! peak.Update(max now)
                                do! FIO.sleep (TimeSpan.FromMilliseconds 5.0)
                                do! inFlight.Update(fun count -> count - 1)
                            }
                        let effect = FIO.forEachParN 3 [ 1..40 ] item

                        runtime.Run(effect).UnsafeSuccess() |> ignore

                        Expect.isLessThanOrEqual (peak.UnsafeGet()) 3 "No more than parallelism effects should run at once"
                        Expect.equal (inFlight.UnsafeGet()) 0 "Every effect should have finished")

                    testAllRuntimes "forEachParN - runs parallelism effects at once when there are enough items" (fun runtime ->
                        let arrived = Ref 0
                        let allArrived = Promise<unit, string>()
                        let item (_: int) : FIO<unit, string> =
                            fio {
                                let! count = arrived.UpdateAndGet((+) 1)

                                if count = 3 then
                                    do! allArrived.Succeed(()).Unit()

                                do! allArrived.Await()
                            }
                        let effect = (FIO.forEachParN 3 [ 1..3 ] item).TimeoutFail "the items never ran at once" (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded _ -> ()
                        | other -> failtest $"Expected all three items to run at once but got %A{other}")

                    testAllRuntimes "forEachParN - a failure interrupts the running elements" (fun runtime ->
                        let finalized = ref false
                        let started = Promise<unit, string>()
                        let item (i: int) : FIO<unit, string> =
                            if i = 0 then
                                started.Await().FlatMap(fun () -> FIO.fail "boom")
                            else
                                started.Succeed(()).Unit().FlatMap(fun () -> FIO.never ())
                                    .Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true))
                        let effect = FIO.forEachParN 2 [ 0; 1 ] item

                        let error = runtime.Run(effect).UnsafeError()

                        Expect.equal error "boom" "The failure should propagate"
                        Expect.isTrue (waitForFlag finalized) "The running peer should be interrupted and finalized")

                    testAllRuntimes "forEachParN - starts no element after a failure" (fun runtime ->
                        let started = Ref<int list> []
                        let item (i: int) : FIO<unit, string> =
                            started.Update(fun seen -> i :: seen).FlatMap(fun () -> if i = 1 then FIO.fail "boom" else FIO.unit ())
                        let effect = FIO.forEachParN 1 [ 0..4 ] item

                        let error = runtime.Run(effect).UnsafeError()

                        Expect.equal error "boom" "The failure should propagate"
                        Expect.equal (List.rev (started.UnsafeGet())) [ 0; 1 ] "No item after the failing one should start")

                    testAllRuntimes "forEachParN - an element that throws ends the effect with a Defect" (fun runtime ->
                        let item (i: int) : FIO<int, string> =
                            if i = 2 then failwith "element threw" else FIO.succeed i
                        let effect = FIO.forEachParN 2 [ 1..4 ] item

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect inner -> Expect.equal inner.Message "element threw" "The thrown exception should be the defect"
                            | cause -> failtest $"Expected a Defect but got %A{cause}"
                        | other -> failtest $"Expected a defect to end the effect but got %A{other}")

                    testAllRuntimes "forEachParN - an interrupted element interrupts its peers and propagates its cause" (fun runtime ->
                        let peerFinalized = ref false
                        let peerStarted = Promise<unit, string>()
                        let element (i: int) : FIO<int, string> =
                            if i = 0 then
                                peerStarted.Await().FlatMap(fun () -> FIO.interrupt (ResourceExhaustion "element ran out") "element stopped")
                            else
                                peerStarted.Succeed(()).Unit().FlatMap(fun () -> FIO.never ())
                                    .Ensuring(FIO.succeedWith (fun () -> peerFinalized.Value <- true))
                        let parent: FIO<FiberResult<int list, string>, string> =
                            fio {
                                let! fiber = (FIO.forEachParN 2 [ 0; 1 ] element).Fork()
                                return! fiber.Await()
                            }

                        let result = runtime.Run(parent).UnsafeSuccess()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | ResourceExhaustion "element ran out" -> Expect.equal ex.message "element stopped" "The element's message should propagate"
                            | cause -> failtest $"Expected the element's cause, got %A{cause}"
                        | other -> failtest $"Expected the interruption to propagate, got %A{other}"
                        Expect.isTrue (waitForFlag peerFinalized) "The peer should be interrupted and finalized")

                    testAllRuntimes "forEachParN - a parallelism below 1 interrupts with InvalidArgument" (fun runtime ->
                        let effect : FIO<int list, string> = FIO.forEachParN 0 [ 1; 2 ] FIO.succeed

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | InvalidArgument("parallelism", _) -> ()
                            | cause -> failtest $"Expected InvalidArgument but got %A{cause}"
                        | other -> failtest $"Expected an interruption but got %A{other}")

                    testAllRuntimes "forEachParN - running the effect again starts afresh" (fun runtime ->
                        let traversal = FIO.forEachParN 2 [ 1..5 ] (fun x -> FIO.succeed (x * x))
                        let effect : FIO<int list * int list, string> = traversal.Zip traversal

                        let first, second = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal first [ 1; 4; 9; 16; 25 ] "The first run should collect every result"
                        Expect.equal second first "The second run should start from the first item again")

                    testAllRuntimes "forEachParN - collects 10000 items in order" (fun runtime ->
                        let effect : FIO<int list, string> = FIO.forEachParN 4 [ 1..10_000 ] FIO.succeed

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [ 1..10_000 ] "Every item should be collected, in order")
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
