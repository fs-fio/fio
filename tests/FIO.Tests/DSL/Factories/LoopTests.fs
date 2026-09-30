module FIO.Tests.Factories.LoopTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System.Threading

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Repetition"
                [
                    testPropertyWithConfig fsCheckConfig "replicateFIO - zero iterations yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.replicateFIO 0 (FIO.succeed 42)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "replicateFIO 0 should yield []"

                    testPropertyWithConfig fsCheckConfig "replicateFIO - negative iterations clamp to empty list"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                id

                        let result =
                            runtime.Run(FIO.replicateFIO -100 effect).UnsafeSuccess()

                        Expect.equal result [] "replicateFIO with negative n should yield []"
                        Expect.equal callCount 0 "replicateFIO with negative n should not evaluate the effect"

                    testPropertyWithConfig fsCheckConfig "replicateFIO - yields List.replicate of a constant success"
                    <| fun (runtime: FIORuntime) ->
                        let n = 5
                        let effect = FIO.replicateFIO n (FIO.succeed 7)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.replicate n 7) "replicateFIO should yield n copies of the result"

                    testPropertyWithConfig fsCheckConfig "replicateFIO - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let effect =
                            (FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount))
                                (fun ex -> ex.Message)
                            ).FlatMap(fun count ->
                                if count = 3 then FIO.fail "boom"
                                else FIO.succeed count)

                        let error =
                            runtime.Run(FIO.replicateFIO 10 effect).UnsafeError()

                        Expect.equal error "boom" "replicateFIO should fail with the first error"
                        Expect.equal callCount 3 "replicateFIO should not evaluate further iterations after failure"

                    testCase "replicateFIO - stack-safe over 10000 iterations"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effect = FIO.replicateFIO 10000 (FIO.unit ())

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length result) 10000 $"replicateFIO on {runtime.GetType().Name} should handle 10000 iterations"

                    testPropertyWithConfig fsCheckConfig "replicateFIODiscard - zero iterations yields unit"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.replicateFIODiscard 0 (FIO.succeed 42)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "replicateFIODiscard 0 should yield ()"

                    testPropertyWithConfig fsCheckConfig "replicateFIODiscard - negative iterations clamp to unit"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let effect =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                id

                        let result =
                            runtime.Run(FIO.replicateFIODiscard -1 effect).UnsafeSuccess()

                        Expect.equal result () "replicateFIODiscard with negative n should yield ()"
                        Expect.equal callCount 0 "replicateFIODiscard with negative n should not evaluate the effect"

                    testPropertyWithConfig fsCheckConfig "replicateFIODiscard - invokes the effect exactly n times"
                    <| fun (runtime: FIORuntime) ->
                        let n = 25
                        let mutable callCount = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                id

                        let result = runtime.Run(FIO.replicateFIODiscard n effect).UnsafeSuccess()

                        Expect.equal result () "replicateFIODiscard should complete with unit"
                        Expect.equal callCount n "replicateFIODiscard should invoke the effect exactly n times"

                    testPropertyWithConfig fsCheckConfig "replicateFIODiscard - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let effect =
                            (FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount))
                                (fun ex -> ex.Message)
                            ).FlatMap(fun count ->
                                if count = 2 then FIO.fail "boom"
                                else FIO.succeed ())

                        let error =
                            runtime.Run(FIO.replicateFIODiscard 10 effect).UnsafeError()

                        Expect.equal error "boom" "replicateFIODiscard should fail with the first error"
                        Expect.equal callCount 2 "replicateFIODiscard should not evaluate further iterations after failure"

                    testCase "replicateFIODiscard - stack-safe over 10000 iterations"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effect = FIO.replicateFIODiscard 10000 (FIO.unit ())

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result () $"replicateFIODiscard on {runtime.GetType().Name} should handle 10000 iterations"
                ]

            testList
                "Stateful loops"
                [
                    testPropertyWithConfig fsCheckConfig "loop - cont false on initial yields empty list and never invokes body"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body s =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore
                                    s)
                                id

                        let result =
                            runtime.Run(FIO.loop 0 (fun _ -> false) ((+) 1) body).UnsafeSuccess()

                        Expect.equal result [] "loop with false-cont should yield []"
                        Expect.equal callCount 0 "loop with false-cont should not invoke body"

                    testPropertyWithConfig fsCheckConfig "loop - standard counted iteration collects results in order"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            FIO.loop 0 (fun s -> s < 5) ((+) 1) (fun s -> FIO.succeed (s * 2))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [ 0; 2; 4; 6; 8 ] "loop should collect body results in iteration order"

                    testPropertyWithConfig fsCheckConfig "loop - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body s =
                            (FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () ->
                                if s = 3 then FIO.fail "boom"
                                else FIO.succeed s)

                        let error =
                            runtime.Run(FIO.loop 0 (fun s -> s < 10) ((+) 1) body).UnsafeError()

                        Expect.equal error "boom" "loop should fail with the first error"
                        Expect.equal callCount 4 "loop should not invoke body after failure"

                    testCase "loop - stack-safe over 10000 iterations"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effect =
                                FIO.loop 0 (fun s -> s < 10000) ((+) 1) (fun _ -> FIO.unit ())

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length result) 10000 $"loop on {runtime.GetType().Name} should handle 10000 iterations"

                    testPropertyWithConfig fsCheckConfig "loopDiscard - cont false on initial yields unit and never invokes body"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body _ =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                id

                        let result =
                            runtime.Run(FIO.loopDiscard 0 (fun _ -> false) ((+) 1) body).UnsafeSuccess()

                        Expect.equal result () "loopDiscard with false-cont should yield ()"
                        Expect.equal callCount 0 "loopDiscard with false-cont should not invoke body"

                    testPropertyWithConfig fsCheckConfig "loopDiscard - invokes body exactly n times"
                    <| fun (runtime: FIORuntime) ->
                        let n = 25
                        let mutable callCount = 0
                        let body _ =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&callCount) |> ignore)
                                id

                        let result =
                            runtime.Run(FIO.loopDiscard 0 (fun s -> s < n) ((+) 1) body).UnsafeSuccess()

                        Expect.equal result () "loopDiscard should complete with unit"
                        Expect.equal callCount n "loopDiscard should invoke body exactly n times"

                    testPropertyWithConfig fsCheckConfig "loopDiscard - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body s =
                            (FIO.attempt
                                (fun () -> Interlocked.Increment(&callCount) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () ->
                                if s = 2 then FIO.fail "boom"
                                else FIO.succeed ())

                        let error =
                            runtime.Run(FIO.loopDiscard 0 (fun s -> s < 10) ((+) 1) body).UnsafeError()

                        Expect.equal error "boom" "loopDiscard should fail with the first error"
                        Expect.equal callCount 3 "loopDiscard should not invoke body after failure"

                    testCase "loopDiscard - stack-safe over 10000 iterations"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effect =
                                FIO.loopDiscard 0 (fun s -> s < 10000) ((+) 1) (fun _ -> FIO.unit ())

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result () $"loopDiscard on {runtime.GetType().Name} should handle 10000 iterations"

                    testPropertyWithConfig fsCheckConfig "iterate - cont false on initial returns initial without invoking body"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body s =
                            FIO.attempt(
                                fun () ->
                                    Interlocked.Increment(&callCount) |> ignore
                                    s + 1)
                                id

                        let result =
                            runtime.Run(FIO.iterate 42 (fun _ -> false) body).UnsafeSuccess()

                        Expect.equal result 42 "iterate with false-cont should yield initial state"
                        Expect.equal callCount 0 "iterate with false-cont should not invoke body"

                    testPropertyWithConfig fsCheckConfig "iterate - standard counted iteration drives state through body"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            FIO.iterate 0 (fun s -> s < 10) (fun s -> FIO.succeed (s + 1))

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 10 "iterate should return the final state for which cont is false"

                    testPropertyWithConfig fsCheckConfig "iterate - short-circuits on body failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let body s =
                            (FIO.attempt
                                (fun () -> Interlocked.Increment(&callCount))
                                (fun ex -> ex.Message)
                            ).FlatMap(fun count ->
                                if count = 4 then FIO.fail "boom"
                                else FIO.succeed (s + 1))

                        let error =
                            runtime.Run(FIO.iterate 0 (fun s -> s < 100) body).UnsafeError()

                        Expect.equal error "boom" "iterate should fail with the first error"
                        Expect.equal callCount 4 "iterate should not invoke body after failure"

                    testCase "iterate - stack-safe over 10000 iterations"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effect =
                                FIO.iterate 0 (fun s -> s < 10000) (fun s -> FIO.succeed (s + 1))

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result 10000 $"iterate on {runtime.GetType().Name} should handle 10000 iterations"
                ]
        ]
