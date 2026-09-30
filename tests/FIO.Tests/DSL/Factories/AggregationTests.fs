module FIO.Tests.Factories.AggregationTests

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
                "Aggregation (fold / reduce)"
                [
                    testPropertyWithConfig fsCheckConfig "mergeAll - empty input yields zero"
                    <| fun (runtime: FIORuntime, zero: int) ->
                        let effect = FIO.mergeAll [] zero (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result zero "mergeAll over an empty seq should yield zero"

                    testPropertyWithConfig fsCheckConfig "mergeAll - folds successful results left-to-right"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed
                        let effect = FIO.mergeAll effects 0 (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.sum xs) "mergeAll should equal List.fold over the successes"

                    testPropertyWithConfig fsCheckConfig "mergeAll - short-circuits on first failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable callCount = 0
                        let mk i =
                            (FIO.attempt
                                (fun () -> Interlocked.Increment(&callCount) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () ->
                                if i = 3 then FIO.fail "boom"
                                else FIO.succeed i)
                        let effects = [ 0 .. 9 ] |> List.map mk

                        let error =
                            runtime.Run(FIO.mergeAll effects 0 (+)).UnsafeError()

                        Expect.equal error "boom" "mergeAll should fail with the first error"
                        Expect.equal callCount 4 "mergeAll should not evaluate further effects after failure"

                    testCase "mergeAll - stack-safe over 10000 effects"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effects = [ 1 .. 10000 ] |> List.map FIO.succeed

                            let effect = FIO.mergeAll effects 0 (+)

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result 50005000 $"mergeAll on {runtime.GetType().Name} should fold 10000 effects"

                    testPropertyWithConfig fsCheckConfig "mergeAllPar - empty input yields zero"
                    <| fun (runtime: FIORuntime, zero: int) ->
                        let effect = FIO.mergeAllPar [] zero (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result zero "mergeAllPar over an empty seq should yield zero"

                    testPropertyWithConfig fsCheckConfig "mergeAllPar - folds successful results in source order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed
                        let effect = FIO.mergeAllPar effects 0 (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.sum xs) "mergeAllPar should equal List.fold over the successes"

                    testPropertyWithConfig fsCheckConfig "mergeAllPar - fails with one of the errors"
                    <| fun (runtime: FIORuntime) ->
                        let effects =
                            [ FIO.succeed 1
                              FIO.fail "boom"
                              FIO.succeed 2 ]

                        let error =
                            runtime.Run(FIO.mergeAllPar effects 0 (+)).UnsafeError()

                        Expect.equal error "boom" "mergeAllPar should propagate the failure"

                    testPropertyWithConfig fsCheckConfig "reduceAll - empty tail returns head's result"
                    <| fun (runtime: FIORuntime, x: int) ->
                        let effect = FIO.reduceAll (FIO.succeed x) [] (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result x "reduceAll with empty tail should return head's result"

                    testPropertyWithConfig fsCheckConfig "reduceAll - reduces head + tail left-to-right"
                    <| fun (runtime: FIORuntime, h: int, tail: int list) ->
                        let tailEffects = tail |> List.map FIO.succeed
                        let effect = FIO.reduceAll (FIO.succeed h) tailEffects (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.fold (+) h tail) "reduceAll should equal List.fold over head + tail"

                    testPropertyWithConfig fsCheckConfig "reduceAll - short-circuits on head failure"
                    <| fun (runtime: FIORuntime) ->
                        let mutable tailEvaluated = 0
                        let mkTail i =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&tailEvaluated) |> ignore
                                    i)
                                (fun ex -> ex.Message)
                        let tail = [ 1; 2; 3 ] |> List.map mkTail

                        let error =
                            runtime.Run(FIO.reduceAll (FIO.fail "boom") tail (+)).UnsafeError()

                        Expect.equal error "boom" "reduceAll should fail with head's error"
                        Expect.equal tailEvaluated 0 "reduceAll should not evaluate tail after head failure"

                    testCase "reduceAll - stack-safe over 10000 effects"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let tail = [ 1 .. 9999 ] |> List.map FIO.succeed
                            let effect = FIO.reduceAll (FIO.succeed 0) tail (+)

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal result 49995000 $"reduceAll on {runtime.GetType().Name} should reduce 10000 effects"

                    testPropertyWithConfig fsCheckConfig "reduceAllPar - empty tail returns head's result"
                    <| fun (runtime: FIORuntime, x: int) ->
                        let effect = FIO.reduceAllPar (FIO.succeed x) [] (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result x "reduceAllPar with empty tail should return head's result"

                    testPropertyWithConfig fsCheckConfig "reduceAllPar - reduces head + tail in source order"
                    <| fun (runtime: FIORuntime, h: int, tail: int list) ->
                        let tailEffects = tail |> List.map FIO.succeed
                        let effect = FIO.reduceAllPar (FIO.succeed h) tailEffects (+)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (List.fold (+) h tail) "reduceAllPar should equal List.fold over head + tail"

                    testPropertyWithConfig fsCheckConfig "reduceAllPar - fails when head fails"
                    <| fun (runtime: FIORuntime) ->
                        let tail = [ FIO.succeed 1; FIO.succeed 2 ]

                        let error =
                            runtime.Run(FIO.reduceAllPar (FIO.fail "boom") tail (+)).UnsafeError()

                        Expect.equal error "boom" "reduceAllPar should fail with head's error"
                ]

            testList
                "Error-tolerant traversals"
                [
                    testPropertyWithConfig fsCheckConfig "partition - empty input yields ([], [])"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.partition [] FIO.succeed

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [] "partition over an empty seq should yield no errors"
                        Expect.equal oks [] "partition over an empty seq should yield no successes"

                    testPropertyWithConfig fsCheckConfig "partition - all-success collects every result and no errors"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.partition xs (fun x -> FIO.succeed x)

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [] "partition with all-success should produce no errors"
                        Expect.equal oks xs "partition with all-success should preserve input order"

                    testPropertyWithConfig fsCheckConfig "partition - all-failure collects every error and no successes"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let f x = FIO.fail (string x)
                        let effect = FIO.partition xs f

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs (List.map string xs) "partition with all-failure should preserve input order in errors"
                        Expect.equal oks [] "partition with all-failure should produce no successes"

                    testPropertyWithConfig fsCheckConfig "partition - mixed input splits and preserves order in both lists"
                    <| fun (runtime: FIORuntime) ->
                        let xs = [ 0; 1; 2; 3; 4; 5 ]
                        let f x =
                            if x % 2 = 0 then FIO.succeed x
                            else FIO.fail (string x)
                        let effect = FIO.partition xs f

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [ "1"; "3"; "5" ] "partition should preserve input order in errors"
                        Expect.equal oks [ 0; 2; 4 ] "partition should preserve input order in successes"

                    testCase "partition - stack-safe over 10000 items"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let xs = [ 1 .. 10000 ]
                            let f x =
                                if x % 2 = 0 then FIO.succeed x
                                else FIO.fail x
                            let effect = FIO.partition xs f

                            let errs, oks =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length errs) 5000 $"partition on {runtime.GetType().Name} should collect 5000 errors"
                            Expect.equal (List.length oks) 5000 $"partition on {runtime.GetType().Name} should collect 5000 successes"

                    testPropertyWithConfig fsCheckConfig "partitionPar - empty input yields ([], [])"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.partitionPar [] FIO.succeed

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [] "partitionPar over an empty seq should yield no errors"
                        Expect.equal oks [] "partitionPar over an empty seq should yield no successes"

                    testPropertyWithConfig fsCheckConfig "partitionPar - all-success collects every result and preserves order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.partitionPar xs (fun x -> FIO.succeed x)

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [] "partitionPar with all-success should produce no errors"
                        Expect.equal oks xs "partitionPar with all-success should preserve input order"

                    testPropertyWithConfig fsCheckConfig "partitionPar - all-failure collects every error and preserves order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let f x = FIO.fail (string x)
                        let effect = FIO.partitionPar xs f

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs (List.map string xs) "partitionPar with all-failure should preserve input order in errors"
                        Expect.equal oks [] "partitionPar with all-failure should produce no successes"

                    testPropertyWithConfig fsCheckConfig "partitionPar - mixed input splits and preserves order in both lists"
                    <| fun (runtime: FIORuntime) ->
                        let xs = [ 0; 1; 2; 3; 4; 5 ]
                        let f x =
                            if x % 2 = 0 then FIO.succeed x
                            else FIO.fail (string x)
                        let effect = FIO.partitionPar xs f

                        let errs, oks =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal errs [ "1"; "3"; "5" ] "partitionPar should preserve input order in errors"
                        Expect.equal oks [ 0; 2; 4 ] "partitionPar should preserve input order in successes"

                    testCase "partitionPar - does not interrupt siblings on failure"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let mutable completed = 0
                            let n = 10
                            let f i =
                                (FIO.attempt
                                    (fun () ->
                                        Interlocked.Increment(&completed) |> ignore)
                                    (fun ex -> ex.Message)
                                ).FlatMap(fun () ->
                                    if i = 0 then FIO.fail "boom"
                                    else FIO.succeed i)

                            let effect = FIO.partitionPar [ 0 .. n - 1 ] f

                            let errs, oks =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal errs [ "boom" ] $"partitionPar on {runtime.GetType().Name} should collect the failure"
                            Expect.equal oks [ 1 .. n - 1 ] $"partitionPar on {runtime.GetType().Name} should let siblings complete"
                            Expect.equal completed n $"partitionPar on {runtime.GetType().Name} should not interrupt siblings on failure"

                    testPropertyWithConfig fsCheckConfig "validate - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.validate [] (fun x -> FIO.succeed x)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "validate over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "validate - all-success returns the result list in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.validate xs (fun x -> FIO.succeed x)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "validate with all-success should preserve input order"

                    testPropertyWithConfig fsCheckConfig "validate - all-failure fails with every error in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        match xs with
                        | [] -> ()
                        | _ ->
                            let f x = FIO.fail (string x)
                            let effect = FIO.validate xs f

                            let errs =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal errs (List.map string xs) "validate with all-failure should preserve input order in errors"

                    testPropertyWithConfig fsCheckConfig "validate - mixed input fails with only the errors"
                    <| fun (runtime: FIORuntime) ->
                        let xs = [ 0; 1; 2; 3; 4; 5 ]
                        let f x =
                            if x % 2 = 0 then FIO.succeed x
                            else FIO.fail (string x)
                        let effect = FIO.validate xs f

                        let errs =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal errs [ "1"; "3"; "5" ] "validate should fail with only the per-input errors in source order"

                    testCase "validate - stack-safe over 10000 items"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let xs = [ 1 .. 10000 ]

                            let effect = FIO.validate xs (fun x -> FIO.succeed x)

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length result) 10000 $"validate on {runtime.GetType().Name} should handle 10000 items"

                    testPropertyWithConfig fsCheckConfig "validatePar - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.validatePar [] (fun x -> FIO.succeed x)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "validatePar over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "validatePar - all-success returns the result list in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effect = FIO.validatePar xs (fun x -> FIO.succeed x)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "validatePar with all-success should preserve input order"

                    testPropertyWithConfig fsCheckConfig "validatePar - all-failure fails with every error in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        match xs with
                        | [] -> ()
                        | _ ->
                            let f x = FIO.fail (string x)
                            let effect = FIO.validatePar xs f

                            let errs =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal errs (List.map string xs) "validatePar with all-failure should preserve input order in errors"

                    testPropertyWithConfig fsCheckConfig "validatePar - mixed input fails with only the errors in source order"
                    <| fun (runtime: FIORuntime) ->
                        let xs = [ 0; 1; 2; 3; 4; 5 ]
                        let f x =
                            if x % 2 = 0 then FIO.succeed x
                            else FIO.fail (string x)
                        let effect = FIO.validatePar xs f

                        let errs =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal errs [ "1"; "3"; "5" ] "validatePar should fail with only the per-input errors in source order"

                    testCase "validatePar - does not interrupt siblings on failure"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let mutable completed = 0
                            let n = 10
                            let f i =
                                (FIO.attempt
                                    (fun () ->
                                        Interlocked.Increment(&completed) |> ignore)
                                    (fun ex -> ex.Message)
                                ).FlatMap(fun () ->
                                    if i = 0 then FIO.fail "boom"
                                    else FIO.succeed i)
                            let effect = FIO.validatePar [ 0 .. n - 1 ] f

                            let errs =
                                runtime.Run(effect).UnsafeError()

                            Expect.equal errs [ "boom" ] $"validatePar on {runtime.GetType().Name} should collect the failure"
                            Expect.equal completed n $"validatePar on {runtime.GetType().Name} should not interrupt siblings on failure"

                    testPropertyWithConfig fsCheckConfig "collectAllSuccesses - empty input yields empty list"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.collectAllSuccesses []

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "collectAllSuccesses over an empty seq should yield []"

                    testPropertyWithConfig fsCheckConfig "collectAllSuccesses - all-success returns every result in order"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map FIO.succeed
                        let effect = FIO.collectAllSuccesses effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result xs "collectAllSuccesses with all-success should return every result in source order"

                    testPropertyWithConfig fsCheckConfig "collectAllSuccesses - all-failure returns empty list"
                    <| fun (runtime: FIORuntime, xs: int list) ->
                        let effects = xs |> List.map (fun x -> FIO.fail (string x))
                        let effect = FIO.collectAllSuccesses effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [] "collectAllSuccesses with all-failure should yield []"

                    testPropertyWithConfig fsCheckConfig "collectAllSuccesses - mixed input drops failures and keeps successes in order"
                    <| fun (runtime: FIORuntime) ->
                        let effects =
                            [ FIO.succeed 1
                              FIO.fail "a"
                              FIO.succeed 2
                              FIO.fail "b"
                              FIO.succeed 3 ]
                        let effect = FIO.collectAllSuccesses effects

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result [ 1; 2; 3 ] "collectAllSuccesses should drop failures and keep successes in source order"

                    testCase "collectAllSuccesses - stack-safe over 10000 effects"
                    <| fun () ->
                        for runtime in allRuntimes () do
                            let effects = [ 1 .. 10000 ] |> List.map FIO.succeed
                            let effect = FIO.collectAllSuccesses effects

                            let result =
                                runtime.Run(effect).UnsafeSuccess()

                            Expect.equal (List.length result) 10000 $"collectAllSuccesses on {runtime.GetType().Name} should handle 10000 effects"
                ]
        ]
