module FIO.Tests.Extensions.RetryTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.WorkStealing

open Expecto

open System

let private failingCounter (count: int ref) : FIO<int, int> =
    fio {
        count.Value <- count.Value + 1
        return! FIO.fail count.Value
    }

let private failingExnCounter (count: int ref) : FIO<int, exn> =
    fio {
        count.Value <- count.Value + 1
        return! FIO.fail (exn (string count.Value))
    }

let private failingUntil (limit: int) (count: int ref) =
    fio {
        count.Value <- count.Value + 1

        if count.Value < limit then
            return! FIO.fail (exn "transient")
        else
            return count.Value
    }

let private countingAttempt (count: int ref) =
    FIO.attempt
        (fun () ->
            count.Value <- count.Value + 1
            count.Value)
        id

let private predicateFailingAt (limit: int) (otherwise: bool) (n: int) =
    if n >= limit then FIO.fail (exn "pred-error") else FIO.succeed otherwise

let private stackSafety name (run: int ref -> FIORuntime -> int) =
    testCase $"{name} - stack safety with 10000 iterations" (fun () ->
        use runtime = new WorkStealingRuntime()
        let count = ref 0
        let result = run count (runtime :> FIORuntime)

        Expect.equal count.Value 10_000 $"{name} should iterate 10000 times"
        Expect.equal result 10_000 $"{name} should end with the final value")

let private predicateErrorPropagates name (run: int ref -> FIORuntime -> exn) =
    testPropertyWithConfig fsCheckConfig $"{name} - predicate error propagates" (fun (runtime: FIORuntime) ->
        let count = ref 0
        let result = run count runtime

        Expect.equal result.Message "pred-error" $"{name} should propagate the predicate's error"
        Expect.equal count.Value 2 $"{name} should stop on the predicate's failure")

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Retry / repeat"
                [
                    testPropertyWithConfig fsCheckConfig "RetryOrElse - falls back after max retries"
                    <| fun (runtime: FIORuntime, fallbackValue: int) ->
                        let effect = FIO.fail("error").RetryOrElse 2 (fun _ -> FIO.succeed fallbackValue) (fun _ -> FIO.unit ())

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result fallbackValue "RetryOrElse should fall back after max retries"

                    testPropertyWithConfig fsCheckConfig "RetryOrElse - succeeds without fallback"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed(value).RetryOrElse 3 (fun _ -> FIO.succeed -1) (fun _ -> FIO.unit ())

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "RetryOrElse should return original value on success"

                    testPropertyWithConfig fsCheckConfig "RetryOrElse - callback called on each retry"
                    <| fun (runtime: FIORuntime, fallbackValue: int) ->
                        let mutable callbackCount = 0

                        let effect = FIO.fail(0).RetryOrElse
                                        3
                                        (fun _ -> FIO.succeed fallbackValue)
                                        (fun _ ->
                                            callbackCount <- callbackCount + 1
                                            FIO.unit ())

                        let _ =
                            runtime.Run(effect).UnsafeResult()

                        Expect.equal callbackCount 2 "RetryOrElse callback should be called on each retry"

                    testPropertyWithConfig fsCheckConfig "Retry - succeeds immediately without retrying"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable attempts = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    attempts <- attempts + 1
                                    value)
                                id

                        let retried = effect.Retry 3 (fun _ -> FIO.unit ())

                        let actual =
                            runtime.Run(retried).UnsafeSuccess()

                        Expect.equal actual value "Retry should succeed"
                        Expect.equal attempts 1 "Retry should not retry on immediate success"

                    testPropertyWithConfig fsCheckConfig "Retry - retries up to max attempts"
                    <| fun (runtime: FIORuntime) ->
                        let mutable attempts = 0

                        let effect =
                            fio {
                                attempts <- attempts + 1
                                return! FIO.fail "error"
                            }

                        let retried = effect.Retry 4 (fun _ -> FIO.unit ())

                        let _ =
                            runtime.Run(retried).UnsafeResult()

                        Expect.equal attempts 4 "Retry should retry up to max"

                    testPropertyWithConfig fsCheckConfig "Retry - succeeds on intermediate attempt"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable attempts = 0

                        let effect =
                            fio {
                                attempts <- attempts + 1
                                if attempts < 3 then
                                    return! FIO.fail "error"
                                else
                                    return value
                            }

                        let retried = effect.Retry 5 (fun _ -> FIO.unit ())

                        let actual =
                            runtime.Run(retried).UnsafeSuccess()

                        Expect.equal actual value "Retry should succeed on third attempt"
                        Expect.equal attempts 3 "Should take 3 attempts"

                    testPropertyWithConfig fsCheckConfig "Retry - callback receives correct attempt numbers"
                    <| fun (runtime: FIORuntime) ->
                        let mutable attempts = []

                        let effect =
                            FIO.fail("error").Retry
                                3
                                (fun (_, attempt, max) ->
                                    attempts <- attempts @ [ (attempt, max) ]
                                    FIO.unit ())

                        let _ =
                            runtime.Run(effect).UnsafeResult()

                        Expect.equal attempts [ 1, 3; 2, 3 ] "Retry callback should receive correct attempt numbers"

                    testPropertyWithConfig fsCheckConfig "RetryUntil - stops when predicate matches"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            fio {
                                count <- count + 1
                                return! FIO.fail count
                            }

                        let result =
                            runtime.Run(effect.RetryUntil(fun error -> error >= 3)).UnsafeError()

                        Expect.equal result 3 "RetryUntil should fail with the error that matched the predicate"
                        Expect.equal count 3 "RetryUntil should retry until predicate matches"

                    testPropertyWithConfig fsCheckConfig "RetryUntil - fails immediately when predicate true on first error"
                    <| fun (runtime: FIORuntime, errValue: int) ->
                        let mutable count = 0

                        let effect =
                            fio {
                                count <- count + 1
                                return! FIO.fail errValue
                            }

                        let result =
                            runtime.Run(effect.RetryUntil(fun _ -> true)).UnsafeError()

                        Expect.equal count 1 "RetryUntil should fail without retrying"
                        Expect.equal result errValue "RetryUntil should fail with the original error"

                    stackSafety "RetryUntil" (fun count runtime ->
                        runtime.Run((failingCounter count).RetryUntil(fun error -> error >= 10_000)).UnsafeError())

                    testPropertyWithConfig fsCheckConfig "RetryUntilEquals - stops when error matches"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            fio {
                                count <- count + 1
                                return! FIO.fail count
                            }

                        let result =
                            runtime.Run(effect.RetryUntilEquals 4).UnsafeError()

                        Expect.equal count 4 "RetryUntilEquals should retry until equality"
                        Expect.equal result 4 "RetryUntilEquals should fail with the matched error"

                    predicateErrorPropagates "RetryUntilFIO" (fun count runtime ->
                        runtime
                            .Run((failingExnCounter count).RetryUntilFIO(fun (ex: exn) -> predicateFailingAt 2 false (int ex.Message)))
                            .UnsafeError())

                    testPropertyWithConfig fsCheckConfig "RetryWhile - stops when predicate becomes false"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            fio {
                                count <- count + 1
                                return! FIO.fail count
                            }

                        let result =
                            runtime.Run(effect.RetryWhile(fun error -> error < 5)).UnsafeError()

                        Expect.equal count 5 "RetryWhile should retry until predicate is false"
                        Expect.equal result 5 "RetryWhile should fail with the error that failed the predicate"

                    stackSafety "RetryWhile" (fun count runtime ->
                        runtime.Run((failingCounter count).RetryWhile(fun error -> error < 10_000)).UnsafeError())

                    predicateErrorPropagates "RetryWhileFIO" (fun count runtime ->
                        runtime
                            .Run((failingExnCounter count).RetryWhileFIO(fun (ex: exn) -> predicateFailingAt 2 true (int ex.Message)))
                            .UnsafeError())

                    testAllRuntimes "Eventually - succeeds after N failures" (fun runtime ->
                        let mutable count = 0

                        let effect =
                            fio {
                                count <- count + 1
                                if count < 5 then
                                    return! FIO.fail (exn "transient")
                                else
                                    return 42
                            }

                        let result =
                            runtime.Run(effect.Eventually()).UnsafeSuccess()

                        Expect.equal result 42 "Eventually should return the first success value"
                        Expect.equal count 5 "Eventually should retry until success")

                    stackSafety "Eventually" (fun count runtime ->
                        runtime.Run((failingUntil 10_000 count).Eventually()).UnsafeSuccess())

                    testPropertyWithConfig fsCheckConfig "RepeatN - repeats N times, returns last result"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    count)
                                id

                        let result =
                            runtime.Run(effect.RepeatN 5).UnsafeSuccess()

                        Expect.equal count 5 "RepeatN should execute 5 times"
                        Expect.equal result 5 "RepeatN should return last result"

                    testAllRuntimes "RepeatN - n=0 interrupts with InvalidArgument" (fun runtime ->
                        let effect = FIO.succeed 1
                        let result = runtime.Run(effect.RepeatN 0).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | InvalidArgument _ -> ()
                            | cause -> failtest $"RepeatN 0 should interrupt with InvalidArgument, got cause: %A{cause}"
                        | other -> failtest $"RepeatN 0 should interrupt with InvalidArgument, got: %A{other}")

                    testPropertyWithConfig fsCheckConfig "RepeatN - n=1 executes exactly once"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    value)
                                id

                        let result =
                            runtime.Run(effect.RepeatN 1).UnsafeSuccess()

                        Expect.equal count 1 "RepeatN(1) should execute exactly once"
                        Expect.equal result value "RepeatN(1) should return the result"

                    testPropertyWithConfig fsCheckConfig "RepeatUntil - stops on first satisfying value"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    count)
                                id

                        let result =
                            runtime.Run(effect.RepeatUntil(fun r -> r >= 3)).UnsafeSuccess()

                        Expect.equal count 3 "RepeatUntil should execute until predicate is satisfied"
                        Expect.equal result 3 "RepeatUntil should return the value that satisfied the predicate"

                    testPropertyWithConfig fsCheckConfig "RepeatUntil - executes once when predicate true on first try"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    value)
                                id

                        let result =
                            runtime.Run(effect.RepeatUntil(fun _ -> true)).UnsafeSuccess()

                        Expect.equal count 1 "RepeatUntil should execute exactly once"
                        Expect.equal result value "RepeatUntil should return the first result"

                    stackSafety "RepeatUntil" (fun count runtime ->
                        runtime.Run((countingAttempt count).RepeatUntil(fun r -> r >= 10_000)).UnsafeSuccess())

                    testPropertyWithConfig fsCheckConfig "RepeatUntilEquals - stops when value matches"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    count)
                                id

                        let result =
                            runtime.Run(effect.RepeatUntilEquals 4).UnsafeSuccess()

                        Expect.equal count 4 "RepeatUntilEquals should execute until equality"
                        Expect.equal result 4 "RepeatUntilEquals should return the matched value"

                    predicateErrorPropagates "RepeatUntilFIO" (fun count runtime ->
                        runtime.Run((countingAttempt count).RepeatUntilFIO(predicateFailingAt 2 false)).UnsafeError())

                    testPropertyWithConfig fsCheckConfig "RepeatWhile - stops when predicate becomes false"
                    <| fun (runtime: FIORuntime) ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    count)
                                id

                        let result =
                            runtime.Run(effect.RepeatWhile(fun r -> r < 5)).UnsafeSuccess()

                        Expect.equal count 5 "RepeatWhile should execute until predicate is false"
                        Expect.equal result 5 "RepeatWhile should return the value that failed the predicate"

                    stackSafety "RepeatWhile" (fun count runtime ->
                        runtime.Run((countingAttempt count).RepeatWhile(fun r -> r < 10_000)).UnsafeSuccess())

                    predicateErrorPropagates "RepeatWhileFIO" (fun count runtime ->
                        runtime.Run((countingAttempt count).RepeatWhileFIO(predicateFailingAt 2 true)).UnsafeError())

                    testAllRuntimes "Forever - loops until interrupted by Timeout" (fun runtime ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () -> count <- count + 1)
                                id

                        let bounded = effect.Forever().Timeout (TimeSpan.FromMilliseconds 100.0)

                        let result =
                            runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result None "Forever should not produce a value before timeout"
                        Expect.isGreaterThan count 0 "Forever should have run at least once before timeout")

                    testAllRuntimes "Forever - fails with the first failure and stops repeating" (fun runtime ->
                        let mutable count = 0

                        let effect =
                            FIO.attempt
                                (fun () ->
                                    count <- count + 1
                                    if count = 3 then failwith "third run")
                                (fun ex -> ex.Message)

                        let result = runtime.Run(effect.Forever<int>()).UnsafeError()

                        Expect.equal result "third run" "Forever should fail with the first failure"
                        Expect.equal count 3 "Forever should not run the effect again after it fails")

                    testAllRuntimes "Forever - takes the result type its context needs" (fun runtime ->
                        let ticking: FIO<string, exn> = (FIO.sleep (TimeSpan.FromMilliseconds 1.0)).Forever()
                        let raced = ticking.RaceFirst((FIO.sleep (TimeSpan.FromMilliseconds 20.0)).Map(fun () -> "done"))

                        Expect.equal (runtime.Run(raced).UnsafeSuccess()) "done" "A loop typed as string should race a string effect")
                ]
        ]
