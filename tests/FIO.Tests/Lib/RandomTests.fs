module FIO.Tests.RandomTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL

open Expecto

open FIO.Runtime

module Random = FIO.Random.Random

[<Tests>]
let randomNextIntTests =
    testList "Random.nextInt" [

        testPropertyWithConfig fsCheckConfig "Random.nextInt returns non-negative int"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextInt()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0 "Should return non-negative int"

        testPropertyWithConfig fsCheckConfig "Random.nextInt returns different values (usually)"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! v1 = Random.nextInt()
                let! v2 = Random.nextInt()
                let! v3 = Random.nextInt()
                return v1, v2, v3
            }

            let v1, v2, v3 = runtime.Run(eff).UnsafeSuccess()

            let allSame = v1 = v2 && v2 = v3
            if allSame then
                let v4, v5, v6 = runtime.Run(eff).UnsafeSuccess()
                let stillAllSame = v4 = v5 && v5 = v6 && v4 = v1
                Expect.isFalse stillAllSame "Random values should vary (failed twice)"
    ]

[<Tests>]
let randomNextIntBoundedTests =
    testList "Random.nextIntBounded" [

        testPropertyWithConfig fsCheckConfig "Random.nextIntBounded returns value in range [0, max)"
        <| fun (runtime: FIORuntime) ->
            let max = 100
            let eff = Random.nextIntBounded max

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0 "Should be >= 0"
            Expect.isLessThan result max "Should be < max"

        testPropertyWithConfig fsCheckConfig "Random.nextIntBounded with max=1 always returns 0"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! results =
                    [1..10]
                    |> List.map (fun _ -> Random.nextIntBounded 1)
                    |> FIO.collectAll
                return results |> List.forall ((=) 0)
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "nextIntBounded(1) should always return 0"
    ]

[<Tests>]
let randomNextIntRangeTests =
    testList "Random.nextIntRange" [

        testPropertyWithConfig fsCheckConfig "Random.nextIntRange returns value in range [min, max)"
        <| fun (runtime: FIORuntime) ->
            let min = 10
            let max = 20
            let eff = Random.nextIntRange(min, max)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result min "Should be >= min"
            Expect.isLessThan result max "Should be < max"

        testPropertyWithConfig fsCheckConfig "Random.nextIntRange with min=max-1 always returns min"
        <| fun (runtime: FIORuntime) ->
            let min = 42
            let max = 43
            let eff = fio {
                let! results =
                    [1..10]
                    |> List.map (fun _ -> Random.nextIntRange(min, max))
                    |> FIO.collectAll
                return results |> List.forall ((=) min)
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "nextIntRange(n, n+1) should always return n"

        testPropertyWithConfig fsCheckConfig "Random.nextIntRange with negative range"
        <| fun (runtime: FIORuntime) ->
            let min = -100
            let max = -50
            let eff = Random.nextIntRange(min, max)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result min "Should be >= min"
            Expect.isLessThan result max "Should be < max"
    ]

[<Tests>]
let randomNextInt64Tests =
    testList "Random.nextInt64" [

        testPropertyWithConfig fsCheckConfig "Random.nextInt64 returns non-negative int64"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextInt64()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0L "Should return non-negative int64"

        testPropertyWithConfig fsCheckConfig "Random.nextInt64 returns different values (usually)"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! v1 = Random.nextInt64()
                let! v2 = Random.nextInt64()
                let! v3 = Random.nextInt64()
                return v1, v2, v3
            }

            let v1, v2, v3 = runtime.Run(eff).UnsafeSuccess()

            let allSame = v1 = v2 && v2 = v3
            if allSame then
                let v4, v5, v6 = runtime.Run(eff).UnsafeSuccess()
                let stillAllSame = v4 = v5 && v5 = v6 && v4 = v1
                Expect.isFalse stillAllSame "Random int64 values should vary (failed twice)"
    ]

[<Tests>]
let randomNextInt64BoundedTests =
    testList "Random.nextInt64Bounded" [

        testPropertyWithConfig fsCheckConfig "Random.nextInt64Bounded returns value in range [0, max)"
        <| fun (runtime: FIORuntime) ->
            let max = 100L
            let eff = Random.nextInt64Bounded max

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0L "Should be >= 0"
            Expect.isLessThan result max "Should be < max"

        testPropertyWithConfig fsCheckConfig "Random.nextInt64Bounded with max=1 always returns 0"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! results =
                    [1..10]
                    |> List.map (fun _ -> Random.nextInt64Bounded 1L)
                    |> FIO.collectAll
                return results |> List.forall ((=) 0L)
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "nextInt64Bounded(1) should always return 0"
    ]

[<Tests>]
let randomNextInt64RangeTests =
    testList "Random.nextInt64Range" [

        testPropertyWithConfig fsCheckConfig "Random.nextInt64Range returns value in range [min, max)"
        <| fun (runtime: FIORuntime) ->
            let min = 10L
            let max = 20L
            let eff = Random.nextInt64Range(min, max)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result min "Should be >= min"
            Expect.isLessThan result max "Should be < max"

        testPropertyWithConfig fsCheckConfig "Random.nextInt64Range with min=max-1 always returns min"
        <| fun (runtime: FIORuntime) ->
            let min = 42L
            let max = 43L
            let eff = fio {
                let! results =
                    [1..10]
                    |> List.map (fun _ -> Random.nextInt64Range(min, max))
                    |> FIO.collectAll
                return results |> List.forall ((=) min)
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "nextInt64Range(n, n+1) should always return n"
    ]

[<Tests>]
let randomNextDoubleTests =
    testList "Random.nextDouble" [

        testPropertyWithConfig fsCheckConfig "Random.nextDouble returns value in [0.0, 1.0)"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextDouble()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0.0 "Should be >= 0.0"
            Expect.isLessThan result 1.0 "Should be < 1.0"

        testPropertyWithConfig fsCheckConfig "Random.nextDouble returns different values (usually)"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! v1 = Random.nextDouble()
                let! v2 = Random.nextDouble()
                return v1 <> v2
            }

            let results = [1..5] |> List.map (fun _ -> runtime.Run(eff).UnsafeSuccess())

            let atLeastOneDifferent = results |> List.exists id
            Expect.isTrue atLeastOneDifferent "Random doubles should vary"
    ]

[<Tests>]
let randomNextDoubleBoundedTests =
    testList "Random.nextDoubleBounded" [

        testPropertyWithConfig fsCheckConfig "Random.nextDoubleBounded returns value in [0.0, max)"
        <| fun (runtime: FIORuntime) ->
            let max = 10.0
            let eff = Random.nextDoubleBounded max

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result 0.0 "Should be >= 0.0"
            Expect.isLessThan result max "Should be < max"
    ]

[<Tests>]
let randomNextDoubleRangeTests =
    testList "Random.nextDoubleRange" [

        testPropertyWithConfig fsCheckConfig "Random.nextDoubleRange returns value in [min, max)"
        <| fun (runtime: FIORuntime) ->
            let min = 5.0
            let max = 15.0
            let eff = Random.nextDoubleRange(min, max)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result min "Should be >= min"
            Expect.isLessThan result max "Should be < max"
    ]

[<Tests>]
let randomNextBytesTests =
    testList "Random.nextBytes" [

        testPropertyWithConfig fsCheckConfig "Random.nextBytes returns array of requested size"
        <| fun (runtime: FIORuntime) ->
            let count = 16
            let eff = Random.nextBytes count

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result.Length count "Should return array of requested size"

        testPropertyWithConfig fsCheckConfig "Random.nextBytes with count=0 returns empty array"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextBytes 0

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isEmpty result "Should return empty array"

        testPropertyWithConfig fsCheckConfig "Random.nextBytes returns different values (usually)"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! b1 = Random.nextBytes 16
                let! b2 = Random.nextBytes 16
                return b1 <> b2
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Random byte arrays should differ"
    ]

[<Tests>]
let randomNextGuidTests =
    testList "Random.nextGuid" [

        testPropertyWithConfig fsCheckConfig "Random.nextGuid returns non-empty GUID"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextGuid()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.notEqual result System.Guid.Empty "Should return non-empty GUID"

        testPropertyWithConfig fsCheckConfig "Random.nextGuid returns unique GUIDs"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! g1 = Random.nextGuid()
                let! g2 = Random.nextGuid()
                let! g3 = Random.nextGuid()
                return [g1; g2; g3] |> List.distinct |> List.length = 3
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Random GUIDs should be unique"
    ]

[<Tests>]
let randomNextBoolTests =
    testList "Random.nextBool" [

        testPropertyWithConfig fsCheckConfig "Random.nextBool returns true or false"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.nextBool()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue (result || not result) "Should return a boolean"

        testPropertyWithConfig fsCheckConfig "Random.nextBool returns both true and false (usually)"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! results =
                    [1..20]
                    |> List.map (fun _ -> Random.nextBool())
                    |> FIO.collectAll
                let hasTrue = results |> List.exists id
                let hasFalse = results |> List.exists not
                return hasTrue && hasFalse
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Should return both true and false over many calls"
    ]

[<Tests>]
let randomShuffleTests =
    testList "Random.shuffle" [

        testPropertyWithConfig fsCheckConfig "Random.shuffle preserves all elements"
        <| fun (runtime: FIORuntime, items: int list) ->
            if List.isEmpty items then ()
            else
                let eff = Random.shuffle items

                let result = runtime.Run(eff).UnsafeSuccess()

                let sortedResult = result |> List.sort
                let sortedOriginal = items |> List.sort
                Expect.equal sortedResult sortedOriginal "Shuffle should preserve all elements"

        testPropertyWithConfig fsCheckConfig "Random.shuffle preserves length"
        <| fun (runtime: FIORuntime, items: int list) ->
            let eff = Random.shuffle items

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result.Length items.Length "Shuffle should preserve length"

        testPropertyWithConfig fsCheckConfig "Random.shuffle on empty list returns empty"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.shuffle []

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isEmpty result "Shuffle of empty should be empty"

        testPropertyWithConfig fsCheckConfig "Random.shuffle on single element returns same"
        <| fun (runtime: FIORuntime, item: int) ->
            let eff = Random.shuffle [item]

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [item] "Shuffle of single should be same"
    ]

[<Tests>]
let randomChoiceTests =
    testList "Random.choice" [

        testPropertyWithConfig fsCheckConfig "Random.choice returns Some element from non-empty list"
        <| fun (runtime: FIORuntime, items: int list) ->
            if List.isEmpty items then ()
            else
                let eff = Random.choice items

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some v -> Expect.contains items v "Choice should return element from list"
                | None -> failtest "Choice should return Some for non-empty list"

        testPropertyWithConfig fsCheckConfig "Random.choice returns None for empty list"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.choice []

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isNone result "Choice should return None for empty list"

        testPropertyWithConfig fsCheckConfig "Random.choice on single element returns that element"
        <| fun (runtime: FIORuntime, item: int) ->
            let eff = Random.choice [item]

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (Some item) "Choice of single should return that element"
    ]

[<Tests>]
let randomChoiceOrFailTests =
    testList "Random.choiceOrFail" [

        testPropertyWithConfig fsCheckConfig "Random.choiceOrFail returns element from non-empty list"
        <| fun (runtime: FIORuntime, items: int list) ->
            if List.isEmpty items then ()
            else
                let eff = Random.choiceOrFail(items, fun () -> exn "empty")

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.contains items result "ChoiceOrFail should return element from list"

        testPropertyWithConfig fsCheckConfig "Random.choiceOrFail fails on empty list"
        <| fun (runtime: FIORuntime) ->
            let eff = Random.choiceOrFail([], fun () -> exn "empty list")

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result.Message "empty list" "Should fail with provided error on empty list"
    ]
