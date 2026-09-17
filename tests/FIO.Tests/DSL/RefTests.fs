module FIO.Tests.RefTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

[<Tests>]
let refTests =
    testList
        "Ref"
        [
            testList
                "Read and write"
                [
                    testPropertyWithConfig fsCheckConfig "Get - yields the initial value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let cell = Ref<int> value
                        let result = runtime.Run(cell.Get<string>()).UnsafeSuccess()

                        Expect.equal result value "Get should yield the value the Ref was created with"

                    testPropertyWithConfig fsCheckConfig "Set - replaces the value"
                    <| fun (runtime: FIORuntime, initial: int, next: int) ->
                        let cell = Ref<int> initial

                        let effect: FIO<int, string> =
                            fio {
                                do! cell.Set next
                                return! cell.Get()
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result next "Get after Set should yield the new value"

                    testPropertyWithConfig fsCheckConfig "GetAndSet - yields the previous value and stores the new one"
                    <| fun (runtime: FIORuntime, initial: int, next: int) ->
                        let cell = Ref<int> initial

                        let effect: FIO<int * int, string> =
                            fio {
                                let! previous = cell.GetAndSet next
                                let! current = cell.Get()
                                return previous, current
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (initial, next) "GetAndSet should yield the old value and store the new one"

                    testAllRuntimes "Get - a null reference round-trips" (fun runtime ->
                        let cell = Ref<string> null
                        let result = runtime.Run(cell.Get<string>()).UnsafeSuccess()

                        Expect.isNull result "A Ref holding null should yield null")
                ]

            testList
                "Update"
                [
                    testPropertyWithConfig fsCheckConfig "Update - applies the function"
                    <| fun (runtime: FIORuntime, initial: int, delta: int) ->
                        let cell = Ref<int> initial

                        let effect: FIO<int, string> =
                            fio {
                                do! cell.Update(fun value -> value + delta)
                                return! cell.Get()
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (initial + delta) "Update should store the function's result"

                    testPropertyWithConfig fsCheckConfig "UpdateAndGet - yields the new value"
                    <| fun (runtime: FIORuntime, initial: int, delta: int) ->
                        let cell = Ref<int> initial
                        let effect: FIO<int, string> = cell.UpdateAndGet(fun value -> value + delta)
                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (initial + delta) "UpdateAndGet should yield the updated value"
                        Expect.equal (cell.UnsafeGet()) (initial + delta) "UpdateAndGet should store the updated value"

                    testPropertyWithConfig fsCheckConfig "GetAndUpdate - yields the previous value"
                    <| fun (runtime: FIORuntime, initial: int, delta: int) ->
                        let cell = Ref<int> initial
                        let effect: FIO<int, string> = cell.GetAndUpdate(fun value -> value + delta)
                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result initial "GetAndUpdate should yield the value before the update"
                        Expect.equal (cell.UnsafeGet()) (initial + delta) "GetAndUpdate should store the updated value"

                    testPropertyWithConfig fsCheckConfig "Modify - yields the result and stores the state"
                    <| fun (runtime: FIORuntime, initial: int, delta: int) ->
                        let cell = Ref<int> initial

                        let effect: FIO<string, string> =
                            cell.Modify(fun value -> $"was {value}", value + delta)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result $"was {initial}" "Modify should yield the transition's result"
                        Expect.equal (cell.UnsafeGet()) (initial + delta) "Modify should store the transition's new value"

                    testAllRuntimes "Update - constructing the effect does not run the function" (fun runtime ->
                        let cell = Ref<int> 0
                        let calls = Ref<int> 0

                        let effect: FIO<unit, string> =
                            cell.Update(fun value ->
                                calls.UnsafeUpdate(fun n -> n + 1)
                                value + 1)

                        Expect.equal (calls.UnsafeGet()) 0 "Constructing the effect must not apply the function"
                        runtime.Run(effect).UnsafeSuccess()
                        runtime.Run(effect).UnsafeSuccess()
                        Expect.equal (calls.UnsafeGet()) 2 "Each run should apply the function once"
                        Expect.equal (cell.UnsafeGet()) 2 "Each run should store its update")

                    testAllRuntimes "Update - a throwing function is a defect, not a typed error" (fun runtime ->
                        let cell = Ref<int> 1
                        let effect: FIO<unit, string> = cell.Update(fun _ -> failwith "transition threw")

                        match runtime.Run(effect).UnsafeResult() with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect inner -> Expect.equal inner.Message "transition threw" "The defect should carry the thrown exception"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}"

                        Expect.equal (cell.UnsafeGet()) 1 "A failed transition must leave the value untouched")
                ]

            testList
                "Concurrency"
                [
                    testAllRuntimes "Update - concurrent increments are not lost" (fun runtime ->
                        let fibers = 8
                        let increments = 1000
                        let cell = Ref<int> 0

                        let effect: FIO<unit, string> =
                            FIO.forEachParDiscard (seq { 1 .. fibers }) (fun _ ->
                                FIO.replicateFIODiscard increments (cell.Update(fun value -> value + 1)))

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.equal (cell.UnsafeGet()) (fibers * increments) "Every increment must be applied exactly once")

                    testAllRuntimes "Modify - concurrent transitions hand out unique results" (fun runtime ->
                        let fibers = 8
                        let cell = Ref<int> 0

                        let effect: FIO<int list, string> =
                            FIO.forEachPar (seq { 1 .. fibers }) (fun _ -> cell.Modify(fun value -> value, value + 1))

                        let results = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal (List.sort results) [ 0 .. fibers - 1 ] "Each transition should observe a distinct previous value")
                ]

            testList
                "Unsafe access"
                [
                    testCase "UnsafeGet/UnsafeUpdate/UnsafeModify work outside an effect" (fun () ->
                        let cell = Ref<int> 10

                        Expect.equal (cell.UnsafeGet()) 10 "UnsafeGet should read the initial value"
                        cell.UnsafeUpdate(fun value -> value * 2)
                        Expect.equal (cell.UnsafeGet()) 20 "UnsafeUpdate should store the function's result"

                        let previous = cell.UnsafeModify(fun value -> value, value + 1)
                        Expect.equal previous 20 "UnsafeModify should yield the transition's result"
                        Expect.equal (cell.UnsafeGet()) 21 "UnsafeModify should store the transition's new value")
                ]
        ]
