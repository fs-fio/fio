/// <summary>Provides property-based tests for atomic reference operations.</summary>
module FIO.Tests.RefTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Ref

open Expecto

open FIO.Runtime
open FIO.Runtime.Direct

let private onError (e: exn) = e.Message

[<Tests>]
let refGetTests =
    testList
        "Ref Get"
        [

            testPropertyWithConfig fsCheckConfig "Get - returns current value"
            <| fun (runtime: FIORuntime, initial: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result initial "Get should return current value"
        ]

[<Tests>]
let refSetTests =
    testList
        "Ref Set"
        [

            testPropertyWithConfig fsCheckConfig "Set - updates value"
            <| fun (runtime: FIORuntime, initial: string, newValue: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        do! ref.Set(newValue, onError)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result newValue "Set should update value"

            testPropertyWithConfig fsCheckConfig "SetExn - updates value"
            <| fun (runtime: FIORuntime, initial: string, newValue: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        do! ref.Set(newValue, id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result newValue "SetExn should update value"

            testPropertyWithConfig fsCheckConfig "Set - multiple sets preserve last value"
            <| fun (runtime: FIORuntime, v1: string, v2: string, v3: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make "initial"
                        do! ref.Set(v1, id)
                        do! ref.Set(v2, id)
                        do! ref.Set(v3, id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result v3 "Last set value should be preserved"
        ]

[<Tests>]
let refGetAndSetTests =
    testList
        "Ref GetAndSet"
        [

            testPropertyWithConfig fsCheckConfig "GetAndSet - returns old value and sets new"
            <| fun (runtime: FIORuntime, initial: string, newValue: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        let! oldValue = ref.GetAndSet(newValue, onError)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "GetAndSet should return old value"
                Expect.equal currentValue newValue "GetAndSet should set new value"

            testPropertyWithConfig fsCheckConfig "GetAndSetExn - returns old value and sets new"
            <| fun (runtime: FIORuntime, initial: string, newValue: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        let! oldValue = ref.GetAndSet(newValue, id)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "GetAndSetExn should return old value"
                Expect.equal currentValue newValue "GetAndSetExn should set new value"
        ]

[<Tests>]
let refGetAndUpdateTests =
    testList
        "Ref GetAndUpdate"
        [

            testPropertyWithConfig fsCheckConfig "GetAndUpdate - returns old value and applies function"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! oldValue = ref.GetAndUpdate((fun x -> x + 10), onError)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "GetAndUpdate should return old value"
                Expect.equal currentValue (initial + 10) "GetAndUpdate should apply function"

            testPropertyWithConfig fsCheckConfig "GetAndUpdateExn - returns old value and applies function"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! oldValue = ref.GetAndUpdate((fun x -> x + 10), id)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "GetAndUpdateExn should return old value"
                Expect.equal currentValue (initial + 10) "GetAndUpdateExn should apply function"

            testPropertyWithConfig fsCheckConfig "GetAndUpdateExn - identity returns same value"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! oldValue = ref.GetAndUpdate(id, id)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "GetAndUpdate id should return old value"
                Expect.equal currentValue initial "GetAndUpdate id should not change value"
        ]

[<Tests>]
let refUpdateAndGetTests =
    testList
        "Ref UpdateAndGet"
        [

            testPropertyWithConfig fsCheckConfig "UpdateAndGet - returns new value and applies function"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! newValue = ref.UpdateAndGet((fun x -> x * 2), onError)
                        let! currentValue = ref.Get()
                        return newValue, currentValue
                    }

                let newValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal newValue (initial * 2) "UpdateAndGet should return new value"
                Expect.equal currentValue (initial * 2) "UpdateAndGet should update value"

            testPropertyWithConfig fsCheckConfig "UpdateAndGetExn - returns new value and applies function"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! newValue = ref.UpdateAndGet((fun x -> x * 2), id)
                        let! currentValue = ref.Get()
                        return newValue, currentValue
                    }

                let newValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal newValue (initial * 2) "UpdateAndGetExn should return new value"
                Expect.equal currentValue (initial * 2) "UpdateAndGetExn should update value"

            testPropertyWithConfig fsCheckConfig "UpdateAndGetExn - with constant function"
            <| fun (runtime: FIORuntime, initial: int, constant: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! newValue = ref.UpdateAndGet((fun _ -> constant), id)
                        return newValue
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result constant "UpdateAndGet constant should return constant"
        ]

[<Tests>]
let refUpdateTests =
    testList
        "Ref Update"
        [

            testPropertyWithConfig fsCheckConfig "Update - applies function without returning"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        do! ref.Update((fun x -> x + 5), onError)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (initial + 5) "Update should apply function"

            testPropertyWithConfig fsCheckConfig "UpdateExn - applies function without returning"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        do! ref.Update((fun x -> x + 5), id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (initial + 5) "UpdateExn should apply function"

            testPropertyWithConfig fsCheckConfig "UpdateExn - multiple updates chain"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        do! ref.Update((fun x -> x + 1), id)
                        do! ref.Update((fun x -> x * 2), id)
                        do! ref.Update((fun x -> x - 3), id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                let expected = (initial + 1) * 2 - 3
                Expect.equal result expected "Multiple updates should chain"
        ]

[<Tests>]
let refCompareAndSetTests =
    testList
        "Ref CompareAndSet"
        [

            testPropertyWithConfig fsCheckConfig "CompareAndSet - succeeds when expected matches"
            <| fun (runtime: FIORuntime, initial: int, newValue: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! success = ref.CompareAndSet(initial, newValue, onError)
                        let! currentValue = ref.Get()
                        return success, currentValue
                    }

                let success, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue success "CompareAndSet should succeed when expected matches"
                Expect.equal currentValue newValue "CompareAndSet should update value"

            testPropertyWithConfig fsCheckConfig "CompareAndSetExn - succeeds when expected matches"
            <| fun (runtime: FIORuntime, initial: int, newValue: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! success = ref.CompareAndSet(initial, newValue, id)
                        let! currentValue = ref.Get()
                        return success, currentValue
                    }

                let success, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue success "CompareAndSetExn should succeed when expected matches"
                Expect.equal currentValue newValue "CompareAndSetExn should update value"

            testPropertyWithConfig fsCheckConfig "CompareAndSetExn - fails when expected does not match"
            <| fun (runtime: FIORuntime, initial: int, wrongExpected: int, newValue: int) ->
                if initial = wrongExpected then
                    ()
                else
                    let eff =
                        fio {
                            let! ref = Ref.makeValue initial
                            let! success = ref.CompareAndSet(wrongExpected, newValue, id)
                            let! currentValue = ref.Get()
                            return success, currentValue
                        }

                    let success, currentValue = runtime.Run(eff).UnsafeSuccess()

                    Expect.isFalse success "CompareAndSetExn should fail when expected does not match"
                    Expect.equal currentValue initial "CompareAndSetExn should not update value"

            testCase "CompareAndSetExn - mutates struct ref when expected matches"
            <| fun () ->
                let runtime = DirectRuntime()

                let eff =
                    fio {
                        let! ref = Ref.makeValue 10
                        let! success = ref.CompareAndSet(10, 99, id)
                        let! value = ref.Get()
                        return success, value
                    }

                let success, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue success "CompareAndSet should report success when expected matches"
                Expect.equal value 99 "CompareAndSet should mutate value when successful"

            testCase "CompareAndSetExn - leaves struct ref unchanged when expected mismatches"
            <| fun () ->
                let runtime = DirectRuntime()

                let eff =
                    fio {
                        let! ref = Ref.makeValue 10
                        let! success = ref.CompareAndSet(11, 99, id)
                        let! value = ref.Get()
                        return success, value
                    }

                let success, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse success "CompareAndSet should report failure when expected mismatches"
                Expect.equal value 10 "CompareAndSet should not mutate value when expected mismatches"
        ]

[<Tests>]
let refModifyTests =
    testList
        "Ref Modify"
        [

            testPropertyWithConfig fsCheckConfig "Modify - returns computed result and updates value"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! result = ref.Modify((fun x -> x * 2, x.ToString()), onError)
                        let! currentValue = ref.Get()
                        return result, currentValue
                    }

                let result, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (initial.ToString()) "Modify should return computed result"
                Expect.equal currentValue (initial * 2) "Modify should update value"

            testPropertyWithConfig fsCheckConfig "ModifyExn - returns computed result and updates value"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! result = ref.Modify((fun x -> (x * 2, x.ToString())), id)
                        let! currentValue = ref.Get()
                        return result, currentValue
                    }

                let result, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (initial.ToString()) "ModifyExn should return computed result"
                Expect.equal currentValue (initial * 2) "ModifyExn should update value"

            testPropertyWithConfig fsCheckConfig "ModifyExn - swap pattern"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! oldValue = ref.Modify((fun x -> (x + 100, x)), id)
                        let! newValue = ref.Get()
                        return oldValue, newValue
                    }

                let oldValue, newValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "Modify should return old value"
                Expect.equal newValue (initial + 100) "Modify should update to new value"
        ]

[<Tests>]
let refValueTypeTests =
    testList
        "RefValue"
        [

            testPropertyWithConfig fsCheckConfig "RefValue - Get returns current value"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result initial "RefValue Get should return current value"

            testPropertyWithConfig fsCheckConfig "RefValue - Set updates value"
            <| fun (runtime: FIORuntime, initial: int, newValue: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        do! ref.Set(newValue, id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result newValue "RefValue Set should update value"

            testPropertyWithConfig fsCheckConfig "RefValue - GetAndSet returns old and sets new"
            <| fun (runtime: FIORuntime, initial: int, newValue: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! oldValue = ref.GetAndSet(newValue, id)
                        let! currentValue = ref.Get()
                        return oldValue, currentValue
                    }

                let oldValue, currentValue = runtime.Run(eff).UnsafeSuccess()

                Expect.equal oldValue initial "RefValue GetAndSet should return old"
                Expect.equal currentValue newValue "RefValue GetAndSet should set new"

            testPropertyWithConfig fsCheckConfig "RefValue - works with decimal struct type"
            <| fun (runtime: FIORuntime) ->
                let initial = 123.45m
                let newValue = 678.90m

                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        do! ref.Set(newValue, id)
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result newValue "RefValue should work with decimal"
        ]

[<Tests>]
let refMakeTests =
    testList
        "Ref.make / Ref.makeValue"
        [

            testPropertyWithConfig fsCheckConfig "Ref.make - creates ref with initial value"
            <| fun (runtime: FIORuntime, initial: string) ->
                let eff =
                    fio {
                        let! ref = Ref.make initial
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result initial "Ref should contain initial value"

            testPropertyWithConfig fsCheckConfig "Ref.makeValue - creates value ref with initial value"
            <| fun (runtime: FIORuntime, initial: int) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue initial
                        let! value = ref.Get()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result initial "RefValue should contain initial value"

            testPropertyWithConfig
                fsCheckConfig
                "Ref.make - lazy (constructing once, running twice yields independent refs)"
            <| fun (runtime: FIORuntime) ->
                let eff = Ref.make "initial"

                let r1 = runtime.Run(eff).UnsafeSuccess()
                let r2 = runtime.Run(eff).UnsafeSuccess()

                runtime.Run(r1.Set("mutated", onError)) |> ignore
                let v2 = runtime.Run(r2.Get()).UnsafeSuccess()

                Expect.isFalse (obj.ReferenceEquals(r1, r2)) "Ref.make must allocate a fresh ref per run"
                Expect.equal v2 "initial" "Mutation of one run's ref must not affect another run's ref"

            testPropertyWithConfig
                fsCheckConfig
                "Ref.makeValue - lazy (constructing once, running twice yields independent refs)"
            <| fun (runtime: FIORuntime) ->
                let eff = Ref.makeValue 0

                let r1 = runtime.Run(eff).UnsafeSuccess()
                let r2 = runtime.Run(eff).UnsafeSuccess()

                runtime.Run(r1.Set(99, onError)) |> ignore
                let v2 = runtime.Run(r2.Get()).UnsafeSuccess()

                Expect.isFalse (obj.ReferenceEquals(r1, r2)) "Ref.makeValue must allocate a fresh ref per run"
                Expect.equal v2 0 "Mutation of one run's ref must not affect another run's ref"
        ]

[<Tests>]
let refConcurrencyTests =
    testList
        "Ref Concurrency"
        [

            testPropertyWithConfig fsCheckConfig "CompareAndSet under contention"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! ref = Ref.makeValue 0
                        let! successCount = Ref.makeValue 0

                        let casFiber id =
                            fio {
                                let! success = ref.CompareAndSet(0, id, Operators.id)

                                if success then
                                    do! successCount.Update((fun x -> x + 1), Operators.id)
                            }

                        let! fibers =
                            [ 1..10 ] |> List.map (fun id -> (casFiber id).Fork()) |> FIO.collectAll

                        for fiber in fibers do
                            do! fiber.Join()

                        let! count = successCount.Get()
                        return count
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "Only one CAS should succeed"
        ]
