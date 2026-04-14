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
                        do! ref.SetExn newValue
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
                        do! ref.SetExn v1
                        do! ref.SetExn v2
                        do! ref.SetExn v3
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
                        let! oldValue = ref.GetAndSetExn newValue
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
                        let! oldValue = ref.GetAndUpdateExn(fun x -> x + 10)
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
                        let! oldValue = ref.GetAndUpdateExn id
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
                        let! newValue = ref.UpdateAndGetExn(fun x -> x * 2)
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
                        let! newValue = ref.UpdateAndGetExn(fun _ -> constant)
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
                        do! ref.UpdateExn(fun x -> x + 5)
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
                        do! ref.UpdateExn(fun x -> x + 1)
                        do! ref.UpdateExn(fun x -> x * 2)
                        do! ref.UpdateExn(fun x -> x - 3)
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
                        let! success = ref.CompareAndSetExn(initial, newValue)
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
                            let! success = ref.CompareAndSetExn(wrongExpected, newValue)
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
                        let! success = ref.CompareAndSetExn(10, 99)
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
                        let! success = ref.CompareAndSetExn(11, 99)
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
                        let! result = ref.ModifyExn(fun x -> x * 2, x.ToString())
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
                        let! oldValue = ref.ModifyExn(fun x -> x + 100, x)
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
                        do! ref.SetExn newValue
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
                        let! oldValue = ref.GetAndSetExn newValue
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
                        do! ref.SetExn newValue
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
                                let! success = ref.CompareAndSetExn(0, id)

                                if success then
                                    do! successCount.UpdateExn(fun x -> x + 1)
                            }

                        let! fibers = [ 1..10 ] |> List.map (fun id -> (casFiber id).Fork()) |> FIO.collectAll

                        for fiber in fibers do
                            do! fiber.Join()

                        let! count = successCount.Get()
                        return count
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "Only one CAS should succeed"
        ]
