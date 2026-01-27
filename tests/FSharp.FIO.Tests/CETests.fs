module FSharp.FIO.Tests.CETests

open FSharp.FIO.Tests.Utilities.FsCheckProperties

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open Expecto

open System

/// Simple IDisposable for testing use/use! keywords
type TestDisposable() =
    let mutable disposed = false

    member _.IsDisposed =
        disposed

    interface IDisposable with
        member _.Dispose() = disposed <- true

[<Tests>]
let ceTests =
    testList "Computation Expression" [

        // 1. return - Returns value wrapped in effect
        testPropertyWithConfig fsCheckPropertyTestsConfig "return wraps value in effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio { return value }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "return should wrap value in effect"

        // 2. return! - Returns existing effect
        testPropertyWithConfig fsCheckPropertyTestsConfig "return! returns existing effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let innerEff = FIO.succeed value
            let eff = fio { return! innerEff }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "return! should return existing effect"

        // 3. let! - Binds effect result to variable
        testPropertyWithConfig fsCheckPropertyTestsConfig "let! binds effect result to variable"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                let! x = FIO.succeed value
                return x + 1
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (value + 1) "let! should bind effect result"

        // 4. do! - Executes unit effect
        testPropertyWithConfig fsCheckPropertyTestsConfig "do! executes unit effect"
        <| fun (runtime: FIORuntime) ->
            let mutable executed = false
            let eff = fio {
                do! FIO.suspend(fun () -> executed <- true; FIO.unit())
                return executed
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "do! should execute unit effect"

        // 5. for...do - Iterates over sequence, executes body for each
        testPropertyWithConfig fsCheckPropertyTestsConfig "for...do iterates over sequence"
        <| fun (runtime: FIORuntime) ->
            let mutable counter = 0
            let eff = fio {
                for i in [1; 2; 3; 4; 5] do
                    do! FIO.suspend(fun () -> counter <- counter + i; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 15 "for...do should iterate and accumulate sum"

        // 6. for...do (empty) - Empty sequence returns unit
        testPropertyWithConfig fsCheckPropertyTestsConfig "for...do with empty sequence returns unit"
        <| fun (runtime: FIORuntime) ->
            let mutable counter = 0
            let eff = fio {
                for _ in ([] : int list) do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 0 "for...do with empty sequence should not execute body"

        // 7. while...do - Loops while condition true
        testPropertyWithConfig fsCheckPropertyTestsConfig "while...do loops while condition true"
        <| fun (runtime: FIORuntime) ->
            let mutable counter = 0
            let eff = fio {
                while counter < 5 do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 5 "while...do should loop until condition is false"

        // 8. while...do (false initially) - Never executes body
        testPropertyWithConfig fsCheckPropertyTestsConfig "while...do with false condition never executes body"
        <| fun (runtime: FIORuntime) ->
            let mutable counter = 0
            let eff = fio {
                while false do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 0 "while...do with false condition should not execute body"

        // 9. try...with - Catches error and recovers
        testPropertyWithConfig fsCheckPropertyTestsConfig "try...with catches error and recovers"
        <| fun (runtime: FIORuntime, errorValue: int, recoveryValue: int) ->
            let eff = fio {
                try
                    return! FIO.fail errorValue
                with
                | err -> return err + recoveryValue
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (errorValue + recoveryValue) "try...with should catch error and recover"

        // 10. try...with - Success path not affected
        testPropertyWithConfig fsCheckPropertyTestsConfig "try...with success path not affected"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                try
                    return value
                with
                | _ -> return -1
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...with should not affect success path"

        // 11. try...finally - Finalizer runs on success
        testPropertyWithConfig fsCheckPropertyTestsConfig "try...finally finalizer runs on success"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable finalizerRan = false
            let eff = 
                (FIO.succeed value).Ensuring(FIO.suspend(fun () -> finalizerRan <- true; FIO.unit()))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...finally should return value"
            Expect.isTrue finalizerRan "try...finally finalizer should run on success"

        // 12. try...finally - Finalizer runs on failure
        testPropertyWithConfig fsCheckPropertyTestsConfig "try...finally finalizer runs on failure"
        <| fun (runtime: FIORuntime, errorValue: int) ->
            let mutable finalizerRan = false
            let eff : FIO<int, int> = 
                (FIO.fail errorValue).Ensuring(FIO.suspend(fun () -> finalizerRan <- true; FIO.unit()))

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result errorValue "try...finally should propagate error"
            Expect.isTrue finalizerRan "try...finally finalizer should run on failure"

        // 13. use - Disposes resource after use
        testPropertyWithConfig fsCheckPropertyTestsConfig "use disposes resource after use"
        <| fun (runtime: FIORuntime, value: int) ->
            let resource = new TestDisposable()
            let eff = fio {
                use _ = resource
                return value
            }

            Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "use should return value"
            Expect.isTrue resource.IsDisposed "use should dispose resource after use"

        // 14. use - Disposes resource even on failure
        testPropertyWithConfig fsCheckPropertyTestsConfig "use disposes resource even on failure"
        <| fun (runtime: FIORuntime, errorValue: int) ->
            let resource = new TestDisposable()
            let eff : FIO<int, int> = fio {
                use _ = resource
                return! FIO.fail errorValue
            }

            Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result errorValue "use should propagate error"
            Expect.isTrue resource.IsDisposed "use should dispose resource even on failure"

        // 15. let! ... and! - MergeSources: zips two effects
        testPropertyWithConfig fsCheckPropertyTestsConfig "let! ... and! zips two effects"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff = fio {
                let! x = FIO.succeed a
                and! y = FIO.succeed b
                return x + y
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b) "let! ... and! should zip two effects"

        // 16. let! ... and! ... and! - MergeSources3: zips three effects
        testPropertyWithConfig fsCheckPropertyTestsConfig "let! ... and! ... and! zips three effects"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
            let eff = fio {
                let! x = FIO.succeed a
                and! y = FIO.succeed b
                and! z = FIO.succeed c
                return x + y + z
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c) "let! ... and! ... and! should zip three effects"

        // 17. yield - Yields value (same as return)
        testPropertyWithConfig fsCheckPropertyTestsConfig "yield yields value same as return"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio { yield value }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "yield should yield value same as return"

        // 18. yield! - Yields effect (same as return!)
        testPropertyWithConfig fsCheckPropertyTestsConfig "yield! yields effect same as return!"
        <| fun (runtime: FIORuntime, value: int) ->
            let innerEff = FIO.succeed value
            let eff = fio { yield! innerEff }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "yield! should yield effect same as return!"
    ]
