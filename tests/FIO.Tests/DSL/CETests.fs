module FIO.Tests.CETests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

/// Simple IDisposable for testing use/use! keywords
type TestDisposable() =
    let mutable disposed = false

    member _.IsDisposed =
        disposed

    interface IDisposable with
        member _.Dispose() = disposed <- true

let private runtimes () = [
    new DirectRuntime() :> FIORuntime
    new CooperativeRuntime() :> FIORuntime
    new ConcurrentRuntime() :> FIORuntime
]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [
        for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> f rt)
    ]

[<Tests>]
let ceTests =
    testList "Computation Expression" [

        // === Bind ===

        testPropertyWithConfig fsCheckConfig "Bind - let! binds effect result to variable"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                let! x = FIO.succeed value
                return x + 1
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (value + 1) "let! should bind effect result"

        testPropertyWithConfig fsCheckConfig "Bind - do! followed by return"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable executed = false
            let eff = fio {
                do! FIO.suspend(fun () -> executed <- true; FIO.unit())
                return value
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "do! followed by return should work"
            Expect.isTrue executed "do! should execute side effect"

        testAllRuntimes "Bind - multiple do! in sequence" (fun runtime ->
            let mutable order = []
            let eff = fio {
                do! FIO.suspend(fun () -> order <- order @ [1]; FIO.unit())
                do! FIO.suspend(fun () -> order <- order @ [2]; FIO.unit())
                do! FIO.suspend(fun () -> order <- order @ [3]; FIO.unit())
                return order
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "Multiple do! should execute in sequence")

        testAllRuntimes "Bind - let! chain short-circuits on error" (fun runtime ->
            let mutable thirdRan = false
            let eff : FIO<int, string> = fio {
                let! a = FIO.succeed 1
                let! _b = FIO.fail "boom"
                do! FIO.suspend(fun () -> thirdRan <- true; FIO.unit())
                return a
            }

            let result = runtime.Run(eff).UnsafeResult()

            match result with
            | Failed err -> Expect.equal err "boom" "Should propagate error"
            | _ -> failtest $"Expected Failed, got {result}"
            Expect.isFalse thirdRan "Effects after failure should not run")

        // === BindReturn ===

        testPropertyWithConfig fsCheckConfig "BindReturn - let!/return maps result"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio.BindReturn(FIO.succeed value, fun x -> x + 1)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (value + 1) "BindReturn should map the bound result"

        testPropertyWithConfig fsCheckConfig "BindReturn - mapper not invoked on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable mapped = false
            let eff : FIO<int, int> =
                fio.BindReturn(FIO.fail err, fun _ -> mapped <- true; 1)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "BindReturn should propagate the error"
            Expect.isFalse mapped "BindReturn mapper should not run on failure"

        // === Return / ReturnFrom / Yield / YieldFrom ===

        testPropertyWithConfig fsCheckConfig "Return - return wraps value in effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio { return value }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "return should wrap value in effect"

        testPropertyWithConfig fsCheckConfig "ReturnFrom - return! returns existing effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let innerEff = FIO.succeed value
            let eff = fio { return! innerEff }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "return! should return existing effect"

        testPropertyWithConfig fsCheckConfig "Yield - yield yields value same as return"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio { yield value }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "yield should yield value same as return"

        testPropertyWithConfig fsCheckConfig "YieldFrom - yield! yields effect same as return!"
        <| fun (runtime: FIORuntime, value: int) ->
            let innerEff = FIO.succeed value
            let eff = fio { yield! innerEff }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "yield! should yield effect same as return!"

        // === Zero ===

        testAllRuntimes "Zero - empty fio block returns unit" (fun runtime ->
            let eff : FIO<unit, string> = fio { () }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "Empty fio block should return unit")

        testAllRuntimes "Zero - if without else uses Zero" (fun runtime ->
            let mutable executed = false
            let eff : FIO<unit, string> = fio {
                if true then
                    do! FIO.suspend(fun () -> executed <- true; FIO.unit())
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "if without else should return unit"
            Expect.isTrue executed "if branch should execute")

        testAllRuntimes "Zero - if false without else returns unit" (fun runtime ->
            let mutable executed = false
            let eff : FIO<unit, string> = fio {
                if false then
                    do! FIO.suspend(fun () -> executed <- true; FIO.unit())
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "if false without else should return unit"
            Expect.isFalse executed "if branch should not execute")

        // === Combine ===

        testAllRuntimes "Combine - sequences multiple statements correctly" (fun runtime ->
            let mutable order = []
            let eff = fio {
                do! FIO.suspend(fun () -> order <- order @ [1]; FIO.unit())
                do! FIO.suspend(fun () -> order <- order @ [2]; FIO.unit())
                if true then
                    do! FIO.suspend(fun () -> order <- order @ [3]; FIO.unit())
                do! FIO.suspend(fun () -> order <- order @ [4]; FIO.unit())
                return ()
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "Combine should return unit"
            Expect.equal order [1; 2; 3; 4] "Statements should execute in order")

        testPropertyWithConfig fsCheckConfig "Combine - stops when first effect fails"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable secondRan = false
            let first : FIO<unit, int> = FIO.fail err
            let second : FIO<unit, int> =
                FIO.suspend(fun () -> secondRan <- true; FIO.unit())
            let eff = fio.Combine(first, second)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Combine should propagate the first error"
            Expect.isFalse secondRan "Second effect should not run after failure"

        testPropertyWithConfig fsCheckConfig "Combine - propagates error from second effect"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable firstRan = false
            let mutable secondRan = false
            let first : FIO<unit, int> =
                FIO.suspend(fun () -> firstRan <- true; FIO.unit())
            let second : FIO<unit, int> =
                FIO.suspend(fun () -> secondRan <- true; FIO.fail err)
            let eff = fio.Combine(first, second)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Combine should propagate the second error"
            Expect.isTrue firstRan "First effect should run before failure"
            Expect.isTrue secondRan "Second effect should run and fail"

        // === Delay ===

        testPropertyWithConfig fsCheckConfig "Delay - body is deferred until Run"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable ran = false
            let eff = fio {
                ran <- true
                return value
            }

            Expect.isFalse ran "Delayed body should not run before Run"
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Delayed body should return expected value"
            Expect.isTrue ran "Delayed body should run during Run"

        // === TryWith ===

        testPropertyWithConfig fsCheckConfig "TryWith - catches error and recovers"
        <| fun (runtime: FIORuntime, errorValue: int, recoveryValue: int) ->
            let eff = fio {
                try
                    return! FIO.fail errorValue
                with
                | err -> return err + recoveryValue
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (errorValue + recoveryValue) "try...with should catch error and recover"

        testPropertyWithConfig fsCheckConfig "TryWith - success path not affected"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                try
                    return value
                with
                | _ -> return -1
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...with should not affect success path"

        testAllRuntimes "TryWith - pattern matching on error" (fun runtime ->
            let eff = fio {
                try
                    return! FIO.fail 42
                with
                | e when e > 40 -> return "large"
                | e when e > 20 -> return "medium"
                | _ -> return "small"
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "large" "try...with should pattern match on error")

        testAllRuntimes "TryWith - nested try...with catches inner error" (fun runtime ->
            let eff = fio {
                try
                    try
                        return! FIO.fail "inner"
                    with
                    | "inner" -> return "caught inner"
                    | _ -> return "other inner"
                with
                | _ -> return "outer"
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "caught inner" "Nested try...with should catch inner error")

        // === TryFinally ===

        testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs on success"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable finalizerRan = false
            let eff = fio {
                try
                    return value
                finally
                    finalizerRan <- true
                    FIO.unit()
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...finally should return value"
            Expect.isTrue finalizerRan "try...finally finalizer should run on success"

        testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs on failure"
        <| fun (runtime: FIORuntime, errorValue: int) ->
            let mutable finalizerRan = false
            let eff : FIO<int, int> = fio {
                try
                    return! FIO.fail errorValue
                finally
                    finalizerRan <- true
                    FIO.unit()
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result errorValue "try...finally should propagate error"
            Expect.isTrue finalizerRan "try...finally finalizer should run on failure"

        testPropertyWithConfig fsCheckConfig "TryFinally - finalizer is deferred until run"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable finalizerRan = false
            let eff = fio {
                try
                    return value
                finally
                    finalizerRan <- true
                    FIO.unit()
            }

            Expect.isFalse finalizerRan "finalizer should not run before Run"
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...finally should return value"
            Expect.isTrue finalizerRan "finalizer should run after Run"

        testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs after multiple statements"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable order = []
            let eff = fio {
                try
                    do! FIO.suspend(fun () -> order <- order @ ["body1"]; FIO.unit())
                    do! FIO.suspend(fun () -> order <- order @ ["body2"]; FIO.unit())
                    return value
                finally
                    order <- order @ ["finally"]
                    FIO.unit()
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "try...finally should return value"
            Expect.equal order ["body1"; "body2"; "finally"] "Finalizer should run after body completes"

        testPropertyWithConfig fsCheckConfig "TryFinally - nested try...finally runs finalizers in correct order"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable order = []
            let eff = fio {
                try
                    try
                        return value
                    finally
                        order <- order @ ["inner"]
                        FIO.unit()
                finally
                    order <- order @ ["outer"]
                    FIO.unit()
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Nested try...finally should return value"
            Expect.equal order ["inner"; "outer"] "Inner finalizer should run before outer"

        testAllRuntimes "TryFinally - finalizer failure propagates when body succeeds" (fun runtime ->
            let eff : FIO<int, string> = fio {
                try
                    return 42
                finally
                    FIO.fail("finalizer error").Map(ignore)
            }

            let result = runtime.Run(eff).UnsafeResult()

            match result with
            | Failed err -> Expect.equal err "finalizer error" "Finalizer error should propagate"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "TryFinally - body error preserved when finalizer also fails" (fun runtime ->
            let eff : FIO<int, string> = fio {
                try
                    return! FIO.fail "body error"
                finally
                    FIO.fail("finalizer error").Map(ignore)
            }

            let result = runtime.Run(eff).UnsafeResult()

            match result with
            | Failed err -> Expect.equal err "body error" "Body error should be preserved over finalizer error"
            | _ -> failtest $"Expected Failed, got {result}")

        // === For ===

        testAllRuntimes "For - iterates over sequence" (fun runtime ->
            let mutable counter = 0
            let eff = fio {
                for i in [1; 2; 3; 4; 5] do
                    do! FIO.suspend(fun () -> counter <- counter + i; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 15 "for...do should iterate and accumulate sum")

        testAllRuntimes "For - empty sequence returns unit" (fun runtime ->
            let mutable counter = 0
            let eff = fio {
                for _ in ([] : int list) do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 0 "for...do with empty sequence should not execute body")

        testPropertyWithConfig fsCheckConfig "For - error in body propagates"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable iterations = 0
            let eff : FIO<unit, int> = fio {
                for i in [1; 2; 3; 4; 5] do
                    iterations <- iterations + 1
                    if i = 3 then
                        do! FIO.fail err
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "for...do should propagate error from body"
            Expect.equal iterations 3 "Loop should have run 3 times before error"

        testAllRuntimes "For - over lazy sequence disposes enumerator" (fun runtime ->
            let mutable enumeratorDisposed = false
            let lazySeq = seq {
                try
                    yield 1
                    yield 2
                    yield 3
                finally
                    enumeratorDisposed <- true
            }
            let mutable sum = 0
            let eff = fio {
                for i in lazySeq do
                    do! FIO.suspend(fun () -> sum <- sum + i; FIO.unit())
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "for...do should complete"
            Expect.equal sum 6 "for...do should iterate all elements"
            Expect.isTrue enumeratorDisposed "Enumerator should be disposed after iteration")

        testAllRuntimes "For - try/with inside for loop" (fun runtime ->
            let mutable recovered = 0
            let eff = fio {
                for i in [1; 2; 3] do
                    try
                        if i = 2 then
                            do! FIO.fail "skip"
                    with
                    | _ -> recovered <- recovered + 1
            }

            runtime.Run(eff).UnsafeSuccess()
            Expect.equal recovered 1 "Should recover from error inside for loop")

        // === While ===

        testAllRuntimes "While - guard is evaluated at runtime" (fun runtime ->
            let mutable started = false
            let mutable counter = 0
            let eff = fio {
                while started && counter < 3 do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
            }

            started <- true
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "while...do should return unit"
            Expect.equal counter 3 "guard should be evaluated at runtime")

        testAllRuntimes "While - loops while condition true" (fun runtime ->
            let mutable counter = 0
            let eff = fio {
                while counter < 5 do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 5 "while...do should loop until condition is false")

        testAllRuntimes "While - false condition never executes body" (fun runtime ->
            let mutable counter = 0
            let eff = fio {
                while false do
                    do! FIO.suspend(fun () -> counter <- counter + 1; FIO.unit())
                return counter
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 0 "while...do with false condition should not execute body")

        testPropertyWithConfig fsCheckConfig "While - error in body propagates"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable iterations = 0
            let eff : FIO<unit, int> = fio {
                while true do
                    iterations <- iterations + 1
                    if iterations = 3 then
                        do! FIO.fail err
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "while...do should propagate error from body"
            Expect.equal iterations 3 "Loop should have run 3 times before error"

        // === Using ===

        testPropertyWithConfig fsCheckConfig "Using - use disposes resource after use"
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

        testPropertyWithConfig fsCheckConfig "Using - use disposes resource even on failure"
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

        testPropertyWithConfig fsCheckConfig "Using - use! acquires and disposes effectful resource"
        <| fun (runtime: FIORuntime, value: int) ->
            let resource = new TestDisposable()
            let eff = fio {
                use! _ = FIO.succeed resource
                return value
            }

            Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "use! should return value"
            Expect.isTrue resource.IsDisposed "use! should dispose effectfully acquired resource"

        testPropertyWithConfig fsCheckConfig "Using - use! failure during acquisition does not call dispose"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable disposeCalled = false
            let eff : FIO<int, int> = fio {
                use! _ = (FIO.fail err).Map(fun _ ->
                    { new IDisposable with member _.Dispose() = disposeCalled <- true })
                return 42
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "use! should propagate acquisition error"
            Expect.isFalse disposeCalled "Dispose should not be called when acquisition fails"

        testPropertyWithConfig fsCheckConfig "Using - cleanup happens before try...with catches error"
        <| fun (runtime: FIORuntime, err: int) ->
            let resource = new TestDisposable()
            let eff = fio {
                try
                    use _ = resource
                    return! FIO.fail err
                with
                | e -> return e
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result err "try...with should catch the error"
            Expect.isTrue resource.IsDisposed "Resource should be disposed before error is caught"

        testPropertyWithConfig fsCheckConfig "Using - nested use blocks dispose in reverse order"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable disposeOrder = []
            let makeResource name =
                { new IDisposable with
                    member _.Dispose() = disposeOrder <- disposeOrder @ [name] }
            let eff = fio {
                use _ = makeResource "first"
                use _ = makeResource "second"
                use _ = makeResource "third"
                return value
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Nested use should return value"
            Expect.equal disposeOrder ["third"; "second"; "first"] "Resources should dispose in reverse order"

        testAllRuntimes "Using - use with null disposable does not throw" (fun runtime ->
            let mutable ran = false
            let eff : FIO<unit, exn> = fio {
                use _ = (null : IDisposable)
                ran <- true
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "use should return unit"
            Expect.isTrue ran "Body should execute even with null disposable")

        // === Match ===

        testPropertyWithConfig fsCheckConfig "Match - match! pattern matches on effect result"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                match! FIO.succeed value with
                | v when v > 0 -> return "positive"
                | v when v < 0 -> return "negative"
                | _ -> return "zero"
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            let expected = if value > 0 then "positive" elif value < 0 then "negative" else "zero"
            Expect.equal result expected "match! should pattern match on effect result"

        // === MergeSources (and!) ===

        testPropertyWithConfig fsCheckConfig "MergeSources - let! ... and! zips two effects"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff = fio {
                let! x = FIO.succeed a
                and! y = FIO.succeed b
                return x + y
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b) "let! ... and! should zip two effects"

        testPropertyWithConfig fsCheckConfig "MergeSources - and! propagates error from second effect"
        <| fun (runtime: FIORuntime, a: int, err: int) ->
            let eff = fio {
                let! x = FIO.succeed a
                and! y = FIO.fail err
                return x + y
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "and! should propagate error from any effect"

        testPropertyWithConfig fsCheckConfig "MergeSources - and! propagates error from first effect"
        <| fun (runtime: FIORuntime, err: int, b: int) ->
            let eff = fio {
                let! x = FIO.fail err
                and! y = FIO.succeed b
                return x + y
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "and! should propagate error from first effect"

        testPropertyWithConfig fsCheckConfig "MergeSources - and! with multiple errors returns first error"
        <| fun (runtime: FIORuntime, err1: int, err2: int) ->
            let eff = fio {
                let! x = FIO.fail err1
                and! y = FIO.fail err2
                return x + y
            }

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err1 "and! should return first error when multiple effects fail"

        testAllRuntimes "MergeSources - and! executes sequentially" (fun runtime ->
            let mutable order = []
            let eff = fio {
                let! a = FIO.suspend(fun () -> order <- order @ [1]; FIO.succeed 1)
                and! b = FIO.suspend(fun () -> order <- order @ [2]; FIO.succeed 2)
                return (a, b)
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (1, 2) "Should return both values"
            Expect.equal order [1; 2] "and! should execute first then second (sequential)")

        // === MergeSources3-5 ===

        testPropertyWithConfig fsCheckConfig "MergeSources3 - let! ... and! ... and! zips three effects"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
            let eff = fio {
                let! x = FIO.succeed a
                and! y = FIO.succeed b
                and! z = FIO.succeed c
                return x + y + z
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c) "let! ... and! ... and! should zip three effects"

        testPropertyWithConfig fsCheckConfig "MergeSources3 - error from first short-circuits later effects"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable secondRan = false
            let mutable thirdRan = false
            let second : FIO<int, int> =
                FIO.suspend(fun () -> secondRan <- true; FIO.succeed 1)
            let third : FIO<int, int> =
                FIO.suspend(fun () -> thirdRan <- true; FIO.succeed 2)
            let eff = fio.MergeSources3(FIO.fail err, second, third)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "MergeSources3 should propagate the first error"
            Expect.isFalse secondRan "Second effect should not run after first failure"
            Expect.isFalse thirdRan "Third effect should not run after first failure"

        testPropertyWithConfig fsCheckConfig "MergeSources4 - let! ... and! x4 zips four effects"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int) ->
            let eff = fio {
                let! w = FIO.succeed a
                and! x = FIO.succeed b
                and! y = FIO.succeed c
                and! z = FIO.succeed d
                return w + x + y + z
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d) "MergeSources4 should zip four effects"

        testPropertyWithConfig fsCheckConfig "MergeSources5 - let! ... and! x5 zips five effects"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int, e: int) ->
            let eff = fio {
                let! v = FIO.succeed a
                and! w = FIO.succeed b
                and! x = FIO.succeed c
                and! y = FIO.succeed d
                and! z = FIO.succeed e
                return v + w + x + y + z
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d + e) "MergeSources5 should zip five effects"

        // === Bind3-5 ===

        testPropertyWithConfig fsCheckConfig "Bind3 - binds three effects and continues"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
            let eff =
                fio.Bind3(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    fun (x, y, z) -> FIO.succeed (x + y + z))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c) "Bind3 should pass all three values to continuation"

        testPropertyWithConfig fsCheckConfig "Bind3 - short-circuits on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable continued = false
            let eff : FIO<int, int> =
                fio.Bind3(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    fun _ -> continued <- true; FIO.succeed 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind3 should propagate the first error"
            Expect.isFalse continued "Bind3 continuation should not run on failure"

        testPropertyWithConfig fsCheckConfig "Bind4 - binds four effects and continues"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int) ->
            let eff =
                fio.Bind4(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    FIO.succeed d,
                    fun (w, x, y, z) -> FIO.succeed (w + x + y + z))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d) "Bind4 should pass all four values to continuation"

        testPropertyWithConfig fsCheckConfig "Bind4 - short-circuits on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable continued = false
            let eff : FIO<int, int> =
                fio.Bind4(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    FIO.succeed 3,
                    fun _ -> continued <- true; FIO.succeed 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind4 should propagate the first error"
            Expect.isFalse continued "Bind4 continuation should not run on failure"

        testPropertyWithConfig fsCheckConfig "Bind5 - binds five effects and continues"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int, e: int) ->
            let eff =
                fio.Bind5(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    FIO.succeed d,
                    FIO.succeed e,
                    fun (v, w, x, y, z) -> FIO.succeed (v + w + x + y + z))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d + e) "Bind5 should pass all five values to continuation"

        testPropertyWithConfig fsCheckConfig "Bind5 - short-circuits on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable continued = false
            let eff : FIO<int, int> =
                fio.Bind5(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    FIO.succeed 3,
                    FIO.succeed 4,
                    fun _ -> continued <- true; FIO.succeed 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind5 should propagate the first error"
            Expect.isFalse continued "Bind5 continuation should not run on failure"

        // === Bind3-5Return ===

        testPropertyWithConfig fsCheckConfig "Bind3Return - maps three results"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
            let eff =
                fio.Bind3Return(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    fun (x, y, z) -> x + y + z)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c) "Bind3Return should map three results"

        testPropertyWithConfig fsCheckConfig "Bind3Return - mapper not invoked on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable mapped = false
            let eff : FIO<int, int> =
                fio.Bind3Return(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    fun _ -> mapped <- true; 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind3Return should propagate the first error"
            Expect.isFalse mapped "Bind3Return mapper should not run on failure"

        testPropertyWithConfig fsCheckConfig "Bind4Return - maps four results"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int) ->
            let eff =
                fio.Bind4Return(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    FIO.succeed d,
                    fun (w, x, y, z) -> w + x + y + z)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d) "Bind4Return should map four results"

        testPropertyWithConfig fsCheckConfig "Bind4Return - mapper not invoked on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable mapped = false
            let eff : FIO<int, int> =
                fio.Bind4Return(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    FIO.succeed 3,
                    fun _ -> mapped <- true; 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind4Return should propagate the first error"
            Expect.isFalse mapped "Bind4Return mapper should not run on failure"

        testPropertyWithConfig fsCheckConfig "Bind5Return - maps five results"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int, e: int) ->
            let eff =
                fio.Bind5Return(
                    FIO.succeed a,
                    FIO.succeed b,
                    FIO.succeed c,
                    FIO.succeed d,
                    FIO.succeed e,
                    fun (v, w, x, y, z) -> v + w + x + y + z)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (a + b + c + d + e) "Bind5Return should map five results"

        testPropertyWithConfig fsCheckConfig "Bind5Return - mapper not invoked on failure"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable mapped = false
            let eff : FIO<int, int> =
                fio.Bind5Return(
                    FIO.fail err,
                    FIO.succeed 1,
                    FIO.succeed 2,
                    FIO.succeed 3,
                    FIO.succeed 4,
                    fun _ -> mapped <- true; 0)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result err "Bind5Return should propagate the first error"
            Expect.isFalse mapped "Bind5Return mapper should not run on failure"

        // === Complex / Integration ===

        testAllRuntimes "Complex - control flow with let!/if/for" (fun runtime ->
            let eff = fio {
                let! start = FIO.succeed 0
                let mutable sum = start
                for i in [1; 2; 3; 4; 5] do
                    let! inc = FIO.succeed i
                    if inc % 2 = 0 then
                        sum <- sum + inc
                return sum
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 6 "Complex control flow should work correctly (sum of evens: 2+4=6)")

        testPropertyWithConfig fsCheckConfig "Complex - nested fio blocks"
        <| fun (runtime: FIORuntime, value: int) ->
            let inner = fio {
                let! x = FIO.succeed value
                return x * 2
            }
            let outer = fio {
                let! y = inner
                return y + 1
            }

            let result = runtime.Run(outer).UnsafeSuccess()

            Expect.equal result (value * 2 + 1) "Nested fio blocks should compose"

        testAllRuntimes "Complex - deeply nested fio blocks (stack safety)" (fun runtime ->
            let rec nested depth =
                if depth = 0 then
                    FIO.succeed depth
                else
                    fio {
                        let! inner = nested (depth - 1)
                        return inner + 1
                    }

            let eff = nested 1000

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 1000 "Deep nesting should not overflow stack")

        // === Concurrency within CE ===

        testAllRuntimes "Concurrency - fork and join within CE" (fun runtime ->
            let eff = fio {
                let! fiber = (FIO.succeed 42).Fork()
                let! result = fiber.Join()
                return result
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 42 "Fork/join within CE should work")

        testAllRuntimes "Concurrency - channel send and receive within CE" (fun runtime ->
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(99).Unit()
                let! msg = chan.Receive()
                return msg
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 99 "Channel send/receive within CE should work")
    ]
