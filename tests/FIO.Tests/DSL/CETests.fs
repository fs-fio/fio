module FIO.Tests.CETests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

/// <summary>
/// Simple IDisposable for testing use/use! keywords.
/// </summary>
type TestDisposable() =
    let mutable disposed = false

    member _.IsDisposed = disposed

    interface IDisposable with
        member _.Dispose() = disposed <- true

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let ceTests =
    testList
        "Computation Expression"
        [

            testPropertyWithConfig fsCheckConfig "Bind - let! binds effect result to variable"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff =
                    fio {
                        let! x = FIO.succeed value
                        return x + 1
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (value + 1) "let! should bind effect result"

            testPropertyWithConfig fsCheckConfig "Bind - do! followed by return"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let eff =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "do! followed by return should work"
                Expect.isTrue executed "do! should execute side effect"

            testAllRuntimes "Bind - multiple do! in sequence" (fun runtime ->
                let mutable order = []

                let eff =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 1 ]
                                FIO.unit ())

                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 2 ]
                                FIO.unit ())

                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 3 ]
                                FIO.unit ())

                        return order
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "Multiple do! should execute in sequence")

            testAllRuntimes "Bind - let! chain short-circuits on error" (fun runtime ->
                let mutable thirdRan = false

                let eff =
                    fio {
                        let! a = FIO.succeed 1
                        let! _b = FIO.fail "boom"

                        do!
                            FIO.suspend (fun () ->
                                thirdRan <- true
                                FIO.unit ())

                        return a
                    }

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Failed err -> Expect.equal err "boom" "Should propagate error"
                | _ -> failtest $"Expected Failed, got {result}"

                Expect.isFalse thirdRan "Effects after failure should not run")

            testPropertyWithConfig fsCheckConfig "BindReturn - let!/return maps result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = fio.BindReturn(FIO.succeed value, fun x -> x + 1)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (value + 1) "BindReturn should map the bound result"

            testPropertyWithConfig fsCheckConfig "BindReturn - mapper not invoked on failure"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable mapped = false

                let eff =
                    fio.BindReturn(
                        FIO.fail err,
                        fun _ ->
                            mapped <- true
                            1
                    )

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "BindReturn should propagate the error"
                Expect.isFalse mapped "BindReturn mapper should not run on failure"

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

            testPropertyWithConfig fsCheckConfig "ReturnFrom - return! after do! exercises ReturnFrom via Combine"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let eff =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        return! FIO.succeed value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "return! after do! should work"
                Expect.isTrue executed "do! before return! should execute"

            testPropertyWithConfig fsCheckConfig "YieldFrom - yield! after do! exercises YieldFrom via Combine"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let eff =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        yield! FIO.succeed value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "yield! after do! should work"
                Expect.isTrue executed "do! before yield! should execute"

            testAllRuntimes "Zero - empty fio block returns unit" (fun runtime ->
                let eff = fio { () }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "Empty fio block should return unit")

            testAllRuntimes "Zero - if without else uses Zero" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        if true then
                            do!
                                FIO.suspend (fun () ->
                                    executed <- true
                                    FIO.unit ())
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "if without else should return unit"
                Expect.isTrue executed "if branch should execute")

            testAllRuntimes "Zero - if false without else returns unit" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        if false then
                            do!
                                FIO.suspend (fun () ->
                                    executed <- true
                                    FIO.unit ())
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "if false without else should return unit"
                Expect.isFalse executed "if branch should not execute")

            testAllRuntimes "Combine - sequences multiple statements correctly" (fun runtime ->
                let mutable order = []

                let eff =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 1 ]
                                FIO.unit ())

                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 2 ]
                                FIO.unit ())

                        if true then
                            do!
                                FIO.suspend (fun () ->
                                    order <- order @ [ 3 ]
                                    FIO.unit ())

                        do!
                            FIO.suspend (fun () ->
                                order <- order @ [ 4 ]
                                FIO.unit ())
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "Combine should return unit"
                Expect.equal order [ 1; 2; 3; 4 ] "Statements should execute in order")

            testPropertyWithConfig fsCheckConfig "Combine - stops when first effect fails"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable secondRan = false
                let first = FIO.fail err

                let second =
                    FIO.suspend (fun () ->
                        secondRan <- true
                        FIO.unit ())

                let eff = fio.Combine(first, second)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Combine should propagate the first error"
                Expect.isFalse secondRan "Second effect should not run after failure"

            testPropertyWithConfig fsCheckConfig "Combine - propagates error from second effect"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable firstRan = false
                let mutable secondRan = false

                let first =
                    FIO.suspend (fun () ->
                        firstRan <- true
                        FIO.unit ())

                let second =
                    FIO.suspend (fun () ->
                        secondRan <- true
                        FIO.fail err)

                let eff = fio.Combine(first, second)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Combine should propagate the second error"
                Expect.isTrue firstRan "First effect should run before failure"
                Expect.isTrue secondRan "Second effect should run and fail"

            testPropertyWithConfig fsCheckConfig "Delay - body is deferred until Run"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable ran = false

                let eff =
                    fio {
                        ran <- true
                        return value
                    }

                Expect.isFalse ran "Delayed body should not run before Run"
                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Delayed body should return expected value"
                Expect.isTrue ran "Delayed body should run during Run"

            testAllRuntimes "Delay - running same effect twice produces independent results" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        counter <- counter + 1
                        return counter
                    }

                let result1 = runtime.Run(eff).UnsafeSuccess()
                let result2 = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result1 1 "First run should return 1"
                Expect.equal result2 2 "Second run should return 2")

            testPropertyWithConfig fsCheckConfig "TryWith - catches error and recovers"
            <| fun (runtime: FIORuntime, errorValue: int, recoveryValue: int) ->
                let eff =
                    fio {
                        try
                            return! FIO.fail errorValue
                        with err ->
                            return err + recoveryValue
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (errorValue + recoveryValue) "try...with should catch error and recover"

            testPropertyWithConfig fsCheckConfig "TryWith - success path not affected"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff =
                    fio {
                        try
                            return value
                        with _ ->
                            return -1
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "try...with should not affect success path"

            testAllRuntimes "TryWith - pattern matching on error" (fun runtime ->
                let eff =
                    fio {
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
                let eff =
                    fio {
                        try
                            try
                                return! FIO.fail "inner"
                            with
                            | "inner" -> return "caught inner"
                            | _ -> return "other inner"
                        with _ ->
                            return "outer"
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result "caught inner" "Nested try...with should catch inner error")

            testAllRuntimes "TryWith - handler that also fails propagates handler error" (fun runtime ->
                let eff =
                    fio {
                        try
                            return! FIO.fail "body-error"
                        with _ ->
                            return! FIO.fail "handler-error"
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "handler-error" "Handler error should propagate when handler fails")

            testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable finalizerRan = false

                let eff =
                    fio {
                        try
                            return value
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.isTrue finalizerRan "try...finally finalizer should run on success"

            testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs on failure"
            <| fun (runtime: FIORuntime, errorValue: int) ->
                let mutable finalizerRan = false

                let eff =
                    fio {
                        try
                            return! FIO.fail errorValue
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result errorValue "try...finally should propagate error"
                Expect.isTrue finalizerRan "try...finally finalizer should run on failure"

            testPropertyWithConfig fsCheckConfig "TryFinally - finalizer is deferred until run"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable finalizerRan = false

                let eff =
                    fio {
                        try
                            return value
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                Expect.isFalse finalizerRan "finalizer should not run before Run"
                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.isTrue finalizerRan "finalizer should run after Run"

            testPropertyWithConfig fsCheckConfig "TryFinally - finalizer runs after multiple statements"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable order = []

                let eff =
                    fio {
                        try
                            do!
                                FIO.suspend (fun () ->
                                    order <- order @ [ "body1" ]
                                    FIO.unit ())

                            do!
                                FIO.suspend (fun () ->
                                    order <- order @ [ "body2" ]
                                    FIO.unit ())

                            return value
                        finally
                            order <- order @ [ "finally" ]
                            FIO.unit ()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.equal order [ "body1"; "body2"; "finally" ] "Finalizer should run after body completes"

            testPropertyWithConfig fsCheckConfig "TryFinally - nested try...finally runs finalizers in correct order"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable order = []

                let eff =
                    fio {
                        try
                            try
                                return value
                            finally
                                order <- order @ [ "inner" ]
                                FIO.unit ()
                        finally
                            order <- order @ [ "outer" ]
                            FIO.unit ()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Nested try...finally should return value"
                Expect.equal order [ "inner"; "outer" ] "Inner finalizer should run before outer"

            testAllRuntimes "TryFinally - finalizer failure propagates when body succeeds" (fun runtime ->
                let eff =
                    fio {
                        try
                            return 42
                        finally
                            FIO.fail("finalizer error").Map ignore
                    }

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Failed err -> Expect.equal err "finalizer error" "Finalizer error should propagate"
                | _ -> failtest $"Expected Failed, got {result}")

            testAllRuntimes "TryFinally - body error preserved when finalizer also fails" (fun runtime ->
                let eff =
                    fio {
                        try
                            return! FIO.fail "body error"
                        finally
                            FIO.fail("finalizer error").Map ignore
                    }

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Failed err -> Expect.equal err "body error" "Body error should be preserved over finalizer error"
                | _ -> failtest $"Expected Failed, got {result}")

            testAllRuntimes "TryFinally inside TryWith - finalizer runs then error caught" (fun runtime ->
                let mutable finalizerRan = false

                let eff =
                    fio {
                        try
                            try
                                return! FIO.fail "body-error"
                            finally
                                finalizerRan <- true
                                FIO.unit ()
                        with err ->
                            return $"caught: {err}"
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result "caught: body-error" "Error should be caught by outer try...with"
                Expect.isTrue finalizerRan "Finalizer should run before error is caught")

            testAllRuntimes "TryWith inside TryFinally - error caught and finalizer still runs" (fun runtime ->
                let mutable finalizerRan = false

                let eff =
                    fio {
                        try
                            try
                                return! FIO.fail "inner-error"
                            with err ->
                                return $"recovered: {err}"
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result "recovered: inner-error" "Inner try...with should catch error"
                Expect.isTrue finalizerRan "Outer finalizer should still run")

            testAllRuntimes "For - iterates over sequence" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        for i in [ 1; 2; 3; 4; 5 ] do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + i
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 15 "for...do should iterate and accumulate sum")

            testAllRuntimes "For - empty sequence returns unit" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        for _ in [] do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 0 "for...do with empty sequence should not execute body")

            testPropertyWithConfig fsCheckConfig "For - error in body propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable iterations = 0

                let eff =
                    fio {
                        for i in [ 1; 2; 3; 4; 5 ] do
                            iterations <- iterations + 1

                            if i = 3 then
                                do! FIO.fail err
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "for...do should propagate error from body"
                Expect.equal iterations 3 "Loop should have run 3 times before error"

            testAllRuntimes "For - over lazy sequence disposes enumerator" (fun runtime ->
                let mutable enumeratorDisposed = false

                let lazySeq =
                    seq {
                        try
                            yield 1
                            yield 2
                            yield 3
                        finally
                            enumeratorDisposed <- true
                    }

                let mutable sum = 0

                let eff =
                    fio {
                        for i in lazySeq do
                            do!
                                FIO.suspend (fun () ->
                                    sum <- sum + i
                                    FIO.unit ())
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "for...do should complete"
                Expect.equal sum 6 "for...do should iterate all elements"
                Expect.isTrue enumeratorDisposed "Enumerator should be disposed after iteration")

            testAllRuntimes "For - try/with inside for loop" (fun runtime ->
                let mutable recovered = 0

                let eff =
                    fio {
                        for i in [ 1; 2; 3 ] do
                            try
                                if i = 2 then
                                    do! FIO.fail "skip"
                            with _ ->
                                recovered <- recovered + 1
                    }

                runtime.Run(eff).UnsafeSuccess()
                Expect.equal recovered 1 "Should recover from error inside for loop")

            testAllRuntimes "For - error in body disposes enumerator" (fun runtime ->
                let mutable enumeratorDisposed = false

                let lazySeq =
                    seq {
                        try
                            yield 1
                            yield 2
                            yield 3
                        finally
                            enumeratorDisposed <- true
                    }

                let eff =
                    fio {
                        for i in lazySeq do
                            if i = 2 then
                                do! FIO.fail "error-at-2"
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "error-at-2" "Error should propagate"
                Expect.isTrue enumeratorDisposed "Enumerator should be disposed even when body fails")

            testAllRuntimes "For - laziness: sequence not enumerated until Run" (fun runtime ->
                let mutable enumerated = false

                let lazySeq =
                    seq {
                        enumerated <- true
                        yield 1
                    }

                let eff =
                    fio {
                        for _ in lazySeq do
                            do! FIO.unit ()
                    }

                Expect.isFalse enumerated "Sequence should not be enumerated before Run"
                runtime.Run(eff).UnsafeSuccess() |> ignore

                Expect.isTrue enumerated "Sequence should be enumerated during Run")

            testAllRuntimes "For - stack safety with large iteration count" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        for _ in seq { 1..10000 } do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 10000 "For should handle 10000 iterations without stack overflow")

            testAllRuntimes "While - guard is evaluated at runtime" (fun runtime ->
                let mutable started = false
                let mutable counter = 0

                let eff =
                    fio {
                        while started && counter < 3 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())
                    }

                started <- true
                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "while...do should return unit"
                Expect.equal counter 3 "guard should be evaluated at runtime")

            testAllRuntimes "While - loops while condition true" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        while counter < 5 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 5 "while...do should loop until condition is false")

            testAllRuntimes "While - false condition never executes body" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        while false do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 0 "while...do with false condition should not execute body")

            testPropertyWithConfig fsCheckConfig "While - error in body propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable iterations = 0

                let eff =
                    fio {
                        while true do
                            iterations <- iterations + 1

                            if iterations = 3 then
                                do! FIO.fail err
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "while...do should propagate error from body"
                Expect.equal iterations 3 "Loop should have run 3 times before error"

            testAllRuntimes "While - stack safety with large iteration count" (fun runtime ->
                let mutable counter = 0

                let eff =
                    fio {
                        while counter < 10000 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 10000 "While should handle 10000 iterations without stack overflow")

            testAllRuntimes "While - laziness: guard not evaluated until Run" (fun runtime ->
                let mutable guardEvaluated = false

                let eff =
                    fio {
                        while (guardEvaluated <- true
                               false) do
                            do! FIO.unit ()
                    }

                Expect.isFalse guardEvaluated "Guard should not be evaluated before Run"
                runtime.Run(eff).UnsafeSuccess() |> ignore

                Expect.isTrue guardEvaluated "Guard should be evaluated during Run")

            testAllRuntimes "While - error recovery in loop body with try...with" (fun runtime ->
                let mutable recovered = 0
                let mutable iterations = 0

                let eff =
                    fio {
                        while iterations < 5 do
                            iterations <- iterations + 1

                            try
                                if iterations % 2 = 0 then
                                    do! FIO.fail "even"
                            with _ ->
                                recovered <- recovered + 1

                        return iterations, recovered
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (5, 2) "While should support error recovery in body")

            testPropertyWithConfig fsCheckConfig "Using - use disposes resource after use"
            <| fun (runtime: FIORuntime, value: int) ->
                let resource = new TestDisposable()

                let eff =
                    fio {
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

                let eff =
                    fio {
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

                let eff =
                    fio {
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

                let eff =
                    fio {
                        use! _ =
                            FIO
                                .fail(err)
                                .Map(fun _ ->
                                    { new IDisposable with
                                        member _.Dispose() = disposeCalled <- true
                                    })

                        return 42
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "use! should propagate acquisition error"
                Expect.isFalse disposeCalled "Dispose should not be called when acquisition fails"

            testPropertyWithConfig fsCheckConfig "Using - cleanup happens before try...with catches error"
            <| fun (runtime: FIORuntime, err: int) ->
                let resource = new TestDisposable()

                let eff =
                    fio {
                        try
                            use _ = resource
                            return! FIO.fail err
                        with e ->
                            return e
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result err "try...with should catch the error"
                Expect.isTrue resource.IsDisposed "Resource should be disposed before error is caught"

            testPropertyWithConfig fsCheckConfig "Using - nested use blocks dispose in reverse order"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable disposeOrder = []

                let makeResource name =
                    { new IDisposable with
                        member _.Dispose() = disposeOrder <- disposeOrder @ [ name ]
                    }

                let eff =
                    fio {
                        use _ = makeResource "first"
                        use _ = makeResource "second"
                        use _ = makeResource "third"
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Nested use should return value"
                Expect.equal disposeOrder [ "third"; "second"; "first" ] "Resources should dispose in reverse order"

            testAllRuntimes "Using - use with null disposable does not throw" (fun runtime ->
                let mutable ran = false

                let eff =
                    fio {
                        use _ = null: IDisposable
                        ran <- true
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "use should return unit"
                Expect.isTrue ran "Body should execute even with null disposable")

            testPropertyWithConfig fsCheckConfig "Match - match! pattern matches on effect result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff =
                    fio {
                        match! FIO.succeed value with
                        | v when v > 0 -> return "positive"
                        | v when v < 0 -> return "negative"
                        | _ -> return "zero"
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                let expected =
                    if value > 0 then "positive"
                    elif value < 0 then "negative"
                    else "zero"

                Expect.equal result expected "match! should pattern match on effect result"

            testPropertyWithConfig fsCheckConfig "Match - match! on failing effect short-circuits"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable reached = false

                let eff =
                    fio {
                        match! FIO.fail err with
                        | _ ->
                            reached <- true
                            return "should not reach"
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "match! on failing effect should propagate error"
                Expect.isFalse reached "Pattern match arms should not execute on failure"

            testAllRuntimes "Match - match! with DU pattern matching" (fun runtime ->
                let eff =
                    fio {
                        match! FIO.succeed (Some 42) with
                        | Some v -> return v
                        | None -> return -1
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 42 "match! should support DU pattern matching")

            testPropertyWithConfig fsCheckConfig "MergeSources - let! ... and! zips two effects"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.succeed b
                        return x + y
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (a + b) "let! ... and! should zip two effects"

            testPropertyWithConfig fsCheckConfig "MergeSources - and! propagates error from second effect"
            <| fun (runtime: FIORuntime, a: int, err: int) ->
                let eff =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.fail err
                        return x + y
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "and! should propagate error from any effect"

            testPropertyWithConfig fsCheckConfig "MergeSources - and! propagates error from first effect"
            <| fun (runtime: FIORuntime, err: int, b: int) ->
                let eff =
                    fio {
                        let! x = FIO.fail err
                        and! y = FIO.succeed b
                        return x + y
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "and! should propagate error from first effect"

            testPropertyWithConfig fsCheckConfig "MergeSources - and! with multiple errors returns first error"
            <| fun (runtime: FIORuntime, err1: int, err2: int) ->
                let eff =
                    fio {
                        let! x = FIO.fail err1
                        and! y = FIO.fail err2
                        return x + y
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err1 "and! should return first error when multiple effects fail"

            testAllRuntimes "MergeSources - and! executes in parallel" (fun runtime ->
                let mutable firstRan = false
                let mutable secondRan = false

                let eff =
                    fio {
                        let! a =
                            FIO.suspend (fun () ->
                                firstRan <- true
                                FIO.succeed 1)

                        and! b =
                            FIO.suspend (fun () ->
                                secondRan <- true
                                FIO.succeed 2)

                        return a, b
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (1, 2) "Should return both values"
                Expect.isTrue firstRan "First effect should have run"
                Expect.isTrue secondRan "Second effect should have run")

            testAllRuntimes "MergeSources3 - and! x3 executes in parallel" (fun runtime ->
                let mutable firstRan = false
                let mutable secondRan = false
                let mutable thirdRan = false

                let eff =
                    fio {
                        let! a =
                            FIO.suspend (fun () ->
                                firstRan <- true
                                FIO.succeed 1)

                        and! b =
                            FIO.suspend (fun () ->
                                secondRan <- true
                                FIO.succeed 2)

                        and! c =
                            FIO.suspend (fun () ->
                                thirdRan <- true
                                FIO.succeed 3)

                        return a, b, c
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (1, 2, 3) "Should return all three values"
                Expect.isTrue firstRan "First effect should have run"
                Expect.isTrue secondRan "Second effect should have run"
                Expect.isTrue thirdRan "Third effect should have run")

            testPropertyWithConfig fsCheckConfig "MergeSources3 - let! ... and! ... and! zips three effects"
            <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
                let eff =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.succeed b
                        and! z = FIO.succeed c
                        return x + y + z
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (a + b + c) "let! ... and! ... and! should zip three effects"

            testPropertyWithConfig fsCheckConfig "MergeSources3 - error from first propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let second =
                    FIO.suspend (fun () ->
                        FIO.succeed 1)

                let third =
                    FIO.suspend (fun () ->
                        FIO.succeed 2)

                let eff = fio.MergeSources3(FIO.fail err, second, third)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "MergeSources3 should propagate the first error"

            testPropertyWithConfig fsCheckConfig "MergeSources4 - let! ... and! x4 zips four effects"
            <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int) ->
                let eff =
                    fio {
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
                let eff =
                    fio {
                        let! v = FIO.succeed a
                        and! w = FIO.succeed b
                        and! x = FIO.succeed c
                        and! y = FIO.succeed d
                        and! z = FIO.succeed e
                        return v + w + x + y + z
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (a + b + c + d + e) "MergeSources5 should zip five effects"

            testPropertyWithConfig fsCheckConfig "MergeSources3 - error from second effect propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.fail err

                        and! _c =
                            FIO.suspend (fun () ->
                                FIO.succeed 3)

                        return ()
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Error from second effect should propagate"

            testPropertyWithConfig fsCheckConfig "MergeSources3 - error from third effect propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.fail err
                        return ()
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Error from third effect should propagate"

            testPropertyWithConfig fsCheckConfig "MergeSources4 - error from third effect propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.fail err
                        and! _d = FIO.succeed 4
                        return ()
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Error from third effect in MergeSources4 should propagate"

            testPropertyWithConfig fsCheckConfig "MergeSources5 - error from fourth effect propagates"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.succeed 3
                        and! _d = FIO.fail err
                        and! _e = FIO.succeed 5
                        return ()
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result err "Error from fourth effect in MergeSources5 should propagate"

            testAllRuntimes "Complex - control flow with let!/if/for" (fun runtime ->
                let eff =
                    fio {
                        let! start = FIO.succeed 0
                        let mutable sum = start

                        for i in [ 1; 2; 3; 4; 5 ] do
                            let! inc = FIO.succeed i

                            if inc % 2 = 0 then
                                sum <- sum + inc

                        return sum
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 6 "Complex control flow should work correctly (sum of evens: 2+4=6)")

            testPropertyWithConfig fsCheckConfig "Complex - nested fio blocks"
            <| fun (runtime: FIORuntime, value: int) ->
                let inner =
                    fio {
                        let! x = FIO.succeed value
                        return x * 2
                    }

                let outer =
                    fio {
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

            testAllRuntimes "Complex - use with for loop keeps resource alive during iteration" (fun runtime ->
                let mutable usedWhileAlive = 0
                let mutable disposedAt = -1

                let resource =
                    { new IDisposable with
                        member _.Dispose() = disposedAt <- usedWhileAlive
                    }

                let eff =
                    fio {
                        use _ = resource

                        for i in [ 1; 2; 3 ] do
                            do!
                                FIO.suspend (fun () ->
                                    usedWhileAlive <- usedWhileAlive + i
                                    FIO.unit ())

                        return usedWhileAlive
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 6 "Should accumulate sum during loop"
                Expect.equal disposedAt 6 "Resource should be disposed after loop completes")

            testPropertyWithConfig fsCheckConfig "Complex - inner CE failure propagates to outer CE"
            <| fun (runtime: FIORuntime, err: int) ->
                let inner = fio { return! FIO.fail err }

                let outer =
                    fio {
                        let! x = inner
                        return x + 1
                    }

                let result = runtime.Run(outer).UnsafeError()

                Expect.equal result err "Inner CE failure should propagate to outer CE"

            testPropertyWithConfig fsCheckConfig "Complex - inner CE failure caught by outer try...with"
            <| fun (runtime: FIORuntime, err: int) ->
                let inner = fio { return! FIO.fail err }

                let outer =
                    fio {
                        try
                            let! x = inner
                            return x + 1
                        with e ->
                            return e * 10
                    }

                let result = runtime.Run(outer).UnsafeSuccess()

                Expect.equal result (err * 10) "Outer try...with should catch inner CE failure"

            testAllRuntimes "Concurrency - fork and join within CE" (fun runtime ->
                let eff =
                    fio {
                        let! fiber = FIO.succeed(42).Fork()
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 42 "Fork/join within CE should work")

            testAllRuntimes "Concurrency - channel send and receive within CE" (fun runtime ->
                let eff =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Send(99).Unit()
                        let! msg = chan.Receive()
                        return msg
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 99 "Channel send/receive within CE should work")
        ]
