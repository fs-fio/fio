module FIO.Tests.CETests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.WorkStealing

open Expecto

open System

type TestDisposable() =
    let mutable disposed = false

    member _.IsDisposed = disposed

    interface IDisposable with
        member _.Dispose() = disposed <- true

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime() :> FIORuntime
        new WorkStealingRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let ceTests =
    testList
        "Computation Expression"
        [
            // ─── Bind ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "Bind - let! binds effectect result to variable"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    fio {
                        let! x = FIO.succeed value
                        return x + 1
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (value + 1) "let! should bind effectect result"

            testPropertyWithConfig fsCheckConfigFast "Bind - do! followed by return"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        return value
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "do! followed by return should work"
                Expect.isTrue executed "do! should execute side effectect"

            testAllRuntimes "Bind - multiple do! in sequence" (fun runtime ->
                let mutable order = []

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "Multiple do! should execute in sequence")

            testAllRuntimes "Bind - let! chain short-circuits on error" (fun runtime ->
                let mutable thirdRan = false

                let effect =
                    fio {
                        let! a = FIO.succeed 1
                        let! _b = FIO.fail "boom"

                        do!
                            FIO.suspend (fun () ->
                                thirdRan <- true
                                FIO.unit ())

                        return a
                    }

                let result = runtime.Run(effect).UnsafeResult()

                match result with
                | Failed error -> Expect.equal error "boom" "Should propagate error"
                | _ -> failtest $"Expected Failed, got {result}"

                Expect.isFalse thirdRan "effectects after failure should not run")

            // ─── BindReturn ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "BindReturn - let!/return maps result"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = fio.BindReturn(FIO.succeed value, fun x -> x + 1)

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (value + 1) "BindReturn should map the bound result"

            testPropertyWithConfig fsCheckConfigFast "BindReturn - mapper not invoked on failure"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable mapped = false

                let effect =
                    fio.BindReturn(
                        FIO.fail error,
                        fun _ ->
                            mapped <- true
                            1
                    )

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "BindReturn should propagate the error"
                Expect.isFalse mapped "BindReturn mapper should not run on failure"

            // ─── Return / ReturnFrom / Yield ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "Return - return wraps value in effectect"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = fio { return value }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "return should wrap value in effect"

            testPropertyWithConfig fsCheckConfigFast "ReturnFrom - return! returns existing effect"
            <| fun (runtime: FIORuntime, value: int) ->
                let innerEff = FIO.succeed value
                let effect = fio { return! innerEff }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "return! should return existing effect"

            testPropertyWithConfig fsCheckConfigFast "Yield - yield yields value same as return"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = fio { yield value }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "yield should yield value same as return"

            testPropertyWithConfig fsCheckConfigFast "YieldFrom - yield! yields effect same as return!"
            <| fun (runtime: FIORuntime, value: int) ->
                let innerEff = FIO.succeed value
                let effect = fio { yield! innerEff }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "yield! should yield effect same as return!"

            testPropertyWithConfig fsCheckConfigFast "ReturnFrom - return! after do! exercises ReturnFrom via Combine"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        return! FIO.succeed value
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "return! after do! should work"
                Expect.isTrue executed "do! before return! should execute"

            testPropertyWithConfig fsCheckConfigFast "YieldFrom - yield! after do! exercises YieldFrom via Combine"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    fio {
                        do!
                            FIO.suspend (fun () ->
                                executed <- true
                                FIO.unit ())

                        yield! FIO.succeed value
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "yield! after do! should work"
                Expect.isTrue executed "do! before yield! should execute"

            // ─── Zero ─────────────────────────────────────────

            testAllRuntimes "Zero - empty fio block returns unit" (fun runtime ->
                let effect = fio { () }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "Empty fio block should return unit")

            testAllRuntimes "Zero - if without else uses Zero" (fun runtime ->
                let mutable executed = false

                let effect =
                    fio {
                        if true then
                            do!
                                FIO.suspend (fun () ->
                                    executed <- true
                                    FIO.unit ())
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "if without else should return unit"
                Expect.isTrue executed "if branch should execute")

            testAllRuntimes "Zero - if false without else returns unit" (fun runtime ->
                let mutable executed = false

                let effect =
                    fio {
                        if false then
                            do!
                                FIO.suspend (fun () ->
                                    executed <- true
                                    FIO.unit ())
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "if false without else should return unit"
                Expect.isFalse executed "if branch should not execute")

            // ─── Combine ─────────────────────────────────────────

            testAllRuntimes "Combine - sequences multiple statements correctly" (fun runtime ->
                let mutable order = []

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "Combine should return unit"
                Expect.equal order [ 1; 2; 3; 4 ] "Statements should execute in order")

            testPropertyWithConfig fsCheckConfigFast "Combine - stops when first effect fails"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable secondRan = false
                let first = FIO.fail error

                let second =
                    FIO.suspend (fun () ->
                        secondRan <- true
                        FIO.unit ())

                let effect = fio.Combine(first, second)

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Combine should propagate the first error"
                Expect.isFalse secondRan "Second effect should not run after failure"

            testPropertyWithConfig fsCheckConfigFast "Combine - propagates error from second effect"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable firstRan = false
                let mutable secondRan = false

                let first =
                    FIO.suspend (fun () ->
                        firstRan <- true
                        FIO.unit ())

                let second =
                    FIO.suspend (fun () ->
                        secondRan <- true
                        FIO.fail error)

                let effect = fio.Combine(first, second)

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Combine should propagate the second error"
                Expect.isTrue firstRan "First effect should run before failure"
                Expect.isTrue secondRan "Second effect should run and fail"

            // ─── Delay ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "Delay - body is deferred until Run"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable ran = false

                let effect =
                    fio {
                        ran <- true
                        return value
                    }

                Expect.isFalse ran "Delayed body should not run before Run"
                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "Delayed body should return expected value"
                Expect.isTrue ran "Delayed body should run during Run"

            testAllRuntimes "Delay - running same effect twice produces independent results" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        counter <- counter + 1
                        return counter
                    }

                let result1 = runtime.Run(effect).UnsafeSuccess()
                let result2 = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result1 1 "First run should return 1"
                Expect.equal result2 2 "Second run should return 2")

            // ─── TryWith ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "TryWith - catches error and recovers"
            <| fun (runtime: FIORuntime, errorValue: int, recoveryValue: int) ->
                let effect =
                    fio {
                        try
                            return! FIO.fail errorValue
                        with error ->
                            return error + recoveryValue
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (errorValue + recoveryValue) "try...with should catch error and recover"

            testPropertyWithConfig fsCheckConfigFast "TryWith - success path not affected"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    fio {
                        try
                            return value
                        with _ ->
                            return -1
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "try...with should not affect success path"

            testAllRuntimes "TryWith - pattern matching on error" (fun runtime ->
                let effect =
                    fio {
                        try
                            return! FIO.fail 42
                        with
                        | e when e > 40 -> return "large"
                        | e when e > 20 -> return "medium"
                        | _ -> return "small"
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result "large" "try...with should pattern match on error")

            testAllRuntimes "TryWith - nested try...with catches inner error" (fun runtime ->
                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result "caught inner" "Nested try...with should catch inner error")

            testAllRuntimes "TryWith - handler that also fails propagates handler error" (fun runtime ->
                let effect =
                    fio {
                        try
                            return! FIO.fail "body-error"
                        with _ ->
                            return! FIO.fail "handler-error"
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result "handler-error" "Handler error should propagate when handler fails")

            // ─── TryFinally ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "TryFinally - finalizer runs on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable finalizerRan = false

                let effect =
                    fio {
                        try
                            return value
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.isTrue finalizerRan "try...finally finalizer should run on success"

            testPropertyWithConfig fsCheckConfigFast "TryFinally - finalizer runs on failure"
            <| fun (runtime: FIORuntime, errorValue: int) ->
                let mutable finalizerRan = false

                let effect =
                    fio {
                        try
                            return! FIO.fail errorValue
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result errorValue "try...finally should propagate error"
                Expect.isTrue finalizerRan "try...finally finalizer should run on failure"

            testPropertyWithConfig fsCheckConfigFast "TryFinally - finalizer is deferred until run"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable finalizerRan = false

                let effect =
                    fio {
                        try
                            return value
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                Expect.isFalse finalizerRan "finalizer should not run before Run"
                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.isTrue finalizerRan "finalizer should run after Run"

            testPropertyWithConfig fsCheckConfigFast "TryFinally - finalizer runs after multiple statements"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable order = []

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "try...finally should return value"
                Expect.equal order [ "body1"; "body2"; "finally" ] "Finalizer should run after body completes"

            testPropertyWithConfig fsCheckConfigFast "TryFinally - nested try...finally runs finalizers in correct order"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable order = []

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "Nested try...finally should return value"
                Expect.equal order [ "inner"; "outer" ] "Inner finalizer should run before outer"

            testAllRuntimes "TryFinally - finalizer failure propagates when body succeeds" (fun runtime ->
                let effect =
                    fio {
                        try
                            return 42
                        finally
                            FIO.fail("finalizer error").Map ignore
                    }

                let result = runtime.Run(effect).UnsafeResult()

                match result with
                | Failed error -> Expect.equal error "finalizer error" "Finalizer error should propagate"
                | _ -> failtest $"Expected Failed, got {result}")

            testAllRuntimes "TryFinally - body error preserved when finalizer also fails" (fun runtime ->
                let effect =
                    fio {
                        try
                            return! FIO.fail "body error"
                        finally
                            FIO.fail("finalizer error").Map ignore
                    }

                let result = runtime.Run(effect).UnsafeResult()

                match result with
                | Failed error -> Expect.equal error "body error" "Body error should be preserved over finalizer error"
                | _ -> failtest $"Expected Failed, got {result}")

            testAllRuntimes "TryFinally inside TryWith - finalizer runs then error caught" (fun runtime ->
                let mutable finalizerRan = false

                let effect =
                    fio {
                        try
                            try
                                return! FIO.fail "body-error"
                            finally
                                finalizerRan <- true
                                FIO.unit ()
                        with error ->
                            return $"caught: {error}"
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result "caught: body-error" "Error should be caught by outer try...with"
                Expect.isTrue finalizerRan "Finalizer should run before error is caught")

            testAllRuntimes "TryWith inside TryFinally - error caught and finalizer still runs" (fun runtime ->
                let mutable finalizerRan = false

                let effect =
                    fio {
                        try
                            try
                                return! FIO.fail "inner-error"
                            with error ->
                                return $"recovered: {error}"
                        finally
                            finalizerRan <- true
                            FIO.unit ()
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result "recovered: inner-error" "Inner try...with should catch error"
                Expect.isTrue finalizerRan "Outer finalizer should still run")

            // ─── For ─────────────────────────────────────────

            testAllRuntimes "For - iterates over sequence" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        for i in [ 1; 2; 3; 4; 5 ] do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + i
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 15 "for...do should iterate and accumulate sum")

            testAllRuntimes "For - empty sequence returns unit" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        for _ in [] do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 0 "for...do with empty sequence should not execute body")

            testPropertyWithConfig fsCheckConfigFast "For - error in body propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable iterations = 0

                let effect =
                    fio {
                        for i in [ 1; 2; 3; 4; 5 ] do
                            iterations <- iterations + 1

                            if i = 3 then
                                do! FIO.fail error
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "for...do should propagate error from body"
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

                let effect =
                    fio {
                        for i in lazySeq do
                            do!
                                FIO.suspend (fun () ->
                                    sum <- sum + i
                                    FIO.unit ())
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "for...do should complete"
                Expect.equal sum 6 "for...do should iterate all elements"
                Expect.isTrue enumeratorDisposed "Enumerator should be disposed after iteration")

            testAllRuntimes "For - try/with inside for loop" (fun runtime ->
                let mutable recovered = 0

                let effect =
                    fio {
                        for i in [ 1; 2; 3 ] do
                            try
                                if i = 2 then
                                    do! FIO.fail "skip"
                            with _ ->
                                recovered <- recovered + 1
                    }

                runtime.Run(effect).UnsafeSuccess()
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

                let effect =
                    fio {
                        for i in lazySeq do
                            if i = 2 then
                                do! FIO.fail "error-at-2"
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result "error-at-2" "Error should propagate"
                Expect.isTrue enumeratorDisposed "Enumerator should be disposed even when body fails")

            testAllRuntimes "For - laziness: sequence not enumerated until Run" (fun runtime ->
                let mutable enumerated = false

                let lazySeq =
                    seq {
                        enumerated <- true
                        yield 1
                    }

                let effect =
                    fio {
                        for _ in lazySeq do
                            do! FIO.unit ()
                    }

                Expect.isFalse enumerated "Sequence should not be enumerated before Run"
                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue enumerated "Sequence should be enumerated during Run")

            testAllRuntimes "For - stack safety with large iteration count" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        for _ in seq { 1..10000 } do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 10000 "For should handle 10000 iterations without stack overflow")

            // ─── While ─────────────────────────────────────────

            testAllRuntimes "While - guard is evaluated at runtime" (fun runtime ->
                let mutable started = false
                let mutable counter = 0

                let effect =
                    fio {
                        while started && counter < 3 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())
                    }

                started <- true
                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "while...do should return unit"
                Expect.equal counter 3 "guard should be evaluated at runtime")

            testAllRuntimes "While - loops while condition true" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        while counter < 5 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 5 "while...do should loop until condition is false")

            testAllRuntimes "While - false condition never executes body" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        while false do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 0 "while...do with false condition should not execute body")

            testPropertyWithConfig fsCheckConfigFast "While - error in body propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable iterations = 0

                let effect =
                    fio {
                        while true do
                            iterations <- iterations + 1

                            if iterations = 3 then
                                do! FIO.fail error
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "while...do should propagate error from body"
                Expect.equal iterations 3 "Loop should have run 3 times before error"

            testAllRuntimes "While - stack safety with large iteration count" (fun runtime ->
                let mutable counter = 0

                let effect =
                    fio {
                        while counter < 10000 do
                            do!
                                FIO.suspend (fun () ->
                                    counter <- counter + 1
                                    FIO.unit ())

                        return counter
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 10000 "While should handle 10000 iterations without stack overflow")

            testAllRuntimes "While - laziness: guard not evaluated until Run" (fun runtime ->
                let mutable guardEvaluated = false

                let effect =
                    fio {
                        while (guardEvaluated <- true
                               false) do
                            do! FIO.unit ()
                    }

                Expect.isFalse guardEvaluated "Guard should not be evaluated before Run"
                runtime.Run(effect).UnsafeSuccess() |> ignore

                Expect.isTrue guardEvaluated "Guard should be evaluated during Run")

            testAllRuntimes "While - error recovery in loop body with try...with" (fun runtime ->
                let mutable recovered = 0
                let mutable iterations = 0

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (5, 2) "While should support error recovery in body")

            // ─── Using ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "Using - use disposes resource after use"
            <| fun (runtime: FIORuntime, value: int) ->
                let resource = new TestDisposable()

                let effect =
                    fio {
                        use _ = resource
                        return value
                    }

                Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "use should return value"
                Expect.isTrue resource.IsDisposed "use should dispose resource after use"

            testPropertyWithConfig fsCheckConfigFast "Using - use disposes resource even on failure"
            <| fun (runtime: FIORuntime, errorValue: int) ->
                let resource = new TestDisposable()

                let effect =
                    fio {
                        use _ = resource
                        return! FIO.fail errorValue
                    }

                Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result errorValue "use should propagate error"
                Expect.isTrue resource.IsDisposed "use should dispose resource even on failure"

            testPropertyWithConfig fsCheckConfigFast "Using - use! acquires and disposes effectful resource"
            <| fun (runtime: FIORuntime, value: int) ->
                let resource = new TestDisposable()

                let effect =
                    fio {
                        use! _ = FIO.succeed resource
                        return value
                    }

                Expect.isFalse resource.IsDisposed "Resource should not be disposed before running"
                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "use! should return value"
                Expect.isTrue resource.IsDisposed "use! should dispose effectfully acquired resource"

            testPropertyWithConfig fsCheckConfigFast "Using - use! failure during acquisition does not call dispose"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable disposeCalled = false

                let effect =
                    fio {
                        use! _ =
                            FIO
                                .fail(error)
                                .Map(fun _ ->
                                    { new IDisposable with
                                        member _.Dispose() = disposeCalled <- true
                                    })

                        return 42
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "use! should propagate acquisition error"
                Expect.isFalse disposeCalled "Dispose should not be called when acquisition fails"

            testPropertyWithConfig fsCheckConfigFast "Using - cleanup happens before try...with catches error"
            <| fun (runtime: FIORuntime, error: int) ->
                let resource = new TestDisposable()

                let effect =
                    fio {
                        try
                            use _ = resource
                            return! FIO.fail error
                        with e ->
                            return e
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result error "try...with should catch the error"
                Expect.isTrue resource.IsDisposed "Resource should be disposed before error is caught"

            testPropertyWithConfig fsCheckConfigFast "Using - nested use blocks dispose in reverse order"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable disposeOrder = []

                let makeResource name =
                    { new IDisposable with
                        member _.Dispose() = disposeOrder <- disposeOrder @ [ name ]
                    }

                let effect =
                    fio {
                        use _ = makeResource "first"
                        use _ = makeResource "second"
                        use _ = makeResource "third"
                        return value
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "Nested use should return value"
                Expect.equal disposeOrder [ "third"; "second"; "first" ] "Resources should dispose in reverse order"

            testAllRuntimes "Using - use with null disposable does not throw" (fun runtime ->
                let mutable ran = false

                let effect =
                    fio {
                        use _ = null: IDisposable
                        ran <- true
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "use should return unit"
                Expect.isTrue ran "Body should execute even with null disposable")

            // ─── Match ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "Match - match! pattern matches on effect result"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    fio {
                        match! FIO.succeed value with
                        | v when v > 0 -> return "positive"
                        | v when v < 0 -> return "negative"
                        | _ -> return "zero"
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                let expected =
                    if value > 0 then "positive"
                    elif value < 0 then "negative"
                    else "zero"

                Expect.equal result expected "match! should pattern match on effect result"

            testPropertyWithConfig fsCheckConfigFast "Match - match! on failing effect short-circuits"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable reached = false

                let effect =
                    fio {
                        match! FIO.fail error with
                        | _ ->
                            reached <- true
                            return "should not reach"
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "match! on failing effect should propagate error"
                Expect.isFalse reached "Pattern match arms should not execute on failure"

            testAllRuntimes "Match - match! with DU pattern matching" (fun runtime ->
                let effect =
                    fio {
                        match! FIO.succeed (Some 42) with
                        | Some v -> return v
                        | None -> return -1
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 42 "match! should support DU pattern matching")

            // ─── MergeSources ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "MergeSources - let! ... and! zips two effects"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let effect =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.succeed b
                        return x + y
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (a + b) "let! ... and! should zip two effects"

            testPropertyWithConfig fsCheckConfigFast "MergeSources - and! propagates error from second effect"
            <| fun (runtime: FIORuntime, a: int, error: int) ->
                let effect =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.fail error
                        return x + y
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "and! should propagate error from any effect"

            testPropertyWithConfig fsCheckConfigFast "MergeSources - and! propagates error from first effect"
            <| fun (runtime: FIORuntime, error: int, b: int) ->
                let effect =
                    fio {
                        let! x = FIO.fail error
                        and! y = FIO.succeed b
                        return x + y
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "and! should propagate error from first effect"

            testPropertyWithConfig fsCheckConfigFast "MergeSources - and! with multiple errors returns first error"
            <| fun (runtime: FIORuntime, err1: int, err2: int) ->
                let effect =
                    fio {
                        let! x = FIO.fail err1
                        and! y = FIO.fail err2
                        return x + y
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result err1 "and! should return first error when multiple effects fail"

            testAllRuntimes "MergeSources - and! executes in parallel" (fun runtime ->
                let mutable firstRan = false
                let mutable secondRan = false

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (1, 2) "Should return both values"
                Expect.isTrue firstRan "First effect should have run"
                Expect.isTrue secondRan "Second effect should have run")

            testAllRuntimes "and! x3 - executes in parallel" (fun runtime ->
                let mutable firstRan = false
                let mutable secondRan = false
                let mutable thirdRan = false

                let effect =
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

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (1, 2, 3) "Should return all three values"
                Expect.isTrue firstRan "First effect should have run"
                Expect.isTrue secondRan "Second effect should have run"
                Expect.isTrue thirdRan "Third effect should have run")

            testPropertyWithConfig fsCheckConfigFast "and! x3 - zips three effects"
            <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
                let effect =
                    fio {
                        let! x = FIO.succeed a
                        and! y = FIO.succeed b
                        and! z = FIO.succeed c
                        return x + y + z
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (a + b + c) "let! ... and! ... and! should zip three effects"

            testPropertyWithConfig fsCheckConfigFast "and! x3 - error from first propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    fio {
                        let! _a = FIO.fail error
                        and! _b = FIO.suspend (fun () -> FIO.succeed 1)
                        and! _c = FIO.suspend (fun () -> FIO.succeed 2)
                        return ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "and! x3 should propagate the first error"

            testPropertyWithConfig fsCheckConfigFast "and! x4 - zips four effects"
            <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int) ->
                let effect =
                    fio {
                        let! w = FIO.succeed a
                        and! x = FIO.succeed b
                        and! y = FIO.succeed c
                        and! z = FIO.succeed d
                        return w + x + y + z
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (a + b + c + d) "and! x4 should zip four effects"

            testPropertyWithConfig fsCheckConfigFast "and! x5 - zips five effects"
            <| fun (runtime: FIORuntime, a: int, b: int, c: int, d: int, e: int) ->
                let effect =
                    fio {
                        let! v = FIO.succeed a
                        and! w = FIO.succeed b
                        and! x = FIO.succeed c
                        and! y = FIO.succeed d
                        and! z = FIO.succeed e
                        return v + w + x + y + z
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (a + b + c + d + e) "and! x5 should zip five effects"

            testPropertyWithConfig fsCheckConfigFast "and! x3 - error from second effect propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.fail error

                        and! _c = FIO.suspend (fun () -> FIO.succeed 3)

                        return ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Error from second effect should propagate"

            testPropertyWithConfig fsCheckConfigFast "and! x3 - error from third effect propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.fail error
                        return ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Error from third effect should propagate"

            testPropertyWithConfig fsCheckConfigFast "and! x4 - error from third effect propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.fail error
                        and! _d = FIO.succeed 4
                        return ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Error from third effect in and! x4 should propagate"

            testPropertyWithConfig fsCheckConfigFast "and! x5 - error from fourth effect propagates"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    fio {
                        let! _a = FIO.succeed 1
                        and! _b = FIO.succeed 2
                        and! _c = FIO.succeed 3
                        and! _d = FIO.fail error
                        and! _e = FIO.succeed 5
                        return ()
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result error "Error from fourth effect in and! x5 should propagate"

            // ─── Complex ─────────────────────────────────────────

            testAllRuntimes "Complex - control flow with let!/if/for" (fun runtime ->
                let effect =
                    fio {
                        let! start = FIO.succeed 0
                        let mutable sum = start

                        for i in [ 1; 2; 3; 4; 5 ] do
                            let! inc = FIO.succeed i

                            if inc % 2 = 0 then
                                sum <- sum + inc

                        return sum
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 6 "Complex control flow should work correctly (sum of evens: 2+4=6)")

            testPropertyWithConfig fsCheckConfigFast "Complex - nested fio blocks"
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

                let effect = nested 1000

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1000 "Deep nesting should not overflow stack")

            testAllRuntimes "Complex - use with for loop keeps resource alive during iteration" (fun runtime ->
                let mutable usedWhileAlive = 0
                let mutable disposedAt = -1

                let resource =
                    { new IDisposable with
                        member _.Dispose() = disposedAt <- usedWhileAlive
                    }

                let effect =
                    fio {
                        use _ = resource

                        for i in [ 1; 2; 3 ] do
                            do!
                                FIO.suspend (fun () ->
                                    usedWhileAlive <- usedWhileAlive + i
                                    FIO.unit ())

                        return usedWhileAlive
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 6 "Should accumulate sum during loop"
                Expect.equal disposedAt 6 "Resource should be disposed after loop completes")

            testPropertyWithConfig fsCheckConfigFast "Complex - inner CE failure propagates to outer CE"
            <| fun (runtime: FIORuntime, error: int) ->
                let inner = fio { return! FIO.fail error }

                let outer =
                    fio {
                        let! x = inner
                        return x + 1
                    }

                let result = runtime.Run(outer).UnsafeError()

                Expect.equal result error "Inner CE failure should propagate to outer CE"

            testPropertyWithConfig fsCheckConfigFast "Complex - inner CE failure caught by outer try...with"
            <| fun (runtime: FIORuntime, error: int) ->
                let inner = fio { return! FIO.fail error }

                let outer =
                    fio {
                        try
                            let! x = inner
                            return x + 1
                        with e ->
                            return e * 10
                    }

                let result = runtime.Run(outer).UnsafeSuccess()

                Expect.equal result (error * 10) "Outer try...with should catch inner CE failure"

            // ─── Concurrency ─────────────────────────────────────────

            testAllRuntimes "Concurrency - fork and join within CE" (fun runtime ->
                let effect =
                    fio {
                        let! fiber = FIO.succeed(42).Fork()
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 42 "Fork/join within CE should work")

            testAllRuntimes "Concurrency - channel send and receive within CE" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(99).Unit()
                        let! msg = chan.Read()
                        return msg
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 99 "Channel send/receive within CE should work")
        ]
