module FIO.Tests.ExtensionTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

[<Tests>]
let extensionTests =
    testList "Extension Methods" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Unit discards result, returns unit"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let result = runtime.Run(eff.Unit()).UnsafeSuccess()
            Expect.equal result () "Unit should discard result and return unit"

        testPropertyWithConfig fsCheckPropertyTestsConfig "As maps to constant value"
        <| fun (runtime: FIORuntime, res: int, constant: string) ->
            let eff = FIO.succeed res
            let result = runtime.Run(eff.As constant).UnsafeSuccess()
            Expect.equal result constant "As should map to the constant value"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Result converts Success to Ok"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Result()).UnsafeSuccess()
            Expect.equal result (Ok res) "Result should convert Success to Ok"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Result converts Error to Error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Result()).UnsafeSuccess()
            Expect.equal result (Error err) "Result should convert Error to Error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Option converts Success to Some"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Option()).UnsafeSuccess()
            Expect.equal result (Some res) "Option should convert Success to Some"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Option converts Error to None"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Option()).UnsafeSuccess()
            Expect.equal result None "Option should convert Error to None"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Choice converts Success to Choice1Of2"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Choice()).UnsafeSuccess()
            Expect.equal result (Choice1Of2 res) "Choice should convert Success to Choice1Of2"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Choice converts Error to Choice2Of2"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Choice()).UnsafeSuccess()
            Expect.equal result (Choice2Of2 err) "Choice should convert Error to Choice2Of2"

        testPropertyWithConfig fsCheckPropertyTestsConfig "When (true) executes effect"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let _ = runtime.Run(eff.When true).UnsafeSuccess()
            Expect.isTrue executed "When(true) should execute the effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "When (false) returns unit without executing"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let result = runtime.Run(eff.When false).UnsafeSuccess()
            Expect.isFalse executed "When(false) should not execute the effect"
            Expect.equal result () "When(false) should return unit"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Unless is inverse of When"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let _ = runtime.Run(eff.Unless false).UnsafeSuccess()
            Expect.isTrue executed "Unless(false) should execute the effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "OrElse falls back on error"
        <| fun (runtime: FIORuntime, err: string, fallback: int) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()
            Expect.equal result fallback "OrElse should return fallback on error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "OrElse passes through on success"
        <| fun (runtime: FIORuntime, res: int, fallback: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()
            Expect.equal result res "OrElse should pass through on success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Zip combines two success values into tuple"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.Zip eff2).UnsafeSuccess()
            Expect.equal result (res1, res2) "Zip should combine two success values into tuple"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Zip first fails returns error (second not executed)"
        <| fun (runtime: FIORuntime, err: string) ->
            let mutable secondExecuted = false
            let eff1 : FIO<int, string> = FIO.fail err
            let eff2 : FIO<int, string> = FIO.attempt((fun () -> secondExecuted <- true; 42), fun ex -> ex.Message)
            let result = runtime.Run(eff1.Zip eff2).UnsafeError()
            Expect.equal result err "Zip should return error when first fails"
            Expect.isFalse secondExecuted "Zip should not execute second effect when first fails"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipError combines two errors into tuple"
        <| fun (runtime: FIORuntime, err1: int, err2: string) ->
            let eff1 : FIO<int, int> = FIO.fail err1
            let eff2 : FIO<int, string> = FIO.fail err2
            let result = runtime.Run(eff1.ZipError eff2).UnsafeError()
            Expect.equal result (err1, err2) "ZipError should combine two errors into tuple"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipRight ignores first result, returns second"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()
            Expect.equal result res2 "ZipRight should return second result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipLeft ignores second result, returns first"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()
            Expect.equal result res1 "ZipLeft should return first result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipPar parallel execution, both succeed"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()
            Expect.equal result (res1, res2) "ZipPar should combine results from parallel execution"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipParRight parallel, returns second"
        <| fun (runtime: FIORuntime, res1: int, res2: int) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<int, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()
            Expect.equal result res2 "ZipParRight should return second result from parallel execution"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipParLeft parallel, returns first"
        <| fun (runtime: FIORuntime, res1: int, res2: int) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<int, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()
            Expect.equal result res1 "ZipParLeft should return first result from parallel execution"

        testPropertyWithConfig fsCheckPropertyTestsConfig "RepeatN repeats N times, returns last result"
        <| fun (runtime: FIORuntime) ->
            let mutable count = 0
            let eff = FIO.attemptExn(fun () -> count <- count + 1; count)
            let result = runtime.Run(eff.RepeatN 5).UnsafeSuccess()
            Expect.equal count 5 "RepeatN should execute 5 times"
            Expect.equal result 5 "RepeatN should return last result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "RepeatN (n=1) executes exactly once"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable count = 0
            let eff = FIO.attemptExn(fun () -> count <- count + 1; res)
            let result = runtime.Run(eff.RepeatN 1).UnsafeSuccess()
            Expect.equal count 1 "RepeatN(1) should execute exactly once"
            Expect.equal result res "RepeatN(1) should return the result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchSome partial function matches, recovery"
        <| fun (runtime: FIORuntime, err: int, recovery: string) ->
            let eff : FIO<string, int> = FIO.fail err
            let pf = fun e -> if e = err then Some (FIO.succeed recovery) else None
            let result = runtime.Run(eff.CatchSome pf).UnsafeSuccess()
            Expect.equal result recovery "CatchSome should recover when partial function matches"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchSome partial function returns None, propagate error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<string, int> = FIO.fail err
            let pf = fun _ -> None
            let result = runtime.Run(eff.CatchSome pf).UnsafeError()
            Expect.equal result err "CatchSome should propagate error when partial function returns None"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Ensuring finalizer runs on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable finalizerRan = false
            let eff : FIO<int, string> = FIO.succeed res
            let finalizer : FIO<unit, string> = FIO.attempt((fun () -> finalizerRan <- true), fun ex -> ex.Message)
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeSuccess()
            Expect.isTrue finalizerRan "Ensuring finalizer should run on success"
            Expect.equal result res "Ensuring should preserve the success result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Ensuring finalizer runs on failure"
        <| fun (runtime: FIORuntime, err: string) ->
            let mutable finalizerRan = false
            let eff : FIO<int, string> = FIO.fail err
            let finalizer : FIO<unit, string> = FIO.attempt((fun () -> finalizerRan <- true), fun ex -> ex.Message)
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.isTrue finalizerRan "Ensuring finalizer should run on failure"
            Expect.equal result err "Ensuring should preserve the error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Ensuring finalizer error on success propagates"
        <| fun (runtime: FIORuntime, res: int, finalizerErr: string) ->
            let eff : FIO<int, string> = FIO.succeed res
            let finalizer : FIO<unit, string> = FIO.fail finalizerErr
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.equal result finalizerErr "Ensuring should propagate finalizer error when main effect succeeds"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Ensuring finalizer error on failure suppressed (main error preserved)"
        <| fun (runtime: FIORuntime, mainErr: string, finalizerErr: string) ->
            let eff : FIO<int, string> = FIO.fail mainErr
            let finalizer : FIO<unit, string> = FIO.fail finalizerErr
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.equal result mainErr "Ensuring should preserve main error, suppressing finalizer error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Timed returns duration and result (duration > 0)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, exn> = FIO.succeed res
            let duration, result = runtime.Run(eff.Timed id).UnsafeSuccess()
            Expect.equal result res "Timed should return the result"
            Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Timed duration should be >= 0"
    ]
