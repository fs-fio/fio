module FIO.Tests.ExtensionTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

[<Tests>]
let extensionTests =
    testList "Extension Methods" [

        testPropertyWithConfig fsCheckConfig "Unit discards result, returns unit"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let result = runtime.Run(eff.Unit()).UnsafeSuccess()
            Expect.equal result () "Unit should discard result and return unit"

        testPropertyWithConfig fsCheckConfig "As maps to constant value"
        <| fun (runtime: FIORuntime, res: int, constant: string) ->
            let eff = FIO.succeed res
            let result = runtime.Run(eff.As constant).UnsafeSuccess()
            Expect.equal result constant "As should map to the constant value"

        testPropertyWithConfig fsCheckConfig "Result converts Success to Ok"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Result()).UnsafeSuccess()
            Expect.equal result (Ok res) "Result should convert Success to Ok"

        testPropertyWithConfig fsCheckConfig "Result converts Error to Error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Result()).UnsafeSuccess()
            Expect.equal result (Error err) "Result should convert Error to Error"

        testPropertyWithConfig fsCheckConfig "Option converts Success to Some"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Option()).UnsafeSuccess()
            Expect.equal result (Some res) "Option should convert Success to Some"

        testPropertyWithConfig fsCheckConfig "Option converts Error to None"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Option()).UnsafeSuccess()
            Expect.equal result None "Option should convert Error to None"

        testPropertyWithConfig fsCheckConfig "Choice converts Success to Choice1Of2"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Choice()).UnsafeSuccess()
            Expect.equal result (Choice1Of2 res) "Choice should convert Success to Choice1Of2"

        testPropertyWithConfig fsCheckConfig "Choice converts Error to Choice2Of2"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Choice()).UnsafeSuccess()
            Expect.equal result (Choice2Of2 err) "Choice should convert Error to Choice2Of2"

        testPropertyWithConfig fsCheckConfig "When (true) executes effect"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let _ = runtime.Run(eff.When true).UnsafeSuccess()
            Expect.isTrue executed "When(true) should execute the effect"

        testPropertyWithConfig fsCheckConfig "When (false) returns unit without executing"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let result = runtime.Run(eff.When false).UnsafeSuccess()
            Expect.isFalse executed "When(false) should not execute the effect"
            Expect.equal result () "When(false) should return unit"

        testPropertyWithConfig fsCheckConfig "Unless is inverse of When"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.attemptExn(fun () -> executed <- true; res)
            let _ = runtime.Run(eff.Unless false).UnsafeSuccess()
            Expect.isTrue executed "Unless(false) should execute the effect"

        testPropertyWithConfig fsCheckConfig "OrElse falls back on error"
        <| fun (runtime: FIORuntime, err: string, fallback: int) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()
            Expect.equal result fallback "OrElse should return fallback on error"

        testPropertyWithConfig fsCheckConfig "OrElse passes through on success"
        <| fun (runtime: FIORuntime, res: int, fallback: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()
            Expect.equal result res "OrElse should pass through on success"

        testPropertyWithConfig fsCheckConfig "OrElse chains fallbacks"
        <| fun (runtime: FIORuntime, fallback: int) ->
            let eff =
                (FIO.fail "err1")
                    .OrElse(FIO.fail "err2")
                    .OrElse(FIO.succeed fallback)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result fallback "OrElse should chain fallbacks"

        testPropertyWithConfig fsCheckConfig "CatchAll recovers from error"
        <| fun (runtime: FIORuntime, err: int, recovery: int) ->
            let eff = (FIO.fail err).CatchAll(fun _ -> FIO.succeed recovery)
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual recovery "CatchAll should recover"

        testPropertyWithConfig fsCheckConfig "CatchAll does not affect success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).CatchAll(fun _ -> FIO.succeed 0)
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual res "CatchAll should not affect success"

        testPropertyWithConfig fsCheckConfig "CatchAll can transform error to new error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = (FIO.fail err).CatchAll(fun e -> FIO.fail(e + 100))
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual (err + 100) "CatchAll can transform to new error"

        testPropertyWithConfig fsCheckConfig "Zip combines two success values into tuple"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.Zip eff2).UnsafeSuccess()
            Expect.equal result (res1, res2) "Zip should combine two success values into tuple"

        testPropertyWithConfig fsCheckConfig "Zip first fails returns error (second not executed)"
        <| fun (runtime: FIORuntime, err: string) ->
            let mutable secondExecuted = false
            let eff1 : FIO<int, string> = FIO.fail err
            let eff2 : FIO<int, string> = FIO.attempt((fun () -> secondExecuted <- true; 42), fun ex -> ex.Message)
            let result = runtime.Run(eff1.Zip eff2).UnsafeError()
            Expect.equal result err "Zip should return error when first fails"
            Expect.isFalse secondExecuted "Zip should not execute second effect when first fails"

        testPropertyWithConfig fsCheckConfig "ZipError combines two errors into tuple"
        <| fun (runtime: FIORuntime, err1: int, err2: string) ->
            let eff1 : FIO<int, int> = FIO.fail err1
            let eff2 : FIO<int, string> = FIO.fail err2
            let result = runtime.Run(eff1.ZipError eff2).UnsafeError()
            Expect.equal result (err1, err2) "ZipError should combine two errors into tuple"

        testPropertyWithConfig fsCheckConfig "ZipRight ignores first result, returns second"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()
            Expect.equal result res2 "ZipRight should return second result"

        testPropertyWithConfig fsCheckConfig "ZipLeft ignores second result, returns first"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()
            Expect.equal result res1 "ZipLeft should return first result"

        testPropertyWithConfig fsCheckConfig "ZipPar parallel execution, both succeed"
        <| fun (runtime: FIORuntime, res1: int, res2: string) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<string, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()
            Expect.equal result (res1, res2) "ZipPar should combine results from parallel execution"

        testPropertyWithConfig fsCheckConfig "ZipParRight parallel, returns second"
        <| fun (runtime: FIORuntime, res1: int, res2: int) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<int, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()
            Expect.equal result res2 "ZipParRight should return second result from parallel execution"

        testPropertyWithConfig fsCheckConfig "ZipParLeft parallel, returns first"
        <| fun (runtime: FIORuntime, res1: int, res2: int) ->
            let eff1 : FIO<int, string> = FIO.succeed res1
            let eff2 : FIO<int, string> = FIO.succeed res2
            let result = runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()
            Expect.equal result res1 "ZipParLeft should return first result from parallel execution"

        testPropertyWithConfig fsCheckConfig "RepeatN repeats N times, returns last result"
        <| fun (runtime: FIORuntime) ->
            let mutable count = 0
            let eff = FIO.attemptExn(fun () -> count <- count + 1; count)
            let result = runtime.Run(eff.RepeatN 5).UnsafeSuccess()
            Expect.equal count 5 "RepeatN should execute 5 times"
            Expect.equal result 5 "RepeatN should return last result"

        testPropertyWithConfig fsCheckConfig "RepeatN (n=1) executes exactly once"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable count = 0
            let eff = FIO.attemptExn(fun () -> count <- count + 1; res)
            let result = runtime.Run(eff.RepeatN 1).UnsafeSuccess()
            Expect.equal count 1 "RepeatN(1) should execute exactly once"
            Expect.equal result res "RepeatN(1) should return the result"

        testPropertyWithConfig fsCheckConfig "CatchSome partial function matches, recovery"
        <| fun (runtime: FIORuntime, err: int, recovery: string) ->
            let eff : FIO<string, int> = FIO.fail err
            let pf = fun e -> if e = err then Some (FIO.succeed recovery) else None
            let result = runtime.Run(eff.CatchSome pf).UnsafeSuccess()
            Expect.equal result recovery "CatchSome should recover when partial function matches"

        testPropertyWithConfig fsCheckConfig "CatchSome partial function returns None, propagate error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<string, int> = FIO.fail err
            let pf = fun _ -> None
            let result = runtime.Run(eff.CatchSome pf).UnsafeError()
            Expect.equal result err "CatchSome should propagate error when partial function returns None"

        testPropertyWithConfig fsCheckConfig "Ensuring finalizer runs on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable finalizerRan = false
            let eff : FIO<int, string> = FIO.succeed res
            let finalizer : FIO<unit, string> = FIO.attempt((fun () -> finalizerRan <- true), fun ex -> ex.Message)
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeSuccess()
            Expect.isTrue finalizerRan "Ensuring finalizer should run on success"
            Expect.equal result res "Ensuring should preserve the success result"

        testPropertyWithConfig fsCheckConfig "Ensuring finalizer runs on failure"
        <| fun (runtime: FIORuntime, err: string) ->
            let mutable finalizerRan = false
            let eff : FIO<int, string> = FIO.fail err
            let finalizer : FIO<unit, string> = FIO.attempt((fun () -> finalizerRan <- true), fun ex -> ex.Message)
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.isTrue finalizerRan "Ensuring finalizer should run on failure"
            Expect.equal result err "Ensuring should preserve the error"

        testPropertyWithConfig fsCheckConfig "Ensuring finalizer error on success propagates"
        <| fun (runtime: FIORuntime, res: int, finalizerErr: string) ->
            let eff : FIO<int, string> = FIO.succeed res
            let finalizer : FIO<unit, string> = FIO.fail finalizerErr
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.equal result finalizerErr "Ensuring should propagate finalizer error when main effect succeeds"

        testPropertyWithConfig fsCheckConfig "Ensuring finalizer error on failure suppressed (main error preserved)"
        <| fun (runtime: FIORuntime, mainErr: string, finalizerErr: string) ->
            let eff : FIO<int, string> = FIO.fail mainErr
            let finalizer : FIO<unit, string> = FIO.fail finalizerErr
            let result = runtime.Run(eff.Ensuring finalizer).UnsafeError()
            Expect.equal result mainErr "Ensuring should preserve main error, suppressing finalizer error"

        testPropertyWithConfig fsCheckConfig "Timed returns duration and result (duration > 0)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, exn> = FIO.succeed res
            let duration, result = runtime.Run(eff.Timed id).UnsafeSuccess()
            Expect.equal result res "Timed should return the result"
            Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Timed duration should be >= 0"

        // Map tests
        testPropertyWithConfig fsCheckConfig "Map transforms success value"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, string> = FIO.succeed res
            let result = runtime.Run(eff.Map(fun x -> x * 2)).UnsafeSuccess()
            Expect.equal result (res * 2) "Map should transform success value"

        testPropertyWithConfig fsCheckConfig "Map preserves error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let result = runtime.Run(eff.Map(fun x -> x * 2)).UnsafeError()
            Expect.equal result err "Map should preserve error"

        // MapError tests
        testPropertyWithConfig fsCheckConfig "MapError transforms error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<string, int> = FIO.fail err
            let result = runtime.Run(eff.MapError(fun e -> e.ToString())).UnsafeError()
            Expect.equal result (err.ToString()) "MapError should transform error"

        testPropertyWithConfig fsCheckConfig "MapError preserves success"
        <| fun (runtime: FIORuntime, res: string) ->
            let eff : FIO<string, int> = FIO.succeed res
            let result = runtime.Run(eff.MapError(fun e -> e.ToString())).UnsafeSuccess()
            Expect.equal result res "MapError should preserve success"

        // MapBoth tests
        testPropertyWithConfig fsCheckConfig "MapBoth transforms success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, int> = FIO.succeed res
            let result = runtime.Run(eff.MapBoth((fun x -> x * 2), (fun e -> e + 100))).UnsafeSuccess()
            Expect.equal result (res * 2) "MapBoth should transform success"

        testPropertyWithConfig fsCheckConfig "MapBoth transforms error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<int, int> = FIO.fail err
            let result = runtime.Run(eff.MapBoth((fun x -> x * 2), (fun e -> e + 100))).UnsafeError()
            Expect.equal result (err + 100) "MapBoth should transform error"

        // Apply tests
        testPropertyWithConfig fsCheckConfig "Apply applies function to value"
        <| fun (runtime: FIORuntime, res: int) ->
            let valEff : FIO<int, string> = FIO.succeed res
            let fnEff : FIO<int -> int, string> = FIO.succeed (fun x -> x * 3)
            let result = runtime.Run(valEff.Apply fnEff).UnsafeSuccess()
            Expect.equal result (res * 3) "Apply should apply function to value"

        testPropertyWithConfig fsCheckConfig "Apply propagates value error"
        <| fun (runtime: FIORuntime, err: string) ->
            let valEff : FIO<int, string> = FIO.fail err
            let fnEff : FIO<int -> int, string> = FIO.succeed (fun x -> x * 3)
            let result = runtime.Run(valEff.Apply fnEff).UnsafeError()
            Expect.equal result err "Apply should propagate value error"

        testPropertyWithConfig fsCheckConfig "Apply propagates function error"
        <| fun (runtime: FIORuntime, res: int, err: string) ->
            let valEff : FIO<int, string> = FIO.succeed res
            let fnEff : FIO<int -> int, string> = FIO.fail err
            let result = runtime.Run(valEff.Apply fnEff).UnsafeError()
            Expect.equal result err "Apply should propagate function error"

        // ApplyError tests
        testPropertyWithConfig fsCheckConfig "ApplyError applies error function"
        <| fun (runtime: FIORuntime, err: int) ->
            let errEff : FIO<string, int> = FIO.fail err
            let fnEff : FIO<string, int -> string> = FIO.fail (fun e -> $"Error: {e}")
            let result = runtime.Run(errEff.ApplyError fnEff).UnsafeError()
            Expect.equal result $"Error: {err}" "ApplyError should apply error function"

        testPropertyWithConfig fsCheckConfig "ApplyError preserves success"
        <| fun (runtime: FIORuntime, res: string) ->
            let succEff : FIO<string, int> = FIO.succeed res
            let fnEff : FIO<string, int -> string> = FIO.fail (fun e -> $"Error: {e}")
            let result = runtime.Run(succEff.ApplyError fnEff).UnsafeSuccess()
            Expect.equal result res "ApplyError should preserve success"

        testPropertyWithConfig fsCheckConfig "Tap executes side effect preserving value"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable sideEffect = 0
            let eff = (FIO.succeed res).Tap(fun r -> FIO.succeed(sideEffect <- r * 2))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual res "Tap should preserve original value"
            Expect.equal sideEffect (res * 2) "Tap should execute side effect"

        testPropertyWithConfig fsCheckConfig "Tap propagates tap effect error"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let eff = (FIO.succeed res).Tap(fun _ -> FIO.fail err)
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual err "Tap should propagate tap error"

        testPropertyWithConfig fsCheckConfig "Tap does not execute on original error"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable executed = false
            let eff = (FIO.fail err).Tap(fun _ -> FIO.succeed(executed <- true))
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual err "Tap should preserve original error"
            Expect.isFalse executed "Tap should not execute on original error"

        testPropertyWithConfig fsCheckConfig "Tap discards tap result"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).Tap(fun _ -> FIO.succeed 999)
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual res "Tap should discard tap result"

        testPropertyWithConfig fsCheckConfig "TapError executes on error preserving error"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable sideEffect = 0
            let eff = (FIO.fail err).TapError(fun e -> FIO.succeed(sideEffect <- e * 2))
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual err "TapError should preserve error"
            Expect.equal sideEffect (err * 2) "TapError should execute side effect"

        testPropertyWithConfig fsCheckConfig "TapError propagates tap effect error"
        <| fun (runtime: FIORuntime, err: int, newErr: int) ->
            let eff = (FIO.fail err).TapError(fun _ -> FIO.fail newErr)
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual newErr "TapError should propagate tap error"

        testPropertyWithConfig fsCheckConfig "TapError does not execute on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = (FIO.succeed res).TapError(fun _ -> FIO.succeed(executed <- true))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual res "TapError should preserve success"
            Expect.isFalse executed "TapError should not execute on success"

        testPropertyWithConfig fsCheckConfig "TapError discards tap result"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = (FIO.fail err).TapError(fun _ -> FIO.succeed 999)
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual err "TapError should discard tap result"

        testPropertyWithConfig fsCheckConfig "TapBoth executes on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable successTap = false
            let mutable errorTap = false
            let eff = (FIO.succeed res).TapBoth(
                (fun _ -> FIO.succeed(successTap <- true)),
                (fun _ -> FIO.succeed(errorTap <- true)))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual res "TapBoth should preserve success"
            Expect.isTrue successTap "TapBoth should execute success tap"
            Expect.isFalse errorTap "TapBoth should not execute error tap on success"

        testPropertyWithConfig fsCheckConfig "TapBoth executes on error"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable successTap = false
            let mutable errorTap = false
            let eff = (FIO.fail err).TapBoth(
                (fun _ -> FIO.succeed(successTap <- true)),
                (fun _ -> FIO.succeed(errorTap <- true)))
            let actual = runtime.Run(eff).UnsafeError()
            Expect.equal actual err "TapBoth should preserve error"
            Expect.isFalse successTap "TapBoth should not execute success tap on error"
            Expect.isTrue errorTap "TapBoth should execute error tap"

        // Tap tests (additional)
        testPropertyWithConfig fsCheckConfig "Tap chain multiple side effects"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable effects : int list = []
            let eff =
                (FIO.succeed res)
                    .Tap(fun r -> effects <- effects @ [r * 1]; FIO.succeed())
                    .Tap(fun r -> effects <- effects @ [r * 2]; FIO.succeed())
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Tap chain should preserve original value"
            Expect.equal effects [res * 1; res * 2] "Tap chain should execute all side effects"

        // TapError tests (additional)
        testPropertyWithConfig fsCheckConfig "TapError chain multiple side effects"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable effects : int list = []
            let eff =
                (FIO.fail err)
                    .TapError(fun e -> effects <- effects @ [e * 1]; FIO.succeed())
                    .TapError(fun e -> effects <- effects @ [e * 2]; FIO.succeed())
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result err "TapError chain should preserve original error"
            Expect.equal effects [err * 1; err * 2] "TapError chain should execute all side effects"

        // Debug tests (verify no throw)
        testPropertyWithConfig fsCheckConfig "Debug does not throw on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).Debug()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Debug should preserve success value"

        testPropertyWithConfig fsCheckConfig "Debug with custom message"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).Debug("Custom")
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Debug with message should preserve success value"

        // DebugError tests
        testPropertyWithConfig fsCheckConfig "DebugError does not throw on error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = (FIO.fail err).DebugError()
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result err "DebugError should preserve error value"

        testPropertyWithConfig fsCheckConfig "DebugError with custom message"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff : FIO<int, string> = (FIO.fail err).DebugError("Custom Error")
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result err "DebugError with message should preserve error value"

        // Fold tests
        testPropertyWithConfig fsCheckConfig "Fold handles success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).Fold((fun e -> e), (fun r -> r * 2))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual (res * 2) "Fold should handle success"

        testPropertyWithConfig fsCheckConfig "Fold handles error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = (FIO.fail err).Fold((fun e -> e + 100), (fun r -> r * 2))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual (err + 100) "Fold should handle error"

        testPropertyWithConfig fsCheckConfig "Fold produces infallible effect"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let successEff = (FIO.succeed res).Fold((fun _ -> -1), (fun r -> r + 1))
            let errorEff = (FIO.fail err).Fold((fun e -> e * 2), (fun _ -> 0))
            let actualSuccess = runtime.Run(successEff).UnsafeSuccess()
            let actualError = runtime.Run(errorEff).UnsafeSuccess()
            Expect.equal actualSuccess (res + 1) "Fold success case"
            Expect.equal actualError (err * 2) "Fold error case"

        // FoldFIO tests
        testPropertyWithConfig fsCheckConfig "FoldFIO handles success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable sideEffect = false
            let eff = (FIO.succeed res).FoldFIO(
                (fun e -> FIO.succeed e),
                (fun r -> sideEffect <- true; FIO.succeed(r * 2)))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue sideEffect "FoldFIO should execute effectful success handler"
            Expect.equal actual (res * 2) "FoldFIO should handle success"

        testPropertyWithConfig fsCheckConfig "FoldFIO handles error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = (FIO.fail err).FoldFIO((fun e -> FIO.succeed(e + 100)), (fun r -> FIO.succeed(r * 2)))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual (err + 100) "FoldFIO should handle error"

        testPropertyWithConfig fsCheckConfig "FoldFIO error handler catches success handler errors"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let eff = (FIO.succeed res).FoldFIO((fun e -> FIO.succeed(e * 10)), (fun _ -> FIO.fail err))
            let actual = runtime.Run(eff).UnsafeSuccess()
            Expect.equal actual (err * 10) "FoldFIO should catch success handler errors"

        // Retry tests (additional)
        testPropertyWithConfig fsCheckConfig "Retry succeeds immediately without retrying"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.attemptExn(fun () -> attempts <- attempts + 1; res)
            let retried = eff.Retry 3
            let actual = runtime.Run(retried).UnsafeSuccess()
            Expect.equal actual res "Retry should succeed"
            Expect.equal attempts 1 "Retry should not retry on immediate success"

        testPropertyWithConfig fsCheckConfig "Retry retries up to max attempts"
        <| fun (runtime: FIORuntime) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                return! FIO.fail "error"
            }
            let retried = eff.Retry 4
            let _ = runtime.Run(retried).UnsafeResult()
            Expect.equal attempts 4 "Retry should retry up to max"

        testPropertyWithConfig fsCheckConfig "Retry succeeds on intermediate attempt"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                if attempts < 3 then return! FIO.fail "error"
                else return res
            }
            let retried = eff.Retry 5
            let actual = runtime.Run(retried).UnsafeSuccess()
            Expect.equal actual res "Retry should succeed on third attempt"
            Expect.equal attempts 3 "Should take 3 attempts"

        testPropertyWithConfig fsCheckConfig "Retry fails after max retries"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.fail "error"
            let retried = eff.Retry 2
            let actual = runtime.Run(retried).UnsafeError()
            Expect.equal actual "error" "Retry should fail after max retries"

        testPropertyWithConfig fsCheckConfig "Retry preserves success value"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.attemptExn(fun () ->
                attempts <- attempts + 1
                if attempts = 1 then failwith "Fail once" else res)
            let retried = eff.Retry 5
            let actual = runtime.Run(retried).UnsafeSuccess()
            Expect.equal actual res "Retry should preserve success value"

        testPropertyWithConfig fsCheckConfig "Retry with callback receives correct attempt numbers"
        <| fun (runtime: FIORuntime) ->
            let mutable attempts = []
            let eff = (FIO.fail "error").Retry(3, fun (_, attempt, max) ->
                attempts <- attempts @ [(attempt, max)]
                FIO.unit())
            let _ = runtime.Run(eff).UnsafeResult()
            Expect.equal attempts [(1, 3); (2, 3)] "Retry callback should receive correct attempt numbers"

        // RetryOrElse tests
        testPropertyWithConfig fsCheckConfig "RetryOrElse falls back after max retries"
        <| fun (runtime: FIORuntime, fallbackValue: int) ->
            let eff = (FIO.fail "error").RetryOrElse(2, fun _ -> FIO.succeed fallbackValue)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result fallbackValue "RetryOrElse should fall back after max retries"

        testPropertyWithConfig fsCheckConfig "RetryOrElse succeeds without fallback if effect succeeds"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = (FIO.succeed value).RetryOrElse(3, fun _ -> FIO.succeed -1)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result value "RetryOrElse should return original value on success"

        testPropertyWithConfig fsCheckConfig "RetryOrElse with callback"
        <| fun (runtime: FIORuntime, fallbackValue: int) ->
            let mutable callbackCount = 0
            let eff : FIO<int, int> = (FIO.fail 0).RetryOrElse(3, (fun _ -> FIO.succeed fallbackValue), fun _ ->
                callbackCount <- callbackCount + 1
                FIO.unit())
            let _ = runtime.Run(eff).UnsafeResult()
            Expect.equal callbackCount 2 "RetryOrElse callback should be called on each retry"

        // Timeout tests
        testPropertyWithConfig fsCheckConfig "Timeout returns Some on fast effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = (FIO.succeed value).Timeout(TimeSpan.FromSeconds 5.0, id)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result (Some value) "Timeout should return Some for fast effect"

        testPropertyWithConfig fsCheckConfig "Timeout returns None on slow effect"
        <| fun (runtime: FIORuntime) ->
            let slowEff = FIO.sleepExn(TimeSpan.FromSeconds 10.0).FlatMap(fun () -> FIO.succeed 42)
            let eff = slowEff.Timeout(TimeSpan.FromMilliseconds 20.0, id)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result None "Timeout should return None for slow effect"

        // Race tests
        testPropertyWithConfig fsCheckConfig "Race returns first completing effect"
        <| fun (runtime: FIORuntime) ->
            let fast = FIO.succeed 1
            let slow = FIO.sleepExn(TimeSpan.FromSeconds 10.0).FlatMap(fun () -> FIO.succeed 2)
            let eff = fast.Race slow
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 1 "Race should return first completing effect"

        testPropertyWithConfig fsCheckConfig "Race propagates error from first completing"
        <| fun (runtime: FIORuntime) ->
            let err = exn "fast error"
            let fast = FIO.fail err
            let slow = FIO.sleepExn(TimeSpan.FromSeconds 10.0).FlatMap(fun () -> FIO.succeed 2)
            let eff = fast.Race slow
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result.Message err.Message "Race should propagate error from first completing"

        // OrInterrupt tests
        testPropertyWithConfig fsCheckConfig "OrInterrupt preserves success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = (FIO.succeed value).OrInterrupt(fun e -> $"Error: {e}")
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result value "OrInterrupt should preserve success"

        testPropertyWithConfig fsCheckConfig "OrInterrupt converts error to interrupt"
        <| fun (runtime: FIORuntime) ->
            let eff : FIO<int, exn> = (FIO.fail "error").OrInterrupt(fun e -> $"Interrupted: {e}")
            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously
            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "OrInterrupt should convert error to interrupt"

        // ZipParError tests
        testPropertyWithConfig fsCheckConfig "ZipParError returns tuple of errors when both fail"
        <| fun (runtime: FIORuntime) ->
            let err1 = "error1"
            let err2 = "error2"
            let eff1 : FIO<int, string> = FIO.fail err1
            let eff2 : FIO<int, string> = FIO.fail err2
            let result = runtime.Run(eff1.ZipParError eff2).UnsafeError()
            Expect.equal result (err1, err2) "ZipParError should return tuple of errors"

        testPropertyWithConfig fsCheckConfig "ZipParError returns success when first succeeds"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff1 : FIO<int, string> = FIO.succeed value
            let eff2 : FIO<int, string> = FIO.fail "error2"
            let result = runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()
            Expect.equal result value "ZipParError should return success when first succeeds"

        testPropertyWithConfig fsCheckConfig "ZipParError returns success when second succeeds"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff1 : FIO<int, string> = FIO.fail "error1"
            let eff2 : FIO<int, string> = FIO.succeed value
            let result = runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()
            Expect.equal result value "ZipParError should return success when second succeeds"

        testPropertyWithConfig fsCheckConfig "ZipParError succeeds when both succeed"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff1 : FIO<int, string> = FIO.succeed res
            let eff2 : FIO<int, string> = FIO.succeed (res + 1)
            let result = runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()
            Expect.equal result res "ZipParError should return first result when both succeed"
    ]
