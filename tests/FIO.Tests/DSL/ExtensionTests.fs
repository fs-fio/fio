/// <summary>Provides property-based tests for FIO extension methods</summary>
module FIO.Tests.ExtensionTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

open Expecto

open System
open System.IO
open System.Diagnostics

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (func: FIORuntime -> unit) =
    testList name
        [ for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> func rt) ]

[<Tests>]
let extensionTests =
    testList
        "Extension Methods"
        [
            // ─── Mapping ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Map - transforms success value"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Map(fun x -> x * 2)).UnsafeSuccess()

                Expect.equal result (res * 2) "Map should transform success value"

            testPropertyWithConfig fsCheckConfig "Map - preserves error"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Map(fun x -> x * 2)).UnsafeError()

                Expect.equal result err "Map should preserve error"

            testPropertyWithConfig fsCheckConfig "MapError - transforms error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.MapError(fun e -> e.ToString())).UnsafeError()

                Expect.equal result (err.ToString()) "MapError should transform error"

            testPropertyWithConfig fsCheckConfig "MapError - preserves success"
            <| fun (runtime: FIORuntime, res: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.MapError(fun e -> e.ToString())).UnsafeSuccess()

                Expect.equal result res "MapError should preserve success"

            testPropertyWithConfig fsCheckConfig "MapBoth - transforms success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.MapBoth (fun x -> x * 2) (fun e -> e + 100)).UnsafeSuccess()

                Expect.equal result (res * 2) "MapBoth should transform success"

            testPropertyWithConfig fsCheckConfig "MapBoth - transforms error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.MapBoth(fun x -> x * 2) (fun e -> e + 100)).UnsafeError()

                Expect.equal result (err + 100) "MapBoth should transform error"

            testPropertyWithConfig fsCheckConfig "MapAttempt - transforms success value"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.MapAttempt (fun x -> x * 2) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result (res * 2) "MapAttempt should transform success value"

            testPropertyWithConfig fsCheckConfig "MapAttempt - preserves original error"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.MapAttempt(fun x -> x * 2) (fun ex -> ex.Message)).UnsafeError()

                Expect.equal result err "MapAttempt should preserve the original typed error"

            testPropertyWithConfig fsCheckConfig "MapAttempt - routes mapper exception through onError"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res
                let boom = "boom"

                let result =
                    runtime.Run(eff.MapAttempt (fun _ -> failwith boom) (fun ex -> ex.Message))
                        .UnsafeError()

                Expect.equal result boom "MapAttempt should route mapper exceptions through onError"

            // ─── Replace / wrap success value ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Unit - discards result, returns unit"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Unit()).UnsafeSuccess()

                Expect.equal result () "Unit should discard result and return unit"

            testPropertyWithConfig fsCheckConfig "As - maps to constant value"
            <| fun (runtime: FIORuntime, res: int, constant: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.As constant).UnsafeSuccess()

                Expect.equal result constant "As should map to the constant value"

            testPropertyWithConfig fsCheckConfig "AsLeft - wraps success in Choice1Of2"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.AsLeft ()).UnsafeSuccess()

                Expect.equal result (Choice1Of2 res) "AsLeft should wrap success in Choice1Of2"

            testPropertyWithConfig fsCheckConfig "AsRight - wraps success in Choice2Of2"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.AsRight ()).UnsafeSuccess()

                Expect.equal result (Choice2Of2 res) "AsRight should wrap success in Choice2Of2"

            testPropertyWithConfig fsCheckConfig "AsSome - wraps success in Some"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.AsSome()).UnsafeSuccess()

                Expect.equal result (Some res) "AsSome should wrap success in Some"

            // ─── Wrap typed error ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "AsLeftError - wraps error in Choice1Of2"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.AsLeftError ()).UnsafeError()

                Expect.equal result (Choice1Of2 err) "AsLeftError should wrap error in Choice1Of2"

            testPropertyWithConfig fsCheckConfig "AsRightError - wraps error in Choice2Of2"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.AsRightError ()).UnsafeError()

                Expect.equal result (Choice2Of2 err) "AsRightError should wrap error in Choice2Of2"

            testPropertyWithConfig fsCheckConfig "AsSomeError - wraps error in Some"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.AsSomeError ()).UnsafeError()

                Expect.equal result (Some err) "AsSomeError should wrap error in Some"

            // ─── Container shape (outcome → infallible) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Result - converts success to Ok"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Result ()).UnsafeSuccess()

                Expect.equal result (Ok res) "Result should convert success to Ok"

            testPropertyWithConfig fsCheckConfig "Result - converts error to Error"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Result ()).UnsafeSuccess()

                Expect.equal result (Error err) "Result should convert error to Error"
    
            testPropertyWithConfig fsCheckConfig "Option - converts success to Some"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Option ()).UnsafeSuccess()

                Expect.equal result (Some res) "Option should convert success to Some"

            testPropertyWithConfig fsCheckConfig "Option - converts error to None"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Option ()).UnsafeSuccess()

                Expect.equal result None "Option should convert error to None"

            testPropertyWithConfig fsCheckConfig "Choice - converts success to Choice1Of2"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Choice ()).UnsafeSuccess()

                Expect.equal result (Choice1Of2 res) "Choice should convert success to Choice1Of2"

            testPropertyWithConfig fsCheckConfig "Choice - converts error to Choice2Of2"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Choice ()).UnsafeSuccess()

                Expect.equal result (Choice2Of2 err) "Choice should convert error to Choice2Of2"

            testPropertyWithConfig fsCheckConfig "Flip - success becomes error"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Flip ()).UnsafeError()

                Expect.equal result res "Flip should move success value to error channel"

            testPropertyWithConfig fsCheckConfig "Flip - error becomes success"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Flip ()).UnsafeSuccess()

                Expect.equal result err "Flip should move typed error to success channel"

            testPropertyWithConfig fsCheckConfig "Flip - double flip restores success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Flip().Flip ()).UnsafeSuccess()

                Expect.equal result res "Flip().Flip() should restore the original success"

            testPropertyWithConfig fsCheckConfig "Flip - double flip restores error"
            <| fun (runtime: FIORuntime, err: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.Flip().Flip ()).UnsafeError()

                Expect.equal result err "Flip().Flip() should restore the original error"

            // ─── Outcome predicates / discards ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Ignore - returns unit on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).Ignore()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual () "Ignore should return unit on success"

            testPropertyWithConfig fsCheckConfig "Ignore - returns unit on failure (swallows error)"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).Ignore()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual () "Ignore should return unit on failure"

            testPropertyWithConfig fsCheckConfig "IsSuccess - returns true on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).IsSuccess()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue actual "IsSuccess should return true on success"

            testPropertyWithConfig fsCheckConfig "IsSuccess - returns false on failure"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).IsSuccess()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse actual "IsSuccess should return false on failure"

            testPropertyWithConfig fsCheckConfig "IsFailure - returns true on failure"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).IsFailure()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue actual "IsFailure should return true on failure"

            testPropertyWithConfig fsCheckConfig "IsFailure - returns false on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).IsFailure()

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse actual "IsFailure should return false on success"

            // ─── Boolean guards ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "When - true executes effect"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable executed = false

                let eff =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            res)
                        id

                let _ =
                    runtime.Run(eff.When true).UnsafeSuccess()

                Expect.isTrue executed "When(true) should execute the effect"

            testPropertyWithConfig fsCheckConfig "When - false returns unit without executing"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable executed = false

                let eff =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            res)
                        id

                let result =
                    runtime.Run(eff.When false).UnsafeSuccess()

                Expect.isFalse executed "When(false) should not execute the effect"
                Expect.equal result () "When(false) should return unit"

            testPropertyWithConfig fsCheckConfig "Unless - false executes effect"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable executed = false

                let eff =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            res)
                        id

                let _ =
                    runtime.Run(eff.Unless false).UnsafeSuccess()

                Expect.isTrue executed "Unless(false) should execute the effect"

            testPropertyWithConfig fsCheckConfig "Unless - true returns unit without executing"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable executed = false

                let eff =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            res)
                        id

                let result =
                    runtime.Run(eff.Unless true).UnsafeSuccess()

                Expect.isFalse executed "Unless(true) should not execute the effect"
                Expect.equal result () "Unless(true) should return unit"

            // ─── Observation (preserve outcome, run side effect) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Tap - executes side effect preserving value"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable sideEffect = 0
                let eff = FIO.succeed(res).Tap(fun r -> FIO.succeed (sideEffect <- r * 2))

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual res "Tap should preserve original value"
                Expect.equal sideEffect (res * 2) "Tap should execute side effect"

            testPropertyWithConfig fsCheckConfig "Tap - propagates tap effect error"
            <| fun (runtime: FIORuntime, res: int, err: int) ->
                let eff = FIO.succeed(res).Tap(fun _ -> FIO.fail err)

                let actual =
                    runtime.Run(eff).UnsafeError()

                Expect.equal actual err "Tap should propagate tap error"

            testPropertyWithConfig fsCheckConfig "Tap - does not execute on original error"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable executed = false
                let eff = FIO.fail(err).Tap(fun _ -> FIO.succeed (executed <- true))

                let actual =
                    runtime.Run(eff).UnsafeError()

                Expect.equal actual err "Tap should preserve original error"
                Expect.isFalse executed "Tap should not execute on original error"

            testPropertyWithConfig fsCheckConfig "TapError - executes on error preserving error"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable sideEffect = 0
                let eff = FIO.fail(err).TapError(fun e -> FIO.succeed (sideEffect <- e * 2))

                let actual =
                    runtime.Run(eff).UnsafeError()

                Expect.equal actual err "TapError should preserve error"
                Expect.equal sideEffect (err * 2) "TapError should execute side effect"

            testPropertyWithConfig fsCheckConfig "TapError - propagates tap effect error"
            <| fun (runtime: FIORuntime, err: int, newErr: int) ->
                let eff = FIO.fail(err).TapError(fun _ -> FIO.fail newErr)

                let actual =
                    runtime.Run(eff).UnsafeError()

                Expect.equal actual newErr "TapError should propagate tap error"

            testPropertyWithConfig fsCheckConfig "TapError - does not execute on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable executed = false
                let eff = FIO.succeed(res).TapError(fun _ -> FIO.succeed (executed <- true))

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual res "TapError should preserve success"
                Expect.isFalse executed "TapError should not execute on success"

            testPropertyWithConfig fsCheckConfig "TapBoth - executes success tap on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable successTap = false
                let mutable errorTap = false

                let eff =
                    FIO.succeed(res)
                        .TapBoth (fun _ -> FIO.succeed (successTap <- true)) (fun _ -> FIO.succeed (errorTap <- true))

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual res "TapBoth should preserve success"
                Expect.isTrue successTap "TapBoth should execute success tap"
                Expect.isFalse errorTap "TapBoth should not execute error tap on success"

            testPropertyWithConfig fsCheckConfig "TapBoth - executes error tap on error"
            <| fun (runtime: FIORuntime, err: int) ->
                let mutable successTap = false
                let mutable errorTap = false

                let eff =
                    FIO.fail(err)
                        .TapBoth (fun _ -> FIO.succeed (successTap <- true)) (fun _ -> FIO.succeed (errorTap <- true))

                let actual = runtime.Run(eff).UnsafeError()

                Expect.equal actual err "TapBoth should preserve error"
                Expect.isFalse successTap "TapBoth should not execute success tap on error"
                Expect.isTrue errorTap "TapBoth should execute error tap"

            // Debug/DebugError tests must run sequentially because they mutate process-global Console.Out/Error
            testSequenced (
                testList
                    "Debug"
                    [
                        testPropertyWithConfig fsCheckConfig "Debug - preserves success value"
                        <| fun (runtime: FIORuntime, res: int) ->
                            let eff = FIO.succeed(res).Debug()
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(eff).UnsafeSuccess()
                                Expect.equal result res "Debug should preserve success value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "Debug - with custom message preserves value"
                        <| fun (runtime: FIORuntime, res: int) ->
                            let eff = FIO.succeed(res).Debug "Custom"
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(eff).UnsafeSuccess()
                                Expect.equal result res "Debug with message should preserve success value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "DebugError - preserves error value"
                        <| fun (runtime: FIORuntime, err: string) ->
                            let eff = FIO.fail(err).DebugError()
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(eff).UnsafeError()
                                Expect.equal result err "DebugError should preserve error value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "DebugError - with custom message preserves error"
                        <| fun (runtime: FIORuntime, err: string) ->
                            let eff = FIO.fail(err).DebugError "Custom Error"
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(eff).UnsafeError()
                                Expect.equal result err "DebugError with message should preserve error value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr
                    ]
            )

            // ─── Recovery / fallback ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "OrElse - falls back on error"
            <| fun (runtime: FIORuntime, err: string, fallback: int) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result fallback "OrElse should return fallback on error"

            testPropertyWithConfig fsCheckConfig "OrElse - passes through on success"
            <| fun (runtime: FIORuntime, res: int, fallback: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.OrElse(FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result res "OrElse should pass through on success"

            testPropertyWithConfig fsCheckConfig "OrElse - chains fallbacks"
            <| fun (runtime: FIORuntime, fallback: int) ->
                let eff = (FIO.fail "err1").OrElse(FIO.fail "err2").OrElse(FIO.succeed fallback)

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result fallback "OrElse should chain fallbacks"

            testPropertyWithConfig fsCheckConfig "OrElseSucceed - falls back to value on error"
            <| fun (runtime: FIORuntime, err: string, fallback: int) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.OrElseSucceed fallback).UnsafeSuccess()

                Expect.equal result fallback "OrElseSucceed should produce the fallback value on error"

            testPropertyWithConfig fsCheckConfig "OrElseSucceed - passes through on success"
            <| fun (runtime: FIORuntime, res: int, fallback: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.OrElseSucceed fallback).UnsafeSuccess()

                Expect.equal result res "OrElseSucceed should pass through the original success"

            testPropertyWithConfig fsCheckConfig "OrElseFail - replaces error with constant"
            <| fun (runtime: FIORuntime, err: string, replacement: int) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.OrElseFail replacement).UnsafeError()

                Expect.equal result replacement "OrElseFail should replace the original error"

            testPropertyWithConfig fsCheckConfig "OrElseFail - passes through on success"
            <| fun (runtime: FIORuntime, res: int, replacement: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.OrElseFail replacement).UnsafeSuccess()

                Expect.equal result res "OrElseFail should pass through the original success"

            testPropertyWithConfig fsCheckConfig "OrElseEither - this succeeds returns Choice1Of2"
            <| fun (runtime: FIORuntime, res: int, fallback: string) ->
                let eff = FIO.succeed res
                let fallbackEff = FIO.succeed fallback

                let result =
                    runtime.Run(eff.OrElseEither fallbackEff).UnsafeSuccess()

                Expect.equal result (Choice1Of2 res) "OrElseEither should return Choice1Of2 on success"

            testPropertyWithConfig fsCheckConfig "OrElseEither - this fails, fallback succeeds returns Choice2Of2"
            <| fun (runtime: FIORuntime, err: string, fallback: string) ->
                let eff = FIO.fail err
                let fallbackEff = FIO.succeed fallback

                let result =
                    runtime.Run(eff.OrElseEither fallbackEff).UnsafeSuccess()

                Expect.equal result (Choice2Of2 fallback) "OrElseEither should return Choice2Of2 when fallback succeeds"

            testPropertyWithConfig fsCheckConfig "OrElseEither - both fail returns fallback error"
            <| fun (runtime: FIORuntime, err: string, fallbackErr: int) ->
                let eff = FIO.fail err
                let fallbackEff = FIO.fail fallbackErr

                let result =
                    runtime.Run(eff.OrElseEither fallbackEff).UnsafeError()

                Expect.equal result fallbackErr "OrElseEither should propagate the fallback's error when both fail"

            testPropertyWithConfig fsCheckConfig "CatchSome - partial function matches, recovers"
            <| fun (runtime: FIORuntime, err: int, recovery: string) ->
                let eff = FIO.fail err
                let func = fun e -> if e = err then Some(FIO.succeed recovery) else None

                let result =
                    runtime.Run(eff.CatchSome func).UnsafeSuccess()

                Expect.equal result recovery "CatchSome should recover when partial function matches"

            testPropertyWithConfig fsCheckConfig "CatchSome - partial function returns None, propagates error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail err
                let func = fun _ -> None

                let result =
                    runtime.Run(eff.CatchSome func).UnsafeError()

                Expect.equal result err "CatchSome should propagate error when partial function returns None"

            testPropertyWithConfig fsCheckConfig "OrInterrupt - preserves success"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).OrInterrupt(fun e -> $"Error: {e}")

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "OrInterrupt should preserve success"

            testPropertyWithConfig fsCheckConfig "OrInterrupt - converts error to interrupt"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.fail("error").OrInterrupt(fun e -> $"Interrupted: {e}")

                let fiber = runtime.Run eff
                let fiberResult =
                    fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                match fiberResult with
                | Interrupted _ -> ()
                | _ -> failtest "OrInterrupt should convert error to interrupt"

            // ─── Filter / partial functions ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "FilterOrFail - predicate passes returns success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrFail (fun _ -> true) "rejected").UnsafeSuccess()

                Expect.equal result res "FilterOrFail should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrFail - predicate fails returns supplied error"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrFail (fun _ -> false) error).UnsafeError()

                Expect.equal result error "FilterOrFail should fail with the supplied error when predicate rejects"

            testPropertyWithConfig fsCheckConfig "FilterOrFail - original failure propagates"
            <| fun (runtime: FIORuntime, originalError: string) ->
                let eff = FIO.fail originalError

                let result =
                    runtime.Run(eff.FilterOrFail(fun _ -> true) "replacement").UnsafeError()

                Expect.equal result originalError "FilterOrFail should propagate the original failure unchanged"

            testPropertyWithConfig fsCheckConfig "FilterOrElse - predicate passes returns success"
            <| fun (runtime: FIORuntime, res: int, fallback: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrElse (fun _ -> true) (FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result res "FilterOrElse should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrElse - predicate fails evaluates fallback"
            <| fun (runtime: FIORuntime, res: int, fallback: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrElse (fun _ -> false) (FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result fallback "FilterOrElse should evaluate fallback when predicate rejects"

            testPropertyWithConfig fsCheckConfig "FilterOrElseWith - predicate passes returns success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrElseWith (fun _ -> true) (fun v -> FIO.succeed (v * 100))).UnsafeSuccess()

                Expect.equal result res "FilterOrElseWith should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrElseWith - predicate fails passes rejected value to fallback"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrElseWith (fun _ -> false) (fun v -> FIO.succeed (v + 1))).UnsafeSuccess()

                Expect.equal result (res + 1) "FilterOrElseWith should pass the rejected value to the fallback"

            testPropertyWithConfig fsCheckConfig "FilterOrInterrupt - predicate passes returns success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.FilterOrInterrupt (fun _ -> true) "should not interrupt").UnsafeSuccess()

                Expect.equal result res "FilterOrInterrupt should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrInterrupt - predicate fails interrupts the fiber"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let outcome =
                    runtime.Run(eff.FilterOrInterrupt (fun _ -> false) "rejected")

                let interrupted =
                    match outcome.Task() |> Async.AwaitTask |> Async.RunSynchronously with
                    | Interrupted _ -> true
                    | _ -> false

                Expect.isTrue interrupted "FilterOrInterrupt should interrupt the fiber when predicate rejects"

            testPropertyWithConfig fsCheckConfig "Reject - partial function matches fails with error"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Reject(fun _ -> Some error)).UnsafeError()

                Expect.equal result error "Reject should fail with the matched error"

            testPropertyWithConfig fsCheckConfig "Reject - partial function does not match passes through"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Reject(fun _ -> None)).UnsafeSuccess()

                Expect.equal result res "Reject should pass through when the partial function does not match"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function matches and effect succeeds fails with computed error"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.RejectFIO(fun _ -> Some (FIO.succeed error))).UnsafeError()

                Expect.equal result error "RejectFIO should fail with the successful result of the rejection effect"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function matches and effect fails propagates that failure"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.RejectFIO(fun _ -> Some (FIO.fail error))).UnsafeError()

                Expect.equal result error "RejectFIO should propagate failure of the rejection effect as the rejection error"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function does not match passes through"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.RejectFIO(fun _ -> None)).UnsafeSuccess()

                Expect.equal result res "RejectFIO should pass through when the partial function does not match"

            testPropertyWithConfig fsCheckConfig "Collect - partial function matches returns extracted value"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Collect error (fun v -> Some (v + 1))).UnsafeSuccess()

                Expect.equal result (res + 1) "Collect should return the extracted value when partial function matches"

            testPropertyWithConfig fsCheckConfig "Collect - partial function does not match fails with supplied error"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.Collect error (fun _ -> None)).UnsafeError()

                Expect.equal result error "Collect should fail with the supplied error when partial function does not match"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function matches and effect succeeds returns extracted value"
            <| fun (runtime: FIORuntime, res: int, extracted: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.CollectFIO error (fun _ -> Some (FIO.succeed extracted))).UnsafeSuccess()

                Expect.equal result extracted "CollectFIO should return the value from the extracted effect when partial function matches"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function matches and effect fails propagates that failure"
            <| fun (runtime: FIORuntime, res: int, innerError: string, outerError: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.CollectFIO outerError (fun _ -> Some (FIO.fail innerError))).UnsafeError()

                Expect.equal result innerError "CollectFIO should propagate the inner effect's failure"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function does not match fails with supplied error"
            <| fun (runtime: FIORuntime, res: int, error: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.CollectFIO error (fun _ -> None)).UnsafeError()

                Expect.equal result error "CollectFIO should fail with the supplied error when partial function does not match"

            // ─── Applicative ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Apply - applies function to value"
            <| fun (runtime: FIORuntime, res: int) ->
                let valEff = FIO.succeed res
                let fnEff = FIO.succeed (fun x -> x * 3)

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeSuccess()

                Expect.equal result (res * 3) "Apply should apply function to value"

            testPropertyWithConfig fsCheckConfig "Apply - propagates value error"
            <| fun (runtime: FIORuntime, err: string) ->
                let valEff = FIO.fail err
                let fnEff = FIO.succeed (fun x -> x * 3)

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeError()

                Expect.equal result err "Apply should propagate value error"

            testPropertyWithConfig fsCheckConfig "Apply - propagates function error"
            <| fun (runtime: FIORuntime, res: int, err: string) ->
                let valEff = FIO.succeed res
                let fnEff = FIO.fail err

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeError()

                Expect.equal result err "Apply should propagate function error"

            testPropertyWithConfig fsCheckConfig "ApplyError - applies error function"
            <| fun (runtime: FIORuntime, err: int) ->
                let errEff = FIO.fail err
                let fnEff = FIO.fail (fun e -> $"Error: {e}")

                let result =
                    runtime.Run(errEff.ApplyError fnEff).UnsafeError()

                Expect.equal result $"Error: {err}" "ApplyError should apply error function"

            testPropertyWithConfig fsCheckConfig "ApplyError - preserves success"
            <| fun (runtime: FIORuntime, res: string) ->
                let succEff = FIO.succeed res
                let fnEff = FIO.fail (fun e -> $"Error: {e}")

                let result =
                    runtime.Run(succEff.ApplyError fnEff).UnsafeSuccess()

                Expect.equal result res "ApplyError should preserve success"

            // ─── Sequential composition (Zip) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Zip - combines two success values into tuple"
            <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.Zip eff2).UnsafeSuccess()

                Expect.equal result (res1, res2) "Zip should combine two success values into tuple"

            testPropertyWithConfig fsCheckConfig "Zip - first fails returns error"
            <| fun (runtime: FIORuntime, err: string) ->
                let mutable secondExecuted = false
                let eff1 = FIO.fail err

                let eff2 =
                    FIO.attempt
                        (fun () ->
                            secondExecuted <- true
                            42)
                        (fun ex -> ex.Message)

                let result =
                    runtime.Run(eff1.Zip eff2).UnsafeError()

                Expect.equal result err "Zip should return error when first fails"
                Expect.isFalse secondExecuted "Zip should not execute second effect when first fails"

            testPropertyWithConfig fsCheckConfig "ZipError - combines two errors into tuple"
            <| fun (runtime: FIORuntime, err1: int, err2: string) ->
                let eff1 = FIO.fail err1
                let eff2 = FIO.fail err2

                let result =
                    runtime.Run(eff1.ZipError eff2).UnsafeError()

                Expect.equal result (err1, err2) "ZipError should combine two errors into tuple"

            testPropertyWithConfig fsCheckConfig "ZipRight - returns second result"
            <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()

                Expect.equal result res2 "ZipRight should return second result"

            testPropertyWithConfig fsCheckConfig "ZipLeft - returns first result"
            <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()

                Expect.equal result res1 "ZipLeft should return first result"

            // ─── Parallel composition (ZipPar) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "ZipPar - both succeed in parallel"
            <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()

                Expect.equal result (res1, res2) "ZipPar should combine results from parallel execution"

            testPropertyWithConfig fsCheckConfig "ZipParError - both fail returns error tuple"
            <| fun (runtime: FIORuntime) ->
                let eff1 = FIO.fail "error1"
                let eff2 = FIO.fail "error2"

                let result =
                    runtime.Run(eff1.ZipParError eff2).UnsafeError()

                Expect.equal result ("error1", "error2") "ZipParError should return tuple of errors"

            testPropertyWithConfig fsCheckConfig "ZipParError - second succeeds when first fails"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff1 = FIO.fail "error1"
                let eff2 = FIO.succeed value

                let result =
                    runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()

                Expect.equal result value "ZipParError should return success when second succeeds"

            testPropertyWithConfig fsCheckConfig "ZipParError - both succeed returns first"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff1 = FIO.succeed res
                let eff2 = FIO.succeed (res + 1)

                let result =
                    runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()

                Expect.equal result res "ZipParError should return first result when both succeed"

            testPropertyWithConfig fsCheckConfig "ZipParRight - returns second result from parallel"
            <| fun (runtime: FIORuntime, res1: int, res2: int) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()

                Expect.equal result res2 "ZipParRight should return second result from parallel execution"

            testPropertyWithConfig fsCheckConfig "ZipParLeft - returns first result from parallel"
            <| fun (runtime: FIORuntime, res1: int, res2: int) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()

                Expect.equal result res1 "ZipParLeft should return first result from parallel execution"

            // ─── Fold / consume outcome ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Fold - handles success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).Fold (fun e -> e) (fun r -> r * 2)

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual (res * 2) "Fold should handle success"

            testPropertyWithConfig fsCheckConfig "Fold - handles error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).Fold (fun e -> e + 100) (fun r -> r * 2)

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual (err + 100) "Fold should handle error"

            testPropertyWithConfig fsCheckConfig "FoldFIO - handles success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    FIO.succeed(res).FoldFIO (fun e -> FIO.succeed e) (fun r -> FIO.succeed (r * 2))

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual (res * 2) "FoldFIO should handle success"

            testPropertyWithConfig fsCheckConfig "FoldFIO - handles error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff =
                    FIO.fail(err).FoldFIO (fun e -> FIO.succeed (e + 100)) (fun r -> FIO.succeed (r * 2))

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual (err + 100) "FoldFIO should handle error"

            testPropertyWithConfig fsCheckConfig "FoldFIO - error handler catches success handler errors"
            <| fun (runtime: FIORuntime, res: int, err: int) ->
                let eff =
                    FIO.succeed(res).FoldFIO (fun e -> FIO.succeed (e * 10)) (fun _ -> FIO.fail err)

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual (err * 10) "FoldFIO should catch success handler errors"

            testPropertyWithConfig fsCheckConfig "OnDone - success branch runs onSuccess and yields unit"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable observedSuccess = 0
                let mutable observedError = 0
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.OnDone
                        (fun _ ->
                            observedError <- observedError + 1
                            FIO.unit ())
                        (fun v ->
                            observedSuccess <- v
                            FIO.unit ()))
                        .UnsafeSuccess()

                Expect.equal result () "OnDone should yield unit"
                Expect.equal observedSuccess res "OnDone should invoke onSuccess with the original value"
                Expect.equal observedError 0 "OnDone should not invoke onError on success"

            testPropertyWithConfig fsCheckConfig "OnDone - error branch runs onError and yields unit"
            <| fun (runtime: FIORuntime, err: string) ->
                let mutable observedError = ""
                let mutable observedSuccess = 0
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.OnDone
                        (fun e ->
                            observedError <- e
                            FIO.unit ())
                        (fun _ ->
                            observedSuccess <- observedSuccess + 1
                            FIO.unit ()))
                        .UnsafeSuccess()

                Expect.equal result () "OnDone should yield unit even on original failure"
                Expect.equal observedError err "OnDone should invoke onError with the original error"
                Expect.equal observedSuccess 0 "OnDone should not invoke onSuccess on failure"

            testPropertyWithConfig fsCheckConfig "OnDone - onSuccess failure propagates"
            <| fun (runtime: FIORuntime, res: int, handlerError: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.OnDone
                        (fun _ -> FIO.unit ())
                        (fun _ -> FIO.fail handlerError))
                        .UnsafeError()

                Expect.equal result handlerError "OnDone should propagate failures from onSuccess"

            testPropertyWithConfig fsCheckConfig "OnDone - onError failure propagates"
            <| fun (runtime: FIORuntime, err: string, handlerError: string) ->
                let eff = FIO.fail err

                let result =
                    runtime.Run(eff.OnDone
                        (fun _ -> FIO.fail handlerError)
                        (fun _ -> FIO.unit ()))
                        .UnsafeError()

                Expect.equal result handlerError "OnDone should propagate failures from onError"

            // ─── Retry (iterate on failure) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "RetryOrElse - falls back after max retries"
            <| fun (runtime: FIORuntime, fallbackValue: int) ->
                let eff = FIO.fail("error").RetryOrElse 2 (fun _ -> FIO.succeed fallbackValue) (fun _ -> FIO.unit ())

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result fallbackValue "RetryOrElse should fall back after max retries"

            testPropertyWithConfig fsCheckConfig "RetryOrElse - succeeds without fallback"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed(value).RetryOrElse 3 (fun _ -> FIO.succeed -1) (fun _ -> FIO.unit ())

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "RetryOrElse should return original value on success"

            testPropertyWithConfig fsCheckConfig "RetryOrElse - callback called on each retry"
            <| fun (runtime: FIORuntime, fallbackValue: int) ->
                let mutable callbackCount = 0

                let eff = FIO.fail(0).RetryOrElse
                            3
                            (fun _ -> FIO.succeed fallbackValue)
                            (fun _ ->
                                callbackCount <- callbackCount + 1
                                FIO.unit ())

                let _ =
                    runtime.Run(eff).UnsafeResult()

                Expect.equal callbackCount 2 "RetryOrElse callback should be called on each retry"

            testPropertyWithConfig fsCheckConfig "Retry - succeeds immediately without retrying"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable attempts = 0

                let eff = 
                    FIO.attempt 
                        (fun () ->
                            attempts <- attempts + 1
                            res)
                        id

                let retried = eff.Retry 3 (fun _ -> FIO.unit ())

                let actual =
                    runtime.Run(retried).UnsafeSuccess()

                Expect.equal actual res "Retry should succeed"
                Expect.equal attempts 1 "Retry should not retry on immediate success"

            testPropertyWithConfig fsCheckConfig "Retry - retries up to max attempts"
            <| fun (runtime: FIORuntime) ->
                let mutable attempts = 0

                let eff =
                    fio {
                        attempts <- attempts + 1
                        return! FIO.fail "error"
                    }

                let retried = eff.Retry 4 (fun _ -> FIO.unit ())

                let _ =
                    runtime.Run(retried).UnsafeResult()

                Expect.equal attempts 4 "Retry should retry up to max"

            testPropertyWithConfig fsCheckConfig "Retry - succeeds on intermediate attempt"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable attempts = 0

                let eff =
                    fio {
                        attempts <- attempts + 1
                        if attempts < 3 then
                            return! FIO.fail "error"
                        else
                            return res
                    }

                let retried = eff.Retry 5 (fun _ -> FIO.unit ())

                let actual =
                    runtime.Run(retried).UnsafeSuccess()

                Expect.equal actual res "Retry should succeed on third attempt"
                Expect.equal attempts 3 "Should take 3 attempts"

            testPropertyWithConfig fsCheckConfig "Retry - callback receives correct attempt numbers"
            <| fun (runtime: FIORuntime) ->
                let mutable attempts = []

                let eff =
                    FIO.fail("error").Retry
                        3
                        (fun (_, attempt, max) ->
                            attempts <- attempts @ [ (attempt, max) ]
                            FIO.unit ())

                let _ =
                    runtime.Run(eff).UnsafeResult()

                Expect.equal attempts [ 1, 3; 2, 3 ] "Retry callback should receive correct attempt numbers"

            testPropertyWithConfig fsCheckConfig "RetryUntil - stops when predicate matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(eff.RetryUntil(fun err -> err >= 3)).UnsafeError()

                Expect.equal result 3 "RetryUntil should fail with the error that matched the predicate"
                Expect.equal count 3 "RetryUntil should retry until predicate matches"

            testPropertyWithConfig fsCheckConfig "RetryUntil - fails immediately when predicate true on first error"
            <| fun (runtime: FIORuntime, errValue: int) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail errValue
                    }

                let result =
                    runtime.Run(eff.RetryUntil(fun _ -> true)).UnsafeError()

                Expect.equal count 1 "RetryUntil should fail without retrying"
                Expect.equal result errValue "RetryUntil should fail with the original error"

            testCase "RetryUntil - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(eff.RetryUntil(fun err -> err >= 10_000)).UnsafeError()

                Expect.equal count 10_000 "RetryUntil should retry 10000 times"
                Expect.equal result 10_000 "RetryUntil should fail with the final error"

            testPropertyWithConfig fsCheckConfig "RetryUntilEquals - stops when error matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(eff.RetryUntilEquals 4).UnsafeError()

                Expect.equal count 4 "RetryUntilEquals should retry until equality"
                Expect.equal result 4 "RetryUntilEquals should fail with the matched error"

            testPropertyWithConfig fsCheckConfig "RetryUntilFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail (exn (string count))
                    }

                let predicate (_: exn) =
                    if count >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed false

                let result =
                    runtime.Run(eff.RetryUntilFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RetryUntilFIO should propagate predicate error"
                Expect.equal count 2 "RetryUntilFIO should stop on predicate failure"

            testPropertyWithConfig fsCheckConfig "RetryWhile - stops when predicate becomes false"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(eff.RetryWhile(fun err -> err < 5)).UnsafeError()

                Expect.equal count 5 "RetryWhile should retry until predicate is false"
                Expect.equal result 5 "RetryWhile should fail with the error that failed the predicate"

            testCase "RetryWhile - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(eff.RetryWhile(fun err -> err < 10_000)).UnsafeError()

                Expect.equal count 10_000 "RetryWhile should retry 10000 times"
                Expect.equal result 10_000 "RetryWhile should fail with the final error"

            testPropertyWithConfig fsCheckConfig "RetryWhileFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        return! FIO.fail (exn (string count))
                    }

                let predicate (_: exn) =
                    if count >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed true

                let result =
                    runtime.Run(eff.RetryWhileFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RetryWhileFIO should propagate predicate error"
                Expect.equal count 2 "RetryWhileFIO should stop on predicate failure"

            testAllRuntimes "Eventually - succeeds after N failures" (fun runtime ->
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        if count < 5 then
                            return! FIO.fail (exn "transient")
                        else
                            return 42
                    }

                let result =
                    runtime.Run(eff.Eventually()).UnsafeSuccess()

                Expect.equal result 42 "Eventually should return the first success value"
                Expect.equal count 5 "Eventually should retry until success")

            testCase "Eventually - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let eff =
                    fio {
                        count <- count + 1
                        if count < 10_000 then
                            return! FIO.fail (exn "transient")
                        else
                            return count
                    }

                let result =
                    runtime.Run(eff.Eventually()).UnsafeSuccess()

                Expect.equal result 10_000 "Eventually should return the final success value"
                Expect.equal count 10_000 "Eventually should retry 10000 times"

            testPropertyWithConfig fsCheckConfig "RepeatN - repeats N times, returns last result"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt 
                        (fun () ->
                            count <- count + 1
                            count)
                        id
                    
                let result =
                    runtime.Run(eff.RepeatN 5).UnsafeSuccess()

                Expect.equal count 5 "RepeatN should execute 5 times"
                Expect.equal result 5 "RepeatN should return last result"

            testPropertyWithConfig fsCheckConfig "RepeatN - n=1 executes exactly once"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            res)
                        id
                    
                let result =
                    runtime.Run(eff.RepeatN 1).UnsafeSuccess()

                Expect.equal count 1 "RepeatN(1) should execute exactly once"
                Expect.equal result res "RepeatN(1) should return the result"

            testPropertyWithConfig fsCheckConfig "RepeatUntil - stops on first satisfying value"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id
                    
                let result =
                    runtime.Run(eff.RepeatUntil(fun r -> r >= 3)).UnsafeSuccess()

                Expect.equal count 3 "RepeatUntil should execute until predicate is satisfied"
                Expect.equal result 3 "RepeatUntil should return the value that satisfied the predicate"

            testPropertyWithConfig fsCheckConfig "RepeatUntil - executes once when predicate true on first try"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            res)
                        id

                let result =
                    runtime.Run(eff.RepeatUntil(fun _ -> true)).UnsafeSuccess()

                Expect.equal count 1 "RepeatUntil should execute exactly once"
                Expect.equal result res "RepeatUntil should return the first result"

            testCase "RepeatUntil - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(eff.RepeatUntil(fun r -> r >= 10_000)).UnsafeSuccess()

                Expect.equal count 10_000 "RepeatUntil should execute 10000 times"
                Expect.equal result 10_000 "RepeatUntil should return the final value"

            testPropertyWithConfig fsCheckConfig "RepeatUntilEquals - stops when value matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(eff.RepeatUntilEquals 4).UnsafeSuccess()

                Expect.equal count 4 "RepeatUntilEquals should execute until equality"
                Expect.equal result 4 "RepeatUntilEquals should return the matched value"

            testPropertyWithConfig fsCheckConfig "RepeatUntilFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let predicate r =
                    if r >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed false

                let result =
                    runtime.Run(eff.RepeatUntilFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RepeatUntilFIO should propagate predicate error"
                Expect.equal count 2 "RepeatUntilFIO should stop on predicate failure"

            testPropertyWithConfig fsCheckConfig "RepeatWhile - stops when predicate becomes false"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(eff.RepeatWhile(fun r -> r < 5)).UnsafeSuccess()

                Expect.equal count 5 "RepeatWhile should execute until predicate is false"
                Expect.equal result 5 "RepeatWhile should return the value that failed the predicate"

            testCase "RepeatWhile - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(eff.RepeatWhile(fun r -> r < 10_000)).UnsafeSuccess()

                Expect.equal count 10_000 "RepeatWhile should execute 10000 times"
                Expect.equal result 10_000 "RepeatWhile should return the final value"

            testPropertyWithConfig fsCheckConfig "RepeatWhileFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let predicate r =
                    if r >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed true

                let result =
                    runtime.Run(eff.RepeatWhileFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RepeatWhileFIO should propagate predicate error"
                Expect.equal count 2 "RepeatWhileFIO should stop on predicate failure"
    
            testAllRuntimes "Forever - loops until interrupted by Timeout" (fun runtime ->
                let mutable count = 0

                let eff =
                    FIO.attempt
                        (fun () -> count <- count + 1)
                        id

                let bounded = eff.Forever().Timeout (TimeSpan.FromMilliseconds 100.0) id

                let result =
                    runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result None "Forever should not produce a value before timeout"
                Expect.isGreaterThan count 0 "Forever should have run at least once before timeout")

            testAllRuntimes "Delay - returns the underlying result after sleeping" (fun runtime ->
                let mutable ran = false

                let eff =
                    FIO.attempt(
                        fun () ->
                            ran <- true
                            7)
                        id

                let sw = Stopwatch.StartNew()

                let result =
                    runtime.Run(eff.Delay (TimeSpan.FromMilliseconds 50.0) id).UnsafeSuccess()
                
                sw.Stop()

                Expect.equal result 7 "Delay should return the underlying effect's result"
                Expect.isTrue ran "Delay should run the underlying effect after sleeping"
                Expect.isGreaterThanOrEqual sw.Elapsed (TimeSpan.FromMilliseconds 40.0) "Delay should sleep for at least most of the requested duration")

            testAllRuntimes "Delay - propagates underlying effect's failure" (fun runtime ->
                let eff = FIO.fail (exn "boom")

                let result =
                    runtime.Run(eff.Delay (TimeSpan.FromMilliseconds 10.0) id).UnsafeError()

                Expect.equal result.Message "boom" "Delay should propagate the underlying error after sleeping")

            testAllRuntimes "Timeout - returns Some on fast effect" (fun runtime ->
                let eff = FIO.succeed(42).Timeout (TimeSpan.FromSeconds 5.0) id

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (Some 42) "Timeout should return Some for fast effect")

            testAllRuntimes "Timeout - returns None on slow effect" (fun runtime ->
                let slowEff =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) id)
                        .FlatMap(fun () -> FIO.succeed 42)

                let eff = slowEff.Timeout (TimeSpan.FromMilliseconds 50.0) id

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result None "Timeout should return None for slow effect")

            testPropertyWithConfig fsCheckConfig "TimeoutFail - effect completes in time returns success"
            <| fun (runtime: FIORuntime, res: int, timeoutError: string) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.TimeoutFail timeoutError (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result res "TimeoutFail should return success when effect completes in time"

            testPropertyWithConfig fsCheckConfig "TimeoutTo - effect completes in time applies onSuccess"
            <| fun (runtime: FIORuntime, res: int, defaultValue: int) ->
                let eff = FIO.succeed res

                let result =
                    runtime.Run(eff.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result (res * 2) "TimeoutTo should apply onSuccess when effect completes in time"

            testCase "TimeoutTo - timeout fires returns default value"
            <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let defaultValue = -1
                let eff =
                    (FIO.sleep (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).FlatMap(fun () -> FIO.succeed 0)

                let result =
                    runtime.Run(eff.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromMilliseconds 50.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result defaultValue "TimeoutTo should return the default value when timeout fires"

            testPropertyWithConfig fsCheckConfig "Timed - returns duration and result"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed res

                let duration, result = runtime.Run(eff.Timed id).UnsafeSuccess()

                Expect.equal result res "Timed should return the result"
                Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Timed duration should be >= 0"

            testAllRuntimes "Race - returns first completing effect" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let eff = fast.Race slow

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "Race should return first completing effect")

            testAllRuntimes "Race - propagates error from first completing" (fun runtime ->
                let err = exn "fast error"
                let fast = FIO.fail err
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let eff = fast.Race slow

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result.Message err.Message "Race should propagate error from first completing")

            testAllRuntimes "RaceEither - first racer wins returns Choice1Of2" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed "slow")
                let eff = fast.RaceEither slow

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (Choice1Of2 1) "RaceEither should return Choice1Of2 when this wins")

            testAllRuntimes "RaceEither - second racer wins returns Choice2Of2" (fun runtime ->
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 1)
                let fast = FIO.succeed "fast"
                let eff = slow.RaceEither fast

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (Choice2Of2 "fast") "RaceEither should return Choice2Of2 when the right racer wins")
                
            testAllRuntimes "RaceFirstSuccess - both succeed, fastest wins" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let eff = fast.RaceFirstSuccess slow

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "RaceFirstSuccess should return the fastest successful value")

            testAllRuntimes "RaceFirstSuccess - fast failure waits for slow success" (fun runtime ->
                let fastFail = FIO.fail (exn "fast error")
                let slowSucceed =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.succeed 7)
                let eff = fastFail.RaceFirstSuccess slowSucceed

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 7 "RaceFirstSuccess should wait for a peer when the first racer fails")

            testAllRuntimes "RaceFirstSuccess - slow failure does not interrupt fast success" (fun runtime ->
                let fastSucceed = FIO.succeed 11
                let slowFail =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.fail (exn "slow"))
                let eff = fastSucceed.RaceFirstSuccess slowFail

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 11 "RaceFirstSuccess should return the fast success without waiting for slow failure")

            testAllRuntimes "RaceFirstSuccess - both fail returns the later error" (fun runtime ->
                let fastFail = FIO.fail (exn "fast error")
                let slowFail =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.fail (exn "slow error"))
                let eff = fastFail.RaceFirstSuccess slowFail

                let result =
                    runtime.Run(eff).UnsafeError()

                Expect.equal result.Message "slow error" "RaceFirstSuccess should fail with the most-recently-received error when both racers fail")

            testPropertyWithConfig fsCheckConfig "FlatMap - chains success values"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).FlatMap(fun x -> FIO.succeed (x + 1))

                let result =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal result (res + 1) "FlatMap should chain success values"

            testPropertyWithConfig fsCheckConfig "FlatMap - short-circuits on error"
            <| fun (runtime: FIORuntime, err: string) ->
                let mutable secondExecuted = false

                let eff =
                    FIO.fail(err)
                        .FlatMap(fun _ ->
                            secondExecuted <- true
                            FIO.succeed 42)

                let result =
                    runtime.Run(eff).UnsafeError()

                Expect.equal result err "FlatMap should return error"
                Expect.isFalse secondExecuted "FlatMap should short-circuit on error"

            testPropertyWithConfig fsCheckConfig "CatchAll - recovers from error"
            <| fun (runtime: FIORuntime, err: int, recovery: int) ->
                let eff = FIO.fail(err).CatchAll(fun _ -> FIO.succeed recovery)

                let actual =
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual recovery "CatchAll should recover"

            testPropertyWithConfig fsCheckConfig "CatchAll - does not affect success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff = FIO.succeed(res).CatchAll(fun _ -> FIO.succeed 0)

                let actual =    
                    runtime.Run(eff).UnsafeSuccess()

                Expect.equal actual res "CatchAll should not affect success"

            testPropertyWithConfig fsCheckConfig "CatchAll - can transform error to new error"
            <| fun (runtime: FIORuntime, err: int) ->
                let eff = FIO.fail(err).CatchAll(fun e -> FIO.fail (e + 100))

                let actual =
                    runtime.Run(eff).UnsafeError()

                Expect.equal actual (err + 100) "CatchAll can transform to new error"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer runs on success"
            <| fun (runtime: FIORuntime, res: int) ->
                let mutable finalizerRan = false
                let eff = FIO.succeed res
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun ex -> ex.Message)

                let result =
                    runtime.Run(eff.Ensuring finalizer).UnsafeSuccess()

                Expect.isTrue finalizerRan "Ensuring finalizer should run on success"
                Expect.equal result res "Ensuring should preserve the success result"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer runs on failure"
            <| fun (runtime: FIORuntime, err: string) ->
                let mutable finalizerRan = false
                let eff = FIO.fail err
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun ex -> ex.Message)

                let result =
                    runtime.Run(eff.Ensuring finalizer).UnsafeError()

                Expect.isTrue finalizerRan "Ensuring finalizer should run on failure"
                Expect.equal result err "Ensuring should preserve the error"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer error on success propagates"
            <| fun (runtime: FIORuntime, res: int, finalizerErr: string) ->
                let eff = FIO.succeed res
                let finalizer = FIO.fail finalizerErr

                let result =
                    runtime.Run(eff.Ensuring finalizer).UnsafeError()

                Expect.equal result finalizerErr "Ensuring should propagate finalizer error when main effect succeeds"

            testPropertyWithConfig fsCheckConfig "Ensuring - main error preserved when finalizer also fails"
            <| fun (runtime: FIORuntime, mainErr: string, finalizerErr: string) ->
                let eff = FIO.fail mainErr
                let finalizer = FIO.fail finalizerErr

                let result =
                    runtime.Run(eff.Ensuring finalizer).UnsafeError()

                Expect.equal result mainErr "Ensuring should preserve main error, suppressing finalizer error"
        ]
