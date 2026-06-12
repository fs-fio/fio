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
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Map(fun x -> x * 2)).UnsafeSuccess()

                Expect.equal result (value * 2) "Map should transform success value"

            testPropertyWithConfig fsCheckConfig "Map - preserves error"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Map(fun x -> x * 2)).UnsafeError()

                Expect.equal result error "Map should preserve error"

            testPropertyWithConfig fsCheckConfig "MapError - transforms error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.MapError(fun e -> e.ToString())).UnsafeError()

                Expect.equal result (error.ToString()) "MapError should transform error"

            testPropertyWithConfig fsCheckConfig "MapError - preserves success"
            <| fun (runtime: FIORuntime, value: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.MapError(fun e -> e.ToString())).UnsafeSuccess()

                Expect.equal result value "MapError should preserve success"

            testPropertyWithConfig fsCheckConfig "MapBoth - transforms success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.MapBoth (fun x -> x * 2) (fun e -> e + 100)).UnsafeSuccess()

                Expect.equal result (value * 2) "MapBoth should transform success"

            testPropertyWithConfig fsCheckConfig "MapBoth - transforms error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.MapBoth(fun x -> x * 2) (fun e -> e + 100)).UnsafeError()

                Expect.equal result (error + 100) "MapBoth should transform error"

            testPropertyWithConfig fsCheckConfig "MapAttempt - transforms success value"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.MapAttempt (fun x -> x * 2) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result (value * 2) "MapAttempt should transform success value"

            testPropertyWithConfig fsCheckConfig "MapAttempt - preserves original error"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.MapAttempt(fun x -> x * 2) (fun ex -> ex.Message)).UnsafeError()

                Expect.equal result error "MapAttempt should preserve the original typed error"

            testPropertyWithConfig fsCheckConfig "MapAttempt - routes mapper exception through onError"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value
                let boom = "boom"

                let result =
                    runtime.Run(effect.MapAttempt (fun _ -> failwith boom) (fun ex -> ex.Message))
                        .UnsafeError()

                Expect.equal result boom "MapAttempt should route mapper exceptions through onError"

            // ─── Replace / wrap success value ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Unit - discards result, returns unit"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Unit()).UnsafeSuccess()

                Expect.equal result () "Unit should discard result and return unit"

            testPropertyWithConfig fsCheckConfig "As - maps to constant value"
            <| fun (runtime: FIORuntime, value: int, constant: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.As constant).UnsafeSuccess()

                Expect.equal result constant "As should map to the constant value"

            testPropertyWithConfig fsCheckConfig "AsLeft - wraps success in Choice1Of2"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.AsLeft ()).UnsafeSuccess()

                Expect.equal result (Choice1Of2 value) "AsLeft should wrap success in Choice1Of2"

            testPropertyWithConfig fsCheckConfig "AsRight - wraps success in Choice2Of2"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.AsRight ()).UnsafeSuccess()

                Expect.equal result (Choice2Of2 value) "AsRight should wrap success in Choice2Of2"

            testPropertyWithConfig fsCheckConfig "AsSome - wraps success in Some"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.AsSome()).UnsafeSuccess()

                Expect.equal result (Some value) "AsSome should wrap success in Some"

            // ─── Wrap typed error ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "AsLeftError - wraps error in Choice1Of2"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.AsLeftError ()).UnsafeError()

                Expect.equal result (Choice1Of2 error) "AsLeftError should wrap error in Choice1Of2"

            testPropertyWithConfig fsCheckConfig "AsRightError - wraps error in Choice2Of2"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.AsRightError ()).UnsafeError()

                Expect.equal result (Choice2Of2 error) "AsRightError should wrap error in Choice2Of2"

            testPropertyWithConfig fsCheckConfig "AsSomeError - wraps error in Some"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.AsSomeError ()).UnsafeError()

                Expect.equal result (Some error) "AsSomeError should wrap error in Some"

            // ─── Container shape (outcome → infallible) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Result - converts success to Ok"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Result ()).UnsafeSuccess()

                Expect.equal result (Ok value) "Result should convert success to Ok"

            testPropertyWithConfig fsCheckConfig "Result - converts error to Error"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Result ()).UnsafeSuccess()

                Expect.equal result (Error error) "Result should convert error to Error"
    
            testPropertyWithConfig fsCheckConfig "Option - converts success to Some"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Option ()).UnsafeSuccess()

                Expect.equal result (Some value) "Option should convert success to Some"

            testPropertyWithConfig fsCheckConfig "Option - converts error to None"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Option ()).UnsafeSuccess()

                Expect.equal result None "Option should convert error to None"

            testPropertyWithConfig fsCheckConfig "Choice - converts success to Choice1Of2"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Choice ()).UnsafeSuccess()

                Expect.equal result (Choice1Of2 value) "Choice should convert success to Choice1Of2"

            testPropertyWithConfig fsCheckConfig "Choice - converts error to Choice2Of2"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Choice ()).UnsafeSuccess()

                Expect.equal result (Choice2Of2 error) "Choice should convert error to Choice2Of2"

            testPropertyWithConfig fsCheckConfig "Flip - success becomes error"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Flip ()).UnsafeError()

                Expect.equal result value "Flip should move success value to error channel"

            testPropertyWithConfig fsCheckConfig "Flip - error becomes success"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Flip ()).UnsafeSuccess()

                Expect.equal result error "Flip should move typed error to success channel"

            testPropertyWithConfig fsCheckConfig "Flip - double flip restores success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Flip().Flip ()).UnsafeSuccess()

                Expect.equal result value "Flip().Flip() should restore the original success"

            testPropertyWithConfig fsCheckConfig "Flip - double flip restores error"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.Flip().Flip ()).UnsafeError()

                Expect.equal result error "Flip().Flip() should restore the original error"

            // ─── Outcome predicates / discards ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Ignore - returns unit on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).Ignore()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual () "Ignore should return unit on success"

            testPropertyWithConfig fsCheckConfig "Ignore - returns unit on failure (swallows error)"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).Ignore()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual () "Ignore should return unit on failure"

            testPropertyWithConfig fsCheckConfig "IsSuccess - returns true on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).IsSuccess()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isTrue actual "IsSuccess should return true on success"

            testPropertyWithConfig fsCheckConfig "IsSuccess - returns false on failure"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).IsSuccess()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isFalse actual "IsSuccess should return false on failure"

            testPropertyWithConfig fsCheckConfig "IsFailure - returns true on failure"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).IsFailure()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isTrue actual "IsFailure should return true on failure"

            testPropertyWithConfig fsCheckConfig "IsFailure - returns false on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).IsFailure()

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isFalse actual "IsFailure should return false on success"

            // ─── Boolean guards ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "When - true executes effect"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            value)
                        id

                let _ =
                    runtime.Run(effect.When true).UnsafeSuccess()

                Expect.isTrue executed "When(true) should execute the effect"

            testPropertyWithConfig fsCheckConfig "When - false returns unit without executing"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            value)
                        id

                let result =
                    runtime.Run(effect.When false).UnsafeSuccess()

                Expect.isFalse executed "When(false) should not execute the effect"
                Expect.equal result () "When(false) should return unit"

            testPropertyWithConfig fsCheckConfig "Unless - false executes effect"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            value)
                        id

                let _ =
                    runtime.Run(effect.Unless false).UnsafeSuccess()

                Expect.isTrue executed "Unless(false) should execute the effect"

            testPropertyWithConfig fsCheckConfig "Unless - true returns unit without executing"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false

                let effect =
                    FIO.attempt
                        (fun () ->
                            executed <- true
                            value)
                        id

                let result =
                    runtime.Run(effect.Unless true).UnsafeSuccess()

                Expect.isFalse executed "Unless(true) should not execute the effect"
                Expect.equal result () "Unless(true) should return unit"

            // ─── Observation (preserve outcome, run side effect) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Tap - executes side effect preserving value"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable sideEffect = 0
                let effect = FIO.succeed(value).Tap(fun r -> FIO.succeed (sideEffect <- r * 2))

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual value "Tap should preserve original value"
                Expect.equal sideEffect (value * 2) "Tap should execute side effect"

            testPropertyWithConfig fsCheckConfig "Tap - propagates tap effect error"
            <| fun (runtime: FIORuntime, value: int, error: int) ->
                let effect = FIO.succeed(value).Tap(fun _ -> FIO.fail error)

                let actual =
                    runtime.Run(effect).UnsafeError()

                Expect.equal actual error "Tap should propagate tap error"

            testPropertyWithConfig fsCheckConfig "Tap - does not execute on original error"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable executed = false
                let effect = FIO.fail(error).Tap(fun _ -> FIO.succeed (executed <- true))

                let actual =
                    runtime.Run(effect).UnsafeError()

                Expect.equal actual error "Tap should preserve original error"
                Expect.isFalse executed "Tap should not execute on original error"

            testPropertyWithConfig fsCheckConfig "TapError - executes on error preserving error"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable sideEffect = 0
                let effect = FIO.fail(error).TapError(fun e -> FIO.succeed (sideEffect <- e * 2))

                let actual =
                    runtime.Run(effect).UnsafeError()

                Expect.equal actual error "TapError should preserve error"
                Expect.equal sideEffect (error * 2) "TapError should execute side effect"

            testPropertyWithConfig fsCheckConfig "TapError - propagates tap effect error"
            <| fun (runtime: FIORuntime, error: int, newErr: int) ->
                let effect = FIO.fail(error).TapError(fun _ -> FIO.fail newErr)

                let actual =
                    runtime.Run(effect).UnsafeError()

                Expect.equal actual newErr "TapError should propagate tap error"

            testPropertyWithConfig fsCheckConfig "TapError - does not execute on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable executed = false
                let effect = FIO.succeed(value).TapError(fun _ -> FIO.succeed (executed <- true))

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual value "TapError should preserve success"
                Expect.isFalse executed "TapError should not execute on success"

            testPropertyWithConfig fsCheckConfig "TapBoth - executes success tap on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable successTap = false
                let mutable errorTap = false

                let effect =
                    FIO.succeed(value)
                        .TapBoth (fun _ -> FIO.succeed (successTap <- true)) (fun _ -> FIO.succeed (errorTap <- true))

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual value "TapBoth should preserve success"
                Expect.isTrue successTap "TapBoth should execute success tap"
                Expect.isFalse errorTap "TapBoth should not execute error tap on success"

            testPropertyWithConfig fsCheckConfig "TapBoth - executes error tap on error"
            <| fun (runtime: FIORuntime, error: int) ->
                let mutable successTap = false
                let mutable errorTap = false

                let effect =
                    FIO.fail(error)
                        .TapBoth (fun _ -> FIO.succeed (successTap <- true)) (fun _ -> FIO.succeed (errorTap <- true))

                let actual = runtime.Run(effect).UnsafeError()

                Expect.equal actual error "TapBoth should preserve error"
                Expect.isFalse successTap "TapBoth should not execute success tap on error"
                Expect.isTrue errorTap "TapBoth should execute error tap"

            // Debug/DebugError tests must run sequentially because they mutate process-global Console.Out/Error
            testSequenced (
                testList
                    "Debug"
                    [
                        testPropertyWithConfig fsCheckConfig "Debug - preserves success value"
                        <| fun (runtime: FIORuntime, value: int) ->
                            let effect = FIO.succeed(value).Debug()
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(effect).UnsafeSuccess()
                                Expect.equal result value "Debug should preserve success value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "Debug - with custom message preserves value"
                        <| fun (runtime: FIORuntime, value: int) ->
                            let effect = FIO.succeed(value).Debug "Custom"
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(effect).UnsafeSuccess()
                                Expect.equal result value "Debug with message should preserve success value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "DebugError - preserves error value"
                        <| fun (runtime: FIORuntime, error: string) ->
                            let effect = FIO.fail(error).DebugError()
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(effect).UnsafeError()
                                Expect.equal result error "DebugError should preserve error value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr

                        testPropertyWithConfig fsCheckConfig "DebugError - with custom message preserves error"
                        <| fun (runtime: FIORuntime, error: string) ->
                            let effect = FIO.fail(error).DebugError "Custom Error"
                            let oldOut = Console.Out
                            let oldErr = Console.Error
                            Console.SetOut TextWriter.Null
                            Console.SetError TextWriter.Null

                            try
                                let result =
                                    runtime.Run(effect).UnsafeError()
                                Expect.equal result error "DebugError with message should preserve error value"
                            finally
                                Console.SetOut oldOut
                                Console.SetError oldErr
                    ]
            )

            // ─── Recovery / fallback ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "OrElse - falls back on error"
            <| fun (runtime: FIORuntime, error: string, fallback: int) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.OrElse(FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result fallback "OrElse should return fallback on error"

            testPropertyWithConfig fsCheckConfig "OrElse - passes through on success"
            <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.OrElse(FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result value "OrElse should pass through on success"

            testPropertyWithConfig fsCheckConfig "OrElse - chains fallbacks"
            <| fun (runtime: FIORuntime, fallback: int) ->
                let effect = (FIO.fail "err1").OrElse(FIO.fail "err2").OrElse(FIO.succeed fallback)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result fallback "OrElse should chain fallbacks"

            testPropertyWithConfig fsCheckConfig "OrElseSucceed - falls back to value on error"
            <| fun (runtime: FIORuntime, error: string, fallback: int) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.OrElseSucceed fallback).UnsafeSuccess()

                Expect.equal result fallback "OrElseSucceed should produce the fallback value on error"

            testPropertyWithConfig fsCheckConfig "OrElseSucceed - passes through on success"
            <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.OrElseSucceed fallback).UnsafeSuccess()

                Expect.equal result value "OrElseSucceed should pass through the original success"

            testPropertyWithConfig fsCheckConfig "OrElseFail - replaces error with constant"
            <| fun (runtime: FIORuntime, error: string, replacement: int) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.OrElseFail replacement).UnsafeError()

                Expect.equal result replacement "OrElseFail should replace the original error"

            testPropertyWithConfig fsCheckConfig "OrElseFail - passes through on success"
            <| fun (runtime: FIORuntime, value: int, replacement: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.OrElseFail replacement).UnsafeSuccess()

                Expect.equal result value "OrElseFail should pass through the original success"

            testPropertyWithConfig fsCheckConfig "OrElseEither - this succeeds returns Choice1Of2"
            <| fun (runtime: FIORuntime, value: int, fallback: string) ->
                let effect = FIO.succeed value
                let fallbackEff = FIO.succeed fallback

                let result =
                    runtime.Run(effect.OrElseEither fallbackEff).UnsafeSuccess()

                Expect.equal result (Choice1Of2 value) "OrElseEither should return Choice1Of2 on success"

            testPropertyWithConfig fsCheckConfig "OrElseEither - this fails, fallback succeeds returns Choice2Of2"
            <| fun (runtime: FIORuntime, error: string, fallback: string) ->
                let effect = FIO.fail error
                let fallbackEff = FIO.succeed fallback

                let result =
                    runtime.Run(effect.OrElseEither fallbackEff).UnsafeSuccess()

                Expect.equal result (Choice2Of2 fallback) "OrElseEither should return Choice2Of2 when fallback succeeds"

            testPropertyWithConfig fsCheckConfig "OrElseEither - both fail returns fallback error"
            <| fun (runtime: FIORuntime, error: string, fallbackErr: int) ->
                let effect = FIO.fail error
                let fallbackEff = FIO.fail fallbackErr

                let result =
                    runtime.Run(effect.OrElseEither fallbackEff).UnsafeError()

                Expect.equal result fallbackErr "OrElseEither should propagate the fallback's error when both fail"

            testPropertyWithConfig fsCheckConfig "CatchSome - partial function matches, recovers"
            <| fun (runtime: FIORuntime, error: int, recovery: string) ->
                let effect = FIO.fail error
                let func = fun e -> if e = error then Some(FIO.succeed recovery) else None

                let result =
                    runtime.Run(effect.CatchSome func).UnsafeSuccess()

                Expect.equal result recovery "CatchSome should recover when partial function matches"

            testPropertyWithConfig fsCheckConfig "CatchSome - partial function returns None, propagates error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail error
                let func = fun _ -> None

                let result =
                    runtime.Run(effect.CatchSome func).UnsafeError()

                Expect.equal result error "CatchSome should propagate error when partial function returns None"

            testPropertyWithConfig fsCheckConfig "OrInterrupt - preserves success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).OrInterrupt(fun e -> $"Error: {e}")

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "OrInterrupt should preserve success"

            testPropertyWithConfig fsCheckConfig "OrInterrupt - converts error to interrupt"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.fail("error").OrInterrupt(fun e -> $"Interrupted: {e}")

                let fiber = runtime.Run effect
                let fiberResult =
                    fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                match fiberResult with
                | Interrupted _ -> ()
                | _ -> failtest "OrInterrupt should convert error to interrupt"

            // ─── Filter / partial functions ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "FilterOrFail - predicate passes returns success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrFail (fun _ -> true) "rejected").UnsafeSuccess()

                Expect.equal result value "FilterOrFail should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrFail - predicate fails returns supplied error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrFail (fun _ -> false) error).UnsafeError()

                Expect.equal result error "FilterOrFail should fail with the supplied error when predicate rejects"

            testPropertyWithConfig fsCheckConfig "FilterOrFail - original failure propagates"
            <| fun (runtime: FIORuntime, originalError: string) ->
                let effect = FIO.fail originalError

                let result =
                    runtime.Run(effect.FilterOrFail(fun _ -> true) "replacement").UnsafeError()

                Expect.equal result originalError "FilterOrFail should propagate the original failure unchanged"

            testPropertyWithConfig fsCheckConfig "FilterOrElse - predicate passes returns success"
            <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrElse (fun _ -> true) (FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result value "FilterOrElse should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrElse - predicate fails evaluates fallback"
            <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrElse (fun _ -> false) (FIO.succeed fallback)).UnsafeSuccess()

                Expect.equal result fallback "FilterOrElse should evaluate fallback when predicate rejects"

            testPropertyWithConfig fsCheckConfig "FilterOrElseWith - predicate passes returns success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrElseWith (fun _ -> true) (fun v -> FIO.succeed (v * 100))).UnsafeSuccess()

                Expect.equal result value "FilterOrElseWith should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrElseWith - predicate fails passes rejected value to fallback"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrElseWith (fun _ -> false) (fun v -> FIO.succeed (v + 1))).UnsafeSuccess()

                Expect.equal result (value + 1) "FilterOrElseWith should pass the rejected value to the fallback"

            testPropertyWithConfig fsCheckConfig "FilterOrInterrupt - predicate passes returns success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.FilterOrInterrupt (fun _ -> true) "should not interrupt").UnsafeSuccess()

                Expect.equal result value "FilterOrInterrupt should return success when predicate accepts"

            testPropertyWithConfig fsCheckConfig "FilterOrInterrupt - predicate fails interrupts the fiber"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let outcome =
                    runtime.Run(effect.FilterOrInterrupt (fun _ -> false) "rejected")

                let interrupted =
                    match outcome.Task() |> Async.AwaitTask |> Async.RunSynchronously with
                    | Interrupted _ -> true
                    | _ -> false

                Expect.isTrue interrupted "FilterOrInterrupt should interrupt the fiber when predicate rejects"

            testPropertyWithConfig fsCheckConfig "Reject - partial function matches fails with error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Reject(fun _ -> Some error)).UnsafeError()

                Expect.equal result error "Reject should fail with the matched error"

            testPropertyWithConfig fsCheckConfig "Reject - partial function does not match passes through"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Reject(fun _ -> None)).UnsafeSuccess()

                Expect.equal result value "Reject should pass through when the partial function does not match"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function matches and effect succeeds fails with computed error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.RejectFIO(fun _ -> Some (FIO.succeed error))).UnsafeError()

                Expect.equal result error "RejectFIO should fail with the successful result of the rejection effect"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function matches and effect fails propagates that failure"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.RejectFIO(fun _ -> Some (FIO.fail error))).UnsafeError()

                Expect.equal result error "RejectFIO should propagate failure of the rejection effect as the rejection error"

            testPropertyWithConfig fsCheckConfig "RejectFIO - partial function does not match passes through"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.RejectFIO(fun _ -> None)).UnsafeSuccess()

                Expect.equal result value "RejectFIO should pass through when the partial function does not match"

            testPropertyWithConfig fsCheckConfig "Collect - partial function matches returns extracted value"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Collect error (fun v -> Some (v + 1))).UnsafeSuccess()

                Expect.equal result (value + 1) "Collect should return the extracted value when partial function matches"

            testPropertyWithConfig fsCheckConfig "Collect - partial function does not match fails with supplied error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.Collect error (fun _ -> None)).UnsafeError()

                Expect.equal result error "Collect should fail with the supplied error when partial function does not match"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function matches and effect succeeds returns extracted value"
            <| fun (runtime: FIORuntime, value: int, extracted: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.CollectFIO error (fun _ -> Some (FIO.succeed extracted))).UnsafeSuccess()

                Expect.equal result extracted "CollectFIO should return the value from the extracted effect when partial function matches"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function matches and effect fails propagates that failure"
            <| fun (runtime: FIORuntime, value: int, innerError: string, outerError: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.CollectFIO outerError (fun _ -> Some (FIO.fail innerError))).UnsafeError()

                Expect.equal result innerError "CollectFIO should propagate the inner effect's failure"

            testPropertyWithConfig fsCheckConfig "CollectFIO - partial function does not match fails with supplied error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.CollectFIO error (fun _ -> None)).UnsafeError()

                Expect.equal result error "CollectFIO should fail with the supplied error when partial function does not match"

            // ─── Applicative ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Apply - applies function to value"
            <| fun (runtime: FIORuntime, value: int) ->
                let valEff = FIO.succeed value
                let fnEff = FIO.succeed (fun x -> x * 3)

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeSuccess()

                Expect.equal result (value * 3) "Apply should apply function to value"

            testPropertyWithConfig fsCheckConfig "Apply - propagates value error"
            <| fun (runtime: FIORuntime, error: string) ->
                let valEff = FIO.fail error
                let fnEff = FIO.succeed (fun x -> x * 3)

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeError()

                Expect.equal result error "Apply should propagate value error"

            testPropertyWithConfig fsCheckConfig "Apply - propagates function error"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let valEff = FIO.succeed value
                let fnEff = FIO.fail error

                let result =
                    runtime.Run(valEff.Apply fnEff).UnsafeError()

                Expect.equal result error "Apply should propagate function error"

            testPropertyWithConfig fsCheckConfig "ApplyError - applies error function"
            <| fun (runtime: FIORuntime, error: int) ->
                let errEff = FIO.fail error
                let fnEff = FIO.fail (fun e -> $"Error: {e}")

                let result =
                    runtime.Run(errEff.ApplyError fnEff).UnsafeError()

                Expect.equal result $"Error: {error}" "ApplyError should apply error function"

            testPropertyWithConfig fsCheckConfig "ApplyError - preserves success"
            <| fun (runtime: FIORuntime, value: string) ->
                let succEff = FIO.succeed value
                let fnEff = FIO.fail (fun e -> $"Error: {e}")

                let result =
                    runtime.Run(succEff.ApplyError fnEff).UnsafeSuccess()

                Expect.equal result value "ApplyError should preserve success"

            // ─── Sequential composition (Zip) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Zip - combines two success values into tuple"
            <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                let eff1 = FIO.succeed res1
                let eff2 = FIO.succeed res2

                let result =
                    runtime.Run(eff1.Zip eff2).UnsafeSuccess()

                Expect.equal result (res1, res2) "Zip should combine two success values into tuple"

            testPropertyWithConfig fsCheckConfig "Zip - first fails returns error"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable secondExecuted = false
                let eff1 = FIO.fail error

                let eff2 =
                    FIO.attempt
                        (fun () ->
                            secondExecuted <- true
                            42)
                        (fun ex -> ex.Message)

                let result =
                    runtime.Run(eff1.Zip eff2).UnsafeError()

                Expect.equal result error "Zip should return error when first fails"
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
            <| fun (runtime: FIORuntime, value: int) ->
                let eff1 = FIO.succeed value
                let eff2 = FIO.succeed (value + 1)

                let result =
                    runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()

                Expect.equal result value "ZipParError should return first result when both succeed"

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
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).Fold (fun e -> e) (fun r -> r * 2)

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual (value * 2) "Fold should handle success"

            testPropertyWithConfig fsCheckConfig "Fold - handles error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).Fold (fun e -> e + 100) (fun r -> r * 2)

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual (error + 100) "Fold should handle error"

            testPropertyWithConfig fsCheckConfig "FoldFIO - handles success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    FIO.succeed(value).FoldFIO (fun e -> FIO.succeed e) (fun r -> FIO.succeed (r * 2))

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual (value * 2) "FoldFIO should handle success"

            testPropertyWithConfig fsCheckConfig "FoldFIO - handles error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect =
                    FIO.fail(error).FoldFIO (fun e -> FIO.succeed (e + 100)) (fun r -> FIO.succeed (r * 2))

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual (error + 100) "FoldFIO should handle error"

            testPropertyWithConfig fsCheckConfig "FoldFIO - error handler catches success handler errors"
            <| fun (runtime: FIORuntime, value: int, error: int) ->
                let effect =
                    FIO.succeed(value).FoldFIO (fun e -> FIO.succeed (e * 10)) (fun _ -> FIO.fail error)

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual (error * 10) "FoldFIO should catch success handler errors"

            testPropertyWithConfig fsCheckConfig "OnDone - success branch runs onSuccess and yields unit"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable observedSuccess = 0
                let mutable observedError = 0
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.OnDone
                        (fun _ ->
                            observedError <- observedError + 1
                            FIO.unit ())
                        (fun v ->
                            observedSuccess <- v
                            FIO.unit ()))
                        .UnsafeSuccess()

                Expect.equal result () "OnDone should yield unit"
                Expect.equal observedSuccess value "OnDone should invoke onSuccess with the original value"
                Expect.equal observedError 0 "OnDone should not invoke onError on success"

            testPropertyWithConfig fsCheckConfig "OnDone - error branch runs onError and yields unit"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable observedError = ""
                let mutable observedSuccess = 0
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.OnDone
                        (fun e ->
                            observedError <- e
                            FIO.unit ())
                        (fun _ ->
                            observedSuccess <- observedSuccess + 1
                            FIO.unit ()))
                        .UnsafeSuccess()

                Expect.equal result () "OnDone should yield unit even on original failure"
                Expect.equal observedError error "OnDone should invoke onError with the original error"
                Expect.equal observedSuccess 0 "OnDone should not invoke onSuccess on failure"

            testPropertyWithConfig fsCheckConfig "OnDone - onSuccess failure propagates"
            <| fun (runtime: FIORuntime, value: int, handlerError: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.OnDone
                        (fun _ -> FIO.unit ())
                        (fun _ -> FIO.fail handlerError))
                        .UnsafeError()

                Expect.equal result handlerError "OnDone should propagate failures from onSuccess"

            testPropertyWithConfig fsCheckConfig "OnDone - onError failure propagates"
            <| fun (runtime: FIORuntime, error: string, handlerError: string) ->
                let effect = FIO.fail error

                let result =
                    runtime.Run(effect.OnDone
                        (fun _ -> FIO.fail handlerError)
                        (fun _ -> FIO.unit ()))
                        .UnsafeError()

                Expect.equal result handlerError "OnDone should propagate failures from onError"

            // ─── Retry (iterate on failure) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "RetryOrElse - falls back after max retries"
            <| fun (runtime: FIORuntime, fallbackValue: int) ->
                let effect = FIO.fail("error").RetryOrElse 2 (fun _ -> FIO.succeed fallbackValue) (fun _ -> FIO.unit ())

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result fallbackValue "RetryOrElse should fall back after max retries"

            testPropertyWithConfig fsCheckConfig "RetryOrElse - succeeds without fallback"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).RetryOrElse 3 (fun _ -> FIO.succeed -1) (fun _ -> FIO.unit ())

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "RetryOrElse should return original value on success"

            testPropertyWithConfig fsCheckConfig "RetryOrElse - callback called on each retry"
            <| fun (runtime: FIORuntime, fallbackValue: int) ->
                let mutable callbackCount = 0

                let effect = FIO.fail(0).RetryOrElse
                                3
                                (fun _ -> FIO.succeed fallbackValue)
                                (fun _ ->
                                    callbackCount <- callbackCount + 1
                                    FIO.unit ())

                let _ =
                    runtime.Run(effect).UnsafeResult()

                Expect.equal callbackCount 2 "RetryOrElse callback should be called on each retry"

            testPropertyWithConfig fsCheckConfig "Retry - succeeds immediately without retrying"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable attempts = 0

                let effect = 
                    FIO.attempt 
                        (fun () ->
                            attempts <- attempts + 1
                            value)
                        id

                let retried = effect.Retry 3 (fun _ -> FIO.unit ())

                let actual =
                    runtime.Run(retried).UnsafeSuccess()

                Expect.equal actual value "Retry should succeed"
                Expect.equal attempts 1 "Retry should not retry on immediate success"

            testPropertyWithConfig fsCheckConfig "Retry - retries up to max attempts"
            <| fun (runtime: FIORuntime) ->
                let mutable attempts = 0

                let effect =
                    fio {
                        attempts <- attempts + 1
                        return! FIO.fail "error"
                    }

                let retried = effect.Retry 4 (fun _ -> FIO.unit ())

                let _ =
                    runtime.Run(retried).UnsafeResult()

                Expect.equal attempts 4 "Retry should retry up to max"

            testPropertyWithConfig fsCheckConfig "Retry - succeeds on intermediate attempt"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable attempts = 0

                let effect =
                    fio {
                        attempts <- attempts + 1
                        if attempts < 3 then
                            return! FIO.fail "error"
                        else
                            return value
                    }

                let retried = effect.Retry 5 (fun _ -> FIO.unit ())

                let actual =
                    runtime.Run(retried).UnsafeSuccess()

                Expect.equal actual value "Retry should succeed on third attempt"
                Expect.equal attempts 3 "Should take 3 attempts"

            testPropertyWithConfig fsCheckConfig "Retry - callback receives correct attempt numbers"
            <| fun (runtime: FIORuntime) ->
                let mutable attempts = []

                let effect =
                    FIO.fail("error").Retry
                        3
                        (fun (_, attempt, max) ->
                            attempts <- attempts @ [ (attempt, max) ]
                            FIO.unit ())

                let _ =
                    runtime.Run(effect).UnsafeResult()

                Expect.equal attempts [ 1, 3; 2, 3 ] "Retry callback should receive correct attempt numbers"

            testPropertyWithConfig fsCheckConfig "RetryUntil - stops when predicate matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(effect.RetryUntil(fun error -> error >= 3)).UnsafeError()

                Expect.equal result 3 "RetryUntil should fail with the error that matched the predicate"
                Expect.equal count 3 "RetryUntil should retry until predicate matches"

            testPropertyWithConfig fsCheckConfig "RetryUntil - fails immediately when predicate true on first error"
            <| fun (runtime: FIORuntime, errValue: int) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail errValue
                    }

                let result =
                    runtime.Run(effect.RetryUntil(fun _ -> true)).UnsafeError()

                Expect.equal count 1 "RetryUntil should fail without retrying"
                Expect.equal result errValue "RetryUntil should fail with the original error"

            testCase "RetryUntil - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(effect.RetryUntil(fun error -> error >= 10_000)).UnsafeError()

                Expect.equal count 10_000 "RetryUntil should retry 10000 times"
                Expect.equal result 10_000 "RetryUntil should fail with the final error"

            testPropertyWithConfig fsCheckConfig "RetryUntilEquals - stops when error matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(effect.RetryUntilEquals 4).UnsafeError()

                Expect.equal count 4 "RetryUntilEquals should retry until equality"
                Expect.equal result 4 "RetryUntilEquals should fail with the matched error"

            testPropertyWithConfig fsCheckConfig "RetryUntilFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail (exn (string count))
                    }

                let predicate (_: exn) =
                    if count >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed false

                let result =
                    runtime.Run(effect.RetryUntilFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RetryUntilFIO should propagate predicate error"
                Expect.equal count 2 "RetryUntilFIO should stop on predicate failure"

            testPropertyWithConfig fsCheckConfig "RetryWhile - stops when predicate becomes false"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(effect.RetryWhile(fun error -> error < 5)).UnsafeError()

                Expect.equal count 5 "RetryWhile should retry until predicate is false"
                Expect.equal result 5 "RetryWhile should fail with the error that failed the predicate"

            testCase "RetryWhile - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail count
                    }

                let result =
                    runtime.Run(effect.RetryWhile(fun error -> error < 10_000)).UnsafeError()

                Expect.equal count 10_000 "RetryWhile should retry 10000 times"
                Expect.equal result 10_000 "RetryWhile should fail with the final error"

            testPropertyWithConfig fsCheckConfig "RetryWhileFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        return! FIO.fail (exn (string count))
                    }

                let predicate (_: exn) =
                    if count >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed true

                let result =
                    runtime.Run(effect.RetryWhileFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RetryWhileFIO should propagate predicate error"
                Expect.equal count 2 "RetryWhileFIO should stop on predicate failure"

            testAllRuntimes "Eventually - succeeds after N failures" (fun runtime ->
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        if count < 5 then
                            return! FIO.fail (exn "transient")
                        else
                            return 42
                    }

                let result =
                    runtime.Run(effect.Eventually()).UnsafeSuccess()

                Expect.equal result 42 "Eventually should return the first success value"
                Expect.equal count 5 "Eventually should retry until success")

            testCase "Eventually - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let effect =
                    fio {
                        count <- count + 1
                        if count < 10_000 then
                            return! FIO.fail (exn "transient")
                        else
                            return count
                    }

                let result =
                    runtime.Run(effect.Eventually()).UnsafeSuccess()

                Expect.equal result 10_000 "Eventually should return the final success value"
                Expect.equal count 10_000 "Eventually should retry 10000 times"

            testPropertyWithConfig fsCheckConfig "RepeatN - repeats N times, returns last result"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt 
                        (fun () ->
                            count <- count + 1
                            count)
                        id
                    
                let result =
                    runtime.Run(effect.RepeatN 5).UnsafeSuccess()

                Expect.equal count 5 "RepeatN should execute 5 times"
                Expect.equal result 5 "RepeatN should return last result"

            testPropertyWithConfig fsCheckConfig "RepeatN - n=1 executes exactly once"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            value)
                        id
                    
                let result =
                    runtime.Run(effect.RepeatN 1).UnsafeSuccess()

                Expect.equal count 1 "RepeatN(1) should execute exactly once"
                Expect.equal result value "RepeatN(1) should return the result"

            testPropertyWithConfig fsCheckConfig "RepeatUntil - stops on first satisfying value"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id
                    
                let result =
                    runtime.Run(effect.RepeatUntil(fun r -> r >= 3)).UnsafeSuccess()

                Expect.equal count 3 "RepeatUntil should execute until predicate is satisfied"
                Expect.equal result 3 "RepeatUntil should return the value that satisfied the predicate"

            testPropertyWithConfig fsCheckConfig "RepeatUntil - executes once when predicate true on first try"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            value)
                        id

                let result =
                    runtime.Run(effect.RepeatUntil(fun _ -> true)).UnsafeSuccess()

                Expect.equal count 1 "RepeatUntil should execute exactly once"
                Expect.equal result value "RepeatUntil should return the first result"

            testCase "RepeatUntil - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(effect.RepeatUntil(fun r -> r >= 10_000)).UnsafeSuccess()

                Expect.equal count 10_000 "RepeatUntil should execute 10000 times"
                Expect.equal result 10_000 "RepeatUntil should return the final value"

            testPropertyWithConfig fsCheckConfig "RepeatUntilEquals - stops when value matches"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(effect.RepeatUntilEquals 4).UnsafeSuccess()

                Expect.equal count 4 "RepeatUntilEquals should execute until equality"
                Expect.equal result 4 "RepeatUntilEquals should return the matched value"

            testPropertyWithConfig fsCheckConfig "RepeatUntilFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let predicate r =
                    if r >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed false

                let result =
                    runtime.Run(effect.RepeatUntilFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RepeatUntilFIO should propagate predicate error"
                Expect.equal count 2 "RepeatUntilFIO should stop on predicate failure"

            testPropertyWithConfig fsCheckConfig "RepeatWhile - stops when predicate becomes false"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(effect.RepeatWhile(fun r -> r < 5)).UnsafeSuccess()

                Expect.equal count 5 "RepeatWhile should execute until predicate is false"
                Expect.equal result 5 "RepeatWhile should return the value that failed the predicate"

            testCase "RepeatWhile - stack safety with 10000 iterations" <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let result =
                    runtime.Run(effect.RepeatWhile(fun r -> r < 10_000)).UnsafeSuccess()

                Expect.equal count 10_000 "RepeatWhile should execute 10000 times"
                Expect.equal result 10_000 "RepeatWhile should return the final value"

            testPropertyWithConfig fsCheckConfig "RepeatWhileFIO - predicate error propagates"
            <| fun (runtime: FIORuntime) ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            count <- count + 1
                            count)
                        id

                let predicate r =
                    if r >= 2 then FIO.fail (exn "pred-error")
                    else FIO.succeed true

                let result =
                    runtime.Run(effect.RepeatWhileFIO predicate).UnsafeError()

                Expect.equal result.Message "pred-error" "RepeatWhileFIO should propagate predicate error"
                Expect.equal count 2 "RepeatWhileFIO should stop on predicate failure"
    
            testAllRuntimes "Forever - loops until interrupted by Timeout" (fun runtime ->
                let mutable count = 0

                let effect =
                    FIO.attempt
                        (fun () -> count <- count + 1)
                        id

                let bounded = effect.Forever().Timeout (TimeSpan.FromMilliseconds 100.0) id

                let result =
                    runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result None "Forever should not produce a value before timeout"
                Expect.isGreaterThan count 0 "Forever should have run at least once before timeout")

            testAllRuntimes "Delay - returns the underlying result after sleeping" (fun runtime ->
                let mutable ran = false

                let effect =
                    FIO.attempt(
                        fun () ->
                            ran <- true
                            7)
                        id

                let sw = Stopwatch.StartNew()

                let result =
                    runtime.Run(effect.Delay (TimeSpan.FromMilliseconds 50.0) id).UnsafeSuccess()
                
                sw.Stop()

                Expect.equal result 7 "Delay should return the underlying effect's result"
                Expect.isTrue ran "Delay should run the underlying effect after sleeping"
                Expect.isGreaterThanOrEqual sw.Elapsed (TimeSpan.FromMilliseconds 40.0) "Delay should sleep for at least most of the requested duration")

            testAllRuntimes "Delay - propagates underlying effect's failure" (fun runtime ->
                let effect = FIO.fail (exn "boom")

                let result =
                    runtime.Run(effect.Delay (TimeSpan.FromMilliseconds 10.0) id).UnsafeError()

                Expect.equal result.Message "boom" "Delay should propagate the underlying error after sleeping")

            testAllRuntimes "Timeout - returns Some on fast effect" (fun runtime ->
                let effect = FIO.succeed(42).Timeout (TimeSpan.FromSeconds 5.0) id

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (Some 42) "Timeout should return Some for fast effect")

            testAllRuntimes "Timeout - returns None on slow effect" (fun runtime ->
                let slowEff =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) id)
                        .FlatMap(fun () -> FIO.succeed 42)

                let effect = slowEff.Timeout (TimeSpan.FromMilliseconds 50.0) id

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result None "Timeout should return None for slow effect")

            testPropertyWithConfig fsCheckConfig "TimeoutFail - effect completes in time returns success"
            <| fun (runtime: FIORuntime, value: int, timeoutError: string) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.TimeoutFail timeoutError (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result value "TimeoutFail should return success when effect completes in time"

            testPropertyWithConfig fsCheckConfig "TimeoutTo - effect completes in time applies onSuccess"
            <| fun (runtime: FIORuntime, value: int, defaultValue: int) ->
                let effect = FIO.succeed value

                let result =
                    runtime.Run(effect.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result (value * 2) "TimeoutTo should apply onSuccess when effect completes in time"

            testCase "TimeoutTo - timeout fires returns default value"
            <| fun () ->
                let runtime = new ConcurrentRuntime() :> FIORuntime
                let defaultValue = -1
                let effect =
                    (FIO.sleep (TimeSpan.FromSeconds 5.0) (fun ex -> ex.Message)).FlatMap(fun () -> FIO.succeed 0)

                let result =
                    runtime.Run(effect.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromMilliseconds 50.0) (fun ex -> ex.Message)).UnsafeSuccess()

                Expect.equal result defaultValue "TimeoutTo should return the default value when timeout fires"

            testPropertyWithConfig fsCheckConfig "Timed - returns duration and result"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed value

                let duration, result = runtime.Run(effect.Timed id).UnsafeSuccess()

                Expect.equal result value "Timed should return the result"
                Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Timed duration should be >= 0"

            testAllRuntimes "RaceFirst - returns first completing effect" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let effect = fast.RaceFirst slow

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1 "RaceFirst should return first completing effect")

            testAllRuntimes "RaceFirst - propagates error from first completing" (fun runtime ->
                let error = exn "fast error"
                let fast = FIO.fail error
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let effect = fast.RaceFirst slow

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result.Message error.Message "RaceFirst should propagate error from first completing")

            testAllRuntimes "RaceEither - first racer wins returns Choice1Of2" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed "slow")
                let effect = fast.RaceEither slow

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (Choice1Of2 1) "RaceEither should return Choice1Of2 when this wins")

            testAllRuntimes "RaceEither - second racer wins returns Choice2Of2" (fun runtime ->
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 1)
                let fast = FIO.succeed "fast"
                let effect = slow.RaceEither fast

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (Choice2Of2 "fast") "RaceEither should return Choice2Of2 when the right racer wins")

            testAllRuntimes "RaceEither - fast failure waits for a slow success" (fun runtime ->
                let fastFail: FIO<int, exn> = FIO.fail (exn "fast error")
                let slowSucceed =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.succeed "slow")
                let effect = fastFail.RaceEither slowSucceed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (Choice2Of2 "slow") "RaceEither should wait for a peer success when the first racer fails")
                
            testAllRuntimes "Race - both succeed, fastest wins" (fun runtime ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let effect = fast.Race slow

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1 "Race should return the fastest successful value")

            testAllRuntimes "Race - fast failure waits for slow success" (fun runtime ->
                let fastFail = FIO.fail (exn "fast error")
                let slowSucceed =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.succeed 7)
                let effect = fastFail.Race slowSucceed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 7 "Race should wait for a peer when the first racer fails")

            testAllRuntimes "Race - slow failure does not interrupt fast success" (fun runtime ->
                let fastSucceed = FIO.succeed 11
                let slowFail =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.fail (exn "slow"))
                let effect = fastSucceed.Race slowFail

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 11 "Race should return the fast success without waiting for slow failure")

            testAllRuntimes "Race - both fail returns the later error" (fun runtime ->
                let fastFail = FIO.fail (exn "fast error")
                let slowFail =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.fail (exn "slow error"))
                let effect = fastFail.Race slowFail

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result.Message "slow error" "Race should fail with the most-recently-received error when both racers fail")

            testPropertyWithConfig fsCheckConfig "FlatMap - chains success values"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).FlatMap(fun x -> FIO.succeed (x + 1))

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (value + 1) "FlatMap should chain success values"

            testPropertyWithConfig fsCheckConfig "FlatMap - short-circuits on error"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable secondExecuted = false

                let effect =
                    FIO.fail(error)
                        .FlatMap(fun _ ->
                            secondExecuted <- true
                            FIO.succeed 42)

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result error "FlatMap should return error"
                Expect.isFalse secondExecuted "FlatMap should short-circuit on error"

            testPropertyWithConfig fsCheckConfig "CatchAll - recovers from error"
            <| fun (runtime: FIORuntime, error: int, recovery: int) ->
                let effect = FIO.fail(error).CatchAll(fun _ -> FIO.succeed recovery)

                let actual =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual recovery "CatchAll should recover"

            testPropertyWithConfig fsCheckConfig "CatchAll - does not affect success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.succeed(value).CatchAll(fun _ -> FIO.succeed 0)

                let actual =    
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal actual value "CatchAll should not affect success"

            testPropertyWithConfig fsCheckConfig "CatchAll - can transform error to new error"
            <| fun (runtime: FIORuntime, error: int) ->
                let effect = FIO.fail(error).CatchAll(fun e -> FIO.fail (e + 100))

                let actual =
                    runtime.Run(effect).UnsafeError()

                Expect.equal actual (error + 100) "CatchAll can transform to new error"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer runs on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable finalizerRan = false
                let effect = FIO.succeed value
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect.Ensuring finalizer).UnsafeSuccess()

                Expect.isTrue finalizerRan "Ensuring finalizer should run on success"
                Expect.equal result value "Ensuring should preserve the success result"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer runs on failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable finalizerRan = false
                let effect = FIO.fail error
                let finalizer = FIO.attempt (fun () -> finalizerRan <- true) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect.Ensuring finalizer).UnsafeError()

                Expect.isTrue finalizerRan "Ensuring finalizer should run on failure"
                Expect.equal result error "Ensuring should preserve the error"

            testPropertyWithConfig fsCheckConfig "Ensuring - finalizer error on success propagates"
            <| fun (runtime: FIORuntime, value: int, finalizerErr: string) ->
                let effect = FIO.succeed value
                let finalizer = FIO.fail finalizerErr

                let result =
                    runtime.Run(effect.Ensuring finalizer).UnsafeError()

                Expect.equal result finalizerErr "Ensuring should propagate finalizer error when main effect succeeds"

            testPropertyWithConfig fsCheckConfig "Ensuring - main error preserved when finalizer also fails"
            <| fun (runtime: FIORuntime, mainErr: string, finalizerErr: string) ->
                let effect = FIO.fail mainErr
                let finalizer = FIO.fail finalizerErr

                let result =
                    runtime.Run(effect.Ensuring finalizer).UnsafeError()

                Expect.equal result mainErr "Ensuring should preserve main error, suppressing finalizer error"
        ]
