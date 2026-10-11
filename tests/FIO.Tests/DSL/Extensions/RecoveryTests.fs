module FIO.Tests.Extensions.RecoveryTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Recovery / fallback"
                [
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
                        | Interrupted ex ->
                            Expect.equal ex.cause ExplicitInterrupt "OrInterrupt should interrupt with ExplicitInterrupt"
                            Expect.stringContains ex.message "Interrupted: error" "OrInterrupt should carry the derived message"
                        | _ -> failtest "OrInterrupt should convert error to interrupt"
                ]

            testList
                "Filter / partial functions"
                [
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
                        let result = outcome.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match result with
                        | Interrupted ex -> Expect.equal ex.cause ExplicitInterrupt "FilterOrInterrupt should interrupt with ExplicitInterrupt"
                        | _ -> ()
                        Expect.isTrue result.IsInterrupted "FilterOrInterrupt should interrupt the fiber when predicate rejects"

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
                ]
        ]
