module FIO.Tests.Factories.BranchingTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Threading

let private stressTestAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in allRuntimes () -> stressTestCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Branching"
                [
                    testPropertyWithConfig fsCheckConfig "ifFIO - true predicate runs onTrue branch"
                    <| fun (runtime: FIORuntime, x: int, y: int) ->
                        let effect = FIO.ifFIO (FIO.succeed true) (FIO.succeed x) (FIO.succeed y)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result x "ifFIO with true predicate should yield onTrue's result"

                    testPropertyWithConfig fsCheckConfig "ifFIO - false predicate runs onFalse branch"
                    <| fun (runtime: FIORuntime, x: int, y: int) ->
                        let effect = FIO.ifFIO (FIO.succeed false) (FIO.succeed x) (FIO.succeed y)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result y "ifFIO with false predicate should yield onFalse's result"

                    testPropertyWithConfig fsCheckConfig "ifFIO - predicate failure propagates without running either branch"
                    <| fun (runtime: FIORuntime) ->
                        let mutable branchEvaluated = 0
                        let mk =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&branchEvaluated) |> ignore
                                    0)
                                (fun ex -> ex.Message)
                        let effect = FIO.ifFIO (FIO.fail "boom") mk mk

                        let error =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal error "boom" "ifFIO should propagate predicate failure"
                        Expect.equal branchEvaluated 0 "ifFIO should not evaluate either branch on predicate failure"

                    testPropertyWithConfig fsCheckConfig "ifFIO - only the selected branch is evaluated"
                    <| fun (runtime: FIORuntime, pick: bool) ->
                        let mutable trueRan = 0
                        let mutable falseRan = 0
                        let onTrue =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&trueRan) |> ignore
                                    1)
                                (fun ex -> ex.Message)
                        let onFalse =
                            FIO.attempt
                                (fun () ->
                                    Interlocked.Increment(&falseRan) |> ignore
                                    2)
                                (fun ex -> ex.Message)

                        let _ =
                            runtime.Run(FIO.ifFIO (FIO.succeed pick) onTrue onFalse).UnsafeSuccess()

                        if pick then
                            Expect.equal trueRan 1 "onTrue should run when predicate is true"
                            Expect.equal falseRan 0 "onFalse should NOT run when predicate is true"
                        else
                            Expect.equal trueRan 0 "onTrue should NOT run when predicate is false"
                            Expect.equal falseRan 1 "onFalse should run when predicate is false"

                    testPropertyWithConfig fsCheckConfig "ifFIO - failure in selected branch propagates"
                    <| fun (runtime: FIORuntime, pick: bool) ->
                        let other = FIO.succeed 0
                        let failing = FIO.fail "branch boom"
                        let effect =
                            if pick then FIO.ifFIO (FIO.succeed true) failing other
                            else FIO.ifFIO (FIO.succeed false) other failing

                        let error =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal error "branch boom" "ifFIO should propagate failure from the selected branch"
                ]

            testList
                "Option unwrapping"
                [
                    testPropertyWithConfig fsCheckConfig "someOrFail - Some unwraps to success"
                    <| fun (runtime: FIORuntime, value: int, error: string) ->
                        let effect = FIO.succeed (Some value)

                        let result =
                            runtime.Run(effect |> FIO.someOrFail error).UnsafeSuccess()

                        Expect.equal result value "someOrFail should unwrap Some to the underlying value"

                    testPropertyWithConfig fsCheckConfig "someOrFail - None fails with supplied error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.succeed None

                        let result =
                            runtime.Run(effect |> FIO.someOrFail error).UnsafeError()

                        Expect.equal result error "someOrFail should fail with the supplied error on None"

                    testPropertyWithConfig fsCheckConfig "someOrFail - original failure propagates"
                    <| fun (runtime: FIORuntime, originalError: string, replacement: string) ->
                        let effect = FIO.fail originalError

                        let result =
                            runtime.Run(effect |> FIO.someOrFail replacement).UnsafeError()

                        Expect.equal result originalError "someOrFail should propagate the original failure unchanged"

                    testPropertyWithConfig fsCheckConfig "someOrElse - Some unwraps to success"
                    <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                        let effect = FIO.succeed (Some value)

                        let result =
                            runtime.Run(effect |> FIO.someOrElse fallback).UnsafeSuccess()

                        Expect.equal result value "someOrElse should unwrap Some to the underlying value"

                    testPropertyWithConfig fsCheckConfig "someOrElse - None substitutes the default"
                    <| fun (runtime: FIORuntime, fallback: int) ->
                        let effect = FIO.succeed None

                        let result =
                            runtime.Run(effect |> FIO.someOrElse fallback).UnsafeSuccess()

                        Expect.equal result fallback "someOrElse should substitute the default value on None"

                    testPropertyWithConfig fsCheckConfig "someOrElseFIO - Some unwraps to success without evaluating the fallback"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable evaluated = 0
                        let fallback =
                            FIO.attempt
                                (fun () ->
                                    evaluated <- evaluated + 1
                                    -1)
                                (fun ex -> ex.Message)
                        let effect = FIO.succeed (Some value)

                        let result =
                            runtime.Run(effect |> FIO.someOrElseFIO fallback).UnsafeSuccess()

                        Expect.equal result value "someOrElseFIO should unwrap Some to the underlying value"
                        Expect.equal evaluated 0 "someOrElseFIO should not evaluate the fallback when Some"

                    testPropertyWithConfig fsCheckConfig "someOrElseFIO - None runs the fallback effect"
                    <| fun (runtime: FIORuntime, fallbackValue: int) ->
                        let effect = FIO.succeed None

                        let result =
                            runtime.Run(effect |> FIO.someOrElseFIO (FIO.succeed fallbackValue)).UnsafeSuccess()

                        Expect.equal result fallbackValue "someOrElseFIO should evaluate the fallback effect on None"

                    testPropertyWithConfig fsCheckConfig "someOrElseFIO - None propagates fallback failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.succeed None

                        let result =
                            runtime.Run(effect |> FIO.someOrElseFIO (FIO.fail error)).UnsafeError()

                        Expect.equal result error "someOrElseFIO should propagate the fallback's failure on None"
                ]

            testList
                "First-success alternatives"
                [
                    testPropertyWithConfig fsCheckConfig "firstSuccessOf - empty tail returns head's outcome on success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let head = FIO.succeed value

                        let result =
                            runtime.Run(FIO.firstSuccessOf head Seq.empty).UnsafeSuccess()

                        Expect.equal result value "firstSuccessOf with empty tail should yield head's success"

                    testPropertyWithConfig fsCheckConfig "firstSuccessOf - empty tail propagates head's failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let head = FIO.fail error

                        let result =
                            runtime.Run(FIO.firstSuccessOf head Seq.empty).UnsafeError()

                        Expect.equal result error "firstSuccessOf with empty tail should propagate head's failure"

                    testPropertyWithConfig fsCheckConfig "firstSuccessOf - head succeeds returns head"
                    <| fun (runtime: FIORuntime, value: int, tailValue: int) ->
                        let head = FIO.succeed value
                        let tail = seq { FIO.succeed tailValue; FIO.succeed (tailValue + 1) }

                        let result =
                            runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                        Expect.equal result value "firstSuccessOf should return head when it succeeds"

                    testPropertyWithConfig fsCheckConfig "firstSuccessOf - first successful tail entry wins"
                    <| fun (runtime: FIORuntime, winner: int) ->
                        let head = FIO.fail "h"
                        let tail = seq {
                            FIO.fail "t0"
                            FIO.succeed winner
                            FIO.succeed (winner + 1)
                        }

                        let result =
                            runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                        Expect.equal result winner "firstSuccessOf should return the first successful tail entry"

                    testPropertyWithConfig fsCheckConfig "firstSuccessOf - all fail returns last failure"
                    <| fun (runtime: FIORuntime, lastError: string) ->
                        let head = FIO.fail "h"
                        let tail = seq {
                            FIO.fail "t0"
                            FIO.fail "t1"
                            FIO.fail lastError
                        }

                        let result =
                            runtime.Run(FIO.firstSuccessOf head tail).UnsafeError()

                        Expect.equal result lastError "firstSuccessOf should return the last failure when all fail"

                    testCase "firstSuccessOf - only evaluates effects up to the first success"
                    <| fun () ->
                        let runtime = new WorkStealingRuntime() :> FIORuntime
                        let mutable evaluated = 0
                        let bump effect =
                            FIO.suspend (fun () ->
                                evaluated <- evaluated + 1
                                effect)
                        let head = bump (FIO.fail "h")
                        let tail = seq {
                            bump (FIO.fail "t0")
                            bump (FIO.succeed 42)
                            bump (FIO.succeed 99)
                        }

                        let result =
                            runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                        Expect.equal result 42 "firstSuccessOf should yield the first success"
                        Expect.equal evaluated 3 "firstSuccessOf should stop evaluating once an effect succeeds"

                    testPropertyWithConfig fsCheckConfig "raceAll - empty sequence interrupts with InvalidArgument"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.raceAll Seq.empty

                        let fiber = runtime.Run effect
                        let fiberResult =
                            fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match fiberResult with
                        | Interrupted ex ->
                            Expect.equal
                                ex.cause
                                (InvalidArgument("effects", "sequence must not be empty"))
                                "raceAll should interrupt with InvalidArgument for an empty sequence"
                        | _ -> failtest "raceAll over empty sequence should result in Interrupted"

                    testAllRuntimes "raceAll - single-element sequence passes through success" (fun runtime ->
                        let effect = FIO.raceAll (seq { FIO.succeed 42 })

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 42 "raceAll over a single success should yield that value")

                    testAllRuntimes "raceAll - single-element sequence propagates failure" (fun runtime ->
                        let error = exn "single failure"
                        let effect = FIO.raceAll (seq { FIO.fail error })

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result.Message error.Message "raceAll over a single failure should propagate that error")

                    testAllRuntimes "raceAll - fastest success wins" (fun runtime ->
                        let fast = FIO.succeed 1
                        let medium =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 2)
                        let slow =
                            (FIO.sleep (TimeSpan.FromSeconds 20.0)).FlatMap(fun () -> FIO.succeed 3)
                        let effect = FIO.raceAll (seq { fast; medium; slow })

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 1 "raceAll should return the fastest successful value")

                    testAllRuntimes "raceAll - failures retire racers without winning" (fun runtime ->
                        let fastFail1 = FIO.fail (exn "fast 1")
                        let fastFail2 = FIO.fail (exn "fast 2")
                        let slowSucceed =
                            (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).FlatMap(fun () -> FIO.succeed 99)
                        let effect = FIO.raceAll (seq { fastFail1; fastFail2; slowSucceed })

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 99 "raceAll should wait for a success even when other racers fail fast")

                    testAllRuntimes "raceAll - all fail surfaces one of the racers' errors" (fun runtime ->
                        let fastFail = FIO.fail (exn "fast")
                        let mediumFail =
                            (FIO.sleep (TimeSpan.FromMilliseconds 30.0)).FlatMap(fun () -> FIO.fail (exn "medium"))
                        let slowFail =
                            (FIO.sleep (TimeSpan.FromMilliseconds 80.0)).FlatMap(fun () -> FIO.fail (exn "slow"))
                        let effect = FIO.raceAll (seq { fastFail; mediumFail; slowFail })

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.contains
                            [ "fast"; "medium"; "slow" ]
                            result.Message
                            "raceAll should fail with one of the racers' error messages when all racers fail")

                    testAllRuntimes "raceAll - interrupted racers retire without breaking the race" (fun runtime ->
                        let interrupted = FIO.interruptNow<int, exn> ()
                        let failing =
                            (FIO.sleep (TimeSpan.FromMilliseconds 30.0)).FlatMap(fun () -> FIO.fail (exn "failing"))
                        let succeeding =
                            (FIO.sleep (TimeSpan.FromMilliseconds 60.0)).FlatMap(fun () -> FIO.succeed 7)
                        let effect = FIO.raceAll (seq { interrupted; failing; succeeding })
                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result 7 "raceAll should still yield the success after other racers were interrupted or failed")

                    testAllRuntimes "raceAll - terminates when the remaining racers fail after one is interrupted" (fun runtime ->
                        let interrupted = FIO.interruptNow<int, exn> ()
                        let fail1 =
                            (FIO.sleep (TimeSpan.FromMilliseconds 30.0)).FlatMap(fun () -> FIO.fail (exn "first"))
                        let fail2 =
                            (FIO.sleep (TimeSpan.FromMilliseconds 60.0)).FlatMap(fun () -> FIO.fail (exn "last"))
                        let effect = FIO.raceAll (seq { interrupted; fail1; fail2 })
                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeError()

                        Expect.contains
                            [ "first"; "last" ]
                            result.Message
                            "raceAll must terminate with a racer error, not hang, when an interrupted racer leaves the race")

                    testAllRuntimes "raceAll - every racer interrupted yields Interrupted" (fun runtime ->
                        let effect =
                            FIO.raceAll (seq {
                                FIO.interruptNow<int, exn> ()
                                (FIO.sleep (TimeSpan.FromMilliseconds 30.0)).FlatMap(fun () -> FIO.interruptNow<int, exn> ())
                            })
                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeResult()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"raceAll should propagate interruption when every racer is interrupted, got: %A{other}")

                    testAllRuntimes "raceAll - a stuck racer does not block a later success" (fun runtime ->
                        let stuck = FIO.never<int, exn> ()
                        let succeeding =
                            (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).FlatMap(fun () -> FIO.succeed 3)
                        let effect = FIO.raceAll (seq { stuck; succeeding })
                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result 3 "raceAll should yield the success and interrupt the stuck racer")

                    stressTestAllRuntimes "raceAll - stress: repeated re-parks over surviving racers" (fun runtime ->
                        let iterations = 500
                        let rec loop i =
                            if i = 0 then
                                FIO.unit ()
                            else
                                let round =
                                    FIO.raceAll (seq {
                                        FIO.fail (exn "retired")
                                        FIO.interruptNow<int, exn> ()
                                        (FIO.sleep (TimeSpan.FromMilliseconds 1.0)).FlatMap(fun () -> FIO.succeed i)
                                    })
                                round.FlatMap <| fun value ->
                                    if value = i then loop (i - 1) else FIO.fail (exn $"wrong winner in round {i}")
                        let bounded =
                            (loop iterations).TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 120.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result () "every raceAll round should settle on the surviving success")
                ]
        ]
