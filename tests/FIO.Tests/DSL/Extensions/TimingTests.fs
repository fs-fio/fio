module FIO.Tests.Extensions.TimingTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Diagnostics

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Timing"
                [
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
                            runtime.Run(effect.Delay (TimeSpan.FromMilliseconds 50.0)).UnsafeSuccess()

                        sw.Stop()

                        Expect.equal result 7 "Delay should return the underlying effect's result"
                        Expect.isTrue ran "Delay should run the underlying effect after sleeping"
                        Expect.isGreaterThanOrEqual sw.Elapsed (TimeSpan.FromMilliseconds 40.0) "Delay should sleep for at least most of the requested duration")

                    testAllRuntimes "Delay - propagates underlying effect's failure" (fun runtime ->
                        let effect = FIO.fail (exn "boom")

                        let result =
                            runtime.Run(effect.Delay (TimeSpan.FromMilliseconds 10.0)).UnsafeError()

                        Expect.equal result.Message "boom" "Delay should propagate the underlying error after sleeping")

                    testAllRuntimes "Timeout - returns Some on fast effect" (fun runtime ->
                        let effect = FIO.succeed(42).Timeout (TimeSpan.FromSeconds 5.0)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (Some 42) "Timeout should return Some for fast effect")

                    testAllRuntimes "Timeout - returns None on slow effect" (fun runtime ->
                        let slowEff =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0))
                                .FlatMap(fun () -> FIO.succeed 42)

                        let effect = slowEff.Timeout (TimeSpan.FromMilliseconds 50.0)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result None "Timeout should return None for slow effect")

                    testPropertyWithConfig fsCheckConfig "TimeoutFail - effect completes in time returns success"
                    <| fun (runtime: FIORuntime, value: int, timeoutError: string) ->
                        let effect = FIO.succeed value

                        let result =
                            runtime.Run(effect.TimeoutFail timeoutError (TimeSpan.FromSeconds 5.0)).UnsafeSuccess()

                        Expect.equal result value "TimeoutFail should return success when effect completes in time"

                    testPropertyWithConfig fsCheckConfig "TimeoutTo - effect completes in time applies onSuccess"
                    <| fun (runtime: FIORuntime, value: int, defaultValue: int) ->
                        let effect = FIO.succeed value

                        let result =
                            runtime.Run(effect.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromSeconds 5.0)).UnsafeSuccess()

                        Expect.equal result (value * 2) "TimeoutTo should apply onSuccess when effect completes in time"

                    testCase "TimeoutTo - timeout fires returns default value"
                    <| fun () ->
                        let runtime = new WorkStealingRuntime() :> FIORuntime
                        let defaultValue = -1
                        let effect =
                            (FIO.sleep (TimeSpan.FromSeconds 5.0)).FlatMap(fun () -> FIO.succeed 0)

                        let result =
                            runtime.Run(effect.TimeoutTo defaultValue (fun v -> v * 2) (TimeSpan.FromMilliseconds 50.0)).UnsafeSuccess()

                        Expect.equal result defaultValue "TimeoutTo should return the default value when timeout fires"

                    testPropertyWithConfig fsCheckConfig "Timed - returns duration and result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed value

                        let duration, result = runtime.Run(effect.Timed ()).UnsafeSuccess()

                        Expect.equal result value "Timed should return the result"
                        Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Timed duration should be >= 0"
                ]
        ]
