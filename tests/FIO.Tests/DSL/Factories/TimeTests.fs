module FIO.Tests.Factories.TimeTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.Threading
open System.Diagnostics

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Time / scheduling"
                [
                    testPropertyWithConfig fsCheckConfig "sleep - delays execution"
                    <| fun (runtime: FIORuntime) ->
                        let duration = TimeSpan.FromMilliseconds 20.0
                        let effect =
                            fio {
                                let sw = Stopwatch.StartNew()
                                do! FIO.sleep duration
                                sw.Stop()
                                return sw.Elapsed
                            }

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isGreaterThanOrEqual result.TotalMilliseconds 15.0 "FIO.sleep should delay execution"

                    testAllRuntimes "sleep - interruption stops the underlying delay" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = (FIO.sleep (TimeSpan.FromMinutes 1.0)).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                return! fiber.InterruptAwaitNow ()
                            }
                        let sw = Stopwatch.StartNew()

                        let result = runtime.Run(effect).UnsafeSuccess()
                        sw.Stop()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtestf "Expected Interrupted, got %A" other
                        Expect.isLessThan
                            sw.Elapsed.TotalSeconds
                            5.0
                            "sleep should cancel the underlying Task.Delay on fiber interrupt, not tick for the full minute")

                    testAllRuntimes "sleep - is polymorphic in the error type and cannot fail" (fun runtime ->
                        let effect: FIO<string, string> =
                            (FIO.sleep (TimeSpan.FromMilliseconds 1.0)).FlatMap(fun () -> FIO.succeed "slept")

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "slept" "sleep should succeed at any 'E")

                    testAllRuntimes "sleep - a negative duration is an invalid argument" (fun runtime ->
                        let effect: FIO<unit, string> = FIO.sleep (TimeSpan.FromSeconds -1.0)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | InvalidArgument("duration", _) -> ()
                            | other -> failtest $"Expected an InvalidArgument cause for duration but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "sleep - a duration beyond the timer maximum is an invalid argument" (fun runtime ->
                        let effect: FIO<unit, string> = FIO.sleep (TimeSpan.FromDays 50.0)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | InvalidArgument("duration", _) -> ()
                            | other -> failtest $"Expected an InvalidArgument cause for duration but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "sleep - the infinite timeout sleeps until interrupted" (fun runtime ->
                        let effect: FIO<FiberResult<unit, string>, string> =
                            fio {
                                let! fiber = (FIO.sleep Timeout.InfiniteTimeSpan).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 20.0)
                                return! fiber.InterruptAwaitNow ()
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testPropertyWithConfig fsCheckConfig "yieldNow - completes successfully"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.yieldNow ()

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "yieldNow should complete with unit"

                    testPropertyWithConfig fsCheckConfig "yieldNow - sequences correctly with subsequent effects"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect =
                            (FIO.yieldNow ()).FlatMap(fun () -> FIO.succeed value)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "yieldNow should sequence into subsequent effects"

                    testAllRuntimes "never - can be raced against completing effect" (fun runtime ->
                        let value = 42
                        let effect = FIO.never().RaceFirst(FIO.succeed value)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "RaceFirst with FIO.never should return the completing effect's result")
                ]
        ]
