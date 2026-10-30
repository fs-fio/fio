module FIO.Tests.Extensions.MappingTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.Threading

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Mapping"
                [
                    testAllRuntimes "Map - a throwing mapper becomes a Defect" (fun runtime ->
                        let effect: FIO<int, string> = FIO.succeed(1).Map(fun (_: int) -> failwith "mapper throws")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect -> Expect.equal defect.Message "mapper throws" "The thrown exception should become the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "MapError - a throwing mapper becomes a Defect" (fun runtime ->
                        let effect: FIO<int, string> = FIO.fail("error").MapError(fun (_: string) -> failwith "mapper throws")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect -> Expect.equal defect.Message "mapper throws" "The thrown exception should become the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "Map - a mapper returning null succeeds with null, as a value rather than a missing effect" (fun runtime ->
                        let effect: FIO<string, string> = FIO.succeed(1).Map(fun (_: int) -> null)

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.equal result (Succeeded null) "A null mapped value should be the success value")

                    testAllRuntimes "Map - maps through a three-deep chain of Maps" (fun runtime ->
                        let effect: FIO<string, string> = FIO.succeed(1).Map((+) 1).Map((*) 3).Map(string)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result "6" "Three chained Maps should apply in order")

                    testAllRuntimes "Map - a mapper that throws after a FlatMap dies with a Defect" (fun runtime ->
                        let ran = ref false
                        let effect: FIO<int, string> =
                            FIO.succeed(1)
                                .FlatMap(fun x -> FIO.succeed (x + 1))
                                .Map(fun (_: int) -> failwith "mapper throws")
                                .FlatMap(fun (x: int) -> FIO.succeedWith (fun () -> ran.Value <- true; x))

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.isFalse ran.Value "The FlatMap after the throwing mapper should not run"
                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect -> Expect.equal defect.Message "mapper throws" "The thrown exception should become the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "Map - a failure skips the mapper" (fun runtime ->
                        let calls = ref 0
                        let effect: FIO<int, string> = FIO.fail("error").Map(fun (x: int) -> calls.Value <- calls.Value + 1; x)

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.equal calls.Value 0 "The mapper should not run on a failure"
                        match result with
                        | Failed error -> Expect.equal error "error" "The error should be kept"
                        | other -> failtest $"Expected Failed but got {other}")

                    testAllRuntimes "Map - an interruption skips the mapper" (fun runtime ->
                        let calls = ref 0
                        let waiting = new ManualResetEventSlim(false)
                        let effect: FIO<int, string> =
                            FIO.succeedWith(fun () -> waiting.Set())
                                .FlatMap(fun () -> FIO.never<int, string> ())
                                .Map(fun x -> calls.Value <- calls.Value + 1; x)
                        let fiber = runtime.Run effect
                        let started = waiting.Wait(TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(fiber.InterruptAwaitNow<string> ()).UnsafeSuccess()

                        Expect.isTrue started "The mapped effect should be waiting when it is interrupted"
                        Expect.equal calls.Value 0 "The mapper should not run on an interruption"
                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "Map - over an Ensuring whose finalizer fails yields the finalizer's failure" (fun runtime ->
                        let calls = ref 0
                        let effect: FIO<int, string> =
                            FIO.succeed(1).Ensuring(FIO.fail "f").Map(fun x -> calls.Value <- calls.Value + 1; x + 1)

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.equal calls.Value 0 "The mapper should not run when the finalizer fails"
                        match result with
                        | Failed error -> Expect.equal error "f" "The finalizer's failure should replace the success"
                        | other -> failtest $"Expected Failed but got {other}")

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
                ]

            testList
                "Replace / wrap success value"
                [
                    testAllRuntimes "Unit - discards the value" (fun runtime ->
                        let effect: FIO<int, string> = FIO.succeed 42

                        let result = runtime.Run(effect.Unit()).UnsafeSuccess()

                        Expect.equal result () "Unit should discard the value and yield unit")

                    testAllRuntimes "As - replaces the value" (fun runtime ->
                        let effect: FIO<int, string> = FIO.succeed 42

                        let result = runtime.Run(effect.As "replaced").UnsafeSuccess()

                        Expect.equal result "replaced" "As should yield the given value")

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
                ]

            testList
                "Wrap typed error"
                [
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
                ]

            testList
                "Container shape (outcome → infallible)"
                [
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
                ]
        ]
