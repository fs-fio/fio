module FIO.Tests.Extensions.ObservationTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.IO

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Observation (preserve outcome, run side effect)"
                [
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

                    testPropertyWithConfig fsCheckConfig "TapBoth - a failing success tap does not run the error tap"
                    <| fun (runtime: FIORuntime, value: int, error: int) ->
                        let mutable errorTap = false

                        let effect =
                            FIO.succeed(value)
                                .TapBoth (fun _ -> FIO.fail error) (fun _ -> FIO.succeed (errorTap <- true))

                        let actual = runtime.Run(effect).UnsafeError()

                        Expect.equal actual error "TapBoth should fail with the success tap's error"
                        Expect.isFalse errorTap "TapBoth should not run the error tap for the success tap's failure"

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
                ]
        ]
