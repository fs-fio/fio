module FIO.Tests.Extensions.PredicateTests

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
                "Outcome predicates / discards"
                [
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
                ]

            testList
                "Boolean guards"
                [
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
                ]
        ]
