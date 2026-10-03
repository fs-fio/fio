module FIO.Tests.Factories.ConstructorTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto
open FsCheck

open System
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private throwsFromUserCode () : int =
    failwith "thunk threw"

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Immediate constructors"
                [
                    testPropertyWithConfig fsCheckConfig "unit - returns unit"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.unit ()

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result () "FIO.unit should return unit"

                    testPropertyWithConfig fsCheckConfig "succeed - returns the provided value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.succeed value

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.succeed should return the provided value"

                    testPropertyWithConfig fsCheckConfig "fail - fails with the provided error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fail error

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "FIO.fail should fail with the provided error"

                    testPropertyWithConfig fsCheckConfig "interrupt - results in Interrupted fiber"
                    <| fun (runtime: FIORuntime) ->
                        let effect = FIO.interrupt ExplicitInterrupt "test interrupt"

                        let fiber = runtime.Run effect
                        let fiberResult =
                            fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match fiberResult with
                        | Interrupted ex ->
                            Expect.equal
                                ex.fiberId
                                fiber.Id
                                "FIO.interrupt should set the fiberId in the exception to the current fiber's ID"
                            Expect.equal
                                ex.cause
                                ExplicitInterrupt
                                "FIO.interrupt should set the provided cause in the exception"
                            Expect.equal
                                ex.message
                                "test interrupt"
                                "FIO.interrupt should set the provided message in the exception"
                        | _ -> failtest "FIO.interrupt should result in Interrupted"

                    testPropertyWithConfig fsCheckConfig "interrupt - with ParentInterrupted cause"
                    <| fun (runtime: FIORuntime) ->
                        let parentGuid = Guid.NewGuid()
                        let effect =
                            FIO.interrupt (ParentInterrupted parentGuid) "parent interrupt test"

                        let fiber = runtime.Run effect
                        let fiberResult =
                            fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match fiberResult with
                        | Interrupted ex ->
                            Expect.equal
                                ex.fiberId
                                fiber.Id
                                "FIO.interrupt should set the fiberId in the exception to the current fiber's ID"
                            Expect.equal
                                ex.cause
                                (ParentInterrupted parentGuid)
                                "FIO.interrupt should set the provided ParentInterrupted cause in the exception"
                            Expect.equal
                                ex.message
                                "parent interrupt test"
                                "FIO.interrupt should set the provided message in the exception"
                        | _ -> failtest "FIO.interrupt with ParentInterrupted should result in Interrupted"

                    testPropertyWithConfig fsCheckConfig "interrupt - with ResourceExhaustion cause"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            FIO.interrupt (ResourceExhaustion "out of memory") "resource test"

                        let fiber = runtime.Run effect
                        let fiberResult =
                            fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match fiberResult with
                        | Interrupted ex ->
                            Expect.equal
                                ex.fiberId
                                fiber.Id
                                "FIO.interrupt should set the fiberId in the exception to the current fiber's ID"
                            Expect.equal
                                ex.cause
                                (ResourceExhaustion "out of memory")
                                "FIO.interrupt should set the provided ResourceExhaustion cause in the exception"
                            Expect.equal
                                ex.message
                                "resource test"
                                "FIO.interrupt should set the provided message in the exception"
                        | _ -> failtest "FIO.interrupt with ResourceExhaustion should result in Interrupted"

                    testPropertyWithConfig fsCheckConfig "interrupt - with InvalidArgument cause"
                    <| fun (runtime: FIORuntime) ->
                        let effect =
                            FIO.interrupt (InvalidArgument("param", "bad value")) "invalid arg test"

                        let fiber = runtime.Run effect
                        let fiberResult =
                            fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                        match fiberResult with
                        | Interrupted ex ->
                            Expect.equal
                                ex.fiberId
                                fiber.Id
                                "FIO.interrupt should set the fiberId in the exception to the current fiber's ID"
                            Expect.equal
                                ex.cause
                                (InvalidArgument("param", "bad value"))
                                "FIO.interrupt should set the provided InvalidArgument cause in the exception"
                            Expect.equal
                                ex.message
                                "invalid arg test"
                                "FIO.interrupt should set the provided message in the exception"
                        | _ -> failtest "FIO.interrupt with InvalidArgument should result in Interrupted"

                    testPropertyWithConfig fsCheckConfig "attempt - succeeds when function succeeds"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.attempt (fun () -> value) (fun ex -> ex.Message)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.attempt should return the function result"

                    testPropertyWithConfig fsCheckConfig "attempt - maps exception to error when function throws"
                    <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
                        let msg = errorMsg.Get
                        let effect = FIO.attempt (fun () -> raise (Exception msg)) (fun ex -> ex.Message)

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result msg "FIO.attempt should map exception to error"

                    testPropertyWithConfig fsCheckConfig "attempt - passes through exception"
                    <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
                        let msg = errorMsg.Get
                        let ex = Exception msg
                        let effect = FIO.attempt (fun () -> raise ex) id

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result.Message msg "FIO.attempt should pass through exception"

                    testPropertyWithConfig fsCheckConfig "succeedWith - succeeds with the function result"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect: FIO<int, string> = FIO.succeedWith (fun () -> value)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.succeedWith should return the function result"

                    testPropertyWithConfig fsCheckConfig "succeedWith - runs the function once per run, not at construction"
                    <| fun (runtime: FIORuntime) ->
                        let calls = ref 0
                        let effect: FIO<unit, string> = FIO.succeedWith (fun () -> calls.Value <- calls.Value + 1)

                        Expect.equal calls.Value 0 "Constructing the effect must not run the function"

                        runtime.Run(effect).UnsafeSuccess()
                        runtime.Run(effect).UnsafeSuccess()

                        Expect.equal calls.Value 2 "Each run should call the function exactly once"

                    testAllRuntimes "succeedWith - a throwing function is a defect, not a typed error"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.succeedWith (fun () -> failwith "thunk threw")

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect inner -> Expect.equal inner.Message "thunk threw" "The defect should carry the thrown exception"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}"

                    testAllRuntimes "succeedWith - a defect keeps the stack trace of the original throw"
                    <| fun runtime ->
                        let effect: FIO<int, string> = FIO.succeedWith throwsFromUserCode

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect inner ->
                                Expect.stringContains
                                    (string inner.StackTrace)
                                    (nameof throwsFromUserCode)
                                    "The defect's exception must still point at the code that threw, not at the error mapper"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}"
                ]

            testList
                "Lift from standard types"
                [
                    testPropertyWithConfig fsCheckConfig "fromResult - converts Ok to success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.fromResult (Ok value)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.fromResult should convert Ok to success"

                    testPropertyWithConfig fsCheckConfig "fromResult - converts Error to fail"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fromResult (Error error)

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "FIO.fromResult should convert Error to fail"

                    testPropertyWithConfig fsCheckConfig "fromOption - converts Some to success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.fromOption (Some value) (fun () -> "none error")

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.fromOption should convert Some to success"

                    testPropertyWithConfig fsCheckConfig "fromOption - converts None to error using onNone"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fromOption None (fun () -> error)

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "FIO.fromOption should convert None to error using onNone"

                    testPropertyWithConfig fsCheckConfig "fromChoice - converts Choice1Of2 to success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let effect = FIO.fromChoice (Choice1Of2 value)

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "FIO.fromChoice should convert Choice1Of2 to success"

                    testPropertyWithConfig fsCheckConfig "fromChoice - converts Choice2Of2 to fail"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fromChoice (Choice2Of2 error)

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result error "FIO.fromChoice should convert Choice2Of2 to fail"
                ]
        ]
