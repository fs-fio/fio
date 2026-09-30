module FIO.Tests.Extensions.ZipTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Applicative"
                [
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
                ]

            testList
                "Sequential composition (Zip)"
                [
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
                ]

            testList
                "Parallel composition (ZipPar)"
                [
                    testPropertyWithConfig fsCheckConfig "ZipPar - combines both success values"
                    <| fun (runtime: FIORuntime, res1: int, res2: string) ->
                        let eff1 = FIO.succeed res1
                        let eff2 = FIO.succeed res2

                        let result =
                            runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()

                        Expect.equal result (res1, res2) "ZipPar should combine both results"

                    testAllRuntimes "ZipPar - runs its operands concurrently (the left operand parks until the right has started)" (fun runtime ->
                        let rendezvous = Channel<unit>()
                        let overlapped = ref false

                        let left: FIO<int, string> =
                            FIO.suspend (fun () ->
                                (rendezvous.Read().Timeout (TimeSpan.FromSeconds 3.0))
                                    .Map(fun signalled ->
                                        overlapped.Value <- Option.isSome signalled
                                        1))

                        let right: FIO<int, string> =
                            FIO.suspend (fun () -> (rendezvous.Write ()).Map(fun _ -> 2))

                        let result = runtime.Run(left.ZipPar right).UnsafeSuccess()

                        Expect.equal result (1, 2) "ZipPar must still combine both values"
                        Expect.isTrue
                            overlapped.Value
                            "ZipPar must run concurrently: the left operand saw the right start before finishing")

                    testAllRuntimes "ZipPar - fails fast when the forked sibling fails (never on left)" (fun runtime ->
                        let error = 42
                        let sentinel = -1

                        let effect =
                            (FIO.never<int, int>().ZipPar(FIO.fail<int, int> error))
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "ZipPar should fail fast with the sibling error, not hang until the timeout")

                    testAllRuntimes "ZipPar - fails fast when the forked sibling fails (never on right)" (fun runtime ->
                        let error = 42
                        let sentinel = -1

                        let effect =
                            ((FIO.fail<int, int> error).ZipPar(FIO.never<int, int>()))
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "ZipPar should fail fast with the sibling error, not hang until the timeout")

                    testAllRuntimes "ZipPar - interrupts the long-running sibling on failure (no leak)" (fun runtime ->
                        let error = 7
                        let sentinel = -1
                        let mutable completedNormally = false

                        let sibling =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0))
                                .FlatMap(fun () -> FIO.attempt (fun () -> completedNormally <- true) (fun _ -> sentinel))

                        let effect =
                            ((FIO.fail<int, int> error).ZipPar sibling)
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "ZipPar should surface the sibling failure"
                        Expect.isFalse completedNormally "ZipPar should interrupt the long-running sibling instead of running it to completion")

                    testAllRuntimes "ZipPar - both succeed still pairs both values" (fun runtime ->
                        let result =
                            runtime.Run((FIO.succeed 1).ZipPar(FIO.succeed "two")).UnsafeSuccess()

                        Expect.equal result (1, "two") "ZipPar should pair both success values in (this, effect) order")

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

                    testPropertyWithConfig fsCheckConfig "ZipParError - both succeed returns one of the values"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let eff1 = FIO.succeed value
                        let eff2 = FIO.succeed (value + 1)

                        let result =
                            runtime.Run(eff1.ZipParError eff2).UnsafeSuccess()

                        Expect.isTrue (result = value || result = value + 1) "ZipParError should return one of the concurrent successes"

                    testAllRuntimes "ZipParError - succeeds fast when this succeeds and the sibling never terminates" (fun runtime ->
                        let value = 99
                        let sentinel = (-1, -1)

                        let effect =
                            ((FIO.succeed value).ZipParError(FIO.never<int, int>()))
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "ZipParError should return the success without waiting on the never-terminating sibling")

                    testAllRuntimes "ZipParError - succeeds fast when the sibling succeeds and this never terminates" (fun runtime ->
                        let value = 99
                        let sentinel = (-1, -1)

                        let effect =
                            ((FIO.never<int, int>()).ZipParError(FIO.succeed value))
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "ZipParError should return the sibling's success even when this never terminates")

                    testAllRuntimes "ZipParError - interrupts the long-running sibling once one side succeeds (no leak)" (fun runtime ->
                        let value = 5
                        let sentinel = (-1, -1)
                        let mutable completedNormally = false

                        let sibling =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0))
                                .FlatMap(fun () -> FIO.attempt (fun () -> completedNormally <- true; 0) (fun _ -> 0))

                        let effect =
                            ((FIO.succeed value).ZipParError sibling)
                                .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "ZipParError should surface the immediate success"
                        Expect.isFalse completedNormally "ZipParError should interrupt the long-running sibling instead of running it to completion")

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
                ]

            testList
                "Fold / consume outcome"
                [
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

                    testPropertyWithConfig fsCheckConfig "FoldFIO - error handler does not catch success handler errors"
                    <| fun (runtime: FIORuntime, value: int, error: int) ->
                        let effect =
                            FIO.succeed(value).FoldFIO (fun e -> FIO.succeed (e * 10)) (fun _ -> FIO.fail error)

                        let actual =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal actual error "A failure of the success handler should propagate unchanged"

                    testPropertyWithConfig fsCheckConfig "FoldFIO - a failure inside nested success handlers runs no error handler"
                    <| fun (runtime: FIORuntime, error: int) ->
                        let mutable handled = 0

                        let rec levels depth : FIO<unit, int> =
                            if depth = 0 then
                                FIO.fail error
                            else
                                FIO.unit().FoldFIO
                                    (fun e -> FIO.succeedWith(fun () -> handled <- handled + 1).FlatMap(fun () -> FIO.fail e))
                                    (fun () -> levels (depth - 1))

                        let actual =
                            runtime.Run(levels 5).UnsafeError()

                        Expect.equal actual error "The innermost failure should propagate unchanged"
                        Expect.equal handled 0 "No error handler should run for a success handler's failure"

                    testAllRuntimes "FoldFIO - deep recursion through the success handler completes" (fun runtime ->
                        let rec loop depth : FIO<int, string> =
                            if depth = 0 then
                                FIO.succeed 42
                            else
                                FIO.unit().FoldFIO (fun (e: string) -> FIO.fail e) (fun () -> loop (depth - 1))

                        let actual =
                            runtime.Run(loop 200_000).UnsafeSuccess()

                        Expect.equal actual 42 "A 200,000-deep FoldFIO recursion should complete")

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
                ]
        ]
