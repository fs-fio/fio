module FIO.Tests.PromiseTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open Expecto

open System

[<Tests>]
let promiseTests =
    testList
        "Promise"
        [
            testList
                "Completion"
                [
                    testPropertyWithConfig fsCheckConfig "Succeed - Await yields the value"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let promise = Promise<int, string>()
                        let effect = promise.Succeed(value).FlatMap(fun _ -> promise.Await())

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "Await should yield the value the promise was completed with"

                    testPropertyWithConfig fsCheckConfig "Fail - Await fails with the error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let promise = Promise<int, string>()
                        let effect = promise.Fail(error).FlatMap(fun _ -> promise.Await())

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "Await should fail with the error the promise was completed with"

                    testAllRuntimes "Succeed - completes the promise only once" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<bool * bool * bool * int, string> =
                            fio {
                                let! first = promise.Succeed 1
                                let! second = promise.Succeed 2
                                let! failed = promise.Fail "late"
                                let! value = promise.Await()
                                return first, second, failed, value
                            }

                        let first, second, failed, value = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue first "The first completion should win"
                        Expect.isFalse second "A second Succeed should be refused"
                        Expect.isFalse failed "A Fail after a Succeed should be refused"
                        Expect.equal value 1 "The first value should stay")

                    testAllRuntimes "Fail - completes the promise only once" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<bool * bool * Result<int, string>, string> =
                            fio {
                                let! first = promise.Fail "first"
                                let! second = promise.Succeed 2
                                let! outcome = promise.Await().Result()
                                return first, second, outcome
                            }

                        let first, second, outcome = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue first "The first completion should win"
                        Expect.isFalse second "A Succeed after a Fail should be refused"
                        Expect.equal outcome (Error "first") "The first error should stay")

                    testAllRuntimes "Succeed - building the effect does not complete the promise" (fun runtime ->
                        let promise = Promise<int, string>()
                        let _ : FIO<bool, string> = promise.Succeed 1

                        let result = runtime.Run(promise.Poll() : FIO<Result<int, string> option, string>).UnsafeSuccess()

                        Expect.isNone result "Building Succeed should not complete the promise")

                    testAllRuntimes "Poll - yields None until the promise is completed, then its outcome" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<Result<int, string> option * Result<int, string> option, string> =
                            fio {
                                let! before = promise.Poll()
                                do! promise.Fail("gone").Unit()
                                let! after = promise.Poll()
                                return before, after
                            }

                        let before, after = runtime.Run(effect).UnsafeSuccess()

                        Expect.isNone before "An open promise should poll as None"
                        Expect.equal after (Some(Error "gone")) "A completed promise should poll as its outcome")
                ]

            testList
                "Await"
                [
                    testAllRuntimes "Await - suspends until the promise is completed" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<FiberResult<int, string> option * int, string> =
                            fio {
                                let! awaiter = promise.Await().Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                let! before = awaiter.Poll()
                                do! promise.Succeed(42).Unit()
                                let! value = awaiter.Join()
                                return before, value
                            }

                        let before, value = runtime.Run(effect).UnsafeSuccess()

                        Expect.isNone before "The awaiter should still wait while the promise is open"
                        Expect.equal value 42 "The awaiter should resume with the value")

                    testAllRuntimes "Await - resumes every awaiter with the value" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<int list, string> =
                            fio {
                                let! awaiters = FIO.collectAll [ for _ in 1..50 -> promise.Await().Fork() ]
                                do! promise.Succeed(7).Unit()
                                return! FIO.forEach awaiters (fun awaiter -> awaiter.Join())
                            }

                        let values = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal values (List.replicate 50 7) "Every awaiter should get the value")

                    testAllRuntimes "Await - fails every awaiter with the error" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<Result<int, string> list, string> =
                            fio {
                                let! awaiters = FIO.collectAll [ for _ in 1..50 -> promise.Await().Result().Fork() ]
                                do! promise.Fail("boom").Unit()
                                return! FIO.forEach awaiters (fun awaiter -> awaiter.Join())
                            }

                        let outcomes = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal outcomes (List.replicate 50 (Error "boom")) "Every awaiter should fail with the error")

                    testAllRuntimes "Await - an interrupted awaiter leaves the promise open" (fun runtime ->
                        let promise = Promise<int, string>()
                        let effect : FIO<FiberResult<int, string> * Result<int, string> option * int, string> =
                            fio {
                                let! awaiter = promise.Await().Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                let! interrupted = awaiter.InterruptAwaitNow()
                                let! stillOpen = promise.Poll()
                                do! promise.Succeed(5).Unit()
                                let! value = promise.Await()
                                return interrupted, stillOpen, value
                            }

                        let interrupted, stillOpen, value = runtime.Run(effect).UnsafeSuccess()

                        match interrupted with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected the awaiter to be interrupted but got %A{other}"
                        Expect.isNone stillOpen "Interrupting an awaiter should not complete the promise"
                        Expect.equal value 5 "The promise should still be completable")

                    testAllRuntimes "Await - an uninterruptible awaiter waits through an interruption" (fun runtime ->
                        let promise = Promise<int, string>()
                        let received = ref false
                        let receive = promise.Await().FlatMap(fun value -> FIO.succeedWith (fun () -> received.Value <- (value = 9)))
                        let effect : FIO<unit, string> =
                            fio {
                                let! awaiter = (FIO.uninterruptible receive).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! awaiter.InterruptNow()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! promise.Succeed(9).Unit()
                            }

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue (waitForFlag received) "The uninterruptible awaiter should keep waiting and receive the value")

                    testCase "Await - resumes when a fiber on the other runtime completes the promise" <| fun () ->
                        use direct = new DirectRuntime() :> FIORuntime
                        use workStealing = new WorkStealingRuntime(testConfig) :> FIORuntime
                        let pairs = [ direct, workStealing; workStealing, direct ]

                        let results =
                            [
                                for awaitOn, completeOn in pairs do
                                    let promise = Promise<int, string>()
                                    let awaiter = awaitOn.Run(promise.Await())
                                    completeOn.Run(promise.Succeed 3 : FIO<bool, string>).UnsafeSuccess() |> ignore
                                    yield resultWithin (TimeSpan.FromSeconds 5.0) awaiter
                            ]

                        Expect.equal results [ Some(Succeeded 3); Some(Succeeded 3) ] "An awaiter should resume whichever runtime completes the promise"
                ]

            testList
                "Stress"
                [
                    stressTestAllRuntimes "Stress - racing completers complete a promise once, and every awaiter sees that outcome" (fun runtime ->
                        let round () : FIO<bool, string> =
                            fio {
                                let promise = Promise<int, string>()
                                let! awaiters = FIO.collectAll [ for _ in 1..4 -> promise.Await().Result().Fork() ]
                                let! wins = FIO.forEachPar [ 1..8 ] (fun i -> if i % 2 = 0 then promise.Succeed i else promise.Fail $"error {i}")
                                let! seen = FIO.forEach awaiters (fun awaiter -> awaiter.Join())
                                let! outcome = promise.Poll()
                                return List.length (List.filter id wins) = 1 && List.forall (fun seenOutcome -> Some seenOutcome = outcome) seen
                            }
                        let effect = FIO.forEach [ 1..2_000 ] (fun _ -> round ())

                        let rounds = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue (List.forall id rounds) "Exactly one completer should win each round, and every awaiter should see its outcome")

                    stressTestAllRuntimes "Stress - awaiters interrupted as the promise is completed all settle" (fun runtime ->
                        let round () : FIO<bool, string> =
                            fio {
                                let promise = Promise<int, string>()
                                let! awaiters = FIO.collectAll [ for _ in 1..4 -> promise.Await().Fork() ]
                                do! promise.Succeed(1).Unit() <&&> FIO.forEachDiscard awaiters (fun awaiter -> awaiter.InterruptNow())
                                let! results = FIO.forEach awaiters (fun awaiter -> awaiter.Await())
                                return results |> List.forall (function Succeeded 1 | Interrupted _ -> true | _ -> false)
                            }
                        let effect = (FIO.forEach [ 1..1_000 ] (fun _ -> round ())).TimeoutFail "timed out" (TimeSpan.FromSeconds 60.0)

                        let rounds = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue (List.forall id rounds) "Every awaiter should end with the value or interrupted")
                ]
        ]
