module FIO.Tests.PromiseTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Promise

open Expecto

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

let private onError (e: exn) = e.Message

[<Tests>]
let promiseSucceedTests =
    testList
        "Promise Succeed"
        [

            testPropertyWithConfig fsCheckConfig "Succeed - completes promise with value"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.Succeed(res, onError)
                        let! value = promise.Await onError
                        return completed, value
                    }

                let completed, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "First succeed should return true"
                Expect.equal value res "Promise should contain succeeded value"

            testPropertyWithConfig fsCheckConfig "SucceedExn - completes promise with value"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.SucceedExn res
                        let! value = promise.AwaitExn()
                        return completed, value
                    }

                let completed, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "First succeed should return true"
                Expect.equal value res "Promise should contain succeeded value"

            testPropertyWithConfig fsCheckConfig "Succeed - second succeed returns false"
            <| fun (runtime: FIORuntime, res1: int, res2: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! first = promise.SucceedExn res1
                        let! second = promise.SucceedExn res2
                        let! value = promise.AwaitExn()
                        return first, second, value
                    }

                let first, second, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue first "First succeed should return true"
                Expect.isFalse second "Second succeed should return false"
                Expect.equal value res1 "Promise should contain first value"
        ]

[<Tests>]
let promiseFailTests =
    testList
        "Promise Fail"
        [

            testPropertyWithConfig fsCheckConfig "Fail - completes promise with error"
            <| fun (runtime: FIORuntime) ->
                let err = "test error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.Fail(err, onError)
                        let! poll = promise.Poll onError
                        return completed, poll
                    }

                let completed, poll = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "First fail should return true"

                match poll with
                | Some(Error e) -> Expect.equal e err "Promise should contain error"
                | _ -> failtest "Expected Some Error poll result"

            testPropertyWithConfig fsCheckConfig "FailExn - completes promise with error"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.FailExn err
                        let! poll = promise.PollExn()
                        return completed, poll
                    }

                let completed, poll = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "First fail should return true"

                match poll with
                | Some(Error e) -> Expect.equal e.Message err.Message "Promise should contain error"
                | _ -> failtest "Expected Some Error poll result"

            testPropertyWithConfig fsCheckConfig "Fail - second fail returns false"
            <| fun (runtime: FIORuntime) ->
                let err1 = exn "error1"
                let err2 = exn "error2"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! first = promise.FailExn err1
                        let! second = promise.FailExn err2
                        return first, second
                    }

                let first, second = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue first "First fail should return true"
                Expect.isFalse second "Second fail should return false"
        ]

[<Tests>]
let promiseCompleteTests =
    testList
        "Promise Complete"
        [

            testPropertyWithConfig fsCheckConfig "Complete - with Ok stores success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.Complete(Ok res, onError)
                        let! value = promise.Await onError
                        return completed, value
                    }

                let completed, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "Complete with Ok should return true"
                Expect.equal value res "Promise should contain value"

            testPropertyWithConfig fsCheckConfig "CompleteExn - with Ok stores success"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.CompleteExn(Ok res)
                        let! value = promise.AwaitExn()
                        return completed, value
                    }

                let completed, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "Complete with Ok should return true"
                Expect.equal value res "Promise should contain value"

            testPropertyWithConfig fsCheckConfig "CompleteExn - with Error stores error"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! completed = promise.CompleteExn(Error err)
                        let! poll = promise.PollExn()
                        return completed, poll
                    }

                let completed, poll = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue completed "Complete with Error should return true"

                match poll with
                | Some(Error e) -> Expect.equal e.Message err.Message "Promise should contain error"
                | _ -> failtest "Expected Some Error poll result"

            testPropertyWithConfig fsCheckConfig "Complete - second complete returns false"
            <| fun (runtime: FIORuntime, res1: int, res2: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! first = promise.CompleteExn(Ok res1)
                        let! second = promise.CompleteExn(Ok res2)
                        let! value = promise.AwaitExn()
                        return first, second, value
                    }

                let first, second, value = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue first "First complete should return true"
                Expect.isFalse second "Second complete should return false"
                Expect.equal value res1 "Promise should contain first value"
        ]

[<Tests>]
let promiseAwaitTests =
    testList
        "Promise Await"
        [

            testPropertyWithConfig fsCheckConfig "Await - returns value on succeeded promise"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.Succeed(res, onError)
                        let! value = promise.Await onError
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "Await should return succeeded value"

            testPropertyWithConfig fsCheckConfig "AwaitExn - returns value on succeeded promise"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.SucceedExn res
                        let! value = promise.AwaitExn()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "Await should return succeeded value"

            testPropertyWithConfig fsCheckConfig "AwaitExn - propagates error on failed promise"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.FailExn err
                        let! _ = promise.AwaitExn()
                        return 0
                    }

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Failed e -> Expect.equal e.Message err.Message "Await should propagate error"
                | other -> failtest $"Expected Failed but got: {other}"

            testAllRuntimes "Await - blocks until completed" (fun runtime ->
                let eff =
                    fio {
                        let! promise = Promise.make ()

                        let completer =
                            FIO.sleepExn (TimeSpan.FromMilliseconds 10.0)
                            >>= fun _ -> promise.SucceedExn 42
                            >>= fun _ -> FIO.unit ()

                        let! _ = completer.Fork()
                        let! value = promise.AwaitExn()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 42 "Await should return value after completion")

            testAllRuntimes "Await - multiple awaiters all receive value" (fun runtime ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! f1 = promise.AwaitExn().Fork()
                        let! f2 = promise.AwaitExn().Fork()
                        let! f3 = promise.AwaitExn().Fork()
                        let! _ = promise.SucceedExn 99
                        let! v1 = f1.Join()
                        let! v2 = f2.Join()
                        let! v3 = f3.Join()
                        return [ v1; v2; v3 ]
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 99; 99; 99 ] "All awaiters should receive same value")
        ]

[<Tests>]
let promisePollTests =
    testList
        "Promise Poll"
        [

            testPropertyWithConfig fsCheckConfig "Poll - returns None when not completed"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! poll = promise.Poll onError
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isNone result "Poll should return None when not completed"

            testPropertyWithConfig fsCheckConfig "PollExn - returns None when not completed"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! poll = promise.PollExn()
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isNone result "Poll should return None when not completed"

            testPropertyWithConfig fsCheckConfig "PollExn - returns Some Ok after succeed"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.SucceedExn res
                        let! poll = promise.PollExn()
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(Ok v) -> Expect.equal v res "Poll should return Some Ok with value"
                | _ -> failtest "Expected Some Ok result"

            testPropertyWithConfig fsCheckConfig "PollExn - returns Some Error after fail"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.FailExn err
                        let! poll = promise.PollExn()
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(Error e) -> Expect.equal e.Message err.Message "Poll should return Some Error"
                | _ -> failtest "Expected Some Error result"
        ]

[<Tests>]
let promiseIsDoneTests =
    testList
        "Promise IsDone"
        [

            testPropertyWithConfig fsCheckConfig "IsDone - returns false before completion"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! promise = Promise.make<int, exn> ()
                        let! isDone = promise.IsDone()
                        return isDone
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse result "IsDone should return false before completion"

            testPropertyWithConfig fsCheckConfig "IsDone - returns true after succeed"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make<int, exn> ()
                        let! _ = promise.SucceedExn res
                        let! isDone = promise.IsDone()
                        return isDone
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "IsDone should return true after succeed"

            testPropertyWithConfig fsCheckConfig "IsDone - returns true after fail"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! _ = promise.FailExn err
                        let! isDone = promise.IsDone()
                        return isDone
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "IsDone should return true after fail"
        ]

[<Tests>]
let promiseMakeTests =
    testList
        "Promise.make"
        [

            testPropertyWithConfig fsCheckConfig "make - creates empty promise"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        let! isDone = promise.IsDone()
                        return isDone
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse result "New promise should not be done"
        ]

[<Tests>]
let promiseSucceedFailFactoryTests =
    testList
        "Promise.succeed / Promise.fail"
        [

            testPropertyWithConfig fsCheckConfig "Promise.succeed - creates pre-succeeded promise"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.succeed (res, onError)
                        let! value = promise.Await onError
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "Pre-succeeded promise should contain value"

            testPropertyWithConfig fsCheckConfig "Promise.succeedExn - creates pre-succeeded promise"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.succeedExn res
                        let! value = promise.AwaitExn()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "Pre-succeeded promise should contain value"

            testPropertyWithConfig fsCheckConfig "Promise.fail - creates pre-failed promise"
            <| fun (runtime: FIORuntime) ->
                let err = "test error"

                let eff =
                    fio {
                        let! promise = Promise.fail (err, onError)
                        let! poll = promise.Poll onError
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(Error e) -> Expect.equal e err "Pre-failed promise should contain error"
                | _ -> failtest "Expected Some Error poll result"

            testPropertyWithConfig fsCheckConfig "Promise.failExn - creates pre-failed promise"
            <| fun (runtime: FIORuntime) ->
                let err = exn "test error"

                let eff =
                    fio {
                        let! promise = Promise.failExn err
                        let! poll = promise.PollExn()
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(Error e) -> Expect.equal e.Message err.Message "Pre-failed promise should contain error"
                | _ -> failtest "Expected Some Error poll result"
        ]

[<Tests>]
let promiseCompleteWithTests =
    testList
        "Promise.completeWith"
        [

            testPropertyWithConfig fsCheckConfig "completeWith - succeeds when effect succeeds"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        do! Promise.completeWith (promise, FIO.succeed res, onError)
                        let! value = promise.Await onError
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "completeWith should complete with effect success"

            testPropertyWithConfig fsCheckConfig "completeWithExn - succeeds when effect succeeds"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! promise = Promise.make ()
                        do! Promise.completeWithExn (promise, FIO.succeed res)
                        let! value = promise.AwaitExn()
                        return value
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result res "completeWithExn should complete with effect success"

            testPropertyWithConfig fsCheckConfig "completeWithExn - fails when effect fails"
            <| fun (runtime: FIORuntime) ->
                let err = exn "effect error"

                let eff =
                    fio {
                        let! promise = Promise.make ()
                        do! Promise.completeWithExn (promise, FIO.fail err)
                        let! poll = promise.PollExn()
                        return poll
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                match result with
                | Some(Error e) -> Expect.equal e.Message err.Message "completeWith should complete with effect error"
                | _ -> failtest "Expected Some Error result"
        ]
