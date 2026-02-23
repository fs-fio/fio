module FIO.Tests.PromiseTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Promise

open Expecto

open FIO.Runtime

[<Tests>]
let promiseCreationTests =
    testList "Promise Creation" [

        testPropertyWithConfig fsCheckConfig "Promise.make creates empty promise"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! promise = Promise.make<int, string>()
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isFalse result "New promise should not be done"

        testPropertyWithConfig fsCheckConfig "Promise.succeed creates pre-succeeded promise"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.succeed<int, exn>(res, id)
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Pre-succeeded promise should be done"

        testPropertyWithConfig fsCheckConfig "Promise.fail creates pre-failed promise"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test error"
            let eff = fio {
                let! promise = Promise.fail<int, exn>(err, id)
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Pre-failed promise should be done"

        testPropertyWithConfig fsCheckConfig "Promise.succeedExn creates pre-succeeded promise"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.succeedExn<int>(res)
                let! result = promise.AwaitExn()
                return result
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Pre-succeeded promise should contain value"
    ]

[<Tests>]
let promiseCompletionTests =
    testList "Promise Completion" [

        testPropertyWithConfig fsCheckConfig "Succeed completes promise with value"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! completed = promise.SucceedExn res
                let! value = promise.AwaitExn()
                return (completed, value)
            }
            let (completed, value) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue completed "First succeed should return true"
            Expect.equal value res "Promise should contain succeeded value"

        testPropertyWithConfig fsCheckConfig "Fail completes promise with error"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test error"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! completed = promise.FailExn err
                let! poll = promise.PollExn()
                return (completed, poll)
            }
            let (completed, poll) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue completed "First fail should return true"
            match poll with
            | Some (Error e) -> Expect.equal e.Message err.Message "Promise should contain error"
            | _ -> failtest "Expected Error poll result"

        testPropertyWithConfig fsCheckConfig "Second Succeed returns false"
        <| fun (runtime: FIORuntime, res1: int, res2: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! first = promise.SucceedExn res1
                let! second = promise.SucceedExn res2
                let! value = promise.AwaitExn()
                return (first, second, value)
            }
            let (first, second, value) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue first "First succeed should return true"
            Expect.isFalse second "Second succeed should return false"
            Expect.equal value res1 "Promise should contain first value"

        testPropertyWithConfig fsCheckConfig "Second Fail returns false"
        <| fun (runtime: FIORuntime) ->
            let err1 = exn "error1"
            let err2 = exn "error2"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! first = promise.FailExn err1
                let! second = promise.FailExn err2
                return (first, second)
            }
            let (first, second) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue first "First fail should return true"
            Expect.isFalse second "Second fail should return false"

        testPropertyWithConfig fsCheckConfig "Complete with Ok succeeds"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! completed = promise.CompleteExn(Ok res)
                let! value = promise.AwaitExn()
                return (completed, value)
            }
            let (completed, value) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue completed "Complete with Ok should return true"
            Expect.equal value res "Promise should contain value"

        testPropertyWithConfig fsCheckConfig "Complete with Error fails"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test error"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! completed = promise.CompleteExn(Error err)
                let! poll = promise.PollExn()
                return (completed, poll)
            }
            let (completed, poll) = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue completed "Complete with Error should return true"
            match poll with
            | Some (Error e) -> Expect.equal e.Message err.Message "Promise should contain error"
            | _ -> failtest "Expected Error poll result"
    ]

[<Tests>]
let promiseAwaitTests =
    testList "Promise Await" [

        testPropertyWithConfig fsCheckConfig "Await blocks until completed"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                // Fork a fiber that completes the promise after a delay
                let completer = FIO.sleepExn(System.TimeSpan.FromMilliseconds 10.0) >>= fun _ -> promise.SucceedExn res >>= fun _ -> FIO.unit()
                let! _ = completer.Fork()
                // Await should block until completed
                let! value = promise.AwaitExn()
                return value
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Await should return value after completion"

        testPropertyWithConfig fsCheckConfig "Await on pre-succeeded promise returns immediately"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.succeed<int, exn>(res, id)
                let! value = promise.AwaitExn()
                return value
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Await on pre-succeeded should return immediately"

        testPropertyWithConfig fsCheckConfig "Multiple awaiters all receive value"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                // Fork multiple awaiters
                let! f1 = promise.AwaitExn().Fork()
                let! f2 = promise.AwaitExn().Fork()
                let! f3 = promise.AwaitExn().Fork()
                // Complete the promise
                let! _ = promise.SucceedExn res
                // Join all
                let! v1 = f1.Join()
                let! v2 = f2.Join()
                let! v3 = f3.Join()
                return [v1; v2; v3]
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result [res; res; res] "All awaiters should receive same value"
    ]

[<Tests>]
let promisePollTests =
    testList "Promise Poll" [

        testPropertyWithConfig fsCheckConfig "Poll returns None when not completed"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! poll = promise.PollExn()
                return poll
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNone result "Poll should return None when not completed"

        testPropertyWithConfig fsCheckConfig "Poll returns Some Ok when succeeded"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! _ = promise.SucceedExn res
                let! poll = promise.PollExn()
                return poll
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            match result with
            | Some (Ok v) -> Expect.equal v res "Poll should return Some Ok with value"
            | _ -> failtest "Expected Some Ok result"

        testPropertyWithConfig fsCheckConfig "Poll returns Some Error when failed"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test error"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! _ = promise.FailExn err
                let! poll = promise.PollExn()
                return poll
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            match result with
            | Some (Error e) -> Expect.equal e.Message err.Message "Poll should return Some Error with error"
            | _ -> failtest "Expected Some Error result"
    ]

[<Tests>]
let promiseIsDoneTests =
    testList "Promise IsDone" [

        testPropertyWithConfig fsCheckConfig "IsDone returns false before completion"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isFalse result "IsDone should return false before completion"

        testPropertyWithConfig fsCheckConfig "IsDone returns true after Succeed"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! _ = promise.SucceedExn res
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "IsDone should return true after Succeed"

        testPropertyWithConfig fsCheckConfig "IsDone returns true after Fail"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                let! _ = promise.FailExn err
                let! isDone = promise.IsDone()
                return isDone
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "IsDone should return true after Fail"
    ]

[<Tests>]
let promiseCompleteWithTests =
    testList "Promise.completeWith" [

        testPropertyWithConfig fsCheckConfig "completeWith succeeds when effect succeeds"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                do! Promise.completeWithExn(promise, FIO.succeed res)
                let! value = promise.AwaitExn()
                return value
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "completeWith should complete with effect success"

        testPropertyWithConfig fsCheckConfig "completeWith fails when effect fails"
        <| fun (runtime: FIORuntime) ->
            let err = exn "effect error"
            let eff = fio {
                let! promise = Promise.make<int, exn>()
                do! Promise.completeWithExn(promise, FIO.fail err)
                let! poll = promise.PollExn()
                return poll
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            match result with
            | Some (Error e) -> Expect.equal e.Message err.Message "completeWith should complete with effect error"
            | _ -> failtest "Expected Some Error result"
    ]
