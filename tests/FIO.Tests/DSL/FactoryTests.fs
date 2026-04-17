module FIO.Tests.FactoryTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto
open FsCheck

open System
open System.Threading.Tasks
open System.Diagnostics

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let factoryTests =
    testList
        "Factory Functions"
        [

            testPropertyWithConfig fsCheckConfig "unit - returns unit"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.unit ()

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "FIO.unit should return unit"

            testPropertyWithConfig fsCheckConfig "succeed - returns the provided value"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.succeed value

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.succeed should return the provided value"

            testPropertyWithConfig fsCheckConfig "fail - fails with the provided error"
            <| fun (runtime: FIORuntime, error: string) ->
                let eff = FIO.fail error

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result error "FIO.fail should fail with the provided error"

            testPropertyWithConfig fsCheckConfig "interrupt - results in Interrupted fiber"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.interrupt (ExplicitInterrupt, "test interrupt")

                let fiber = runtime.Run eff
                let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

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

            testPropertyWithConfig fsCheckConfig "interrupt - with Timeout cause"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.interrupt<int, exn> (Timeout 1000.0, "timeout test")

                let fiber = runtime.Run eff
                let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                match fiberResult with
                | Interrupted ex ->
                    Expect.equal
                        ex.fiberId
                        fiber.Id
                        "FIO.interrupt should set the fiberId in the exception to the current fiber's ID"

                    Expect.equal
                        ex.cause
                        (Timeout 1000.0)
                        "FIO.interrupt should set the provided Timeout cause in the exception"

                    Expect.equal
                        ex.message
                        "timeout test"
                        "FIO.interrupt should set the provided message in the exception"
                | _ -> failtest "FIO.interrupt with Timeout should result in Interrupted"

            testPropertyWithConfig fsCheckConfig "interrupt - with ParentInterrupted cause"
            <| fun (runtime: FIORuntime) ->
                let parentGuid = Guid.NewGuid()

                let eff =
                    FIO.interrupt<int, exn> (ParentInterrupted parentGuid, "parent interrupt test")

                let fiber = runtime.Run eff
                let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

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
                let eff =
                    FIO.interrupt<int, exn> (ResourceExhaustion "out of memory", "resource test")

                let fiber = runtime.Run eff
                let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

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
                let eff =
                    FIO.interrupt<int, exn> (InvalidArgument("param", "bad value"), "invalid arg test")

                let fiber = runtime.Run eff
                let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

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
                let eff = FIO.attempt ((fun () -> value), (fun ex -> ex.Message))

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.attempt should return the function result"

            testPropertyWithConfig fsCheckConfig "attempt - maps exception to error when function throws"
            <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
                let msg = errorMsg.Get
                let eff = FIO.attempt ((fun () -> raise (Exception msg)), (fun ex -> ex.Message))

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result msg "FIO.attempt should map exception to error"

            testPropertyWithConfig fsCheckConfig "attemptExn - succeeds when function succeeds"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.attempt ((fun () -> value), id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.attemptExn should return the function result"

            testPropertyWithConfig fsCheckConfig "attemptExn - passes through exception"
            <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
                let msg = errorMsg.Get
                let ex = Exception msg
                let eff = FIO.attempt ((fun () -> raise ex), id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result.Message msg "FIO.attemptExn should pass through exception"

            testPropertyWithConfig fsCheckConfig "fromResult - converts Ok to success"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.fromResult (Ok value)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.fromResult should convert Ok to success"

            testPropertyWithConfig fsCheckConfig "fromResult - converts Error to fail"
            <| fun (runtime: FIORuntime, error: string) ->
                let eff = FIO.fromResult (Error error)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result error "FIO.fromResult should convert Error to fail"

            testPropertyWithConfig fsCheckConfig "fromOption - converts Some to success"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.fromOption (Some value, fun () -> "none error")

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.fromOption should convert Some to success"

            testPropertyWithConfig fsCheckConfig "fromOption - converts None to error using onNone"
            <| fun (runtime: FIORuntime, error: string) ->
                let eff = FIO.fromOption (None, fun () -> error)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result error "FIO.fromOption should convert None to error using onNone"

            testPropertyWithConfig fsCheckConfig "fromChoice - converts Choice1Of2 to success"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.fromChoice (Choice1Of2 value)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.fromChoice should convert Choice1Of2 to success"

            testPropertyWithConfig fsCheckConfig "fromChoice - converts Choice2Of2 to fail"
            <| fun (runtime: FIORuntime, error: string) ->
                let eff = FIO.fromChoice (Choice2Of2 error)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result error "FIO.fromChoice should convert Choice2Of2 to fail"

            testPropertyWithConfig fsCheckConfig "awaitTask - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.awaitTask (Task.CompletedTask, fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "FIO.awaitTask should complete successfully"

            testAllRuntimes "awaitTask - maps exception on faulted task" (fun runtime ->
                let eff =
                    FIO.awaitTask (Task.FromException(Exception "task failed"), fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "task failed" "FIO.awaitTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitTaskExn - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.awaitTask (Task.CompletedTask, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "FIO.awaitTaskExn should complete successfully"

            testPropertyWithConfig fsCheckConfig "awaitTaskExn - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "test error"
                let faultedTask = Task.FromException ex
                let eff = FIO.awaitTask (faultedTask, id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result.Message "test error" "FIO.awaitTaskExn should propagate exception"

            testPropertyWithConfig fsCheckConfig "awaitGenericTask - returns task result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.awaitGenericTask (Task.FromResult value, fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitGenericTask should return task result"

            testAllRuntimes "awaitGenericTask - maps exception on faulted task" (fun runtime ->
                let eff =
                    FIO.awaitGenericTask (
                        Task.FromException<int>(Exception "generic task failed"),
                        fun ex -> ex.Message
                    )

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "generic task failed" "FIO.awaitGenericTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitGenericTaskExn - returns task result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.awaitGenericTask (Task.FromResult value, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitGenericTaskExn should return task result"

            testPropertyWithConfig fsCheckConfig "awaitGenericTaskExn - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "generic task error"
                let faultedTask = Task.FromException<int> ex
                let eff = FIO.awaitGenericTask (faultedTask, id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic task error"
                    "FIO.awaitGenericTaskExn should propagate exception"

            testPropertyWithConfig fsCheckConfig "awaitAsync - returns async result"
            <| fun (runtime: FIORuntime, value: int) ->
                let asyncComp = async { return value }
                let eff = FIO.awaitAsync (asyncComp, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitAsync should return async result"

            testAllRuntimes "awaitAsync - maps exception on failed async" (fun runtime ->
                let asyncComp = async { return failwith "async failed" }
                let eff = FIO.awaitAsync (asyncComp, fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result "async failed" "FIO.awaitAsync should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitAsyncExn - returns async result"
            <| fun (runtime: FIORuntime, value: int) ->
                let asyncComp = async { return value }
                let eff = FIO.awaitAsync (asyncComp, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitAsyncExn should return async result"

            testAllRuntimes "awaitAsyncExn - propagates exception on failed async" (fun runtime ->
                let asyncComp = async { return failwith "async exn failed" }
                let eff = FIO.awaitAsync (asyncComp, id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result.Message "async exn failed" "FIO.awaitAsyncExn should propagate exception")

            testAllRuntimes "fromTask - forks task into fiber" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        let! fiber =
                            FIO.fromTask (
                                (fun () ->
                                    executed <- true
                                    Task.CompletedTask),
                                id
                            )

                        let! result = fiber.Join()
                        return executed, result
                    }

                let wasExecuted, _ = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue wasExecuted "FIO.fromTask should execute the task")

            testAllRuntimes "fromTask - propagates faulted task as error" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.fromTask ((fun () -> Task.FromException(Exception "task err")), fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "task err" "FIO.fromTask should propagate faulted task error")

            testAllRuntimes "fromTaskExn - forks task into fiber" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        let! fiber =
                            FIO.fromTask (
                                (fun () ->
                                    executed <- true
                                    Task.CompletedTask),
                                id
                            )

                        let! _ = fiber.Join()
                        return executed
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "FIO.fromTaskExn should execute the task")

            testAllRuntimes "fromTaskExn - propagates faulted task as exception" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.fromTask ((fun () -> Task.FromException(Exception "task exn err")), id)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result.Message "task exn err" "FIO.fromTaskExn should propagate exception")

            testAllRuntimes "fromGenericTask - forks generic task into fiber" (fun runtime ->
                let value = 42

                let eff =
                    fio {
                        let! fiber = FIO.fromGenericTask ((fun () -> Task.FromResult value), id)
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.fromGenericTask should return task result")

            testAllRuntimes "fromGenericTask - propagates faulted task as error" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.fromGenericTask (
                                (fun () -> Task.FromException<int>(Exception "generic err")),
                                fun ex -> ex.Message
                            )

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "generic err" "FIO.fromGenericTask should propagate faulted task error")

            testAllRuntimes "fromGenericTaskExn - forks generic task into fiber" (fun runtime ->
                let value = 42

                let eff =
                    fio {
                        let! fiber = FIO.fromGenericTask ((fun () -> Task.FromResult value), id)
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.fromGenericTaskExn should return task result")

            testAllRuntimes "fromGenericTaskExn - propagates faulted task as exception" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.fromGenericTask ((fun () -> Task.FromException<int>(Exception "generic exn err")), id)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic exn err"
                    "FIO.fromGenericTaskExn should propagate exception")

            testPropertyWithConfig fsCheckConfig "suspend - defers effect construction"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable constructed = false

                let eff =
                    FIO.suspend (fun () ->
                        constructed <- true
                        FIO.succeed value)

                Expect.isFalse constructed "Effect should not be constructed before run"

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue constructed "Effect should be constructed after run"
                Expect.equal result value "FIO.suspend should return the inner effect result"

            testPropertyWithConfig fsCheckConfig "suspend - allows recursive effs"
            <| fun (runtime: FIORuntime) ->
                let rec countdown n =
                    if n <= 0 then
                        FIO.succeed n
                    else
                        FIO.suspend (fun () -> countdown (n - 1))

                let result = runtime.Run(countdown 100).UnsafeSuccess()
                Expect.equal result 0 "Recursive suspend should work"

            testPropertyWithConfig fsCheckConfig "sleep - delays execution"
            <| fun (runtime: FIORuntime) ->
                let duration = TimeSpan.FromMilliseconds 20.0

                let eff =
                    fio {
                        let sw = Stopwatch.StartNew()
                        do! FIO.sleep (duration, id)
                        sw.Stop()
                        return sw.Elapsed
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isGreaterThanOrEqual result.TotalMilliseconds 15.0 "FIO.sleep should delay execution"

            testPropertyWithConfig fsCheckConfig "sleepExn - delays execution"
            <| fun (runtime: FIORuntime) ->
                let duration = TimeSpan.FromMilliseconds 15.0

                let eff =
                    fio {
                        let sw = Stopwatch.StartNew()
                        do! FIO.sleep (duration, id)
                        sw.Stop()
                        return sw.Elapsed
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isGreaterThanOrEqual result.TotalMilliseconds 10.0 "FIO.sleepExn should delay execution"

            testAllRuntimes "never - can be raced against completing effect" (fun runtime ->
                let value = 42
                let eff = (FIO.never<int, exn> ()).Race(FIO.succeed value)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Race with FIO.never should return the completing effect's result")

            testPropertyWithConfig fsCheckConfig "acquireRelease - runs release on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable released = false
                let acquire = FIO.succeed "resource"

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.succeed value

                let eff = FIO.acquireRelease (acquire, release, useResource)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue released "Release should be called on success"
                Expect.equal result value "Should return use result"

            testPropertyWithConfig fsCheckConfig "acquireRelease - runs release on use failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable released = false
                let acquire = FIO.succeed "resource"

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.fail error

                let eff = FIO.acquireRelease (acquire, release, useResource)

                let result = runtime.Run(eff).UnsafeError()

                Expect.isTrue released "Release should be called even on use failure"
                Expect.equal result error "Should return use error"

            testPropertyWithConfig fsCheckConfig "acquireRelease - does not run release when acquire fails"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable released = false
                let acquire = FIO.fail error

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.succeed 42

                let eff = FIO.acquireRelease (acquire, release, useResource)

                let result = runtime.Run(eff).UnsafeError()

                Expect.isFalse released "Release should not be called when acquire fails"
                Expect.equal result error "Should return acquire error"

            testPropertyWithConfig fsCheckConfig "acquireRelease - releases in reverse order when nested"
            <| fun (runtime: FIORuntime) ->
                let mutable releaseOrder = []
                let acquire1 = FIO.succeed "r1"

                let release1 =
                    fun _ -> FIO.attempt((fun () -> releaseOrder <- releaseOrder @ [ 1 ]), id).Unit()

                let acquire2 = FIO.succeed "r2"

                let release2 =
                    fun _ -> FIO.attempt((fun () -> releaseOrder <- releaseOrder @ [ 2 ]), id).Unit()

                let eff =
                    FIO.acquireRelease (
                        acquire1,
                        release1,
                        fun _ -> FIO.acquireRelease (acquire2, release2, fun _ -> FIO.succeed 42)
                    )

                let _ = runtime.Run(eff).UnsafeSuccess()
                Expect.equal releaseOrder [ 2; 1 ] "Nested resources should release in reverse order"

            testPropertyWithConfig fsCheckConfig "collectAll - preserves order"
            <| fun (runtime: FIORuntime) ->
                let mutable order = []

                let effs =
                    [
                        FIO.attempt (
                            (fun () ->
                                order <- order @ [ 1 ]
                                1),
                            id
                        )
                        FIO.attempt (
                            (fun () ->
                                order <- order @ [ 2 ]
                                2),
                            id
                        )
                        FIO.attempt (
                            (fun () ->
                                order <- order @ [ 3 ]
                                3),
                            id
                        )
                    ]

                let eff = FIO.collectAll effs

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "collectAll should return results in order"
                Expect.equal order [ 1; 2; 3 ] "collectAll should execute in order"

            testPropertyWithConfig fsCheckConfig "collectAll - with single element returns singleton list"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.collectAll [ FIO.succeed value ]

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ value ] "collectAll with single element should return singleton list"

            testPropertyWithConfig fsCheckConfig "collectAll - with empty collection returns empty list"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.collectAll []

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [] "collectAll with empty collection should return empty list"

            testPropertyWithConfig fsCheckConfig "collectAll - short-circuits on error"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable executed = []

                let effs =
                    [
                        FIO.attempt (
                            (fun () ->
                                executed <- executed @ [ 1 ]
                                1),
                            fun _ -> "err"
                        )
                        FIO.fail error
                        FIO.attempt (
                            (fun () ->
                                executed <- executed @ [ 3 ]
                                3),
                            fun _ -> "err"
                        )
                    ]

                let eff = FIO.collectAll effs

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result error "collectAll should return the error"
                Expect.equal executed [ 1 ] "collectAll should not execute effs after error"

            testPropertyWithConfig fsCheckConfig "collectAll - large input preserves order"
            <| fun (runtime: FIORuntime) ->
                let items = [ 1..2_000 ]
                let effs = items |> List.map FIO.succeed
                let result = runtime.Run(FIO.collectAll effs).UnsafeSuccess()
                Expect.equal result items "collectAll should preserve order for large collections"

            testAllRuntimes "collectAllPar - returns all results" (fun runtime ->
                let effs = [ FIO.succeed 1; FIO.succeed 2; FIO.succeed 3 ]

                let eff = FIO.collectAllPar effs

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "collectAllPar should return all results in order")

            testAllRuntimes "collectAllPar - preserves result order" (fun runtime ->
                let effs =
                    [
                        FIO.sleep(TimeSpan.FromMilliseconds 30.0, id).FlatMap(fun () -> FIO.succeed 3)
                        FIO.sleep(TimeSpan.FromMilliseconds 10.0, id).FlatMap(fun () -> FIO.succeed 1)
                        FIO.sleep(TimeSpan.FromMilliseconds 20.0, id).FlatMap(fun () -> FIO.succeed 2)
                    ]

                let result = runtime.Run(FIO.collectAllPar effs).UnsafeSuccess()

                Expect.equal result [ 3; 1; 2 ] "collectAllPar should preserve order regardless of completion time")

            testAllRuntimes "collectAllPar - with empty list returns empty" (fun runtime ->
                let result = runtime.Run(FIO.collectAllPar []).UnsafeSuccess()

                Expect.equal result [] "collectAllPar with empty list should return empty")

            testAllRuntimes "collectAllPar - propagates first error" (fun runtime ->
                let effs = [ FIO.succeed 1; FIO.fail "error"; FIO.succeed 3 ]

                let result = runtime.Run(FIO.collectAllPar effs).UnsafeError()

                Expect.equal result "error" "collectAllPar should propagate error")

            testAllRuntimes "collectAllPar - large input preserves order" (fun runtime ->
                let items = [ 1..500 ]

                let effs =
                    items
                    |> List.map (fun i ->
                        FIO.sleep(TimeSpan.FromMilliseconds(float (i % 5 + 1)), id).FlatMap(fun () -> FIO.succeed i))

                let result = runtime.Run(FIO.collectAllPar effs).UnsafeSuccess()

                Expect.equal result items "collectAllPar should preserve input order for large collections")

            testAllRuntimes "collectAllPar - large input propagates first failure" (fun runtime ->
                let effs =
                    [ for i in 1..1_000 -> if i = 100 then FIO.fail "boom" else FIO.succeed i ]

                let result = runtime.Run(FIO.collectAllPar effs).UnsafeError()

                Expect.equal result "boom" "collectAllPar should propagate first failure for large input")

            testPropertyWithConfig fsCheckConfig "forEach - maps and collects sequentially"
            <| fun (runtime: FIORuntime) ->
                let mutable order = []
                let items = [ 1; 2; 3 ]

                let f =
                    fun x ->
                        FIO.attempt (
                            (fun () ->
                                order <- order @ [ x ]
                                x * 2),
                            id
                        )

                let eff = FIO.forEach (items, f)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 2; 4; 6 ] "forEach should map and collect results"
                Expect.equal order [ 1; 2; 3 ] "forEach should execute sequentially in order"

            testPropertyWithConfig fsCheckConfig "forEach - with empty list returns empty"
            <| fun (runtime: FIORuntime) ->
                let result =
                    runtime.Run(FIO.forEach ([], fun x -> FIO.succeed (x * 2))).UnsafeSuccess()

                Expect.equal result [] "forEach with empty list should return empty"

            testPropertyWithConfig fsCheckConfig "forEach - short-circuits on error"
            <| fun (runtime: FIORuntime) ->
                let mutable executed = []
                let items = [ 1; 2; 3 ]

                let f =
                    fun (x: int) ->
                        if x = 2 then
                            FIO.fail "boom"
                        else
                            FIO.attempt (
                                (fun () ->
                                    executed <- executed @ [ x ]
                                    x),
                                fun _ -> "err"
                            )

                let result = runtime.Run(FIO.forEach (items, f)).UnsafeError()

                Expect.equal result "boom" "forEach should propagate error"
                Expect.equal executed [ 1 ] "forEach should not execute effs after error"

            testAllRuntimes "forEachPar - maps and collects in parallel" (fun runtime ->
                let items = [ 1; 2; 3 ]
                let f = fun x -> FIO.succeed (x * 2)

                let eff = FIO.forEachPar (items, f)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result [ 2; 4; 6 ] "forEachPar should map and collect results in order")

            testAllRuntimes "forEachPar - preserves result order" (fun runtime ->
                let items = [ 1; 2; 3; 4; 5 ]

                let f =
                    fun x ->
                        FIO
                            .sleep(TimeSpan.FromMilliseconds(float (10 * (6 - x))), id)
                            .FlatMap(fun () -> FIO.succeed (x * 2))

                let result = runtime.Run(FIO.forEachPar (items, f)).UnsafeSuccess()

                Expect.equal result [ 2; 4; 6; 8; 10 ] "forEachPar should preserve order")

            testAllRuntimes "forEachPar - with empty list returns empty" (fun runtime ->
                let result =
                    runtime.Run(FIO.forEachPar ([], fun x -> FIO.succeed (x * 2))).UnsafeSuccess()

                Expect.equal result [] "forEachPar with empty list should return empty")

            testAllRuntimes "forEachPar - propagates error" (fun runtime ->
                let items = [ 1; 2; 3 ]
                let f = fun (x: int) -> if x = 2 then FIO.fail "boom" else FIO.succeed (x * 2)

                let result = runtime.Run(FIO.forEachPar (items, f)).UnsafeError()

                Expect.equal result "boom" "forEachPar should propagate error")
        ]
