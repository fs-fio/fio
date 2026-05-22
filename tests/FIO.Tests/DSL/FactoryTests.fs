/// <summary>Provides property-based tests for FIO effect factory functions such as attempt, fromResult, fromOption, and sleep.</summary>
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

            testPropertyWithConfig fsCheckConfig "awaitUnitTask - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.awaitUnitTask (Task.CompletedTask, fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "FIO.awaitUnitTask should complete successfully"

            testAllRuntimes "awaitUnitTask - maps exception on faulted task" (fun runtime ->
                let eff =
                    FIO.awaitUnitTask (Task.FromException(Exception "task failed"), fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "task failed" "FIO.awaitUnitTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitUnitTaskExn - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let eff = FIO.awaitUnitTask (Task.CompletedTask, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result () "FIO.awaitUnitTaskExn should complete successfully"

            testPropertyWithConfig fsCheckConfig "awaitUnitTaskExn - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "test error"
                let faultedTask = Task.FromException ex
                let eff = FIO.awaitUnitTask (faultedTask, id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result.Message "test error" "FIO.awaitUnitTaskExn should propagate exception"

            testPropertyWithConfig fsCheckConfig "awaitTask - returns task result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.awaitTask (Task.FromResult value, fun ex -> ex.Message)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitTask should return task result"

            testAllRuntimes "awaitTask - maps exception on faulted task" (fun runtime ->
                let eff =
                    FIO.awaitTask (
                        Task.FromException<int>(Exception "generic task failed"),
                        fun ex -> ex.Message
                    )

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "generic task failed" "FIO.awaitTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitTaskExn - returns task result"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = FIO.awaitTask (Task.FromResult value, id)

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.awaitTaskExn should return task result"

            testPropertyWithConfig fsCheckConfig "awaitTaskExn - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "generic task error"
                let faultedTask = Task.FromException<int> ex
                let eff = FIO.awaitTask (faultedTask, id)

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic task error"
                    "FIO.awaitTaskExn should propagate exception"

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

            testCase "awaitAsync - does not start async at construction time" (fun () ->
                let mutable started = false

                let asyncComp =
                    async {
                        started <- true
                        return 42
                    }

                let _eff = FIO.awaitAsync (asyncComp, id)

                Expect.isFalse started "Async should not be started at effect construction time")

            testAllRuntimes "forkUnitTask - forks task into fiber" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask (
                                (fun () ->
                                    executed <- true
                                    Task.CompletedTask),
                                id
                            )

                        let! result = fiber.Join()
                        return executed, result
                    }

                let wasExecuted, _ = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue wasExecuted "FIO.forkUnitTask should execute the task")

            testCase "forkUnitTask - does not allocate fiber at construction time" (fun () ->
                let mutable taskStarted = false

                let _eff =
                    FIO.forkUnitTask (
                        (fun () ->
                            taskStarted <- true
                            Task.CompletedTask),
                        id
                    )

                Expect.isFalse taskStarted "Task should not be started at effect construction time")

            testCase "forkTask - does not allocate fiber at construction time" (fun () ->
                let mutable taskStarted = false

                let _eff =
                    FIO.forkTask (
                        (fun () ->
                            taskStarted <- true
                            Task.FromResult 42),
                        id
                    )

                Expect.isFalse taskStarted "Task should not be started at effect construction time")

            testAllRuntimes "forkUnitTask - propagates faulted task as error" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask ((fun () -> Task.FromException(Exception "task err")), fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "task err" "FIO.forkUnitTask should propagate faulted task error")

            testAllRuntimes "forkUnitTaskExn - forks task into fiber" (fun runtime ->
                let mutable executed = false

                let eff =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask (
                                (fun () ->
                                    executed <- true
                                    Task.CompletedTask),
                                id
                            )

                        let! _ = fiber.Join()
                        return executed
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "FIO.forkUnitTaskExn should execute the task")

            testAllRuntimes "forkUnitTaskExn - propagates faulted task as exception" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask ((fun () -> Task.FromException(Exception "task exn err")), id)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains result.Message "task exn err" "FIO.forkUnitTaskExn should propagate exception")

            testAllRuntimes "forkTask - forks generic task into fiber" (fun runtime ->
                let value = 42

                let eff =
                    fio {
                        let! fiber = FIO.forkTask ((fun () -> Task.FromResult value), id)
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.forkTask should return task result")

            testAllRuntimes "forkTask - propagates faulted task as error" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.forkTask (
                                (fun () -> Task.FromException<int>(Exception "generic err")),
                                fun ex -> ex.Message
                            )

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.equal result "generic err" "FIO.forkTask should propagate faulted task error")

            testAllRuntimes "forkTaskExn - forks generic task into fiber" (fun runtime ->
                let value = 42

                let eff =
                    fio {
                        let! fiber = FIO.forkTask ((fun () -> Task.FromResult value), id)
                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "FIO.forkTaskExn should return task result")

            testAllRuntimes "forkTaskExn - propagates faulted task as exception" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            FIO.forkTask ((fun () -> Task.FromException<int>(Exception "generic exn err")), id)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(eff).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic exn err"
                    "FIO.forkTaskExn should propagate exception")

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
        ]
