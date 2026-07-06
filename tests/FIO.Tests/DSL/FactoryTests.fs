module FIO.Tests.FactoryTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto
open FsCheck

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime() :> FIORuntime
        new WorkStealingRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let factoryTests =
    testList
        "Factory Functions"
        [
            // ─── Immediate constructors ─────────────────────────────────────────

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

            // ─── Lift from standard types ─────────────────────────────────────────

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

            // ─── Suspension / fiber-context primitives ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "suspend - defers effect construction"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable constructed = false

                let effect =
                    FIO.suspend (fun () ->
                        constructed <- true
                        FIO.succeed value)

                Expect.isFalse constructed "Effect should not be constructed before run"

                let result =
                    runtime.Run(effect).UnsafeSuccess()

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

            // ─── Task / Async adapters ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "awaitUnitTask - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.awaitUnitTask Task.CompletedTask (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "FIO.awaitUnitTask should complete successfully"

            testAllRuntimes "awaitUnitTask - maps exception on faulted task" (fun runtime ->
                let effect =
                    FIO.awaitUnitTask (Task.FromException(Exception "task failed")) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "task failed" "FIO.awaitUnitTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitUnitTask - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "test error"
                let faultedTask = Task.FromException ex
                let effect = FIO.awaitUnitTask faultedTask id

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.stringContains result.Message "test error" "FIO.awaitUnitTask should propagate exception"

            testPropertyWithConfig fsCheckConfig "awaitTask - returns task result"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect = FIO.awaitTask (Task.FromResult value) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "FIO.awaitTask should return task result"

            testAllRuntimes "awaitTask - maps exception on faulted task" (fun runtime ->
                let effect = FIO.awaitTask (Task.FromException<int>(Exception "generic task failed")) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "generic task failed" "FIO.awaitTask should map exception to error")

            testPropertyWithConfig fsCheckConfig "awaitTask - propagates exception"
            <| fun (runtime: FIORuntime) ->
                let ex = Exception "generic task error"
                let faultedTask = Task.FromException<int> ex
                let effect = FIO.awaitTask faultedTask id

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic task error"
                    "FIO.awaitTask should propagate exception"

            // Regression: the worker runtimes resume a parked AwaitTask via a foreign-thread
            // continuation. A throwing onError there must not escape as an unhandled thread-pool
            // exception (which would crash the process); the raw task error surfaces instead.
            testCase "awaitTask - throwing onError on a worker runtime fails gracefully" (fun () ->
                let workerRuntimes: FIORuntime list =
                    [ new PollingRuntime()
                      new SignalingRuntime()
                      new WorkStealingRuntime() ]

                for runtime in workerRuntimes do
                    let faulting: Task<int> =
                        task {
                            do! Task.Delay 5
                            return raise (Exception "task boom")
                        }

                    let throwingOnError: exn -> exn = fun _ -> raise (Exception "onError threw")
                    let effect = FIO.awaitTask faulting throwingOnError

                    let result =
                        runtime.Run(effect).UnsafeError()

                    Expect.stringContains
                        result.Message
                        "task boom"
                        $"{runtime.GetType().Name}: throwing onError must not crash; raw task error should surface")

            testPropertyWithConfig fsCheckConfig "awaitAsync - returns async result"
            <| fun (runtime: FIORuntime, value: int) ->
                let asyncComp = async { return value }
                let effect = FIO.awaitAsync asyncComp id

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "FIO.awaitAsync should return async result"

            testAllRuntimes "awaitAsync - maps exception on failed async" (fun runtime ->
                let asyncComp = async { return failwith "async failed" }
                let effect = FIO.awaitAsync asyncComp (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.stringContains result "async failed" "FIO.awaitAsync should map exception to error")

            testAllRuntimes "awaitAsync - propagates exception on failed async" (fun runtime ->
                let asyncComp = async { return failwith "async failed" }
                let effect = FIO.awaitAsync asyncComp id

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.stringContains result.Message "async failed" "FIO.awaitAsync should propagate exception")

            testCase "awaitAsync - does not start async at construction time" (fun () ->
                let mutable started = false

                let asyncComp =
                    async {
                        started <- true
                        return 42
                    }

                let _eff = FIO.awaitAsync asyncComp id

                Expect.isFalse started "Async should not be started at effect construction time")

            testAllRuntimes "awaitAsync - interruption cancels the underlying async" (fun runtime ->
                let asyncComp =
                    async {
                        do! Async.Sleep 60_000
                        return 42
                    }

                let effect =
                    fio {
                        let! fiber = (FIO.awaitAsync asyncComp (fun ex -> ex.Message)).Fork()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun ex -> ex.Message)
                        return! fiber.InterruptAwaitNow ()
                    }

                let sw = Stopwatch.StartNew()
                let result = runtime.Run(effect).UnsafeSuccess()
                sw.Stop()

                match result with
                | Interrupted _ -> ()
                | other -> failtestf "Expected Interrupted, got %A" other

                Expect.isLessThan
                    sw.Elapsed.TotalSeconds
                    5.0
                    "awaitAsync should cancel the underlying async on fiber interrupt, not run for 60 seconds")

            testAllRuntimes "forkUnitTask - forks task into fiber" (fun runtime ->
                let mutable executed = false

                let effect =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask
                                (fun () ->
                                    executed <- true
                                    Task.CompletedTask)
                                id

                        let! result = fiber.Join()
                        return executed, result
                    }

                let wasExecuted, _ =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isTrue wasExecuted "FIO.forkUnitTask should execute the task")

            testCase "forkUnitTask - does not allocate fiber at construction time" (fun () ->
                let mutable taskStarted = false

                let _eff =
                    FIO.forkUnitTask
                        (fun () ->
                            taskStarted <- true
                            Task.CompletedTask)
                        id

                Expect.isFalse taskStarted "Task should not be started at effect construction time")

            testAllRuntimes "forkUnitTask - propagates faulted task as error" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask
                                (fun () -> Task.FromException(Exception "task error"))
                                (fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "task error" "FIO.forkUnitTask should propagate faulted task error")

            testAllRuntimes "forkUnitTask - propagates faulted task as exception" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask
                                (fun () ->
                                    Task.FromException(Exception "task error"))
                                id

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.stringContains result.Message "task error" "FIO.forkUnitTask should propagate exception")

            testCase "forkTask - does not allocate fiber at construction time" (fun () ->
                let mutable taskStarted = false

                let _eff =
                    FIO.forkTask
                        (fun () ->
                            taskStarted <- true
                            Task.FromResult 42)
                        id

                Expect.isFalse taskStarted "Task should not be started at effect construction time")

            testAllRuntimes "forkTask - forks generic task into fiber" (fun runtime ->
                let value = 42

                let effect =
                    fio {
                        let! fiber =
                            FIO.forkTask
                                (fun () -> Task.FromResult value)
                                id
                        let! result = fiber.Join()
                        return result
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "FIO.forkTask should return task result")

            testAllRuntimes "forkTask - propagates faulted task as error" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkTask
                                (fun () -> Task.FromException<int>(Exception "generic error"))
                                (fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "generic error" "FIO.forkTask should propagate faulted task error")

            testAllRuntimes "forkTask - propagates faulted task as exception" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkTask
                                (fun () -> Task.FromException<int>(Exception "generic error"))
                                id

                        let! result = fiber.Join()
                        return result
                    }

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.stringContains
                    result.Message
                    "generic error"
                    "FIO.forkTask should propagate exception")

            testAllRuntimes "forkUnitTask - outer error channel independent of inner" (fun runtime ->
                let effect: FIO<int, int> =
                    (FIO.forkUnitTask
                        (fun () -> Task.CompletedTask)
                        (fun ex -> ex.Message))
                        .FlatMap(fun _ -> FIO.succeed 1)

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1 "Outer error channel should be free to differ from the inner fiber's error type")

            testAllRuntimes "forkUnitTask - synchronous throw from factory surfaces via onError" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkUnitTask
                                (fun () -> failwith "boom")
                                (fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result "boom" "Synchronous throw from taskFactory should reach onError")

            testAllRuntimes "forkTask - synchronous throw from factory surfaces via onError" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            FIO.forkTask
                                (fun () -> failwith "boom": Task<int>)
                                (fun ex -> ex.Message)

                        let! result = fiber.Join()
                        return result
                    }

                let result = runtime.Run(effect).UnsafeError()

                Expect.equal result "boom" "Synchronous throw from taskFactory should reach onError")

            // ─── Callback adapter ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "async - synchronous Ok callback yields success"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    FIO.async (fun cb -> cb (Ok value)) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "async should complete with the Ok value"

            testPropertyWithConfig fsCheckConfig "async - synchronous Error callback yields failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect: FIO<int, string> =
                    FIO.async (fun cb -> cb (Error error)) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result error "async should fail with the Error value"

            testCase "async - delayed callback completes correctly"
            <| fun () ->
                let runtime: FIORuntime = new WorkStealingRuntime() :> FIORuntime
                let effect =
                    FIO.async (fun cb ->
                        let _ = Task.Run(fun () ->
                            Thread.Sleep 20
                            cb (Ok 42))
                        ()) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 42 "async should complete when the callback fires after a delay"

            testCase "async - subsequent callback invocations are ignored"
            <| fun () ->
                let runtime: FIORuntime = new WorkStealingRuntime() :> FIORuntime
                let effect =
                    FIO.async (fun cb ->
                        cb (Ok 1)
                        cb (Ok 2)
                        cb (Error "boom")) (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1 "async should record only the first callback invocation"

            testAllRuntimes "async - register that throws synchronously surfaces error via onError" (fun runtime ->
                let effect: FIO<int, string> =
                    FIO.async
                        (fun _ -> failwith "register threw")
                        (fun ex -> ex.Message)

                let result = runtime.Run(effect).UnsafeError()

                Expect.stringContains
                    result
                    "register threw"
                    "Synchronous throw in register should surface via onError, not deadlock the fiber")

            testAllRuntimes "async - interruption cancels the awaiting fiber" (fun runtime ->
                let effect =
                    fio {
                        let! fiber =
                            (FIO.async (fun _ -> ()) (fun ex -> ex.Message): FIO<int, string>).Fork()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun ex -> ex.Message)
                        return! fiber.InterruptAwaitNow ()
                    }

                let sw = Stopwatch.StartNew()
                let result = runtime.Run(effect).UnsafeSuccess()
                sw.Stop()

                match result with
                | Interrupted _ -> ()
                | other -> failtestf "Expected Interrupted, got %A" other

                Expect.isLessThan
                    sw.Elapsed.TotalSeconds
                    5.0
                    "async should unblock on fiber interrupt when no callback ever fires")

            // ─── Time / scheduling ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfigFast "sleep - delays execution"
            <| fun (runtime: FIORuntime) ->
                let duration = TimeSpan.FromMilliseconds 20.0

                let effect =
                    fio {
                        let sw = Stopwatch.StartNew()
                        do! FIO.sleep duration id
                        sw.Stop()
                        return sw.Elapsed
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isGreaterThanOrEqual result.TotalMilliseconds 15.0 "FIO.sleep should delay execution"

            testAllRuntimes "sleep - interruption stops the underlying delay" (fun runtime ->
                let effect =
                    fio {
                        let! fiber = (FIO.sleep (TimeSpan.FromMinutes 1.0) (fun ex -> ex.Message)).Fork()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun ex -> ex.Message)
                        return! fiber.InterruptAwaitNow ()
                    }

                let sw = Stopwatch.StartNew()
                let result = runtime.Run(effect).UnsafeSuccess()
                sw.Stop()

                match result with
                | Interrupted _ -> ()
                | other -> failtestf "Expected Interrupted, got %A" other

                Expect.isLessThan
                    sw.Elapsed.TotalSeconds
                    5.0
                    "sleep should cancel the underlying Task.Delay on fiber interrupt, not tick for the full minute")

            testPropertyWithConfig fsCheckConfig "yieldNow - completes successfully"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.yieldNow (fun ex -> ex.Message)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "yieldNow should complete with unit"

            testPropertyWithConfig fsCheckConfig "yieldNow - sequences correctly with subsequent effects"
            <| fun (runtime: FIORuntime, value: int) ->
                let effect =
                    (FIO.yieldNow (fun ex -> ex.Message)).FlatMap(fun () -> FIO.succeed value)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "yieldNow should sequence into subsequent effects"

            testAllRuntimes "never - can be raced against completing effect" (fun runtime ->
                let value = 42
                let effect = FIO.never().RaceFirst(FIO.succeed value)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result value "RaceFirst with FIO.never should return the completing effect's result")

            // ─── Resource management ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "acquireReleaseWith - runs release on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable released = false
                let acquire = FIO.succeed "resource"

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.succeed value

                let effect = FIO.acquireReleaseWith acquire release useResource

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.isTrue released "Release should be called on success"
                Expect.equal result value "Should return use result"

            testPropertyWithConfig fsCheckConfig "acquireReleaseWith - runs release on use failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable released = false
                let acquire = FIO.succeed "resource"

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.fail error

                let effect = FIO.acquireReleaseWith acquire release useResource

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.isTrue released "Release should be called even on use failure"
                Expect.equal result error "Should return use error"

            testPropertyWithConfig fsCheckConfig "acquireReleaseWith - does not run release when acquire fails"
            <| fun (runtime: FIORuntime, error: string) ->
                let mutable released = false
                let acquire = FIO.fail error

                let release =
                    fun _ ->
                        released <- true
                        FIO.unit ()

                let useResource = fun _ -> FIO.succeed 42

                let effect = FIO.acquireReleaseWith acquire release useResource

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.isFalse released "Release should not be called when acquire fails"
                Expect.equal result error "Should return acquire error"

            testPropertyWithConfig fsCheckConfig "acquireReleaseWith - releases in reverse order when nested"
            <| fun (runtime: FIORuntime) ->
                let mutable releaseOrder = []
                let acquire1 = FIO.succeed "r1"

                let release1 =
                    fun _ -> (FIO.attempt (fun () -> releaseOrder <- releaseOrder @ [ 1 ]) id).Unit()

                let acquire2 = FIO.succeed "r2"

                let release2 =
                    fun _ -> (FIO.attempt (fun () -> releaseOrder <- releaseOrder @ [ 2 ]) id).Unit()

                let effect =
                    FIO.acquireReleaseWith
                        acquire1
                        release1
                        (fun _ -> FIO.acquireReleaseWith acquire2 release2 (fun _ -> FIO.succeed 42))

                let _ =
                    runtime.Run(effect).UnsafeSuccess()
                Expect.equal releaseOrder [ 2; 1 ] "Nested resources should release in reverse order"

            // ─── Sequential traversals ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "forEach - empty input yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.forEach [] FIO.succeed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "forEach over an empty seq should yield []"

            testPropertyWithConfig fsCheckConfig "forEach - round-trip identity over list of ints"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.forEach xs FIO.succeed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "forEach with FIO.succeed should preserve input"

            testPropertyWithConfig fsCheckConfig "forEach - applies f to each input in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.forEach xs (fun i -> FIO.succeed (i + 1))

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.map (fun i -> i + 1) xs) "forEach should apply f to each input"

            testPropertyWithConfig fsCheckConfig "forEach - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0

                let f i =
                    (FIO.attempt
                        (fun () -> Interlocked.Increment(&callCount) |> ignore)
                        (fun ex -> ex.Message)
                    ).FlatMap(fun () ->
                        if i = 2 then FIO.fail "boom"
                        else FIO.succeed i)

                let effect = FIO.forEach [ 1; 2; 3; 4 ] f

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "boom" "forEach should fail with the first error"
                Expect.equal callCount 2 "forEach should not invoke f after failure"

            testCase "forEach - stack-safe over 10000 items"
            <| fun () ->
                for runtime in runtimes () do
                    let xs = [ 1 .. 10000 ]
                    let effect = FIO.forEach xs FIO.succeed

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length result) 10000 $"forEach on {runtime.GetType().Name} should handle 10000 items"

            testPropertyWithConfig fsCheckConfig "forEachDiscard - empty input completes with unit"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.forEachDiscard [] FIO.succeed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "forEachDiscard over an empty seq should yield ()"

            testPropertyWithConfig fsCheckConfig "forEachDiscard - applies f to each input without collecting"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let mutable sum = 0

                let f i =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Add(&sum, i) |> ignore)
                        id

                let effect = FIO.forEachDiscard xs f

                let _ =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal sum (List.sum xs) "forEachDiscard should invoke f for every input"

            testPropertyWithConfig fsCheckConfig "forEachDiscard - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0

                let f i =
                    (FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        (fun ex -> ex.Message)
                    ).FlatMap(fun () ->
                        if i = 2 then FIO.fail "boom"
                        else FIO.succeed ())

                let effect = FIO.forEachDiscard [ 1; 2; 3; 4 ] f

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result "boom" "forEachDiscard should fail with the first error"
                Expect.equal callCount 2 "forEachDiscard should not invoke f after failure"

            testCase "forEachDiscard - stack-safe over 10000 items"
            <| fun () ->
                for runtime in runtimes () do
                    let xs = [ 1 .. 10000 ]
                    let effect = FIO.forEachDiscard xs (fun _ -> FIO.unit ())

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result () $"forEachDiscard on {runtime.GetType().Name} should handle 10000 items"

            // ─── Parallel traversals ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "forEachPar - empty input yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.forEachPar [] FIO.succeed

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "forEachPar over an empty seq should yield []"

            testPropertyWithConfig fsCheckConfigFast "forEachPar - preserves input order despite parallel execution"
            <| fun (runtime: FIORuntime) ->
                let rnd = Random()
                let xs = [ 1 .. 50 ]

                let f i =
                    FIO.attempt
                        (fun () ->
                            Thread.Sleep(rnd.Next(0, 3))
                            i)
                        id

                let effect = FIO.forEachPar xs f

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "forEachPar should preserve input order"

            testCase "forEachPar - fails with one of the errors and interrupts peers"
            <| fun () ->
                for runtime in runtimes () do
                    let mutable peerCompleted = 0
                    let started = new ManualResetEventSlim(false)
                    let failureItems = 5

                    let f i =
                        if i = 0 then
                            (FIO.attempt
                                (fun () -> started.Wait(TimeSpan.FromSeconds 1.0) |> ignore)
                                (fun ex -> ex.Message)
                            ).FlatMap(fun () -> FIO.fail "boom")
                        else
                            (FIO.attempt
                                (fun () -> started.Set())
                                (fun ex -> ex.Message))
                                .FlatMap(fun () ->
                                    FIO.sleep (TimeSpan.FromMilliseconds 200.0) (fun ex -> ex.Message))
                                .FlatMap(fun () ->
                                    FIO.attempt
                                        (fun () -> Interlocked.Increment(&peerCompleted) |> ignore)
                                        (fun ex -> ex.Message))

                    let effect = FIO.forEachPar [ 0 .. failureItems ] f

                    let error =
                        runtime.Run(effect).UnsafeError()

                    Expect.equal error "boom" $"forEachPar on {runtime.GetType().Name} should propagate the failure"
                    Expect.isLessThan peerCompleted failureItems $"forEachPar on {runtime.GetType().Name} should interrupt at least one peer"

            testCase "forEachPar - fails fast even when an earlier peer never terminates"
            <| fun () ->
                for runtime in runtimes () do
                    let sentinel = -1

                    let effect =
                        (FIO.forEachPar [ 0; 1 ] (fun i ->
                            if i = 0 then FIO.never<int, int>() else FIO.fail 99))
                            .TimeoutFail sentinel (TimeSpan.FromSeconds 2.0) (fun _ -> sentinel)

                    let error =
                        runtime.Run(effect).UnsafeError()

                    Expect.equal error 99 $"forEachPar on {runtime.GetType().Name} should observe the late failure without hanging on the never-terminating earlier peer"

            testPropertyWithConfig fsCheckConfig "forEachParDiscard - applies f to each input without collecting"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let mutable sum = 0

                let f i =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Add(&sum, i) |> ignore)
                        id

                let effect = FIO.forEachParDiscard xs f

                let _ =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal sum (List.sum xs) "forEachParDiscard should invoke f for every input"

            // ─── Collect aliases (over seq<FIO>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "collectAll - mirrors forEach with id"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed

                let effect = FIO.collectAll effects

                let result
                    = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "collectAll should collect successes in order"

            testPropertyWithConfig fsCheckConfig "collectAllDiscard - completes with unit"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed

                let effect = FIO.collectAllDiscard effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "collectAllDiscard should complete with unit"

            testPropertyWithConfig fsCheckConfig "collectAllPar - mirrors forEachPar with id"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed

                let effect = FIO.collectAllPar effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "collectAllPar should collect successes in source order"

            testPropertyWithConfig fsCheckConfig "collectAllParDiscard - completes with unit"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed

                let effect = FIO.collectAllParDiscard effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "collectAllParDiscard should complete with unit"

            // ─── Repetition ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "replicateFIO - zero iterations yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.replicateFIO 0 (FIO.succeed 42)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "replicateFIO 0 should yield []"

            testPropertyWithConfig fsCheckConfig "replicateFIO - negative iterations clamp to empty list"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        id

                let result =
                    runtime.Run(FIO.replicateFIO -100 effect).UnsafeSuccess()

                Expect.equal result [] "replicateFIO with negative n should yield []"
                Expect.equal callCount 0 "replicateFIO with negative n should not evaluate the effect"

            testPropertyWithConfig fsCheckConfig "replicateFIO - yields List.replicate of a constant success"
            <| fun (runtime: FIORuntime) ->
                let n = 5
                let effect = FIO.replicateFIO n (FIO.succeed 7)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.replicate n 7) "replicateFIO should yield n copies of the result"

            testPropertyWithConfig fsCheckConfig "replicateFIO - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let effect =
                    (FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount))
                        (fun ex -> ex.Message)
                    ).FlatMap(fun count ->
                        if count = 3 then FIO.fail "boom"
                        else FIO.succeed count)

                let error =
                    runtime.Run(FIO.replicateFIO 10 effect).UnsafeError()

                Expect.equal error "boom" "replicateFIO should fail with the first error"
                Expect.equal callCount 3 "replicateFIO should not evaluate further iterations after failure"

            testCase "replicateFIO - stack-safe over 10000 iterations"
            <| fun () ->
                for runtime in runtimes () do
                    let effect = FIO.replicateFIO 10000 (FIO.unit ())

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length result) 10000 $"replicateFIO on {runtime.GetType().Name} should handle 10000 iterations"

            testPropertyWithConfig fsCheckConfig "replicateFIODiscard - zero iterations yields unit"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.replicateFIODiscard 0 (FIO.succeed 42)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result () "replicateFIODiscard 0 should yield ()"

            testPropertyWithConfig fsCheckConfig "replicateFIODiscard - negative iterations clamp to unit"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let effect =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        id

                let result =
                    runtime.Run(FIO.replicateFIODiscard -1 effect).UnsafeSuccess()

                Expect.equal result () "replicateFIODiscard with negative n should yield ()"
                Expect.equal callCount 0 "replicateFIODiscard with negative n should not evaluate the effect"

            testPropertyWithConfig fsCheckConfig "replicateFIODiscard - invokes the effect exactly n times"
            <| fun (runtime: FIORuntime) ->
                let n = 25
                let mutable callCount = 0

                let effect =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        id

                let result = runtime.Run(FIO.replicateFIODiscard n effect).UnsafeSuccess()

                Expect.equal result () "replicateFIODiscard should complete with unit"
                Expect.equal callCount n "replicateFIODiscard should invoke the effect exactly n times"

            testPropertyWithConfig fsCheckConfig "replicateFIODiscard - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let effect =
                    (FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount))
                        (fun ex -> ex.Message)
                    ).FlatMap(fun count ->
                        if count = 2 then FIO.fail "boom"
                        else FIO.succeed ())

                let error =
                    runtime.Run(FIO.replicateFIODiscard 10 effect).UnsafeError()

                Expect.equal error "boom" "replicateFIODiscard should fail with the first error"
                Expect.equal callCount 2 "replicateFIODiscard should not evaluate further iterations after failure"

            testCase "replicateFIODiscard - stack-safe over 10000 iterations"
            <| fun () ->
                for runtime in runtimes () do
                    let effect = FIO.replicateFIODiscard 10000 (FIO.unit ())

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result () $"replicateFIODiscard on {runtime.GetType().Name} should handle 10000 iterations"

            // ─── Stateful loops ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "loop - cont false on initial yields empty list and never invokes body"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body s =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore
                            s)
                        id

                let result =
                    runtime.Run(FIO.loop 0 (fun _ -> false) ((+) 1) body).UnsafeSuccess()

                Expect.equal result [] "loop with false-cont should yield []"
                Expect.equal callCount 0 "loop with false-cont should not invoke body"

            testPropertyWithConfig fsCheckConfig "loop - standard counted iteration collects results in order"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    FIO.loop 0 (fun s -> s < 5) ((+) 1) (fun s -> FIO.succeed (s * 2))

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 0; 2; 4; 6; 8 ] "loop should collect body results in iteration order"

            testPropertyWithConfig fsCheckConfig "loop - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body s =
                    (FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        (fun ex -> ex.Message)
                    ).FlatMap(fun () ->
                        if s = 3 then FIO.fail "boom"
                        else FIO.succeed s)

                let error =
                    runtime.Run(FIO.loop 0 (fun s -> s < 10) ((+) 1) body).UnsafeError()

                Expect.equal error "boom" "loop should fail with the first error"
                Expect.equal callCount 4 "loop should not invoke body after failure"

            testCase "loop - stack-safe over 10000 iterations"
            <| fun () ->
                for runtime in runtimes () do
                    let effect =
                        FIO.loop 0 (fun s -> s < 10000) ((+) 1) (fun _ -> FIO.unit ())

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length result) 10000 $"loop on {runtime.GetType().Name} should handle 10000 iterations"

            testPropertyWithConfig fsCheckConfig "loopDiscard - cont false on initial yields unit and never invokes body"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body _ =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        id

                let result =
                    runtime.Run(FIO.loopDiscard 0 (fun _ -> false) ((+) 1) body).UnsafeSuccess()

                Expect.equal result () "loopDiscard with false-cont should yield ()"
                Expect.equal callCount 0 "loopDiscard with false-cont should not invoke body"

            testPropertyWithConfig fsCheckConfig "loopDiscard - invokes body exactly n times"
            <| fun (runtime: FIORuntime) ->
                let n = 25
                let mutable callCount = 0
                let body _ =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&callCount) |> ignore)
                        id

                let result =
                    runtime.Run(FIO.loopDiscard 0 (fun s -> s < n) ((+) 1) body).UnsafeSuccess()

                Expect.equal result () "loopDiscard should complete with unit"
                Expect.equal callCount n "loopDiscard should invoke body exactly n times"

            testPropertyWithConfig fsCheckConfig "loopDiscard - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body s =
                    (FIO.attempt
                        (fun () -> Interlocked.Increment(&callCount) |> ignore)
                        (fun ex -> ex.Message)
                    ).FlatMap(fun () ->
                        if s = 2 then FIO.fail "boom"
                        else FIO.succeed ())

                let error =
                    runtime.Run(FIO.loopDiscard 0 (fun s -> s < 10) ((+) 1) body).UnsafeError()

                Expect.equal error "boom" "loopDiscard should fail with the first error"
                Expect.equal callCount 3 "loopDiscard should not invoke body after failure"

            testCase "loopDiscard - stack-safe over 10000 iterations"
            <| fun () ->
                for runtime in runtimes () do
                    let effect =
                        FIO.loopDiscard 0 (fun s -> s < 10000) ((+) 1) (fun _ -> FIO.unit ())

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result () $"loopDiscard on {runtime.GetType().Name} should handle 10000 iterations"

            testPropertyWithConfig fsCheckConfig "iterate - cont false on initial returns initial without invoking body"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body s =
                    FIO.attempt(
                        fun () ->
                            Interlocked.Increment(&callCount) |> ignore
                            s + 1)
                        id

                let result =
                    runtime.Run(FIO.iterate 42 (fun _ -> false) body).UnsafeSuccess()

                Expect.equal result 42 "iterate with false-cont should yield initial state"
                Expect.equal callCount 0 "iterate with false-cont should not invoke body"

            testPropertyWithConfig fsCheckConfig "iterate - standard counted iteration drives state through body"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    FIO.iterate 0 (fun s -> s < 10) (fun s -> FIO.succeed (s + 1))

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 10 "iterate should return the final state for which cont is false"

            testPropertyWithConfig fsCheckConfig "iterate - short-circuits on body failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let body s =
                    (FIO.attempt
                        (fun () -> Interlocked.Increment(&callCount))
                        (fun ex -> ex.Message)
                    ).FlatMap(fun count ->
                        if count = 4 then FIO.fail "boom"
                        else FIO.succeed (s + 1))

                let error =
                    runtime.Run(FIO.iterate 0 (fun s -> s < 100) body).UnsafeError()

                Expect.equal error "boom" "iterate should fail with the first error"
                Expect.equal callCount 4 "iterate should not invoke body after failure"

            testCase "iterate - stack-safe over 10000 iterations"
            <| fun () ->
                for runtime in runtimes () do
                    let effect =
                        FIO.iterate 0 (fun s -> s < 10000) (fun s -> FIO.succeed (s + 1))

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result 10000 $"iterate on {runtime.GetType().Name} should handle 10000 iterations"

            // ─── Aggregation (fold / reduce) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "mergeAll - empty input yields zero"
            <| fun (runtime: FIORuntime, zero: int) ->
                let effect = FIO.mergeAll [] zero (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result zero "mergeAll over an empty seq should yield zero"

            testPropertyWithConfig fsCheckConfig "mergeAll - folds successful results left-to-right"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed
                let effect = FIO.mergeAll effects 0 (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.sum xs) "mergeAll should equal List.fold over the successes"

            testPropertyWithConfig fsCheckConfig "mergeAll - short-circuits on first failure"
            <| fun (runtime: FIORuntime) ->
                let mutable callCount = 0
                let mk i =
                    (FIO.attempt
                        (fun () -> Interlocked.Increment(&callCount) |> ignore)
                        (fun ex -> ex.Message)
                    ).FlatMap(fun () ->
                        if i = 3 then FIO.fail "boom"
                        else FIO.succeed i)
                let effects = [ 0 .. 9 ] |> List.map mk

                let error =
                    runtime.Run(FIO.mergeAll effects 0 (+)).UnsafeError()

                Expect.equal error "boom" "mergeAll should fail with the first error"
                Expect.equal callCount 4 "mergeAll should not evaluate further effects after failure"

            testCase "mergeAll - stack-safe over 10000 effects"
            <| fun () ->
                for runtime in runtimes () do
                    let effects = [ 1 .. 10000 ] |> List.map FIO.succeed

                    let effect = FIO.mergeAll effects 0 (+)

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result 50005000 $"mergeAll on {runtime.GetType().Name} should fold 10000 effects"

            testPropertyWithConfig fsCheckConfig "mergeAllPar - empty input yields zero"
            <| fun (runtime: FIORuntime, zero: int) ->
                let effect = FIO.mergeAllPar [] zero (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result zero "mergeAllPar over an empty seq should yield zero"

            testPropertyWithConfig fsCheckConfig "mergeAllPar - folds successful results in source order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed
                let effect = FIO.mergeAllPar effects 0 (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.sum xs) "mergeAllPar should equal List.fold over the successes"

            testPropertyWithConfig fsCheckConfig "mergeAllPar - fails with one of the errors"
            <| fun (runtime: FIORuntime) ->
                let effects =
                    [ FIO.succeed 1
                      FIO.fail "boom"
                      FIO.succeed 2 ]

                let error =
                    runtime.Run(FIO.mergeAllPar effects 0 (+)).UnsafeError()

                Expect.equal error "boom" "mergeAllPar should propagate the failure"

            testPropertyWithConfig fsCheckConfig "reduceAll - empty tail returns head's result"
            <| fun (runtime: FIORuntime, x: int) ->
                let effect = FIO.reduceAll (FIO.succeed x) [] (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result x "reduceAll with empty tail should return head's result"

            testPropertyWithConfig fsCheckConfig "reduceAll - reduces head + tail left-to-right"
            <| fun (runtime: FIORuntime, h: int, tail: int list) ->
                let tailEffects = tail |> List.map FIO.succeed
                let effect = FIO.reduceAll (FIO.succeed h) tailEffects (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.fold (+) h tail) "reduceAll should equal List.fold over head + tail"

            testPropertyWithConfig fsCheckConfig "reduceAll - short-circuits on head failure"
            <| fun (runtime: FIORuntime) ->
                let mutable tailEvaluated = 0
                let mkTail i =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&tailEvaluated) |> ignore
                            i)
                        (fun ex -> ex.Message)
                let tail = [ 1; 2; 3 ] |> List.map mkTail

                let error =
                    runtime.Run(FIO.reduceAll (FIO.fail "boom") tail (+)).UnsafeError()

                Expect.equal error "boom" "reduceAll should fail with head's error"
                Expect.equal tailEvaluated 0 "reduceAll should not evaluate tail after head failure"

            testCase "reduceAll - stack-safe over 10000 effects"
            <| fun () ->
                for runtime in runtimes () do
                    let tail = [ 1 .. 9999 ] |> List.map FIO.succeed
                    let effect = FIO.reduceAll (FIO.succeed 0) tail (+)

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal result 49995000 $"reduceAll on {runtime.GetType().Name} should reduce 10000 effects"

            testPropertyWithConfig fsCheckConfig "reduceAllPar - empty tail returns head's result"
            <| fun (runtime: FIORuntime, x: int) ->
                let effect = FIO.reduceAllPar (FIO.succeed x) [] (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result x "reduceAllPar with empty tail should return head's result"

            testPropertyWithConfig fsCheckConfig "reduceAllPar - reduces head + tail in source order"
            <| fun (runtime: FIORuntime, h: int, tail: int list) ->
                let tailEffects = tail |> List.map FIO.succeed
                let effect = FIO.reduceAllPar (FIO.succeed h) tailEffects (+)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (List.fold (+) h tail) "reduceAllPar should equal List.fold over head + tail"

            testPropertyWithConfig fsCheckConfig "reduceAllPar - fails when head fails"
            <| fun (runtime: FIORuntime) ->
                let tail = [ FIO.succeed 1; FIO.succeed 2 ]

                let error =
                    runtime.Run(FIO.reduceAllPar (FIO.fail "boom") tail (+)).UnsafeError()

                Expect.equal error "boom" "reduceAllPar should fail with head's error"

            // ─── Error-tolerant traversals ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "partition - empty input yields ([], [])"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.partition [] FIO.succeed

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [] "partition over an empty seq should yield no errors"
                Expect.equal oks [] "partition over an empty seq should yield no successes"

            testPropertyWithConfig fsCheckConfig "partition - all-success collects every result and no errors"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.partition xs (fun x -> FIO.succeed x)

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [] "partition with all-success should produce no errors"
                Expect.equal oks xs "partition with all-success should preserve input order"

            testPropertyWithConfig fsCheckConfig "partition - all-failure collects every error and no successes"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let f x = FIO.fail (string x)
                let effect = FIO.partition xs f

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs (List.map string xs) "partition with all-failure should preserve input order in errors"
                Expect.equal oks [] "partition with all-failure should produce no successes"

            testPropertyWithConfig fsCheckConfig "partition - mixed input splits and preserves order in both lists"
            <| fun (runtime: FIORuntime) ->
                let xs = [ 0; 1; 2; 3; 4; 5 ]
                let f x =
                    if x % 2 = 0 then FIO.succeed x
                    else FIO.fail (string x)
                let effect = FIO.partition xs f

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [ "1"; "3"; "5" ] "partition should preserve input order in errors"
                Expect.equal oks [ 0; 2; 4 ] "partition should preserve input order in successes"

            testCase "partition - stack-safe over 10000 items"
            <| fun () ->
                for runtime in runtimes () do
                    let xs = [ 1 .. 10000 ]
                    let f x =
                        if x % 2 = 0 then FIO.succeed x
                        else FIO.fail x
                    let effect = FIO.partition xs f

                    let errs, oks =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length errs) 5000 $"partition on {runtime.GetType().Name} should collect 5000 errors"
                    Expect.equal (List.length oks) 5000 $"partition on {runtime.GetType().Name} should collect 5000 successes"

            testPropertyWithConfig fsCheckConfig "partitionPar - empty input yields ([], [])"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.partitionPar [] FIO.succeed

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [] "partitionPar over an empty seq should yield no errors"
                Expect.equal oks [] "partitionPar over an empty seq should yield no successes"

            testPropertyWithConfig fsCheckConfig "partitionPar - all-success collects every result and preserves order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.partitionPar xs (fun x -> FIO.succeed x)

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [] "partitionPar with all-success should produce no errors"
                Expect.equal oks xs "partitionPar with all-success should preserve input order"

            testPropertyWithConfig fsCheckConfig "partitionPar - all-failure collects every error and preserves order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let f x = FIO.fail (string x)
                let effect = FIO.partitionPar xs f

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs (List.map string xs) "partitionPar with all-failure should preserve input order in errors"
                Expect.equal oks [] "partitionPar with all-failure should produce no successes"

            testPropertyWithConfig fsCheckConfig "partitionPar - mixed input splits and preserves order in both lists"
            <| fun (runtime: FIORuntime) ->
                let xs = [ 0; 1; 2; 3; 4; 5 ]
                let f x =
                    if x % 2 = 0 then FIO.succeed x
                    else FIO.fail (string x)
                let effect = FIO.partitionPar xs f

                let errs, oks =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal errs [ "1"; "3"; "5" ] "partitionPar should preserve input order in errors"
                Expect.equal oks [ 0; 2; 4 ] "partitionPar should preserve input order in successes"

            testCase "partitionPar - does not interrupt siblings on failure"
            <| fun () ->
                for runtime in runtimes () do
                    let mutable completed = 0
                    let n = 10
                    let f i =
                        (FIO.attempt
                            (fun () ->
                                Interlocked.Increment(&completed) |> ignore)
                            (fun ex -> ex.Message)
                        ).FlatMap(fun () ->
                            if i = 0 then FIO.fail "boom"
                            else FIO.succeed i)

                    let effect = FIO.partitionPar [ 0 .. n - 1 ] f

                    let errs, oks =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal errs [ "boom" ] $"partitionPar on {runtime.GetType().Name} should collect the failure"
                    Expect.equal oks [ 1 .. n - 1 ] $"partitionPar on {runtime.GetType().Name} should let siblings complete"
                    Expect.equal completed n $"partitionPar on {runtime.GetType().Name} should not interrupt siblings on failure"

            testPropertyWithConfig fsCheckConfig "validate - empty input yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.validate [] (fun x -> FIO.succeed x)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "validate over an empty seq should yield []"

            testPropertyWithConfig fsCheckConfig "validate - all-success returns the result list in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.validate xs (fun x -> FIO.succeed x)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "validate with all-success should preserve input order"

            testPropertyWithConfig fsCheckConfig "validate - all-failure fails with every error in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                match xs with
                | [] -> ()
                | _ ->
                    let f x = FIO.fail (string x)
                    let effect = FIO.validate xs f

                    let errs =
                        runtime.Run(effect).UnsafeError()

                    Expect.equal errs (List.map string xs) "validate with all-failure should preserve input order in errors"

            testPropertyWithConfig fsCheckConfig "validate - mixed input fails with only the errors"
            <| fun (runtime: FIORuntime) ->
                let xs = [ 0; 1; 2; 3; 4; 5 ]
                let f x =
                    if x % 2 = 0 then FIO.succeed x
                    else FIO.fail (string x)
                let effect = FIO.validate xs f

                let errs =
                    runtime.Run(effect).UnsafeError()

                Expect.equal errs [ "1"; "3"; "5" ] "validate should fail with only the per-input errors in source order"

            testCase "validate - stack-safe over 10000 items"
            <| fun () ->
                for runtime in runtimes () do
                    let xs = [ 1 .. 10000 ]

                    let effect = FIO.validate xs (fun x -> FIO.succeed x)

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length result) 10000 $"validate on {runtime.GetType().Name} should handle 10000 items"

            testPropertyWithConfig fsCheckConfig "validatePar - empty input yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.validatePar [] (fun x -> FIO.succeed x)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "validatePar over an empty seq should yield []"

            testPropertyWithConfig fsCheckConfig "validatePar - all-success returns the result list in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effect = FIO.validatePar xs (fun x -> FIO.succeed x)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "validatePar with all-success should preserve input order"

            testPropertyWithConfig fsCheckConfig "validatePar - all-failure fails with every error in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                match xs with
                | [] -> ()
                | _ ->
                    let f x = FIO.fail (string x)
                    let effect = FIO.validatePar xs f

                    let errs =
                        runtime.Run(effect).UnsafeError()

                    Expect.equal errs (List.map string xs) "validatePar with all-failure should preserve input order in errors"

            testPropertyWithConfig fsCheckConfig "validatePar - mixed input fails with only the errors in source order"
            <| fun (runtime: FIORuntime) ->
                let xs = [ 0; 1; 2; 3; 4; 5 ]
                let f x =
                    if x % 2 = 0 then FIO.succeed x
                    else FIO.fail (string x)
                let effect = FIO.validatePar xs f

                let errs =
                    runtime.Run(effect).UnsafeError()

                Expect.equal errs [ "1"; "3"; "5" ] "validatePar should fail with only the per-input errors in source order"

            testCase "validatePar - does not interrupt siblings on failure"
            <| fun () ->
                for runtime in runtimes () do
                    let mutable completed = 0
                    let n = 10
                    let f i =
                        (FIO.attempt
                            (fun () ->
                                Interlocked.Increment(&completed) |> ignore)
                            (fun ex -> ex.Message)
                        ).FlatMap(fun () ->
                            if i = 0 then FIO.fail "boom"
                            else FIO.succeed i)
                    let effect = FIO.validatePar [ 0 .. n - 1 ] f

                    let errs =
                        runtime.Run(effect).UnsafeError()

                    Expect.equal errs [ "boom" ] $"validatePar on {runtime.GetType().Name} should collect the failure"
                    Expect.equal completed n $"validatePar on {runtime.GetType().Name} should not interrupt siblings on failure"

            testPropertyWithConfig fsCheckConfig "collectAllSuccesses - empty input yields empty list"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.collectAllSuccesses []

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "collectAllSuccesses over an empty seq should yield []"

            testPropertyWithConfig fsCheckConfig "collectAllSuccesses - all-success returns every result in order"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map FIO.succeed
                let effect = FIO.collectAllSuccesses effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result xs "collectAllSuccesses with all-success should return every result in source order"

            testPropertyWithConfig fsCheckConfig "collectAllSuccesses - all-failure returns empty list"
            <| fun (runtime: FIORuntime, xs: int list) ->
                let effects = xs |> List.map (fun x -> FIO.fail (string x))
                let effect = FIO.collectAllSuccesses effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [] "collectAllSuccesses with all-failure should yield []"

            testPropertyWithConfig fsCheckConfig "collectAllSuccesses - mixed input drops failures and keeps successes in order"
            <| fun (runtime: FIORuntime) ->
                let effects =
                    [ FIO.succeed 1
                      FIO.fail "a"
                      FIO.succeed 2
                      FIO.fail "b"
                      FIO.succeed 3 ]
                let effect = FIO.collectAllSuccesses effects

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "collectAllSuccesses should drop failures and keep successes in source order"

            testCase "collectAllSuccesses - stack-safe over 10000 effects"
            <| fun () ->
                for runtime in runtimes () do
                    let effects = [ 1 .. 10000 ] |> List.map FIO.succeed
                    let effect = FIO.collectAllSuccesses effects

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal (List.length result) 10000 $"collectAllSuccesses on {runtime.GetType().Name} should handle 10000 effects"

            // ─── Branching ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "ifFIO - true predicate runs onTrue branch"
            <| fun (runtime: FIORuntime, x: int, y: int) ->
                let effect = FIO.ifFIO (FIO.succeed true) (FIO.succeed x) (FIO.succeed y)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result x "ifFIO with true predicate should yield onTrue's result"

            testPropertyWithConfig fsCheckConfig "ifFIO - false predicate runs onFalse branch"
            <| fun (runtime: FIORuntime, x: int, y: int) ->
                let effect = FIO.ifFIO (FIO.succeed false) (FIO.succeed x) (FIO.succeed y)

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result y "ifFIO with false predicate should yield onFalse's result"

            testPropertyWithConfig fsCheckConfig "ifFIO - predicate failure propagates without running either branch"
            <| fun (runtime: FIORuntime) ->
                let mutable branchEvaluated = 0
                let mk =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&branchEvaluated) |> ignore
                            0)
                        (fun ex -> ex.Message)
                let effect = FIO.ifFIO (FIO.fail "boom") mk mk

                let error =
                    runtime.Run(effect).UnsafeError()

                Expect.equal error "boom" "ifFIO should propagate predicate failure"
                Expect.equal branchEvaluated 0 "ifFIO should not evaluate either branch on predicate failure"

            testPropertyWithConfig fsCheckConfig "ifFIO - only the selected branch is evaluated"
            <| fun (runtime: FIORuntime, pick: bool) ->
                let mutable trueRan = 0
                let mutable falseRan = 0
                let onTrue =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&trueRan) |> ignore
                            1)
                        (fun ex -> ex.Message)
                let onFalse =
                    FIO.attempt
                        (fun () ->
                            Interlocked.Increment(&falseRan) |> ignore
                            2)
                        (fun ex -> ex.Message)

                let _ =
                    runtime.Run(FIO.ifFIO (FIO.succeed pick) onTrue onFalse).UnsafeSuccess()

                if pick then
                    Expect.equal trueRan 1 "onTrue should run when predicate is true"
                    Expect.equal falseRan 0 "onFalse should NOT run when predicate is true"
                else
                    Expect.equal trueRan 0 "onTrue should NOT run when predicate is false"
                    Expect.equal falseRan 1 "onFalse should run when predicate is false"

            testPropertyWithConfig fsCheckConfig "ifFIO - failure in selected branch propagates"
            <| fun (runtime: FIORuntime, pick: bool) ->
                let other = FIO.succeed 0
                let failing = FIO.fail "branch boom"
                let effect =
                    if pick then FIO.ifFIO (FIO.succeed true) failing other
                    else FIO.ifFIO (FIO.succeed false) other failing

                let error =
                    runtime.Run(effect).UnsafeError()

                Expect.equal error "branch boom" "ifFIO should propagate failure from the selected branch"

            // ─── Option unwrapping ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "someOrFail - Some unwraps to success"
            <| fun (runtime: FIORuntime, value: int, error: string) ->
                let effect = FIO.succeed (Some value)

                let result =
                    runtime.Run(effect |> FIO.someOrFail error).UnsafeSuccess()

                Expect.equal result value "someOrFail should unwrap Some to the underlying value"

            testPropertyWithConfig fsCheckConfig "someOrFail - None fails with supplied error"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.succeed None

                let result =
                    runtime.Run(effect |> FIO.someOrFail error).UnsafeError()

                Expect.equal result error "someOrFail should fail with the supplied error on None"

            testPropertyWithConfig fsCheckConfig "someOrFail - original failure propagates"
            <| fun (runtime: FIORuntime, originalError: string, replacement: string) ->
                let effect = FIO.fail originalError

                let result =
                    runtime.Run(effect |> FIO.someOrFail replacement).UnsafeError()

                Expect.equal result originalError "someOrFail should propagate the original failure unchanged"

            testPropertyWithConfig fsCheckConfig "someOrElse - Some unwraps to success"
            <| fun (runtime: FIORuntime, value: int, fallback: int) ->
                let effect = FIO.succeed (Some value)

                let result =
                    runtime.Run(effect |> FIO.someOrElse fallback).UnsafeSuccess()

                Expect.equal result value "someOrElse should unwrap Some to the underlying value"

            testPropertyWithConfig fsCheckConfig "someOrElse - None substitutes the default"
            <| fun (runtime: FIORuntime, fallback: int) ->
                let effect = FIO.succeed None

                let result =
                    runtime.Run(effect |> FIO.someOrElse fallback).UnsafeSuccess()

                Expect.equal result fallback "someOrElse should substitute the default value on None"

            testPropertyWithConfig fsCheckConfig "someOrElseFIO - Some unwraps to success without evaluating the fallback"
            <| fun (runtime: FIORuntime, value: int) ->
                let mutable evaluated = 0
                let fallback =
                    FIO.attempt
                        (fun () ->
                            evaluated <- evaluated + 1
                            -1)
                        (fun ex -> ex.Message)
                let effect = FIO.succeed (Some value)

                let result =
                    runtime.Run(effect |> FIO.someOrElseFIO fallback).UnsafeSuccess()

                Expect.equal result value "someOrElseFIO should unwrap Some to the underlying value"
                Expect.equal evaluated 0 "someOrElseFIO should not evaluate the fallback when Some"

            testPropertyWithConfig fsCheckConfig "someOrElseFIO - None runs the fallback effect"
            <| fun (runtime: FIORuntime, fallbackValue: int) ->
                let effect = FIO.succeed None

                let result =
                    runtime.Run(effect |> FIO.someOrElseFIO (FIO.succeed fallbackValue)).UnsafeSuccess()

                Expect.equal result fallbackValue "someOrElseFIO should evaluate the fallback effect on None"

            testPropertyWithConfig fsCheckConfig "someOrElseFIO - None propagates fallback failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.succeed None

                let result =
                    runtime.Run(effect |> FIO.someOrElseFIO (FIO.fail error)).UnsafeError()

                Expect.equal result error "someOrElseFIO should propagate the fallback's failure on None"

            // ─── First-success alternatives ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "firstSuccessOf - empty tail returns head's outcome on success"
            <| fun (runtime: FIORuntime, value: int) ->
                let head = FIO.succeed value

                let result =
                    runtime.Run(FIO.firstSuccessOf head Seq.empty).UnsafeSuccess()

                Expect.equal result value "firstSuccessOf with empty tail should yield head's success"

            testPropertyWithConfig fsCheckConfig "firstSuccessOf - empty tail propagates head's failure"
            <| fun (runtime: FIORuntime, error: string) ->
                let head = FIO.fail error

                let result =
                    runtime.Run(FIO.firstSuccessOf head Seq.empty).UnsafeError()

                Expect.equal result error "firstSuccessOf with empty tail should propagate head's failure"

            testPropertyWithConfig fsCheckConfig "firstSuccessOf - head succeeds returns head"
            <| fun (runtime: FIORuntime, value: int, tailValue: int) ->
                let head = FIO.succeed value
                let tail = seq { FIO.succeed tailValue; FIO.succeed (tailValue + 1) }

                let result =
                    runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                Expect.equal result value "firstSuccessOf should return head when it succeeds"

            testPropertyWithConfig fsCheckConfig "firstSuccessOf - first successful tail entry wins"
            <| fun (runtime: FIORuntime, winner: int) ->
                let head = FIO.fail "h"
                let tail = seq {
                    FIO.fail "t0"
                    FIO.succeed winner
                    FIO.succeed (winner + 1)
                }

                let result =
                    runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                Expect.equal result winner "firstSuccessOf should return the first successful tail entry"

            testPropertyWithConfig fsCheckConfig "firstSuccessOf - all fail returns last failure"
            <| fun (runtime: FIORuntime, lastError: string) ->
                let head = FIO.fail "h"
                let tail = seq {
                    FIO.fail "t0"
                    FIO.fail "t1"
                    FIO.fail lastError
                }

                let result =
                    runtime.Run(FIO.firstSuccessOf head tail).UnsafeError()

                Expect.equal result lastError "firstSuccessOf should return the last failure when all fail"

            testCase "firstSuccessOf - only evaluates effects up to the first success"
            <| fun () ->
                let runtime: FIORuntime = new WorkStealingRuntime() :> FIORuntime
                let mutable evaluated = 0
                let bump effect =
                    FIO.suspend (fun () ->
                        evaluated <- evaluated + 1
                        effect)
                let head = bump (FIO.fail "h")
                let tail = seq {
                    bump (FIO.fail "t0")
                    bump (FIO.succeed 42)
                    bump (FIO.succeed 99)
                }

                let result =
                    runtime.Run(FIO.firstSuccessOf head tail).UnsafeSuccess()

                Expect.equal result 42 "firstSuccessOf should yield the first success"
                Expect.equal evaluated 3 "firstSuccessOf should stop evaluating once an effect succeeds"

            testPropertyWithConfig fsCheckConfig "raceAll - empty sequence interrupts with InvalidArgument"
            <| fun (runtime: FIORuntime) ->
                let effect = FIO.raceAll Seq.empty

                let fiber = runtime.Run effect
                let fiberResult =
                    fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

                match fiberResult with
                | Interrupted ex ->
                    Expect.equal
                        ex.cause
                        (InvalidArgument("effects", "sequence must not be empty"))
                        "raceAll should interrupt with InvalidArgument for an empty sequence"
                | _ -> failtest "raceAll over empty sequence should result in Interrupted"

            testAllRuntimes "raceAll - single-element sequence passes through success" (fun runtime ->
                let effect = FIO.raceAll (seq { FIO.succeed 42 })

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 42 "raceAll over a single success should yield that value")

            testAllRuntimes "raceAll - single-element sequence propagates failure" (fun runtime ->
                let error = exn "single failure"
                let effect = FIO.raceAll (seq { FIO.fail error })

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.equal result.Message error.Message "raceAll over a single failure should propagate that error")

            testAllRuntimes "raceAll - fastest success wins" (fun runtime ->
                let fast = FIO.succeed 1
                let medium =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 2)
                let slow =
                    (FIO.sleep (TimeSpan.FromSeconds 20.0) id).FlatMap(fun () -> FIO.succeed 3)
                let effect = FIO.raceAll (seq { fast; medium; slow })

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 1 "raceAll should return the fastest successful value")

            testAllRuntimes "raceAll - failures retire racers without winning" (fun runtime ->
                let fastFail1 = FIO.fail (exn "fast 1")
                let fastFail2 = FIO.fail (exn "fast 2")
                let slowSucceed =
                    (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).FlatMap(fun () -> FIO.succeed 99)
                let effect = FIO.raceAll (seq { fastFail1; fastFail2; slowSucceed })

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 99 "raceAll should wait for a success even when other racers fail fast")

            testAllRuntimes "raceAll - all fail surfaces one of the racers' errors" (fun runtime ->
                let fastFail = FIO.fail (exn "fast")
                let mediumFail =
                    (FIO.sleep (TimeSpan.FromMilliseconds 30.0) id).FlatMap(fun () -> FIO.fail (exn "medium"))
                let slowFail =
                    (FIO.sleep (TimeSpan.FromMilliseconds 80.0) id).FlatMap(fun () -> FIO.fail (exn "slow"))
                let effect = FIO.raceAll (seq { fastFail; mediumFail; slowFail })

                let result =
                    runtime.Run(effect).UnsafeError()

                Expect.contains
                    [ "fast"; "medium"; "slow" ]
                    result.Message
                    "raceAll should fail with one of the racers' error messages when all racers fail")
        ]
