module FIO.Tests.Factories.TaskInteropTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Task / Async adapters"
                [
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

                    testCase "awaitTask - a throwing onError resumed on a worker thread surfaces the task error instead of crashing the process" (fun () ->
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

                            match runtime.Run(effect).UnsafeResult() with
                            | Interrupted ex ->
                                match ex.cause with
                                | Defect defect ->
                                    Expect.stringContains
                                        defect.Message
                                        "task boom"
                                        $"{runtime.GetType().Name}: throwing onError must not crash; the raw task error should surface as the defect"
                                | other -> failtest $"{runtime.GetType().Name}: expected a Defect cause but got {other}"
                            | other -> failtest $"{runtime.GetType().Name}: expected Interrupted but got {other}")

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
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
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
                ]

            testList
                "Callback adapter"
                [
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
                        let runtime = new WorkStealingRuntime() :> FIORuntime
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
                        let runtime = new WorkStealingRuntime() :> FIORuntime
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
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
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
                ]
        ]
