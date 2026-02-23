module FIO.Tests.FactoryTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto
open FsCheck

open System
open System.Threading.Tasks

[<Tests>]
let factoryTests =
    testList "Factory Functions" [

        // 1. FIO.unit - Returns unit effect
        testPropertyWithConfig fsCheckConfig "FIO.unit returns unit"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.unit ()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "FIO.unit should return unit"

        // 2. FIO.succeed - Succeeds with value
        testPropertyWithConfig fsCheckConfig "FIO.succeed returns the provided value"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.succeed value

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.succeed should return the provided value"

        // 3. FIO.fail - Fails with error
        testPropertyWithConfig fsCheckConfig "FIO.fail fails with the provided error"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fail error

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fail should fail with the provided error"

        // 4. FIO.interrupt - Self-interruption
        testPropertyWithConfig fsCheckConfig "FIO.interrupt results in Interrupted fiber"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt (InterruptionCause.ExplicitInterrupt, "test interrupt")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt should result in Interrupted"

        // 5. FIO.attempt - Side-effecting function succeeds
        testPropertyWithConfig fsCheckConfig "FIO.attempt succeeds when function succeeds"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.attempt ((fun () -> value), (fun ex -> ex.Message))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.attempt should return the function result"

        // 6. FIO.attempt - Function throws, mapped error
        testPropertyWithConfig fsCheckConfig "FIO.attempt maps exception to error when function throws"
        <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
            let msg = errorMsg.Get
            let eff = FIO.attempt ((fun () -> raise (Exception msg)), (fun ex -> ex.Message))

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result msg "FIO.attempt should map exception to error"

        // 7. FIO.attemptExn - Exception passthrough
        testPropertyWithConfig fsCheckConfig "FIO.attemptExn passes through exception"
        <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
            let msg = errorMsg.Get
            let ex = Exception msg
            let eff = FIO.attemptExn (fun () -> raise ex)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result.Message msg "FIO.attemptExn should pass through exception"

        // 8. FIO.fromResult - Result.Ok to success
        testPropertyWithConfig fsCheckConfig "FIO.fromResult converts Ok to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromResult (Ok value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromResult should convert Ok to success"

        // 9. FIO.fromResult - Result.Error to fail
        testPropertyWithConfig fsCheckConfig "FIO.fromResult converts Error to fail"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromResult (Error error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromResult should convert Error to fail"

        // 10. FIO.fromOption - Some to success
        testPropertyWithConfig fsCheckConfig "FIO.fromOption converts Some to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromOption (Some value, fun () -> "none error")

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromOption should convert Some to success"

        // 11. FIO.fromOption - None to error (using onNone)
        testPropertyWithConfig fsCheckConfig "FIO.fromOption converts None to error using onNone"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromOption (None, fun () -> error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromOption should convert None to error using onNone"

        // 12. FIO.fromChoice - Choice1Of2 to success
        testPropertyWithConfig fsCheckConfig "FIO.fromChoice converts Choice1Of2 to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromChoice (Choice1Of2 value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromChoice should convert Choice1Of2 to success"

        // 13. FIO.fromChoice - Choice2Of2 to fail
        testPropertyWithConfig fsCheckConfig "FIO.fromChoice converts Choice2Of2 to fail"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromChoice (Choice2Of2 error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromChoice should convert Choice2Of2 to fail"

        // 14. FIO.awaitTask - Task completes successfully
        testPropertyWithConfig fsCheckConfig "FIO.awaitTask completes successfully"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.awaitTask (Task.CompletedTask, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "FIO.awaitTask should complete successfully"

        // 15. FIO.awaitGenericTask - Task<T> returns value
        testPropertyWithConfig fsCheckConfig "FIO.awaitGenericTask returns task result"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.awaitGenericTask (Task.FromResult value, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.awaitGenericTask should return task result"

        // 16. FIO.suspend - Lazy effect construction (verify deferred)
        testPropertyWithConfig fsCheckConfig "FIO.suspend defers effect construction"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable constructed = false
            let eff = FIO.suspend (fun () ->
                constructed <- true
                FIO.succeed value)

            Expect.isFalse constructed "Effect should not be constructed before run"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue constructed "Effect should be constructed after run"
            Expect.equal result value "FIO.suspend should return the inner effect result"

        // 17. FIO.suspend - Recursive effects
        testPropertyWithConfig fsCheckConfig "FIO.suspend allows recursive effects"
        <| fun (runtime: FIORuntime) ->
            let rec countdown n : FIO<int, exn> =
                if n <= 0 then FIO.succeed n
                else FIO.suspend(fun () -> countdown (n - 1))
            let result = runtime.Run(countdown 100).UnsafeSuccess()
            Expect.equal result 0 "Recursive suspend should work"

        // 18. FIO.acquireRelease - acquire succeeds, use succeeds, release runs
        testPropertyWithConfig fsCheckConfig "FIO.acquireRelease runs release on success"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.succeed value

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue released "Release should be called on success"
            Expect.equal result value "Should return use result"

        // 19. FIO.acquireRelease - acquire succeeds, use fails, release still runs
        testPropertyWithConfig fsCheckConfig "FIO.acquireRelease runs release on use failure"
        <| fun (runtime: FIORuntime, error: string) ->
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.fail error

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeError()

            Expect.isTrue released "Release should be called even on use failure"
            Expect.equal result error "Should return use error"

        // 20. FIO.acquireRelease - acquire fails, release does NOT run
        testPropertyWithConfig fsCheckConfig "FIO.acquireRelease does not run release when acquire fails"
        <| fun (runtime: FIORuntime, error: string) ->
            let mutable released = false
            let acquire = FIO.fail error
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.succeed 42

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeError()

            Expect.isFalse released "Release should not be called when acquire fails"
            Expect.equal result error "Should return acquire error"

        // 21. FIO.acquireRelease - Nested resources release in reverse order
        testPropertyWithConfig fsCheckConfig "FIO.acquireRelease releases in reverse order when nested"
        <| fun (runtime: FIORuntime) ->
            let mutable releaseOrder = []
            let acquire1 = FIO.succeed "r1"
            let release1 = fun _ -> releaseOrder <- releaseOrder @ [1]; FIO.unit()
            let acquire2 = FIO.succeed "r2"
            let release2 = fun _ -> releaseOrder <- releaseOrder @ [2]; FIO.unit()
            let eff = FIO.acquireRelease(acquire1, release1, fun _ ->
                FIO.acquireRelease(acquire2, release2, fun _ ->
                    FIO.succeed 42))
            let _ = runtime.Run(eff).UnsafeSuccess()
            Expect.equal releaseOrder [2; 1] "Nested resources should release in reverse order"

        // 22. FIO.collectAll - Sequential collection preserves order
        testPropertyWithConfig fsCheckConfig "FIO.collectAll preserves order"
        <| fun (runtime: FIORuntime) ->
            let mutable order = []
            let effects = [
                FIO.attempt ((fun () -> order <- order @ [1]; 1), id)
                FIO.attempt ((fun () -> order <- order @ [2]; 2), id)
                FIO.attempt ((fun () -> order <- order @ [3]; 3), id)
            ]

            let eff = FIO.collectAll effects

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "collectAll should return results in order"
            Expect.equal order [1; 2; 3] "collectAll should execute in order"

        // 23. FIO.collectAll - Empty collection returns empty list
        testPropertyWithConfig fsCheckConfig "FIO.collectAll with empty collection returns empty list"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.collectAll []

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [] "collectAll with empty collection should return empty list"

        // 24. FIO.collectAll - Error short-circuits (remaining not executed)
        testPropertyWithConfig fsCheckConfig "FIO.collectAll short-circuits on error"
        <| fun (runtime: FIORuntime, error: string) ->
            let mutable executed = []
            let effects: FIO<int, string> list = [
                FIO.attempt ((fun () -> executed <- executed @ [1]; 1), fun _ -> "err")
                FIO.fail error
                FIO.attempt ((fun () -> executed <- executed @ [3]; 3), fun _ -> "err")
            ]

            let eff = FIO.collectAll effects

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "collectAll should return the error"
            Expect.equal executed [1] "collectAll should not execute effects after error"

        // 25. FIO.collectAllPar - Parallel collection returns all results
        testPropertyWithConfig fsCheckConfig "FIO.collectAllPar returns all results"
        <| fun (runtime: FIORuntime) ->
            let effects = [
                FIO.succeed 1
                FIO.succeed 2
                FIO.succeed 3
            ]

            let eff = FIO.collectAllPar effects

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "collectAllPar should return all results in order"

        // 26. FIO.collectAllPar - Preserves result order regardless of completion time
        testPropertyWithConfig fsCheckConfig "FIO.collectAllPar preserves result order"
        <| fun (runtime: FIORuntime) ->
            let effects = [
                FIO.sleepExn(TimeSpan.FromMilliseconds 30.0).FlatMap(fun () -> FIO.succeed 3)
                FIO.sleepExn(TimeSpan.FromMilliseconds 10.0).FlatMap(fun () -> FIO.succeed 1)
                FIO.sleepExn(TimeSpan.FromMilliseconds 20.0).FlatMap(fun () -> FIO.succeed 2)
            ]
            let result = runtime.Run(FIO.collectAllPar effects).UnsafeSuccess()
            Expect.equal result [3; 1; 2] "collectAllPar should preserve order regardless of completion time"

        // 27. FIO.collectAllPar - Empty list returns empty list
        testPropertyWithConfig fsCheckConfig "FIO.collectAllPar with empty list returns empty"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(FIO.collectAllPar []).UnsafeSuccess()
            Expect.equal result [] "collectAllPar with empty list should return empty"

        // 28. FIO.collectAllPar - Propagates first error
        testPropertyWithConfig fsCheckConfig "FIO.collectAllPar propagates first error"
        <| fun (runtime: FIORuntime) ->
            let effects : FIO<int, string> list = [
                FIO.succeed 1
                FIO.fail "error"
                FIO.succeed 3
            ]
            let result = runtime.Run(FIO.collectAllPar effects).UnsafeError()
            Expect.equal result "error" "collectAllPar should propagate error"

        // 29. FIO.forEach - Maps and collects sequentially
        testPropertyWithConfig fsCheckConfig "FIO.forEach maps and collects sequentially"
        <| fun (runtime: FIORuntime) ->
            let mutable order = []
            let items = [1; 2; 3]
            let f = fun x ->
                FIO.attempt ((fun () ->
                    order <- order @ [x]
                    x * 2), id)

            let eff = FIO.forEach (items, f)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [2; 4; 6] "forEach should map and collect results"
            Expect.equal order [1; 2; 3] "forEach should execute sequentially in order"

        // 30. FIO.forEachPar - Maps and collects in parallel
        testPropertyWithConfig fsCheckConfig "FIO.forEachPar maps and collects in parallel"
        <| fun (runtime: FIORuntime) ->
            let items = [1; 2; 3]
            let f = fun x -> FIO.succeed (x * 2)

            let eff = FIO.forEachPar (items, f)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [2; 4; 6] "forEachPar should map and collect results in order"

        // 31. FIO.forEachPar - Preserves result order regardless of completion time
        testPropertyWithConfig fsCheckConfig "FIO.forEachPar preserves result order"
        <| fun (runtime: FIORuntime) ->
            let items = [1; 2; 3; 4; 5]
            let f = fun x ->
                FIO.sleepExn(TimeSpan.FromMilliseconds(float (10 * (6 - x))))
                    .FlatMap(fun () -> FIO.succeed(x * 2))
            let result = runtime.Run(FIO.forEachPar(items, f)).UnsafeSuccess()
            Expect.equal result [2; 4; 6; 8; 10] "forEachPar should preserve order"

        // 32. FIO.forEachPar - Empty list returns empty list
        testPropertyWithConfig fsCheckConfig "FIO.forEachPar with empty list returns empty"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(FIO.forEachPar([], fun x -> FIO.succeed(x * 2))).UnsafeSuccess()
            Expect.equal result [] "forEachPar with empty list should return empty"

        // 33. FIO.sleep - Delays execution
        testPropertyWithConfig fsCheckConfig "FIO.sleep delays execution"
        <| fun (runtime: FIORuntime) ->
            let duration = TimeSpan.FromMilliseconds 20.0
            let eff = fio {
                let sw = System.Diagnostics.Stopwatch.StartNew()
                do! FIO.sleep(duration, id)
                sw.Stop()
                return sw.Elapsed
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result.TotalMilliseconds 15.0 "FIO.sleep should delay execution"

        // 34. FIO.sleepExn - Delays execution with exn error
        testPropertyWithConfig fsCheckConfig "FIO.sleepExn delays execution"
        <| fun (runtime: FIORuntime) ->
            let duration = TimeSpan.FromMilliseconds 15.0
            let eff = fio {
                let sw = System.Diagnostics.Stopwatch.StartNew()
                do! FIO.sleepExn duration
                sw.Stop()
                return sw.Elapsed
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isGreaterThanOrEqual result.TotalMilliseconds 10.0 "FIO.sleepExn should delay execution"

        // 35. FIO.never - Effect that never completes (tested via Race)
        testPropertyWithConfig fsCheckConfig "FIO.never can be raced against completing effect"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = (FIO.never<int, exn>()).Race(FIO.succeed value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "Race with FIO.never should return the completing effect's result"

        // 36. FIO.fromOptionExn - None throws ArgumentException
        testPropertyWithConfig fsCheckConfig "FIO.fromOptionExn returns success for Some"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromOptionExn (Some value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromOptionExn should return value for Some"

        // 37. FIO.fromOptionExn - None case
        testPropertyWithConfig fsCheckConfig "FIO.fromOptionExn fails with ArgumentException for None"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.fromOptionExn<int> None

            let result = runtime.Run(eff).UnsafeError()

            Expect.isTrue (result :? ArgumentException) "FIO.fromOptionExn should fail with ArgumentException for None"

        // 38. FIO.awaitTaskExn - Task completes successfully
        testPropertyWithConfig fsCheckConfig "FIO.awaitTaskExn completes successfully"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.awaitTaskExn Task.CompletedTask

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "FIO.awaitTaskExn should complete successfully"

        // 39. FIO.awaitTaskExn - Task with exception
        testPropertyWithConfig fsCheckConfig "FIO.awaitTaskExn propagates exception"
        <| fun (runtime: FIORuntime) ->
            let ex = Exception "test error"
            let faultedTask = Task.FromException ex
            let eff = FIO.awaitTaskExn faultedTask

            let result = runtime.Run(eff).UnsafeError()

            Expect.stringContains result.Message "test error" "FIO.awaitTaskExn should propagate exception"

        // 40. FIO.awaitGenericTaskExn - Task<T> with exception
        testPropertyWithConfig fsCheckConfig "FIO.awaitGenericTaskExn propagates exception"
        <| fun (runtime: FIORuntime) ->
            let ex = Exception "generic task error"
            let faultedTask = Task.FromException<int> ex
            let eff = FIO.awaitGenericTaskExn faultedTask

            let result = runtime.Run(eff).UnsafeError()

            Expect.stringContains result.Message "generic task error" "FIO.awaitGenericTaskExn should propagate exception"

        // 41. FIO.awaitAsync - Async computation succeeds
        testPropertyWithConfig fsCheckConfig "FIO.awaitAsync returns async result"
        <| fun (runtime: FIORuntime, value: int) ->
            let asyncComp = async { return value }
            let eff = FIO.awaitAsync(asyncComp, id)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.awaitAsync should return async result"

        // 42. FIO.awaitAsyncExn - Async computation succeeds
        testPropertyWithConfig fsCheckConfig "FIO.awaitAsyncExn returns async result"
        <| fun (runtime: FIORuntime, value: int) ->
            let asyncComp = async { return value }
            let eff = FIO.awaitAsyncExn asyncComp

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.awaitAsyncExn should return async result"

        // 43. FIO.fromTask - Forks Task into Fiber
        testPropertyWithConfig fsCheckConfig "FIO.fromTask forks task into fiber"
        <| fun (runtime: FIORuntime) ->
            let mutable executed = false
            let eff = fio {
                let! fiber = FIO.fromTask((fun () ->
                    executed <- true
                    Task.CompletedTask), id)
                let! result = fiber.Join()
                return (executed, result)
            }

            let (wasExecuted, _) = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue wasExecuted "FIO.fromTask should execute the task"

        // 44. FIO.fromTaskExn - Forks Task into Fiber
        testPropertyWithConfig fsCheckConfig "FIO.fromTaskExn forks task into fiber"
        <| fun (runtime: FIORuntime) ->
            let mutable executed = false
            let eff = fio {
                let! fiber = FIO.fromTaskExn(fun () ->
                    executed <- true
                    Task.CompletedTask)
                let! _ = fiber.Join()
                return executed
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "FIO.fromTaskExn should execute the task"

        // 45. FIO.fromGenericTask - Forks Task<T> into Fiber
        testPropertyWithConfig fsCheckConfig "FIO.fromGenericTask forks generic task into fiber"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                let! fiber = FIO.fromGenericTask((fun () -> Task.FromResult value), id)
                let! result = fiber.Join()
                return result
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromGenericTask should return task result"

        // 46. FIO.fromGenericTaskExn - Forks Task<T> into Fiber
        testPropertyWithConfig fsCheckConfig "FIO.fromGenericTaskExn forks generic task into fiber"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = fio {
                let! fiber = FIO.fromGenericTaskExn(fun () -> Task.FromResult value)
                let! result = fiber.Join()
                return result
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromGenericTaskExn should return task result"

        // 47. FIO.interrupt with Timeout cause
        testPropertyWithConfig fsCheckConfig "FIO.interrupt with Timeout cause"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt<int, exn> (InterruptionCause.Timeout 1000.0, "timeout test")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt with Timeout should result in Interrupted"

        // 48. FIO.interrupt with ParentInterrupted cause
        testPropertyWithConfig fsCheckConfig "FIO.interrupt with ParentInterrupted cause"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt<int, exn> (InterruptionCause.ParentInterrupted (Guid.NewGuid()), "parent interrupt test")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt with ParentInterrupted should result in Interrupted"

        // 49. FIO.interrupt with ResourceExhaustion cause
        testPropertyWithConfig fsCheckConfig "FIO.interrupt with ResourceExhaustion cause"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt<int, exn> (InterruptionCause.ResourceExhaustion "out of memory", "resource test")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt with ResourceExhaustion should result in Interrupted"

        // 50. FIO.interrupt with InvalidArgument cause
        testPropertyWithConfig fsCheckConfig "FIO.interrupt with InvalidArgument cause"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt<int, exn> (InterruptionCause.InvalidArgument ("param", "bad value"), "invalid arg test")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt with InvalidArgument should result in Interrupted"
    ]
