module FSharp.FIO.Tests.FactoryTests

open FSharp.FIO.Tests.Utilities.FsCheckProperties

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open Expecto
open FsCheck

open System
open System.Threading.Tasks

[<Tests>]
let factoryTests =
    testList "Factory Functions" [

        // 1. FIO.unit - Returns unit effect
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.unit returns unit"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.unit ()

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "FIO.unit should return unit"

        // 2. FIO.succeed - Succeeds with value
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.succeed returns the provided value"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.succeed value

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.succeed should return the provided value"

        // 3. FIO.fail - Fails with error
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fail fails with the provided error"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fail error

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fail should fail with the provided error"

        // 4. FIO.interrupt - Self-interruption
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.interrupt results in Interrupted fiber"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.interrupt (InterruptionCause.ExplicitInterrupt, "test interrupt")

            let fiber = runtime.Run(eff)
            let fiberResult = fiber.Task() |> Async.AwaitTask |> Async.RunSynchronously

            match fiberResult with
            | FiberResult.Interrupted _ -> ()
            | _ -> failtest "FIO.interrupt should result in Interrupted"

        // 5. FIO.attempt - Side-effecting function succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.attempt succeeds when function succeeds"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.attempt ((fun () -> value), (fun ex -> ex.Message))

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.attempt should return the function result"

        // 6. FIO.attempt - Function throws, mapped error
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.attempt maps exception to error when function throws"
        <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
            let msg = errorMsg.Get
            let eff = FIO.attempt ((fun () -> raise (Exception msg)), (fun ex -> ex.Message))

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result msg "FIO.attempt should map exception to error"

        // 7. FIO.attemptExn - Exception passthrough
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.attemptExn passes through exception"
        <| fun (runtime: FIORuntime, errorMsg: NonEmptyString) ->
            let msg = errorMsg.Get
            let ex = Exception msg
            let eff = FIO.attemptExn (fun () -> raise ex)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result.Message msg "FIO.attemptExn should pass through exception"

        // 8. FIO.fromResult - Result.Ok to success
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromResult converts Ok to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromResult (Ok value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromResult should convert Ok to success"

        // 9. FIO.fromResult - Result.Error to fail
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromResult converts Error to fail"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromResult (Error error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromResult should convert Error to fail"

        // 10. FIO.fromOption - Some to success
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromOption converts Some to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromOption (Some value, fun () -> "none error")

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromOption should convert Some to success"

        // 11. FIO.fromOption - None to error (using onNone)
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromOption converts None to error using onNone"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromOption (None, fun () -> error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromOption should convert None to error using onNone"

        // 12. FIO.fromChoice - Choice1Of2 to success
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromChoice converts Choice1Of2 to success"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.fromChoice (Choice1Of2 value)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.fromChoice should convert Choice1Of2 to success"

        // 13. FIO.fromChoice - Choice2Of2 to fail
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.fromChoice converts Choice2Of2 to fail"
        <| fun (runtime: FIORuntime, error: string) ->
            let eff = FIO.fromChoice (Choice2Of2 error)

            let result = runtime.Run(eff).UnsafeError()

            Expect.equal result error "FIO.fromChoice should convert Choice2Of2 to fail"

        // 14. FIO.awaitTask - Task completes successfully
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.awaitTask completes successfully"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.awaitTask (Task.CompletedTask, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "FIO.awaitTask should complete successfully"

        // 15. FIO.awaitGenericTask - Task<T> returns value
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.awaitGenericTask returns task result"
        <| fun (runtime: FIORuntime, value: int) ->
            let eff = FIO.awaitGenericTask (Task.FromResult value, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result value "FIO.awaitGenericTask should return task result"

        // 16. FIO.suspend - Lazy effect construction (verify deferred)
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.suspend defers effect construction"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable constructed = false
            let eff = FIO.suspend (fun () ->
                constructed <- true
                FIO.succeed value)

            Expect.isFalse constructed "Effect should not be constructed before run"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue constructed "Effect should be constructed after run"
            Expect.equal result value "FIO.suspend should return the inner effect result"

        // 17. FIO.acquireRelease - acquire succeeds, use succeeds, release runs
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.acquireRelease runs release on success"
        <| fun (runtime: FIORuntime, value: int) ->
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.succeed value

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue released "Release should be called on success"
            Expect.equal result value "Should return use result"

        // 18. FIO.acquireRelease - acquire succeeds, use fails, release still runs
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.acquireRelease runs release on use failure"
        <| fun (runtime: FIORuntime, error: string) ->
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.fail error

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeError()

            Expect.isTrue released "Release should be called even on use failure"
            Expect.equal result error "Should return use error"

        // 19. FIO.acquireRelease - acquire fails, release does NOT run
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.acquireRelease does not run release when acquire fails"
        <| fun (runtime: FIORuntime, error: string) ->
            let mutable released = false
            let acquire = FIO.fail error
            let release = fun _ -> released <- true; FIO.unit ()
            let useResource = fun _ -> FIO.succeed 42

            let eff = FIO.acquireRelease (acquire, release, useResource)

            let result = runtime.Run(eff).UnsafeError()

            Expect.isFalse released "Release should not be called when acquire fails"
            Expect.equal result error "Should return acquire error"

        // 20. FIO.collectAll - Sequential collection preserves order
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.collectAll preserves order"
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

        // 21. FIO.collectAll - Empty collection returns empty list
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.collectAll with empty collection returns empty list"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.collectAll []

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [] "collectAll with empty collection should return empty list"

        // 22. FIO.collectAll - Error short-circuits (remaining not executed)
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.collectAll short-circuits on error"
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

        // 23. FIO.collectAllPar - Parallel collection returns all results
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.collectAllPar returns all results"
        <| fun (runtime: FIORuntime) ->
            let effects = [
                FIO.succeed 1
                FIO.succeed 2
                FIO.succeed 3
            ]

            let eff = FIO.collectAllPar effects

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "collectAllPar should return all results in order"

        // 24. FIO.forEach - Maps and collects sequentially
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.forEach maps and collects sequentially"
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

        // 25. FIO.forEachPar - Maps and collects in parallel
        testPropertyWithConfig fsCheckPropertyTestsConfig "FIO.forEachPar maps and collects in parallel"
        <| fun (runtime: FIORuntime) ->
            let items = [1; 2; 3]
            let f = fun x -> FIO.succeed (x * 2)

            let eff = FIO.forEachPar (items, f)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [2; 4; 6] "forEachPar should map and collect results in order"
    ]
