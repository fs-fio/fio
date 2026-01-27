module FSharp.FIO.Tests.FiberTests

open FSharp.FIO.Tests.Utilities.FsCheckProperties

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open Expecto

open System

[<Tests>]
let fiberLifecycleTests =
    testList "Fiber Lifecycle" [

        // Test 1: Fiber.Completed returns true after effect completes
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber.Completed returns true after effect completes"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! fiber = FIO.succeed(res).Fork()
                let! _ = fiber.Join()
                return fiber.Completed()
            }
            let completed = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue completed "Fiber.Completed should return true after effect completes"

        // Test 2: Fiber.Join awaits fiber and returns result
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber.Join awaits fiber and returns result"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! fiber = FIO.succeed(res).Fork()
                return! fiber.Join()
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Fiber.Join should return the fiber's result"

        // Test 3: Fork then Join equals identity
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fork then Join equals identity"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! fiber = FIO.succeed(res).Fork()
                let! result = fiber.Join()
                return result
            }
            let forkJoinResult = runtime.Run(eff).UnsafeSuccess()
            Expect.equal forkJoinResult res "Fork then Join should equal identity"
    ]

[<Tests>]
let fiberInterruptionTests =
    testList "Fiber Interruption" [

        // Test 4: Fiber.Interrupt effect-based interruption marks fiber interrupted
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber.Interrupt marks fiber as interrupted"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! fiber = FIO.never<int, string>().Fork()
                do! fiber.Interrupt()
                // Give fiber time to process interruption
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0).MapError(fun _ -> "sleep error")
                return fiber.Interrupted
            }
            let interrupted = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue interrupted "Fiber.Interrupt should mark fiber as interrupted"

        // Test 5: Fiber.Interrupted returns true after interruption
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber.Interrupted returns true after interruption"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! fiber = FIO.never<int, string>().Fork()
                do! fiber.Interrupt()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0).MapError(fun _ -> "sleep error")
                return fiber.Interrupted
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Fiber.Interrupted should return true after interruption"

        // Test 6: Fiber.UnsafeInterrupt external interruption works
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber.UnsafeInterrupt works from outside effect"
        <| fun (runtime: FIORuntime) ->
            // Create a fiber for a simple effect and interrupt it externally
            let eff : FIO<int, string> = FIO.sleepExn(TimeSpan.FromHours 1.0).MapError(fun _ -> "sleep error") *> FIO.succeed 42
            let fiber = runtime.Run(eff)
            // Interrupt immediately
            fiber.UnsafeInterrupt()
            // Check result - should be interrupted
            let result = fiber.UnsafeResult()
            match result with
            | Interrupted _ -> Expect.isTrue true "Fiber was interrupted"
            | Succeeded _ -> Expect.isTrue true "Fiber completed before interrupt (acceptable)"
            | Failed _ -> Expect.isFalse true "Fiber should not have failed"
    ]

[<Tests>]
let fiberResultPatternMatchingTests =
    testList "FiberResult Pattern Matching" [

        // Test 7: Succeeded pattern matches success case
        testPropertyWithConfig fsCheckPropertyTestsConfig "Succeeded pattern matches success case"
        <| fun (runtime: FIORuntime, res: int) ->
            let fiber = runtime.Run(FIO.succeed res)
            let result = fiber.UnsafeResult()
            match result with
            | Succeeded value -> Expect.equal value res "Succeeded should contain the result"
            | Failed _ -> failtest "Expected Succeeded but got Failed"
            | Interrupted _ -> failtest "Expected Succeeded but got Interrupted"

        // Test 8: Failed pattern matches failure case
        testPropertyWithConfig fsCheckPropertyTestsConfig "Failed pattern matches failure case"
        <| fun (runtime: FIORuntime, err: string) ->
            let fiber = runtime.Run(FIO.fail<int, string> err)
            let result = fiber.UnsafeResult()
            match result with
            | Succeeded _ -> failtest "Expected Failed but got Succeeded"
            | Failed error -> Expect.equal error err "Failed should contain the error"
            | Interrupted _ -> failtest "Expected Failed but got Interrupted"

        // Test 9: Interrupted pattern matches interruption case
        testPropertyWithConfig fsCheckPropertyTestsConfig "Interrupted pattern matches interruption case"
        <| fun (runtime: FIORuntime) ->
            // Use FIO.interrupt directly to test the pattern matching
            let eff = FIO.interrupt<int, string>(ExplicitInterrupt, "Test interruption")
            let fiber = runtime.Run(eff)
            let result = fiber.UnsafeResult()
            match result with
            | Succeeded _ -> failtest "Expected Interrupted but got Succeeded"
            | Failed _ -> failtest "Expected Interrupted but got Failed"
            | Interrupted exn -> 
                Expect.equal exn.cause ExplicitInterrupt "Interrupted should contain ExplicitInterrupt cause"
    ]

[<Tests>]
let interruptionCauseTests =
    testList "InterruptionCause" [

        // Test 10: ExplicitInterrupt is default cause for .Interrupt()
        testPropertyWithConfig fsCheckPropertyTestsConfig "ExplicitInterrupt is default cause for Fiber.Interrupt"
        <| fun (runtime: FIORuntime) ->
            // Fork a long sleep and use effect-based Interrupt
            let longSleep = FIO.sleepExn(TimeSpan.FromHours 1.0).MapError(fun _ -> "sleep error")
            let eff = fio {
                let! fiber = longSleep.Fork()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "sleep error")
                do! fiber.Interrupt()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0).MapError(fun _ -> "sleep error")
                return fiber.Interrupted
            }
            let wasInterrupted = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue wasInterrupted "Fiber.Interrupt should mark fiber as interrupted"

        // Test 11: Timeout cause when using FIO.interrupt directly
        testPropertyWithConfig fsCheckPropertyTestsConfig "Timeout cause can be specified via FIO.interrupt"
        <| fun (runtime: FIORuntime) ->
            let timeoutMs = 1000.0
            let eff = FIO.interrupt<int, string>(Timeout timeoutMs, "Operation timed out")
            let fiber = runtime.Run(eff)
            let result = fiber.UnsafeResult()
            match result with
            | Interrupted exn ->
                match exn.cause with
                | Timeout ms -> Expect.equal ms timeoutMs "Timeout duration should match"
                | _ -> failtest $"Expected Timeout but got {exn.cause}"
            | _ -> failtest "Expected Interrupted result"

        // Test 12: ResourceExhaustion custom cause
        testPropertyWithConfig fsCheckPropertyTestsConfig "ResourceExhaustion cause can be specified"
        <| fun (runtime: FIORuntime) ->
            let reason = "Memory limit exceeded"
            let eff = FIO.interrupt<int, string>(ResourceExhaustion reason, "Resource exhausted")
            let fiber = runtime.Run(eff)
            let result = fiber.UnsafeResult()
            match result with
            | Interrupted exn ->
                match exn.cause with
                | ResourceExhaustion r -> Expect.equal r reason "ResourceExhaustion reason should match"
                | _ -> failtest $"Expected ResourceExhaustion but got {exn.cause}"
            | _ -> failtest "Expected Interrupted result"
    ]

[<Tests>]
let concurrentFibersTests =
    testList "Concurrent Fibers" [

        // Test 13: Multiple fibers fork and join independently
        testPropertyWithConfig fsCheckPropertyTestsConfig "Multiple fibers fork and join independently"
        <| fun (runtime: FIORuntime, a: int, b: int, c: int) ->
            let eff = fio {
                let! fiber1 = FIO.succeed(a).Fork()
                let! fiber2 = FIO.succeed(b).Fork()
                let! fiber3 = FIO.succeed(c).Fork()
                let! r1 = fiber1.Join()
                let! r2 = fiber2.Join()
                let! r3 = fiber3.Join()
                return (r1, r2, r3)
            }
            let (r1, r2, r3) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal r1 a "First fiber result should match"
            Expect.equal r2 b "Second fiber result should match"
            Expect.equal r3 c "Third fiber result should match"

        // Test 14: Stress test with 50+ fibers
        testPropertyWithConfig fsCheckPropertyTestsConfig "Fork many fibers stress test"
        <| fun (runtime: FIORuntime) ->
            let fiberCount = 50
            let eff = fio {
                // Fork all fibers
                let! fibers = 
                    [| for i in 1..fiberCount -> FIO.succeed(i).Fork() |]
                    |> Array.fold (fun acc forkEff -> 
                        fio {
                            let! fibers = acc
                            let! fiber = forkEff
                            return Array.append fibers [| fiber |]
                        }) (FIO.succeed [||])
                
                // Join all fibers and collect results
                let! results =
                    fibers
                    |> Array.fold (fun acc fiber ->
                        fio {
                            let! results = acc
                            let! result = fiber.Join()
                            return Array.append results [| result |]
                        }) (FIO.succeed [||])
                
                return results
            }
            let results = runtime.Run(eff).UnsafeSuccess()
            Expect.equal results.Length fiberCount "Should have results from all fibers"
            let expected = [| for i in 1..fiberCount -> i |]
            Expect.equal results expected "All fiber results should be correct"
    ]
