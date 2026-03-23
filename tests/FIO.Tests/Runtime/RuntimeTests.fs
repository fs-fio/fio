module FIO.Tests.RuntimeTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

open Expecto

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

let private interruptedJoinEffect () : FIO<int, exn> =
    fio {
        let! fiber = FIO.never<int, exn>().Fork()
        do! fiber.Interrupt()
        let! _ = fiber.Join<int, exn>()
        return 42
    }

let private assertInterruptedChildJoin (runtimeName: string) (runtime: FIORuntime) =
    try
        let result =
            runtime
                .Run(interruptedJoinEffect ())
                .Task<int, exn>()
                .WaitAsync(TimeSpan.FromSeconds 2.0)
                .GetAwaiter()
                .GetResult()

        match result with
        | Interrupted _ ->
            ()
        | Succeeded value ->
            failtest $"{runtimeName} should report interruption when joining an interrupted child, but succeeded with {value}"
        | Failed err ->
            failtest $"{runtimeName} should report interruption when joining an interrupted child, but failed with {err}"
    with
    | :? TimeoutException ->
        failtest $"{runtimeName} timed out while joining an interrupted child fiber"

let private assertRunSerialization (runtimeName: string) (runtime: FIORuntime) =
    use gate = new ManualResetEventSlim(false)
    let eff : FIO<unit, exn> = FIO.sleepExn (TimeSpan.FromMilliseconds 250.0)
    let runOne () =
        Task.Run(fun () ->
            gate.Wait()
            runtime.Run(eff).Task<unit, exn>().Wait())

    let t1 = runOne()
    let t2 = runOne()
    let sw = Stopwatch.StartNew()
    gate.Set()
    Task.WaitAll [| t1; t2 |]
    sw.Stop()

    Expect.isTrue
        (sw.ElapsedMilliseconds >= 430L)
        $"{runtimeName} should serialize concurrent Run calls on the same runtime instance"

let private assertCompletesWithin<'R, 'E> (timeout: TimeSpan) (fiber: Fiber<'R, 'E>) =
    try
        fiber
            .Task<'R, 'E>()
            .WaitAsync(timeout)
            .GetAwaiter()
            .GetResult()
    with
    | :? TimeoutException ->
        failtest $"Expected computation to complete within {timeout}, but it timed out."

[<Tests>]
let directRuntimeTests =
    testList "DirectRuntime" [

        testCase "DirectRuntime executes basic effect"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff = FIO.succeed 42
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "DirectRuntime should execute basic effect"

        testCase "DirectRuntime handles error effect"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff : FIO<int, string> = FIO.fail "error"
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result "error" "DirectRuntime should handle error"

        testCase "DirectRuntime handles fork and join"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff = fio {
                let! fiber = (FIO.succeed 42).Fork()
                let! result = fiber.Join()
                return result
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "DirectRuntime should handle fork/join"

        testCase "DirectRuntime handles parallel effects"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff = (FIO.succeed 1).ZipPar(FIO.succeed 2)
            let (r1, r2) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal r1 1 "First result should be 1"
            Expect.equal r2 2 "Second result should be 2"

        testCase "DirectRuntime handles channel operations"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(42).Unit()
                let! msg = chan.Receive()
                return msg
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "DirectRuntime should handle channels"

        testCase "DirectRuntime handles interruption"
        <| fun () ->
            let runtime = new DirectRuntime()
            let eff = FIO.interrupt<int, string>(ExplicitInterrupt, "test")
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Interrupted _ -> Expect.isTrue true "Should be interrupted"
            | _ -> Expect.isTrue false "Should be interrupted"

        testCase "DirectRuntime join on interrupted child returns interruption"
        <| fun () ->
            let runtime = new DirectRuntime() :> FIORuntime
            assertInterruptedChildJoin "DirectRuntime" runtime

        testCase "DirectRuntime serializes concurrent Run calls on same runtime"
        <| fun () ->
            let runtime = new DirectRuntime() :> FIORuntime
            assertRunSerialization "DirectRuntime" runtime
    ]

[<Tests>]
let cooperativeRuntimeTests =
    testList "CooperativeRuntime" [

        testCase "CooperativeRuntime executes basic effect"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = FIO.succeed 42
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "CooperativeRuntime should execute basic effect"

        testCase "CooperativeRuntime handles error effect"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff : FIO<int, string> = FIO.fail "error"
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result "error" "CooperativeRuntime should handle error"

        testCase "CooperativeRuntime handles fork and join"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = fio {
                let! fiber = (FIO.succeed 42).Fork()
                let! result = fiber.Join()
                return result
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "CooperativeRuntime should handle fork/join"

        testCase "CooperativeRuntime handles parallel effects"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = (FIO.succeed 1).ZipPar(FIO.succeed 2)
            let (r1, r2) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal r1 1 "First result should be 1"
            Expect.equal r2 2 "Second result should be 2"

        testCase "CooperativeRuntime handles channel operations"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(42).Unit()
                let! msg = chan.Receive()
                return msg
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "CooperativeRuntime should handle channels"

        testCase "CooperativeRuntime handles interruption"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = FIO.interrupt<int, string>(ExplicitInterrupt, "test")
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Interrupted _ -> Expect.isTrue true "Should be interrupted"
            | _ -> Expect.isTrue false "Should be interrupted"

        testCase "CooperativeRuntime join on interrupted child returns interruption"
        <| fun () ->
            let runtime = new CooperativeRuntime() :> FIORuntime
            assertInterruptedChildJoin "CooperativeRuntime" runtime

        testCase "CooperativeRuntime serializes concurrent Run calls on same runtime"
        <| fun () ->
            let runtime = new CooperativeRuntime() :> FIORuntime
            assertRunSerialization "CooperativeRuntime" runtime

        testCase "CooperativeRuntime polling blocked receive stress completes under timeout"
        <| fun () ->
            let runtime = new CooperativeRuntime({ EWC = 2; EWS = 200; BWC = 2 })
            let total = 1_500
            let eff = fio {
                let chan = Channel<int>()
                let sender =
                    fio {
                        for i in 1..total do
                            do! chan.Send(i).Unit()
                    }
                let receiver =
                    fio {
                        let mutable received = 0
                        let mutable sum = 0
                        while received < total do
                            let! msg = chan.Receive<int, exn>()
                            received <- received + 1
                            sum <- sum + msg
                        return sum
                    }

                let! _, sum = sender <&> receiver
                return sum
            }

            let result = runtime.Run(eff) |> assertCompletesWithin (TimeSpan.FromSeconds 8.0)
            match result with
            | Succeeded sum ->
                let expected = total * (total + 1) / 2
                Expect.equal sum expected "Should receive all messages without loss"
            | Failed err ->
                failtest $"Unexpected failure in cooperative polling stress test: {err}"
            | Interrupted ex ->
                failtest $"Unexpected interruption in cooperative polling stress test: {ex.Message}"

        testCase "CooperativeRuntime polling join stress completes under timeout"
        <| fun () ->
            let runtime = new CooperativeRuntime { EWC = 2; EWS = 200; BWC = 2 }
            let fiberCount = 300
            let eff = fio {
                let! fibers =
                    [1..fiberCount]
                    |> List.map (fun _ -> FIO.sleepExn(TimeSpan.FromMilliseconds 1.0).Fork())
                    |> FIO.collectAll

                let! _ =
                    fibers
                    |> List.map (fun fiber -> fiber.Join())
                    |> FIO.collectAll
                return fiberCount
            }

            let result = runtime.Run(eff) |> assertCompletesWithin (TimeSpan.FromSeconds 8.0)
            match result with
            | Succeeded joinedCount ->
                Expect.equal joinedCount fiberCount "All forked fibers should join successfully"
            | Failed err ->
                failtest $"Unexpected failure in cooperative join polling stress test: {err}"
            | Interrupted ex ->
                failtest $"Unexpected interruption in cooperative join polling stress test: {ex.Message}"

        testCase "CooperativeRuntime handles many fibers"
        <| fun () ->
            let runtime = new CooperativeRuntime()
            let eff = fio {
                let! fibers =
                    [1..50]
                    |> List.map (fun i -> (FIO.succeed i).Fork())
                    |> FIO.collectAll
                let! results =
                    fibers
                    |> List.map (fun f -> f.Join())
                    |> FIO.collectAll
                return results
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result [1..50] "Should handle many fibers"
    ]

[<Tests>]
let concurrentRuntimeTests =
    testList "ConcurrentRuntime" [

        testCase "ConcurrentRuntime executes basic effect"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = FIO.succeed 42
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "ConcurrentRuntime should execute basic effect"

        testCase "ConcurrentRuntime handles error effect"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff : FIO<int, string> = FIO.fail "error"
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result "error" "ConcurrentRuntime should handle error"

        testCase "ConcurrentRuntime handles fork and join"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = fio {
                let! fiber = (FIO.succeed 42).Fork()
                let! result = fiber.Join()
                return result
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "ConcurrentRuntime should handle fork/join"

        testCase "ConcurrentRuntime handles parallel effects"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = (FIO.succeed 1).ZipPar(FIO.succeed 2)
            let (r1, r2) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal r1 1 "First result should be 1"
            Expect.equal r2 2 "Second result should be 2"

        testCase "ConcurrentRuntime handles channel operations"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(42).Unit()
                let! msg = chan.Receive()
                return msg
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 42 "ConcurrentRuntime should handle channels"

        testCase "ConcurrentRuntime handles interruption"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = FIO.interrupt<int, string>(ExplicitInterrupt, "test")
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Interrupted _ -> Expect.isTrue true "Should be interrupted"
            | _ -> Expect.isTrue false "Should be interrupted"

        testCase "ConcurrentRuntime join on interrupted child returns interruption"
        <| fun () ->
            let runtime = new ConcurrentRuntime() :> FIORuntime
            assertInterruptedChildJoin "ConcurrentRuntime" runtime

        testCase "ConcurrentRuntime serializes concurrent Run calls on same runtime"
        <| fun () ->
            let runtime = new ConcurrentRuntime() :> FIORuntime
            assertRunSerialization "ConcurrentRuntime" runtime

        testCase "ConcurrentRuntime channel signal dedup stress has no message loss"
        <| fun () ->
            let runtime = new ConcurrentRuntime { EWC = 4; EWS = 200; BWC = 2 }
            let total = 5_000
            let eff = fio {
                let chan = Channel<int>()
                let sender =
                    fio {
                        for i in 1..total do
                            do! chan.Send(i).Unit()
                    }
                let receiver =
                    fio {
                        let mutable received = 0
                        let mutable sum = 0
                        while received < total do
                            let! msg = chan.Receive<int, exn>()
                            received <- received + 1
                            sum <- sum + msg
                        return sum
                    }

                let! (_, sum) = sender <&> receiver
                return sum
            }

            let result = runtime.Run eff |> assertCompletesWithin (TimeSpan.FromSeconds 8.0)
            match result with
            | Succeeded sum ->
                let expected = total * (total + 1) / 2
                Expect.equal sum expected "Concurrent runtime should deliver all channel messages under stress"
            | Failed err ->
                failtest $"Unexpected failure in concurrent signal stress test: {err}"
            | Interrupted ex ->
                failtest $"Unexpected interruption in concurrent signal stress test: {ex.Message}"

        testCase "ConcurrentRuntime handles many concurrent fibers"
        <| fun () ->
            let runtime = new ConcurrentRuntime()
            let eff = fio {
                let! fibers =
                    [1..100]
                    |> List.map (fun i -> (FIO.succeed i).Fork())
                    |> FIO.collectAll
                let! results =
                    fibers
                    |> List.map (fun f -> f.Join())
                    |> FIO.collectAll
                return results
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result [1..100] "Should handle many concurrent fibers"
    ]

[<Tests>]
let runtimeComparisonTests =
    testList "Runtime Comparison" [

        testPropertyWithConfig fsCheckConfig "All runtimes produce same result for pure effects"
        <| fun (res: int) ->
            let eff = FIO.succeed res
            let directResult = (new DirectRuntime()).Run(eff).UnsafeSuccess()
            let cooperativeResult = (new CooperativeRuntime()).Run(eff).UnsafeSuccess()
            let concurrentResult = (new ConcurrentRuntime()).Run(eff).UnsafeSuccess()
            Expect.equal directResult res "Direct should match"
            Expect.equal cooperativeResult res "Cooperative should match"
            Expect.equal concurrentResult res "Concurrent should match"

        testPropertyWithConfig fsCheckConfig "All runtimes produce same result for error effects"
        <| fun (err: string) ->
            let eff : FIO<int, string> = FIO.fail err
            let directResult = (new DirectRuntime()).Run(eff).UnsafeError()
            let cooperativeResult = (new CooperativeRuntime()).Run(eff).UnsafeError()
            let concurrentResult = (new ConcurrentRuntime()).Run(eff).UnsafeError()
            Expect.equal directResult err "Direct should match"
            Expect.equal cooperativeResult err "Cooperative should match"
            Expect.equal concurrentResult err "Concurrent should match"

        testPropertyWithConfig fsCheckConfig "All runtimes handle Map correctly"
        <| fun (runtime: FIORuntime, res: int, f: int -> int) ->
            let eff = (FIO.succeed res).Map f
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result (f res) "Map should apply function"

        testPropertyWithConfig fsCheckConfig "All runtimes handle FlatMap correctly"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = (FIO.succeed res).FlatMap(fun x -> FIO.succeed(x * 2))
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result (res * 2) "FlatMap should chain effects"

        testPropertyWithConfig fsCheckConfig "All runtimes handle Zip correctly"
        <| fun (runtime: FIORuntime, a: int, b: string) ->
            let eff = (FIO.succeed a).Zip(FIO.succeed b)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result (a, b) "Zip should combine results"

        testPropertyWithConfig fsCheckConfig "All runtimes handle ZipPar correctly"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff : FIO<int * int, string> = (FIO.succeed a).ZipPar(FIO.succeed b)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result (a, b) "ZipPar should combine results"

        testPropertyWithConfig fsCheckConfig "All runtimes handle OrElse correctly"
        <| fun (runtime: FIORuntime, err: string, fallback: int) ->
            let eff : FIO<int, string> = (FIO.fail err).OrElse(FIO.succeed fallback)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result fallback "OrElse should provide fallback"

        testPropertyWithConfig fsCheckConfig "All runtimes handle CatchAll correctly"
        <| fun (runtime: FIORuntime, err: string, recovery: int) ->
            let eff : FIO<int, string> = (FIO.fail err).CatchAll(fun _ -> FIO.succeed recovery)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result recovery "CatchAll should recover"
    ]

[<Tests>]
let runtimeResourceCleanupTests =
    testList "Runtime Resource Cleanup" [

        testPropertyWithConfig fsCheckConfig "Ensuring runs on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable finalizerRan = false
            let eff : FIO<int, exn> =
                (FIO.succeed res).Ensuring(FIO.attemptExn(fun () -> finalizerRan <- true))
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Should return result"
            Expect.isTrue finalizerRan "Finalizer should run on success"

        testPropertyWithConfig fsCheckConfig "Ensuring runs on error"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test error"
            let mutable finalizerRan = false
            let eff : FIO<int, exn> =
                (FIO.fail err).Ensuring(FIO.attemptExn(fun () -> finalizerRan <- true))
            let _ = runtime.Run(eff).UnsafeResult()
            Expect.isTrue finalizerRan "Finalizer should run on error"

        testPropertyWithConfig fsCheckConfig "acquireRelease runs release on success"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release _ = FIO.attemptExn(fun () -> released <- true)
            let use' _ = FIO.succeed res
            let eff = FIO.acquireRelease(acquire, release, use')
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Should return result"
            Expect.isTrue released "Should release on success"

        testPropertyWithConfig fsCheckConfig "acquireRelease runs release on error"
        <| fun (runtime: FIORuntime) ->
            let err = exn "test"
            let mutable released = false
            let acquire = FIO.succeed "resource"
            let release _ = FIO.attemptExn(fun () -> released <- true)
            let use' _ = FIO.fail err
            let eff = FIO.acquireRelease(acquire, release, use')
            let _ = runtime.Run(eff).UnsafeResult()
            Expect.isTrue released "Should release on error"
    ]
