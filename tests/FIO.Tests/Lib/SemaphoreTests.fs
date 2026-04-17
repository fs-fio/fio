module FIO.Tests.SemaphoreTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Semaphore
open FIO.Ref

open Expecto

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System

let private onError (e: exn) = e.Message

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let semaphoreAcquireTests =
    testList
        "Semaphore Acquire"
        [

            testPropertyWithConfig fsCheckConfig "Acquire decreases available permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 3
                        let! before = sem.Available()
                        do! sem.Acquire onError
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 3 "Should have 3 permits before"
                Expect.equal after 2 "Should have 2 permits after acquire"

            testPropertyWithConfig fsCheckConfig "AcquireExn decreases available permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 3
                        let! before = sem.Available()
                        do! sem.Acquire id
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 3 "Should have 3 permits before"
                Expect.equal after 2 "Should have 2 permits after acquire"

            testPropertyWithConfig fsCheckConfig "Multiple acquires decrease permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        do! sem.Acquire id
                        do! sem.Acquire id
                        do! sem.Acquire id
                        let! available = sem.Available()
                        return available
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 2 "Should have 2 permits after 3 acquires"
        ]

[<Tests>]
let semaphoreTryAcquireTests =
    testList
        "Semaphore TryAcquire"
        [

            testPropertyWithConfig fsCheckConfig "TryAcquire returns true when permit available"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! acquired = sem.TryAcquire(TimeSpan.FromMilliseconds 100.0, onError)
                        return acquired
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "TryAcquire should return true when permit available"

            testPropertyWithConfig fsCheckConfig "TryAcquire returns false when no permit and timeout"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        do! sem.Acquire onError
                        let! acquired = sem.TryAcquire(TimeSpan.FromMilliseconds 10.0, onError)
                        return acquired
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse result "TryAcquire should return false when no permit"

            testPropertyWithConfig fsCheckConfig "TryAcquireExn returns true when permit available"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! acquired = sem.TryAcquire(TimeSpan.FromMilliseconds 100.0, id)
                        return acquired
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "TryAcquireExn should return true when permit available"

            testPropertyWithConfig fsCheckConfig "TryAcquireExn returns false when no permit and timeout"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        do! sem.Acquire id
                        let! acquired = sem.TryAcquire(TimeSpan.FromMilliseconds 10.0, id)
                        return acquired
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse result "TryAcquireExn should return false when no permit"
        ]

[<Tests>]
let semaphoreReleaseTests =
    testList
        "Semaphore Release"
        [

            testPropertyWithConfig fsCheckConfig "Release increases available permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 3
                        do! sem.Acquire onError
                        let! before = sem.Available()
                        do! sem.Release onError
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 2 "Should have 2 permits before release"
                Expect.equal after 3 "Should have 3 permits after release"

            testPropertyWithConfig fsCheckConfig "ReleaseExn increases available permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 3
                        do! sem.Acquire id
                        let! before = sem.Available()
                        do! sem.Release id
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 2 "Should have 2 permits before release"
                Expect.equal after 3 "Should have 3 permits after release"
        ]

[<Tests>]
let semaphoreReleaseManyTests =
    testList
        "Semaphore ReleaseMany"
        [

            testPropertyWithConfig fsCheckConfig "ReleaseMany increases permits by count"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        do! sem.Acquire onError
                        do! sem.Acquire onError
                        do! sem.Acquire onError
                        let! before = sem.Available()
                        do! sem.ReleaseMany(3, onError)
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 2 "Should have 2 permits before"
                Expect.equal after 5 "Should have 5 permits after releasing 3"

            testPropertyWithConfig fsCheckConfig "ReleaseManyExn increases permits by count"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        do! sem.Acquire id
                        do! sem.Acquire id
                        do! sem.Acquire id
                        let! before = sem.Available()
                        do! sem.ReleaseMany(3, id)
                        let! after = sem.Available()
                        return before, after
                    }

                let before, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 2 "Should have 2 permits before"
                Expect.equal after 5 "Should have 5 permits after releasing 3"
        ]

[<Tests>]
let semaphoreAvailableTests =
    testList
        "Semaphore Available"
        [

            testPropertyWithConfig fsCheckConfig "Available returns initial count"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        let! available = sem.Available()
                        return available
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 5 "Should return initial permit count"

            testPropertyWithConfig fsCheckConfig "Available reflects acquire and release"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        do! sem.Acquire id
                        do! sem.Acquire id
                        do! sem.Acquire id
                        let! afterAcquire = sem.Available()
                        do! sem.Release id
                        do! sem.Release id
                        let! afterRelease = sem.Available()
                        return afterAcquire, afterRelease
                    }

                let afterAcquire, afterRelease = runtime.Run(eff).UnsafeSuccess()

                Expect.equal afterAcquire 2 "Should have 2 permits after 3 acquires"
                Expect.equal afterRelease 4 "Should have 4 permits after 2 releases"
        ]

[<Tests>]
let semaphoreWithPermitTests =
    testList
        "Semaphore WithPermit"
        [

            testPropertyWithConfig fsCheckConfig "WithPermit acquires, runs, and releases"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! before = sem.Available()

                        let! result =
                            sem.WithPermit(
                                fio {
                                    let! during = sem.Available()
                                    return res, during
                                },
                                onError
                            )

                        let! after = sem.Available()
                        return before, result, after
                    }

                let before, (value, during), after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 1 "Should have 1 permit before"
                Expect.equal during 0 "Should have 0 permits during"
                Expect.equal after 1 "Should have 1 permit after"
                Expect.equal value res "Should return inner result"

            testPropertyWithConfig fsCheckConfig "WithPermitExn acquires, runs, and releases"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! before = sem.Available()

                        let! result =
                            sem.WithPermit(
                                fio {
                                    let! during = sem.Available()
                                    return res, during
                                },
                                id
                            )

                        let! after = sem.Available()
                        return before, result, after
                    }

                let before, (value, during), after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 1 "Should have 1 permit before"
                Expect.equal during 0 "Should have 0 permits during"
                Expect.equal after 1 "Should have 1 permit after"
                Expect.equal value res "Should return inner result"

            testPropertyWithConfig fsCheckConfig "WithPermit releases on error"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! _ = sem.WithPermit(FIO.fail (exn "error"), id).CatchAll(fun _ -> FIO.unit ())
                        let! after = sem.Available()
                        return after
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "WithPermit should release on error"

            testAllRuntimes "WithPermit releases on interrupt"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        let! fiber = sem.WithPermit(FIO.sleep (TimeSpan.FromSeconds 60.0, id), id).Fork()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        let! during = sem.Available()
                        do! fiber.Interrupt()
                        do! fiber.Join().CatchAll(fun _ -> FIO.unit ())
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, id)
                        let! after = sem.Available()
                        return during, after
                    }

                let during, after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal during 0 "Should have 0 permits during"
                Expect.equal after 1 "WithPermit should release on interrupt"
        ]

[<Tests>]
let semaphoreWithPermitsTests =
    testList
        "Semaphore WithPermits"
        [

            testPropertyWithConfig fsCheckConfig "WithPermits acquires multiple, runs, and releases"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        let! before = sem.Available()

                        let! result =
                            sem.WithPermits(
                                3,
                                fio {
                                    let! during = sem.Available()
                                    return res, during
                                },
                                onError
                            )

                        let! after = sem.Available()
                        return before, result, after
                    }

                let before, (value, during), after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 5 "Should have 5 permits before"
                Expect.equal during 2 "Should have 2 permits during (5-3)"
                Expect.equal after 5 "Should have 5 permits after"
                Expect.equal value res "Should return inner result"

            testPropertyWithConfig fsCheckConfig "WithPermitsExn acquires multiple, runs, and releases"
            <| fun (runtime: FIORuntime, res: int) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        let! before = sem.Available()

                        let! result =
                            sem.WithPermits(
                                3,
                                fio {
                                    let! during = sem.Available()
                                    return res, during
                                },
                                id
                            )

                        let! after = sem.Available()
                        return before, result, after
                    }

                let before, (value, during), after = runtime.Run(eff).UnsafeSuccess()

                Expect.equal before 5 "Should have 5 permits before"
                Expect.equal during 2 "Should have 2 permits during (5-3)"
                Expect.equal after 5 "Should have 5 permits after"
                Expect.equal value res "Should return inner result"

            testPropertyWithConfig fsCheckConfig "WithPermits releases all on error"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5

                        let! _ =
                            sem.WithPermits(3, FIO.fail (exn "error"), id).CatchAll(fun _ -> FIO.unit ())

                        let! after = sem.Available()
                        return after
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 5 "WithPermits should release all on error"
        ]

[<Tests>]
let semaphoreMakeTests =
    testList
        "Semaphore.make / Semaphore.binary"
        [

            testPropertyWithConfig fsCheckConfig "Semaphore.make creates semaphore with permits"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 5
                        let! available = sem.Available()
                        return available
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 5 "Semaphore should have 5 permits"

            testPropertyWithConfig fsCheckConfig "Semaphore.binary creates semaphore with 1 permit"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.binary ()
                        let! available = sem.Available()
                        return available
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "Binary semaphore should have 1 permit"

            testPropertyWithConfig fsCheckConfig "Semaphore.make with invalid permits interrupts"
            <| fun (runtime: FIORuntime) ->
                let eff = Semaphore.make 0

                let result = runtime.Run(eff).UnsafeResult()

                match result with
                | Interrupted _ -> ()
                | other -> failtest $"Expected Interrupted but got: {other}"

            testAllRuntimes
                "Semaphore.make - lazy (constructing once, running twice yields distinct semaphores)"
                (fun runtime ->
                    let eff = Semaphore.make 1

                    let s1 = runtime.Run(eff).UnsafeSuccess()
                    let s2 = runtime.Run(eff).UnsafeSuccess()

                    try
                        Expect.isFalse
                            (obj.ReferenceEquals(s1, s2))
                            "Semaphore.make must allocate a fresh semaphore per run, not at construction"
                    finally
                        (s1 :> IDisposable).Dispose()
                        (s2 :> IDisposable).Dispose())
        ]

[<Tests>]
let semaphoreConcurrencyTests =
    testList
        "Semaphore Concurrency"
        [

            testAllRuntimes "Binary semaphore limits to 1 concurrent" (fun runtime ->
                let eff =
                    fio {
                        let! sem = Semaphore.binary ()
                        let! maxConcurrent = Ref.makeValue 0
                        let! currentConcurrent = Ref.makeValue 0

                        let task =
                            fio {
                                do! currentConcurrent.Update((fun x -> x + 1), id)
                                let! current = currentConcurrent.Get()
                                let! max = maxConcurrent.Get()

                                if current > max then
                                    do! maxConcurrent.Set(current, id)

                                do! FIO.sleep (TimeSpan.FromMilliseconds 10.0, id)
                                do! currentConcurrent.Update((fun x -> x - 1), id)
                            }

                        let! fibers =
                            [ 1..5 ]
                            |> List.map (fun _ -> sem.WithPermit(task, id).Fork())
                            |> FIO.collectAll

                        for fiber in fibers do
                            do! fiber.Join()

                        let! max = maxConcurrent.Get()
                        return max
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result 1 "Binary semaphore should limit to 1 concurrent")

            testAllRuntimes "Semaphore limits to N concurrent" (fun runtime ->
                let permitCount = 3

                let eff =
                    fio {
                        let! sem = Semaphore.make permitCount
                        let! maxConcurrent = Ref.makeValue 0
                        let! currentConcurrent = Ref.makeValue 0

                        let task =
                            fio {
                                do! currentConcurrent.Update((fun x -> x + 1), id)
                                let! current = currentConcurrent.Get()
                                let! max = maxConcurrent.Get()

                                if current > max then
                                    do! maxConcurrent.Set(current, id)

                                do! FIO.sleep (TimeSpan.FromMilliseconds 10.0, id)
                                do! currentConcurrent.Update((fun x -> x - 1), id)
                            }

                        let! fibers =
                            [ 1..10 ]
                            |> List.map (fun _ -> sem.WithPermit(task, id).Fork())
                            |> FIO.collectAll

                        for fiber in fibers do
                            do! fiber.Join()

                        let! max = maxConcurrent.Get()
                        return max
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isLessThanOrEqual result permitCount $"Max concurrent should be <= {permitCount}")
        ]

[<Tests>]
let semaphoreDisposalTests =
    testList
        "Semaphore Disposal"
        [

            testPropertyWithConfig fsCheckConfig "Semaphore implements IDisposable"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! sem = Semaphore.make 1
                        do! sem.Acquire id
                        do! sem.Release id
                        (sem :> IDisposable).Dispose()
                        return true
                    }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue result "Semaphore should be disposable"
        ]
