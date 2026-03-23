module FIO.Tests.SemaphoreTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Semaphore
open FIO.Ref

open Expecto

open FIO.Runtime

open System

[<Tests>]
let semaphoreCreationTests =
    testList "Semaphore Creation" [

        testPropertyWithConfig fsCheckConfig "Semaphore.make creates semaphore with permits"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 5
                let! available = sem.Available()
                return available
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 5 "Semaphore should have 5 permits"

        testPropertyWithConfig fsCheckConfig "Semaphore.binary creates semaphore with 1 permit"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.binary<exn>()
                let! available = sem.Available()
                return available
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 1 "Binary semaphore should have 1 permit"

        testPropertyWithConfig fsCheckConfig "Semaphore.make with invalid permits interrupts"
        <| fun (runtime: FIORuntime) ->
            let eff = Semaphore.make<exn> 0
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Interrupted _ -> Expect.isTrue true "Should interrupt with invalid permits"
            | _ -> Expect.isTrue false "Should interrupt with invalid permits"
    ]

[<Tests>]
let semaphoreAcquireReleaseTests =
    testList "Semaphore Acquire/Release" [

        testPropertyWithConfig fsCheckConfig "Acquire decreases available permits"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 3
                let! before = sem.Available()
                do! sem.AcquireExn()
                let! after = sem.Available()
                return (before, after)
            }
            let (before, after) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal before 3 "Should have 3 permits before"
            Expect.equal after 2 "Should have 2 permits after acquire"

        testPropertyWithConfig fsCheckConfig "Release increases available permits"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 3
                do! sem.AcquireExn()
                let! before = sem.Available()
                do! sem.ReleaseExn()
                let! after = sem.Available()
                return (before, after)
            }
            let (before, after) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal before 2 "Should have 2 permits before release"
            Expect.equal after 3 "Should have 3 permits after release"

        testPropertyWithConfig fsCheckConfig "Multiple acquires and releases"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 5
                do! sem.AcquireExn()
                do! sem.AcquireExn()
                do! sem.AcquireExn()
                let! afterAcquire = sem.Available()
                do! sem.ReleaseExn()
                do! sem.ReleaseExn()
                let! afterRelease = sem.Available()
                return (afterAcquire, afterRelease)
            }
            let (afterAcquire, afterRelease) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal afterAcquire 2 "Should have 2 permits after 3 acquires"
            Expect.equal afterRelease 4 "Should have 4 permits after 2 releases"
    ]

[<Tests>]
let semaphoreTryAcquireTests =
    testList "Semaphore TryAcquire" [

        testPropertyWithConfig fsCheckConfig "TryAcquire returns true when permit available"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                let! acquired = sem.TryAcquireExn(TimeSpan.FromMilliseconds 100.0)
                return acquired
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "TryAcquire should return true when permit available"

        testPropertyWithConfig fsCheckConfig "TryAcquire returns false when no permit and timeout"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                do! sem.AcquireExn()  // Take the only permit
                let! acquired = sem.TryAcquireExn(TimeSpan.FromMilliseconds 10.0)
                return acquired
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isFalse result "TryAcquire should return false when no permit"
    ]

[<Tests>]
let semaphoreReleaseManyTests =
    testList "Semaphore ReleaseMany" [

        testPropertyWithConfig fsCheckConfig "ReleaseMany increases permits by count"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 5
                do! sem.AcquireExn()
                do! sem.AcquireExn()
                do! sem.AcquireExn()
                let! before = sem.Available()
                do! sem.ReleaseManyExn 3
                let! after = sem.Available()
                return (before, after)
            }
            let (before, after) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal before 2 "Should have 2 permits before"
            Expect.equal after 5 "Should have 5 permits after releasing 3"
    ]

[<Tests>]
let semaphoreWithPermitTests =
    testList "Semaphore WithPermit" [

        testPropertyWithConfig fsCheckConfig "WithPermit acquires, runs, and releases"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                let! before = sem.Available()
                let! result = sem.WithPermitExn(fio {
                    let! during = sem.Available()
                    return (res, during)
                })
                let! after = sem.Available()
                return (before, result, after)
            }
            let (before, (value, during), after) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal before 1 "Should have 1 permit before"
            Expect.equal during 0 "Should have 0 permits during"
            Expect.equal after 1 "Should have 1 permit after"
            Expect.equal value res "Should return inner result"

        testPropertyWithConfig fsCheckConfig "WithPermit releases on error"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                let innerEff = fio {
                    return! FIO.fail(exn err)
                }
                let! _ = sem.WithPermitExn(innerEff).CatchAll(fun _ -> FIO.unit())
                let! after = sem.Available()
                return after
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 1 "WithPermit should release on error"

        testPropertyWithConfig fsCheckConfig "WithPermit releases on interrupt"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                let innerEff = fio {
                    return! FIO.interrupt<unit, exn>(ExplicitInterrupt, "test interrupt")
                }
                let! fiber = sem.WithPermitExn(innerEff).Fork()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0)
                // The fiber should have completed (with interrupt) and released
                let! after = sem.Available()
                return after
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 1 "WithPermit should release on interrupt"
    ]

[<Tests>]
let semaphoreWithPermitsTests =
    testList "Semaphore WithPermits" [

        testPropertyWithConfig fsCheckConfig "WithPermits acquires multiple, runs, and releases"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 5
                let! before = sem.Available()
                let! result = sem.WithPermitsExn(3, fio {
                    let! during = sem.Available()
                    return (res, during)
                })
                let! after = sem.Available()
                return (before, result, after)
            }
            let (before, (value, during), after) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal before 5 "Should have 5 permits before"
            Expect.equal during 2 "Should have 2 permits during (5-3)"
            Expect.equal after 5 "Should have 5 permits after"
            Expect.equal value res "Should return inner result"

        testPropertyWithConfig fsCheckConfig "WithPermits releases all on error"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 5
                let innerEff = fio {
                    return! FIO.fail(exn "error")
                }
                let! _ = sem.WithPermitsExn(3, innerEff).CatchAll(fun _ -> FIO.unit())
                let! after = sem.Available()
                return after
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 5 "WithPermits should release all on error"
    ]

[<Tests>]
let semaphoreConcurrencyTests =
    testList "Semaphore Concurrency" [

        testPropertyWithConfig fsCheckConfig "Binary semaphore limits to 1 concurrent"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.binary<exn>()
                let! maxConcurrent = Ref.makeValue<int, exn> 0
                let! currentConcurrent = Ref.makeValue<int, exn> 0
                let task = fio {
                    do! currentConcurrent.UpdateExn(fun x -> x + 1)
                    let! current = currentConcurrent.Get()
                    let! max = maxConcurrent.Get()
                    if current > max then
                        do! maxConcurrent.SetExn current
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0)
                    do! currentConcurrent.UpdateExn(fun x -> x - 1)
                }
                // Fork 5 fibers using the semaphore
                let! fibers =
                    [1..5]
                    |> List.map (fun _ -> sem.WithPermitExn(task).Fork())
                    |> FIO.collectAll
                for fiber in fibers do
                    do! fiber.Join()
                let! max = maxConcurrent.Get()
                return max
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result 1 "Binary semaphore should limit to 1 concurrent"

        testPropertyWithConfig fsCheckConfig "Semaphore limits to N concurrent"
        <| fun (runtime: FIORuntime) ->
            let permitCount = 3
            let eff = fio {
                let! sem = Semaphore.make<exn> permitCount
                let! maxConcurrent = Ref.makeValue<int, exn> 0
                let! currentConcurrent = Ref.makeValue<int, exn> 0
                let task = fio {
                    do! currentConcurrent.UpdateExn(fun x -> x + 1)
                    let! current = currentConcurrent.Get()
                    let! max = maxConcurrent.Get()
                    if current > max then
                        do! maxConcurrent.SetExn current
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0)
                    do! currentConcurrent.UpdateExn(fun x -> x - 1)
                }
                // Fork 10 fibers using the semaphore
                let! fibers =
                    [1..10]
                    |> List.map (fun _ -> sem.WithPermitExn(task).Fork())
                    |> FIO.collectAll
                for fiber in fibers do
                    do! fiber.Join()
                let! max = maxConcurrent.Get()
                return max
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isLessThanOrEqual result permitCount $"Max concurrent should be <= {permitCount}"
    ]

[<Tests>]
let semaphoreDisposalTests =
    testList "Semaphore Disposal" [

        testPropertyWithConfig fsCheckConfig "Semaphore implements IDisposable"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! sem = Semaphore.make<exn> 1
                // Use the semaphore, then dispose
                do! sem.AcquireExn()
                do! sem.ReleaseExn()
                (sem :> System.IDisposable).Dispose()
                return true
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Semaphore should be disposable"
    ]
