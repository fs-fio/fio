module FIO.Tests.SemaphoreTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

let private trackPeak (inFlight: Ref<int>) (peak: Ref<int>) (work: FIO<unit, string>) : FIO<unit, string> =
    FIO.acquireReleaseWith
        (inFlight.UpdateAndGet((+) 1))
        (fun _ -> inFlight.Update(fun count -> count - 1))
        (fun now -> peak.Update(max now).FlatMap(fun () -> work))

[<Tests>]
let semaphoreTests =
    testList
        "Semaphore"
        [
            testList
                "Permits"
                [
                    testCase "Semaphore - fewer than 1 permit throws ArgumentOutOfRangeException" <| fun () ->
                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Semaphore 0 |> ignore) "Semaphore 0"
                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Semaphore(-1) |> ignore) "Semaphore -1"

                    testCase "Available - starts at the number of permits" <| fun () ->
                        let semaphore = Semaphore 3

                        let available = semaphore.Available

                        Expect.equal available 3 "A new semaphore should have all its permits"

                    testPropertyWithConfig fsCheckConfig "WithPermit - yields the effect's value, holding a permit while it runs"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let semaphore = Semaphore 2
                        let effect : FIO<int * int, string> = semaphore.WithPermit(FIO.succeedWith (fun () -> value, semaphore.Available))

                        let result, availableWhileHeld = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "WithPermit should yield the effect's value"
                        Expect.equal availableWhileHeld 1 "The effect should run holding one permit"
                        Expect.equal semaphore.Available 2 "The permit should be returned once the effect ends"

                    testAllRuntimes "WithPermit - returns the permit when the effect fails" (fun runtime ->
                        let semaphore = Semaphore 1
                        let effect : FIO<unit, string> = semaphore.WithPermit(FIO.fail "boom")

                        let error = runtime.Run(effect).UnsafeError()

                        Expect.equal error "boom" "The effect's error should propagate"
                        Expect.equal semaphore.Available 1 "The permit should be returned after a failure")

                    testAllRuntimes "WithPermit - returns the permit when the effect is interrupted" (fun runtime ->
                        let semaphore = Semaphore 1
                        let effect : FIO<FiberResult<unit, string>, string> =
                            fio {
                                let! holder = semaphore.WithPermit(FIO.never ()).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                return! holder.InterruptAwaitNow()
                            }

                        let result = runtime.Run(effect).UnsafeSuccess()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected the holder to be interrupted but got %A{other}"
                        Expect.isTrue (waitUntil (fun () -> semaphore.Available = 1)) "The permit should be returned after an interruption")

                    testAllRuntimes "WithPermit - returns the permit when the effect throws" (fun runtime ->
                        let semaphore = Semaphore 1
                        let effect : FIO<unit, string> = semaphore.WithPermit(FIO.suspend (fun () -> failwith "effect threw"))

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect _ -> ()
                            | cause -> failtest $"Expected a Defect but got %A{cause}"
                        | other -> failtest $"Expected a defect but got %A{other}"
                        Expect.isTrue (waitUntil (fun () -> semaphore.Available = 1)) "The permit should be returned after a defect")
                ]

            testList
                "Bounding"
                [
                    testAllRuntimes "WithPermit - never runs more effects at once than there are permits" (fun runtime ->
                        let semaphore = Semaphore 3
                        let inFlight = Ref 0
                        let peak = Ref 0
                        let user = semaphore.WithPermit(trackPeak inFlight peak (FIO.sleep (TimeSpan.FromMilliseconds 5.0)))
                        let effect = FIO.forEachParDiscard [ 1..20 ] (fun _ -> user)

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.isLessThanOrEqual (peak.UnsafeGet()) 3 "No more effects than permits should run at once"
                        Expect.equal semaphore.Available 3 "Every permit should be returned")

                    testAllRuntimes "WithPermit - lets as many effects run at once as there are permits" (fun runtime ->
                        let semaphore = Semaphore 3
                        let arrived = Ref 0
                        let allArrived = Promise<unit, string>()
                        let user =
                            semaphore.WithPermit(
                                fio {
                                    let! count = arrived.UpdateAndGet((+) 1)

                                    if count = 3 then
                                        do! allArrived.Succeed(()).Unit()

                                    do! allArrived.Await()
                                })
                        let effect = (FIO.forEachParDiscard [ 1..3 ] (fun _ -> user)).TimeoutFail "the users never held permits at once" (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded () -> ()
                        | other -> failtest $"Expected three users to hold permits at once but got %A{other}")

                    testAllRuntimes "WithPermit - an interrupted waiter takes no permit" (fun runtime ->
                        let semaphore = Semaphore 1
                        let holding = Promise<unit, string>()
                        let release = Promise<unit, string>()
                        let ran = ref false
                        let effect : FIO<FiberResult<unit, string> * bool, string> =
                            fio {
                                let! holder = semaphore.WithPermit(holding.Succeed(()).Unit().FlatMap(fun () -> release.Await())).Fork()
                                do! holding.Await()
                                let! waiter = semaphore.WithPermit(FIO.succeedWith (fun () -> ran.Value <- true)).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                let! waited = waiter.InterruptAwaitNow()
                                do! release.Succeed(()).Unit()
                                do! holder.Join()
                                let! next = semaphore.WithPermit(FIO.unit ()).Timeout(TimeSpan.FromSeconds 5.0)
                                return waited, next.IsSome
                            }

                        let waited, nextGotPermit = runtime.Run(effect).UnsafeSuccess()

                        match waited with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected the waiter to be interrupted but got %A{other}"
                        Expect.isFalse ran.Value "The interrupted waiter's effect should never run"
                        Expect.isTrue nextGotPermit "The permit should still be available to the next caller"
                        Expect.isTrue (waitUntil (fun () -> semaphore.Available = 1)) "No permit should be lost")

                    testAllRuntimes "WithPermit - an uninterruptible waiter keeps waiting through an interruption" (fun runtime ->
                        let semaphore = Semaphore 1
                        let holding = Promise<unit, string>()
                        let entered = Promise<unit, string>()
                        let release = Promise<unit, string>()
                        let ran = ref false
                        let shielded : FIO<unit, string> =
                            FIO.uninterruptible (entered.Succeed(()).Unit().FlatMap(fun () -> semaphore.WithPermit(FIO.succeedWith (fun () -> ran.Value <- true))))
                        let effect : FIO<unit, string> =
                            fio {
                                let! holder = semaphore.WithPermit(holding.Succeed(()).Unit().FlatMap(fun () -> release.Await())).Fork()
                                do! holding.Await()
                                let! waiter = shielded.Fork()
                                do! entered.Await()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! waiter.InterruptNow()
                                do! release.Succeed(()).Unit()
                                do! holder.Join()
                            }

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue (waitForFlag ran) "The uninterruptible waiter should get the permit and run its effect"
                        Expect.isTrue (waitUntil (fun () -> semaphore.Available = 1)) "Its permit should be returned afterwards")

                    testAllRuntimes "WithPermit - runs the effect at the caller's interruptibility" (fun runtime ->
                        let semaphore = Semaphore 1
                        let tokenOf (effect: FIO<Threading.CancellationToken, string>) = effect.Map(fun token -> token.CanBeCanceled)
                        let effect : FIO<bool * bool, string> =
                            fio {
                                let! interruptible = tokenOf (semaphore.WithPermit(FIO.cancellationToken ()))
                                let! shielded = tokenOf (FIO.uninterruptible (semaphore.WithPermit(FIO.cancellationToken ())))
                                return interruptible, shielded
                            }

                        let interruptible, shielded = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue interruptible "An interruptible caller's effect should see a token that can be cancelled"
                        Expect.isFalse shielded "An uninterruptible caller's effect should run uninterruptibly")
                ]

            testList
                "Stress"
                [
                    stressTestAllRuntimes "Stress - interrupting waiters as permits are granted never leaks a permit" (fun runtime ->
                        let rec permitReturned (semaphore: Semaphore) (tries: int) : FIO<unit, string> =
                            if semaphore.Available = 1 then FIO.unit ()
                            elif tries = 0 then FIO.fail "A permit leaked"
                            else (FIO.sleep (TimeSpan.FromMilliseconds 1.0)).FlatMap(fun () -> permitReturned semaphore (tries - 1))
                        let round (semaphore: Semaphore) : FIO<unit, string> =
                            fio {
                                let! holder = semaphore.WithPermit(FIO.yieldNow ()).Fork()
                                let! waiters = FIO.collectAll [ for _ in 1..3 -> semaphore.WithPermit(FIO.unit ()).Fork() ]
                                do! FIO.forEachDiscard waiters (fun waiter -> waiter.InterruptNow())
                                do! FIO.forEachDiscard (holder :: waiters) (fun fiber -> fiber.Await().Unit())
                                do! permitReturned semaphore 5_000
                            }
                        let rounds (semaphore: Semaphore) = FIO.forEachDiscard [ 1..4_000 ] (fun _ -> round semaphore)
                        let effect =
                            (FIO.forEachParDiscard (List.init 8 (fun _ -> Semaphore 1)) rounds).TimeoutFail "timed out" (TimeSpan.FromSeconds 120.0)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Succeeded () -> ()
                        | other -> failtest $"Every round should get its permit back, but got %A{other}")

                    stressTestAllRuntimes "Stress - the permit bound holds under interruption churn" (fun runtime ->
                        let permits = 3
                        let semaphore = Semaphore permits
                        let inFlight = Ref 0
                        let peak = Ref 0
                        let user = semaphore.WithPermit(trackPeak inFlight peak (FIO.yieldNow ()))
                        let round (index: int) : FIO<unit, string> =
                            fio {
                                let! users = FIO.collectAll [ for _ in 1..12 -> user.Fork() ]
                                let doomed = users |> List.indexed |> List.filter (fun (i, _) -> (i + index) % 3 = 0) |> List.map snd
                                do! FIO.forEachDiscard doomed (fun fiber -> fiber.InterruptNow())
                                do! FIO.forEachDiscard users (fun fiber -> fiber.Await().Unit())
                            }
                        let effect = (FIO.forEachDiscard [ 1..500 ] round).TimeoutFail "timed out" (TimeSpan.FromSeconds 60.0)

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.isLessThanOrEqual (peak.UnsafeGet()) permits "No more users than permits should ever hold one at once"
                        Expect.isTrue (waitUntil (fun () -> semaphore.Available = permits)) "Every permit should be returned")
                ]
        ]
