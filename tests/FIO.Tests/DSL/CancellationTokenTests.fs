/// <summary>Provides tests for the FIO.cancellationToken factory and its interaction with fiber interruption.</summary>
module FIO.Tests.CancellationTokenTests

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let cancellationTokenTests =
    testList
        "FIO.cancellationToken"
        [
            // ─── Within the running fiber ─────────────────────────────────────────

            testAllRuntimes "Returns a non-cancelled token in a healthy fiber" (fun runtime ->
                let eff = FIO.cancellationToken<string> ()
                let token = runtime.Run(eff).UnsafeSuccess()

                Expect.isFalse
                    token.IsCancellationRequested
                    "cancellationToken should yield a non-cancelled token for a running fiber")

            testAllRuntimes "Cancels when the running fiber is interrupted" (fun runtime ->
                let eff =
                    fio {
                        let! fiber =
                            (fio {
                                let! ct = FIO.cancellationToken ()
                                do! FIO.never<unit, string> ()
                                return ct
                            })
                                .Fork()

                        do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "sleep error")
                        do! fiber.InterruptNow ()
                        return fiber.CancellationToken.IsCancellationRequested
                    }

                let cancelled = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue cancelled "Fiber's CancellationToken should be cancelled after Interrupt")

            testAllRuntimes "Two reads inside the same fiber yield equal tokens" (fun runtime ->
                let eff =
                    fio {
                        let! ct1 = FIO.cancellationToken<string> ()
                        let! ct2 = FIO.cancellationToken<string> ()
                        return ct1 = ct2
                    }

                let equal = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue equal "Repeated reads of cancellationToken in the same fiber should be equal")

            // ─── Across fiber boundaries ─────────────────────────────────────────

            testAllRuntimes "Sibling fibers receive distinct tokens" (fun runtime ->
                let eff =
                    fio {
                        let! f1 = FIO.cancellationToken<string>().Fork()
                        let! f2 = FIO.cancellationToken<string>().Fork()
                        let! ct1 = f1.Join()
                        let! ct2 = f2.Join()
                        return ct1 <> ct2
                    }

                let distinct = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue distinct "Sibling fibers should each see their own CancellationToken")

            testAllRuntimes "Token from a forked fiber matches that fiber's CancellationToken" (fun runtime ->
                let eff =
                    fio {
                        let! fiber = FIO.cancellationToken<string>().Fork()
                        let! ct = fiber.Join()
                        return ct = fiber.CancellationToken
                    }

                let matches = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue matches "Token observed inside a fiber should equal the fiber's CancellationToken")

            testAllRuntimes "Parent interruption cancels child's observed token" (fun runtime ->
                let eff =
                    fio {
                        let! parent =
                            (fio {
                                let! childFiber =
                                    (fio {
                                        let! ct = FIO.cancellationToken ()
                                        do! FIO.never<unit, string> ()
                                        return ct
                                    })
                                        .Fork()

                                let! ct = childFiber.Join()
                                return ct
                            })
                                .Fork()

                        do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0) id).MapError(fun _ -> "sleep error")
                        do! parent.InterruptNow ()
                        return parent.CancellationToken.IsCancellationRequested
                    }

                let cancelled = runtime.Run(eff).UnsafeSuccess()

                Expect.isTrue
                    cancelled
                    "Interrupting a parent fiber should cancel its CancellationToken (and propagate to children)")
        ]
