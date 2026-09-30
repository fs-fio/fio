module FIO.Tests.Factories.SuspensionTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System
open System.Threading

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Suspension / fiber-context primitives"
                [
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

                    testAllRuntimes "cancellationToken - yields a non-cancelled token in a healthy fiber" (fun runtime ->
                        let effect = FIO.cancellationToken<string> ()
                        let token = runtime.Run(effect).UnsafeSuccess()

                        Expect.isFalse
                            token.IsCancellationRequested
                            "cancellationToken should yield a non-cancelled token for a running fiber")

                    testAllRuntimes "cancellationToken - yields a token that is cancelled when the fiber is interrupted" (fun runtime ->
                        let observed = ref CancellationToken.None

                        let effect =
                            fio {
                                let! fiber =
                                    (fio {
                                        let! ct = FIO.cancellationToken ()
                                        observed.Value <- ct
                                        do! FIO.never<unit, string> ()
                                        return ct
                                    })
                                        .Fork()

                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "sleep error")
                                do! fiber.InterruptNow ()
                                return observed.Value <> CancellationToken.None, observed.Value.IsCancellationRequested
                            }

                        let observedToken, cancelled = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue observedToken "The forked fiber should have read its token before the interrupt"
                        Expect.isTrue cancelled "The token yielded by cancellationToken should be cancelled after Interrupt")

                    testAllRuntimes "cancellationToken - two reads inside the same fiber yield equal tokens" (fun runtime ->
                        let effect =
                            fio {
                                let! ct1 = FIO.cancellationToken<string> ()
                                let! ct2 = FIO.cancellationToken<string> ()
                                return ct1 = ct2
                            }

                        let equal = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue equal "Repeated reads of cancellationToken in the same fiber should be equal")

                    testAllRuntimes "cancellationToken - sibling fibers receive distinct tokens" (fun runtime ->
                        let effect =
                            fio {
                                let! f1 = FIO.cancellationToken<string>().Fork()
                                let! f2 = FIO.cancellationToken<string>().Fork()
                                let! ct1 = f1.Join()
                                let! ct2 = f2.Join()
                                return ct1 <> ct2
                            }

                        let distinct = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue distinct "Sibling fibers should each see their own CancellationToken")

                    testAllRuntimes "cancellationToken - matches the forked fiber's CancellationToken" (fun runtime ->
                        let effect =
                            fio {
                                let! fiber = FIO.cancellationToken<string>().Fork()
                                let! ct = fiber.Join()
                                return ct = fiber.CancellationToken
                            }

                        let matches = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue matches "Token observed inside a fiber should equal the fiber's CancellationToken")

                    testAllRuntimes "cancellationToken - a parent's interruption cancels the child's observed token" (fun runtime ->
                        let effect =
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

                                do! (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).MapError(fun _ -> "sleep error")
                                do! parent.InterruptNow ()
                                return parent.CancellationToken.IsCancellationRequested
                            }

                        let cancelled = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue
                            cancelled
                            "Interrupting a parent fiber should cancel its CancellationToken (and propagate to children)")
                ]
        ]
