module FIO.Tests.Extensions.RacingTests

open FIO.Tests.Utilities

open FIO.DSL

open Expecto

open System

[<Tests>]
let tests =
    testList
        "Extension Methods"
        [
            testList
                "Racing"
                [
                    testAllRuntimes "RaceFirst - returns first completing effect" (fun runtime ->
                        let fast = FIO.succeed 1
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 2)
                        let effect = fast.RaceFirst slow

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 1 "RaceFirst should return first completing effect")

                    testAllRuntimes "RaceFirst - propagates error from first completing" (fun runtime ->
                        let error = exn "fast error"
                        let fast = FIO.fail error
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 2)
                        let effect = fast.RaceFirst slow

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result.Message error.Message "RaceFirst should propagate error from first completing")

                    testAllRuntimes "RaceFirst - an interruption that settles first wins the race" (fun runtime ->
                        let interruptedSide =
                            (FIO.sleep (TimeSpan.FromMilliseconds 50.0))
                                .FlatMap(fun () -> FIO.interrupt ExplicitInterrupt "settled first")
                        let slowSuccess =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 2)
                        let effect = interruptedSide.RaceFirst slowSuccess

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"RaceFirst should yield the interruption when it settles first, got: %A{other}")

                    testAllRuntimes "RaceFirst - loser's Ensuring finalizer runs after losing the race" (fun runtime ->
                        let finalized = Channel<int>()
                        let loser = (FIO.never<int, exn>()).Ensuring((finalized.Write 1).Unit())
                        let winner = (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).FlatMap(fun () -> FIO.succeed 42)

                        let effect =
                            (winner.RaceFirst loser).FlatMap <| fun value ->
                                finalized.Read().Map <| fun _ -> value

                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result 42 "RaceFirst should interrupt the loser and run its Ensuring finalizer")

                    testAllRuntimes "Race - a failed first-settler does not win; the race yields the other side's success" (fun runtime ->
                        let failing = FIO.fail (exn "fast failure")
                        let succeeding =
                            (FIO.sleep (TimeSpan.FromMilliseconds 100.0)).FlatMap(fun () -> FIO.succeed 5)
                        let effect = failing.Race succeeding

                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result 5 "Race is first-to-succeed: a fast failure must not win")

                    testAllRuntimes "Race - an interrupted first-settler does not win; the race yields the other side's success" (fun runtime ->
                        let interruptedSide =
                            (FIO.sleep (TimeSpan.FromMilliseconds 30.0))
                                .FlatMap(fun () -> FIO.interrupt ExplicitInterrupt "settled first")
                        let succeeding =
                            (FIO.sleep (TimeSpan.FromMilliseconds 100.0)).FlatMap(fun () -> FIO.succeed 5)
                        let effect = interruptedSide.Race succeeding

                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromSeconds 5.0)

                        let result = runtime.Run(bounded).UnsafeSuccess()

                        Expect.equal result 5 "Race is first-to-succeed: an early interruption must not win")

                    testAllRuntimes "Race - waits on a never-settling loser when the winner fails (documented first-to-succeed semantics)" (fun runtime ->
                        let failing = FIO.fail (exn "fast failure")
                        let never = FIO.never<int, exn> ()
                        let effect = failing.Race never

                        let bounded =
                            effect.TimeoutFail (exn "timeout") (TimeSpan.FromMilliseconds 500.0)

                        let result = runtime.Run(bounded).UnsafeError()

                        Expect.equal result.Message "timeout" "Race must keep waiting for a success after a failure settles first")

                    testAllRuntimes "RaceEither - first racer wins returns Choice1Of2" (fun runtime ->
                        let fast = FIO.succeed 1
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed "slow")
                        let effect = fast.RaceEither slow

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (Choice1Of2 1) "RaceEither should return Choice1Of2 when this wins")

                    testAllRuntimes "RaceEither - second racer wins returns Choice2Of2" (fun runtime ->
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 1)
                        let fast = FIO.succeed "fast"
                        let effect = slow.RaceEither fast

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (Choice2Of2 "fast") "RaceEither should return Choice2Of2 when the right racer wins")

                    testAllRuntimes "RaceEither - fast failure waits for a slow success" (fun runtime ->
                        let fastFail: FIO<int, exn> = FIO.fail (exn "fast error")
                        let slowSucceed =
                            (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).FlatMap(fun () -> FIO.succeed "slow")
                        let effect = fastFail.RaceEither slowSucceed

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result (Choice2Of2 "slow") "RaceEither should wait for a peer success when the first racer fails")

                    testAllRuntimes "Race - both succeed, fastest wins" (fun runtime ->
                        let fast = FIO.succeed 1
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 2)
                        let effect = fast.Race slow

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 1 "Race should return the fastest successful value")

                    testAllRuntimes "Race - slow failure does not interrupt fast success" (fun runtime ->
                        let fastSucceed = FIO.succeed 11
                        let slowFail =
                            (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.fail (exn "slow"))
                        let effect = fastSucceed.Race slowFail

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 11 "Race should return the fast success without waiting for slow failure")

                    testAllRuntimes "Race - both fail returns the later error" (fun runtime ->
                        let fastFail = FIO.fail (exn "fast error")
                        let slowFail =
                            (FIO.sleep (TimeSpan.FromMilliseconds 50.0)).FlatMap(fun () -> FIO.fail (exn "slow error"))
                        let effect = fastFail.Race slowFail

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.equal result.Message "slow error" "Race should fail with the most-recently-received error when both racers fail")
                ]
        ]
