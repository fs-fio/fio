module FIO.Tests.Factories.InterruptibilityTests

open FIO.Tests.Utilities

open FIO.DSL

open Expecto

open System
open System.Threading

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Interruptibility"
                [
                    testCase "uninterruptibleMask - building the effect does not run the body" <| fun () ->
                        let mutable ran = false

                        let _ =
                            FIO.uninterruptibleMask (fun _ ->
                                ran <- true
                                FIO.unit<exn> ())

                        Expect.isFalse ran "The body should run only when the effect runs"

                    testAllRuntimes "uninterruptible - an interruption takes effect once the region ends" (fun runtime ->
                        let entered = new ManualResetEventSlim(false)
                        let gate = new ManualResetEventSlim(false)
                        let finished = ref false
                        let after = ref false

                        let region =
                            (FIO.attempt (fun () ->
                                entered.Set()
                                gate.Wait()) id)
                                .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 20.0))
                                .FlatMap(fun () -> FIO.succeedWith (fun () -> finished.Value <- true))

                        let fiber =
                            runtime.Run((FIO.uninterruptible region).FlatMap(fun () -> FIO.succeedWith (fun () -> after.Value <- true)))

                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The region should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        gate.Set()

                        Expect.isTrue (waitForFlag finished) "The region should run to its end despite the interruption"
                        Thread.Sleep 100
                        Expect.isFalse after.Value "Nothing after the region should run"

                        match fiber.Task().Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted, got {other}")

                    testAllRuntimes "uninterruptible - code after the region does not run once the fiber is interrupted" (fun runtime ->
                        let entered = new ManualResetEventSlim false
                        let gate = new ManualResetEventSlim false
                        let after = ref false
                        let unwound = ref false

                        let region =
                            FIO.attempt (fun () ->
                                entered.Set()
                                gate.Wait()) id

                        let effect =
                            fio {
                                do! FIO.uninterruptible region
                                after.Value <- true
                            }

                        let fiber = runtime.Run(effect.Ensuring(FIO.succeedWith (fun () -> unwound.Value <- true)))
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The region should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        gate.Set()

                        Expect.isTrue (waitForFlag unwound) "The fiber should unwind once the region ends"
                        Expect.isFalse after.Value "Code in the continuation after the region should not run")

                    testAllRuntimes "Ensuring - code after a finalizer does not run once the fiber was interrupted during it" (fun runtime ->
                        let entered = new ManualResetEventSlim false
                        let gate = new ManualResetEventSlim false
                        let after = ref false
                        let unwound = ref false

                        let finalizer =
                            FIO.attempt (fun () ->
                                entered.Set()
                                gate.Wait()) id

                        let effect =
                            fio {
                                do! FIO.unit<exn>().Ensuring finalizer
                                after.Value <- true
                            }

                        let fiber = runtime.Run(effect.Ensuring(FIO.succeedWith (fun () -> unwound.Value <- true)))
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The finalizer should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        gate.Set()

                        Expect.isTrue (waitForFlag unwound) "The fiber should unwind once the finalizer ends"
                        Expect.isFalse after.Value "Code in the continuation after the finalizer should not run")

                    testAllRuntimes "Uninterruptible - the member form defers interruption too" (fun runtime ->
                        let entered = new ManualResetEventSlim false
                        let gate = new ManualResetEventSlim false
                        let finished = ref false

                        let region =
                            (FIO.attempt (fun () ->
                                entered.Set()
                                gate.Wait()) id)
                                .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 20.0))
                                .FlatMap(fun () -> FIO.succeedWith (fun () -> finished.Value <- true))

                        let fiber = runtime.Run(region.Uninterruptible())
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The region should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        gate.Set()

                        Expect.isTrue (waitForFlag finished) "The region should run to its end despite the interruption")

                    testAllRuntimes "uninterruptibleMask - restore runs the body interruptibly" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let released = ref false

                        let body =
                            FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never<unit, exn> ())

                        let effect =
                            FIO.uninterruptibleMask (fun restore ->
                                restore.Restore(body).Ensuring(FIO.succeedWith (fun () -> released.Value <- true)))

                        let fiber = runtime.Run effect
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The restored body should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag released) "Interrupting the restored body should run its finalizer")

                    testAllRuntimes "uninterruptibleMask - restore inside a finalizer stays uninterruptible" (fun runtime ->
                        let finished = ref false

                        let finalizer =
                            FIO.uninterruptibleMask (fun restore ->
                                restore.Restore(
                                    FIO.sleep(TimeSpan.FromMilliseconds 50.0).FlatMap(fun () ->
                                        FIO.succeedWith (fun () -> finished.Value <- true))))

                        let started = new ManualResetEventSlim false

                        let body =
                            FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never<unit, exn> ())

                        let fiber = runtime.Run(body.Ensuring finalizer)
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The body should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag finished) "The finalizer should run to its end")

                    testAllRuntimes "uninterruptible - FIO.cancellationToken cannot be cancelled inside the region" (fun runtime ->
                        let inside, outside =
                            runtime
                                .Run(
                                    (FIO.uninterruptible (FIO.cancellationToken<exn> ())).FlatMap(fun inner ->
                                        FIO.cancellationToken<exn>().Map(fun outer -> inner.CanBeCanceled, outer.CanBeCanceled))
                                )
                                .UnsafeSuccess()

                        Expect.isFalse inside "Inside the region the token should not be cancellable"
                        Expect.isTrue outside "Outside the region the fiber's own token should be used")

                    testAllRuntimes "uninterruptibleMask - each restorer restores its own mask's outer interruptibility" (fun runtime ->
                        let innerRestored, outerRestored =
                            runtime
                                .Run(
                                    FIO.uninterruptibleMask (fun outerMask ->
                                        FIO.uninterruptibleMask (fun innerMask ->
                                            innerMask.Restore(FIO.cancellationToken<exn> ()).FlatMap(fun inner ->
                                                outerMask
                                                    .Restore(FIO.cancellationToken<exn> ())
                                                    .Map(fun outer -> inner.CanBeCanceled, outer.CanBeCanceled))))
                                )
                                .UnsafeSuccess()

                        Expect.isFalse innerRestored "The inner restorer should restore the outer mask's uninterruptible level"
                        Expect.isTrue outerRestored "The outer restorer should restore the caller's interruptible level")

                    testAllRuntimes "uninterruptibleMask - a restore in tail position leaves the levels around it intact" (fun runtime ->
                        let inTail, afterInner, afterOuter =
                            runtime
                                .Run(
                                    FIO.uninterruptibleMask(fun outerMask ->
                                        FIO.uninterruptibleMask(fun _ -> outerMask.Restore(FIO.cancellationToken<exn> ()))
                                            .FlatMap(fun inTail -> FIO.cancellationToken<exn>().Map(fun afterInner -> inTail, afterInner)))
                                        .FlatMap(fun (inTail, afterInner) ->
                                            FIO.cancellationToken<exn>().Map(fun afterOuter ->
                                                inTail.CanBeCanceled, afterInner.CanBeCanceled, afterOuter.CanBeCanceled))
                                )
                                .UnsafeSuccess()

                        Expect.isTrue inTail "A restore in tail position should run at the caller's interruptible level"
                        Expect.isFalse afterInner "Code after the inner mask should be uninterruptible again"
                        Expect.isTrue afterOuter "Code after the outer mask should be interruptible")

                    testAllRuntimes "uninterruptible - a region that ends at an interruptible level in tail position still ends the fiber there" (fun runtime ->
                        let entered = new ManualResetEventSlim(false)
                        let gate = new ManualResetEventSlim(false)
                        let finished = ref false
                        let after = ref false

                        let body =
                            (FIO.attempt (fun () ->
                                entered.Set()
                                gate.Wait()) id)
                                .FlatMap(fun () -> FIO.succeedWith (fun () -> finished.Value <- true))

                        let effect =
                            FIO.uninterruptibleMask(fun mask ->
                                (FIO.uninterruptible (mask.Restore(FIO.uninterruptible body)))
                                    .FlatMap(fun () -> FIO.succeedWith (fun () -> after.Value <- true)))

                        let fiber = runtime.Run effect
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The body should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        gate.Set()

                        Expect.isTrue (waitForFlag finished) "The body should run to its end despite the interruption"
                        Thread.Sleep 100
                        Expect.isFalse after.Value "Nothing after the restored region should run once the fiber is interrupted"

                        match fiber.Task().Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted, got {other}")

                    testAllRuntimes "Ensuring - a finalizer can time out its cleanup after an interruption" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let cleaned = ref false

                        let cleanup =
                            FIO.succeedWith (fun () -> cleaned.Value <- true)

                        let body =
                            FIO.unit<exn>().Fork()
                                .FlatMap(fun _ -> FIO.succeedWith (fun () -> started.Set()))
                                .FlatMap(fun () -> FIO.never<unit, exn> ())

                        let fiber = runtime.Run(body.Ensuring(cleanup.Timeout(TimeSpan.FromSeconds 1.0).Unit()))
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The body should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag cleaned) "A cleanup raced against its timeout must still run")

                    testAllRuntimes "uninterruptible - work the region forks finishes despite an interruption" (fun runtime ->
                        let entered = new ManualResetEventSlim(false)
                        let recorded = ref false
                        let pair = ref (0, 0)

                        let left : FIO<int, exn> =
                            FIO.succeedWith(fun () -> entered.Set())
                                .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                .Map(fun () -> 1)

                        let right =
                            (FIO.sleep (TimeSpan.FromMilliseconds 100.0)).Map(fun () -> 2)

                        let region : FIO<unit, exn> =
                            (left <&> right).FlatMap(fun zipped ->
                                FIO.succeedWith (fun () ->
                                    pair.Value <- zipped
                                    recorded.Value <- true))

                        let fiber = runtime.Run(FIO.uninterruptible region)
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The region's forked work should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag recorded) "The region's forked work must finish despite the interruption"
                        Expect.equal pair.Value (1, 2) "Both sides should have produced their value")

                    testAllRuntimes "acquireReleaseWith - an acquire that times out still hands its resource to release" (fun runtime ->
                        let entered = new ManualResetEventSlim(false)
                        let released = ref false

                        let acquire =
                            (FIO.succeedWith(fun () -> entered.Set())
                                .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                .Map(fun () -> 42))
                                .TimeoutFail (exn "acquire timed out") (TimeSpan.FromSeconds 5.0)

                        let effect =
                            FIO.acquireReleaseWith
                                acquire
                                (fun _ -> FIO.succeedWith (fun () -> released.Value <- true))
                                (fun _ -> FIO.never<unit, exn> ())

                        let fiber = runtime.Run effect
                        Expect.isTrue (entered.Wait(TimeSpan.FromSeconds 5.0)) "The acquire should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag released) "A resource acquired under a timeout must still be released")

                    testAllRuntimes "Ensuring - a fiber a finalizer forks is interrupted when the fiber exits" (fun runtime ->
                        let forkedFinalized = ref false

                        let forked =
                            FIO.never<unit, exn>().Ensuring(FIO.succeedWith (fun () -> forkedFinalized.Value <- true))

                        let effect =
                            fio {
                                let! fiber = (FIO.never<unit, exn>().Ensuring(forked.Fork().Unit())).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! fiber.InterruptNow()
                            }

                        runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue forkedFinalized.Value "A fiber must not finish unwinding before what its finalizer forked")

                    testAllRuntimes "Fork - an ordinary child is interrupted with its parent, before the parent's finalizers end" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let finalizerDone = ref false
                        let childInterrupted = ref false

                        let effect =
                            FIO.never<unit, exn>().Fork().FlatMap(fun child ->
                                FIO.succeedWith(fun () -> started.Set())
                                    .FlatMap(fun () -> FIO.never<unit, exn> ())
                                    .Ensuring(
                                        (FIO.sleep (TimeSpan.FromMilliseconds 100.0)).FlatMap(fun () ->
                                            FIO.succeedWith (fun () ->
                                                childInterrupted.Value <- child.IsInterrupted()
                                                finalizerDone.Value <- true))))

                        let fiber = runtime.Run effect
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "The body should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag finalizerDone) "The parent's finalizer should run"
                        Expect.isTrue childInterrupted.Value "An ordinary child must be interrupted as soon as its parent is")
                ]
        ]
