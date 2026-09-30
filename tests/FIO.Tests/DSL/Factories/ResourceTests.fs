module FIO.Tests.Factories.ResourceTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Threading
open System.Threading.Tasks

[<Tests>]
let tests =
    testList
        "Factory Functions"
        [
            testList
                "Resource management"
                [
                    testPropertyWithConfig fsCheckConfig "acquireReleaseWith - runs release on success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let mutable released = false
                        let acquire = FIO.succeed "resource"

                        let release =
                            fun _ ->
                                released <- true
                                FIO.unit ()

                        let useResource = fun _ -> FIO.succeed value

                        let effect = FIO.acquireReleaseWith acquire release useResource

                        let result =
                            runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue released "Release should be called on success"
                        Expect.equal result value "Should return use result"

                    testPropertyWithConfig fsCheckConfig "acquireReleaseWith - runs release on use failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let mutable released = false
                        let acquire = FIO.succeed "resource"

                        let release =
                            fun _ ->
                                released <- true
                                FIO.unit ()

                        let useResource = fun _ -> FIO.fail error

                        let effect = FIO.acquireReleaseWith acquire release useResource

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.isTrue released "Release should be called even on use failure"
                        Expect.equal result error "Should return use error"

                    testPropertyWithConfig fsCheckConfig "acquireReleaseWith - does not run release when acquire fails"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let mutable released = false
                        let acquire = FIO.fail error

                        let release =
                            fun _ ->
                                released <- true
                                FIO.unit ()

                        let useResource = fun _ -> FIO.succeed 42

                        let effect = FIO.acquireReleaseWith acquire release useResource

                        let result =
                            runtime.Run(effect).UnsafeError()

                        Expect.isFalse released "Release should not be called when acquire fails"
                        Expect.equal result error "Should return acquire error"

                    testPropertyWithConfig fsCheckConfig "acquireReleaseWith - releases in reverse order when nested"
                    <| fun (runtime: FIORuntime) ->
                        let mutable releaseOrder = []
                        let acquire1 = FIO.succeed "r1"

                        let release1 =
                            fun _ -> (FIO.attempt (fun () -> releaseOrder <- releaseOrder @ [ 1 ]) id).Unit()

                        let acquire2 = FIO.succeed "r2"

                        let release2 =
                            fun _ -> (FIO.attempt (fun () -> releaseOrder <- releaseOrder @ [ 2 ]) id).Unit()

                        let effect =
                            FIO.acquireReleaseWith
                                acquire1
                                release1
                                (fun _ -> FIO.acquireReleaseWith acquire2 release2 (fun _ -> FIO.succeed 42))

                        let _ =
                            runtime.Run(effect).UnsafeSuccess()
                        Expect.equal releaseOrder [ 2; 1 ] "Nested resources should release in reverse order"

                    testAllRuntimes "acquireReleaseWith - interrupting use runs release" (fun runtime ->
                        let using = new ManualResetEventSlim(false)
                        let released = ref false

                        let useResource =
                            fun _ -> FIO.succeedWith(fun () -> using.Set()).FlatMap(fun () -> FIO.never<unit, exn> ())

                        let effect =
                            FIO.acquireReleaseWith (FIO.succeed "resource") (fun _ -> FIO.succeedWith (fun () -> released.Value <- true)) useResource

                        let fiber = runtime.Run effect
                        Expect.isTrue (using.Wait(TimeSpan.FromSeconds 5.0)) "Use should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()

                        Expect.isTrue (waitForFlag released) "Release should run when use is interrupted")

                    testAllRuntimes "acquireReleaseWith - releases what a multi-step acquire created after an interrupt" (fun runtime ->
                        let started = new ManualResetEventSlim(false)
                        let proceed = new ManualResetEventSlim(false)
                        let released = new ManualResetEventSlim(false)

                        let acquire =
                            (FIO.attempt (fun () ->
                                started.Set()
                                proceed.Wait()
                                "resource") id).Map id

                        let used = ref false
                        let release = fun _ -> FIO.succeedWith (fun () -> released.Set())
                        let useResource = fun _ -> FIO.succeedWith (fun () -> used.Value <- true)
                        let fiber = runtime.Run(FIO.acquireReleaseWith acquire release useResource)

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        proceed.Set()

                        match fiber.Task().Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted, got {other}"

                        Expect.isTrue
                            (released.Wait(TimeSpan.FromSeconds 5.0))
                            "Release should run for a resource that acquire created"

                        Expect.isFalse used.Value "Use should not run once the fiber was interrupted during acquire")

                    testAllRuntimes "acquireReleaseWith - a use function that throws still runs release" (fun runtime ->
                        let released = ref false
                        let thrown = InvalidOperationException "use threw"

                        let effect : FIO<unit, exn> =
                            FIO.acquireReleaseWith
                                (FIO.succeed "resource")
                                (fun _ -> FIO.succeedWith (fun () -> released.Value <- true))
                                (fun _ -> raise thrown)

                        match runtime.Run(effect).Task().Result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect -> Expect.isTrue (obj.ReferenceEquals(defect, thrown)) "The thrown exception should be the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}"

                        Expect.isTrue (waitForFlag released) "Release should run when use throws")

                    testAllRuntimes "acquireReleaseWith - an acquire that fails after an interrupt ends interrupted, without release" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let proceed = new ManualResetEventSlim false
                        let unwound = new ManualResetEventSlim false
                        let released = ref false

                        let acquire =
                            (FIO.attempt (fun () ->
                                started.Set()
                                proceed.Wait()) id)
                                .FlatMap(fun () -> FIO.fail (InvalidOperationException "acquire failed" :> exn))

                        let effect =
                            (FIO.acquireReleaseWith acquire (fun _ -> FIO.succeedWith (fun () -> released.Value <- true)) (fun _ -> FIO.unit ()))
                                .Ensuring(FIO.succeedWith (fun () -> unwound.Set()))

                        let fiber = runtime.Run effect
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        proceed.Set()

                        Expect.isTrue (unwound.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should unwind"

                        match fiber.Task().Result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted, got {other}"

                        Expect.isFalse released.Value "Release should not run when acquire failed")

                    testAllRuntimes "acquireReleaseWith - the failure of an interrupted acquire reaches no error handler" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let proceed = new ManualResetEventSlim false
                        let unwound = new ManualResetEventSlim false
                        let handled = ref false

                        let acquire =
                            (FIO.attempt (fun () ->
                                started.Set()
                                proceed.Wait()) id)
                                .FlatMap(fun () -> FIO.fail (InvalidOperationException "acquire failed" :> exn))

                        let effect =
                            (FIO.acquireReleaseWith acquire (fun _ -> FIO.unit ()) (fun _ -> FIO.unit ()))
                                .CatchAll(fun _ ->
                                    handled.Value <- true
                                    FIO.unit ())
                                .Ensuring(FIO.succeedWith (fun () -> unwound.Set()))

                        let fiber = runtime.Run effect
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        proceed.Set()

                        Expect.isTrue (unwound.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should unwind"
                        Expect.isFalse handled.Value "No error handler should run once the fiber was interrupted during acquire")

                    testAllRuntimes "acquireReleaseWith - the failure of an interrupted acquire reaches no error handler in a restored region" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let proceed = new ManualResetEventSlim false
                        let unwound = new ManualResetEventSlim false
                        let handlerEffectRan = ref false

                        let acquire =
                            (FIO.attempt (fun () ->
                                started.Set()
                                proceed.Wait()) id)
                                .FlatMap(fun () -> FIO.fail (InvalidOperationException "acquire failed" :> exn))

                        let effect =
                            (FIO.uninterruptibleMask (fun restorer ->
                                (restorer.Restore(FIO.acquireReleaseWith acquire (fun _ -> FIO.unit ()) (fun _ -> FIO.unit ())))
                                    .CatchAll(fun _ -> FIO.succeedWith (fun () -> handlerEffectRan.Value <- true))))
                                .Ensuring(FIO.succeedWith (fun () -> unwound.Set()))

                        let fiber = runtime.Run effect
                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"
                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        proceed.Set()

                        Expect.isTrue (unwound.Wait(TimeSpan.FromSeconds 5.0)) "The fiber should unwind"
                        Expect.isFalse handlerEffectRan.Value "No error handler should run once the fiber was interrupted during acquire")

                    testAllRuntimes "acquireReleaseWith - acquire and release run uninterruptibly, use at the caller's level" (fun runtime ->
                        let cancellable () = FIO.cancellationToken<exn>().Map(fun token -> token.CanBeCanceled)
                        let inRelease = ref true

                        let effect =
                            FIO.acquireReleaseWith
                                (cancellable ())
                                (fun _ -> cancellable().Map(fun canBeCanceled -> inRelease.Value <- canBeCanceled))
                                (fun inAcquire -> cancellable().Map(fun inUse -> inAcquire, inUse))

                        let inAcquire, inUse = runtime.Run(effect).UnsafeSuccess()
                        Expect.isFalse inAcquire "Acquire should run uninterruptibly"
                        Expect.isTrue inUse "Use should run at the caller's interruptible level"
                        Expect.isFalse inRelease.Value "Release should run uninterruptibly"

                        let inUseWithinRegion =
                            runtime
                                .Run(FIO.uninterruptible (FIO.acquireReleaseWith (FIO.unit<exn> ()) (fun _ -> FIO.unit ()) (fun () -> cancellable ())))
                                .UnsafeSuccess()

                        Expect.isFalse inUseWithinRegion "Use inside an uninterruptible region should stay uninterruptible")

                    testAllRuntimes "acquireReleaseWith - releases through CatchAll and Fork" (fun runtime ->
                        let released = ref 0

                        let failing () : FIO<int, exn> =
                            FIO.acquireReleaseWith
                                (FIO.succeed 1)
                                (fun _ -> FIO.succeedWith (fun () -> released.Value <- released.Value + 1))
                                (fun _ -> FIO.fail (InvalidOperationException "use failed" :> exn))

                        let recovered = runtime.Run(failing().CatchAll(fun _ -> FIO.succeed 42)).UnsafeSuccess()
                        Expect.equal recovered 42 "CatchAll should recover the failure of use"

                        let joined = runtime.Run(failing().Fork().FlatMap(fun fiber -> fiber.Join()).Result()).UnsafeSuccess()
                        Expect.isError joined "Joining the forked fiber should surface the failure of use"
                        Expect.equal released.Value 2 "Release should run on both paths")

                    testList
                        "acquireReleaseWith - releases what an async acquire produced before a late interrupt"
                        [
                            let oneWorker = { testConfig with EvaluationWorkers = 1 }

                            for name, make in
                                [
                                    "PollingRuntime", (fun () -> new PollingRuntime(oneWorker) :> FIORuntime)
                                    "SignalingRuntime", (fun () -> new SignalingRuntime(oneWorker) :> FIORuntime)
                                    "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(oneWorker) :> FIORuntime)
                                ] ->
                                testCase name (fun () ->
                                    let runtime = make ()
                                    let control = new DirectRuntime()
                                    let parked = new ManualResetEventSlim false
                                    let blocking = new ManualResetEventSlim false
                                    let unblock = new ManualResetEventSlim false
                                    let released = new ManualResetEventSlim false
                                    let resource = TaskCompletionSource<string> TaskCreationOptions.RunContinuationsAsynchronously

                                    let acquire =
                                        FIO.succeedWith(fun () -> parked.Set()).FlatMap(fun () -> FIO.awaitTask resource.Task id)

                                    let release = fun _ -> FIO.succeedWith (fun () -> released.Set())

                                    try
                                        let fiber = runtime.Run(FIO.acquireReleaseWith acquire release (fun _ -> FIO.unit ()))
                                        Expect.isTrue (parked.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"
                                        Thread.Sleep 50

                                        let blocker =
                                            runtime.Run(FIO.succeedWith (fun () ->
                                                blocking.Set()
                                                unblock.Wait()) : FIO<unit, exn>)

                                        Expect.isTrue (blocking.Wait(TimeSpan.FromSeconds 5.0)) "The blocker should hold the only worker"
                                        resource.SetResult "resource"
                                        Thread.Sleep 50
                                        control.Run(fiber.InterruptNow()).Task().Wait()
                                        unblock.Set()
                                        blocker.Task().Wait()
                                        fiber.Task().Wait()

                                        Expect.isTrue
                                            (released.Wait(TimeSpan.FromSeconds 5.0))
                                            "Release should run for a resource the awaited task produced"
                                    finally
                                        unblock.Set()

                                        match box runtime with
                                        | :? IDisposable as disposable -> disposable.Dispose()
                                        | _ -> ())
                        ]
                ]
        ]
