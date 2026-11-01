module FIO.Tests.Factories.ResourceTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
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
                        let result = fiber.Task().Result

                        match result with
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

                        let result = runtime.Run(effect).Task().Result

                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect defect -> Expect.isTrue (obj.ReferenceEquals(defect, thrown)) "The thrown exception should be the defect"
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}"
                        Expect.isTrue (waitForFlag released) "Release should run when use throws")

                    testCase "acquireReleaseWith - a null acquire throws when the effect is built" (fun () ->
                        let acquire = Unchecked.defaultof<FIO<string, string>>

                        Expect.throwsT<NullReferenceException>
                            (fun () -> FIO.acquireReleaseWith acquire (fun _ -> FIO.unit ()) FIO.succeed |> ignore)
                            "A null acquire should be rejected where the effect is built")

                    testAllRuntimes "acquireReleaseWith - a release that returns null dies with a Defect and still runs an enclosing finalizer" (fun runtime ->
                        let finalized = ref 0
                        let uses = ref 0
                        let useResource resource =
                            uses.Value <- uses.Value + 1
                            FIO.succeed resource
                        let effect : FIO<string, string> =
                            (FIO.acquireReleaseWith (FIO.succeed "resource") (fun _ -> Unchecked.defaultof<FIO<unit, string>>) useResource)
                                .Ensuring(FIO.succeedWith (fun () -> finalized.Value <- finalized.Value + 1))

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.isTrue (waitUntil (fun () -> finalized.Value > 0)) "The enclosing finalizer should run"
                        Expect.equal finalized.Value 1 "The enclosing finalizer should run once"
                        Expect.equal uses.Value 0 "Use should not be called once release returned null"
                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect _ -> ()
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

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

                        let result = fiber.Task().Result

                        match result with
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
                                    "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(oneWorker) :> FIORuntime)
                                ] ->
                                testCase name (fun () ->
                                    let runtime = make ()
                                    use control = new DirectRuntime()
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

            testList
                "Outcome-aware resource management"
                [
                    testPropertyWithConfig fsCheckConfig "acquireReleaseExitWith - releases with Succeeded on success"
                    <| fun (runtime: FIORuntime, value: int) ->
                        let seen = ref None
                        let effect : FIO<int, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.succeed "resource")
                                (fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                                (fun _ -> FIO.succeed value)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result value "Should return the use result"
                        Expect.equal seen.Value (Some(Succeeded value)) "Release should see the success of use"

                    testPropertyWithConfig fsCheckConfig "acquireReleaseExitWith - releases with Failed on use failure"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let seen = ref None
                        let effect : FIO<int, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.succeed "resource")
                                (fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                                (fun _ -> FIO.fail error)

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "Should return the use error"
                        Expect.equal seen.Value (Some(Failed error)) "Release should see the failure of use"

                    testPropertyWithConfig fsCheckConfig "acquireReleaseExitWith - does not run release when acquire fails"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let released = ref false
                        let effect : FIO<int, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.fail error)
                                (fun _ _ -> FIO.succeedWith (fun () -> released.Value <- true))
                                (fun (_: string) -> FIO.succeed 42)

                        let result = runtime.Run(effect).UnsafeError()

                        Expect.equal result error "Should return the acquire error"
                        Expect.isFalse released.Value "Release should not run when acquire fails"

                    testAllRuntimes "acquireReleaseExitWith - releases with Interrupted and its cause when use is interrupted" (fun runtime ->
                        let using = new ManualResetEventSlim false
                        let seen = ref None
                        let effect : FIO<unit, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.succeed "resource")
                                (fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                                (fun _ -> FIO.succeedWith(fun () -> using.Set()).FlatMap(fun () -> FIO.never ()))

                        let fiber = runtime.Run effect

                        Expect.isTrue (using.Wait(TimeSpan.FromSeconds 5.0)) "Use should start"

                        runtime.Run(fiber.Interrupt (ResourceExhaustion "pool closed") "stopping" : FIO<unit, string>).Task().Wait()

                        Expect.isTrue (waitUntil (fun () -> seen.Value.IsSome)) "Release should run when use is interrupted"
                        match seen.Value with
                        | Some(Interrupted ex) -> Expect.equal ex.cause (ResourceExhaustion "pool closed") "Release should see the interruption's cause"
                        | other -> failtest $"Expected Interrupted but got %A{other}")

                    testAllRuntimes "acquireReleaseExitWith - a use function that throws releases with a Defect" (fun runtime ->
                        let thrown = InvalidOperationException "use threw"
                        let seen = ref None
                        let effect : FIO<unit, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.succeed "resource")
                                (fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                                (fun _ -> raise thrown)

                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got %A{other}"
                        Expect.isTrue (waitUntil (fun () -> seen.Value.IsSome)) "Release should run when use throws"
                        match seen.Value with
                        | Some(Interrupted ex) ->
                            match ex.cause with
                            | Defect defect -> Expect.isTrue (obj.ReferenceEquals(defect, thrown)) "Release should see the thrown exception as the defect"
                            | cause -> failtest $"Expected a Defect but got %A{cause}"
                        | other -> failtest $"Expected Interrupted but got %A{other}")

                    testAllRuntimes "acquireReleaseExitWith - releases with Interrupted what a multi-step acquire created after an interrupt" (fun runtime ->
                        let started = new ManualResetEventSlim false
                        let proceed = new ManualResetEventSlim false
                        let seen = ref None
                        let used = ref false
                        let acquire =
                            (FIO.attempt (fun () ->
                                started.Set()
                                proceed.Wait()
                                "resource") id).Map id
                        let release = fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome)
                        let useResource = fun _ -> FIO.succeedWith (fun () -> used.Value <- true)

                        let fiber = runtime.Run(FIO.acquireReleaseExitWith acquire release useResource)

                        Expect.isTrue (started.Wait(TimeSpan.FromSeconds 5.0)) "Acquire should start"

                        runtime.Run(fiber.InterruptNow()).Task().Wait()
                        proceed.Set()

                        Expect.isTrue (waitUntil (fun () -> seen.Value.IsSome)) "Release should run for a resource that acquire created"
                        match seen.Value with
                        | Some(Interrupted _) -> ()
                        | other -> failtest $"Expected release to see the interruption but got %A{other}"
                        Expect.isFalse used.Value "Use should not run once the fiber was interrupted during acquire")

                    testAllRuntimes "acquireReleaseExitWith - acquire and release run uninterruptibly, use at the caller's level" (fun runtime ->
                        let cancellable () = FIO.cancellationToken<exn>().Map(fun token -> token.CanBeCanceled)
                        let inRelease = ref true
                        let effect =
                            FIO.acquireReleaseExitWith
                                (cancellable ())
                                (fun _ _ -> cancellable().Map(fun canBeCanceled -> inRelease.Value <- canBeCanceled))
                                (fun inAcquire -> cancellable().Map(fun inUse -> inAcquire, inUse))

                        let inAcquire, inUse = runtime.Run(effect).UnsafeSuccess()

                        Expect.isFalse inAcquire "Acquire should run uninterruptibly"
                        Expect.isTrue inUse "Use should run at the caller's interruptible level"
                        Expect.isFalse inRelease.Value "Release should run uninterruptibly")

                    testAllRuntimes "acquireReleaseExitWith - a failing release replaces a success, not a failure" (fun runtime ->
                        let failingRelease = fun () (_: FiberResult<int, string>) -> FIO.fail "release failed"
                        let afterSuccess = FIO.acquireReleaseExitWith (FIO.succeed ()) failingRelease (fun () -> FIO.succeed 1)
                        let afterFailure = FIO.acquireReleaseExitWith (FIO.succeed ()) failingRelease (fun () -> FIO.fail "use failed")

                        let fromSuccess = runtime.Run(afterSuccess).UnsafeError()

                        Expect.equal fromSuccess "release failed" "A failing release should replace a success"

                        let fromFailure = runtime.Run(afterFailure).UnsafeError()

                        Expect.equal fromFailure "use failed" "A failing release should not replace the failure of use")

                    testAllRuntimes "acquireReleaseExitWith - each release sees the outcome of its own use" (fun runtime ->
                        let seen = Collections.Generic.List<string * FiberResult<int, string>>()
                        let record label = fun () outcome -> FIO.succeedWith (fun () -> seen.Add((label, outcome)))
                        let inner = FIO.acquireReleaseExitWith (FIO.succeed ()) (record "inner") (fun () -> FIO.fail "inner failed")
                        let effect = FIO.acquireReleaseExitWith (FIO.succeed ()) (record "outer") (fun () -> inner.CatchAll(fun _ -> FIO.succeed 7))

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result 7 "The outer use should recover"
                        Expect.equal (List.ofSeq seen) [ "inner", Failed "inner failed"; "outer", Succeeded 7 ] "Each release should see its own use's outcome, innermost first")

                    testCase "acquireReleaseExitWith - a null acquire throws when the effect is built" (fun () ->
                        let acquire = Unchecked.defaultof<FIO<string, string>>

                        Expect.throwsT<NullReferenceException>
                            (fun () -> FIO.acquireReleaseExitWith acquire (fun _ _ -> FIO.unit ()) FIO.succeed |> ignore)
                            "A null acquire should be rejected where the effect is built")

                    testAllRuntimes "acquireReleaseWith - a use that returns null dies with a Defect and still runs release" (fun runtime ->
                        let released = ref false
                        let effect : FIO<int, string> =
                            FIO.acquireReleaseWith
                                (FIO.succeed "resource")
                                (fun _ -> FIO.succeedWith (fun () -> released.Value <- true))
                                (fun _ -> Unchecked.defaultof<FIO<int, string>>)

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.isTrue (waitForFlag released) "Release should run when use returns null"
                        match result with
                        | Interrupted ex ->
                            match ex.cause with
                            | Defect _ -> ()
                            | other -> failtest $"Expected a Defect cause but got {other}"
                        | other -> failtest $"Expected Interrupted but got {other}")

                    testAllRuntimes "acquireReleaseExitWith - a use that returns null dies with a Defect and hands release the interruption" (fun runtime ->
                        let seen = ref None
                        let effect : FIO<int, string> =
                            FIO.acquireReleaseExitWith
                                (FIO.succeed "resource")
                                (fun _ outcome -> FIO.succeedWith (fun () -> seen.Value <- Some outcome))
                                (fun _ -> Unchecked.defaultof<FIO<int, string>>)

                        let result = runtime.Run(effect).UnsafeResult()

                        Expect.isTrue (waitUntil (fun () -> seen.Value.IsSome)) "Release should run when use returns null"
                        match result, seen.Value with
                        | Interrupted ex, Some(Interrupted seenEx) ->
                            match ex.cause, seenEx.cause with
                            | Defect _, Defect _ -> ()
                            | other -> failtest $"Expected Defect causes but got %A{other}"
                        | other -> failtest $"Expected an interrupted fiber whose release saw the interruption but got %A{other}")
                ]
        ]
