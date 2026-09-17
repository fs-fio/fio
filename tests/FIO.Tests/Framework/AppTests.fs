module FIO.Tests.AppTests

open FIO.App
open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open System
open System.IO
open System.Threading

open Expecto

let private silenceErr (body: unit -> 'a) : 'a =
    let original = Console.Error
    Console.SetError TextWriter.Null
    try body ()
    finally Console.SetError original

type private TestApp
    (
        effect: FIO<int, string>,
        log: ResizeArray<string>,
        ?onShutdown: FIO<unit, string>,
        ?shutdownTimeout: TimeSpan,
        ?onOutcome: AppResult<int, string> -> FIO<unit, string>,
        ?outcomeTimeout: TimeSpan
    ) =
    inherit FIOApp<int, string>()

    override _.effect = effect
    override _.onOutcomeTimeout = defaultArg outcomeTimeout (TimeSpan.FromSeconds 10.0)
    override _.onShutdownTimeout = defaultArg shutdownTimeout (TimeSpan.FromSeconds 10.0)

    override _.onOutcome outcome =
        let caseName =
            match outcome with
            | AppSucceeded _ -> "Succeeded"
            | AppFailed _ -> "Failed"
            | AppInterrupted _ -> "Interrupted"
            | AppFatalError _ -> "FatalError"

        let mark =
            FIO.attempt
                (fun () -> log.Add ("onOutcomeRan:" + caseName))
                (fun (ex: exn) -> ex.Message)

        match onOutcome with
        | Some hook -> mark.FlatMap(fun _ -> hook outcome)
        | None -> mark

    override _.onShutdown() =
        let mark =
            FIO.attempt
                (fun () -> log.Add "onShutdownRan")
                (fun (ex: exn) -> ex.Message)
        match onShutdown with
        | Some hook -> hook.FlatMap(fun _ -> mark)
        | None -> mark

    override _.mapExitCode outcome =
        match outcome with
        | AppSucceeded _ ->
            log.Add "outcome:Succeeded"
            0
        | AppFailed _ ->
            log.Add "outcome:Failed"
            1
        | AppInterrupted _ ->
            log.Add "outcome:Interrupted"
            130
        | AppFatalError _ ->
            log.Add "outcome:FatalError"
            2

type private MinimalApp(effect: FIO<int, string>) =
    inherit FIOApp<int, string>()
    override _.effect = effect

type private CustomExitCodeApp(effect: FIO<int, string>) =
    inherit FIOApp<int, string>()

    override _.effect = effect

    override _.mapExitCode outcome =
        match outcome with
        | AppSucceeded _ -> 10
        | AppFailed _ -> 20
        | AppFatalError _ -> 30
        | AppInterrupted _ -> 40

type private ThrowingDisposeRuntime() =
    inherit DirectRuntime()

    interface IDisposable with
        member _.Dispose() = raise (InvalidOperationException "dispose boom")

type private ThrowingDisposeApp(effect: FIO<int, string>) =
    inherit FIOApp<int, string>()

    override _.effect = effect
    override _.runtime = new ThrowingDisposeRuntime() :> FIORuntime

type private FatalErrorApp(log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    override _.effect = FIO.succeed 42
    override _.runtime = failwith "fatal"

    override _.mapExitCode outcome =
        match outcome with
        | AppFatalError _ -> log.Add "outcome:FatalError"; 99
        | AppSucceeded _ -> log.Add "outcome:Succeeded"; 0
        | AppFailed _ -> log.Add "outcome:Failed"; 1
        | AppInterrupted _ -> log.Add "outcome:Interrupted"; 130

type private FatalAfterRuntimeApp(log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    override _.effect = failwith "effect construction exploded"

    override _.onOutcome outcome =
        let name =
            match outcome with
            | AppSucceeded _ -> "Succeeded"
            | AppFailed _ -> "Failed"
            | AppInterrupted _ -> "Interrupted"
            | AppFatalError _ -> "FatalError"

        FIO.attempt (fun () -> log.Add ("onOutcome:" + name)) (fun ex -> ex.Message)

    override _.onShutdown () =
        FIO.attempt (fun () -> log.Add "onShutdown") (fun ex -> ex.Message)

    override _.mapExitCode outcome =
        match outcome with
        | AppFatalError _ -> 77
        | _ -> 0

type private FatalCleanupThrowsApp() =
    inherit FIOApp<int, string>()

    override _.effect = failwith "effect construction exploded"
    override _.onOutcome _ = FIO.attempt (fun () -> failwith "cleanup exploded") (fun ex -> ex.Message)

    override _.mapExitCode outcome =
        match outcome with
        | AppFatalError _ -> 88
        | _ -> 0

[<Tests>]
let appTests =
    testSequenced
    <| testList
        "FIOApp"
        [
            testList
                "Runtime & Run basics"
                [
                    testCase "runtime - defaults to WorkStealingRuntime"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.isTrue (app.runtime :? WorkStealingRuntime) "Default runtime should be WorkStealingRuntime"

                    testCase "Run - success effect returns exit code 0 by default"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log)

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Success should return exit code 0"
                        Expect.contains (Seq.toList log) "outcome:Succeeded" "mapExitCode should see AppSucceeded"

                    testCase "Run - success runs onShutdown after the main effect"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log)

                        app.Run() |> ignore

                        let order = Seq.toList log

                        let outcomeIdx = List.findIndex (fun e -> e = "outcome:Succeeded") order
                        let shutdownIdx = List.findIndex (fun e -> e = "onShutdownRan") order

                        Expect.isLessThan shutdownIdx outcomeIdx "onShutdown should run before mapExitCode classifies the outcome"

                    testCase "RunAsync - returns same result as Run"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log)

                        let exitCode = app.RunAsync().Result

                        Expect.equal exitCode 0 "RunAsync should return exit code 0 for success"
                ]

            testList
                "Exit code mapping"
                [
                    testCase "Run - error effect returns exit code 1 by default"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.fail "error", log)

                            let exitCode = app.Run()

                            Expect.equal exitCode 1 "Error should return exit code 1"
                            Expect.contains (Seq.toList log) "outcome:Failed" "mapExitCode should see AppFailed")

                    testCase "Run - interrupted effect returns exit code 130 by default"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.interruptNow (), log)

                            let exitCode = app.Run()

                            Expect.equal exitCode 130 "Interrupted should return exit code 130"
                            Expect.contains (Seq.toList log) "outcome:Interrupted" "mapExitCode should see AppInterrupted")

                    testCase "Run - a defect in the effect is a fatal error with the thrown exception"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let seen = ref None

                            let app =
                                TestApp(
                                    FIO.succeedWith (fun () -> failwith "effect defect"),
                                    log,
                                    onOutcome = fun outcome ->
                                        FIO.succeedWith (fun () -> seen.Value <- Some outcome))

                            let exitCode = app.Run()

                            Expect.equal exitCode 2 "A defect is a crash, not an interruption"
                            Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"

                            match seen.Value with
                            | Some(AppFatalError ex) -> Expect.equal ex.Message "effect defect" "onOutcome should see the thrown exception itself"
                            | other -> failtest $"Expected AppFatalError but got {other}")

                    testCase "Run - an invalid argument in the effect is a fatal error"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp((FIO.sleep (TimeSpan.FromSeconds -1.0)).FlatMap(fun () -> FIO.succeed 1), log)

                            let exitCode = app.Run()

                            Expect.equal exitCode 2 "A rejected argument is a crash, not an interruption"
                            Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError")

                    testCase "Run - fatal error (runtime construction throws) returns exit code 2 by default"
                    <| fun () ->
                        silenceErr (fun () ->
                            let app =
                                { new FIOApp<int, string>() with
                                    override _.effect = FIO.succeed 42
                                    override _.runtime = failwith "fatal" }

                            let exitCode = app.Run()

                            Expect.equal exitCode 2 "Fatal error should return exit code 2")

                    testCase "Run - fatal error path invokes mapExitCode with AppFatalError"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = FatalErrorApp log

                            let exitCode = app.Run()

                            Expect.equal exitCode 99 "Custom fatal-error code should win"
                            Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError")

                    testCase "mapExitCode - default classification is 0/1/130/2"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.equal (app.mapExitCode (AppSucceeded 42)) 0 "AppSucceeded -> 0"
                        Expect.equal (app.mapExitCode (AppFailed "x")) 1 "AppFailed -> 1"

                        let ex = FiberInterruptedException(Guid.NewGuid(), ExplicitInterrupt, "x") :?> FiberInterruptedException
                        Expect.equal (app.mapExitCode (AppInterrupted ex)) 130 "AppInterrupted -> 130"
                        Expect.equal (app.mapExitCode (AppFatalError (exn "x"))) 2 "AppFatalError -> 2"

                    testCase "Custom mapExitCode - success uses custom code"
                    <| fun () ->
                        let exitCode = CustomExitCodeApp(FIO.succeed 42).Run()

                        Expect.equal exitCode 10 "Custom success exit code should be 10"

                    testCase "Custom mapExitCode - error uses custom code"
                    <| fun () ->
                        let exitCode = CustomExitCodeApp(FIO.fail "error").Run()

                        Expect.equal exitCode 20 "Custom error exit code should be 20"
                ]

            testList
                "Shutdown hooks"
                [
                    testCase "onShutdown - default hook is a no-op that does not affect exit code"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 42)

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Default shutdown hook should not change success exit code"

                    testCase "onShutdown - custom success hook runs"
                    <| fun () ->
                        let mutable hookRan = false
                        let hook = FIO.attempt (fun () -> hookRan <- true) (fun (ex: exn) -> ex.Message)
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log, onShutdown = hook)

                        app.Run() |> ignore

                        Expect.isTrue hookRan "Custom shutdown hook should have run"
                        Expect.contains (Seq.toList log) "onShutdownRan" "Hook completion should be logged"

                    testCase "onShutdown - failing hook does not prevent exit code from being computed"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log, onShutdown = FIO.fail "hook error")

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Failing shutdown should not change the main outcome's exit code"

                    testCase "onShutdownTimeout - defaults to 10 seconds"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.equal app.onShutdownTimeout (TimeSpan.FromSeconds 10.0) "Default timeout should be 10 seconds"

                    testCase "onShutdownTimeout - is overridable"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()

                            let app =
                                TestApp(
                                    FIO.succeed 42,
                                    log,
                                    onShutdown = FIO.never (),
                                    shutdownTimeout = TimeSpan.FromMilliseconds 100.0
                                )

                            let exitCode = app.Run()

                            Expect.equal exitCode 0 "Timed-out shutdown should still produce the main outcome's exit code")
                ]

            testList
                "onOutcome"
                [
                    testCase "onOutcome - default hook is a no-op that does not affect exit code"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 42)

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Default outcome hook should not change success exit code"

                    testCase "onOutcome - runs with AppSucceeded on success"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log)

                        app.Run() |> ignore

                        Expect.contains (Seq.toList log) "onOutcomeRan:Succeeded" "onOutcome should observe AppSucceeded"

                    testCase "onOutcome - runs with AppFailed on error"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.fail "boom", log)

                            app.Run() |> ignore

                            Expect.contains (Seq.toList log) "onOutcomeRan:Failed" "onOutcome should observe AppFailed")

                    testCase "onOutcome - runs with AppInterrupted on interruption"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.never (), log)

                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            app.Stop()
                            runTask.Result |> ignore

                            Expect.contains (Seq.toList log) "onOutcomeRan:Interrupted" "onOutcome should observe AppInterrupted")

                    testCase "Finalizers - run before onOutcome and onShutdown on success"
                    <| fun () ->
                        let log = ResizeArray()

                        let effect: FIO<int, string> =
                            (FIO.succeed 42)
                                .Ensuring(FIO.attempt (fun () -> log.Add "finalizerRan") (fun (ex: exn) -> ex.Message))

                        TestApp(effect, log).Run() |> ignore

                        let order = Seq.toList log
                        let finalizerIdx = List.findIndex (fun e -> e = "finalizerRan") order
                        let outcomeIdx = List.findIndex (fun e -> e = "onOutcomeRan:Succeeded") order
                        let shutdownIdx = List.findIndex (fun e -> e = "onShutdownRan") order

                        Expect.isLessThan finalizerIdx outcomeIdx "the effect's finalizer should run before onOutcome"
                        Expect.isLessThan finalizerIdx shutdownIdx "the effect's finalizer should run before onShutdown"

                    testCase "Finalizers - run before onOutcome and onShutdown on Stop()"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()

                            // Slow on purpose: the hooks must wait for it, not happen to run after it.
                            let finalizer: FIO<unit, string> =
                                (FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                    .FlatMap(fun () -> FIO.attempt (fun () -> log.Add "finalizerRan") (fun (ex: exn) -> ex.Message))

                            let effect: FIO<int, string> = (FIO.never ()).Ensuring finalizer

                            let app = TestApp(effect, log)
                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            app.Stop()
                            runTask.Result |> ignore

                            let order = Seq.toList log
                            let finalizerIdx = List.findIndex (fun e -> e = "finalizerRan") order
                            let outcomeIdx = List.findIndex (fun e -> e = "onOutcomeRan:Interrupted") order
                            let shutdownIdx = List.findIndex (fun e -> e = "onShutdownRan") order

                            Expect.isLessThan finalizerIdx outcomeIdx "the interrupted effect's finalizer should run before onOutcome"
                            Expect.isLessThan finalizerIdx shutdownIdx "the interrupted effect's finalizer should run before onShutdown")

                    testCase "onOutcome - runs before onShutdown"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log)

                        app.Run() |> ignore

                        let order = Seq.toList log
                        let outcomeIdx = List.findIndex (fun e -> e = "onOutcomeRan:Succeeded") order
                        let shutdownIdx = List.findIndex (fun e -> e = "onShutdownRan") order

                        Expect.isLessThan outcomeIdx shutdownIdx "onOutcome should run before onShutdown"

                    testCase "onOutcome - failing hook does not prevent exit code from being computed"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.succeed 42, log, onOutcome = fun _ -> FIO.fail "hook error")

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Failing outcome hook should not change the exit code"

                    testCase "onOutcomeTimeout - defaults to 10 seconds"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.equal app.onOutcomeTimeout (TimeSpan.FromSeconds 10.0) "Default outcome timeout should be 10 seconds"

                    testCase "onOutcomeTimeout - hanging hook is bounded and still produces exit code"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()

                            let app =
                                TestApp(
                                    FIO.succeed 42,
                                    log,
                                    onOutcome = (fun _ -> FIO.never ()),
                                    outcomeTimeout = TimeSpan.FromMilliseconds 100.0
                                )

                            let exitCode = app.Run()

                            Expect.equal exitCode 0 "Timed-out outcome hook should still produce the main outcome's exit code")
                ]

            testList
                "Lifecycle / Stop"
                [
                    testCase "Run - a single instance cannot be run more than once"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)
                        app.Run() |> ignore

                        Expect.throwsT<InvalidOperationException>
                            (fun () -> app.Run() |> ignore)
                            "Re-running a single instance should throw"

                    testCase "Run - sequential runs do not leak resources"
                    <| fun () ->
                        for _ in 1..10 do
                            let log = ResizeArray()
                            let app = TestApp(FIO.succeed 42, log)
                            let exitCode = app.Run()

                            Expect.equal exitCode 0 "Each run should succeed"

                    testCase "Run - runtime disposal failure is contained and does not mask the exit code"
                    <| fun () ->
                        silenceErr (fun () ->
                            let app = ThrowingDisposeApp(FIO.succeed 42)

                            let exitCode = app.Run()

                            Expect.equal exitCode 0 "A throwing runtime Dispose should not prevent the normal exit code")

                    testCase "IsRunning - false before Run"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.isFalse app.IsRunning "IsRunning should be false before Run"

                    testCase "IsRunning - false after Run completes"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)
                        app.Run() |> ignore

                        Expect.isFalse app.IsRunning "IsRunning should be false after Run completes"

                    testCase "Stop - interrupts running effect"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.never (), log)

                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            app.Stop()
                            let exitCode = runTask.Result

                            Expect.equal exitCode 130 "Stop should cause interrupted exit code"
                            Expect.contains (Seq.toList log) "outcome:Interrupted" "Stop should yield AppInterrupted")

                    testCase "Stop - a request that races startup still interrupts the effect"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let app = TestApp(FIO.never (), log)

                            let runTask = app.RunAsync()
                            app.Stop()
                            let exitCode = runTask.Result

                            Expect.equal exitCode 130 "A Stop issued immediately after RunAsync must not be lost"
                            Expect.contains (Seq.toList log) "outcome:Interrupted" "The effect should have been interrupted")

                    testCase "Stop - no-op when not running"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        app.Stop()

                        Expect.isFalse app.IsRunning "Stop on idle app should not throw"
                ]

            testList
                "Fatal errors raised after the runtime exists"
                [
                    testCase "Run - fatal error after runtime creation still runs onOutcome and onShutdown"
                    <| fun () ->
                        let log = ResizeArray<string>()
                        let exitCode = silenceErr (fun () -> FatalAfterRuntimeApp(log).Run())

                        Expect.equal exitCode 77 "The fatal exit code must be produced by mapExitCode"
                        Expect.contains log "onOutcome:FatalError" "onOutcome must run with AppFatalError"
                        Expect.contains log "onShutdown" "onShutdown must still run after a fatal error"

                    testCase "Run - a throwing fatal-cleanup hook does not mask the exit code"
                    <| fun () ->
                        let exitCode = silenceErr (fun () -> FatalCleanupThrowsApp().Run())

                        Expect.equal exitCode 88 "A failing cleanup hook must not change the fatal exit code"
                ]
        ]
