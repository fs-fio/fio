module FIO.Tests.AppTests

open FIO.Tests.Utilities
open FIO.Tests.ChildProcess

open FIO.App
open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open System
open System.IO
open System.Threading

open Expecto

let private silenceErr (body: unit -> 'a) =
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
        ?outcomeTimeout: TimeSpan,
        ?runtime: FIORuntime
    ) =
    inherit FIOApp<int, string>()

    override _.effect = effect

    override _.runtime =
        match runtime with
        | Some runtime -> runtime
        | None -> new WorkStealingRuntime() :> FIORuntime
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
        | AppSucceeded _ -> log.Add "outcome:Succeeded"
        | AppFailed _ -> log.Add "outcome:Failed"
        | AppInterrupted _ -> log.Add "outcome:Interrupted"
        | AppFatalError _ -> log.Add "outcome:FatalError"

        base.mapExitCode outcome

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
    override _.onOutcome _ = failwith "cleanup exploded while building its effect"

    override _.mapExitCode outcome =
        match outcome with
        | AppFatalError _ -> 88
        | _ -> 0

type private ThrowingHookApp(log: ResizeArray<string>, throwingHook: string) =
    inherit FIOApp<int, string>()

    override _.effect = FIO.succeed 0

    override _.onOutcome _ =
        log.Add "onOutcome"

        if throwingHook = "onOutcome" then
            failwith "onOutcome threw while building its effect"

        FIO.unit ()

    override _.onShutdown () =
        log.Add "onShutdown"

        if throwingHook = "onShutdown" then
            failwith "onShutdown threw while building its effect"

        FIO.unit ()

type private ThrowingExitCodeApp(log: ResizeArray<string>, effect: unit -> FIO<int, string>) =
    inherit FIOApp<int, string>()

    override _.effect = effect ()
    override _.onOutcome _ = FIO.attempt (fun () -> log.Add "onOutcome") (fun ex -> ex.Message)
    override _.onShutdown () = FIO.attempt (fun () -> log.Add "onShutdown") (fun ex -> ex.Message)
    override _.mapExitCode _ = failwith "mapExitCode threw"

type private StopWhileCreatingRuntimeApp() as this =
    inherit FIOApp<int, string>()

    override _.effect = FIO.never ()

    override _.runtime =
        this.Stop()
        new WorkStealingRuntime() :> FIORuntime

type private FinalizingHooksApp(log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    let finalizer (name: string) : FIO<unit, string> =
        FIO.succeedWith (fun () -> lock log (fun () -> log.Add name))

    let slowly (name: string) : FIO<unit, string> =
        (FIO.sleep (TimeSpan.FromMilliseconds 100.0)).FlatMap(fun () -> finalizer name)

    override _.effect = FIO.succeed 0

    override _.onOutcome _ =
        FIO.succeedWith(fun () -> failwith "onOutcome crashed")
            .Ensuring(slowly "outcomeInner")
            .Ensuring(finalizer "outcomeOuter")

    override _.onShutdown () =
        FIO.interruptNow<unit, string>()
            .Ensuring(slowly "shutdownInner")
            .Ensuring(finalizer "shutdownOuter")

type private DisposedRuntimeApp(log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    let disposed =
        let runtime = new DirectRuntime()
        (runtime :> IDisposable).Dispose()
        runtime :> FIORuntime

    override _.runtime = disposed
    override _.effect = FIO.succeed 0
    override _.onOutcome _ = FIO.attempt (fun () -> log.Add "onOutcome") (fun ex -> ex.Message)
    override _.onShutdown () = FIO.attempt (fun () -> log.Add "onShutdown") (fun ex -> ex.Message)

    override _.mapExitCode outcome =
        match outcome with
        | AppFatalError(:? ObjectDisposedException) -> log.Add "outcome:FatalError(ObjectDisposedException)"
        | other -> log.Add $"outcome:{other}"

        base.mapExitCode outcome

let private runLogged (run: TestApp -> int) (effect: FIO<int, string>) =
    let log = ResizeArray()
    let exitCode = run (TestApp(effect, log))
    exitCode, Seq.toList log

let private testUnix name (body: unit -> unit) =
    testCase name (fun () ->
        if OperatingSystem.IsWindows() then
            skiptest "A POSIX signal cannot be sent to a single process on Windows"

        body ())

let private runSignalled (scenario: string) (signal: string) =
    use child = new ChildProcess([ scenario ])
    Expect.isTrue (child.WaitForLine "ready" (TimeSpan.FromSeconds 60.0)) $"The child app should start; output: {child.Output}"
    child.Signal signal
    let exitCode = child.WaitForExit(TimeSpan.FromSeconds 20.0)
    exitCode, child.Output |> List.filter (fun line -> line <> "ready")

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
                        let effects: FIO<int, string> list = [ FIO.succeed 42; FIO.fail "boom"; FIO.interruptNow () ]

                        let viaRun = effects |> List.map (runLogged (fun app -> app.Run()))
                        let viaRunAsync = effects |> List.map (runLogged (fun app -> app.RunAsync().Result))

                        Expect.equal viaRunAsync viaRun "RunAsync should give the exit code and run the hooks as Run does"
                        Expect.equal (List.map fst viaRunAsync) [ 0; 1; 130 ] "RunAsync should map success, failure and interruption"
                ]

            testList
                "Exit code mapping"
                [
                    testCase "Run - error effect returns exit code 1 by default"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.fail "error", log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "Error should return exit code 1"
                        Expect.contains (Seq.toList log) "outcome:Failed" "mapExitCode should see AppFailed"

                    testCase "Run - interrupted effect returns exit code 130 by default"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.interruptNow (), log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 130 "Interrupted should return exit code 130"
                        Expect.contains (Seq.toList log) "outcome:Interrupted" "mapExitCode should see AppInterrupted"

                    testCase "Run - a defect in the effect is a fatal error with the thrown exception"
                    <| fun () ->
                        let log = ResizeArray()
                        let seen = ref None
                        let app =
                            TestApp(
                                FIO.succeedWith (fun () -> failwith "effect defect"),
                                log,
                                onOutcome = fun outcome ->
                                    FIO.succeedWith (fun () -> seen.Value <- Some outcome))

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "A defect is a crash, not an interruption"
                        Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"
                        match seen.Value with
                        | Some(AppFatalError ex) -> Expect.equal ex.Message "effect defect" "onOutcome should see the thrown exception itself"
                        | other -> failtest $"Expected AppFatalError but got {other}"

                    testCase "Run - a race whose racers all crashed is a fatal error, not an interruption"
                    <| fun () ->
                        let log = ResizeArray()
                        let crashing: FIO<int, string> = FIO.succeedWith (fun () -> failwith "racer defect")
                        let app = TestApp(FIO.raceAll (seq { crashing; crashing }), log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "Racers that crashed are a crash of the race"
                        Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"

                    testCase "Run - an invalid argument in the effect is a fatal error"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp((FIO.sleep (TimeSpan.FromSeconds -1.0)).FlatMap(fun () -> FIO.succeed 1), log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "A rejected argument is a crash, not an interruption"
                        Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"

                    testCase "Run - an effect interrupted with ParentInterrupted exits with 130"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.interrupt (ParentInterrupted(Guid.NewGuid())) "The parent was interrupted.", log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 130 "A parent's interruption is an interruption"
                        Expect.contains (Seq.toList log) "outcome:Interrupted" "mapExitCode should see AppInterrupted"

                    testCase "Run - an effect interrupted with ResourceExhaustion is a fatal error"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.interrupt (ResourceExhaustion "out of handles") "Resources ran out.", log)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "Running out of resources is a crash, not an interruption"
                        Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"

                    testCase "Run - fatal error (runtime construction throws) returns exit code 1 by default"
                    <| fun () ->
                        let app =
                            { new FIOApp<int, string>() with
                                override _.effect = FIO.succeed 42
                                override _.runtime = failwith "fatal" }

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "Fatal error should return exit code 1, as in ZIO"

                    testCase "Run - fatal error path invokes mapExitCode with AppFatalError"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = FatalErrorApp log

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 99 "Custom fatal-error code should win"
                        Expect.contains (Seq.toList log) "outcome:FatalError" "mapExitCode should see AppFatalError"

                    testCase "mapExitCode - default classification is 0/1/130/1"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)
                        let ex = FiberInterruptedException(Guid.NewGuid(), ExplicitInterrupt, "x") :?> FiberInterruptedException

                        Expect.equal (app.mapExitCode (AppSucceeded 42)) 0 "AppSucceeded -> 0"
                        Expect.equal (app.mapExitCode (AppFailed "x")) 1 "AppFailed -> 1"
                        Expect.equal (app.mapExitCode (AppInterrupted ex)) 130 "AppInterrupted -> 130"
                        Expect.equal (app.mapExitCode (AppFatalError (exn "x"))) 1 "AppFatalError -> 1"

                    testCase "mapExitCode - a custom mapping applies to success"
                    <| fun () ->
                        let exitCode = CustomExitCodeApp(FIO.succeed 42).Run()

                        Expect.equal exitCode 10 "Custom success exit code should be 10"

                    testCase "mapExitCode - a custom mapping applies to an error"
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
                        let hook = FIO.attempt (fun () -> log.Add "shutdownHookFailing") (fun (ex: exn) -> ex.Message)
                        let app = TestApp(FIO.succeed 42, log, onShutdown = hook.FlatMap(fun () -> FIO.fail "hook error"))

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Failing shutdown should not change the main outcome's exit code"
                        Expect.sequenceEqual log [ "onOutcomeRan:Succeeded"; "shutdownHookFailing"; "outcome:Succeeded" ] "The failing shutdown hook should have run before the exit code was computed"

                    testCase "onShutdown - a hook that throws while building its effect keeps the exit code"
                    <| fun () ->
                        let log = ResizeArray()

                        let exitCode = silenceErr (fun () -> ThrowingHookApp(log, "onShutdown").Run())

                        Expect.equal exitCode 0 "A throwing shutdown hook must not change the exit code"
                        Expect.sequenceEqual log [ "onOutcome"; "onShutdown" ] "Each hook should run once"

                    testCase "onShutdownTimeout - defaults to 10 seconds"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.equal app.onShutdownTimeout (TimeSpan.FromSeconds 10.0) "Default timeout should be 10 seconds"

                    testCase "onShutdownTimeout - is overridable"
                    <| fun () ->
                        let log = ResizeArray()
                        let app =
                            TestApp(
                                FIO.succeed 42,
                                log,
                                onShutdown = FIO.never (),
                                shutdownTimeout = TimeSpan.FromMilliseconds 100.0
                            )

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 0 "Timed-out shutdown should still produce the main outcome's exit code"

                    testCase "onShutdownTimeout - a timed-out hook still runs all its finalizers before Run returns"
                    <| fun () ->
                        let log = ResizeArray()
                        let outerFinalizerRan = ref false
                        let hook =
                            (FIO.never ())
                                .Ensuring(FIO.sleep (TimeSpan.FromMilliseconds 300.0))
                                .Ensuring(FIO.succeedWith (fun () -> outerFinalizerRan.Value <- true))
                        let app =
                            TestApp(FIO.succeed 42, log, onShutdown = hook, shutdownTimeout = TimeSpan.FromMilliseconds 100.0)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 0 "A timed-out shutdown hook should keep the main outcome's exit code"
                        Expect.isTrue outerFinalizerRan.Value "Every finalizer of the interrupted hook should run before Run returns"

                    testCase "onShutdownTimeout - TimeSpan.MaxValue is rejected before the effect runs"
                    <| fun () ->
                        let log = ResizeArray()
                        let effect = FIO.attempt (fun () -> log.Add "effectRan"; 0) (fun (ex: exn) -> ex.Message)
                        let app = TestApp(effect, log, shutdownTimeout = TimeSpan.MaxValue)

                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> app.Run() |> ignore) "An out-of-range shutdown timeout must be rejected when the app starts"
                        Expect.isEmpty log "Nothing should run when the configuration is rejected"

                    testCase "onShutdownTimeout - Timeout.InfiniteTimeSpan waits for the hook"
                    <| fun () ->
                        let log = ResizeArray()
                        let hook = FIO.sleep (TimeSpan.FromMilliseconds 200.0)
                        let app = TestApp(FIO.succeed 42, log, onShutdown = hook, shutdownTimeout = Timeout.InfiniteTimeSpan)

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "An infinite shutdown timeout should keep the main outcome's exit code"
                        Expect.contains (Seq.toList log) "onShutdownRan" "The hook should run to completion"
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
                        let log = ResizeArray()
                        let app = TestApp(FIO.fail "boom", log)

                        silenceErr (fun () -> app.Run()) |> ignore

                        Expect.contains (Seq.toList log) "onOutcomeRan:Failed" "onOutcome should observe AppFailed"

                    testCase "onOutcome - runs with AppInterrupted on interruption"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.never (), log)

                        silenceErr (fun () ->
                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            app.Stop()
                            runTask.Result |> ignore)

                        Expect.contains (Seq.toList log) "onOutcomeRan:Interrupted" "onOutcome should observe AppInterrupted"

                    testCase "Finalizers - run before onOutcome and onShutdown on success"
                    <| fun () ->
                        let log = ResizeArray()
                        let effect =
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
                        let log = ResizeArray()
                        let finalizer =
                            (FIO.sleep (TimeSpan.FromMilliseconds 100.0))
                                .FlatMap(fun () -> FIO.attempt (fun () -> log.Add "finalizerRan") (fun (ex: exn) -> ex.Message))
                        let effect = (FIO.never ()).Ensuring finalizer
                        let app = TestApp(effect, log)

                        silenceErr (fun () ->
                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            app.Stop()
                            runTask.Result |> ignore)

                        let order = Seq.toList log
                        let finalizerIdx = List.findIndex (fun e -> e = "finalizerRan") order
                        let outcomeIdx = List.findIndex (fun e -> e = "onOutcomeRan:Interrupted") order
                        let shutdownIdx = List.findIndex (fun e -> e = "onShutdownRan") order
                        Expect.isLessThan finalizerIdx outcomeIdx "the interrupted effect's finalizer should run before onOutcome"
                        Expect.isLessThan finalizerIdx shutdownIdx "the interrupted effect's finalizer should run before onShutdown"

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
                        let hook = FIO.attempt (fun () -> log.Add "outcomeHookFailing") (fun (ex: exn) -> ex.Message)
                        let app = TestApp(FIO.succeed 42, log, onOutcome = fun _ -> hook.FlatMap(fun () -> FIO.fail "hook error"))

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "Failing outcome hook should not change the exit code"
                        Expect.sequenceEqual log [ "onOutcomeRan:Succeeded"; "outcomeHookFailing"; "onShutdownRan"; "outcome:Succeeded" ] "The failing outcome hook should have run, then onShutdown, before the exit code was computed"

                    testCase "onOutcome - a hook that throws while building its effect keeps the exit code and still runs onShutdown"
                    <| fun () ->
                        let log = ResizeArray()

                        let exitCode = silenceErr (fun () -> ThrowingHookApp(log, "onOutcome").Run())

                        Expect.equal exitCode 0 "A throwing outcome hook must not change the exit code"
                        Expect.sequenceEqual log [ "onOutcome"; "onShutdown" ] "onShutdown should still run, and each hook once"

                    testCase "onOutcomeTimeout - defaults to 10 seconds"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        Expect.equal app.onOutcomeTimeout (TimeSpan.FromSeconds 10.0) "Default outcome timeout should be 10 seconds"

                    testCase "onOutcomeTimeout - hanging hook is bounded and still produces exit code"
                    <| fun () ->
                        let log = ResizeArray()
                        let app =
                            TestApp(
                                FIO.succeed 42,
                                log,
                                onOutcome = (fun _ -> FIO.never ()),
                                outcomeTimeout = TimeSpan.FromMilliseconds 100.0
                            )

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 0 "Timed-out outcome hook should still produce the main outcome's exit code"

                    testCase "onOutcomeTimeout - a negative value is rejected before the effect runs"
                    <| fun () ->
                        let log = ResizeArray()
                        let effect = FIO.attempt (fun () -> log.Add "effectRan"; 0) (fun (ex: exn) -> ex.Message)
                        let app = TestApp(effect, log, outcomeTimeout = TimeSpan.FromSeconds(-1.0))

                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> app.Run() |> ignore) "A negative outcome timeout must be rejected when the app starts"
                        Expect.isEmpty log "Nothing should run when the configuration is rejected"
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
                        let app = ThrowingDisposeApp(FIO.succeed 42)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 0 "A throwing runtime Dispose should not prevent the normal exit code"

                    testCase "Run - a runtime shut down under a running app ends it as interrupted"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let runtime = new WorkStealingRuntime(testConfig)
                            let app = TestApp(FIO.never (), log, runtime = runtime)

                            let runTask = app.RunAsync()
                            Thread.Sleep 100
                            runtime.Shutdown(TimeSpan.FromSeconds 5.0)
                            let exitCode = runTask.Result

                            Expect.equal exitCode 130 "An app whose runtime was shut down was interrupted"
                            Expect.contains (Seq.toList log) "outcome:Interrupted" "mapExitCode should see AppInterrupted")

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
                        let log = ResizeArray()
                        let app = TestApp(FIO.never (), log)

                        let exitCode =
                            silenceErr (fun () ->
                                let runTask = app.RunAsync()
                                Thread.Sleep 100
                                app.Stop()
                                runTask.Result)

                        Expect.equal exitCode 130 "Stop should cause interrupted exit code"
                        Expect.contains (Seq.toList log) "outcome:Interrupted" "Stop should yield AppInterrupted"

                    testCase "Stop - a cancellation callback that throws does not escape Stop"
                    <| fun () ->
                        silenceErr (fun () ->
                            let log = ResizeArray()
                            let registered = new ManualResetEventSlim(false)
                            let effect =
                                FIO.cancellationToken().FlatMap(fun token ->
                                    token.Register(fun () -> failwith "A cancellation callback threw.") |> ignore
                                    registered.Set()
                                    FIO.never ())
                            let app = TestApp(effect, log)

                            let runTask = app.RunAsync()

                            Expect.isTrue (registered.Wait(TimeSpan.FromSeconds 5.0)) "The effect should register its callback"

                            app.Stop()
                            let exitCode = runTask.Result

                            Expect.equal exitCode 130 "Stop must still interrupt the effect"
                            Expect.contains (Seq.toList log) "outcome:Interrupted" "Stop should yield AppInterrupted")

                    testCase "Stop - a request that races startup still interrupts the effect"
                    <| fun () ->
                        let log = ResizeArray()
                        let app = TestApp(FIO.never (), log)

                        let exitCode =
                            silenceErr (fun () ->
                                let runTask = app.RunAsync()
                                app.Stop()
                                runTask.Result)

                        Expect.equal exitCode 130 "A Stop issued immediately after RunAsync must not be lost"
                        Expect.contains (Seq.toList log) "outcome:Interrupted" "The effect should have been interrupted"

                    testCase "Stop - no-op when not running"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)

                        app.Stop()

                        Expect.isFalse app.IsRunning "Stop on idle app should not throw"

                    testCase "Stop - a request made before Run is not remembered"
                    <| fun () ->
                        let app = MinimalApp(FIO.succeed 1)
                        app.Stop()

                        let exitCode = app.Run()

                        Expect.equal exitCode 0 "A Stop on an app that was not running must not interrupt its later run"

                    testCase "Stop - a request made while the runtime is being created interrupts the effect"
                    <| fun () ->
                        let app = StopWhileCreatingRuntimeApp()

                        let runTask = silenceErr (fun () -> app.RunAsync())
                        let ended = runTask.Wait(TimeSpan.FromSeconds 20.0)

                        Expect.isTrue ended "The effect should be interrupted once it is forked"
                        Expect.equal runTask.Result 130 "The app should end as interrupted"

                    testUnix "Run - a SIGTERM interrupts the effect, runs its finalizers before the hooks and exits with 130" (fun () ->
                        let exitCode, output = runSignalled "app" "TERM"

                        Expect.equal exitCode (Some 130) $"A SIGTERM must end the app as interrupted; output: {output}"
                        Expect.sequenceEqual output [ "finalized"; "outcome:Interrupted"; "shutdown" ] "Finalizers must run before the hooks")

                    testUnix "Run - a Ctrl+C (SIGINT) interrupts the effect and exits with 130" (fun () ->
                        let exitCode, output = runSignalled "app" "INT"

                        Expect.equal exitCode (Some 130) $"A Ctrl+C must end the app as interrupted; output: {output}"
                        Expect.sequenceEqual output [ "finalized"; "outcome:Interrupted"; "shutdown" ] "Finalizers must run before the hooks")

                    testUnix "Run - a second SIGTERM while a finalizer hangs terminates the process" (fun () ->
                        use child = new ChildProcess([ "app-hanging-finalizer" ])

                        Expect.isTrue (child.WaitForLine "ready" (TimeSpan.FromSeconds 60.0)) $"The child app should start; output: {child.Output}"

                        child.Signal "TERM"

                        Expect.isTrue (child.WaitForLine "finalizing" (TimeSpan.FromSeconds 10.0)) "The first SIGTERM should start the shutdown"

                        child.Signal "TERM"

                        Expect.isSome (child.WaitForExit(TimeSpan.FromSeconds 10.0)) "A second SIGTERM must terminate the process"
                        Expect.isFalse (child.Output |> List.contains "shutdown") "The hooks must not run once the process is terminated")

                    testUnix "Run - a second Ctrl+C (SIGINT) while a finalizer hangs terminates the process" (fun () ->
                        use child = new ChildProcess([ "app-hanging-finalizer" ])

                        Expect.isTrue (child.WaitForLine "ready" (TimeSpan.FromSeconds 60.0)) $"The child app should start; output: {child.Output}"

                        child.Signal "INT"

                        Expect.isTrue (child.WaitForLine "finalizing" (TimeSpan.FromSeconds 10.0)) "The first Ctrl+C should start the shutdown"

                        child.Signal "INT"

                        Expect.isSome (child.WaitForExit(TimeSpan.FromSeconds 10.0)) "A second Ctrl+C must terminate the process"
                        Expect.isFalse (child.Output |> List.contains "shutdown") "The hooks must not run once the process is terminated")

                    testUnix "Run - leaves a second Ctrl+C to another handler that cancels it" (fun () ->
                        use child = new ChildProcess([ "app-beside-canceling-handler" ])

                        Expect.isTrue (child.WaitForLine "ready" (TimeSpan.FromSeconds 60.0)) $"The child app should start; output: {child.Output}"

                        child.Signal "INT"

                        Expect.isTrue (child.WaitForLine "finalizing" (TimeSpan.FromSeconds 10.0)) "The first Ctrl+C should start the shutdown"

                        child.Signal "INT"
                        let exited = child.WaitForExit(TimeSpan.FromSeconds 3.0)

                        Expect.isNone exited "The app must not overrule a handler that cancelled the second Ctrl+C")

                    testUnix "Run - leaves a second SIGTERM to another handler that cancels it" (fun () ->
                        use child = new ChildProcess([ "app-beside-canceling-sigterm-handler" ])

                        Expect.isTrue (child.WaitForLine "ready" (TimeSpan.FromSeconds 60.0)) $"The child app should start; output: {child.Output}"

                        child.Signal "TERM"

                        Expect.isTrue (child.WaitForLine "finalizing" (TimeSpan.FromSeconds 10.0)) "The first SIGTERM should start the shutdown"

                        child.Signal "TERM"
                        let exited = child.WaitForExit(TimeSpan.FromSeconds 3.0)

                        Expect.isNone exited "The app must not overrule a handler that cancelled the second SIGTERM")

                    FsCheckProperties.stressTestCase "Run - an app whose runtime is shut down under it always ends, 200 times in a row" (fun () ->
                        let outcomes =
                            silenceErr (fun () ->
                                [
                                    for _ in 1..200 ->
                                        let log = ResizeArray()
                                        let started = new ManualResetEventSlim false
                                        let runtime = new WorkStealingRuntime(testConfig)
                                        let effect: FIO<int, string> = FIO.succeedWith(fun () -> started.Set()).FlatMap(fun () -> FIO.never ())
                                        let app = TestApp(effect, log, runtime = runtime)
                                        let runTask = app.RunAsync()
                                        started.Wait(TimeSpan.FromSeconds 5.0) |> ignore
                                        runtime.Shutdown(TimeSpan.FromSeconds 5.0)
                                        if runTask.Wait(TimeSpan.FromSeconds 10.0) then Some runTask.Result else None
                                ])

                        Expect.allEqual outcomes (Some 130) "Every app should end as interrupted")

                    testUnix "Run - a SIGTERM while the runtime is being created interrupts the effect and exits with 130" (fun () ->
                        use child = new ChildProcess([ "app-slow-runtime" ])

                        Expect.isTrue (child.WaitForLine "creating-runtime" (TimeSpan.FromSeconds 60.0)) $"The child app should start creating its runtime; output: {child.Output}"

                        child.Signal "TERM"
                        let exitCode = child.WaitForExit(TimeSpan.FromSeconds 20.0)

                        Expect.equal exitCode (Some 130) $"A SIGTERM during startup must end the app as interrupted; output: {child.Output}"
                        Expect.contains child.Output "outcome:Interrupted" "The hooks must see the interruption")
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

                    testCase "Run - a throwing mapExitCode exits with 1 and runs each hook once"
                    <| fun () ->
                        let log = ResizeArray<string>()
                        let app = ThrowingExitCodeApp(log, fun () -> FIO.succeed 0)

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "A mapExitCode that throws is a crash"
                        Expect.sequenceEqual log [ "onOutcome"; "onShutdown" ] "Each hook should run once"

                    testCase "Run - a throwing mapExitCode on the fatal path exits with 1 and runs each hook once"
                    <| fun () ->
                        let log = ResizeArray<string>()
                        let app = ThrowingExitCodeApp(log, fun () -> failwith "effect construction exploded")

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 1 "A mapExitCode that throws is a crash"
                        Expect.sequenceEqual log [ "onOutcome"; "onShutdown" ] "Each hook should run once"

                    testCase "Run - a hook that crashes or interrupts itself still runs its own finalizers"
                    <| fun () ->
                        let log = ResizeArray<string>()
                        let app = FinalizingHooksApp log

                        let exitCode = silenceErr (fun () -> app.Run())

                        Expect.equal exitCode 0 "A failing hook should not change the exit code"
                        let order (name: string) = Seq.findIndex ((=) name) log

                        Expect.equal (List.sort (List.ofSeq log)) [ "outcomeInner"; "outcomeOuter"; "shutdownInner"; "shutdownOuter" ] "Every finalizer of each hook should run"
                        Expect.isLessThan (order "outcomeInner") (order "outcomeOuter") "A hook's inner finalizer runs before its outer one"
                        Expect.isLessThan (order "shutdownInner") (order "shutdownOuter") "A hook's inner finalizer runs before its outer one"

                    testCase "Run - a runtime disposed before Run is a fatal error and its hooks are contained"
                    <| fun () ->
                        let log = ResizeArray<string>()

                        let exitCode = silenceErr (fun () -> DisposedRuntimeApp(log).Run())

                        Expect.equal exitCode 1 "A disposed runtime is a fatal error"
                        Expect.contains log "outcome:FatalError(ObjectDisposedException)" "mapExitCode should see the ObjectDisposedException"
                        Expect.isFalse (log.Contains "onOutcome" || log.Contains "onShutdown") "Hooks cannot run on a disposed runtime"
                ]
        ]
