/// <summary>Provides tests for FIOApp run lifecycle, shutdown behavior, and exit-code mapping.</summary>
module FIO.Tests.AppTests

open FIO.App
open FIO.DSL
open FIO.Runtime.Concurrent

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
        ?shutdownTimeout: TimeSpan
    ) =
    inherit FIOApp<int, string>()

    override _.effect = effect
    override _.onShutdownTimeout = defaultArg shutdownTimeout (TimeSpan.FromSeconds 10.0)

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

[<Tests>]
let appTests =
    testSequenced
    <| testList
        "FIOApp"
        [
            // ─── Runtime & Run basics ─────────────────────────────────────────

            testCase "runtime - defaults to ConcurrentRuntime"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isTrue (app.runtime :? ConcurrentRuntime) "Default runtime should be ConcurrentRuntime"

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

            // ─── Exit code mapping ─────────────────────────────────────────

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

            // ─── Shutdown hooks ─────────────────────────────────────────

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

            // ─── Lifecycle / Stop ─────────────────────────────────────────

            testCase "Run - sequential runs do not leak resources"
            <| fun () ->
                for _ in 1..10 do
                    let log = ResizeArray()
                    let app = TestApp(FIO.succeed 42, log)
                    let exitCode = app.Run()

                    Expect.equal exitCode 0 "Each run should succeed"

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

            testCase "Stop - no-op when not running"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                app.Stop()

                Expect.isFalse app.IsRunning "Stop on idle app should not throw"
        ]
