module FIO.Tests.AppTests

open FIO.App
open FIO.DSL
open FIO.Runtime.Concurrent

open System
open System.Threading

open Expecto

type private TestApp
    (eff: FIO<int, string>, log: ResizeArray<string>, ?onShutdown: FIO<unit, string>, ?shutdownTimeout: TimeSpan) =
    inherit FIOApp<int, string>()

    override _.effect = eff
    override _.onShutdownTimeout = defaultArg shutdownTimeout (TimeSpan.FromSeconds 10.0)
    override _.onShutdown() = defaultArg onShutdown (FIO.unit ())
    override _.onStart() = log.Add "onStart"
    override _.onRuntimeInitialized _ = log.Add "onRuntimeInitialized"
    override _.onFiberRunning() = log.Add "onFiberRunning"
    override _.onSuccess _ = log.Add "onSuccess"
    override _.onError _ = log.Add "onError"
    override _.onInterrupted _ = log.Add "onInterrupted"
    override _.onFatalError _ = log.Add "onFatalError"
    override _.onShutdownRequested() = log.Add "onShutdownRequested"

    override _.onShutdownComplete result =
        match result with
        | Succeeded _ -> log.Add "onShutdownCompleteSuccess"
        | Failed _ -> log.Add "onShutdownCompleteError"
        | Interrupted _ -> log.Add "onShutdownCompleteInterrupted"

    override _.onShutdownFailed _ = log.Add "onShutdownFailed"

type private MinimalApp(eff: FIO<int, string>) =
    inherit FIOApp<int, string>()

    override _.effect = eff

type private CustomNameApp() =
    inherit FIOApp<int, string>()

    override _.effect = FIO.succeed 42
    override _.name = "MyCustomApp"

type private CustomExitCodeApp(eff: FIO<int, string>, log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    override _.effect = eff
    override _.exitCodeSuccess _ = 10
    override _.exitCodeError _ = 20
    override _.exitCodeFatalError _ = 30
    override _.exitCodeInterrupted _ = 40
    override _.onStart() = log.Add "onStart"
    override _.onRuntimeInitialized _ = log.Add "onRuntimeInitialized"
    override _.onFiberRunning() = log.Add "onFiberRunning"
    override _.onSuccess _ = log.Add "onSuccess"
    override _.onError _ = log.Add "onError"
    override _.onInterrupted _ = log.Add "onInterrupted"
    override _.onFatalError _ = log.Add "onFatalError"

    override _.onShutdownComplete result =
        match result with
        | Succeeded _ -> log.Add "onShutdownCompleteSuccess"
        | Failed _ -> log.Add "onShutdownCompleteError"
        | Interrupted _ -> log.Add "onShutdownCompleteInterrupted"

    override _.onShutdownFailed _ = log.Add "onShutdownFailed"

type private FatalErrorApp(log: ResizeArray<string>) =
    inherit FIOApp<int, string>()

    override _.effect = FIO.succeed 42
    override _.runtime = failwith "fatal"
    override _.onStart() = log.Add "onStart"
    override _.onFatalError _ = log.Add "onFatalError"

[<Tests>]
let appTests =
    testList
        "FIOApp"
        [
            testCase "Id - each instance has unique id"
            <| fun () ->
                let app1 = MinimalApp(FIO.succeed 1)
                let app2 = MinimalApp(FIO.succeed 2)

                Expect.notEqual app1.Id app2.Id "Each app instance should have a unique Id"

            testCase "name - defaults to type name"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal app.name "MinimalApp" "Default name should be the type name"

            testCase "name - can be overridden"
            <| fun () ->
                let app = CustomNameApp()

                Expect.equal app.name "MyCustomApp" "Overridden name should be used"

            testCase "version - returns non-empty string"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isNotEmpty app.version "Version should not be empty"

            testCase "showBanner - defaults to false"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isFalse app.showBanner "Default showBanner should be false"

            testCase "banner - contains name and version"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)
                let banner = app.banner

                Expect.stringContains banner app.name "Banner should contain app name"
                Expect.stringContains banner app.version "Banner should contain app version"

            testCase "ToString - contains name and id"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)
                let str = app.ToString()

                Expect.stringContains str app.name "ToString should contain name"
                Expect.stringContains str (string app.Id) "ToString should contain id"

            testCase "runtime - defaults to ConcurrentRuntime"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isTrue (app.runtime :? ConcurrentRuntime) "Default runtime should be ConcurrentRuntime"

            testCase "verbose - defaults to false"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isFalse app.verbose "Default verbose should be false"

            testCase "Run - success effect returns exit code 0"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)

                let exitCode = app.Run()

                Expect.equal exitCode 0 "Success should return exit code 0"

            testCase "Run - success calls lifecycle handlers in order"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)

                app.Run() |> ignore

                let expected =
                    [
                        "onStart"
                        "onRuntimeInitialized"
                        "onFiberRunning"
                        "onSuccess"
                        "onShutdownCompleteSuccess"
                    ]

                Expect.equal (Seq.toList log) expected "Lifecycle handlers should be called in order"

            testCase "RunAsync - returns same result as Run"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)

                let exitCode = app.RunAsync().Result

                Expect.equal exitCode 0 "RunAsync should return exit code 0 for success"

            testCase "Run - error effect returns exit code 1"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.fail "err", log)

                let exitCode = app.Run()

                Expect.equal exitCode 1 "Error should return exit code 1"

            testCase "Run - error calls onError handler"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.fail "err", log)

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onError" "onError should be called"

            testCase "Run - interrupted effect returns exit code 130"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.interrupt (ExplicitInterrupt, "test"), log)

                let exitCode = app.Run()

                Expect.equal exitCode 130 "Interrupted should return exit code 130"

            testCase "Run - interrupted calls onInterrupted handler"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.interrupt (ExplicitInterrupt, "test"), log)

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onInterrupted" "onInterrupted should be called"

            testCase "Run - fatal error returns exit code 2"
            <| fun () ->
                let log = ResizeArray()
                let app = FatalErrorApp log

                let exitCode = app.Run()

                Expect.equal exitCode 2 "Fatal error should return exit code 2"

            testCase "Run - fatal error calls onFatalError after onStart"
            <| fun () ->
                let log = ResizeArray()
                let app = FatalErrorApp log

                app.Run() |> ignore

                Expect.equal
                    (Seq.toList log)
                    [ "onStart"; "onFatalError" ]
                    "onStart should run before fatal, then onFatalError"

            testCase "exitCodeSuccess - defaults to 0"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal (app.exitCodeSuccess 42) 0 "Default exitCodeSuccess should be 0"

            testCase "exitCodeError - defaults to 1"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal (app.exitCodeError "err") 1 "Default exitCodeError should be 1"

            testCase "exitCodeFatalError - defaults to 2"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal (app.exitCodeFatalError (exn "x")) 2 "Default exitCodeFatalError should be 2"

            testCase "exitCodeInterrupted - defaults to 130"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                let ex =
                    FiberInterruptedException(Guid.NewGuid(), ExplicitInterrupt, "test") :?> FiberInterruptedException

                Expect.equal (app.exitCodeInterrupted ex) 130 "Default exitCodeInterrupted should be 130"

            testCase "Custom exit codes - success uses custom code"
            <| fun () ->
                let log = ResizeArray()
                let exitCode = CustomExitCodeApp(FIO.succeed 42, log).Run()

                Expect.equal exitCode 10 "Custom success exit code should be 10"

            testCase "Custom exit codes - error uses custom code"
            <| fun () ->
                let log = ResizeArray()
                let exitCode = CustomExitCodeApp(FIO.fail "err", log).Run()

                Expect.equal exitCode 20 "Custom error exit code should be 20"

            testCase "onShutdown - default calls onShutdownCompleteSuccess"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onShutdownCompleteSuccess" "Default shutdown should succeed"

            testCase "onShutdown - custom success hook runs and calls onShutdownCompleteSuccess"
            <| fun () ->
                let mutable hookRan = false
                let hook = FIO.attempt ((fun () -> hookRan <- true), fun (ex: exn) -> ex.Message)
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log, onShutdown = hook)

                app.Run() |> ignore

                Expect.isTrue hookRan "Custom shutdown hook should have run"

                Expect.contains
                    (Seq.toList log)
                    "onShutdownCompleteSuccess"
                    "Successful hook should call onShutdownCompleteSuccess"

            testCase "onShutdown - error calls onShutdownCompleteError"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log, onShutdown = FIO.fail "hook error")

                app.Run() |> ignore

                Expect.contains
                    (Seq.toList log)
                    "onShutdownCompleteError"
                    "Failed hook should call onShutdownCompleteError"

            testCase "onShutdown - timeout defaults to 10 seconds"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal app.onShutdownTimeout (TimeSpan.FromSeconds 10.0) "Default timeout should be 10 seconds"

            testCase "onShutdown - timeout calls onShutdownFailed"
            <| fun () ->
                let log = ResizeArray()

                let app =
                    TestApp(
                        FIO.succeed 42,
                        log,
                        onShutdown = FIO.never (),
                        shutdownTimeout = TimeSpan.FromMilliseconds 100.0
                    )

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onShutdownFailed" "Timed out hook should call onShutdownFailed"

            testCase "Run - sequential runs do not leak resources"
            <| fun () ->
                for _ in 1..10 do
                    let log = ResizeArray()
                    let app = TestApp(FIO.succeed 42, log)
                    let exitCode = app.Run()

                    Expect.equal exitCode 0 "Each run should succeed"

            testCase "Runtime - returns ConcurrentRuntime instance"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isTrue (app.Runtime :? ConcurrentRuntime) "Runtime should be ConcurrentRuntime"

            testCase "Runtime - returns same instance on repeated access"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)
                let r1 = app.Runtime
                let r2 = app.Runtime

                Expect.isTrue (obj.ReferenceEquals(r1, r2)) "Runtime should return the same cached instance"

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

                let runTask = app.RunAsync()
                Thread.Sleep 100
                app.Stop()
                let exitCode = runTask.Result

                Expect.equal exitCode 130 "Stop should cause interrupted exit code"
                Expect.contains (Seq.toList log) "onShutdownRequested" "Stop should call onShutdownRequested"
                Expect.contains (Seq.toList log) "onInterrupted" "Stop should call onInterrupted"

            testCase "Stop - no-op when not running"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                app.Stop()

                Expect.isFalse app.IsRunning "Stop on idle app should not throw"

            testCase "Stop - no-op after Run completes"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)
                app.Run() |> ignore

                app.Stop()

                Expect.isFalse
                    (Seq.toList log |> List.contains "onShutdownRequested")
                    "Stop after completion should not call onShutdownRequested"
        ]
