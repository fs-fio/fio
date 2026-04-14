module FIO.Tests.AppTests

open FIO.App
open FIO.DSL
open FIO.Runtime.Concurrent

open System

open Expecto

type private TestApp(eff: FIO<int, string>, log: ResizeArray<string>, ?hook: FIO<unit, string>, ?hookTimeout: TimeSpan)
    =
    inherit
        FIOApp<int, string>(
            settings =
                { FIOAppSettings.Default with
                    ShutdownHookTimeout = defaultArg hookTimeout (TimeSpan.FromSeconds 10.0)
                },
            handlers =
                { FIOAppHandlers.Silent<int, string>() with
                    OnStart = fun () -> log.Add "onStart"
                    OnRuntimeInitialized = fun _ -> log.Add "onRuntimeInitialized"
                    OnFiberRunning = fun () -> log.Add "onFiberRunning"
                    OnSuccess = fun _ -> log.Add "onSuccess"
                    OnError = fun _ -> log.Add "onError"
                    OnInterrupted = fun _ -> log.Add "onInterrupted"
                    OnFatalError = fun _ -> log.Add "onFatalError"
                    OnShutdownRequested = fun () -> log.Add "onShutdownRequested"
                    OnShutdownHookSuccess = fun () -> log.Add "onShutdownHookSuccess"
                    OnShutdownHookError = fun _ -> log.Add "onShutdownHookError"
                    OnShutdownHookTimeout = fun _ -> log.Add "onShutdownHookTimeout"
                    OnShutdownHookException = fun _ -> log.Add "onShutdownHookException"
                    OnShutdownHookInterrupted = fun _ -> log.Add "onShutdownHookInterrupted"
                }
        )

    override _.effect = eff

    override _.shutdownHook() = defaultArg hook (FIO.unit ())

type private MinimalApp(eff: FIO<int, string>) =
    inherit FIOApp<int, string>(handlers = FIOAppHandlers.Silent<int, string>())

    override _.effect = eff

type private CustomNameApp() =
    inherit
        FIOApp<int, string>(
            settings = { FIOAppSettings.Default with Name = Some "MyCustomApp" },
            handlers = FIOAppHandlers.Silent<int, string>()
        )

    override _.effect = FIO.succeed 42

type private CustomExitCodeApp(eff: FIO<int, string>, log: ResizeArray<string>) =
    inherit
        FIOApp<int, string>(
            handlers =
                { FIOAppHandlers.Silent<int, string>() with
                    ExitCodeSuccess = fun _ -> 10
                    ExitCodeError = fun _ -> 20
                    ExitCodeFatalError = fun _ -> 30
                    ExitCodeInterrupted = fun _ -> 40
                    OnStart = fun () -> log.Add "onStart"
                    OnRuntimeInitialized = fun _ -> log.Add "onRuntimeInitialized"
                    OnFiberRunning = fun () -> log.Add "onFiberRunning"
                    OnSuccess = fun _ -> log.Add "onSuccess"
                    OnError = fun _ -> log.Add "onError"
                    OnInterrupted = fun _ -> log.Add "onInterrupted"
                    OnFatalError = fun _ -> log.Add "onFatalError"
                    OnShutdownHookSuccess = fun () -> log.Add "onShutdownHookSuccess"
                    OnShutdownHookError = fun _ -> log.Add "onShutdownHookError"
                    OnShutdownHookTimeout = fun _ -> log.Add "onShutdownHookTimeout"
                    OnShutdownHookException = fun _ -> log.Add "onShutdownHookException"
                    OnShutdownHookInterrupted = fun _ -> log.Add "onShutdownHookInterrupted"
                }
        )

    override _.effect = eff

type private FatalErrorApp(log: ResizeArray<string>) =
    inherit
        FIOApp<int, string>(
            handlers =
                { FIOAppHandlers.Silent<int, string>() with
                    ConfigureThreadPool = fun () -> failwith "fatal"
                    OnStart = fun () -> log.Add "onStart"
                    OnFatalError = fun _ -> log.Add "onFatalError"
                }
        )

    override _.effect = FIO.succeed 42

type private ConfigTrackingApp(eff: FIO<int, string>, configured: bool ref) =
    inherit
        FIOApp<int, string>(
            handlers =
                { FIOAppHandlers.Silent<int, string>() with
                    ConfigureThreadPool = fun () -> configured.Value <- true
                }
        )

    override _.effect = eff

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

                Expect.equal app.Name "MinimalApp" "Default name should be the type name"

            testCase "name - can be overridden"
            <| fun () ->
                let app = CustomNameApp()

                Expect.equal app.Name "MyCustomApp" "Overridden name should be used"

            testCase "version - returns non-empty string"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isNotEmpty app.Version "Version should not be empty"

            testCase "description - defaults to empty string"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal app.Settings.Description "" "Default description should be empty"

            testCase "showBanner - defaults to false"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isFalse app.Settings.ShowBanner "Default showBanner should be false"

            testCase "banner - contains name and version"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)
                let banner = app.Banner

                Expect.stringContains banner app.Name "Banner should contain app name"
                Expect.stringContains banner app.Version "Banner should contain app version"

            testCase "ToString - contains name and id"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)
                let str = app.ToString()

                Expect.stringContains str app.Name "ToString should contain name"
                Expect.stringContains str (string app.Id) "ToString should contain id"

            testCase "runtime - defaults to ConcurrentRuntime"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isTrue (app.runtime :? ConcurrentRuntime) "Default runtime should be ConcurrentRuntime"

            testCase "verbose - defaults to false"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.isFalse app.Settings.Verbose "Default verbose should be false"

            testCase "configureThreadPool - called during Run"
            <| fun () ->
                let configured = ref false
                let app = ConfigTrackingApp(FIO.succeed 42, configured)

                Expect.isFalse configured.Value "Should not be configured before Run"
                app.Run() |> ignore
                Expect.isTrue configured.Value "Should be configured after Run"

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
                        "onShutdownHookSuccess"
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

                Expect.equal (app.Handlers.ExitCodeSuccess 42) 0 "Default exitCodeSuccess should be 0"

            testCase "exitCodeError - defaults to 1"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal (app.Handlers.ExitCodeError "err") 1 "Default exitCodeError should be 1"

            testCase "exitCodeFatalError - defaults to 2"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal (app.Handlers.ExitCodeFatalError(exn "x")) 2 "Default exitCodeFatalError should be 2"

            testCase "exitCodeInterrupted - defaults to 130"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                let ex =
                    FiberInterruptedException(Guid.NewGuid(), ExplicitInterrupt, "test") :?> FiberInterruptedException

                Expect.equal (app.Handlers.ExitCodeInterrupted ex) 130 "Default exitCodeInterrupted should be 130"

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

            testCase "shutdownHook - default calls onShutdownHookSuccess"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log)

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onShutdownHookSuccess" "Default shutdown hook should succeed"

            testCase "shutdownHook - custom success hook runs and calls onShutdownHookSuccess"
            <| fun () ->
                let mutable hookRan = false
                let hook = FIO.attempt ((fun () -> hookRan <- true), fun (ex: exn) -> ex.Message)
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log, hook = hook)

                app.Run() |> ignore

                Expect.isTrue hookRan "Custom shutdown hook should have run"

                Expect.contains
                    (Seq.toList log)
                    "onShutdownHookSuccess"
                    "Successful hook should call onShutdownHookSuccess"

            testCase "shutdownHook - error calls onShutdownHookError"
            <| fun () ->
                let log = ResizeArray()
                let app = TestApp(FIO.succeed 42, log, hook = FIO.fail "hook error")

                app.Run() |> ignore

                Expect.contains (Seq.toList log) "onShutdownHookError" "Failed hook should call onShutdownHookError"

            testCase "shutdownHookTimeout - defaults to 10 seconds"
            <| fun () ->
                let app = MinimalApp(FIO.succeed 1)

                Expect.equal
                    app.Settings.ShutdownHookTimeout
                    (TimeSpan.FromSeconds 10.0)
                    "Default timeout should be 10 seconds"

            testCase "shutdownHook - timeout calls onShutdownHookTimeout"
            <| fun () ->
                let log = ResizeArray()

                let app =
                    TestApp(FIO.succeed 42, log, hook = FIO.never (), hookTimeout = TimeSpan.FromMilliseconds 100.0)

                app.Run() |> ignore

                Expect.contains
                    (Seq.toList log)
                    "onShutdownHookTimeout"
                    "Timed out hook should call onShutdownHookTimeout"
        ]
