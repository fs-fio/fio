/// <summary>Provides tests for FIOApp configuration defaults and overridable members.</summary>
module FIO.Tests.AppConfigTests

open FIO.AppConfig
open FIO.DSL
open FIO.Runtime.Concurrent

open System
open System.Threading

open Expecto

[<Tests>]
let appConfigTests =
    testList
        "FIOAppConfig"
        [
            testCase "create - defaults Name to FIOApp"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal config.Name "FIOApp" "Default name should be FIOApp"

            testCase "create - defaults Version to 0.0.0"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal config.Version "0.0.0" "Default version should be 0.0.0"

            testCase "create - defaults ShowBanner to false"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.isFalse config.ShowBanner "Default ShowBanner should be false"

            testCase "create - Banner produces box with name and version"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)
                let banner = config.Banner "MyApp" "1.0"

                Expect.stringContains banner "MyApp" "Banner should contain name"
                Expect.stringContains banner "1.0" "Banner should contain version"

            testCase "create - defaults OnShutdownTimeout to 10 seconds"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal config.OnShutdownTimeout (TimeSpan.FromSeconds 10.0) "Default timeout should be 10 seconds"

            testCase "create - defaults ExitCodeSuccess to 0"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal (config.ExitCodeSuccess 42) 0 "Default ExitCodeSuccess should be 0"

            testCase "create - defaults ExitCodeError to 1"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal (config.ExitCodeError "err") 1 "Default ExitCodeError should be 1"

            testCase "create - defaults ExitCodeFatalError to 2"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                Expect.equal (config.ExitCodeFatalError(exn "x")) 2 "Default ExitCodeFatalError should be 2"

            testCase "create - defaults ExitCodeInterrupted to 130"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)

                let ex =
                    FiberInterruptedException(Guid.NewGuid(), ExplicitInterrupt, "test") :?> FiberInterruptedException

                Expect.equal (config.ExitCodeInterrupted ex) 130 "Default ExitCodeInterrupted should be 130"

            testCase "run - success effect returns exit code 0"
            <| fun () ->
                let exitCode = FIOAppConfig.create (FIO.succeed 42) |> FIOApp.run

                Expect.equal exitCode 0 "Success should return exit code 0"

            testCase "run - success calls lifecycle handlers in order"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    OnStart = fun () -> log.Add "onStart"
                    OnRuntimeInitialized = fun _ -> log.Add "onRuntimeInitialized"
                    OnFiberRunning = fun () -> log.Add "onFiberRunning"
                    OnSuccess = fun _ -> log.Add "onSuccess"
                    OnShutdownComplete =
                        fun result ->
                            match result with
                            | Succeeded _ -> log.Add "onShutdownCompleteSuccess"
                            | _ -> ()
                }
                |> FIOApp.run
                |> ignore

                let expected =
                    [
                        "onStart"
                        "onRuntimeInitialized"
                        "onFiberRunning"
                        "onSuccess"
                        "onShutdownCompleteSuccess"
                    ]

                Expect.equal (Seq.toList log) expected "Lifecycle handlers should be called in order"

            testCase "runAsync - returns same result as run"
            <| fun () ->
                let exitCode =
                    FIOAppConfig.create (FIO.succeed 42) |> FIOApp.runAsync |> fun t -> t.Result

                Expect.equal exitCode 0 "runAsync should return exit code 0 for success"

            testCase "run - error effect returns exit code 1"
            <| fun () ->
                let exitCode = FIOAppConfig.create (FIO.fail "err") |> FIOApp.run

                Expect.equal exitCode 1 "Error should return exit code 1"

            testCase "run - error calls OnError handler"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.fail "err") with
                    OnError = fun _ -> log.Add "onError"
                }
                |> FIOApp.run
                |> ignore

                Expect.contains (Seq.toList log) "onError" "OnError should be called"

            testCase "run - interrupted effect returns exit code 130"
            <| fun () ->
                let exitCode =
                    FIOAppConfig.create (FIO.interrupt (ExplicitInterrupt, "test")) |> FIOApp.run

                Expect.equal exitCode 130 "Interrupted should return exit code 130"

            testCase "run - interrupted calls OnInterrupted handler"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.interrupt (ExplicitInterrupt, "test")) with
                    OnInterrupted = fun _ -> log.Add "onInterrupted"
                }
                |> FIOApp.run
                |> ignore

                Expect.contains (Seq.toList log) "onInterrupted" "OnInterrupted should be called"

            testCase "run - fatal error returns exit code 2"
            <| fun () ->
                let exitCode =
                    { FIOAppConfig.create (FIO.succeed 42) with
                        Runtime = fun () -> failwith "fatal"
                    }
                    |> FIOApp.run

                Expect.equal exitCode 2 "Fatal error should return exit code 2"

            testCase "run - fatal error calls OnFatalError after OnStart"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    Runtime = fun () -> failwith "fatal"
                    OnStart = fun () -> log.Add "onStart"
                    OnFatalError = fun _ -> log.Add "onFatalError"
                }
                |> FIOApp.run
                |> ignore

                Expect.equal
                    (Seq.toList log)
                    [ "onStart"; "onFatalError" ]
                    "OnStart should run before fatal, then OnFatalError"

            testCase "Custom exit codes - success uses custom code"
            <| fun () ->
                let exitCode =
                    { FIOAppConfig.create (FIO.succeed 42) with
                        ExitCodeSuccess = fun _ -> 10
                    }
                    |> FIOApp.run

                Expect.equal exitCode 10 "Custom success exit code should be 10"

            testCase "Custom exit codes - error uses custom code"
            <| fun () ->
                let exitCode =
                    { FIOAppConfig.create (FIO.fail "err") with
                        ExitCodeError = fun _ -> 20
                    }
                    |> FIOApp.run

                Expect.equal exitCode 20 "Custom error exit code should be 20"

            testCase "OnShutdown - default calls OnShutdownComplete with success"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    OnShutdownComplete =
                        fun result ->
                            match result with
                            | Succeeded _ -> log.Add "onShutdownCompleteSuccess"
                            | _ -> ()
                }
                |> FIOApp.run
                |> ignore

                Expect.contains (Seq.toList log) "onShutdownCompleteSuccess" "Default shutdown should succeed"

            testCase "OnShutdown - custom hook runs and calls OnShutdownComplete"
            <| fun () ->
                let mutable hookRan = false
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    OnShutdown = fun () -> FIO.attempt ((fun () -> hookRan <- true), fun (ex: exn) -> ex.Message)
                    OnShutdownComplete =
                        fun result ->
                            match result with
                            | Succeeded _ -> log.Add "onShutdownCompleteSuccess"
                            | _ -> ()
                }
                |> FIOApp.run
                |> ignore

                Expect.isTrue hookRan "Custom shutdown hook should have run"

                Expect.contains
                    (Seq.toList log)
                    "onShutdownCompleteSuccess"
                    "Successful hook should call OnShutdownComplete"

            testCase "OnShutdown - error calls OnShutdownComplete with error"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    OnShutdown = fun () -> FIO.fail "hook error"
                    OnShutdownComplete =
                        fun result ->
                            match result with
                            | Failed _ -> log.Add "onShutdownCompleteError"
                            | _ -> ()
                }
                |> FIOApp.run
                |> ignore

                Expect.contains
                    (Seq.toList log)
                    "onShutdownCompleteError"
                    "Failed hook should call OnShutdownComplete with error"

            testCase "OnShutdown - timeout calls OnShutdownFailed"
            <| fun () ->
                let log = ResizeArray()

                { FIOAppConfig.create (FIO.succeed 42) with
                    OnShutdown = fun () -> FIO.never ()
                    OnShutdownTimeout = TimeSpan.FromMilliseconds 100.0
                    OnShutdownFailed = fun _ -> log.Add "onShutdownFailed"
                }
                |> FIOApp.run
                |> ignore

                Expect.contains (Seq.toList log) "onShutdownFailed" "Timed out hook should call OnShutdownFailed"

            testCase "run - sequential runs do not leak resources"
            <| fun () ->
                for _ in 1..10 do
                    let exitCode = FIOAppConfig.create (FIO.succeed 42) |> FIOApp.run

                    Expect.equal exitCode 0 "Each run should succeed"

            testCase "Runtime - defaults to ConcurrentRuntime"
            <| fun () ->
                let config = FIOAppConfig.create (FIO.succeed 1)
                let runtime = config.Runtime()

                Expect.isTrue (runtime :? ConcurrentRuntime) "Default runtime should be ConcurrentRuntime"

                match box runtime with
                | :? IDisposable as d -> d.Dispose()
                | _ -> ()

            testCase "start - Stop interrupts running effect"
            <| fun () ->
                let log = ResizeArray()

                let handle =
                    { FIOAppConfig.create (FIO.never ()) with
                        OnShutdownRequested = fun () -> log.Add "onShutdownRequested"
                        OnInterrupted = fun _ -> log.Add "onInterrupted"
                    }
                    |> FIOApp.start

                Thread.Sleep 100
                handle.Stop()
                let exitCode = handle.ExitCode.Result

                Expect.equal exitCode 130 "Stop should cause interrupted exit code"
                Expect.contains (Seq.toList log) "onShutdownRequested" "Stop should call OnShutdownRequested"
                Expect.contains (Seq.toList log) "onInterrupted" "Stop should call OnInterrupted"

            testCase "start - IsRunning true while running"
            <| fun () ->
                let handle = FIOAppConfig.create (FIO.never ()) |> FIOApp.start

                Thread.Sleep 100
                let wasRunning = handle.IsRunning()
                handle.Stop()
                handle.ExitCode.Wait()
                let isRunningAfter = handle.IsRunning()

                Expect.isTrue wasRunning "IsRunning should be true while running"
                Expect.isFalse isRunningAfter "IsRunning should be false after stop"

            testCase "start - Stop is no-op after completion"
            <| fun () ->
                let handle = FIOAppConfig.create (FIO.succeed 42) |> FIOApp.start

                handle.ExitCode.Wait()
                handle.Stop()

                Expect.isFalse (handle.IsRunning()) "Stop after completion should be safe"

            testCase "Shared state via closure - effect and OnShutdown share state"
            <| fun () ->
                let sharedState = ResizeArray()

                let exitCode =
                    { FIOAppConfig.create (FIO.attempt ((fun () -> sharedState.Add "effect"), id)) with
                        OnShutdown = fun () -> FIO.attempt ((fun () -> sharedState.Add "shutdown"), id)
                    }
                    |> FIOApp.run

                Expect.equal exitCode 0 "Should succeed"
                Expect.contains (Seq.toList sharedState) "effect" "Effect should have run"
                Expect.contains (Seq.toList sharedState) "shutdown" "Shutdown should have run"
        ]
