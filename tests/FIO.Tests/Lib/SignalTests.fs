module FIO.Tests.SignalTests

open FIO.Tests.Utilities

open FIO.DSL
open FIO.Signal
open FIO.Runtime

open Expecto

open System
open System.Diagnostics
open System.Runtime.InteropServices

let private sendToThisProcess (signal: string) =
    FIO.attempt
        (fun () ->
            use kill = Process.Start("kill", $"-{signal} {Environment.ProcessId}")
            kill.WaitForExit())
        id

let private testUnix name (f: FIORuntime -> unit) =
    testAllRuntimes name (fun runtime ->
        if OperatingSystem.IsWindows() then
            skiptest "Windows supports only SIGINT, SIGQUIT, SIGHUP and SIGTERM"

        f runtime)

// A signal reaches every subscription in the process, so no two of these tests may overlap.
[<Tests>]
let signalTests =
    testSequenced (
        testList
            "Signal"
            [
                testUnix "subscribe - Next yields a signal sent to the process" (fun runtime ->
                    let effect =
                        Signal.subscribe [ PosixSignal.SIGWINCH ] id (fun subscription ->
                            fio {
                                do! sendToThisProcess "WINCH"
                                return! subscription.Next().Timeout(TimeSpan.FromSeconds 5.0)
                            })

                    Expect.equal (runtime.Run(effect).UnsafeSuccess()) (Some PosixSignal.SIGWINCH) "The signal should arrive through Next")

                testUnix "subscribe - Next can be interrupted while waiting" (fun runtime ->
                    let effect =
                        Signal.subscribe [ PosixSignal.SIGWINCH ] id (fun subscription ->
                            subscription.Next().Timeout(TimeSpan.FromMilliseconds 50.0))

                    Expect.isNone (runtime.Run(effect).UnsafeSuccess()) "No signal was sent, so the wait should time out")

                testUnix "subscribe - Next fails through onError once the body has ended" (fun runtime ->
                    let effect =
                        fio {
                            let! subscription = Signal.subscribe [ PosixSignal.SIGWINCH ] (fun ex -> ex.GetType().Name) FIO.succeed
                            return! subscription.Next().Timeout(TimeSpan.FromSeconds 5.0)
                        }

                    match runtime.Run(effect).UnsafeResult() with
                    | Failed name -> Expect.equal name "ChannelClosedException" "The subscription should end with its body"
                    | other -> failtest $"Expected Failed but got {other}")

                testAllRuntimes "subscribe - fails through onError for a signal the platform does not support" (fun runtime ->
                    let effect =
                        Signal.subscribe [ enum<PosixSignal> -1000 ] (fun ex -> ex.GetType().Name) (fun _ -> FIO.unit ())

                    match runtime.Run(effect).UnsafeResult() with
                    | Failed name -> Expect.equal name "PlatformNotSupportedException" "Registration should fail through onError"
                    | other -> failtest $"Expected Failed but got {other}")
            ]
    )
