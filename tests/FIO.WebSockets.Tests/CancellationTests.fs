/// <summary>Provides tests that verify FIO.WebSockets cooperates with fiber cancellation through the running fiber's CancellationToken.</summary>
module FIO.WebSockets.Tests.CancellationTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Diagnostics
open System.Threading

open Expecto

/// <summary>Returns a sleep effect that maps any exception to the supplied WsError, so it can sit inside fio { } blocks.</summary>
let private sleepMs (ms: float) =
    FIO.sleep (TimeSpan.FromMilliseconds ms, WsError.fromException)

/// <summary>Polls the fiber state until it terminates or the timeout elapses, returning whether it terminated within the budget.</summary>
let private waitForTerminal (fiber: Fiber<'R, 'E>) (budgetMs: int) =
    fio {
        let stopwatch = Stopwatch.StartNew()
        let mutable terminal = fiber.IsCompleted() || fiber.IsInterrupted()

        while not terminal && stopwatch.ElapsedMilliseconds < int64 budgetMs do
            do! sleepMs 20.0
            terminal <- fiber.IsCompleted() || fiber.IsInterrupted()

        return terminal, stopwatch.ElapsedMilliseconds
    }

[<Tests>]
let cancellationTests =
    testList
        "WebSockets - Cancellation"
        [

            testAllRuntimes "ReceiveMessage() interruption with no explicit CT terminates promptly" (fun runtime ->
                withTestServer
                    (fun ws ->
                        // Server holds the connection open without sending; client's
                        // ReceiveMessage() (no CT) blocks until cancelled.
                        fio {
                            do! sleepMs 5_000.0
                            do! ws.Close()
                        })
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectStringWith $"ws://localhost:{port}/"

                            let! receiveFiber = (ws.ReceiveMessage()).Fork()
                            do! sleepMs 100.0
                            do! receiveFiber.Interrupt()

                            let! terminated, elapsed = waitForTerminal receiveFiber 2_000

                            Expect.isTrue
                                terminated
                                $"ReceiveMessage fiber should reach terminal state within 2s; took {elapsed}ms"

                            Expect.isTrue
                                (receiveFiber.IsInterrupted())
                                "ReceiveMessage fiber should report Interrupted after Interrupt"

                            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                        })
                    runtime)

            testAllRuntimes "Connect interruption against unreachable URL terminates promptly" (fun runtime ->
                let eff =
                    fio {
                        // 192.0.2.0/24 (TEST-NET-1) is reserved; routable but always discards.
                        let! connectFiber = (WebSocketClient.connectStringWith "ws://192.0.2.1:9/").Fork()

                        do! sleepMs 100.0
                        do! connectFiber.Interrupt()

                        let! terminated, elapsed = waitForTerminal connectFiber 5_000

                        return terminated, elapsed, connectFiber.IsInterrupted()
                    }

                let terminated, elapsed, interrupted = (runtime.Run eff).UnsafeSuccess()

                Expect.isTrue terminated $"Connect fiber should reach terminal state within 5s; took {elapsed}ms"

                Expect.isTrue interrupted "Connect fiber should report Interrupted state after Interrupt")

            testAllRuntimes "Explicit pre-cancelled CT short-circuits ReceiveMessage" (fun runtime ->
                withTestServer
                    (fun ws ->
                        fio {
                            do! sleepMs 5_000.0
                            do! ws.Close()
                        })
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectStringWith $"ws://localhost:{port}/"

                            let cts = new CancellationTokenSource()
                            cts.Cancel()

                            let stopwatch = Stopwatch.StartNew()

                            let! attemptResult =
                                (ws.ReceiveMessage(cts.Token).Map(fun _ -> Ok()))
                                    .CatchAll(fun err -> FIO.succeed (Error err))

                            stopwatch.Stop()

                            match attemptResult with
                            | Ok() -> failtest "Expected explicit pre-cancelled CT to short-circuit ReceiveMessage"
                            | Error _ -> ()

                            Expect.isLessThan
                                stopwatch.ElapsedMilliseconds
                                1_000L
                                "Pre-cancelled CT should fail ReceiveMessage immediately"

                            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                        })
                    runtime)

            testAllRuntimes "Receive timeout still fires when no fiber interruption occurs" (fun runtime ->
                withTestServer
                    (fun ws ->
                        fio {
                            do! sleepMs 5_000.0
                            do! ws.Close()
                        })
                    (fun port ->
                        fio {
                            let config = { WebSocketConfig.defaultConfig with ReceiveTimeout = 200 }

                            let! ws =
                                WebSocketClient.connect (Uri $"ws://localhost:{port}/") config CancellationToken.None

                            let stopwatch = Stopwatch.StartNew()

                            let! attemptResult =
                                (ws.ReceiveMessage().Map(fun _ -> Ok())).CatchAll(fun err -> FIO.succeed (Error err))

                            stopwatch.Stop()

                            match attemptResult with
                            | Ok() -> failtest "Expected timeout to abort ReceiveMessage but it succeeded"
                            | Error _ -> ()

                            Expect.isLessThan
                                stopwatch.ElapsedMilliseconds
                                2_000L
                                "ReceiveTimeout configured at 200ms should fire well under 2s"

                            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                        })
                    runtime)
        ]
