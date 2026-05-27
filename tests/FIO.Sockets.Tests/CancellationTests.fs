/// <summary>Provides tests that verify FIO.Sockets cooperates with fiber cancellation via the running fiber's CancellationToken.</summary>
module FIO.Sockets.Tests.CancellationTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets

open System
open System.Diagnostics

open Expecto

/// <summary>Returns a sleep effect that maps any exception to the supplied SocketError, so it can sit inside fio { } blocks.</summary>
let private sleepMs (ms: float) =
    FIO.sleep (TimeSpan.FromMilliseconds ms) SocketError.fromException

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
        "Sockets - Cancellation"
        [

            testAllRuntimes "Connect interruption against unreachable host terminates promptly" (fun runtime ->
                let eff =
                    fio {
                        // 192.0.2.0/24 (TEST-NET-1) is reserved; routable but always discards.
                        let! config = SocketConfig.create ("192.0.2.1", 1)

                        let! connectFiber = (SocketClient.connect config).Fork()
                        do! sleepMs 100.0
                        do! connectFiber.Interrupt()

                        let! terminated, elapsed = waitForTerminal connectFiber 3_000

                        return terminated, elapsed, connectFiber.IsInterrupted()
                    }

                let terminated, elapsed, interrupted = (runtime.Run eff).UnsafeSuccess()

                Expect.isTrue terminated $"Connect fiber should reach terminal state within 3s; took {elapsed}ms"

                Expect.isTrue interrupted "Connect fiber should report Interrupted state after Interrupt"

                Expect.isLessThan
                    elapsed
                    3_000L
                    "Interrupted connect should terminate well under the OS connect timeout")

            testAllRuntimes "ReceiveBytes interruption mid-block terminates promptly" (fun runtime ->
                withTestServer
                    (fun socket ->
                        // Server holds the connection open without sending anything,
                        // so the client's ReceiveBytes blocks until cancelled.
                        fio {
                            do! sleepMs 5_000.0
                            do! socket.Close()
                        })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            let! receiveFiber = (socket.ReceiveBytes 8192).Fork()
                            do! sleepMs 100.0
                            do! receiveFiber.Interrupt()

                            let! terminated, elapsed = waitForTerminal receiveFiber 2_000

                            Expect.isTrue
                                terminated
                                $"ReceiveBytes fiber should reach terminal state within 2s; took {elapsed}ms"

                            Expect.isTrue
                                (receiveFiber.IsInterrupted())
                                "ReceiveBytes fiber should report Interrupted after Interrupt"

                            do! socket.Close()
                        })
                    runtime)

            testAllRuntimes "Parent interruption propagates to child reading on a socket" (fun runtime ->
                withTestServer
                    (fun socket ->
                        fio {
                            do! sleepMs 5_000.0
                            do! socket.Close()
                        })
                    (fun port ->
                        fio {
                            let! config = SocketConfig.create ("127.0.0.1", port)
                            let! socket = SocketClient.connect config

                            // Parent holds the child as state so we can inspect it after parent dies.
                            let childRef = ref Unchecked.defaultof<Fiber<byte[] * int, SocketError>>

                            let! parent =
                                (fio {
                                    let! childFiber = (socket.ReceiveBytes 8192).Fork()
                                    childRef.Value <- childFiber
                                    // Parent holds forever; will be interrupted externally.
                                    do! FIO.never<unit, SocketError> ()
                                })
                                    .Fork()

                            do! sleepMs 150.0
                            do! parent.Interrupt()

                            let! parentTerminated, _ = waitForTerminal parent 2_000
                            let! childTerminated, _ = waitForTerminal childRef.Value 2_000

                            Expect.isTrue parentTerminated "Parent fiber should reach terminal state after Interrupt"
                            Expect.isTrue (parent.IsInterrupted()) "Parent fiber should be interrupted"

                            Expect.isTrue
                                childTerminated
                                "Child fiber reading on the socket should also reach terminal state"

                            Expect.isTrue
                                (childRef.Value.IsInterrupted())
                                "Child fiber should be interrupted via parent-child propagation"

                            do! socket.Close()
                        })
                    runtime)
        ]
