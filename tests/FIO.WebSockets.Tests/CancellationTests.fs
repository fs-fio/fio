module FIO.WebSockets.Tests.CancellationTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.WebSockets

open System
open System.Threading
open System.Diagnostics

open Expecto

[<Tests>]
let cancellationTests =
    testList
        "Cancellation"
        [

            testAllRuntimes "ReceiveMessage - an interruption with no explicit token terminates promptly" (fun runtime ->
                withTestServer
                    (fun ws ->
                        fio {
                            do! sleepMs 5_000.0
                            do! ws.Close()
                        })
                    (fun port ->
                        fio {
                            let! ws = WebSocketClient.connectStringWith $"ws://localhost:{port}/"

                            let! receiveFiber = (ws.ReceiveMessage()).Fork()
                            do! sleepMs 100.0
                            do! receiveFiber.InterruptNow ()

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

            testAllRuntimes "connect - an interruption against an unreachable URL terminates promptly" (fun runtime ->
                let effect =
                    fio {
                        let! connectFiber = (WebSocketClient.connectStringWith "ws://192.0.2.1:9/").Fork()

                        do! sleepMs 100.0
                        do! connectFiber.InterruptNow ()

                        let! terminated, elapsed = waitForTerminal connectFiber 5_000

                        return terminated, elapsed, connectFiber.IsInterrupted()
                    }

                let terminated, elapsed, interrupted = (runtime.Run effect).UnsafeSuccess()

                Expect.isTrue terminated $"Connect fiber should reach terminal state within 5s; took {elapsed}ms"

                Expect.isTrue interrupted "Connect fiber should report Interrupted state after Interrupt")

            testAllRuntimes "withConnection - an interruption against an unreachable URL unwinds promptly" (fun runtime ->
                let effect =
                    fio {
                        let! connectFiber =
                            (WebSocketClient.withConnectionString "ws://192.0.2.1:9/" (fun _ -> FIO.unit ())).Fork()

                        do! sleepMs 100.0
                        do! connectFiber.InterruptNow ()
                    }

                let stopwatch = Stopwatch.StartNew()
                let unwound = (runtime.Run effect).Task().Wait(TimeSpan.FromSeconds 5.0)

                Expect.isTrue unwound $"The scope should unwind within 5s; took {stopwatch.ElapsedMilliseconds}ms")

            testAllRuntimes "serve - interrupted while waiting for a connection, unwinds promptly" (fun runtime ->
                let port = findAvailablePort ()

                let effect =
                    fio {
                        let! server =
                            (WebSocketServer.serve $"http://127.0.0.1:{port}/" WebSocketConfig.defaultConfig (fun _ -> FIO.unit ())).Fork()

                        do! sleepMs 200.0
                        do! server.InterruptNow ()
                    }

                let stopwatch = Stopwatch.StartNew()
                let unwound = (runtime.Run effect).Task().Wait(TimeSpan.FromSeconds 5.0)

                Expect.isTrue unwound $"The scope should unwind within 5s; took {stopwatch.ElapsedMilliseconds}ms")

            testAllRuntimes "serve - interrupted with a connection open, sends its client a going-away close" (fun runtime ->
                let port = findAvailablePort ()

                let handler (ws: WebSocket) =
                    fio {
                        do! ws.SendText "ready"
                        do! noopHandler ws
                    }

                let effect =
                    fio {
                        let! server = (WebSocketServer.serve $"http://127.0.0.1:{port}/" WebSocketConfig.defaultConfig handler).Fork()
                        let! client = connectWhenListening $"ws://127.0.0.1:{port}/"
                        let! _ready = client.ReceiveMessage()
                        let! pending = client.ReceiveMessage().Fork()
                        do! sleepMs 100.0
                        do! server.InterruptNow()
                        let! outcome = pending.Await().Timeout(TimeSpan.FromSeconds 5.0)
                        do! client.CloseIfOpen()
                        do! server.Await().Unit()
                        return outcome
                    }

                match runWithTimeout runtime effect with
                | Some(Succeeded(ConnectionClosed(Some Net.WebSockets.WebSocketCloseStatus.EndpointUnavailable, _))) -> ()
                | other -> failtest $"Expected the client to receive a going-away close, but got {other}")

            testAllRuntimes "serve - interrupts a handler that outlasts the shutdown timeout, once its finalizers have run" (fun runtime ->
                let port = findAvailablePort ()
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 500
                let finalized = ref false

                let handler (ws: WebSocket) =
                    (fio {
                        do! ws.SendText "ready"
                        do! FIO.never ()
                    }).Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true))

                let effect =
                    fio {
                        let! server = (WebSocketServer.serve $"http://127.0.0.1:{port}/" config handler).Fork()
                        let! client = connectWhenListening $"ws://127.0.0.1:{port}/"
                        let! _ready = client.ReceiveMessage()
                        let! closer = client.ReceiveMessage().FlatMap(fun _ -> client.CloseIfOpen()).Fork()
                        do! server.InterruptNow()
                        do! closer.Await().Unit()
                    }

                let stopwatch = Stopwatch.StartNew()
                runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isTrue finalized.Value "The handler's finalizers must run before serve has shut down"
                Expect.isGreaterThanOrEqual stopwatch.ElapsedMilliseconds 450L "The handler gets the shutdown timeout to finish"
                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "A handler that outlasts the timeout must be interrupted")

            testAllRuntimes "serve - survives a handler that throws, closes its connection and still shuts down" (fun runtime ->
                let port = findAvailablePort ()
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 500
                let attempts = ref 0

                let handler (ws: WebSocket) =
                    if Interlocked.Increment attempts = 1 then
                        failwith "handler threw"
                    else
                        ws.SendText "still alive"

                let effect =
                    fio {
                        let! server = (WebSocketServer.serve $"http://127.0.0.1:{port}/" config handler).Fork()
                        let! first = connectWhenListening $"ws://127.0.0.1:{port}/"
                        let! firstOutcome = first.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)
                        do! first.CloseIfOpen()
                        let! second = connectWhenListening $"ws://127.0.0.1:{port}/"
                        let! reply = second.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)
                        do! second.CloseIfOpen()
                        do! server.InterruptNow()
                        return firstOutcome, reply
                    }

                let stopwatch = Stopwatch.StartNew()
                let firstOutcome, reply = runWithTimeout runtime effect
                stopwatch.Stop()

                match firstOutcome with
                | Some(ConnectionClosed _) -> ()
                | other -> failtest $"Expected the connection whose handler threw to be closed, but got {other}"

                Expect.equal reply (Some(Frame(Text "still alive"))) "A handler that throws must not stop the server"
                Expect.isLessThan stopwatch.ElapsedMilliseconds 10_000L "The server must still shut down")

            testSequenced (
                testAllRuntimes "serve - refuses a connection that arrives during its shutdown with 503" (fun runtime ->
                    let port = findAvailablePort ()
                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 5_000

                    let handler (ws: WebSocket) =
                        fio {
                            do! ws.SendText "ready"
                            do! FIO.never ()
                        }

                    let effect =
                        fio {
                            let! server = (WebSocketServer.serve $"http://127.0.0.1:{port}/" config handler).Fork()
                            let! client = connectWhenListening $"ws://127.0.0.1:{port}/"
                            let! _ready = client.ReceiveMessage()
                            let! _closer = client.ReceiveMessage().FlatMap(fun _ -> client.CloseIfOpen()).Fork()
                            do! server.InterruptNow()
                            do! sleepMs 300.0
                            return! (WebSocketClient.connectDefault $"ws://127.0.0.1:{port}/").Result()
                        }

                    match runWithTimeout runtime effect with
                    | Error(ConnectionFailed message) -> Expect.stringContains message "503" "A connection during the shutdown must be refused with 503"
                    | other -> failtest $"Expected ConnectionFailed with a 503 but got {other}"))

            testAllRuntimes "acceptLoop - interrupted, sends a going-away close and then stops the listener" (fun runtime ->
                let handler (ws: WebSocket) =
                    fio {
                        do! ws.SendText "ready"
                        do! noopHandler ws
                    }

                let effect =
                    fio {
                        let! port, listener = startTestListener ()
                        let! loop = (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig handler).Fork()
                        let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                        let! _ready = client.ReceiveMessage()
                        let! pending = client.ReceiveMessage().Fork()
                        do! sleepMs 100.0
                        do! loop.InterruptNow()
                        let! outcome = pending.Await().Timeout(TimeSpan.FromSeconds 5.0)
                        do! client.CloseIfOpen()
                        return outcome, listener
                    }

                let outcome, listener = runWithTimeout runtime effect

                match outcome with
                | Some(Succeeded(ConnectionClosed(Some Net.WebSockets.WebSocketCloseStatus.EndpointUnavailable, _))) -> ()
                | other -> failtest $"Expected a going-away close but got {other}"

                Expect.isFalse listener.IsListening "The loop must stop the listener once it has shut down"
                listener.Close())

            testAllRuntimes "ReceiveMessage - an explicit pre-cancelled token short-circuits the receive" (fun runtime ->
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
                                    .CatchAll(fun error -> FIO.succeed (Error error))

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

            testAllRuntimes "ReceiveMessage - the receive timeout still fires when the fiber is not interrupted" (fun runtime ->
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
                                (ws.ReceiveMessage().Map(fun _ -> Ok())).CatchAll(fun error -> FIO.succeed (Error error))

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
