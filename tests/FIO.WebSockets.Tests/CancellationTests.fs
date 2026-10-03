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
                let terminated, elapsed, interrupted =
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
                                let interrupted = receiveFiber.IsInterrupted()
                                do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                return terminated, elapsed, interrupted
                            })
                        runtime

                Expect.isTrue
                    terminated
                    $"ReceiveMessage fiber should reach terminal state within 2s; took {elapsed}ms"
                Expect.isTrue
                    interrupted
                    "ReceiveMessage fiber should report Interrupted after Interrupt")

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

            testAllRuntimes "accept - an interruption while waiting stops the listener" (fun runtime ->
                let effect =
                    fio {
                        let! _, listener = startTestListener ()
                        let! acceptFiber = (WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig).Fork()
                        do! sleepMs 100.0
                        do! acceptFiber.InterruptNow()
                        let! terminated, _ = waitForTerminal acceptFiber 2_000

                        let stopwatch = Stopwatch.StartNew()
                        let mutable listening = listener.IsListening

                        while listening && stopwatch.ElapsedMilliseconds < 2_000L do
                            do! sleepMs 20.0
                            listening <- listener.IsListening

                        return terminated, listening, listener
                    }

                let terminated, listening, listener = runWithTimeout runtime effect

                Expect.isTrue terminated "The interrupted accept should end promptly"
                Expect.isFalse listening "Interrupting accept must stop the listener"
                listener.Close())

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

                let outcome = runWithTimeout runtime effect

                match outcome with
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

            testAllRuntimes "serve - shuts down within its shutdown timeout when SendTimeout is 0 and a peer never reads" (fun runtime ->
                let port = findAvailablePort ()
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0 |> WebSocketConfig.withShutdownTimeout 500
                let chunk = Array.zeroCreate<byte> (1024 * 1024)
                let handler (ws: WebSocket) =
                    let rec flood () = (ws.SendBinary chunk).FlatMap(fun () -> flood ())
                    fio {
                        do! ws.SendText "ready"
                        do! flood ()
                    }
                let effect =
                    fio {
                        let! server = (WebSocketServer.serve $"http://127.0.0.1:{port}/" config handler).Fork()
                        let! client = connectWhenListening $"ws://127.0.0.1:{port}/"
                        let! _ready = client.ReceiveMessage()
                        do! sleepMs 500.0
                        do! server.InterruptNow()
                        return client
                    }

                let stopwatch = Stopwatch.StartNew()
                let client = runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "Shutdown must end within its timeout even while a send to the peer is stuck"

                runWithTimeout runtime (client.Abort()))

            testAllRuntimes "serve - shuts down within its shutdown timeout when SendTimeout is 0 and a peer never answers the close" (fun runtime ->
                let port = findAvailablePort ()
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0 |> WebSocketConfig.withShutdownTimeout 500
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
                        do! server.InterruptNow()
                        return client
                    }

                let stopwatch = Stopwatch.StartNew()
                let client = runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "Shutdown must end within its timeout even when the peer never answers the close"

                runWithTimeout runtime (client.Abort()))

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

                    let outcome = runWithTimeout runtime effect

                    match outcome with
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
                let attemptResult, elapsedMs =
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
                                do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                return attemptResult, stopwatch.ElapsedMilliseconds
                            })
                        runtime

                match attemptResult with
                | Ok() -> failtest "Expected explicit pre-cancelled CT to short-circuit ReceiveMessage"
                | Error _ -> ()
                Expect.isLessThan
                    elapsedMs
                    1_000L
                    "Pre-cancelled CT should fail ReceiveMessage immediately")

            testAllRuntimes "SendText - an explicit pre-cancelled token fails with SendFailed" (fun runtime ->
                let outcome =
                    withTestServer
                        noopHandler
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectStringWith $"ws://localhost:{port}/"
                                let cts = new CancellationTokenSource()
                                cts.Cancel()
                                let! outcome =
                                    (ws.SendText("cancelled", cts.Token))
                                        .Map(fun _ -> None)
                                        .CatchAll(fun error -> FIO.succeed (Some error))
                                do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                return outcome
                            })
                        runtime

                match outcome with
                | Some(SendFailed _) -> ()
                | other -> failtest $"Expected SendFailed but got {other}")

            testAllRuntimes "ReceiveMessage - a queued receive whose fiber gave up hands the lock back once granted" (fun runtime ->
                let go = Channel<unit>()

                let first, second =
                    withTestServer
                        (fun ws ->
                            fio {
                                do! (go.Read ()).Unit()
                                do! ws.SendText "one"
                                do! (go.Read ()).Unit()
                                do! ws.SendText "two"
                                do! noopHandler ws
                            })
                        (fun port ->
                            fio {
                                let! ws = WebSocketClient.connectStringWith $"ws://localhost:{port}/"
                                let! holder = ws.ReceiveMessage().Fork()
                                do! sleepMs 200.0
                                let! scope =
                                    (fio {
                                        let! queued = ws.ReceiveMessage(CancellationToken.None).Fork()
                                        do! sleepMs 200.0
                                        do! queued.InterruptNow()
                                    }).Fork()
                                do! scope.Await().Unit()
                                do! (go.Write ()).Unit()
                                let! first = holder.Join()
                                do! (go.Write ()).Unit()
                                let! second = ws.ReceiveMessage().Timeout(TimeSpan.FromSeconds 2.0)
                                do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                return first, second
                            })
                        runtime

                Expect.equal first (Frame(Text "one")) "The holder should receive the first message"
                Expect.equal second (Some(Frame(Text "two"))) "The lock must be free once the abandoned wait (on a token that never cancels) is granted")

            testAllRuntimes "ReceiveMessage - the receive timeout still fires when the fiber is not interrupted" (fun runtime ->
                let attemptResult, elapsedMs =
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
                                do! ws.Close().CatchAll(fun _ -> FIO.unit ())
                                return attemptResult, stopwatch.ElapsedMilliseconds
                            })
                        runtime

                match attemptResult with
                | Ok() -> failtest "Expected timeout to abort ReceiveMessage but it succeeded"
                | Error _ -> ()
                Expect.isLessThan
                    elapsedMs
                    2_000L
                    "ReceiveTimeout configured at 200ms should fire well under 2s")
        ]
