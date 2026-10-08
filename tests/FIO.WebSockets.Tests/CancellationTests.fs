module FIO.WebSockets.Tests.CancellationTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.WebSockets
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Threading
open System.Diagnostics
open System.Threading.Tasks
open System.Collections.Concurrent

open Expecto

// Direct runs fibers on the thread pool; only the worker runtimes have a worker that a test can hold.
let private testSingleWorkerRuntimes name (test: FIORuntime -> unit) =
    let config = { testConfig with EvaluationWorkers = 1 }

    testList
        name
        [
            for runtimeName, make in
                [
                    "PollingRuntime", (fun () -> new PollingRuntime(config) :> FIORuntime)
                    "SignalingRuntime", (fun () -> new SignalingRuntime(config) :> FIORuntime)
                    "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(config) :> FIORuntime)
                ] ->
                testCase runtimeName (fun () ->
                    use runtime = make ()
                    test runtime)
        ]

// Occupies the runtime's only worker until the returned event is set, so a fiber that resumes waits in its queue.
let private holdWorker (runtime: FIORuntime) =
    let held = new ManualResetEventSlim false
    let release = new ManualResetEventSlim false

    let hold : FIO<unit, WsError> =
        FIO.succeedWith (fun () ->
            held.Set()
            release.Wait())

    runtime.Run hold |> ignore

    if not (held.Wait(TimeSpan.FromSeconds 5.0)) then
        failtest "The runtime's only worker should be held"

    release

let private stateOnceAborted (client: WebSocket) =
    fio {
        let! first = client.State()
        let mutable state = first
        let mutable attempts = 0

        while state <> Net.WebSockets.WebSocketState.Aborted && attempts < 100 do
            do! sleepMs 20.0
            let! next = client.State()
            state <- next
            attempts <- attempts + 1

        return state
    }

// The message arrives while the worker is held and the fiber is interrupted before it resumes: read, never handed over.
let private stateAfterReadForInterruptedFiber (runtime: FIORuntime) =
    use peerRuntime = new WorkStealingRuntime(testConfig)
    let send = Tasks.TaskCompletionSource()
    let port, listener = runWithTimeout peerRuntime (startTestListener ())

    try
        peerRuntime.Run(
            fio {
                let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                do! FIO.awaitUnitTask send.Task WsError.fromException
                do! ws.SendText "lost"
                do! FIO.never ()
            })
        |> ignore

        let client = runWithTimeout runtime (connectWhenListening $"ws://localhost:{port}/")
        let reading = runtime.Run(client.ReceiveMessage())
        Thread.Sleep 200
        let release = holdWorker runtime
        send.SetResult()
        Thread.Sleep 300
        runWithTimeout peerRuntime (reading.InterruptNow())
        release.Set()
        runWithTimeout peerRuntime (stateOnceAborted client)
    finally
        runWithTimeout peerRuntime (WebSocketServer.close listener)

// The request arrives while the worker is held, and the accepting fiber is interrupted before it can resume.
let private answeredAfterAcceptInterrupted (runtime: FIORuntime) =
    use otherRuntime = new WorkStealingRuntime(testConfig)
    let port, listener = runWithTimeout runtime (startTestListener ())

    try
        let accepting = runtime.Run(WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig)
        Thread.Sleep 200
        let release = holdWorker runtime
        use client = new Net.WebSockets.ClientWebSocket()
        use timeout = new CancellationTokenSource(TimeSpan.FromSeconds 3.0)
        let connecting = client.ConnectAsync(Uri $"ws://localhost:{port}/", timeout.Token)
        Thread.Sleep 300
        runWithTimeout otherRuntime (accepting.InterruptNow())
        release.Set()

        try
            connecting.Wait(TimeSpan.FromSeconds 5.0) |> ignore
        with _ ->
            ()

        connecting.IsCompleted && not timeout.IsCancellationRequested
    finally
        runWithTimeout otherRuntime (WebSocketServer.close listener)

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
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url WebSocketConfig.defaultConfig (fun _ -> FIO.unit ()))
                        (fun served ->
                            fio {
                                do! sleepMs 200.0
                                do! served.Server.InterruptNow ()
                            })

                let stopwatch = Stopwatch.StartNew()
                let unwound = (runtime.Run effect).Task().Wait(TimeSpan.FromSeconds 5.0)

                Expect.isTrue unwound $"The scope should unwind within 5s; took {stopwatch.ElapsedMilliseconds}ms")

            testAllRuntimes "serve - interrupted with a connection open, sends its client a going-away close" (fun runtime ->
                let handler (ws: WebSocket) =
                    fio {
                        do! ws.SendText "ready"
                        do! noopHandler ws
                    }
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url WebSocketConfig.defaultConfig handler)
                        (fun served ->
                            fio {
                                let! client = connectWhenListening served.Url
                                let! _ready = client.ReceiveMessage()
                                let! pending = client.ReceiveMessage().Fork()
                                do! sleepMs 100.0
                                do! served.Server.InterruptNow()
                                let! outcome = pending.Await().Timeout(TimeSpan.FromSeconds 5.0)
                                do! client.CloseIfOpen()
                                do! served.Server.Await().Unit()
                                return outcome
                            })

                let outcome = runWithTimeout runtime effect

                match outcome with
                | Some(Succeeded(ConnectionClosed(Some Net.WebSockets.WebSocketCloseStatus.EndpointUnavailable, _))) -> ()
                | other -> failtest $"Expected the client to receive a going-away close, but got {other}")

            testAllRuntimes "serve - interrupts a handler that outlasts the shutdown timeout, once its finalizers have run" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 500
                let finalized = ref false
                let handler (ws: WebSocket) =
                    (fio {
                        do! ws.SendText "ready"
                        do! FIO.never ()
                    }).Ensuring(FIO.succeedWith (fun () -> finalized.Value <- true))
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url config handler)
                        (fun served ->
                            fio {
                                let! client = connectWhenListening served.Url
                                let! _ready = client.ReceiveMessage()
                                let! closer = client.ReceiveMessage().FlatMap(fun _ -> client.CloseIfOpen()).Fork()
                                do! served.Server.InterruptNow()
                                do! closer.Await().Unit()
                            })

                let stopwatch = Stopwatch.StartNew()
                runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isTrue finalized.Value "The handler's finalizers must run before serve has shut down"
                Expect.isGreaterThanOrEqual stopwatch.ElapsedMilliseconds 450L "The handler gets the shutdown timeout to finish"
                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "A handler that outlasts the timeout must be interrupted")

            testAllRuntimes "serve - shuts down within its shutdown timeout when SendTimeout is 0 and a peer never reads" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0 |> WebSocketConfig.withShutdownTimeout 500
                let chunk = Array.zeroCreate<byte> (1024 * 1024)
                let handler (ws: WebSocket) =
                    let rec flood () = (ws.SendBinary chunk).FlatMap(fun () -> flood ())
                    fio {
                        do! ws.SendText "ready"
                        do! flood ()
                    }
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url config handler)
                        (fun served ->
                            fio {
                                let! client = connectWhenListening served.Url
                                let! _ready = client.ReceiveMessage()
                                do! sleepMs 500.0
                                do! served.Server.InterruptNow()
                                return client
                            })

                let stopwatch = Stopwatch.StartNew()
                let client = runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "Shutdown must end within its timeout even while a send to the peer is stuck"

                runWithTimeout runtime (client.Abort()))

            testAllRuntimes "serve - shuts down within its shutdown timeout when SendTimeout is 0 and a peer never answers the close" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0 |> WebSocketConfig.withShutdownTimeout 500
                let handler (ws: WebSocket) =
                    fio {
                        do! ws.SendText "ready"
                        do! FIO.never ()
                    }
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url config handler)
                        (fun served ->
                            fio {
                                let! client = connectWhenListening served.Url
                                let! _ready = client.ReceiveMessage()
                                do! served.Server.InterruptNow()
                                return client
                            })

                let stopwatch = Stopwatch.StartNew()
                let client = runWithTimeout runtime effect
                stopwatch.Stop()

                Expect.isLessThan stopwatch.ElapsedMilliseconds 5_000L "Shutdown must end within its timeout even when the peer never answers the close"

                runWithTimeout runtime (client.Abort()))

            testAllRuntimes "serve - survives a handler that throws, closes its connection and still shuts down" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 500
                let attempts = ref 0
                let handler (ws: WebSocket) =
                    if Interlocked.Increment attempts = 1 then
                        failwith "handler threw"
                    else
                        ws.SendText "still alive"
                let effect =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url config handler)
                        (fun served ->
                            fio {
                                let! first = connectWhenListening served.Url
                                let! firstOutcome = first.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)
                                do! first.CloseIfOpen()
                                let! second = connectWhenListening served.Url
                                let! reply = second.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)
                                do! second.CloseIfOpen()
                                do! served.Server.InterruptNow()
                                return firstOutcome, reply
                            })

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
                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 5_000
                    let handler (ws: WebSocket) =
                        fio {
                            do! ws.SendText "ready"
                            do! FIO.never ()
                        }
                    let effect =
                        withServerOn
                            "127.0.0.1"
                            (fun url -> WebSocketServer.serve url config handler)
                            (fun served ->
                                fio {
                                    let! client = connectWhenListening served.Url
                                    let! _ready = client.ReceiveMessage()
                                    let! _closer = client.ReceiveMessage().FlatMap(fun _ -> client.CloseIfOpen()).Fork()
                                    do! served.Server.InterruptNow()
                                    do! sleepMs 300.0
                                    return! (WebSocketClient.connectDefault served.Url).Result()
                                })

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

            testList
                "serve - shuts down within its shutdown timeout while a handler that returned waits for a close the peer never answers"
                [
                    for label, sendTimeout in [ "no send timeout", Some 0; "a 3 s send timeout", Some 3_000; "the default send timeout", None ] ->
                        testAllRuntimes label (fun runtime ->
                            let config =
                                match sendTimeout with
                                | Some timeout -> WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout timeout
                                | None -> WebSocketConfig.defaultConfig
                                |> WebSocketConfig.withShutdownTimeout 500
                            let handler (ws: WebSocket) = ws.SendText "ready"
                            let effect =
                                withServerOn
                                    "127.0.0.1"
                                    (fun url -> WebSocketServer.serve url config handler)
                                    (fun served ->
                                        fio {
                                            let! client = connectWhenListening served.Url
                                            let! _ready = client.ReceiveMessage()
                                            do! sleepMs 100.0
                                            let stopwatch = Stopwatch.StartNew()
                                            do! served.Server.InterruptNow()
                                            let! _ = FIO.awaitTask (Task.WhenAny(served.Unwound, Task.Delay 10_000)) WsError.fromException
                                            return client, stopwatch.ElapsedMilliseconds
                                        })

                            let client, elapsed = runWithTimeout runtime effect

                            Expect.isLessThan elapsed 2_000L "A close waiting in a handler's finalizer must not outlast the shutdown timeout"

                            runWithTimeout runtime (client.Abort()))
                ]

            stressTestAllRuntimes "serve - shuts down within its shutdown timeout with 200 idle handlers whose peers never answer the close" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0 |> WebSocketConfig.withShutdownTimeout 500
                let handler (_: WebSocket) : FIO<unit, WsError> = FIO.never ()
                let round () =
                    withServerOn
                        "127.0.0.1"
                        (fun url -> WebSocketServer.serve url config handler)
                        (fun served ->
                            fio {
                                let! first = connectWhenListening served.Url
                                let! rest = FIO.forEach [ 2..200 ] (fun _ -> connectWhenListening served.Url)
                                do! sleepMs 200.0
                                let stopwatch = Stopwatch.StartNew()
                                do! served.Server.InterruptNow()
                                let! _ = FIO.awaitTask (Task.WhenAny(served.Unwound, Task.Delay 10_000)) WsError.fromException
                                let elapsed = stopwatch.ElapsedMilliseconds
                                do! FIO.forEachDiscard (first :: rest) (fun client -> client.Abort())
                                return served.Unwound.IsCompleted, elapsed
                            })

                let rounds = [ for _ in 1..3 -> runWithTimeout runtime (round ()) ]

                Expect.all rounds (fun (unwound, elapsed) -> unwound && elapsed < 5_000L) "The shutdown should not wait for closes nobody answers")

            testAllRuntimes "ReceiveMessage - interrupted while reading with an explicit token, aborts the connection" (fun runtime ->
                let state, next =
                    withTestEchoServer
                        (fun port ->
                            fio {
                                let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                let! reading = client.ReceiveMessage(CancellationToken.None).Fork()
                                do! sleepMs 200.0
                                let! _ = reading.InterruptAwaitNow()
                                do! sleepMs 100.0
                                let! state = client.State()
                                let! next =
                                    (client.ReceiveMessage().Map(Ok).CatchAll(fun error -> FIO.succeed (Error error)))
                                        .Timeout(TimeSpan.FromSeconds 2.0)
                                return state, next
                            })
                        runtime

                Expect.equal state Net.WebSockets.WebSocketState.Aborted "The read the fiber left behind would take the peer's next message, so the connection must end"
                match next with
                | Some(Error(Closed _)) -> ()
                | other -> failtest $"A receive after the abort must fail with Closed, got %A{other}")

            testAllRuntimes "TryReceive - cancelled through the caller's token, fails with ReceiveFailed instead of reporting a disconnect" (fun runtime ->
                let cancellation = new CancellationTokenSource()
                let outcome =
                    withTestEchoServer
                        (fun port ->
                            fio {
                                let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                let! receiving = client.TryReceive(Codec.text, cancellation.Token).Fork()
                                do! sleepMs 200.0
                                cancellation.Cancel()
                                return! receiving.Await().Timeout(TimeSpan.FromSeconds 5.0)
                            })
                        runtime

                match outcome with
                | Some(Failed(ReceiveFailed _)) -> ()
                | other -> failtest $"A cancellation by the caller is not a closed connection, got %A{other}")

            testAllRuntimes "SendText - after this side closed its output, fails with Closed" (fun runtime ->
                let outcome =
                    withTestEchoServer
                        (fun port ->
                            fio {
                                let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! client.CloseOutput()
                                let! outcome = client.SendText("too late").Map(Ok).CatchAll(fun error -> FIO.succeed (Error error))
                                do! client.Abort()
                                return outcome
                            })
                        runtime

                match outcome with
                | Error(Closed _) -> ()
                | other -> failtest $"A send on a connection this side closed must fail with Closed, got %A{other}")

            stressTestAllRuntimes "SendText - an interrupted send queued behind a stuck one never delivers another connection's bytes" (fun runtime ->
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0
                let megabyte = Array.zeroCreate<byte> (1024 * 1024)
                let connect (url: string) =
                    (WebSocketClient.connect (Uri url) config CancellationToken.None)
                        .Retry 60 (fun (_, _, _) -> sleepMs 50.0)
                let round () =
                    let go = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
                    let seen = ConcurrentQueue<string>()
                    let connected = ref 0
                    let rec drain (record: bool) (ws: WebSocket) : FIO<unit, WsError> =
                        fio {
                            match! ws.ReceiveMessage() with
                            | Frame(Text text) ->
                                if record then seen.Enqueue text
                                return! drain record ws
                            | Frame _ -> return! drain record ws
                            | ConnectionClosed _ -> ()
                        }
                    let handler (ws: WebSocket) =
                        if Interlocked.Increment connected = 1 then
                            (FIO.awaitUnitTask go.Task WsError.fromException).FlatMap(fun () -> drain true ws)
                        else
                            drain false ws

                    runWithTimeout
                        runtime
                        (withServedUrl
                            (fun url -> WebSocketServer.serve url (config |> WebSocketConfig.withShutdownTimeout 500) handler)
                            (fun url ->
                                fio {
                                    let! victim = connect url
                                    do! sleepMs 100.0
                                    let! other = connect url
                                    let rec flood () : FIO<unit, WsError> =
                                        (victim.SendBinary(megabyte, CancellationToken.None)).FlatMap(fun () -> flood ())
                                    let! stuck = (flood ()).Fork()
                                    do! sleepMs 700.0
                                    do! stuck.InterruptNow()
                                    do! sleepMs 150.0
                                    let! abandoned = victim.SendText(String('A', 3000), CancellationToken.None).Fork()
                                    do! sleepMs 200.0
                                    do! abandoned.InterruptNow()
                                    do! sleepMs 150.0
                                    do! FIO.forEachParDiscard [ 1..8 ] (fun _ ->
                                            FIO.forEachDiscard [ 1..200 ] (fun _ -> other.SendText(String('B', 3000))))
                                    go.TrySetResult() |> ignore
                                    do! sleepMs 1500.0
                                    do! victim.Abort()
                                    do! other.Abort()
                                }))

                    seen.ToArray() |> Array.filter (fun text -> text |> Seq.exists (fun c -> c <> 'A'))

                let foreign = [ for _ in 1..8 -> round () ] |> Array.concat

                Expect.isEmpty foreign "A frame the first connection's peer receives must carry what was sent on that connection")

            testAllRuntimes "TryReceive - after this side closed or aborted the connection, yields Disconnected" (fun runtime ->
                let afterClose, afterAbort =
                    withTestEchoServer
                        (fun port ->
                            fio {
                                let! closed = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! closed.Close()
                                let! afterClose = closed.TryReceive Codec.text
                                let! aborted = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! aborted.Abort()
                                let! afterAbort = aborted.TryReceive Codec.text
                                return afterClose, afterAbort
                            })
                        runtime

                match afterClose, afterAbort with
                | Disconnected _, Disconnected _ -> ()
                | other -> failtest $"A connection this side ended should read as Disconnected, got %A{other}")

            testAllRuntimes "SendText - on a disposed connection fails with Closed" (fun runtime ->
                let outcome =
                    withTestEchoServer
                        (fun port ->
                            fio {
                                let! client = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                                do! client.Dispose()
                                return! client.SendText("too late").Map(Ok).CatchAll(fun error -> FIO.succeed (Error error))
                            })
                        runtime

                match outcome with
                | Error(Closed _) -> ()
                | other -> failtest $"A send on a disposed connection must fail with Closed, got %A{other}")

            testAllRuntimes "SendBinary - cancelled through the caller's token while in flight, fails with SendFailed" (fun runtime ->
                let cancellation = new CancellationTokenSource()
                let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withSendTimeout 0
                let megabyte = Array.zeroCreate<byte> (1024 * 1024)
                let outcome =
                    withTestServer
                        (fun _ -> sleepMs 3_000.0)
                        (fun port ->
                            fio {
                                let! client = WebSocketClient.connect (Uri $"ws://localhost:{port}/") config CancellationToken.None
                                let rec flood () : FIO<unit, WsError> =
                                    (client.SendBinary(megabyte, cancellation.Token)).FlatMap(fun () -> flood ())
                                let! flooding = (flood ()).Fork()
                                do! sleepMs 500.0
                                cancellation.Cancel()
                                let! outcome = flooding.Await().Timeout(TimeSpan.FromSeconds 5.0)
                                do! client.Abort()
                                return outcome
                            })
                        runtime

                match outcome with
                | Some(Failed(SendFailed _)) -> ()
                | other -> failtest $"A cancellation by the caller is not a closed connection, got %A{other}")

            testSequenced (
                testAllRuntimes "serve - reports a handler that throws inside its effect on standard error" (fun runtime ->
                    let config = WebSocketConfig.defaultConfig |> WebSocketConfig.withShutdownTimeout 500
                    let handler (_: WebSocket) : FIO<unit, WsError> =
                        FIO.succeedWith (fun () -> failwith "the handler crashed")
                    let original = Console.Error
                    use captured = new IO.StringWriter()
                    Console.SetError captured

                    try
                        let effect =
                            withServerOn
                                "127.0.0.1"
                                (fun url -> WebSocketServer.serve url config handler)
                                (fun served ->
                                    fio {
                                        let! client = connectWhenListening served.Url
                                        let! _ = client.ReceiveMessage().Timeout(TimeSpan.FromSeconds 5.0)
                                        do! sleepMs 200.0
                                        do! client.Abort()
                                        do! served.Server.InterruptNow()
                                    })

                        runWithTimeout runtime effect
                        let logged = captured.ToString()

                        Expect.stringContains logged "the handler crashed" "A handler's crash should be reported"
                    finally
                        Console.SetError original))

            testSingleWorkerRuntimes "ReceiveMessage - a message read for a fiber interrupted before it was handed over aborts the connection" (fun runtime ->
                let state = stateAfterReadForInterruptedFiber runtime

                Expect.equal state Net.WebSockets.WebSocketState.Aborted "The message nobody received is lost, so the connection must not go on")

            testSingleWorkerRuntimes "accept - a request that arrived before the accept was interrupted is still answered" (fun runtime ->
                let answered = answeredAfterAcceptInterrupted runtime

                Expect.isTrue answered "A request the accept had already taken must not be left without a response")
        ]
