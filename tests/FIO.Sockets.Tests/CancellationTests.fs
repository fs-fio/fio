module FIO.Sockets.Tests.CancellationTests

open FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.IO
open System.Net
open System.Threading
open System.Diagnostics
open System.Threading.Tasks

open Expecto

[<Tests>]
let cancellationTests =
    testList
        "Cancellation"
        [

            testAllRuntimes "connect - an interruption against an unreachable host terminates promptly" (fun runtime ->
                let effect =
                    fio {
                        let! config = SocketConfig.create "192.0.2.1" 1234

                        let! connectFiber = (SocketClient.connect config).Fork()
                        do! sleepMs 100.0
                        do! connectFiber.InterruptNow ()

                        let! terminated, elapsed = waitForTerminal connectFiber 3_000

                        return terminated, elapsed, connectFiber.IsInterrupted()
                    }

                let terminated, elapsed, interrupted = (runtime.Run effect).UnsafeSuccess()

                Expect.isTrue terminated $"Connect fiber should reach terminal state within 3s; took {elapsed}ms"
                Expect.isTrue interrupted "Connect fiber should report Interrupted state after Interrupt"
                Expect.isLessThan
                    elapsed
                    3_000L
                    "Interrupted connect should terminate well under the OS connect timeout")

            testAllRuntimes "serve - interrupted while waiting for a connection, unwinds promptly" (fun runtime ->
                let port = freePort ()
                let effect =
                    fio {
                        let! config = ServerSocketConfig.create "127.0.0.1" port
                        let! server = (ServerSocket.serve config (fun _ -> FIO.unit ())).Fork()
                        do! sleepMs 200.0
                        do! server.InterruptNow ()
                    }
                let stopwatch = Stopwatch.StartNew()

                let unwound = (runtime.Run effect).Task().Wait(TimeSpan.FromSeconds 3.0)

                Expect.isTrue unwound $"The scope should unwind within 3s; took {stopwatch.ElapsedMilliseconds}ms")

            testAllRuntimes "serve - interrupts its handlers with the loop, not after the fiber's other finalizers" (fun runtime ->
                let port = freePort ()
                let handlerStarted = new ManualResetEventSlim false
                let handlerFinalized = ref false
                let finalizedBeforeCleanupEnded = ref false
                let cleanupEnded = ref false
                let handler (_: Socket) =
                    FIO.succeedWith(fun () -> handlerStarted.Set())
                        .FlatMap(fun () -> FIO.never ())
                        .Ensuring(FIO.succeedWith (fun () -> handlerFinalized.Value <- true))
                let cleanup =
                    (sleepMs 300.0).FlatMap(fun () ->
                        FIO.succeedWith (fun () ->
                            finalizedBeforeCleanupEnded.Value <- handlerFinalized.Value
                            cleanupEnded.Value <- true))

                let server =
                    runtime.Run(
                        (fio {
                            let! config = ServerSocketConfig.create "127.0.0.1" port
                            return! ServerSocket.serve config handler
                        }).Ensuring(cleanup))
                use client = new Sockets.TcpClient()
                let deadline = Stopwatch.StartNew()
                let mutable connected = false
                while not connected && deadline.ElapsedMilliseconds < 5_000L do
                    try
                        client.Connect(IPAddress.Loopback, port)
                        connected <- true
                    with _ ->
                        Thread.Sleep 20

                Expect.isTrue connected "The server should start listening"
                Expect.isTrue (handlerStarted.Wait(TimeSpan.FromSeconds 5.0)) "The handler should start"

                runtime.Run(server.InterruptNow()).Task().Wait()
                let waited = Stopwatch.StartNew()
                while not cleanupEnded.Value && waited.ElapsedMilliseconds < 5_000L do
                    Thread.Sleep 10

                Expect.isTrue cleanupEnded.Value "The outer cleanup should run"
                Expect.isTrue finalizedBeforeCleanupEnded.Value "The handler should be interrupted before the cleanup ends")

            testSequenced
            <| testList
                "acceptLoop - hands a connection accepted as it is interrupted to a handler that closes it"
                [
                    let oneWorker = { EvaluationWorkers = 1; EvaluationSteps = 200; BlockingWorkers = 1 }

                    for name, make in
                        [
                            "PollingRuntime", (fun () -> new PollingRuntime(oneWorker) :> FIORuntime)
                            "SignalingRuntime", (fun () -> new SignalingRuntime(oneWorker) :> FIORuntime)
                            "WorkStealingRuntime", (fun () -> new WorkStealingRuntime(oneWorker) :> FIORuntime)
                        ] ->
                        testCase name (fun () ->
                            let runtime = make ()
                            use control = new DirectRuntime()
                            let blocking = new ManualResetEventSlim(false)
                            let unblock = new ManualResetEventSlim(false)
                            let port = freePort ()

                            try
                                let server =
                                    runtime.Run(
                                        fio {
                                            let! config = ServerSocketConfig.create "127.0.0.1" port
                                            return! ServerSocket.serve config (fun _ -> FIO.never ())
                                        })
                                use probe = new Sockets.TcpClient()
                                let deadline = Stopwatch.StartNew()
                                let mutable listening = false
                                while not listening && deadline.ElapsedMilliseconds < 5_000L do
                                    try
                                        probe.Connect(IPAddress.Loopback, port)
                                        listening <- true
                                    with _ ->
                                        Thread.Sleep 20

                                Expect.isTrue listening "The server should start listening"

                                Thread.Sleep 100
                                let blocker =
                                    runtime.Run(FIO.succeedWith (fun () ->
                                        blocking.Set()
                                        unblock.Wait()) : FIO<unit, exn>)

                                Expect.isTrue (blocking.Wait(TimeSpan.FromSeconds 5.0)) "The blocker should hold the only worker"

                                use client = new Sockets.TcpClient()
                                client.Connect(IPAddress.Loopback, port)
                                client.ReceiveTimeout <- 1_000
                                Thread.Sleep 50
                                control.Run(server.InterruptNow()).Task().Wait()
                                unblock.Set()
                                blocker.Task().Wait()
                                let closed =
                                    try
                                        client.GetStream().Read(Array.zeroCreate<byte> 1, 0, 1) = 0
                                    with :? IOException as ex ->
                                        match ex.InnerException with
                                        | :? Sockets.SocketException as socketEx -> socketEx.SocketErrorCode <> Sockets.SocketError.TimedOut
                                        | _ -> true

                                Expect.isTrue closed "The accepted connection should be closed, not left open"
                            finally
                                unblock.Set()

                                match box runtime with
                                | :? IDisposable as disposable -> disposable.Dispose()
                                | _ -> ())
                ]

            testAllRuntimes "withConnection - an interruption against an unreachable host unwinds promptly" (fun runtime ->
                let effect =
                    fio {
                        let! config = SocketConfig.create "192.0.2.1" 1234
                        let! connectFiber = (SocketClient.withConnection config (fun _ -> FIO.unit ())).Fork()
                        do! sleepMs 100.0
                        do! connectFiber.InterruptNow ()
                    }
                let stopwatch = Stopwatch.StartNew()

                let unwound = (runtime.Run effect).Task().Wait(TimeSpan.FromSeconds 3.0)

                Expect.isTrue unwound $"The scope should unwind within 3s; took {stopwatch.ElapsedMilliseconds}ms")

            testAllRuntimes "ReceiveBytes - an interruption mid-block terminates promptly" (fun runtime ->
                let terminated, elapsed, interrupted =
                    withTestServer
                        (fun socket ->
                            fio {
                                do! sleepMs 5_000.0
                                do! socket.Close()
                            })
                        (fun port ->
                            fio {
                                let! config = SocketConfig.create "127.0.0.1" port
                                let! socket = SocketClient.connect config
                                let! receiveFiber = (socket.ReceiveBytes 8192).Fork()
                                do! sleepMs 100.0
                                do! receiveFiber.InterruptNow ()
                                let! terminated, elapsed = waitForTerminal receiveFiber 2_000
                                let interrupted = receiveFiber.IsInterrupted()
                                do! socket.Close()
                                return terminated, elapsed, interrupted
                            })
                        runtime

                Expect.isTrue
                    terminated
                    $"ReceiveBytes fiber should reach terminal state within 2s; took {elapsed}ms"
                Expect.isTrue
                    interrupted
                    "ReceiveBytes fiber should report Interrupted after Interrupt")

            testAllRuntimes "Interruption - a parent's interruption propagates to a child reading on a socket" (fun runtime ->
                let parentTerminated, parentInterrupted, childTerminated, childInterrupted =
                    withTestServer
                        (fun socket ->
                            fio {
                                do! sleepMs 5_000.0
                                do! socket.Close()
                            })
                        (fun port ->
                            fio {
                                let! config = SocketConfig.create "127.0.0.1" port
                                let! socket = SocketClient.connect config
                                let childTcs = TaskCompletionSource<Fiber<byte[] * int, SocketError>>()
                                let! parent =
                                    (fio {
                                        let! childFiber = (socket.ReceiveBytes 8192).Fork()
                                        childTcs.SetResult childFiber
                                        do! FIO.never<unit, SocketError> ()
                                    })
                                        .Fork()
                                do! sleepMs 150.0
                                do! parent.InterruptNow ()
                                let! parentTerminated, _ = waitForTerminal parent 2_000
                                let! child = FIO.awaitTask childTcs.Task SocketError.fromException
                                let! childTerminated, _ = waitForTerminal child 2_000
                                let parentInterrupted = parent.IsInterrupted()
                                let childInterrupted = child.IsInterrupted()
                                do! socket.Close()
                                return parentTerminated, parentInterrupted, childTerminated, childInterrupted
                            })
                        runtime

                Expect.isTrue parentTerminated "Parent fiber should reach terminal state after Interrupt"
                Expect.isTrue parentInterrupted "Parent fiber should be interrupted"
                Expect.isTrue
                    childTerminated
                    "Child fiber reading on the socket should also reach terminal state"
                Expect.isTrue
                    childInterrupted
                    "Child fiber should be interrupted via parent-child propagation")
        ]
