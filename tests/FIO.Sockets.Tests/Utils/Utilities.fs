module FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open System
open System.Net
open System.Diagnostics

open Expecto
open FsCheck.FSharp

let testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

module FsCheckProperties =

    // One pair for every property: FsCheck asks for the generator once per property and never disposes it.
    let private propertyRuntimes =
        lazy [ new DirectRuntime() :> FIORuntime; new WorkStealingRuntime(testConfig) :> FIORuntime ]

    type Generators =
        static member Runtime() =
            Gen.elements propertyRuntimes.Value |> Arb.fromGen

    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new WorkStealingRuntime(testConfig) :> FIORuntime
    ]

let private disposeRuntime (runtime: FIORuntime) =
    match box runtime with
    | :? IDisposable as d -> d.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
    testSequenced (
        testList
            name
            [
                for rt in runtimes () ->
                    testCase (rt.GetType().Name) (fun () ->
                        try
                            f rt
                        finally
                            disposeRuntime rt)
            ]
    )

let noopHandler (_socket: Socket) =
    FIO.unit ()

let echoHandler (socket: Socket) =
    fio {
        let! data, _ = socket.ReceiveBytes 8192
        do! socket.SendBytes data
    }

// Async.RunSynchronously's timeout waits for the computation to stop, which a hung fiber never does, so it never fires.
let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, SocketError>) =
    let fiber = runtime.Run effect
    let result = fiber.Task()

    if not (result.Wait(TimeSpan.FromSeconds 10.0)) then
        runtime.Run(fiber.InterruptNow()) |> ignore
        failtest $"The effect did not finish within 10 s on {runtime.Name}"

    match result.Result with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let sleepMs (ms: float) =
    FIO.sleep (TimeSpan.FromMilliseconds ms)

let freePort () =
    let probe = new Sockets.TcpListener(IPAddress.Loopback, 0)
    probe.Start()
    let port = (probe.LocalEndpoint :?> IPEndPoint).Port
    probe.Stop()
    port

// The port freePort returns can be taken before the server binds it, so a server whose bind failed is started again on a fresh port.
let serveAndConnect (runtime: FIORuntime) (serveOn: int -> FIO<unit, SocketError>) =
    let rec start attemptsLeft =
        let port = freePort ()
        let server = runtime.Run(serveOn port)
        let deadline = Stopwatch.StartNew()
        let mutable client = new Sockets.TcpClient()

        while not client.Connected && not (server.IsTerminal()) && deadline.ElapsedMilliseconds < 5_000L do
            try
                client.ConnectAsync(IPAddress.Loopback, port).Wait(500) |> ignore
            with _ ->
                ()

            if not client.Connected then
                client.Dispose()
                client <- new Sockets.TcpClient()
                Threading.Thread.Sleep 20

        let lostPort =
            not client.Connected
            && server.IsTerminal()
            && (match server.Task().Result with
                | Failed(BindFailed _) -> true
                | _ -> false)

        if lostPort && attemptsLeft > 1 then
            client.Dispose()
            start (attemptsLeft - 1)
        else
            server, port, client

    start 5

let waitForTerminal (fiber: Fiber<'A, 'E>) (budgetMs: int) =
    fio {
        let stopwatch = Stopwatch.StartNew()
        let mutable terminal = fiber.IsCompleted() || fiber.IsInterrupted()

        while not terminal && stopwatch.ElapsedMilliseconds < int64 budgetMs do
            do! sleepMs 20.0
            terminal <- fiber.IsCompleted() || fiber.IsInterrupted()

        return terminal, stopwatch.ElapsedMilliseconds
    }

let connectWhenListening (host: string) (port: int) =
    (SocketClient.connectWith host port)
        .Retry 60 (fun (_, _, _) -> FIO.sleep (TimeSpan.FromMilliseconds 50.0))

let withTestServer
    (handler: Socket -> FIO<unit, SocketError>)
    (action: int -> FIO<'A, SocketError>)
    (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port

            let! serverFiber =
                (fio {
                    let! socket = ServerSocket.accept server
                    do! handler socket
                    do! socket.Close()
                }).Fork()

            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, SocketError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = (ServerSocket.acceptLoop echoHandler server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect
