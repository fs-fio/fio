module FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Sockets
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Net

open Expecto
open FsCheck

module FsCheckProperties =

    type Generators =
        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new PollingRuntime() :> FIORuntime)
                    Gen.constant (new SignalingRuntime() :> FIORuntime)
                    Gen.constant (new WorkStealingRuntime() :> FIORuntime)
                ]
            |> Arb.fromGen

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
        new PollingRuntime() :> FIORuntime
        new SignalingRuntime() :> FIORuntime
        new WorkStealingRuntime() :> FIORuntime
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

let private runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, SocketError>) =
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 10_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

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
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) SocketError.fromException
            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect
