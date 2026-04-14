module FIO.Sockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative
open FIO.Sockets

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
                    Gen.constant (new CooperativeRuntime() :> FIORuntime)
                    Gen.constant (new ConcurrentRuntime() :> FIORuntime)
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
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private disposeRuntime (rt: FIORuntime) =
    match box rt with
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

let noopHandler (_socket: Socket) : FIO<unit, SocketError> = FIO.unit ()

let echoHandler (socket: Socket) =
    fio {
        let! data, _ = socket.ReceiveBytes 8192
        do! socket.SendBytes data
    }

let private runWithTimeout (runtime: FIORuntime) (eff: FIO<'R, SocketError>) : 'R =
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let withTestServer
    (handler: Socket -> FIO<unit, SocketError>)
    (action: int -> FIO<'R, SocketError>)
    (runtime: FIORuntime)
    =
    let eff =
        fio {
            let! config = ServerSocketConfig.create ("127.0.0.1", 0)
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port

            let! serverFiber =
                (fio {
                    let! socket = ServerSocket.accept server
                    do! handler socket
                    do! socket.Close()
                })
                    .Fork()

            let! result = action port
            do! serverFiber.Interrupt()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime eff

let withTestEchoServer (action: int -> FIO<'R, SocketError>) (runtime: FIORuntime) =
    let eff =
        fio {
            let! config = ServerSocketConfig.create ("127.0.0.1", 0)
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = ServerSocket.acceptLoop(echoHandler, server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, SocketError.fromException)
            let! result = action port
            do! serverFiber.Interrupt()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime eff
