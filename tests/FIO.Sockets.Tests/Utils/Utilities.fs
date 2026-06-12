/// <summary>Provides test utilities, generators, and server helpers for FIO Sockets property-based and integration tests.</summary>
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

/// <summary>Provides FsCheck generators and configuration for property-based testing across all runtimes.</summary>
module FsCheckProperties =

    /// <summary>Provides FsCheck arbitrary instances for generating FIO runtime values in property tests.</summary>
    type Generators =
        /// <summary>Creates an FsCheck arbitrary that produces one of the three FIO runtimes (Direct, Cooperative, Concurrent) with equal probability.</summary>
        /// <returns>An arbitrary that generates FIORuntime instances for property-based testing.</returns>
        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new CooperativeRuntime() :> FIORuntime)
                    Gen.constant (new ConcurrentRuntime() :> FIORuntime)
                ]
            |> Arb.fromGen

    /// <summary>Returns the standard FsCheck configuration for FIO Sockets tests with 100 max tests and runtime generators registered.</summary>
    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

/// <summary>Represents a JSON-serializable test message used in socket communication tests.</summary>
[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

/// <summary>Creates a list containing one instance of each FIO runtime for exhaustive runtime testing.</summary>
/// <returns>A list of DirectRuntime, CooperativeRuntime, and ConcurrentRuntime instances.</returns>
let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

/// <summary>Transforms the runtime into a disposed state if it implements IDisposable.</summary>
/// <param name="rt">The runtime to dispose.</param>
let private disposeRuntime (rt: FIORuntime) =
    match box rt with
    | :? IDisposable as d -> d.Dispose()
    | _ -> ()

/// <summary>Builds a sequenced test list that runs the given test function against all three FIO runtimes, disposing each after use.</summary>
/// <param name="name">The name for the test list.</param>
/// <param name="f">The test function to execute with each runtime.</param>
/// <returns>An Expecto test that runs <paramref name="f"/> against Direct, Cooperative, and Concurrent runtimes sequentially.</returns>
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

let private runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, SocketError>) : 'A =
    let fiber = runtime.Run effect

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
    (action: int -> FIO<'A, SocketError>)
    (runtime: FIORuntime)
    =
    let effect =
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
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, SocketError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create ("127.0.0.1", 0)
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = ServerSocket.acceptLoop(echoHandler, server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) SocketError.fromException
            let! result = action port
            do! serverFiber.InterruptNow ()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect
