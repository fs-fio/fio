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

/// <summary>Creates a socket handler effect that accepts and immediately completes without processing data.</summary>
/// <param name="_socket">The socket connection (unused).</param>
/// <returns>An effect that completes with unit immediately.</returns>
let noopHandler (_socket: Socket) : FIO<unit, SocketError> = FIO.unit ()

/// <summary>Creates a socket handler effect that reads up to 8192 bytes and echoes them back to the sender.</summary>
/// <param name="socket">The connected socket to echo on.</param>
/// <returns>An effect that reads bytes and sends them back on the same socket.</returns>
let echoHandler (socket: Socket) =
    fio {
        let! data, _ = socket.ReceiveBytes 8192
        do! socket.SendBytes data
    }

/// <summary>Transforms an effect into its result value by running it on the given runtime with a 10-second timeout.</summary>
/// <param name="runtime">The runtime to execute the effect on.</param>
/// <param name="eff">The effect to run.</param>
/// <returns>The success value of the effect, or fails the test on error or interruption.</returns>
let private runWithTimeout (runtime: FIORuntime) (eff: FIO<'A, SocketError>) : 'A =
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

/// <summary>Builds a test server that binds to a random port, accepts one connection handled by the given handler, and runs the client action against that port.</summary>
/// <param name="handler">The effect to run for the accepted socket connection.</param>
/// <param name="action">A function from port number to the client effect to execute against the server.</param>
/// <param name="runtime">The runtime to execute the server and client effects on.</param>
/// <returns>The result of the client action effect.</returns>
let withTestServer
    (handler: Socket -> FIO<unit, SocketError>)
    (action: int -> FIO<'A, SocketError>)
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
            do! serverFiber.Interrupt ExplicitInterrupt "Interrupted"
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime eff

/// <summary>Builds a test echo server that binds to a random port, runs an accept loop with echo handling, and executes the client action against that port.</summary>
/// <param name="action">A function from port number to the client effect to execute against the echo server.</param>
/// <param name="runtime">The runtime to execute the server and client effects on.</param>
/// <returns>The result of the client action effect.</returns>
let withTestEchoServer (action: int -> FIO<'A, SocketError>) (runtime: FIORuntime) =
    let eff =
        fio {
            let! config = ServerSocketConfig.create ("127.0.0.1", 0)
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = ServerSocket.acceptLoop(echoHandler, server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0) SocketError.fromException
            let! result = action port
            do! serverFiber.Interrupt ExplicitInterrupt "Interrupted"
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime eff
