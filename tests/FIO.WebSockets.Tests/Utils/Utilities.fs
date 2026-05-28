/// <summary>Provides test utilities, generators, and server helpers for FIO WebSockets property-based and integration tests.</summary>
module FIO.WebSockets.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

open System
open System.Net

open Expecto
open FsCheck

open FIO.WebSockets

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

    /// <summary>Returns the standard FsCheck configuration for FIO WebSockets tests with 100 max tests and runtime generators registered.</summary>
    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

/// <summary>Represents a JSON-serializable test message used in WebSocket communication tests.</summary>
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

/// <summary>Builds a test list that runs the given test function against all three FIO runtimes, disposing each after use.</summary>
/// <param name="name">The name for the test list.</param>
/// <param name="f">The test function to execute with each runtime.</param>
/// <returns>An Expecto test that runs <paramref name="f"/> against Direct, Cooperative, and Concurrent runtimes.</returns>
let testAllRuntimes name (f: FIORuntime -> unit) =
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

/// <summary>Creates an available TCP port by binding to port 0 and returning the OS-assigned port number.</summary>
/// <returns>An available port number on the loopback interface.</returns>
let findAvailablePort () =
    let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

/// <summary>Creates a WebSocket handler effect that receives messages in a loop until the connection closes without sending any response.</summary>
/// <param name="ws">The WebSocket connection to drain.</param>
/// <returns>An effect that loops receiving frames until a ConnectionClosed message arrives.</returns>
let noopHandler (ws: WebSocket) : FIO<unit, WsError> =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame _ -> return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

/// <summary>Creates a WebSocket handler effect that echoes each received frame back to the sender until the connection closes.</summary>
/// <param name="ws">The WebSocket connection to echo on.</param>
/// <returns>An effect that loops receiving and echoing frames until a ConnectionClosed message arrives.</returns>
let echoHandler (ws: WebSocket) : FIO<unit, WsError> =
    let rec loop () =
        fio {
            match! ws.ReceiveMessage() with
            | Frame frame ->
                do! ws.SendFrame frame
                return! loop ()
            | ConnectionClosed _ -> ()
        }

    loop ()

/// <summary>Transforms an effect into its result value by running it on the given runtime with a 10-second timeout.</summary>
/// <param name="runtime">The runtime to execute the effect on.</param>
/// <param name="eff">The effect to run.</param>
/// <returns>The success value of the effect, or fails the test on error or interruption.</returns>
let private runWithTimeout (runtime: FIORuntime) (eff: FIO<'A, WsError>) : 'A =
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

/// <summary>Builds a test WebSocket server that listens on a random port, accepts one connection handled by the given handler, and runs the client action against that port.</summary>
/// <param name="handler">The effect to run for the accepted WebSocket connection.</param>
/// <param name="action">A function from port number to the client effect to execute against the server.</param>
/// <param name="runtime">The runtime to execute the server and client effects on.</param>
/// <returns>The result of the client action effect.</returns>
let withTestServer (handler: WebSocket -> FIO<unit, WsError>) (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let eff =
        fio {
            let! listener = WebSocketServer.start url

            let! serverFiber =
                (fio {
                    let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
                    do! handler ws
                    do! ws.Close()
                })
                    .Fork()

            let! result = action port
            do! serverFiber.Interrupt ExplicitInterrupt "Interrupted"
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime eff

/// <summary>Builds a test echo WebSocket server that listens on a random port, runs an accept loop with echo handling, and executes the client action against that port.</summary>
/// <param name="action">A function from port number to the client effect to execute against the echo server.</param>
/// <param name="runtime">The runtime to execute the server and client effects on.</param>
/// <returns>The result of the client action effect.</returns>
let withTestEchoServer (action: int -> FIO<'A, WsError>) (runtime: FIORuntime) =
    let port = findAvailablePort ()
    let url = $"http://localhost:{port}/"

    let closingEchoHandler (ws: WebSocket) =
        fio {
            do! echoHandler ws
            do! ws.Close().CatchAll(fun _ -> FIO.unit ())
        }

    let eff =
        fio {
            let! listener = WebSocketServer.start url

            let! serverFiber =
                (WebSocketServer.acceptLoop listener WebSocketConfig.defaultConfig closingEchoHandler).Fork()

            let! result = action port
            do! serverFiber.Interrupt ExplicitInterrupt "Interrupted"
            do! WebSocketServer.close listener
            return result
        }

    runWithTimeout runtime eff
