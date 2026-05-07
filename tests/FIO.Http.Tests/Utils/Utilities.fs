/// <summary>Provides test utilities, generators, request builders, and server helpers for FIO HTTP property-based and integration tests.</summary>
module FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative
open FIO.Runtime.Default
open FIO.Http

open System
open System.Net

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging

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

    /// <summary>Returns the standard FsCheck configuration for FIO HTTP tests with 100 max tests and runtime generators registered.</summary>
    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

/// <summary>Represents a JSON-serializable test message used in HTTP request/response tests.</summary>
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

/// <summary>Transforms an effect into its result value by running it on the given runtime with a 10-second timeout.</summary>
/// <param name="runtime">The runtime to execute the effect on.</param>
/// <param name="eff">The effect to run.</param>
/// <returns>The success value of the effect, or fails the test on error or interruption.</returns>
let runWithTimeout (runtime: FIORuntime) (eff: FIO<'R, exn>) : 'R =
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

/// <summary>Creates an HTTP request with the specified method and path.</summary>
/// <param name="method">The HTTP method for the request.</param>
/// <param name="path">The request path.</param>
/// <returns>A new HttpRequest configured with the given method and path.</returns>
let makeRequest (method: HttpMethod) (path: string) : HttpRequest = HttpRequest.create method path

/// <summary>Creates an HTTP GET request for the specified path.</summary>
/// <param name="path">The request path.</param>
/// <returns>A new HttpRequest configured as a GET request.</returns>
let makeGetRequest (path: string) : HttpRequest = makeRequest HttpMethod.GET path

/// <summary>Creates an HTTP POST request for the specified path with a text body.</summary>
/// <param name="path">The request path.</param>
/// <param name="body">The text content to include as the request body.</param>
/// <returns>A new HttpRequest configured as a POST request with the given body.</returns>
let makePostRequest (path: string) (body: string) : HttpRequest =
    HttpRequest.create HttpMethod.POST path
    |> HttpRequest.withBody (RequestBody.Text body)

/// <summary>Transforms a request into its response by dispatching it through the given routes and running the resulting effect with a 10-second timeout.</summary>
/// <param name="runtime">The runtime to execute the dispatched effect on.</param>
/// <param name="routes">The route table to dispatch the request against.</param>
/// <param name="request">The HTTP request to dispatch.</param>
/// <returns>The HttpResponse produced by the matched route handler.</returns>
let dispatchAndRun (runtime: FIORuntime) (routes: Routes<exn>) (request: HttpRequest) : HttpResponse =
    let eff = Routes.dispatch request routes
    let fiber = runtime.Run(eff)

    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun a -> Async.RunSynchronously(a, timeout = 10_000)
    with
    | Succeeded v -> v
    | Failed e -> failtest $"Effect failed: {e}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

/// <summary>Creates an available TCP port by binding to port 0 and returning the OS-assigned port number.</summary>
/// <returns>An available port number on the loopback interface.</returns>
let findAvailablePort () =
    let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

/// <summary>Builds a test HTTP server with the given routes, starts it on a random port, executes the action, and tears down the server afterwards.</summary>
/// <param name="routes">The route table for the test server to handle.</param>
/// <param name="action">A function from port number to execute test assertions against the running server.</param>
let withTestHttpServer (routes: Routes<exn>) (action: int -> unit) =
    let port = findAvailablePort ()
    let config = ServerConfig.create "127.0.0.1" port
    let runtime = new DefaultRuntime()

    let builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder()
    builder.Logging.ClearProviders() |> ignore

    builder.WebHost.ConfigureKestrel(fun options -> options.Listen(System.Net.IPAddress.Parse "127.0.0.1", port))
    |> ignore

    let app = builder.Build()

    Microsoft.AspNetCore.Builder.RunExtensions.Run(
        app,
        Microsoft.AspNetCore.Http.RequestDelegate(fun ctx ->
            task {
                try
                    do! KestrelBridge.handleRequest runtime routes config.MaxRequestBodySize ctx
                with ex ->
                    let msg =
                        sprintf "%s\n%s" ex.Message (if isNull ex.StackTrace then "" else ex.StackTrace)

                    ctx.Response.StatusCode <- 500
                    let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
                    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            }
            :> System.Threading.Tasks.Task)
    )

    let startTask = app.StartAsync()
    startTask.Wait()

    try
        System.Threading.Thread.Sleep(200)
        action port
    finally
        app.StopAsync().Wait()
        app.DisposeAsync().AsTask().Wait()
        (runtime :> IDisposable).Dispose()
