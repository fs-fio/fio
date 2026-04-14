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

let makeRequest (method: HttpMethod) (path: string) : HttpRequest = HttpRequest.create method path

let makeGetRequest (path: string) : HttpRequest = makeRequest HttpMethod.GET path

let makePostRequest (path: string) (body: string) : HttpRequest =
    HttpRequest.create HttpMethod.POST path
    |> HttpRequest.withBody (RequestBody.Text body)

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

let findAvailablePort () =
    let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

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
