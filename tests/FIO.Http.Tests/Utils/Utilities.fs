module FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Default
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System
open System.Net
open System.Text
open System.Threading

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging

open Expecto

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime() :> FIORuntime
        new SignalingRuntime() :> FIORuntime
        new WorkStealingRuntime() :> FIORuntime
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
                    testCase (rt.GetType().Name) <| fun () ->
                        try
                            f rt
                        finally
                            disposeRuntime rt
            ]
    )

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, exn>) =
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 30_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let makeRequest (method: HttpMethod) (path: string) =
    HttpRequest.create method path

let makeGetRequest (path: string) =
    makeRequest HttpMethod.GET path

let dispatchAndRun (runtime: FIORuntime) (routes: Routes<exn>) (request: HttpRequest) =
    let effect = Routes.dispatch request routes
    let fiber = runtime.Run effect
    match
        fiber.Task()
        |> Async.AwaitTask
        |> fun async -> Async.RunSynchronously(async, timeout = 10_000)
    with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let findAvailablePort () =
    let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

let private startTestHttpApp (routes: Routes<exn>) (runtime: DefaultRuntime) =
    let rec attempt remaining =
        let port = findAvailablePort ()
        let config = ServerConfig.create "127.0.0.1" port

        let builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder()
        builder.Logging.ClearProviders() |> ignore

        builder.WebHost.ConfigureKestrel(fun options ->
            options.Listen(System.Net.IPAddress.Parse "127.0.0.1", port))
            |> ignore

        let app = builder.Build()

        RunExtensions.Run(
            app,
            Microsoft.AspNetCore.Http.RequestDelegate(fun ctx ->
                task {
                    try
                        do! KestrelBridge.handleRequest runtime routes config.MaxRequestBodySize ctx
                    with ex ->
                        let message =
                            sprintf "%s\n%s" ex.Message (if isNull ex.StackTrace then "" else ex.StackTrace)
                        ctx.Response.StatusCode <- 500
                        let bytes = Encoding.UTF8.GetBytes message
                        do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                })
        )

        try
            app.StartAsync().Wait()
            port, app
        with _ when remaining > 0 ->
            (try app.DisposeAsync().AsTask().Wait() with _ -> ())
            Thread.Sleep 50
            attempt (remaining - 1)

    attempt 10

let withTestHttpServer (routes: Routes<exn>) (action: int -> unit) =
    let runtime = new DefaultRuntime()
    let port, app = startTestHttpApp routes runtime

    try
        Thread.Sleep 200
        action port
    finally
        app.StopAsync().Wait()
        app.DisposeAsync().AsTask().Wait()
        (runtime :> IDisposable).Dispose()
