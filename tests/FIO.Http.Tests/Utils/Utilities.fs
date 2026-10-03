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
open System.Net.Sockets
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging

open Expecto

let testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime(testConfig) :> FIORuntime
        new SignalingRuntime(testConfig) :> FIORuntime
        new WorkStealingRuntime(testConfig) :> FIORuntime
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

let getWhenListening (url: string) =
    use client = new Http.HttpClient()
    let deadline = DateTime.UtcNow.AddSeconds 10.0
    let mutable result = None

    while result.IsNone && DateTime.UtcNow < deadline do
        try
            result <- Some(client.GetStringAsync(url).Result)
        with _ ->
            Thread.Sleep 25

    match result with
    | Some body -> body
    | None -> failtest $"Server at {url} never became ready"

// Kestrel aborts a connection whose client half-closes, which would race the response, so the request must ask for
// `Connection: close` and the socket stays open until the server ends it.
let sendRawRequest (port: int) (request: string) =
    use tcp = new TcpClient()
    tcp.Connect("127.0.0.1", port)
    use stream = tcp.GetStream()

    let bytes = Encoding.ASCII.GetBytes request
    stream.Write(bytes, 0, bytes.Length)
    stream.Flush()

    stream.ReadTimeout <- 5000
    use reader = new IO.StreamReader(stream, Encoding.ASCII)
    try reader.ReadToEnd() with _ -> ""

let findAvailablePort () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

// Windows retries a refused loopback connect for about two seconds, so a connect that does not finish quickly counts as
// nothing listening.
let isListening (port: int) =
    use tcp = new TcpClient()

    try
        tcp.ConnectAsync("127.0.0.1", port).Wait(TimeSpan.FromMilliseconds 500.0) && tcp.Connected
    with _ ->
        false

let private startTestHttpApp (routes: Routes<exn>) (maxBodySize: int64) (runtime: DefaultRuntime) =
    let rec attempt remaining =
        let port = findAvailablePort ()

        let builder = WebApplication.CreateBuilder()
        builder.Logging.ClearProviders() |> ignore

        builder.WebHost.ConfigureKestrel(fun options ->
            options.Listen(IPAddress.Parse "127.0.0.1", port))
            |> ignore

        let app = builder.Build()

        RunExtensions.Run(
            app,
            Microsoft.AspNetCore.Http.RequestDelegate(fun ctx ->
                task {
                    try
                        do! KestrelBridge.handleRequest runtime routes maxBodySize ctx
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
            try app.DisposeAsync().AsTask().Wait() with _ -> ()
            Thread.Sleep 50
            attempt (remaining - 1)

    attempt 10

let withTestHttpServerMaxBody (maxBodySize: int64) (routes: Routes<exn>) (action: int -> 'T) : 'T =
    let runtime = new DefaultRuntime()
    let port, app = startTestHttpApp routes maxBodySize runtime

    try
        Thread.Sleep 200
        action port
    finally
        app.StopAsync().Wait()
        app.DisposeAsync().AsTask().Wait()
        (runtime :> IDisposable).Dispose()

let withTestHttpServer (routes: Routes<exn>) (action: int -> 'T) : 'T =
    withTestHttpServerMaxBody (ServerConfig.create "127.0.0.1" 0).MaxRequestBodySize routes action
