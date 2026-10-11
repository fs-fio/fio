namespace FIO.Http

open FIO.DSL
open FIO.Runtime.Default

open System
open System.Net
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions

/// Configuration for an HTTP server.
type ServerConfig =
    {
        /// The host or IP address to bind to.
        Host: string
        /// The port to listen on.
        Port: int
        /// The maximum request body size, in bytes.
        MaxRequestBodySize: int64
        /// How long, in milliseconds, a stopping server lets requests in flight finish before it interrupts their
        /// handlers; 0 or less waits indefinitely.
        ShutdownTimeout: int
    }

[<RequireQualifiedAccess>]
module ServerConfig =

    /// The default server configuration (127.0.0.1:8080, 30 MB body limit, 10 s shutdown).
    let defaultConfig : ServerConfig =
        {
            Host = "127.0.0.1"
            Port = 8080
            MaxRequestBodySize = 30L * 1024L * 1024L
            ShutdownTimeout = 10_000
        }

    /// Creates a server configuration for the given host and port.
    let create host port : ServerConfig =
        { defaultConfig with Host = host; Port = port }

    /// Sets the maximum request body size on a configuration.
    let withMaxBodySize maxSize config : ServerConfig =
        { config with MaxRequestBodySize = maxSize }

    /// Sets how long, in milliseconds, a stopping server lets requests in flight finish; 0 or less waits indefinitely.
    let withShutdownTimeout (timeout: int) (config: ServerConfig) : ServerConfig =
        { config with ShutdownTimeout = timeout }

type private ServerState =
    | Created
    | Started
    | Stopped

// The host's default lifetime cancels every Ctrl+C and SIGTERM and stops the host itself; run handles signals instead.
type private QuietLifetime() =
    interface IHostLifetime with
        member _.WaitForStartAsync _ = Task.CompletedTask
        member _.StopAsync _ = Task.CompletedTask

/// A configured HTTP server instance.
type FIOServer =
    private
        {
            Config: ServerConfig
            Routes: Routes<exn>
            Runtime: DefaultRuntime
            Host: IHost option ref
            OwnsRuntime: bool
            State: ServerState ref
            // Serialises start and stop, so neither sees the other half done.
            Gate: SemaphoreSlim
            Tracker: RequestTracker
        }

[<RequireQualifiedAccess>]
module Server =

    let private newServer (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) (ownsRuntime: bool) =
        {
            Config = config
            Routes = routes
            Runtime = runtime
            Host = ref None
            OwnsRuntime = ownsRuntime
            State = ref Created
            Gate = new SemaphoreSlim(1, 1)
            Tracker = RequestTracker runtime
        }

    /// Creates a server using the given runtime.
    let createWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<FIOServer, exn> =
        FIO.attempt (fun () -> newServer config routes runtime false) id

    /// Creates a server with its own default runtime.
    let create (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        FIO.attempt (fun () -> newServer config routes (new DefaultRuntime()) true) id

    let internal runtimeOf (server: FIOServer) : DefaultRuntime =
        server.Runtime

    // Disposing a runtime waits for its fibers to unwind, so it runs on a pool thread and is awaited, not blocked on.
    let private releaseOwnedRuntime (server: FIOServer) : FIO<unit, exn> =
        if server.OwnsRuntime then
            (FIO.awaitUnitTask (Task.Run(fun () -> (server.Runtime :> IDisposable).Dispose())) id)
                .CatchAll(fun _ -> FIO.unit ())
        else
            FIO.unit ()

    // Both run uninterruptibly: a start or a stop left half done would leave a host that nothing refers to.
    let private locked (server: FIOServer) (body: FIO<'A, exn>) : FIO<'A, exn> =
        FIO.acquireReleaseWith
            (FIO.suspend (fun () -> FIO.awaitUnitTask (server.Gate.WaitAsync()) id))
            (fun () -> FIO.succeedWith (fun () -> server.Gate.Release() |> ignore))
            (fun () -> FIO.uninterruptible body)

    let private buildHost (server: FIOServer) =
        let builder = WebApplication.CreateBuilder()
        // The server listens where its configuration says and stops when it is told: files and environment variables
        // found at run time get no say.
        builder.Configuration.Sources.Clear()
        builder.Logging.ClearProviders() |> ignore
        builder.Services.RemoveAll<IHostLifetime>() |> ignore
        builder.Services.AddSingleton<IHostLifetime>(QuietLifetime()) |> ignore

        builder.Services.Configure<HostOptions>(fun (options: HostOptions) ->
            options.ShutdownTimeout <-
                if server.Config.ShutdownTimeout > 0 then TimeSpan.FromSeconds 5.0
                else Timeout.InfiniteTimeSpan)
        |> ignore

        builder.WebHost.ConfigureKestrel(fun options ->
            options.Limits.MaxRequestBodySize <- Nullable server.Config.MaxRequestBodySize

            match IPAddress.TryParse server.Config.Host with
            | true, ip -> options.Listen(ip, server.Config.Port)
            | _ ->
                if server.Config.Host.Equals("localhost", StringComparison.OrdinalIgnoreCase) then
                    options.ListenLocalhost server.Config.Port
                else
                    for address in Dns.GetHostAddresses server.Config.Host do
                        options.Listen(address, server.Config.Port))
        |> ignore

        let app = builder.Build()

        app.Run(
            Microsoft.AspNetCore.Http.RequestDelegate(fun ctx ->
                KestrelBridge.handleTracked
                    server.Tracker
                    server.Runtime
                    server.Routes
                    server.Config.MaxRequestBodySize
                    KestrelBridge.defaultJsonOptions
                    ctx
                :> Task))

        app

    /// Starts the server listening, returning the running server. A server that was stopped cannot be started again.
    let start (server: FIOServer) : FIO<FIOServer, exn> =
        locked server (
            FIO.suspend (fun () ->
                match server.State.Value with
                | Started -> FIO.succeed server
                | Stopped ->
                    let message = "FIO HTTP Server was stopped and cannot be started again; create a new one."
                    FIO.fail (InvalidOperationException message :> exn)
                | Created ->
                    fio {
                        let! app = FIO.attempt (fun () -> buildHost server) id

                        do!
                            (FIO.awaitUnitTask (app.StartAsync()) id).CatchAll <| fun startError ->
                                (FIO.awaitUnitTask (app.DisposeAsync().AsTask()) id)
                                    .Ignore().FlatMap(fun () -> FIO.fail startError)

                        do!
                            FIO.attempt (fun () ->
                                server.Host.Value <- Some(app :> IHost)
                                server.State.Value <- Started
                                printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}")
                                id

                        return server
                    }))

    /// Stops the server: new requests get 503, requests in flight get the shutdown timeout, then are interrupted with 503.
    /// Never call it from one of the server's own handlers, which it would wait for and then interrupt.
    let stop (server: FIOServer) : FIO<FIOServer, exn> =
        locked server (
            FIO.suspend (fun () ->
                match server.State.Value, server.Host.Value with
                | Started, Some host ->
                    let drain =
                        FIO.suspend (fun () ->
                            let drained = server.Tracker.BeginStop()

                            let waited =
                                if server.Config.ShutdownTimeout > 0 then
                                    Task.WhenAny(drained, Task.Delay server.Config.ShutdownTimeout) :> Task
                                else
                                    drained

                            (FIO.awaitUnitTask waited id).FlatMap(fun () -> FIO.attempt (fun () -> server.Tracker.Cut()) id))

                    let release =
                        (FIO.attempt (fun () ->
                            server.Host.Value <- None
                            server.State.Value <- Stopped
                            host.Dispose()) id)
                            .CatchAll(fun _ -> FIO.unit ())
                            .FlatMap(fun () -> releaseOwnedRuntime server)

                    (drain.CatchAll(fun _ -> FIO.unit ()))
                        .FlatMap(fun () -> FIO.awaitUnitTask (host.StopAsync()) id)
                        .Ensuring(release)
                        .FlatMap(fun () -> FIO.attempt (fun () -> printfn "FIO HTTP Server stopped") id)
                        .Map(fun () -> server)
                | Stopped, _ -> FIO.succeed server
                | _ ->
                    (FIO.attempt (fun () -> server.State.Value <- Stopped) id)
                        .FlatMap(fun () -> releaseOwnedRuntime server)
                        .FlatMap(fun () -> FIO.attempt (fun () -> printfn "FIO HTTP Server not running") id)
                        .Map(fun () -> server)))

    /// Waits until the running server is asked to stop, by stop or by the first Ctrl+C, SIGTERM or SIGQUIT, then stops it
    /// as stop does; a further signal is left to its default, which ends the process.
    let run (server: FIOServer) : FIO<unit, exn> =
        fio {
            match server.Host.Value with
            | Some host ->
                let lifetime = host.Services.GetRequiredService<IHostApplicationLifetime>()
                let stopping = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
                let claimed = ref 0

                // Only the first signal is cancelled, so a second one can still end a stop that hangs.
                let onSignal (context: PosixSignalContext) =
                    if Interlocked.Exchange(&claimed.contents, 1) = 0 then
                        context.Cancel <- true
                        stopping.TrySetResult() |> ignore

                let signals =
                    [ PosixSignal.SIGINT; PosixSignal.SIGTERM; PosixSignal.SIGQUIT ]
                    |> List.choose (fun signal ->
                        try Some(PosixSignalRegistration.Create(signal, onSignal))
                        with _ -> None)

                let registration = lifetime.ApplicationStopping.Register(fun () -> stopping.TrySetResult() |> ignore)

                let release =
                    FIO.succeedWith (fun () ->
                        registration.Dispose()

                        for signal in signals do
                            signal.Dispose())

                do! (FIO.awaitUnitTask stopping.Task id).Ensuring release
                do! (stop server).Unit()
            | None ->
                return! FIO.fail (exn "FIO HTTP Server not started. Call Server.start first.")
        }

    // The caller never sees a server whose start failed, so its runtime is released here or not at all.
    let private startOrRelease (server: FIOServer) : FIO<FIOServer, exn> =
        (start server).CatchAll(fun startError ->
            (stop server).Ignore().FlatMap(fun () -> FIO.fail startError))

    // A fiber interrupted while its server started could not hand the server to anyone: it is stopped again.
    let private handOver (server: FIO<FIOServer, exn>) : FIO<FIOServer, exn> =
        FIO.cancellationToken().FlatMap(fun cancellationToken ->
            FIO.uninterruptible (
                server.FlatMap(startOrRelease).FlatMap(fun started ->
                    if cancellationToken.IsCancellationRequested then stop started
                    else FIO.succeed started)))

    /// Creates and starts a server using the given runtime. Prefer runServerWithRuntime, which also stops it.
    let startServerWithRuntime
        (config: ServerConfig)
        (routes: Routes<exn>)
        (runtime: DefaultRuntime) : FIO<FIOServer, exn> =
        handOver (createWithRuntime config routes runtime)

    /// Creates and starts a server with its own default runtime. Prefer runServer, which also stops it.
    let startServer (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        handOver (create config routes)

    let private runScoped (server: FIO<FIOServer, exn>) : FIO<unit, exn> =
        FIO.acquireReleaseWith
            (server.FlatMap startOrRelease)
            (fun started -> (stop started).Unit())
            run

    /// Creates, starts, and runs a server using the given runtime until shutdown, then stops it.
    let runServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<unit, exn> =
        runScoped (createWithRuntime config routes runtime)

    /// Creates, starts, and runs a server with its own default runtime until shutdown, then stops it.
    let runServer (config: ServerConfig) (routes: Routes<exn>) : FIO<unit, exn> =
        runScoped (create config routes)

module ServerBuilder =

    /// Sets the host on a configuration.
    let host host (config: ServerConfig) : ServerConfig =
        { config with Host = host }

    /// Sets the port on a configuration.
    let port port (config: ServerConfig) : ServerConfig =
        { config with Port = port }

    /// Sets the maximum request body size on a configuration.
    let maxBodySize size (config: ServerConfig) : ServerConfig =
        { config with MaxRequestBodySize = size }

    /// Sets how long, in milliseconds, a stopping server lets requests in flight finish; 0 or less waits indefinitely.
    let shutdownTimeout (timeout: int) (config: ServerConfig) : ServerConfig =
        { config with ShutdownTimeout = timeout }

    /// Creates and starts a server from the configuration and routes.
    let startNow (routes: Routes<exn>) (config: ServerConfig) : FIO<FIOServer, exn> =
        Server.startServer config routes

    /// Creates, starts, and runs a server from the configuration and routes.
    let runNow (routes: Routes<exn>) (config: ServerConfig) : FIO<unit, exn> =
        Server.runServer config routes
