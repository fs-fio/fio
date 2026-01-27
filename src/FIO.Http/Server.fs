namespace FIO.Http

open FIO.DSL
open FIO.Runtime.Default

open System.Net
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

/// <summary>
/// Configuration for the HTTP server.
/// </summary>
type ServerConfig = {
    /// <summary>The host address to bind to.</summary>
    Host: string
    /// <summary>The port to listen on.</summary>
    Port: int
    /// <summary>Maximum request body size in bytes. Default is 30MB (30 * 1024 * 1024).</summary>
    MaxRequestBodySize: int64
}

/// <summary>
/// Functions for creating server configurations.
/// </summary>
module ServerConfig =

    /// <summary>
    /// Default server configuration (127.0.0.1:8080, 30MB max body size).
    /// </summary>
    let defaultConfig = {
        Host = "127.0.0.1"
        Port = 8080
        MaxRequestBodySize = 30L * 1024L * 1024L // 30MB
    }

    /// <summary>
    /// Creates a server configuration.
    /// </summary>
    /// <param name="host">The host address.</param>
    /// <param name="port">The port number.</param>
    let create host port = {
        Host = host
        Port = port
        MaxRequestBodySize = 30L * 1024L * 1024L // 30MB default
    }

    /// <summary>
    /// Sets the maximum request body size.
    /// </summary>
    /// <param name="maxSize">Maximum body size in bytes.</param>
    /// <param name="config">The configuration to modify.</param>
    let withMaxBodySize maxSize config = {
        config with MaxRequestBodySize = maxSize
    }

/// <summary>
/// Represents a running FIO HTTP server.
/// </summary>
type FIOServer = private {
    Config: ServerConfig
    Routes: Routes<exn>
    Runtime: DefaultRuntime
    Host: IHost option
}

/// <summary>
/// Functions for creating and managing FIO HTTP servers.
/// </summary>
module Server =

    /// <summary>
    /// Creates a new FIO HTTP server with a provided runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    let createWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<FIOServer, exn> =
        FIO.attempt((fun () ->
            {
                Config = config
                Routes = routes
                Runtime = runtime
                Host = None
            }), id)

    /// <summary>
    /// Creates a new FIO HTTP server with default runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let create (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        createWithRuntime config routes (new DefaultRuntime())

    /// <summary>
    /// Starts the HTTP server.
    /// </summary>
    /// <param name="server">The server to start.</param>
    let start (server: FIOServer) : FIO<FIOServer, exn> =
        fio {
            let builder = WebApplication.CreateBuilder()

            builder.WebHost.ConfigureKestrel(fun options ->
                options.Listen(IPAddress.Parse server.Config.Host, server.Config.Port)
            ) |> ignore

            let app = builder.Build()

            app.Run(fun ctx ->
                KestrelBridge.handleRequest server.Runtime server.Routes server.Config.MaxRequestBodySize ctx
            ) |> ignore

            let! startResult =
                try
                    FIO.awaitTask(app.StartAsync(), id)
                with exn ->
                    app.DisposeAsync().AsTask() |> Async.AwaitTask |> Async.RunSynchronously
                    raise exn
   
            printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}"
            return { server with Host = Some app }
        }

    /// <summary>
    /// Stops the HTTP server and disposes resources.
    /// </summary>
    /// <param name="server">The server to stop.</param>
    let stop (server: FIOServer) : FIO<FIOServer, exn> =
        fio {
            match server.Host with
            | Some host ->
                do! FIO.awaitTask(host.StopAsync(), id)
                // IHost uses synchronous Dispose, not DisposeAsync
                do! FIO.attempt((fun () -> host.Dispose()), id)
                printfn "FIO HTTP Server stopped"
                return { server with Host = None }
            | None ->
                printfn "FIO HTTP Server not running"
                return server
        }

    /// <summary>
    /// Waits for the server to shut down.
    /// </summary>
    /// <param name="server">The server to wait on.</param>
    let run (server: FIOServer) : FIO<unit, exn> =
        fio {
            match server.Host with
            | Some host ->
                do! FIO.awaitTask(host.WaitForShutdownAsync(), id)
            | None ->
                return! FIO.fail(exn "FIO HTTP Server not started. Call Server.start first.")
        }

    /// <summary>
    /// Creates and starts a server in one operation with a provided runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    let startServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<FIOServer, exn> =
        fio {
            let! server = createWithRuntime config routes runtime
            let! startedServer = start server
            return startedServer
        }

    /// <summary>
    /// Creates and starts a server in one operation with default runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let startServer (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        startServerWithRuntime config routes (new DefaultRuntime())

    /// <summary>
    /// Creates, starts, and runs a server until shutdown (blocking) with a provided runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    let runServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<unit, exn> =
        fio {
            let! server = startServerWithRuntime config routes runtime
            do! run server
        }

    /// <summary>
    /// Creates, starts, and runs a server until shutdown (blocking) with default runtime.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let runServer (config: ServerConfig) (routes: Routes<exn>) : FIO<unit, exn> =
        runServerWithRuntime config routes (new DefaultRuntime())

/// <summary>
/// Fluent builder functions for server configuration.
/// </summary>
module ServerBuilder =

    /// <summary>
    /// Starts building a server with routes.
    /// </summary>
    /// <param name="routes">The routes to serve.</param>
    let server (routes: Routes<exn>) =
        ServerConfig.defaultConfig, routes

    /// <summary>
    /// Sets the host address for the server.
    /// </summary>
    /// <param name="host">The host address.</param>
    /// <param name="config">The current configuration tuple.</param>
    let host (host: string) ((config, routes): ServerConfig * Routes<exn>) =
        { config with Host = host }, routes

    /// <summary>
    /// Sets the port for the server.
    /// </summary>
    /// <param name="port">The port number.</param>
    /// <param name="config">The current configuration tuple.</param>
    let port (port: int) (config, routes) =
        { config with Port = port }, routes

    /// <summary>
    /// Starts the server immediately.
    /// </summary>
    /// <param name="config">The configuration tuple.</param>
    let startNow (config, routes) =
        Server.startServer config routes

    /// <summary>
    /// Starts and runs the server until shutdown.
    /// </summary>
    /// <param name="config">The configuration tuple.</param>
    let runNow (config, routes) =
        Server.runServer config routes
