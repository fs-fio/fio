namespace FIO.Http

open FIO.DSL
open FIO.Runtime.Default

open System.Net
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

/// Configuration for the HTTP server.
type ServerConfig =
    {
        /// The host address to bind to.
        Host: string
        /// The port to listen on.
        Port: int
        /// Maximum request body size in bytes. Default is 30MB (30 * 1024 * 1024).
        MaxRequestBodySize: int64
    }

/// Functions for creating server configurations.
module ServerConfig =

    /// Default server configuration (127.0.0.1:8080, 30MB max body size).
    let defaultConfig =
        {
            Host = "127.0.0.1"
            Port = 8080
            MaxRequestBodySize = 30L * 1024L * 1024L // 30MB
        }

    /// Creates a server configuration.
    /// <param name="host">The host address to bind to.</param>
    /// <param name="port">The port to listen on.</param>
    /// <returns>A server configuration with the specified host and port.</returns>
    let create host port =
        {
            Host = host
            Port = port
            MaxRequestBodySize = 30L * 1024L * 1024L // 30MB default
        }

    /// Sets the maximum request body size.
    /// <param name="maxSize">Maximum body size in bytes.</param>
    /// <param name="config">The server configuration to update.</param>
    /// <returns>The updated server configuration.</returns>
    let withMaxBodySize maxSize config =
        { config with MaxRequestBodySize = maxSize }

/// Represents a running FIO HTTP server.
type FIOServer =
    private
        {
            Config: ServerConfig
            Routes: Routes<exn>
            Runtime: DefaultRuntime
            Host: IHost option
        }

/// Functions for creating and managing FIO HTTP servers.
module Server =

    /// Creates a new FIO HTTP server with a provided runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    /// <returns>An effect that produces a new FIO HTTP server.</returns>
    let createWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<FIOServer, exn> =
        FIO.attempt (
            (fun () ->
                {
                    Config = config
                    Routes = routes
                    Runtime = runtime
                    Host = None
                }),
            id
        )

    /// Creates a new FIO HTTP server with default runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that produces a new FIO HTTP server.</returns>
    let create (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        createWithRuntime config routes (new DefaultRuntime())

    /// Starts the HTTP server.
    /// <param name="server">The server to start.</param>
    /// <returns>An effect that produces the started server.</returns>
    let start (server: FIOServer) : FIO<FIOServer, exn> =
        fio {
            let builder = WebApplication.CreateBuilder()
            builder.Logging.ClearProviders() |> ignore

            builder.WebHost.ConfigureKestrel(fun options ->
                options.Listen(IPAddress.Parse server.Config.Host, server.Config.Port))
            |> ignore

            let app = builder.Build()

            app.Run(fun ctx ->
                KestrelBridge.handleRequest server.Runtime server.Routes server.Config.MaxRequestBodySize ctx)
            |> ignore

            do!
                FIO
                    .awaitTask(app.StartAsync(), id)
                    .CatchAll(fun startError ->
                        FIO
                            .awaitTask(app.DisposeAsync().AsTask(), id)
                            .CatchAll(fun _ -> FIO.unit ())
                            .FlatMap(fun _ -> FIO.fail startError))

            printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}"
            return { server with Host = Some app }
        }

    /// Stops the HTTP server and disposes resources.
    /// <param name="server">The server to stop.</param>
    /// <returns>An effect that produces the stopped server.</returns>
    let stop (server: FIOServer) : FIO<FIOServer, exn> =
        fio {
            match server.Host with
            | Some host ->
                do! FIO.awaitTask (host.StopAsync(), id)
                // IHost uses synchronous Dispose, not DisposeAsync
                do! FIO.attempt ((fun () -> host.Dispose()), id)
                printfn "FIO HTTP Server stopped"
                return { server with Host = None }
            | None ->
                printfn "FIO HTTP Server not running"
                return server
        }

    /// Waits for the server to shut down.
    /// <param name="server">The running server to wait on.</param>
    /// <returns>An effect that completes when the server shuts down.</returns>
    let run (server: FIOServer) : FIO<unit, exn> =
        fio {
            match server.Host with
            | Some host -> do! FIO.awaitTask (host.WaitForShutdownAsync(), id)
            | None -> return! FIO.fail (exn "FIO HTTP Server not started. Call Server.start first.")
        }

    /// Creates and starts a server in one operation with a provided runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    /// <returns>An effect that produces the started server.</returns>
    let startServerWithRuntime
        (config: ServerConfig)
        (routes: Routes<exn>)
        (runtime: DefaultRuntime)
        : FIO<FIOServer, exn> =
        fio {
            let! server = createWithRuntime config routes runtime
            let! startedServer = start server
            return startedServer
        }

    /// Creates and starts a server in one operation with default runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that produces the started server.</returns>
    let startServer (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        startServerWithRuntime config routes (new DefaultRuntime())

    /// Creates, starts, and runs a server until shutdown (blocking) with a provided runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<unit, exn> =
        fio {
            let! server = startServerWithRuntime config routes runtime
            do! run server
        }

    /// Creates, starts, and runs a server until shutdown (blocking) with default runtime.
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runServer (config: ServerConfig) (routes: Routes<exn>) : FIO<unit, exn> =
        runServerWithRuntime config routes (new DefaultRuntime())

/// Fluent builder functions for server configuration.
module ServerBuilder =

    /// Sets the host address for the server.
    /// <param name="host">The host address to bind to.</param>
    /// <returns>The updated configuration and routes tuple.</returns>
    let host (host: string) ((config, routes): ServerConfig * Routes<exn>) = { config with Host = host }, routes

    /// Sets the port for the server.
    /// <param name="port">The port number to listen on.</param>
    /// <returns>The updated configuration and routes tuple.</returns>
    let port (port: int) (config, routes) = { config with Port = port }, routes

    /// Starts the server immediately.
    /// <returns>An effect that produces the started server.</returns>
    let startNow (config, routes) = Server.startServer config routes

    /// Starts and runs the server until shutdown.
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runNow (config, routes) = Server.runServer config routes
