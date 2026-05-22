namespace FIO.Http

open FIO.DSL
open FIO.Runtime.Default

open System.Net
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

/// <summary>Represents the configuration for an HTTP server.</summary>
type ServerConfig =
    {
        /// <summary>Represents the host address to bind the server to.</summary>
        Host: string
        /// <summary>Represents the port number to listen on.</summary>
        Port: int
        /// <summary>Represents the maximum request body size in bytes.</summary>
        MaxRequestBodySize: int64
    }

/// <summary>Provides functions for creating server configurations.</summary>
module ServerConfig =

    /// <summary>Returns the default server configuration bound to 127.0.0.1:8080 with a 30 MB body size limit.</summary>
    /// <returns>A server configuration with default host, port, and body size settings.</returns>
    let defaultConfig =
        {
            Host = "127.0.0.1"
            Port = 8080
            MaxRequestBodySize = 30L * 1024L * 1024L // 30MB
        }

    /// <summary>Creates a server configuration with the given host and port.</summary>
    /// <param name="host">The host address to bind to.</param>
    /// <param name="port">The port to listen on.</param>
    /// <returns>A server configuration with the specified host and port.</returns>
    let create host port =
        {
            Host = host
            Port = port
            MaxRequestBodySize = 30L * 1024L * 1024L // 30MB default
        }

    /// <summary>Creates a server configuration with the specified maximum request body size.</summary>
    /// <param name="maxSize">Maximum body size in bytes.</param>
    /// <param name="config">The server configuration to update.</param>
    /// <returns>The updated server configuration.</returns>
    let withMaxBodySize maxSize config =
        { config with MaxRequestBodySize = maxSize }

/// <summary>Represents a running FIO HTTP server.</summary>
type FIOServer =
    private
        {
            /// <summary>Represents the server configuration.</summary>
            Config: ServerConfig
            /// <summary>Represents the registered route table.</summary>
            Routes: Routes<exn>
            /// <summary>Represents the FIO runtime used for effect evaluation.</summary>
            Runtime: DefaultRuntime
            /// <summary>Represents the running Kestrel web host instance.</summary>
            Host: IHost option
        }

/// <summary>Provides functions for creating and running FIO HTTP servers.</summary>
module Server =

    /// <summary>Creates a new FIO HTTP server with the given runtime.</summary>
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

    /// <summary>Creates a new FIO HTTP server with the default runtime.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that produces a new FIO HTTP server.</returns>
    let create (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        createWithRuntime config routes (new DefaultRuntime())

    /// <summary>Builds an effect that starts the HTTP server and begins accepting requests.</summary>
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
                    .awaitUnitTask(app.StartAsync(), id)
                    .CatchAll(fun startError ->
                        FIO
                            .awaitUnitTask(app.DisposeAsync().AsTask(), id)
                            .CatchAll(fun _ -> FIO.unit ())
                            .FlatMap(fun _ -> FIO.fail startError))

            printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}"
            return { server with Host = Some app }
        }

    /// <summary>Builds an effect that stops the HTTP server and releases its resources.</summary>
    /// <param name="server">The server to stop.</param>
    /// <returns>An effect that produces the stopped server.</returns>
    let stop (server: FIOServer) : FIO<FIOServer, exn> =
        fio {
            match server.Host with
            | Some host ->
                do! FIO.awaitUnitTask (host.StopAsync(), id)
                // IHost uses synchronous Dispose, not DisposeAsync
                do! FIO.attempt ((fun () -> host.Dispose()), id)
                printfn "FIO HTTP Server stopped"
                return { server with Host = None }
            | None ->
                printfn "FIO HTTP Server not running"
                return server
        }

    /// <summary>Builds an effect that blocks until the HTTP server shuts down.</summary>
    /// <param name="server">The running server to wait on.</param>
    /// <returns>An effect that completes when the server shuts down.</returns>
    let run (server: FIOServer) : FIO<unit, exn> =
        fio {
            match server.Host with
            | Some host -> do! FIO.awaitUnitTask (host.WaitForShutdownAsync(), id)
            | None -> return! FIO.fail (exn "FIO HTTP Server not started. Call Server.start first.")
        }

    /// <summary>Creates and starts an HTTP server in one operation with the given runtime.</summary>
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

    /// <summary>Creates and starts an HTTP server in one operation with the default runtime.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that produces the started server.</returns>
    let startServer (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        startServerWithRuntime config routes (new DefaultRuntime())

    /// <summary>Builds an effect that creates, starts, and runs an HTTP server until shutdown with the given runtime.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <param name="runtime">The FIO runtime to use for executing effects.</param>
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) : FIO<unit, exn> =
        fio {
            let! server = startServerWithRuntime config routes runtime
            do! run server
        }

    /// <summary>Builds an effect that creates, starts, and runs an HTTP server until shutdown with the default runtime.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to handle requests.</param>
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runServer (config: ServerConfig) (routes: Routes<exn>) : FIO<unit, exn> =
        runServerWithRuntime config routes (new DefaultRuntime())

/// <summary>Provides fluent builder functions for server configuration.</summary>
module ServerBuilder =

    /// <summary>Creates an updated configuration with the specified host address.</summary>
    /// <param name="host">The host address to bind to.</param>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The registered route table.</param>
    /// <returns>The updated configuration and routes tuple.</returns>
    let host (host: string) ((config, routes): ServerConfig * Routes<exn>) = { config with Host = host }, routes

    /// <summary>Creates an updated configuration with the specified port.</summary>
    /// <param name="port">The port number to listen on.</param>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The registered route table.</param>
    /// <returns>The updated configuration and routes tuple.</returns>
    let port (port: int) (config, routes) = { config with Port = port }, routes

    /// <summary>Builds an effect that starts the server with the current configuration.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The registered route table.</param>
    /// <returns>An effect that produces the started server.</returns>
    let startNow (config, routes) = Server.startServer config routes

    /// <summary>Builds an effect that starts and runs the server until shutdown.</summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The registered route table.</param>
    /// <returns>An effect that runs the server until shutdown.</returns>
    let runNow (config, routes) = Server.runServer config routes
