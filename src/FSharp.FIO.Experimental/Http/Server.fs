/// <summary>
/// FIO HTTP server types and functions.
/// </summary>
namespace FSharp.FIO.Experimental.Http

open FSharp.FIO.DSL
open FSharp.FIO.Runtime.Default

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
}

/// <summary>
/// Functions for creating server configurations.
/// </summary>
module ServerConfig =

    /// <summary>
    /// Default server configuration (127.0.0.1:8080).
    /// </summary>
    let defaultConfig = {
        Host = "127.0.0.1"
        Port = 8080
    }

    /// <summary>
    /// Creates a server configuration.
    /// </summary>
    /// <param name="host">The host address.</param>
    /// <param name="port">The port number.</param>
    let create host port = {
        Host = host
        Port = port
    }

/// <summary>
/// Represents a running FIO HTTP server.
/// </summary>
type FIOServer = private {
    Config: ServerConfig
    Routes: Routes<exn>
    Runtime: DefaultRuntime
    mutable Host: IHost option
}

/// <summary>
/// Functions for creating and managing FIO HTTP servers.
/// </summary>
module Server =

    /// <summary>
    /// Creates a new FIO HTTP server.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let create (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        FIO.Attempt(
            (fun () ->
                {
                    Config = config
                    Routes = routes
                    Runtime = new DefaultRuntime()
                    Host = None
                }),
            id)

    /// <summary>
    /// Starts the HTTP server.
    /// </summary>
    /// <param name="server">The server to start.</param>
    let start (server: FIOServer) : FIO<unit, exn> =
        fio {
            // Build the web application
            let builder = WebApplication.CreateBuilder()

            builder.WebHost.ConfigureKestrel(fun options ->
                options.Listen(IPAddress.Parse server.Config.Host, server.Config.Port)
            ) |> ignore

            let app = builder.Build()

            app.Run(fun ctx ->
                KestrelBridge.handleRequest server.Runtime server.Routes ctx
            ) |> ignore

            // Start the server asynchronously and wait for it to be ready
            do! FIO.AwaitTask(app.StartAsync(), id)

            server.Host <- Some app

            printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}"
        }

    /// <summary>
    /// Stops the HTTP server.
    /// </summary>
    /// <param name="server">The server to stop.</param>
    let stop (server: FIOServer) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                match server.Host with
                | Some host ->
                    host.StopAsync() |> Async.AwaitTask |> Async.RunSynchronously
                    server.Host <- None
                    printfn "FIO HTTP Server stopped"
                | None ->
                    printfn "FIO HTTP Server not running"
            ),
            id)

    /// <summary>
    /// Waits for the server to shut down.
    /// </summary>
    /// <param name="server">The server to wait on.</param>
    let run (server: FIOServer) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                match server.Host with
                | Some host ->
                    host.WaitForShutdownAsync() |> Async.AwaitTask |> Async.RunSynchronously
                | None ->
                    failwith "FIO HTTP Server not started. Call Server.start first."
            ),
            id)

    /// <summary>
    /// Creates and starts a server in one operation.
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let startServer (config: ServerConfig) (routes: Routes<exn>) : FIO<FIOServer, exn> =
        fio {
            let! server = create config routes
            do! start server
            return server
        }

    /// <summary>
    /// Creates, starts, and runs a server until shutdown (blocking).
    /// </summary>
    /// <param name="config">The server configuration.</param>
    /// <param name="routes">The routes to serve.</param>
    let runServer (config: ServerConfig) (routes: Routes<exn>) : FIO<unit, exn> =
        fio {
            let! server = startServer config routes
            do! run server
        }

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
