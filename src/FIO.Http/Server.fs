namespace FIO.Http

open FIO.DSL
open FIO.Runtime.Default

open System
open System.Net
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

type ServerConfig =
    {
        Host: string
        Port: int
        MaxRequestBodySize: int64
    }

[<RequireQualifiedAccess>]
module ServerConfig =

    let defaultConfig =
        {
            Host = "127.0.0.1"
            Port = 8080
            MaxRequestBodySize = 30L * 1024L * 1024L
        }

    let create host port =
        {
            Host = host
            Port = port
            MaxRequestBodySize = 30L * 1024L * 1024L
        }

    let withMaxBodySize maxSize config =
        { config with MaxRequestBodySize = maxSize }

type FIOServer =
    private
        {
            Config: ServerConfig
            Routes: Routes<exn>
            Runtime: DefaultRuntime
            Host: IHost option ref
            OwnsRuntime: bool
        }

[<RequireQualifiedAccess>]
module Server =

    let createWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) =
        FIO.attempt (fun () ->
            {
                Config = config
                Routes = routes
                Runtime = runtime
                Host = ref None
                OwnsRuntime = false
            })
            id

    let create (config: ServerConfig) (routes: Routes<exn>) =
        FIO.attempt (fun () ->
            {
                Config = config
                Routes = routes
                Runtime = new DefaultRuntime()
                Host = ref None
                OwnsRuntime = true
            })
            id

    let start (server: FIOServer) =
        fio {
            let! app =
                FIO.attempt (fun () ->
                    let builder = WebApplication.CreateBuilder()
                    builder.Logging.ClearProviders() |> ignore

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

                    app.Run(fun ctx ->
                        KestrelBridge.handleRequest
                            server.Runtime
                            server.Routes
                            server.Config.MaxRequestBodySize
                            ctx)
                    |> ignore

                    app)
                    id

            do!
                (FIO.awaitUnitTask (app.StartAsync()) id).CatchAll <| fun startError ->
                    (FIO.awaitUnitTask (app.DisposeAsync().AsTask()) id)
                        .Ignore().FlatMap(fun () -> FIO.fail startError)

            do!
                FIO.attempt (fun () ->
                    server.Host.Value <- Some(app :> IHost)
                    printfn $"FIO HTTP Server listening on http://{server.Config.Host}:{server.Config.Port}")
                    id

            return server
        }

    let stop (server: FIOServer) =
        fio {
            match server.Host.Value with
            | Some host ->
                do! FIO.awaitUnitTask (host.StopAsync()) id
                do!
                    FIO.attempt (fun () ->
                        host.Dispose()
                        server.Host.Value <- None
                        if server.OwnsRuntime then
                            (server.Runtime :> IDisposable).Dispose()
                        printfn "FIO HTTP Server stopped")
                        id
                return server
            | None ->
                printfn "FIO HTTP Server not running"
                return server
        }

    let run (server: FIOServer) =
        fio {
            match server.Host.Value with
            | Some host ->
                do! FIO.awaitUnitTask (host.WaitForShutdownAsync()) id
            | None ->
                return! FIO.fail (exn "FIO HTTP Server not started. Call Server.start first.")
        }

    let startServerWithRuntime
        (config: ServerConfig)
        (routes: Routes<exn>)
        (runtime: DefaultRuntime) =
        fio {
            let! server = createWithRuntime config routes runtime
            let! startedServer = start server
            return startedServer
        }

    let startServer (config: ServerConfig) (routes: Routes<exn>) =
        fio {
            let! server = create config routes
            let! startedServer = start server
            return startedServer
        }

    let runServerWithRuntime (config: ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) =
        fio {
            let! server = startServerWithRuntime config routes runtime
            do! (run server).Ensuring((stop server).Unit())
        }

    let runServer (config: ServerConfig) (routes: Routes<exn>) =
        fio {
            let! server = startServer config routes
            do! (run server).Ensuring((stop server).Unit())
        }

module ServerBuilder =

    let host host (config: ServerConfig) =
        { config with Host = host }

    let port port (config: ServerConfig) =
        { config with Port = port }

    let maxBodySize size (config: ServerConfig) =
        { config with MaxRequestBodySize = size }

    let startNow (routes: Routes<exn>) (config: ServerConfig) =
        Server.startServer config routes

    let runNow (routes: Routes<exn>) (config: ServerConfig) =
        Server.runServer config routes
