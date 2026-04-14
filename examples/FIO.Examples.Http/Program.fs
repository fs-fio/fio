/// <summary>
/// FIO.Http example - HTTP server with routing and middleware.
/// Endpoints: GET /, GET /json, GET /echo
/// </summary>
module private FIO.Examples.Http.Program

open FIO.DSL
open FIO.App
open FIO.Http
open FIO.Http.RoutesOperators
open FIO.Http.MiddlewareOperators

open System

/// <summary>
/// FIOApp wrapper for the HTTP server.
/// </summary>
type private HttpApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Simple text response handler.
    /// </summary>
    let helloHandler = HttpHandler.text "Hello from FIO HTTP!"

    /// <summary>
    /// JSON response handler with timestamp.
    /// </summary>
    let jsonHandler =
        HttpHandler.okJson {| message = "JSON response"; timestamp = DateTime.UtcNow |}

    /// <summary>
    /// Echo handler that returns the requested path.
    /// </summary>
    let echoHandler request =
        HttpHandler.text $"You requested: {request.Path}" request

    /// <summary>
    /// Route definitions combining all handlers.
    /// </summary>
    let routes =
        get "/" helloHandler ++ get "/json" jsonHandler ++ get "/echo" echoHandler

    /// <summary>
    /// Logging middleware that prints request info.
    /// </summary>
    let logging =
        Middleware.before (fun request ->
            FIO.attempt (
                (fun () ->
                    let ts = DateTime.Now.ToString "HH:mm:ss"
                    printfn $"[{ts}] {request.Method} {request.Path}"),
                id
            ))

    override _.effect = Server.runServer ServerConfig.defaultConfig (routes @@ logging)

[<EntryPoint>]
let main _ = HttpApp().Run()
