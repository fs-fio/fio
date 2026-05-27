/// <summary>Provides an HTTP server example demonstrating FIO.Http routing, handler composition, and middleware pipeline patterns.</summary>
module private FIO.Examples.Http.Program

open FIO.DSL
open FIO.App
open FIO.Http
open FIO.Http.RoutesOperators
open FIO.Http.MiddlewareOperators

open System

/// <summary>Represents an HTTP server application that demonstrates route definition, JSON responses, and request logging middleware using FIO.Http.</summary>
/// <remarks>Defines three GET endpoints (/, /json, /echo) composed with the <c>++</c> route operator and applies a logging middleware via the <c>@@</c> middleware operator.</remarks>
type private HttpApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds a plain-text response handler that replies with a static greeting message.</summary>
    let helloHandler = HttpHandler.text "Hello from FIO HTTP!"

    /// <summary>Builds a JSON response handler that replies with a message and the current UTC timestamp as a serialized anonymous record.</summary>
    let jsonHandler =
        HttpHandler.okJson {| message = "JSON response"; timestamp = DateTime.UtcNow |}

    /// <summary>Builds a text response handler that echoes the requested path back to the caller, demonstrating per-request access to <c>HttpRequest</c> fields.</summary>
    /// <param name="request">The incoming HTTP request whose path is included in the response body.</param>
    /// <returns>An effect that sends a plain-text response containing the request path.</returns>
    let echoHandler request =
        HttpHandler.text $"You requested: {request.Path}" request

    /// <summary>Combines the hello, JSON, and echo handlers into a single route table using the <c>++</c> route composition operator.</summary>
    let routes =
        get "/" helloHandler ++ get "/json" jsonHandler ++ get "/echo" echoHandler

    /// <summary>Builds a before-middleware that logs the HTTP method and path of each incoming request with a timestamp to standard output.</summary>
    /// <remarks>Uses <c>Middleware.before</c> to run a side-effecting action before the request reaches the matched route handler.</remarks>
    let logging =
        Middleware.before (fun request ->
            FIO.attempt
                (fun () ->
                    let ts = DateTime.Now.ToString "HH:mm:ss"
                    printfn $"[{ts}] {request.Method} {request.Path}")
                id)

    override _.effect =
        Server.runServer ServerConfig.defaultConfig (routes @@ logging)

[<EntryPoint>]
let main _ = HttpApp().Run()
