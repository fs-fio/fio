module private FIO.Examples.Http

open FIO.DSL
open FIO.App
open FIO.Http
open FIO.Http.SimpleRoutes
open FIO.Http.RoutesOperators
open FIO.Http.MiddlewareOperators

open System
open System.Text.Json

// A small HTTP server demonstrating routing, JSON, query params, and middleware.
type HttpApp() =
    inherit FIOApp<unit, exn>()

    // Responds with a plain-text greeting.
    let helloHandler = HttpHandler.text "Hello from FIO HTTP!"

    // Returns a JSON object built from an anonymous record.
    let jsonHandler request =
        HttpHandler.okJson {| message = "JSON response"; timestamp = DateTime.UtcNow |} request

    // Echoes the requested path back to the caller.
    let echoHandler request =
        HttpHandler.text $"You requested: {request.Path}" request

    // Parses a JSON body and greets the name, falling back to 400 on bad input.
    let greetHandler (request: HttpRequest) =
        (FIO.attempt (fun () ->
                use doc = JsonDocument.Parse(request.Body.AsString())
                let name = doc.RootElement.GetProperty("name").GetString()
                Response.okJson {| greeting = $"Hello, {name}!" |})
            id)
            .Fold (fun _ ->
                Response.badRequestText "Invalid or missing JSON body. Expected { \"name\": \"...\" }.") id

    // Reads the 'n' query parameter and returns its square, validating the input.
    let squareHandler request =
        match Map.tryFind "n" request.QueryParams with
        | Some(value :: _) ->
            match Int32.TryParse value with
            | true, n -> FIO.succeed (Response.okJson {| input = n; square = n * n |})
            | _ -> FIO.succeed (Response.badRequestText "Query parameter 'n' must be an integer.")
        | _ -> FIO.succeed (Response.badRequestText "Missing query parameter 'n'.")

    // Maps each path and method to its handler.
    let routes =
        get "/" helloHandler
        ++ get "/json" jsonHandler
        ++ get "/echo" echoHandler
        ++ post "/greet" greetHandler
        ++ get "/square" squareHandler

    // Middleware that logs each incoming request with a timestamp.
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
