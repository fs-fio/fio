module private FIO.Examples.Http

open FIO.DSL
open FIO.App
open FIO.Http
open FIO.Http.SimpleRoutes
open FIO.Http.RoutesOperators
open FIO.Http.MiddlewareOperators

open System

type private HttpApp() =
    inherit FIOApp<unit, exn>()

    let helloHandler = HttpHandler.text "Hello from FIO HTTP!"

    let jsonHandler =
        HttpHandler.okJson {| message = "JSON response"; timestamp = DateTime.UtcNow |}

    let echoHandler request =
        HttpHandler.text $"You requested: {request.Path}" request

    let routes =
        get "/" helloHandler ++ get "/json" jsonHandler ++ get "/echo" echoHandler

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
