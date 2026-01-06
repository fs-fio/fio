open FSharp.FIO.DSL
open FSharp.FIO.Runtime.Default
open FSharp.FIO.Http
open FSharp.FIO.Http.RoutesOperators
open FSharp.FIO.Http.MiddlewareOperators

open System

let helloHandler : HttpHandler<exn> =
    HttpHandler.text "Hello from FIO HTTP!"

let jsonHandler : HttpHandler<exn> =
    HttpHandler.okJson {| message = "This is JSON"; timestamp = DateTime.UtcNow |}

let echoHandler : HttpHandler<exn> =
    fun request ->
        let path = request.Path
        HttpHandler.text $"You requested: {path}" request

let routes =
    GET "/" helloHandler ++ GET "/json" jsonHandler ++ GET "/echo" echoHandler

let logging : Middleware<exn> =
    Middleware.before (fun request ->
        FIO.Attempt((fun () ->
            let timestamp = System.DateTime.Now.ToString "HH:mm:ss"
            printfn $"[{timestamp}] {request.Method} {request.Path}"), id))

let routesWithMiddleware =
    routes @@ logging

[<EntryPoint>]
let main _ =
    printfn "Starting FIO HTTP Server Example..."
    printfn "Press Ctrl+C to stop the server"
    printfn ""

    let config = ServerConfig.defaultConfig
    let serverEffect = Server.runServer config routesWithMiddleware

    let runtime = new DefaultRuntime()
    let fiber = runtime.Run serverEffect

    match fiber.UnsafeResult() with
    | Ok () ->
        printfn "FIO HTTP Server stopped successfully"
        0
    | Error exn ->
        printfn $"FIO HTTP Server error: {exn.Message}"
        1
