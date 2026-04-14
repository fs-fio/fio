module FIO.Http.Tests.MiddlewareTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http

open System

open Expecto

let private applyMiddleware
    (middleware: Middleware<exn>)
    (handler: HttpHandler<exn>)
    (request: HttpRequest)
    (runtime: FIO.Runtime.FIORuntime)
    : HttpResponse =
    let routes = Routes.route (Route.get "/test") handler |> Middleware.apply middleware
    dispatchAndRun runtime routes request

[<Tests>]
let middlewareTests =
    testList
        "Middleware"
        [

            testAllRuntimes "addHeader adds header to all responses" (fun runtime ->
                let mw = Middleware.addHeader "X-Custom" "value"

                let resp =
                    applyMiddleware mw (HttpHandler.text "hello") (makeGetRequest "/test") runtime

                Expect.equal (HttpResponse.header "X-Custom" resp) (Some "value") "Header added")

            testAllRuntimes "addHeaders adds multiple headers" (fun runtime ->
                let mw = Middleware.addHeaders [ "X-A", "1"; "X-B", "2" ]

                let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.equal (HttpResponse.header "X-A" resp) (Some "1") "X-A"
                Expect.equal (HttpResponse.header "X-B" resp) (Some "2") "X-B")

            testAllRuntimes "before runs effect before handler" (fun runtime ->
                let mutable ran = false
                let mw = Middleware.before (fun _ -> FIO.attemptExn (fun () -> ran <- true))

                let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.isTrue ran "Before effect ran"
                Expect.equal resp.Status HttpStatusCode.OK "200")

            testAllRuntimes "after runs effect after handler" (fun runtime ->
                let mutable capturedStatus = HttpStatusCode.Continue

                let mw =
                    Middleware.after (fun _ resp -> FIO.attemptExn (fun () -> capturedStatus <- resp.Status))

                let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.equal capturedStatus HttpStatusCode.OK "Captured status"
                Expect.equal resp.Status HttpStatusCode.OK "200")

            testAllRuntimes "compose applies outer after inner" (fun runtime ->
                let inner = Middleware.addHeader "X-Inner" "true"
                let outer = Middleware.addHeader "X-Outer" "true"
                let composed = Middleware.compose outer inner

                let resp = applyMiddleware composed HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.equal (HttpResponse.header "X-Inner" resp) (Some "true") "Inner"
                Expect.equal (HttpResponse.header "X-Outer" resp) (Some "true") "Outer")

            testAllRuntimes "requestId adds X-Request-ID header" (fun runtime ->
                let mw = Middleware.requestId (fun () -> "test-id-123")

                let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.equal (HttpResponse.header "X-Request-ID" resp) (Some "test-id-123") "Request ID")

            testAllRuntimes "logging calls logger with request" (fun runtime ->
                let mutable loggedPath = ""

                let mw =
                    Middleware.logging (fun req -> FIO.attemptExn (fun () -> loggedPath <- req.Path))

                let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                Expect.equal loggedPath "/test" "Logged path"
                Expect.equal resp.Status HttpStatusCode.OK "200")

            testList
                "timeout"
                [

                    testAllRuntimes "returns handler response when fast enough" (fun runtime ->
                        let mw = Middleware.timeout (TimeSpan.FromSeconds 5.0) id

                        let resp =
                            applyMiddleware mw (HttpHandler.text "fast") (makeGetRequest "/test") runtime

                        match resp.Body with
                        | Text t -> Expect.equal t "fast" "Fast response"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "returns 408 when handler exceeds duration" (fun runtime ->
                        let slowHandler =
                            fun _ ->
                                FIO
                                    .sleep(TimeSpan.FromSeconds 10.0, id)
                                    .FlatMap(fun () -> FIO.succeed (Response.okText "slow"))

                        let mw = Middleware.timeout (TimeSpan.FromMilliseconds 100.0) id

                        let resp = applyMiddleware mw slowHandler (makeGetRequest "/test") runtime

                        Expect.equal resp.Status HttpStatusCode.RequestTimeout "408 Timeout")

                    testAllRuntimes "timeoutExn convenience works" (fun runtime ->
                        let mw = Middleware.timeoutExn (TimeSpan.FromSeconds 5.0)

                        let resp =
                            applyMiddleware mw (HttpHandler.text "ok") (makeGetRequest "/test") runtime

                        Expect.equal resp.Status HttpStatusCode.OK "200")
                ]

            testList
                "cors"
                [

                    testAllRuntimes "adds CORS headers for normal request" (fun runtime ->
                        let mw =
                            Middleware.cors [ "http://example.com" ] [ "GET"; "POST" ] [ "Content-Type" ]

                        let req =
                            makeGetRequest "/test" |> HttpRequest.withHeader "Origin" "http://example.com"

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal
                            (HttpResponse.header "Access-Control-Allow-Origin" resp)
                            (Some "http://example.com")
                            "Allow-Origin"

                        Expect.equal
                            (HttpResponse.header "Access-Control-Allow-Methods" resp)
                            (Some "GET, POST")
                            "Allow-Methods")

                    testAllRuntimes "returns 204 for OPTIONS preflight" (fun runtime ->
                        let mw = Middleware.cors [ "*" ] [ "GET"; "POST" ] [ "Content-Type" ]
                        let routes = Routes.route (Route.get "/test") HttpHandler.ok |> Middleware.apply mw

                        let req =
                            HttpRequest.create HttpMethod.OPTIONS "/test"
                            |> HttpRequest.withHeader "Origin" "http://example.com"

                        let resp = dispatchAndRun runtime routes req

                        Expect.equal resp.Status HttpStatusCode.NoContent "204 Preflight"
                        Expect.isSome (HttpResponse.header "Access-Control-Max-Age" resp) "Max-Age header")

                    testAllRuntimes "returns 403 for disallowed origin" (fun runtime ->
                        let mw = Middleware.cors [ "http://allowed.com" ] [ "GET" ] [ "Content-Type" ]

                        let req =
                            makeGetRequest "/test" |> HttpRequest.withHeader "Origin" "http://evil.com"

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal resp.Status HttpStatusCode.Forbidden "403")

                    testAllRuntimes "allows request without Origin header" (fun runtime ->
                        let mw = Middleware.cors [ "http://allowed.com" ] [ "GET" ] [ "Content-Type" ]

                        let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                        Expect.equal resp.Status HttpStatusCode.OK "No origin = allowed")
                ]

            testList
                "basicAuth"
                [

                    testAllRuntimes "passes with valid credentials" (fun runtime ->
                        let authenticate user pass =
                            FIO.succeed (user = "admin" && pass = "secret")

                        let mw = Middleware.basicAuth authenticate
                        let encoded = Convert.ToBase64String(Text.Encoding.UTF8.GetBytes "admin:secret")

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" $"Basic {encoded}"

                        let resp = applyMiddleware mw (HttpHandler.text "authed") req runtime

                        Expect.equal resp.Status HttpStatusCode.OK "Authenticated")

                    testAllRuntimes "rejects invalid credentials" (fun runtime ->
                        let authenticate user pass =
                            FIO.succeed (user = "admin" && pass = "secret")

                        let mw = Middleware.basicAuth authenticate
                        let encoded = Convert.ToBase64String(Text.Encoding.UTF8.GetBytes "admin:wrong")

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" $"Basic {encoded}"

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

                    testAllRuntimes "rejects missing Authorization header" (fun runtime ->
                        let authenticate _ _ = FIO.succeed true
                        let mw = Middleware.basicAuth authenticate

                        let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

                    testAllRuntimes "rejects malformed base64" (fun runtime ->
                        let authenticate _ _ = FIO.succeed true
                        let mw = Middleware.basicAuth authenticate

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" "Basic !!not-valid-base64!!"

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401 for bad base64")

                    testAllRuntimes "handles colon in password" (fun runtime ->
                        let authenticate user pass =
                            FIO.succeed (user = "admin" && pass = "pass:word:with:colons"): FIO<bool, exn>

                        let mw = Middleware.basicAuth authenticate

                        let encoded =
                            Convert.ToBase64String(Text.Encoding.UTF8.GetBytes "admin:pass:word:with:colons")

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" $"Basic {encoded}"

                        let resp = applyMiddleware mw (HttpHandler.text "ok") req runtime

                        Expect.equal resp.Status HttpStatusCode.OK "Colons in password")
                ]

            testList
                "bearerAuth"
                [

                    testAllRuntimes "passes with valid token and attaches user" (fun runtime ->
                        let authenticate token =
                            if token = "valid-token" then
                                FIO.succeed (Some "user1")
                            else
                                FIO.succeed None

                        let mw = Middleware.bearerAuth authenticate

                        let handler =
                            fun req ->
                                match HttpRequest.metadata<string> "User" req with
                                | Some user -> FIO.succeed (Response.okText user)
                                | None -> FIO.succeed (Response.badRequestText "no user")

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" "Bearer valid-token"

                        let resp = applyMiddleware mw handler req runtime

                        match resp.Body with
                        | Text t -> Expect.equal t "user1" "User metadata"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "rejects invalid token" (fun runtime ->
                        let authenticate _ = FIO.succeed None
                        let mw = Middleware.bearerAuth authenticate

                        let req =
                            makeGetRequest "/test"
                            |> HttpRequest.withHeader "Authorization" "Bearer bad-token"

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

                    testAllRuntimes "rejects missing Authorization" (fun runtime ->
                        let authenticate _ = FIO.succeed (Some "user")
                        let mw = Middleware.bearerAuth authenticate

                        let resp = applyMiddleware mw HttpHandler.ok (makeGetRequest "/test") runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

                    testAllRuntimes "rejects empty token" (fun runtime ->
                        let authenticate _ = FIO.succeed (Some "user")
                        let mw = Middleware.bearerAuth authenticate
                        let req = makeGetRequest "/test" |> HttpRequest.withHeader "Authorization" "Bearer "

                        let resp = applyMiddleware mw HttpHandler.ok req runtime

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401 for empty token")
                ]

            testAllRuntimes "errorHandler catches and converts error to response" (fun runtime ->
                let mw =
                    Middleware.errorHandler (fun (ex: exn) -> Response.internalServerErrorText ex.Message)

                let failingHandler = fun _ -> FIO.fail (exn "boom")
                let resp = applyMiddleware mw failingHandler (makeGetRequest "/test") runtime

                Expect.equal resp.Status HttpStatusCode.InternalServerError "500"

                match resp.Body with
                | Text t -> Expect.stringContains t "boom" "Error message"
                | _ -> failtest "Expected Text body")

            testList
                "MiddlewareOperators"
                [

                    testAllRuntimes "@@ applies middleware to routes" (fun runtime ->
                        let routes =
                            MiddlewareOperators.(@@)
                                (Routes.route (Route.get "/test") HttpHandler.ok)
                                (Middleware.addHeader "X-Applied" "yes")

                        let resp = dispatchAndRun runtime routes (makeGetRequest "/test")

                        Expect.equal (HttpResponse.header "X-Applied" resp) (Some "yes") "Applied")

                    testAllRuntimes "+++ composes two middlewares" (fun runtime ->
                        let composed =
                            MiddlewareOperators.(+++) (Middleware.addHeader "X-A" "1") (Middleware.addHeader "X-B" "2")

                        let resp = applyMiddleware composed HttpHandler.ok (makeGetRequest "/test") runtime

                        Expect.isSome (HttpResponse.header "X-A" resp) "X-A"
                        Expect.isSome (HttpResponse.header "X-B" resp) "X-B")
                ]
        ]
