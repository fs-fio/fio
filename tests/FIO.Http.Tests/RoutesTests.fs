module FIO.Http.Tests.RoutesTests

open FIO.Tests.Utilities
open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Http.SimpleRoutes

open Expecto

[<Tests>]
let routesTests =
    testList
        "Routes"
        [
            testAllRuntimesSequenced "empty - dispatches to 404" (fun runtime ->
                let resp = dispatchAndRun runtime Routes.empty (makeGetRequest "/anything")

                Expect.equal resp.Status HttpStatusCode.NotFound "404")

            testAllRuntimesSequenced "route - creates a dispatchable single route" (fun runtime ->
                let routes = Routes.route (Route.get "/hello") (HttpHandler.text "world")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/hello")

                Expect.equal resp.Status HttpStatusCode.OK "200"
                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "world" "Body"
                | _ -> failtest "Expected Text body")

            testAllRuntimesSequenced "single - creates a parameterized route" (fun runtime ->
                let pattern = RoutePattern.get (RoutePath.withInt [ "users" ] [])
                let routes =
                    Routes.single pattern (fun params' ->
                        match params' with
                        | [ :? int as id ] -> HttpHandler.text $"user-{id}"
                        | _ -> HttpHandler.badRequest)

                let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/42")

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "user-42" "Param extracted"
                | _ -> failtest "Expected Text body")

            testAllRuntimesSequenced "combine - merges two route sets" (fun runtime ->
                let r1 = Routes.route (Route.get "/a") (HttpHandler.text "A")
                let r2 = Routes.route (Route.get "/b") (HttpHandler.text "B")
                let combined = Routes.combine r1 r2

                let respA = dispatchAndRun runtime combined (makeGetRequest "/a")
                let respB = dispatchAndRun runtime combined (makeGetRequest "/b")

                match respA.Body, respB.Body with
                | ResponseBody.Text a, ResponseBody.Text b ->
                    Expect.equal a "A" "Route A"
                    Expect.equal b "B" "Route B"
                | _ -> failtest "Expected Text bodies")

            testAllRuntimesSequenced "dispatch - matches an exact route via the index" (fun runtime ->
                let routes = Routes.route (Route.get "/exact") (HttpHandler.text "found")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/exact")

                Expect.equal resp.Status HttpStatusCode.OK "Exact match")

            testAllRuntimesSequenced "dispatch - normalizes trailing slashes" (fun runtime ->
                let routes = Routes.route (Route.get "/path") (HttpHandler.text "ok")
                let req =
                    { HttpRequest.create HttpMethod.GET "/path/" with
                        PathSegments = [ "path" ]
                    }

                let resp = dispatchAndRun runtime routes req

                Expect.equal resp.Status HttpStatusCode.OK "Trailing slash normalized")

            testAllRuntimesSequenced "dispatch - returns notFound for an unmatched path" (fun runtime ->
                let routes = Routes.route (Route.get "/exists") (HttpHandler.text "here")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/missing")

                Expect.equal resp.Status HttpStatusCode.NotFound "404")

            testAllRuntimesSequenced "dispatch - returns 405 with an Allow header for the wrong method" (fun runtime ->
                let routes = Routes.route (Route.get "/only-get") (HttpHandler.text "ok")

                let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.POST "/only-get")

                Expect.equal resp.Status HttpStatusCode.MethodNotAllowed "Wrong method -> 405"
                Expect.equal (HttpResponse.header "Allow" resp) (Some "GET") "Allow header lists GET")

            testAllRuntimesSequenced "dispatch - falls through to notFound for OPTIONS" (fun runtime ->
                let routes = Routes.route (Route.get "/only-get") (HttpHandler.text "ok")

                let resp =
                    dispatchAndRun runtime routes (makeRequest HttpMethod.OPTIONS "/only-get")

                Expect.equal resp.Status HttpStatusCode.NotFound "OPTIONS not treated as 405")

            testAllRuntimesSequenced "dispatch - routes HEAD to the matching GET handler" (fun runtime ->
                let routes = Routes.route (Route.get "/page") (HttpHandler.text "content")

                let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.HEAD "/page")

                Expect.equal resp.Status HttpStatusCode.OK "HEAD served by GET handler")

            testAllRuntimesSequenced "withNotFound - changes the fallback handler" (fun runtime ->
                let routes = Routes.empty |> Routes.withNotFound (HttpHandler.text "custom 404")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/anything")

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "custom 404" "Custom not found"
                | _ -> failtest "Expected Text body")

            testAllRuntimesSequenced "add - appends a route" (fun runtime ->
                let routes =
                    Routes.empty |> Routes.addRoute (Route.get "/added") (HttpHandler.text "added")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/added")

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "added" "Added route"
                | _ -> failtest "Expected Text body")

            testAllRuntimesSequenced "add - keeps the first handler when a path is added twice" (fun runtime ->
                let routes =
                    Routes.empty
                    |> Routes.addRoute (Route.get "/twice") (HttpHandler.text "first")
                    |> Routes.addRoute (Route.get "/twice") (HttpHandler.text "second")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/twice")

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "first" "The first handler added for a path must win"
                | _ -> failtest "Expected Text body")

            testAllRuntimesSequenced "fromList - creates routes from pattern-handler pairs" (fun runtime ->
                let routes =
                    Routes.fromList
                        [
                            Route.get "/one", HttpHandler.text "1"
                            Route.get "/two", HttpHandler.text "2"
                        ]

                let r1 = dispatchAndRun runtime routes (makeGetRequest "/one")
                let r2 = dispatchAndRun runtime routes (makeGetRequest "/two")

                match r1.Body, r2.Body with
                | ResponseBody.Text a, ResponseBody.Text b ->
                    Expect.equal a "1" "First"
                    Expect.equal b "2" "Second"
                | _ -> failtest "Expected Text bodies")

            testAllRuntimesSequenced "map - transforms every handler" (fun runtime ->
                let routes =
                    Routes.route (Route.get "/test") (HttpHandler.text "original")
                    |> Routes.map (fun handler ->
                        fun req ->
                            fio {
                                let! resp = handler req
                                return HttpResponse.withHeader "X-Mapped" "true" resp
                            })

                let resp = dispatchAndRun runtime routes (makeGetRequest "/test")

                Expect.equal (HttpResponse.header "X-Mapped" resp) (Some "true") "Transformed")

            testList
                "RoutesOperators"
                [

                    testAllRuntimesSequenced "( ++ ) - combines routes" (fun runtime ->
                        let routes =
                            RoutesOperators.(++)
                                (Routes.route (Route.get "/a") (HttpHandler.text "A"))
                                (Routes.route (Route.get "/b") (HttpHandler.text "B"))

                        let r = dispatchAndRun runtime routes (makeGetRequest "/b")

                        match r.Body with
                        | ResponseBody.Text t -> Expect.equal t "B" "Combined"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimesSequenced "( => ) - creates a route from a pattern and a handler" (fun runtime ->
                        let routes = RoutesOperators.(=>) (Route.get "/op") (HttpHandler.text "via op")

                        let r = dispatchAndRun runtime routes (makeGetRequest "/op")

                        match r.Body with
                        | ResponseBody.Text t -> Expect.equal t "via op" "Operator route"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "TypedRoutes"
                [

                    testAllRuntimesSequenced "getInt - extracts an integer parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.getInt [ "users" ] [] (fun id -> HttpHandler.text $"id={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/99")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "id=99" "Int param"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimesSequenced "postInt - extracts an integer parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.postInt [ "users" ] [] (fun id -> HttpHandler.text $"posted={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.POST "/users/7")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "posted=7" "postInt must bind the id"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimesSequenced "putInt - extracts an integer parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.putInt [ "users" ] [] (fun id -> HttpHandler.text $"put={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.PUT "/users/12")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "put=12" "putInt must bind the id"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimesSequenced "deleteInt - extracts an integer parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.deleteInt [ "users" ] [] (fun id -> HttpHandler.text $"deleted={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.DELETE "/users/3")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "deleted=3" "deleteInt must bind the id"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimesSequenced
                        "TypedRoutes - an int route does not match a non-integer segment"
                        (fun runtime ->
                            let reached = ref false
                            let routes =
                                TypedRoutes.getInt [ "users" ] [] (fun id ->
                                    fun request ->
                                        reached.Value <- true
                                        HttpHandler.text $"id={id}" request)

                            let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/abc")

                            Expect.isFalse reached.Value "A non-integer segment must never reach the typed handler"
                            Expect.equal resp.Status HttpStatusCode.NotFound "It must fall through to not-found")

                    testAllRuntimesSequenced "TypedRoutes - an int route does not match a wrong method" (fun runtime ->
                        let routes =
                            TypedRoutes.postInt [ "users" ] [] (fun id -> HttpHandler.text $"posted={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/7")

                        Expect.notEqual resp.Status HttpStatusCode.OK "A GET must not reach a POST-only typed route")

                    testAllRuntimesSequenced "getString - extracts a string parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.getString [ "items" ] [] (fun name -> HttpHandler.text $"name={name}")

                        let resp =
                            dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/items/widget")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "name=widget" "String param"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "SimpleRoutes"
                [

                    testAllRuntimesSequenced "get/post/put/delete/patch - create routes" (fun runtime ->
                        let routes =
                            get "/g" (HttpHandler.text "G")
                            |> Routes.combine (post "/p" (HttpHandler.text "P"))
                            |> Routes.combine (put "/u" (HttpHandler.text "U"))
                            |> Routes.combine (delete "/d" (HttpHandler.text "D"))
                            |> Routes.combine (patch "/a" (HttpHandler.text "A"))

                        let rG = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/g")
                        let rP = dispatchAndRun runtime routes (makeRequest HttpMethod.POST "/p")
                        let rU = dispatchAndRun runtime routes (makeRequest HttpMethod.PUT "/u")
                        let rD = dispatchAndRun runtime routes (makeRequest HttpMethod.DELETE "/d")
                        let rA = dispatchAndRun runtime routes (makeRequest HttpMethod.PATCH "/a")

                        Expect.equal rG.Status HttpStatusCode.OK "GET"
                        Expect.equal rP.Status HttpStatusCode.OK "POST"
                        Expect.equal rU.Status HttpStatusCode.OK "PUT"
                        Expect.equal rD.Status HttpStatusCode.OK "DELETE"
                        Expect.equal rA.Status HttpStatusCode.OK "PATCH")
                ]

            testList
                "RouteBuilder CE"
                [
                    testAllRuntimesSequenced "routes - yields an empty collection" (fun runtime ->
                        let collected = RouteBuilder.routes { () }
                        let request = makeGetRequest "/anything"

                        let response = dispatchAndRun runtime collected request

                        Expect.equal response.Status HttpStatusCode.NotFound "An empty CE must route nothing")

                    testAllRuntimesSequenced "routes - combines yielded collections" (fun runtime ->
                        let collected =
                            RouteBuilder.routes {
                                yield SimpleRoutes.get "/a" (HttpHandler.text "A")
                                yield SimpleRoutes.get "/b" (HttpHandler.text "B")
                            }

                        let a = dispatchAndRun runtime collected (makeGetRequest "/a")
                        let b = dispatchAndRun runtime collected (makeGetRequest "/b")

                        match a.Body, b.Body with
                        | ResponseBody.Text ta, ResponseBody.Text tb ->
                            Expect.equal ta "A" "First yielded route must be reachable"
                            Expect.equal tb "B" "Second yielded route must be reachable"
                        | other -> failtest $"Expected text bodies from both routes, got {other}")

                    testAllRuntimesSequenced "routes - preserves first-match order" (fun runtime ->
                        let collected =
                            RouteBuilder.routes {
                                yield SimpleRoutes.get "/dup" (HttpHandler.text "first")
                                yield SimpleRoutes.get "/dup" (HttpHandler.text "second")
                            }

                        let response = dispatchAndRun runtime collected (makeGetRequest "/dup")

                        match response.Body with
                        | ResponseBody.Text t -> Expect.equal t "first" "The earlier yielded route must win"
                        | other -> failtest $"Expected a text body, got {other}")
                ]
        ]
