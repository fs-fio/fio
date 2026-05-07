/// <summary>Provides tests for HTTP route registration and request dispatch.</summary>
module FIO.Http.Tests.RoutesTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http

open Expecto

[<Tests>]
let routesTests =
    testList
        "Routes"
        [

            testAllRuntimes "empty dispatches to 404" (fun runtime ->
                let resp = dispatchAndRun runtime Routes.empty (makeGetRequest "/anything")

                Expect.equal resp.Status HttpStatusCode.NotFound "404")

            testAllRuntimes "route creates dispatchable single route" (fun runtime ->
                let routes = Routes.route (Route.get "/hello") (HttpHandler.text "world")
                let resp = dispatchAndRun runtime routes (makeGetRequest "/hello")

                Expect.equal resp.Status HttpStatusCode.OK "200"

                match resp.Body with
                | Text t -> Expect.equal t "world" "Body"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "single creates parameterized route" (fun runtime ->
                let pattern = RoutePattern.get (RoutePath.withInt [ "users" ] [])

                let routes =
                    Routes.single pattern (fun params' ->
                        match params' with
                        | [ :? int as id ] -> HttpHandler.text $"user-{id}"
                        | _ -> HttpHandler.badRequest)

                let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/42")

                match resp.Body with
                | Text t -> Expect.equal t "user-42" "Param extracted"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "combine merges two route sets" (fun runtime ->
                let r1 = Routes.route (Route.get "/a") (HttpHandler.text "A")
                let r2 = Routes.route (Route.get "/b") (HttpHandler.text "B")
                let combined = Routes.combine r1 r2

                let respA = dispatchAndRun runtime combined (makeGetRequest "/a")
                let respB = dispatchAndRun runtime combined (makeGetRequest "/b")

                match respA.Body, respB.Body with
                | Text a, Text b ->
                    Expect.equal a "A" "Route A"
                    Expect.equal b "B" "Route B"
                | _ -> failtest "Expected Text bodies")

            testAllRuntimes "dispatch matches exact route via index" (fun runtime ->
                let routes = Routes.route (Route.get "/exact") (HttpHandler.text "found")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/exact")

                Expect.equal resp.Status HttpStatusCode.OK "Exact match")

            testAllRuntimes "dispatch normalizes trailing slashes" (fun runtime ->
                let routes = Routes.route (Route.get "/path") (HttpHandler.text "ok")

                let req =
                    { HttpRequest.create HttpMethod.GET "/path/" with
                        PathSegments = [ "path" ]
                    }

                let resp = dispatchAndRun runtime routes req

                Expect.equal resp.Status HttpStatusCode.OK "Trailing slash normalized")

            testAllRuntimes "dispatch returns notFound for unmatched path" (fun runtime ->
                let routes = Routes.route (Route.get "/exists") (HttpHandler.text "here")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/missing")

                Expect.equal resp.Status HttpStatusCode.NotFound "404")

            testAllRuntimes "dispatch returns notFound for wrong method" (fun runtime ->
                let routes = Routes.route (Route.get "/only-get") (HttpHandler.text "ok")

                let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.POST "/only-get")

                Expect.equal resp.Status HttpStatusCode.NotFound "Wrong method -> 404")

            testAllRuntimes "withNotFound changes fallback handler" (fun runtime ->
                let routes = Routes.empty |> Routes.withNotFound (HttpHandler.text "custom 404")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/anything")

                match resp.Body with
                | Text t -> Expect.equal t "custom 404" "Custom not found"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "add appends route" (fun runtime ->
                let routes =
                    Routes.empty |> Routes.addRoute (Route.get "/added") (HttpHandler.text "added")

                let resp = dispatchAndRun runtime routes (makeGetRequest "/added")

                match resp.Body with
                | Text t -> Expect.equal t "added" "Added route"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "ofList creates from pattern-handler pairs" (fun runtime ->
                let routes =
                    Routes.ofList
                        [
                            Route.get "/one", HttpHandler.text "1"
                            Route.get "/two", HttpHandler.text "2"
                        ]

                let r1 = dispatchAndRun runtime routes (makeGetRequest "/one")
                let r2 = dispatchAndRun runtime routes (makeGetRequest "/two")

                match r1.Body, r2.Body with
                | Text a, Text b ->
                    Expect.equal a "1" "First"
                    Expect.equal b "2" "Second"
                | _ -> failtest "Expected Text bodies")

            testAllRuntimes "map transforms all handlers" (fun runtime ->
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

                    testAllRuntimes "++ combines routes" (fun runtime ->
                        let routes =
                            RoutesOperators.(++)
                                (Routes.route (Route.get "/a") (HttpHandler.text "A"))
                                (Routes.route (Route.get "/b") (HttpHandler.text "B"))

                        let r = dispatchAndRun runtime routes (makeGetRequest "/b")

                        match r.Body with
                        | Text t -> Expect.equal t "B" "Combined"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "=> creates route from pattern and handler" (fun runtime ->
                        let routes = RoutesOperators.(=>) (Route.get "/op") (HttpHandler.text "via op")

                        let r = dispatchAndRun runtime routes (makeGetRequest "/op")

                        match r.Body with
                        | Text t -> Expect.equal t "via op" "Operator route"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "TypedRoutes"
                [

                    testAllRuntimes "getInt extracts integer parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.getInt [ "users" ] [] (fun id -> HttpHandler.text $"id={id}")

                        let resp = dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/users/99")

                        match resp.Body with
                        | Text t -> Expect.equal t "id=99" "Int param"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "getString extracts string parameter" (fun runtime ->
                        let routes =
                            TypedRoutes.getString [ "items" ] [] (fun name -> HttpHandler.text $"name={name}")

                        let resp =
                            dispatchAndRun runtime routes (makeRequest HttpMethod.GET "/items/widget")

                        match resp.Body with
                        | Text t -> Expect.equal t "name=widget" "String param"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "SimpleRoutes"
                [

                    testAllRuntimes "get/post/put/delete/patch create routes" (fun runtime ->
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
        ]
