module FIO.Http.Tests.RoutePatternTests

open FIO.Http

open Expecto

[<Tests>]
let routePatternTests =
    testList
        "RoutePattern"
        [

            testList
                "RoutePath"
                [

                    testCase "exact matches same segments"
                    <| fun () ->
                        let result =
                            RoutePath.tryMatch (RoutePath.exact [ "users"; "list" ]) [ "users"; "list" ]

                        Expect.isSome result "Should match"

                        match result with
                        | Some(params', remaining) ->
                            Expect.isEmpty params' "No params"
                            Expect.isEmpty remaining "No remaining"
                        | None -> failtest "Expected match"

                    testCase "exact rejects different segments"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users" ]) [ "posts" ]

                        Expect.isNone result "Should not match"

                    testCase "exact rejects longer path"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users" ]) [ "users"; "123" ]

                        Expect.isNone result "Longer path should not match"

                    testCase "exact rejects shorter path"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users"; "list" ]) [ "users" ]

                        Expect.isNone result "Shorter path should not match"

                    testCase "exact matches empty segments for root"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact []) []

                        Expect.isSome result "Root should match empty"

                    testCase "prefix matches exact prefix"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "api" ]

                        Expect.isSome result "Exact prefix match"

                    testCase "prefix matches and returns remaining segments"
                    <| fun () ->
                        let result =
                            RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "api"; "v1"; "users" ]

                        match result with
                        | Some(_, remaining) -> Expect.equal remaining [ "v1"; "users" ] "Remaining segments"
                        | None -> failtest "Expected match"

                    testCase "prefix rejects non-matching"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "web" ]

                        Expect.isNone result "Should not match"

                    testCase "fromString parses simple path"
                    <| fun () ->
                        let path = RoutePath.fromString "/users/list"
                        let result = RoutePath.tryMatch path [ "users"; "list" ]

                        Expect.isSome result "Should match"

                    testCase "fromString handles root path"
                    <| fun () ->
                        let path = RoutePath.fromString "/"
                        let result = RoutePath.tryMatch path []

                        Expect.isSome result "Root should match"

                    testCase "withInt matches integer parameter"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] []

                        let result = RoutePath.tryMatch path [ "users"; "42" ]

                        match result with
                        | Some(params', _) ->
                            Expect.equal params'.Length 1 "One param"
                            Expect.equal (params'.[0] :?> int) 42 "Parsed int"
                        | None -> failtest "Expected match"

                    testCase "withInt rejects non-integer"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] []

                        let result = RoutePath.tryMatch path [ "users"; "abc" ]

                        Expect.isNone result "Non-integer should not match"

                    testCase "withInt matches with before and after segments"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] [ "posts" ]

                        let result = RoutePath.tryMatch path [ "users"; "5"; "posts" ]

                        match result with
                        | Some(params', _) -> Expect.equal (params'.[0] :?> int) 5 "Parsed int"
                        | None -> failtest "Expected match"

                    testCase "withString matches string parameter"
                    <| fun () ->
                        let path = RoutePath.withString [ "files" ] []

                        let result = RoutePath.tryMatch path [ "files"; "readme.md" ]

                        match result with
                        | Some(params', _) -> Expect.equal (params'.[0] :?> string) "readme.md" "Parsed string"
                        | None -> failtest "Expected match"

                    testCase "withString matches with before and after"
                    <| fun () ->
                        let path = RoutePath.withString [ "api" ] [ "details" ]

                        let result = RoutePath.tryMatch path [ "api"; "item1"; "details" ]

                        match result with
                        | Some(params', _) -> Expect.equal (params'.[0] :?> string) "item1" "Parsed string"
                        | None -> failtest "Expected match"
                ]

            testList
                "RoutePattern matching"
                [

                    testCase "create with GET and exact path matches"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.GET "/users"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isSome result "Should match"

                    testCase "tryMatch rejects wrong method"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.POST "/users"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isNone result "Wrong method should not match"

                    testCase "tryMatch rejects wrong path"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.GET "/posts"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isNone result "Wrong path should not match"

                    testCase "method-specific constructors create correct methods"
                    <| fun () ->
                        let path = RoutePath.exact [ "test" ]

                        Expect.equal (RoutePattern.get path).Method HttpMethod.GET "GET"
                        Expect.equal (RoutePattern.post path).Method HttpMethod.POST "POST"
                        Expect.equal (RoutePattern.put path).Method HttpMethod.PUT "PUT"
                        Expect.equal (RoutePattern.delete path).Method HttpMethod.DELETE "DELETE"
                        Expect.equal (RoutePattern.patch path).Method HttpMethod.PATCH "PATCH"
                        Expect.equal (RoutePattern.head path).Method HttpMethod.HEAD "HEAD"
                        Expect.equal (RoutePattern.options path).Method HttpMethod.OPTIONS "OPTIONS"
                ]

            testList
                "Route string parsing"
                [

                    testCase "fromString parses GET /path"
                    <| fun () ->
                        let pattern = Route.fromString "GET /users"

                        Expect.equal pattern.Method HttpMethod.GET "GET"

                        let req = HttpRequest.create HttpMethod.GET "/users"

                        Expect.isSome (RoutePattern.tryMatch pattern req) "Match"

                    testCase "fromString parses route with parameter"
                    <| fun () ->
                        let pattern = Route.fromString "GET /users/:id"
                        let req = HttpRequest.create HttpMethod.GET "/users/hello"

                        let result = RoutePattern.tryMatch pattern req

                        match result with
                        | Some params' ->
                            Expect.equal params'.Length 1 "One param"
                            Expect.equal (params'.[0] :?> string) "hello" "String param"
                        | None -> failtest "Expected match"

                    testCase "fromString throws for invalid format"
                    <| fun () -> Expect.throws (fun () -> Route.fromString "INVALID" |> ignore) "Invalid format"

                    testCase "Route.get/post/put/delete create from string"
                    <| fun () ->
                        let gp = Route.get "/test"
                        Expect.equal gp.Method HttpMethod.GET "GET"

                        let pp = Route.post "/test"
                        Expect.equal pp.Method HttpMethod.POST "POST"

                        let up = Route.put "/test"
                        Expect.equal up.Method HttpMethod.PUT "PUT"

                        let dp = Route.delete "/test"
                        Expect.equal dp.Method HttpMethod.DELETE "DELETE"
                ]

            testList
                "RouteOperators"
                [

                    testCase "=> creates route pattern from method and path"
                    <| fun () ->
                        let pattern = RouteOperators.(=>) HttpMethod.GET "/api"
                        Expect.equal pattern.Method HttpMethod.GET "GET"

                        let req = HttpRequest.create HttpMethod.GET "/api"
                        Expect.isSome (RoutePattern.tryMatch pattern req) "Match"
                ]
        ]
