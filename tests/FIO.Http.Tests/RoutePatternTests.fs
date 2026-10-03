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

                    testCase "exact - matches the same segments"
                    <| fun () ->
                        let result =
                            RoutePath.tryMatch (RoutePath.exact [ "users"; "list" ]) [ "users"; "list" ]

                        Expect.isSome result "Should match"
                        match result with
                        | Some(params', remaining) ->
                            Expect.isEmpty params' "No params"
                            Expect.isEmpty remaining "No remaining"
                        | None -> failtest "Expected match"

                    testCase "exact - rejects different segments"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users" ]) [ "posts" ]

                        Expect.isNone result "Should not match"

                    testCase "exact - rejects a longer path"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users" ]) [ "users"; "123" ]

                        Expect.isNone result "Longer path should not match"

                    testCase "exact - rejects a shorter path"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact [ "users"; "list" ]) [ "users" ]

                        Expect.isNone result "Shorter path should not match"

                    testCase "exact - matches empty segments for the root"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.exact []) []

                        Expect.isSome result "Root should match empty"

                    testCase "prefix - matches an exact prefix"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "api" ]

                        Expect.isSome result "Exact prefix match"

                    testCase "prefix - matches and returns the remaining segments"
                    <| fun () ->
                        let result =
                            RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "api"; "v1"; "users" ]

                        match result with
                        | Some(_, remaining) -> Expect.equal remaining [ "v1"; "users" ] "Remaining segments"
                        | None -> failtest "Expected match"

                    testCase "prefix - rejects a non-matching path"
                    <| fun () ->
                        let result = RoutePath.tryMatch (RoutePath.prefix [ "api" ]) [ "web" ]

                        Expect.isNone result "Should not match"

                    testCase "fromString - parses a simple path"
                    <| fun () ->
                        let path = RoutePath.fromString "/users/list"

                        let result = RoutePath.tryMatch path [ "users"; "list" ]

                        Expect.isSome result "Should match"

                    testCase "fromString - handles the root path"
                    <| fun () ->
                        let path = RoutePath.fromString "/"

                        let result = RoutePath.tryMatch path []

                        Expect.isSome result "Root should match"

                    testCase "fromString - a parameter path rejects a different literal or a shorter path"
                    <| fun () ->
                        let path = RoutePath.fromString "/users/:id"

                        let differentLiteral = RoutePath.tryMatch path [ "posts"; "42" ]
                        let missingParameter = RoutePath.tryMatch path [ "users" ]

                        Expect.isNone differentLiteral "A different literal segment should not match"
                        Expect.isNone missingParameter "A path missing the parameter should not match"

                    testCase "withInt - matches an integer parameter"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] []

                        let result = RoutePath.tryMatch path [ "users"; "42" ]

                        match result with
                        | Some(params', _) ->
                            Expect.equal params'.Length 1 "One param"
                            Expect.equal (params'.[0] :?> int) 42 "Parsed int"
                        | None -> failtest "Expected match"

                    testCase "withInt - rejects a non-integer"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] []

                        let result = RoutePath.tryMatch path [ "users"; "abc" ]

                        Expect.isNone result "Non-integer should not match"

                    testCase "withInt - rejects a path whose leading segments differ"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] []

                        let result = RoutePath.tryMatch path [ "7"; "42" ]

                        Expect.isNone result "A different leading segment should not match"

                    testCase "withInt - rejects a path whose trailing segments differ"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] [ "posts" ]

                        let result = RoutePath.tryMatch path [ "users"; "5"; "comments" ]

                        Expect.isNone result "A different trailing segment should not match"

                    testCase "withInt - matches with segments before and after"
                    <| fun () ->
                        let path = RoutePath.withInt [ "users" ] [ "posts" ]

                        let result = RoutePath.tryMatch path [ "users"; "5"; "posts" ]

                        match result with
                        | Some(params', _) -> Expect.equal (params'.[0] :?> int) 5 "Parsed int"
                        | None -> failtest "Expected match"

                    testCase "withString - matches a string parameter"
                    <| fun () ->
                        let path = RoutePath.withString [ "files" ] []

                        let result = RoutePath.tryMatch path [ "files"; "readme.md" ]

                        match result with
                        | Some(params', _) -> Expect.equal (params'.[0] :?> string) "readme.md" "Parsed string"
                        | None -> failtest "Expected match"

                    testCase "withString - matches with segments before and after"
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

                    testCase "create - with GET and an exact path matches"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.GET "/users"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isSome result "Should match"

                    testCase "tryMatch - rejects the wrong method"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.POST "/users"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isNone result "Wrong method should not match"

                    testCase "tryMatch - rejects the wrong path"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.exact [ "users" ])
                        let req = HttpRequest.create HttpMethod.GET "/posts"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isNone result "Wrong path should not match"

                    testCase "tryMatch - rejects extra segments after a parameter"
                    <| fun () ->
                        let pattern = RoutePattern.get (RoutePath.withInt [ "users" ] [])
                        let req = HttpRequest.create HttpMethod.GET "/users/42/extra"

                        let result = RoutePattern.tryMatch pattern req

                        Expect.isNone result "Segments left over after the parameter should not match"

                    testCase "get/post/put/delete - create patterns with their methods"
                    <| fun () ->
                        let path = RoutePath.exact [ "test" ]

                        let get = RoutePattern.get path
                        let post = RoutePattern.post path
                        let put = RoutePattern.put path
                        let delete = RoutePattern.delete path
                        let patch = RoutePattern.patch path
                        let head = RoutePattern.head path
                        let options = RoutePattern.options path

                        Expect.equal get.Method HttpMethod.GET "GET"
                        Expect.equal post.Method HttpMethod.POST "POST"
                        Expect.equal put.Method HttpMethod.PUT "PUT"
                        Expect.equal delete.Method HttpMethod.DELETE "DELETE"
                        Expect.equal patch.Method HttpMethod.PATCH "PATCH"
                        Expect.equal head.Method HttpMethod.HEAD "HEAD"
                        Expect.equal options.Method HttpMethod.OPTIONS "OPTIONS"
                ]

            testList
                "Route string parsing"
                [

                    testCase "fromString - parses GET /path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/users"

                        let pattern = Route.fromString "GET /users"

                        Expect.equal pattern.Method HttpMethod.GET "GET"
                        Expect.isSome (RoutePattern.tryMatch pattern req) "Match"

                    testCase "fromString - parses a route with a parameter"
                    <| fun () ->
                        let pattern = Route.fromString "GET /users/:id"
                        let req = HttpRequest.create HttpMethod.GET "/users/hello"

                        let result = RoutePattern.tryMatch pattern req

                        match result with
                        | Some params' ->
                            Expect.equal params'.Length 1 "One param"
                            Expect.equal (params'.[0] :?> string) "hello" "String param"
                        | None -> failtest "Expected match"

                    testCase "fromString - throws for an invalid format"
                    <| fun () -> Expect.throws (fun () -> Route.fromString "INVALID" |> ignore) "Invalid format"

                    testCase "Route.get/post/put/delete - create from a string"
                    <| fun () ->
                        let gp = Route.get "/test"
                        let pp = Route.post "/test"
                        let up = Route.put "/test"
                        let dp = Route.delete "/test"

                        Expect.equal gp.Method HttpMethod.GET "GET"
                        Expect.equal pp.Method HttpMethod.POST "POST"
                        Expect.equal up.Method HttpMethod.PUT "PUT"
                        Expect.equal dp.Method HttpMethod.DELETE "DELETE"
                ]

            testList
                "RouteOperators"
                [

                    testCase "( => ) - creates a route pattern from a method and a path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/api"

                        let pattern = RouteOperators.(=>) HttpMethod.GET "/api"

                        Expect.equal pattern.Method HttpMethod.GET "GET"
                        Expect.isSome (RoutePattern.tryMatch pattern req) "Match"
                ]
        ]
