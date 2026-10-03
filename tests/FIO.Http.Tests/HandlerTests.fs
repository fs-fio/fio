module FIO.Http.Tests.HandlerTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Runtime

open Expecto

let private runHandler (runtime: FIORuntime) (handler: HttpHandler<exn>) =
    runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

[<Tests>]
let handlerTests =
    testList
        "HttpHandler"
        [
            testList
                "Factories"
                [
                    testAllRuntimes "succeed - returns a constant response" (fun runtime ->
                        let handler = HttpHandler.succeed Response.ok

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.OK "200")

                    testAllRuntimes "fail - returns an error" (fun runtime ->
                        let handler = HttpHandler.fail (exn "boom")

                        let error = runtime.Run(handler (makeGetRequest "/")).UnsafeError()

                        Expect.equal error.Message "boom" "Error message")

                    testAllRuntimes "fromFIO - runs the effect ignoring the request" (fun runtime ->
                        let handler = HttpHandler.fromFIO (FIO.succeed (Response.okText "from effect"))

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.OK "200")

                    testAllRuntimes "fromFunc - wraps a pure function" (fun runtime ->
                        let handler = HttpHandler.fromFunc (fun req -> Response.okText req.Path)

                        let resp = runtime.Run(handler (makeGetRequest "/test")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "/test" "Body from path"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "Response builders"
                [
                    testAllRuntimes "ok - returns 200" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.ok (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.OK "200")

                    testAllRuntimes "okJson - returns 200 with a JSON body" (fun runtime ->
                        let handler = HttpHandler.okJson {| msg = "hi" |}

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.OK "200"
                        match resp.Body with
                        | ResponseBody.Json _ -> ()
                        | _ -> failtest "Expected Json body")

                    testAllRuntimes "text - returns 200 with a text body" (fun runtime ->
                        let handler = HttpHandler.text "hello"

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "hello" "Body"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "noContent - returns 204" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.noContent (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.NoContent "204")

                    testAllRuntimes "notFound - returns 404" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.notFound (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.NotFound "404")

                    testAllRuntimes "badRequest - returns 400" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.badRequest (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.BadRequest "400")

                    testAllRuntimes "serverError - returns 500" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.serverError (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.InternalServerError "500")

                    testAllRuntimes "unauthorized - returns 401" (fun runtime ->
                        let resp =
                            runtime.Run(HttpHandler.unauthorized (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

                    testAllRuntimes "forbidden - returns 403" (fun runtime ->
                        let resp = runtime.Run(HttpHandler.forbidden (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.Forbidden "403")

                    testAllRuntimes "redirect - permanent returns 301 with Location" (fun runtime ->
                        let handler = HttpHandler.redirect "/new" true

                        let resp = runtime.Run(handler (makeGetRequest "/old")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.MovedPermanently "301"
                        Expect.equal (HttpResponse.header "Location" resp) (Some "/new") "Location")

                    testAllRuntimes "redirect - temporary returns 302 with Location" (fun runtime ->
                        let handler = HttpHandler.redirect "/temp" false

                        let resp = runtime.Run(handler (makeGetRequest "/old")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.Found "302")
                ]

            testList
                "Combinators"
                [
                    testAllRuntimes "map - transforms the response" (fun runtime ->
                        let handler =
                            HttpHandler.text "hello"
                            |> HttpHandler.map (fun resp -> HttpResponse.withHeader "X-Mapped" "true" resp)

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal (HttpResponse.header "X-Mapped" resp) (Some "true") "Mapped header")

                    testAllRuntimes "bind - chains to a new handler" (fun runtime ->
                        let handler =
                            HttpHandler.text "step1" |> HttpHandler.bind (fun _ -> HttpHandler.text "step2")

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "step2" "Chained result"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "orElse - falls back on failure" (fun runtime ->
                        let failing = fun _ -> FIO.fail (exn "fail")
                        let fallback = HttpHandler.text "recovered"
                        let handler = failing |> HttpHandler.orElse fallback

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "recovered" "Fallback"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "mapError - transforms the error type" (fun runtime ->
                        let handler =
                            HttpHandler.fail "original"
                            |> HttpHandler.mapError (fun (s: string) -> s + " mapped")

                        let error = runtime.Run(handler (makeGetRequest "/")).UnsafeError()

                        Expect.equal error "original mapped" "Mapped error")

                    testAllRuntimes "tap - runs a side effect without changing the response" (fun runtime ->
                        let mutable tapped = false
                        let handler =
                            HttpHandler.text "hello"
                            |> HttpHandler.tap (fun _ -> FIO.attempt (fun () -> tapped <- true) id)

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.OK "200"
                        Expect.isTrue tapped "Side effect ran")
                ]

            testList
                "Control flow"
                [
                    testAllRuntimes "when' - runs the handler when the predicate is true" (fun runtime ->
                        let handler =
                            HttpHandler.when'
                                (fun req -> req.Method = HttpMethod.GET)
                                (HttpHandler.text "matched")
                                Response.notFound

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "matched" "Matched"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "when' - returns the fallback when the predicate is false" (fun runtime ->
                        let handler =
                            HttpHandler.when'
                                (fun req -> req.Method = HttpMethod.POST)
                                (HttpHandler.text "matched")
                                Response.notFound

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal resp.Status HttpStatusCode.NotFound "Fallback")

                    testAllRuntimes "ifElse - runs the branch the predicate selects" (fun runtime ->
                        let handler =
                            HttpHandler.ifElse
                                (fun req -> req.Method = HttpMethod.GET)
                                (HttpHandler.text "get")
                                (HttpHandler.text "other")

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "get" "GET branch"
                        | _ -> failtest "Expected Text body")
                ]

            testList
                "JSON parsing"
                [
                    testAllRuntimes "parseJsonBody - parses valid JSON" (fun runtime ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text """{"Id":1,"Text":"hello"}""")
                        let parser = HttpHandler.parseJsonBody None

                        let msg = runtime.Run(parser req).UnsafeSuccess()

                        Expect.equal msg.Id 1 "Id"
                        Expect.equal msg.Text "hello" "Text")

                    testAllRuntimes "parseJsonBody - fails on invalid JSON" (fun runtime ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "not json")
                        let parser = HttpHandler.parseJsonBody None

                        let result = runtime.Run(parser req).UnsafeResult()

                        match result with
                        | Failed _ -> ()
                        | other -> failtest $"Expected failure but got {other}")

                    testAllRuntimes "parseJsonBody - fails on an empty body" (fun runtime ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "   ")

                        let result = runtime.Run(HttpHandler.parseJsonBody<TestMessage> None req).UnsafeResult()

                        match result with
                        | Failed error -> Expect.stringContains error.Message "Request body is empty" "An empty body must be named as such"
                        | other -> failtest $"Expected failure but got {other}")

                    testAllRuntimes "parseJsonBody - fails on a JSON null" (fun runtime ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "null")

                        let result = runtime.Run(HttpHandler.parseJsonBody<TestMessage> None req).UnsafeResult()

                        match result with
                        | Failed error -> Expect.stringContains error.Message "deserialized to null" "A JSON null must be rejected"
                        | other -> failtest $"Expected failure but got {other}")

                    testAllRuntimes "jsonBody - maps a parse failure through onError" (fun runtime ->
                        let handlerRan = ref false
                        let received = ref None
                        let handler =
                            HttpHandler.jsonBody<TestMessage, string>
                                (fun _ ->
                                    handlerRan.Value <- true
                                    FIO.succeed Response.ok)
                                (fun ex ->
                                    received.Value <- Some ex
                                    "mapped")
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "not json")

                        let result = runtime.Run(handler req).UnsafeResult()

                        match result with
                        | Failed error -> Expect.equal error "mapped" "The failure must be the one onError produced"
                        | other -> failtest $"Expected failure but got {other}"
                        match received.Value with
                        | Some(:? System.Text.Json.JsonException) -> ()
                        | other -> failtest $"onError must receive the parse failure, got {other}"
                        Expect.isFalse handlerRan.Value "The handler must not run when the body does not parse")
                ]

            testList
                "Reader / Local"
                [
                    testAllRuntimes "local - modifies the request for the inner handler" (fun runtime ->
                        let inner = fun req -> FIO.succeed (Response.okText req.Path)
                        let handler = HttpHandler.local (fun req -> { req with Path = "/modified" }) inner

                        let resp = runtime.Run(handler (makeGetRequest "/original")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "/modified" "Modified path"
                        | _ -> failtest "Expected Text body")

                    testAllRuntimes "asks - extracts a value from the request" (fun runtime ->
                        let extractor = HttpHandler.asks (fun req -> req.Path)

                        let path = runtime.Run(extractor (makeGetRequest "/hello")).UnsafeSuccess()

                        Expect.equal path "/hello" "Extracted path")

                    testList
                        "HttpHandlerOperators"
                        [

                            testAllRuntimes "( <!> ) - maps the response" (fun runtime ->
                                let handler =
                                    (fun resp -> HttpResponse.withHeader "X-Op" "true" resp)
                                    |> HttpHandlerOperators.(<!>)
                                    <| HttpHandler.ok

                                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                                Expect.equal (HttpResponse.header "X-Op" resp) (Some "true") "Operator map")

                            testAllRuntimes "( <|> ) - falls back on failure" (fun runtime ->
                                let failing = fun _ -> FIO.fail (exn "fail")
                                let handler = HttpHandlerOperators.(<|>) failing (HttpHandler.text "ok")

                                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                                match resp.Body with
                                | ResponseBody.Text t -> Expect.equal t "ok" "Fallback via operator"
                                | _ -> failtest "Expected Text body")
                        ]
                ]

            testList
                "Status-only handlers"
                [
                    testAllRuntimes "ok - returns 200 with an empty body" (fun runtime ->
                        let response = runHandler runtime HttpHandler.ok

                        Expect.equal response.Status HttpStatusCode.OK "ok must be 200"
                        Expect.equal response.Body ResponseBody.Empty "ok must carry no body")

                    testAllRuntimes "noContent - returns 204 with an empty body" (fun runtime ->
                        let response = runHandler runtime HttpHandler.noContent

                        Expect.equal response.Status HttpStatusCode.NoContent "noContent must be 204"
                        Expect.equal response.Body ResponseBody.Empty "204 must carry no body")
                ]

            testList
                "Body-carrying error handlers"
                [
                    testAllRuntimes "notFoundText - carries the message" (fun runtime ->
                        let response = runHandler runtime (HttpHandler.notFoundText "no such thing")

                        Expect.equal response.Status HttpStatusCode.NotFound "404"
                        match response.Body with
                        | ResponseBody.Text t -> Expect.equal t "no such thing" "Message must reach the body"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimes "badRequestText - carries the message" (fun runtime ->
                        let response = runHandler runtime (HttpHandler.badRequestText "bad input")

                        Expect.equal response.Status HttpStatusCode.BadRequest "400"
                        match response.Body with
                        | ResponseBody.Text t -> Expect.equal t "bad input" "Message must reach the body"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimes "serverErrorText - carries the message" (fun runtime ->
                        let response = runHandler runtime (HttpHandler.serverErrorText "it broke")

                        Expect.equal response.Status HttpStatusCode.InternalServerError "500"
                        match response.Body with
                        | ResponseBody.Text t -> Expect.equal t "it broke" "Message must reach the body"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimes "html - sets an HTML content type" (fun runtime ->
                        let response = runHandler runtime (HttpHandler.html "<h1>hi</h1>")

                        Expect.equal response.Status HttpStatusCode.OK "200"
                        match response.Body with
                        | ResponseBody.Text t -> Expect.equal t "<h1>hi</h1>" "HTML must reach the body"
                        | other -> failtest $"Expected a text body, got {other}")

                    testAllRuntimes "bytes - carries the payload and content type" (fun runtime ->
                        let payload = [| 7uy; 8uy; 9uy |]

                        let response = runHandler runtime (HttpHandler.bytes payload "application/octet-stream")

                        Expect.equal response.Status HttpStatusCode.OK "200"
                        match response.Body with
                        | ResponseBody.Bytes b -> Expect.sequenceEqual b payload "Payload must round-trip"
                        | other -> failtest $"Expected a byte body, got {other}")
                ]
        ]
