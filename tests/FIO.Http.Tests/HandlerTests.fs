module FIO.Http.Tests.HandlerTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http

open Expecto

[<Tests>]
let handlerTests =
    testList
        "HttpHandler"
        [
            // ─── Factories ─────────────────────────────────────────

            testAllRuntimes "succeed returns constant response" (fun runtime ->
                let handler = HttpHandler.succeed Response.ok

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.OK "200")

            testAllRuntimes "fail returns error" (fun runtime ->
                let handler = HttpHandler.fail (exn "boom")

                let error = runtime.Run(handler (makeGetRequest "/")).UnsafeError()

                Expect.equal error.Message "boom" "Error message")

            testAllRuntimes "fromFIO runs effect ignoring request" (fun runtime ->
                let handler = HttpHandler.fromFIO (FIO.succeed (Response.okText "from effect"))

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.OK "200")

            testAllRuntimes "fromFunc wraps pure function" (fun runtime ->
                let handler = HttpHandler.fromFunc (fun req -> Response.okText req.Path)

                let resp = runtime.Run(handler (makeGetRequest "/test")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "/test" "Body from path"
                | _ -> failtest "Expected Text body")

            // ─── Response builders ─────────────────────────────────────────

            testAllRuntimes "ok returns 200" (fun runtime ->
                let resp = runtime.Run(HttpHandler.ok (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.OK "200")

            testAllRuntimes "okJson returns 200 with JSON body" (fun runtime ->
                let handler = HttpHandler.okJson {| msg = "hi" |}

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.OK "200"

                match resp.Body with
                | ResponseBody.Json _ -> ()
                | _ -> failtest "Expected Json body")

            testAllRuntimes "text returns 200 with text" (fun runtime ->
                let handler = HttpHandler.text "hello"

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "hello" "Body"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "noContent returns 204" (fun runtime ->
                let resp = runtime.Run(HttpHandler.noContent (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.NoContent "204")

            testAllRuntimes "notFound returns 404" (fun runtime ->
                let resp = runtime.Run(HttpHandler.notFound (makeGetRequest "/")).UnsafeSuccess()
                Expect.equal resp.Status HttpStatusCode.NotFound "404")

            testAllRuntimes "badRequest returns 400" (fun runtime ->
                let resp = runtime.Run(HttpHandler.badRequest (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.BadRequest "400")

            testAllRuntimes "serverError returns 500" (fun runtime ->
                let resp = runtime.Run(HttpHandler.serverError (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.InternalServerError "500")

            testAllRuntimes "unauthorized returns 401" (fun runtime ->
                let resp =
                    runtime.Run(HttpHandler.unauthorized (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.Unauthorized "401")

            testAllRuntimes "forbidden returns 403" (fun runtime ->
                let resp = runtime.Run(HttpHandler.forbidden (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.Forbidden "403")

            testAllRuntimes "redirect permanent returns 301 with Location" (fun runtime ->
                let handler = HttpHandler.redirect "/new" true

                let resp = runtime.Run(handler (makeGetRequest "/old")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.MovedPermanently "301"
                Expect.equal (HttpResponse.header "Location" resp) (Some "/new") "Location")

            testAllRuntimes "redirect temporary returns 302 with Location" (fun runtime ->
                let handler = HttpHandler.redirect "/temp" false

                let resp = runtime.Run(handler (makeGetRequest "/old")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.Found "302")

            // ─── Combinators ─────────────────────────────────────────

            testAllRuntimes "map transforms response" (fun runtime ->
                let handler =
                    HttpHandler.text "hello"
                    |> HttpHandler.map (fun resp -> HttpResponse.withHeader "X-Mapped" "true" resp)

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal (HttpResponse.header "X-Mapped" resp) (Some "true") "Mapped header")

            testAllRuntimes "bind chains to new handler" (fun runtime ->
                let handler =
                    HttpHandler.text "step1" |> HttpHandler.bind (fun _ -> HttpHandler.text "step2")

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "step2" "Chained result"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "orElse falls back on failure" (fun runtime ->
                let failing = fun _ -> FIO.fail (exn "fail")
                let fallback = HttpHandler.text "recovered"
                let handler = failing |> HttpHandler.orElse fallback

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "recovered" "Fallback"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "mapError transforms error type" (fun runtime ->
                let handler =
                    HttpHandler.fail "original"
                    |> HttpHandler.mapError (fun (s: string) -> s + " mapped")

                let error = runtime.Run(handler (makeGetRequest "/")).UnsafeError()

                Expect.equal error "original mapped" "Mapped error")

            testAllRuntimes "tap runs side effect without changing response" (fun runtime ->
                let mutable tapped = false

                let handler =
                    HttpHandler.text "hello"
                    |> HttpHandler.tap (fun _ -> FIO.attempt (fun () -> tapped <- true) id)

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.OK "200"
                Expect.isTrue tapped "Side effect ran")

            // ─── Control flow ─────────────────────────────────────────

            testAllRuntimes "when' runs handler when predicate is true" (fun runtime ->
                let handler =
                    HttpHandler.when'
                        (fun req -> req.Method = HttpMethod.GET)
                        (HttpHandler.text "matched")
                        Response.notFound

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "matched" "Matched"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "when' returns fallback when predicate is false" (fun runtime ->
                let handler =
                    HttpHandler.when'
                        (fun req -> req.Method = HttpMethod.POST)
                        (HttpHandler.text "matched")
                        Response.notFound

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                Expect.equal resp.Status HttpStatusCode.NotFound "Fallback")

            testAllRuntimes "ifElse runs correct branch" (fun runtime ->
                let handler =
                    HttpHandler.ifElse
                        (fun req -> req.Method = HttpMethod.GET)
                        (HttpHandler.text "get")
                        (HttpHandler.text "other")

                let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "get" "GET branch"
                | _ -> failtest "Expected Text body")

            // ─── JSON parsing ─────────────────────────────────────────

            testAllRuntimes "parseJsonBody parses valid JSON" (fun runtime ->
                let req =
                    HttpRequest.create HttpMethod.POST "/data"
                    |> HttpRequest.withBody (RequestBody.Text """{"Id":1,"Text":"hello"}""")

                let parser = HttpHandler.parseJsonBody None
                let msg = runtime.Run(parser req).UnsafeSuccess()

                Expect.equal msg.Id 1 "Id"
                Expect.equal msg.Text "hello" "Text")

            testAllRuntimes "parseJsonBody fails on invalid JSON" (fun runtime ->
                let req =
                    HttpRequest.create HttpMethod.POST "/data"
                    |> HttpRequest.withBody (RequestBody.Text "not json")

                let parser = HttpHandler.parseJsonBody None
                let fiber = runtime.Run(parser req)

                match fiber.UnsafeResult() with
                | Failed _ -> ()
                | other -> failtest $"Expected failure but got {other}")

            // ─── Reader / Local ─────────────────────────────────────────

            testAllRuntimes "local modifies request for inner handler" (fun runtime ->
                let inner = fun req -> FIO.succeed (Response.okText req.Path)
                let handler = HttpHandler.local (fun req -> { req with Path = "/modified" }) inner

                let resp = runtime.Run(handler (makeGetRequest "/original")).UnsafeSuccess()

                match resp.Body with
                | ResponseBody.Text t -> Expect.equal t "/modified" "Modified path"
                | _ -> failtest "Expected Text body")

            testAllRuntimes "asks extracts value from request" (fun runtime ->
                let extractor = HttpHandler.asks (fun req -> req.Path)

                let path = runtime.Run(extractor (makeGetRequest "/hello")).UnsafeSuccess()

                Expect.equal path "/hello" "Extracted path")

            testList
                "HttpHandlerOperators"
                [

                    testAllRuntimes "<!> maps response" (fun runtime ->
                        let handler =
                            (fun resp -> HttpResponse.withHeader "X-Op" "true" resp)
                            |> HttpHandlerOperators.(<!>)
                            <| HttpHandler.ok

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        Expect.equal (HttpResponse.header "X-Op" resp) (Some "true") "Operator map")

                    testAllRuntimes "<|> falls back on failure" (fun runtime ->
                        let failing = fun _ -> FIO.fail (exn "fail")
                        let handler = HttpHandlerOperators.(<|>) failing (HttpHandler.text "ok")

                        let resp = runtime.Run(handler (makeGetRequest "/")).UnsafeSuccess()

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "ok" "Fallback via operator"
                        | _ -> failtest "Expected Text body")
                ]
        ]
