/// <summary>Provides tests for HTTP response builder functions and status codes.</summary>
module FIO.Http.Tests.ResponseTests

open FIO.Http

open System.IO

open Expecto

[<Tests>]
let responseTests =
    testList
        "Response"
        [

            testList
                "2xx"
                [

                    testCase "ok returns 200"
                    <| fun () -> Expect.equal Response.ok.Status HttpStatusCode.OK "200"

                    testCase "okText returns 200 with text body and Content-Type"
                    <| fun () ->
                        let resp = Response.okText "hello"

                        Expect.equal resp.Status HttpStatusCode.OK "200"

                        Expect.equal
                            (HttpResponse.header "Content-Type" resp)
                            (Some "text/plain; charset=utf-8")
                            "Content-Type"

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "hello" "Body"
                        | _ -> failtest "Expected Text body"

                    testCase "okJson returns 200 with JSON body and Content-Type"
                    <| fun () ->
                        let resp = Response.okJson {| name = "test" |}

                        Expect.equal resp.Status HttpStatusCode.OK "200"

                        Expect.equal
                            (HttpResponse.header "Content-Type" resp)
                            (Some "application/json; charset=utf-8")
                            "Content-Type"

                        match resp.Body with
                        | ResponseBody.Json _ -> ()
                        | _ -> failtest "Expected Json body"

                    testCase "okHtml returns 200 with HTML Content-Type"
                    <| fun () ->
                        let resp = Response.okHtml "<h1>Hi</h1>"

                        Expect.equal resp.Status HttpStatusCode.OK "200"

                        Expect.equal
                            (HttpResponse.header "Content-Type" resp)
                            (Some "text/html; charset=utf-8")
                            "Content-Type"

                    testCase "okBytes returns 200 with byte body"
                    <| fun () ->
                        let resp = Response.okBytes [| 1uy; 2uy |] "application/octet-stream"

                        Expect.equal resp.Status HttpStatusCode.OK "200"

                        match resp.Body with
                        | ResponseBody.Bytes b -> Expect.equal b.Length 2 "2 bytes"
                        | _ -> failtest "Expected Bytes body"

                    testCase "okStream returns 200 with stream body"
                    <| fun () ->
                        use ms = new MemoryStream()
                        let resp = Response.okStream ms (Some 0L) "application/octet-stream"

                        Expect.equal resp.Status HttpStatusCode.OK "200"

                        match resp.Body with
                        | ResponseBody.Stream _ -> ()
                        | _ -> failtest "Expected Stream body"

                    testCase "okStream throws for null stream"
                    <| fun () ->
                        Expect.throws
                            (fun () -> Response.okStream null None "application/octet-stream" |> ignore)
                            "Null stream"

                    testCase "created returns 201"
                    <| fun () -> Expect.equal Response.created.Status HttpStatusCode.Created "201"

                    testCase "createdAt returns 201 with Location header"
                    <| fun () ->
                        let resp = Response.createdAt "/users/42"

                        Expect.equal resp.Status HttpStatusCode.Created "201"
                        Expect.equal (HttpResponse.header "Location" resp) (Some "/users/42") "Location"

                    testCase "createdJson returns 201 with JSON body"
                    <| fun () ->
                        let resp = Response.createdJson {| id = 1 |}

                        Expect.equal resp.Status HttpStatusCode.Created "201"

                        match resp.Body with
                        | ResponseBody.Json _ -> ()
                        | _ -> failtest "Expected Json body"

                    testCase "accepted returns 202"
                    <| fun () -> Expect.equal Response.accepted.Status HttpStatusCode.Accepted "202"

                    testCase "noContent returns 204"
                    <| fun () -> Expect.equal Response.noContent.Status HttpStatusCode.NoContent "204"
                ]

            testList
                "3xx"
                [

                    testCase "movedPermanently returns 301 with Location"
                    <| fun () ->
                        let resp = Response.movedPermanently "/new"

                        Expect.equal resp.Status HttpStatusCode.MovedPermanently "301"
                        Expect.equal (HttpResponse.header "Location" resp) (Some "/new") "Location"

                    testCase "found returns 302 with Location"
                    <| fun () ->
                        let resp = Response.found "/temp"

                        Expect.equal resp.Status HttpStatusCode.Found "302"
                        Expect.equal (HttpResponse.header "Location" resp) (Some "/temp") "Location"

                    testCase "seeOther returns 303 with Location"
                    <| fun () ->
                        let resp = Response.seeOther "/other"

                        Expect.equal resp.Status HttpStatusCode.SeeOther "303"

                    testCase "notModified returns 304"
                    <| fun () ->

                        Expect.equal Response.notModified.Status HttpStatusCode.NotModified "304"

                    testCase "temporaryRedirect returns 307 with Location"
                    <| fun () ->
                        let resp = Response.temporaryRedirect "/temp"

                        Expect.equal resp.Status HttpStatusCode.TemporaryRedirect "307"

                    testCase "permanentRedirect returns 308 with Location"
                    <| fun () ->
                        let resp = Response.permanentRedirect "/perm"

                        Expect.equal resp.Status HttpStatusCode.PermanentRedirect "308"
                ]

            testList
                "4xx"
                [

                    testCase "badRequest returns 400"
                    <| fun () -> Expect.equal Response.badRequest.Status HttpStatusCode.BadRequest "400"

                    testCase "badRequestText returns 400 with text"
                    <| fun () ->
                        let resp = Response.badRequestText "invalid"

                        Expect.equal resp.Status HttpStatusCode.BadRequest "400"

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "invalid" "Body"
                        | _ -> failtest "Expected Text body"

                    testCase "unauthorized returns 401"
                    <| fun () -> Expect.equal Response.unauthorized.Status HttpStatusCode.Unauthorized "401"

                    testCase "unauthorizedWith returns 401 with WWW-Authenticate"
                    <| fun () ->
                        let resp = Response.unauthorizedWith "Bearer"
                        Expect.equal (HttpResponse.header "WWW-Authenticate" resp) (Some "Bearer") "WWW-Authenticate"

                    testCase "forbidden returns 403"
                    <| fun () -> Expect.equal Response.forbidden.Status HttpStatusCode.Forbidden "403"

                    testCase "notFound returns 404"
                    <| fun () -> Expect.equal Response.notFound.Status HttpStatusCode.NotFound "404"

                    testCase "notFoundText returns 404 with text"
                    <| fun () ->
                        let resp = Response.notFoundText "not here"
                        Expect.equal resp.Status HttpStatusCode.NotFound "404"

                    testCase "methodNotAllowed returns 405 with Allow header"
                    <| fun () ->
                        let resp = Response.methodNotAllowed [ "GET"; "POST" ]

                        Expect.equal resp.Status HttpStatusCode.MethodNotAllowed "405"
                        Expect.equal (HttpResponse.header "Allow" resp) (Some "GET, POST") "Allow"

                    testCase "requestTimeout returns 408"
                    <| fun () -> Expect.equal Response.requestTimeout.Status HttpStatusCode.RequestTimeout "408"

                    testCase "conflict returns 409"
                    <| fun () -> Expect.equal Response.conflict.Status HttpStatusCode.Conflict "409"

                    testCase "tooManyRequests returns 429"
                    <| fun () -> Expect.equal Response.tooManyRequests.Status HttpStatusCode.TooManyRequests "429"

                    testCase "tooManyRequestsAfter returns 429 with Retry-After"
                    <| fun () ->
                        let resp = Response.tooManyRequestsAfter 60

                        Expect.equal resp.Status HttpStatusCode.TooManyRequests "429"
                        Expect.equal (HttpResponse.header "Retry-After" resp) (Some "60") "Retry-After"
                ]

            testList
                "5xx"
                [

                    testCase "internalServerError returns 500"
                    <| fun () ->
                        Expect.equal Response.internalServerError.Status HttpStatusCode.InternalServerError "500"

                    testCase "internalServerErrorText returns 500 with text"
                    <| fun () ->
                        let resp = Response.internalServerErrorText "oops"
                        Expect.equal resp.Status HttpStatusCode.InternalServerError "500"

                    testCase "notImplemented returns 501"
                    <| fun () -> Expect.equal Response.notImplemented.Status HttpStatusCode.NotImplemented "501"

                    testCase "badGateway returns 502"
                    <| fun () -> Expect.equal Response.badGateway.Status HttpStatusCode.BadGateway "502"

                    testCase "serviceUnavailable returns 503"
                    <| fun () -> Expect.equal Response.serviceUnavailable.Status HttpStatusCode.ServiceUnavailable "503"

                    testCase "serviceUnavailableAfter returns 503 with Retry-After"
                    <| fun () ->
                        let resp = Response.serviceUnavailableAfter 120

                        Expect.equal (HttpResponse.header "Retry-After" resp) (Some "120") "Retry-After"

                    testCase "gatewayTimeout returns 504"
                    <| fun () -> Expect.equal Response.gatewayTimeout.Status HttpStatusCode.GatewayTimeout "504"
                ]

            testList
                "Generic"
                [

                    testCase "status creates response with given code"
                    <| fun () ->
                        let resp = Response.status HttpStatusCode.ImATeapot

                        Expect.equal resp.Status HttpStatusCode.ImATeapot "418"

                    testCase "statusText creates response with code and text"
                    <| fun () ->
                        let resp = Response.statusText HttpStatusCode.OK "fine"

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "fine" "Body"
                        | _ -> failtest "Expected Text body"

                    testCase "statusJson creates response with code and JSON"
                    <| fun () ->
                        let resp = Response.statusJson HttpStatusCode.OK {| ok = true |}

                        match resp.Body with
                        | ResponseBody.Json _ -> ()
                        | _ -> failtest "Expected Json body"
                ]
        ]
