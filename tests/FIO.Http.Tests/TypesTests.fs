/// <summary>Provides tests for HTTP domain types including HttpError, HttpMethod, and HttpStatusCode.</summary>
module FIO.Http.Tests.TypesTests

open FIO.Http

open System
open System.IO
open System.Text

open Expecto

[<Tests>]
let typesTests =
    testList
        "Types"
        [

            testList
                "HttpError"
                [

                    testCase "fromException wraps in GeneralError"
                    <| fun () ->
                        let exn = Exception "test"
                        let err = HttpError.fromException exn

                        match err with
                        | GeneralError e ->
                            Expect.isTrue (Object.ReferenceEquals(e, exn)) "Should wrap same exception reference"
                        | _ -> failtest "Expected GeneralError"

                    testCase "toException unwraps GeneralError"
                    <| fun () ->
                        let original = Exception "test"
                        let err = GeneralError original
                        let result = HttpError.toException err

                        Expect.isTrue
                            (Object.ReferenceEquals(result, original))
                            "Should return same exception reference"

                    testCase "toException creates Exception for other variants"
                    <| fun () ->
                        let err = InvalidRoute "/bad"
                        let result = HttpError.toException err

                        Expect.stringContains result.Message "/bad" "Exception message should contain error details"

                    testCase "ToString produces readable messages"
                    <| fun () ->
                        Expect.stringContains (string (InvalidRoute "/x")) "/x" "InvalidRoute"
                        Expect.stringContains (string (TimeoutError "slow")) "slow" "TimeoutError"
                        Expect.stringContains (string (ServerFailed(Exception "boom"))) "boom" "ServerFailed"
                ]

            testList
                "HttpMethod"
                [

                    testCase "ToString returns correct string for standard methods"
                    <| fun () ->
                        Expect.equal (string HttpMethod.GET) "GET" "GET"
                        Expect.equal (string HttpMethod.POST) "POST" "POST"
                        Expect.equal (string HttpMethod.PUT) "PUT" "PUT"
                        Expect.equal (string HttpMethod.DELETE) "DELETE" "DELETE"
                        Expect.equal (string HttpMethod.PATCH) "PATCH" "PATCH"
                        Expect.equal (string HttpMethod.HEAD) "HEAD" "HEAD"
                        Expect.equal (string HttpMethod.OPTIONS) "OPTIONS" "OPTIONS"
                        Expect.equal (string HttpMethod.TRACE) "TRACE" "TRACE"
                        Expect.equal (string HttpMethod.CONNECT) "CONNECT" "CONNECT"

                    testCase "fromString parses all standard methods"
                    <| fun () ->
                        Expect.equal (HttpMethod.fromString "GET") HttpMethod.GET "GET"
                        Expect.equal (HttpMethod.fromString "POST") HttpMethod.POST "POST"
                        Expect.equal (HttpMethod.fromString "PUT") HttpMethod.PUT "PUT"
                        Expect.equal (HttpMethod.fromString "DELETE") HttpMethod.DELETE "DELETE"
                        Expect.equal (HttpMethod.fromString "PATCH") HttpMethod.PATCH "PATCH"
                        Expect.equal (HttpMethod.fromString "HEAD") HttpMethod.HEAD "HEAD"
                        Expect.equal (HttpMethod.fromString "OPTIONS") HttpMethod.OPTIONS "OPTIONS"

                    testCase "fromString is case-insensitive"
                    <| fun () ->
                        Expect.equal (HttpMethod.fromString "get") HttpMethod.GET "lowercase"
                        Expect.equal (HttpMethod.fromString "Post") HttpMethod.POST "mixed case"

                    testCase "fromString returns Custom for unknown method"
                    <| fun () ->
                        match HttpMethod.fromString "PURGE" with
                        | HttpMethod.Custom s -> Expect.equal s "PURGE" "Custom method"
                        | _ -> failtest "Expected Custom"
                ]

            testList
                "RequestBody"
                [

                    testCase "Empty AsBytes returns empty array"
                    <| fun () -> Expect.equal (RequestBody.Empty.AsBytes()) Array.empty "Empty bytes"

                    testCase "Empty AsString returns empty string"
                    <| fun () -> Expect.equal (RequestBody.Empty.AsString()) "" "Empty string"

                    testCase "Text AsBytes returns UTF8 bytes"
                    <| fun () ->
                        let body = RequestBody.Text "hello"
                        Expect.equal (body.AsBytes()) (Encoding.UTF8.GetBytes "hello") "UTF8 bytes"

                    testCase "Text AsString returns original text"
                    <| fun () ->
                        let body = RequestBody.Text "hello"
                        Expect.equal (body.AsString()) "hello" "Original text"

                    testCase "Bytes AsBytes returns same array"
                    <| fun () ->
                        let bytes = [| 1uy; 2uy; 3uy |]
                        let body = RequestBody.Bytes bytes
                        Expect.equal (body.AsBytes()) bytes "Same bytes"

                    testCase "Bytes AsString decodes UTF8"
                    <| fun () ->
                        let bytes = Encoding.UTF8.GetBytes "test"
                        let body = RequestBody.Bytes bytes
                        Expect.equal (body.AsString()) "test" "Decoded string"
                ]

            testList
                "ResponseBody"
                [

                    testCase "Empty ContentLength is Some 0"
                    <| fun () -> Expect.equal ResponseBody.Empty.ContentLength (Some 0L) "Empty = 0"

                    testCase "Bytes ContentLength matches length"
                    <| fun () ->
                        let body = ResponseBody.Bytes [| 1uy; 2uy; 3uy |]
                        Expect.equal body.ContentLength (Some 3L) "3 bytes"

                    testCase "Text ContentLength is UTF8 byte count"
                    <| fun () ->
                        let body = ResponseBody.Text "hello"
                        Expect.equal body.ContentLength (Some 5L) "5 bytes for hello"

                    testCase "Stream ContentLength returns length parameter"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), Some 42L)
                        Expect.equal body.ContentLength (Some 42L) "Explicit length"

                    testCase "Stream ContentLength returns None when no length"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), None)
                        Expect.equal body.ContentLength None "No length"

                    testCase "Json ContentLength is None"
                    <| fun () ->
                        let body = ResponseBody.Json {| x = 1 |}
                        Expect.equal body.ContentLength None "Json unknown until serialized"
                ]

            testList
                "HttpRequest"
                [

                    testCase "create sets method and path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/users"
                        Expect.equal req.Method HttpMethod.GET "Method"
                        Expect.equal req.Path "/users" "Path"

                    testCase "create splits path segments"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/api/v1/users"
                        Expect.equal req.PathSegments [ "api"; "v1"; "users" ] "Segments"

                    testCase "create handles root path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.equal req.PathSegments [] "Root has no segments"

                    testCase "withQueryParam adds parameter"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/search"
                            |> HttpRequest.withQueryParam "q" "test"

                        Expect.equal (HttpRequest.queryParam "q" req) (Some "test") "Query param"

                    testCase "withQueryParam appends to existing key"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/search"
                            |> HttpRequest.withQueryParam "tag" "a"
                            |> HttpRequest.withQueryParam "tag" "b"

                        Expect.equal (HttpRequest.queryParams "tag" req) [ "a"; "b" ] "Multi-value"

                    testCase "withHeader adds header"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withHeader "Accept" "application/json"

                        Expect.equal (HttpRequest.header "Accept" req) (Some "application/json") "Header"

                    testCase "withBody sets body"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "payload")

                        Expect.equal (req.Body.AsString()) "payload" "Body"

                    testCase "withMetadata adds typed metadata"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withMetadata "RequestId" (box "abc-123")

                        Expect.equal (HttpRequest.metadata<string> "RequestId" req) (Some "abc-123") "Metadata"

                    testCase "metadata returns None for wrong type"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withMetadata "count" (box 42)

                        Expect.isNone (HttpRequest.metadata<string> "count" req) "Wrong type"

                    testCase "metadata returns None for missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.metadata<string> "missing" req) "Missing key"

                    testCase "queryParam returns None for missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.queryParam "missing" req) "Missing"

                    testCase "header returns None for missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.header "missing" req) "Missing"
                ]

            testList
                "HttpResponse"
                [

                    testCase "create sets status with empty body"
                    <| fun () ->
                        let resp = HttpResponse.create HttpStatusCode.OK
                        Expect.equal resp.Status HttpStatusCode.OK "Status"
                        Expect.equal resp.Body ResponseBody.Empty "Empty body"

                    testCase "withHeader adds header"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Custom" "value"

                        Expect.equal (HttpResponse.header "X-Custom" resp) (Some "value") "Header"

                    testCase "withHeader throws for empty name"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                HttpResponse.create HttpStatusCode.OK
                                |> HttpResponse.withHeader "" "value"
                                |> ignore)
                            "Empty header name"

                    testCase "withHeader throws for invalid characters"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                HttpResponse.create HttpStatusCode.OK
                                |> HttpResponse.withHeader "Bad Header" "value"
                                |> ignore)
                            "Space in header name"

                    testCase "withBody sets body"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withBody (ResponseBody.Text "hi")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "hi" "Body text"
                        | _ -> failtest "Expected Text body"

                    testCase "withStatus changes status code"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withStatus HttpStatusCode.NotFound

                        Expect.equal resp.Status HttpStatusCode.NotFound "Updated status"

                    testCase "headers returns all values"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Multi" "a"
                            |> HttpResponse.withHeader "X-Multi" "b"

                        Expect.equal (HttpResponse.headers "X-Multi" resp) [ "a"; "b" ] "Multi-value"
                ]

            testList
                "ServerConfig"
                [

                    testCase "defaultConfig has expected values"
                    <| fun () ->
                        let cfg = ServerConfig.defaultConfig
                        Expect.equal cfg.Host "127.0.0.1" "Host"
                        Expect.equal cfg.Port 8080 "Port"
                        Expect.equal cfg.MaxRequestBodySize (30L * 1024L * 1024L) "MaxBodySize"

                    testCase "create sets host and port"
                    <| fun () ->
                        let cfg = ServerConfig.create "0.0.0.0" 3000
                        Expect.equal cfg.Host "0.0.0.0" "Host"
                        Expect.equal cfg.Port 3000 "Port"

                    testCase "withMaxBodySize updates field"
                    <| fun () ->
                        let cfg = ServerConfig.defaultConfig |> ServerConfig.withMaxBodySize (1024L * 1024L)
                        Expect.equal cfg.MaxRequestBodySize (1024L * 1024L) "1MB"
                ]
        ]
