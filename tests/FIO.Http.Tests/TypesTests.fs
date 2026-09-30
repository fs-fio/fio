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

                    testCase "fromException - wraps an exception in GeneralError"
                    <| fun () ->
                        let ex = Exception "test"
                        let error = HttpError.fromException ex

                        match error with
                        | GeneralError e ->
                            Expect.isTrue (Object.ReferenceEquals(e, ex)) "Should wrap same exception reference"
                        | _ -> failtest "Expected GeneralError"

                    testCase "toException - unwraps GeneralError"
                    <| fun () ->
                        let original = Exception "test"
                        let error = GeneralError original
                        let result = HttpError.toException error

                        Expect.isTrue
                            (Object.ReferenceEquals(result, original))
                            "Should return same exception reference"

                    testCase "toException - creates an Exception for the other cases"
                    <| fun () ->
                        let error = InvalidRoute "/bad"
                        let result = HttpError.toException error

                        Expect.stringContains result.Message "/bad" "Exception message should contain error details"

                    testCase "ToString - produces readable messages"
                    <| fun () ->
                        Expect.stringContains (string (InvalidRoute "/x")) "/x" "InvalidRoute"
                        Expect.stringContains (string (TimeoutError "slow")) "slow" "TimeoutError"
                        Expect.stringContains (string (ServerFailed(Exception "boom"))) "boom" "ServerFailed"
                ]

            testList
                "HttpMethod"
                [

                    testCase "ToString - returns the method name for the standard methods"
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

                    testCase "fromString - parses all standard methods"
                    <| fun () ->
                        Expect.equal (HttpMethod.fromString "GET") HttpMethod.GET "GET"
                        Expect.equal (HttpMethod.fromString "POST") HttpMethod.POST "POST"
                        Expect.equal (HttpMethod.fromString "PUT") HttpMethod.PUT "PUT"
                        Expect.equal (HttpMethod.fromString "DELETE") HttpMethod.DELETE "DELETE"
                        Expect.equal (HttpMethod.fromString "PATCH") HttpMethod.PATCH "PATCH"
                        Expect.equal (HttpMethod.fromString "HEAD") HttpMethod.HEAD "HEAD"
                        Expect.equal (HttpMethod.fromString "OPTIONS") HttpMethod.OPTIONS "OPTIONS"

                    testCase "fromString - is case-insensitive"
                    <| fun () ->
                        Expect.equal (HttpMethod.fromString "get") HttpMethod.GET "lowercase"
                        Expect.equal (HttpMethod.fromString "Post") HttpMethod.POST "mixed case"

                    testCase "fromString - returns Custom for an unknown method"
                    <| fun () ->
                        match HttpMethod.fromString "PURGE" with
                        | HttpMethod.Custom s -> Expect.equal s "PURGE" "Custom method"
                        | _ -> failtest "Expected Custom"
                ]

            testList
                "RequestBody"
                [

                    testCase "AsBytes - returns an empty array for Empty"
                    <| fun () -> Expect.equal (RequestBody.Empty.AsBytes()) Array.empty "Empty bytes"

                    testCase "AsString - returns an empty string for Empty"
                    <| fun () -> Expect.equal (RequestBody.Empty.AsString()) "" "Empty string"

                    testCase "AsBytes - returns the UTF-8 bytes of Text"
                    <| fun () ->
                        let body = RequestBody.Text "hello"
                        Expect.equal (body.AsBytes()) (Encoding.UTF8.GetBytes "hello") "UTF8 bytes"

                    testCase "AsString - returns the original text of Text"
                    <| fun () ->
                        let body = RequestBody.Text "hello"
                        Expect.equal (body.AsString()) "hello" "Original text"

                    testCase "AsBytes - returns the same array for Bytes"
                    <| fun () ->
                        let bytes = [| 1uy; 2uy; 3uy |]
                        let body = RequestBody.Bytes bytes
                        Expect.equal (body.AsBytes()) bytes "Same bytes"

                    testCase "AsString - decodes Bytes as UTF-8"
                    <| fun () ->
                        let bytes = Encoding.UTF8.GetBytes "test"
                        let body = RequestBody.Bytes bytes
                        Expect.equal (body.AsString()) "test" "Decoded string"
                ]

            testList
                "ResponseBody"
                [

                    testCase "ContentLength - is Some 0 for Empty"
                    <| fun () -> Expect.equal ResponseBody.Empty.ContentLength (Some 0L) "Empty = 0"

                    testCase "ContentLength - matches the length for Bytes"
                    <| fun () ->
                        let body = ResponseBody.Bytes [| 1uy; 2uy; 3uy |]
                        Expect.equal body.ContentLength (Some 3L) "3 bytes"

                    testCase "ContentLength - is the UTF-8 byte count for Text"
                    <| fun () ->
                        let body = ResponseBody.Text "hello"
                        Expect.equal body.ContentLength (Some 5L) "5 bytes for hello"

                    testCase "ContentLength - returns the length parameter for Stream"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), Some 42L)
                        Expect.equal body.ContentLength (Some 42L) "Explicit length"

                    testCase "ContentLength - is None for a Stream without a length"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), None)
                        Expect.equal body.ContentLength None "No length"

                    testCase "ContentLength - is None for Json"
                    <| fun () ->
                        let body = ResponseBody.Json {| x = 1 |}
                        Expect.equal body.ContentLength None "Json unknown until serialized"
                ]

            testList
                "HttpRequest"
                [

                    testCase "create - sets the method and path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/users"
                        Expect.equal req.Method HttpMethod.GET "Method"
                        Expect.equal req.Path "/users" "Path"

                    testCase "create - splits the path into segments"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/api/v1/users"
                        Expect.equal req.PathSegments [ "api"; "v1"; "users" ] "Segments"

                    testCase "create - handles the root path"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.equal req.PathSegments [] "Root has no segments"

                    testCase "withQueryParam - adds a parameter"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/search"
                            |> HttpRequest.withQueryParam "q" "test"

                        Expect.equal (HttpRequest.queryParam "q" req) (Some "test") "Query param"

                    testCase "withQueryParam - appends to an existing key"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/search"
                            |> HttpRequest.withQueryParam "tag" "a"
                            |> HttpRequest.withQueryParam "tag" "b"

                        Expect.equal (HttpRequest.queryParams "tag" req) [ "a"; "b" ] "Multi-value"

                    testCase "withHeader - adds a header"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withHeader "Accept" "application/json"

                        Expect.equal (HttpRequest.header "Accept" req) (Some "application/json") "Header"

                    testCase "withBody - sets the body"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/data"
                            |> HttpRequest.withBody (RequestBody.Text "payload")

                        Expect.equal (req.Body.AsString()) "payload" "Body"

                    testCase "withMetadata - adds typed metadata"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withMetadata "RequestId" (box "abc-123")

                        Expect.equal (HttpRequest.metadata<string> "RequestId" req) (Some "abc-123") "Metadata"

                    testCase "metadata - returns None for the wrong type"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withMetadata "count" (box 42)

                        Expect.isNone (HttpRequest.metadata<string> "count" req) "Wrong type"

                    testCase "metadata - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.metadata<string> "missing" req) "Missing key"

                    testCase "queryParam - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.queryParam "missing" req) "Missing"

                    testCase "header - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"
                        Expect.isNone (HttpRequest.header "missing" req) "Missing"

                    testCase "header - lookup is case-insensitive"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withHeader "Content-Type" "application/json"

                        Expect.equal
                            (HttpRequest.header "content-type" req)
                            (Some "application/json")
                            "Lowercase lookup"

                        Expect.equal
                            (HttpRequest.header "CONTENT-TYPE" req)
                            (Some "application/json")
                            "Uppercase lookup"

                    testCase "bodyText - decodes bytes as UTF-8 by default"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/"
                            |> HttpRequest.withBody (RequestBody.Bytes(Encoding.UTF8.GetBytes "héllo"))

                        Expect.equal (HttpRequest.bodyText req) "héllo" "UTF-8 decode"
                ]

            testList
                "HttpResponse"
                [

                    testCase "create - sets the status with an empty body"
                    <| fun () ->
                        let resp = HttpResponse.create HttpStatusCode.OK
                        Expect.equal resp.Status HttpStatusCode.OK "Status"
                        Expect.equal resp.Body ResponseBody.Empty "Empty body"

                    testCase "withHeader - adds a header to the response"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Custom" "value"

                        Expect.equal (HttpResponse.header "X-Custom" resp) (Some "value") "Header"

                    testCase "withHeader - throws for an empty name"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                HttpResponse.create HttpStatusCode.OK
                                |> HttpResponse.withHeader "" "value"
                                |> ignore)
                            "Empty header name"

                    testCase "withHeader - throws for invalid characters"
                    <| fun () ->
                        Expect.throws
                            (fun () ->
                                HttpResponse.create HttpStatusCode.OK
                                |> HttpResponse.withHeader "Bad Header" "value"
                                |> ignore)
                            "Space in header name"

                    testCase "withHeader - accepts uppercase letters across the full A-Z range"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "Content-Type" "text/plain"
                            |> HttpResponse.withHeader "Accept-Encoding" "gzip"

                        Expect.equal (HttpResponse.header "Content-Type" resp) (Some "text/plain") "Content-Type"
                        Expect.equal (HttpResponse.header "Accept-Encoding" resp) (Some "gzip") "Accept-Encoding"

                    testCase "withBody - sets the response body"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withBody (ResponseBody.Text "hi")

                        match resp.Body with
                        | ResponseBody.Text t -> Expect.equal t "hi" "Body text"
                        | _ -> failtest "Expected Text body"

                    testCase "withStatus - changes the status code"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withStatus HttpStatusCode.NotFound

                        Expect.equal resp.Status HttpStatusCode.NotFound "Updated status"

                    testCase "headers - returns all values"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Multi" "a"
                            |> HttpResponse.withHeader "X-Multi" "b"

                        Expect.equal (HttpResponse.headers "X-Multi" resp) [ "a"; "b" ] "Multi-value"

                    testCase "header - lookup on a response is case-insensitive"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Custom" "v"

                        Expect.equal (HttpResponse.header "x-custom" resp) (Some "v") "CI response lookup"
                ]

            testList
                "ServerConfig"
                [

                    testCase "defaultConfig - has the expected values"
                    <| fun () ->
                        let cfg = ServerConfig.defaultConfig
                        Expect.equal cfg.Host "127.0.0.1" "Host"
                        Expect.equal cfg.Port 8080 "Port"
                        Expect.equal cfg.MaxRequestBodySize (30L * 1024L * 1024L) "MaxBodySize"

                    testCase "create - sets the host and port"
                    <| fun () ->
                        let cfg = ServerConfig.create "0.0.0.0" 3000
                        Expect.equal cfg.Host "0.0.0.0" "Host"
                        Expect.equal cfg.Port 3000 "Port"

                    testCase "withMaxBodySize - updates the field"
                    <| fun () ->
                        let cfg = ServerConfig.defaultConfig |> ServerConfig.withMaxBodySize (1024L * 1024L)
                        Expect.equal cfg.MaxRequestBodySize (1024L * 1024L) "1MB"
                ]
        ]
