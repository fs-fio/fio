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
                        let invalidRoute = string (InvalidRoute "/x")
                        let timeoutError = string (TimeoutError "slow")
                        let serverFailed = string (ServerFailed(Exception "boom"))

                        Expect.stringContains invalidRoute "/x" "InvalidRoute"
                        Expect.stringContains timeoutError "slow" "TimeoutError"
                        Expect.stringContains serverFailed "boom" "ServerFailed"
                ]

            testList
                "HttpMethod"
                [

                    testCase "ToString - returns the method name for the standard methods"
                    <| fun () ->
                        let get = string HttpMethod.GET
                        let post = string HttpMethod.POST
                        let put = string HttpMethod.PUT
                        let delete = string HttpMethod.DELETE
                        let patch = string HttpMethod.PATCH
                        let head = string HttpMethod.HEAD
                        let options = string HttpMethod.OPTIONS
                        let trace = string HttpMethod.TRACE
                        let connect = string HttpMethod.CONNECT

                        Expect.equal get "GET" "GET"
                        Expect.equal post "POST" "POST"
                        Expect.equal put "PUT" "PUT"
                        Expect.equal delete "DELETE" "DELETE"
                        Expect.equal patch "PATCH" "PATCH"
                        Expect.equal head "HEAD" "HEAD"
                        Expect.equal options "OPTIONS" "OPTIONS"
                        Expect.equal trace "TRACE" "TRACE"
                        Expect.equal connect "CONNECT" "CONNECT"

                    testCase "fromString - parses all standard methods"
                    <| fun () ->
                        let get = HttpMethod.fromString "GET"
                        let post = HttpMethod.fromString "POST"
                        let put = HttpMethod.fromString "PUT"
                        let delete = HttpMethod.fromString "DELETE"
                        let patch = HttpMethod.fromString "PATCH"
                        let head = HttpMethod.fromString "HEAD"
                        let options = HttpMethod.fromString "OPTIONS"

                        Expect.equal get HttpMethod.GET "GET"
                        Expect.equal post HttpMethod.POST "POST"
                        Expect.equal put HttpMethod.PUT "PUT"
                        Expect.equal delete HttpMethod.DELETE "DELETE"
                        Expect.equal patch HttpMethod.PATCH "PATCH"
                        Expect.equal head HttpMethod.HEAD "HEAD"
                        Expect.equal options HttpMethod.OPTIONS "OPTIONS"

                    testCase "fromString - is case-insensitive"
                    <| fun () ->
                        let lowercase = HttpMethod.fromString "get"
                        let mixedCase = HttpMethod.fromString "Post"

                        Expect.equal lowercase HttpMethod.GET "lowercase"
                        Expect.equal mixedCase HttpMethod.POST "mixed case"

                    testCase "fromString - returns Custom for an unknown method"
                    <| fun () ->
                        let parsed = HttpMethod.fromString "PURGE"

                        match parsed with
                        | HttpMethod.Custom s -> Expect.equal s "PURGE" "Custom method"
                        | _ -> failtest "Expected Custom"
                ]

            testList
                "RequestBody"
                [

                    testCase "AsBytes - returns an empty array for Empty"
                    <| fun () ->
                        let actual = RequestBody.Empty.AsBytes()

                        Expect.equal actual Array.empty "Empty bytes"

                    testCase "AsString - returns an empty string for Empty"
                    <| fun () ->
                        let actual = RequestBody.Empty.AsString()

                        Expect.equal actual "" "Empty string"

                    testCase "AsBytes - returns the UTF-8 bytes of Text"
                    <| fun () ->
                        let body = RequestBody.Text "hello"

                        let actual = body.AsBytes()

                        Expect.equal actual (Encoding.UTF8.GetBytes "hello") "UTF8 bytes"

                    testCase "AsString - returns the original text of Text"
                    <| fun () ->
                        let body = RequestBody.Text "hello"

                        let actual = body.AsString()

                        Expect.equal actual "hello" "Original text"

                    testCase "AsBytes - returns the same array for Bytes"
                    <| fun () ->
                        let bytes = [| 1uy; 2uy; 3uy |]
                        let body = RequestBody.Bytes bytes

                        let actual = body.AsBytes()

                        Expect.equal actual bytes "Same bytes"

                    testCase "AsString - decodes Bytes as UTF-8"
                    <| fun () ->
                        let bytes = Encoding.UTF8.GetBytes "test"
                        let body = RequestBody.Bytes bytes

                        let actual = body.AsString()

                        Expect.equal actual "test" "Decoded string"
                ]

            testList
                "ResponseBody"
                [

                    testCase "ContentLength - is Some 0 for Empty"
                    <| fun () ->
                        let actual = ResponseBody.Empty.ContentLength

                        Expect.equal actual (Some 0L) "Empty = 0"

                    testCase "ContentLength - matches the length for Bytes"
                    <| fun () ->
                        let body = ResponseBody.Bytes [| 1uy; 2uy; 3uy |]

                        let actual = body.ContentLength

                        Expect.equal actual (Some 3L) "3 bytes"

                    testCase "ContentLength - is the UTF-8 byte count for Text"
                    <| fun () ->
                        let body = ResponseBody.Text "hello"

                        let actual = body.ContentLength

                        Expect.equal actual (Some 5L) "5 bytes for hello"

                    testCase "ContentLength - returns the length parameter for Stream"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), Some 42L)

                        let actual = body.ContentLength

                        Expect.equal actual (Some 42L) "Explicit length"

                    testCase "ContentLength - is None for a Stream without a length"
                    <| fun () ->
                        let body = ResponseBody.Stream(new MemoryStream(), None)

                        let actual = body.ContentLength

                        Expect.equal actual None "No length"

                    testCase "ContentLength - is None for Json"
                    <| fun () ->
                        let body = ResponseBody.Json {| x = 1 |}

                        let actual = body.ContentLength

                        Expect.equal actual None "Json unknown until serialized"
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

                    testCase "withHeader - throws for an invalid header name"
                    <| fun () ->
                        Expect.throwsT<ArgumentException>
                            (fun () ->
                                HttpRequest.create HttpMethod.GET "/"
                                |> HttpRequest.withHeader "Bad Header" "value"
                                |> ignore)
                            "A header name with a space must be rejected"

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

                        let actual = HttpRequest.metadata<string> "count" req

                        Expect.isNone actual "Wrong type"

                    testCase "metadata - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"

                        let actual = HttpRequest.metadata<string> "missing" req

                        Expect.isNone actual "Missing key"

                    testCase "queryParam - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"

                        let actual = HttpRequest.queryParam "missing" req

                        Expect.isNone actual "Missing"

                    testCase "header - returns None for a missing key"
                    <| fun () ->
                        let req = HttpRequest.create HttpMethod.GET "/"

                        let actual = HttpRequest.header "missing" req

                        Expect.isNone actual "Missing"

                    testCase "header - lookup is case-insensitive"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.GET "/"
                            |> HttpRequest.withHeader "Content-Type" "application/json"

                        let lowercase = HttpRequest.header "content-type" req
                        let uppercase = HttpRequest.header "CONTENT-TYPE" req

                        Expect.equal lowercase (Some "application/json") "Lowercase lookup"
                        Expect.equal uppercase (Some "application/json") "Uppercase lookup"

                    testCase "bodyText - decodes bytes as UTF-8 by default"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/"
                            |> HttpRequest.withBody (RequestBody.Bytes(Encoding.UTF8.GetBytes "héllo"))

                        let actual = HttpRequest.bodyText req

                        Expect.equal actual "héllo" "UTF-8 decode"

                    testCase "bodyText - decodes bytes with the charset from Content-Type"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/"
                            |> HttpRequest.withHeader "Content-Type" "text/plain; charset=iso-8859-1"
                            |> HttpRequest.withBody (RequestBody.Bytes(Encoding.Latin1.GetBytes "héllo"))

                        let actual = HttpRequest.bodyText req

                        Expect.equal actual "héllo" "The declared charset must be used"

                    testCase "bodyText - falls back to UTF-8 for an unknown charset"
                    <| fun () ->
                        let req =
                            HttpRequest.create HttpMethod.POST "/"
                            |> HttpRequest.withHeader "Content-Type" "text/plain; charset=no-such-charset"
                            |> HttpRequest.withBody (RequestBody.Bytes(Encoding.UTF8.GetBytes "héllo"))

                        let actual = HttpRequest.bodyText req

                        Expect.equal actual "héllo" "An unknown charset must fall back to UTF-8"
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

                        let actual = HttpResponse.headers "X-Multi" resp

                        Expect.equal actual [ "a"; "b" ] "Multi-value"

                    testCase "header - lookup on a response is case-insensitive"
                    <| fun () ->
                        let resp =
                            HttpResponse.create HttpStatusCode.OK
                            |> HttpResponse.withHeader "X-Custom" "v"

                        let actual = HttpResponse.header "x-custom" resp

                        Expect.equal actual (Some "v") "CI response lookup"
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
