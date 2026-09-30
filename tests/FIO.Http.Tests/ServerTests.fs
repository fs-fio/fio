module FIO.Http.Tests.ServerTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Runtime
open FIO.Runtime.Default
open FIO.Http.SimpleRoutes

open System
open System.Text
open System.Net.Http
open System.Net.Sockets

open Expecto

[<Tests>]
let serverTests =
    testSequenced (
        testList
            "Server"
            [
                testList
                    "Routing"
                    [
                        testCase "Routing - responds to a GET request"
                        <| fun () ->
                            let routes = get "/" (HttpHandler.text "hello")

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()

                                let resp = client.GetAsync($"http://127.0.0.1:{port}/").Result
                                Expect.equal (int resp.StatusCode) 200 "200"

                                let body = resp.Content.ReadAsStringAsync().Result
                                Expect.equal body "hello" "Body")

                        testCase "Routing - routes to the matching handler"
                        <| fun () ->
                            let routes =
                                get "/a" (HttpHandler.text "A")
                                |> Routes.combine (get "/b" (HttpHandler.text "B"))

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()

                                let bodyA = client.GetStringAsync($"http://127.0.0.1:{port}/a").Result
                                let bodyB = client.GetStringAsync($"http://127.0.0.1:{port}/b").Result

                                Expect.equal bodyA "A" "Route A"
                                Expect.equal bodyB "B" "Route B")

                        testCase "Routing - returns 404 for an unknown path"
                        <| fun () ->
                            let routes = get "/known" (HttpHandler.text "known")

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()

                                let resp = client.GetAsync($"http://127.0.0.1:{port}/unknown").Result

                                Expect.equal (int resp.StatusCode) 404 "404")
                    ]

                testList
                    "Body handling"
                    [
                        testCase "Body handling - serves a JSON response body"
                        <| fun () ->
                            let routes = get "/json" (HttpHandler.okJson {| message = "hello" |})

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()
                                let resp = client.GetAsync($"http://127.0.0.1:{port}/json").Result
                                Expect.equal (int resp.StatusCode) 200 "200"

                                let ct = resp.Content.Headers.ContentType.ToString()
                                Expect.stringContains ct "application/json" "JSON content-type"

                                let body = resp.Content.ReadAsStringAsync().Result
                                Expect.stringContains body "hello" "JSON body")

                        testCase "Body handling - accepts a POST with a request body"
                        <| fun () ->
                            let routes =
                                post "/echo" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()

                                let content =
                                    new StringContent("test payload", Encoding.UTF8, "text/plain")

                                let resp = client.PostAsync($"http://127.0.0.1:{port}/echo", content).Result
                                Expect.equal (int resp.StatusCode) 200 "200"

                                let body = resp.Content.ReadAsStringAsync().Result
                                Expect.equal body "test payload" "Echoed body")

                        testCase "Body handling - serves a text response body"
                        <| fun () ->
                            let routes = get "/text" (HttpHandler.text "plain text")

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()

                                let body = client.GetStringAsync($"http://127.0.0.1:{port}/text").Result
                                Expect.equal body "plain text" "Text body")
                    ]

                testList
                    "Request body limits and rejection paths"
                    [
                        testCase "Body limits - rejects a body larger than the limit with 413"
                        <| fun () ->
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            withTestHttpServerMaxBody 64L routes (fun port ->
                                use client = new HttpClient()

                                let content =
                                    new StringContent(String.replicate 500 "x", Encoding.UTF8, "text/plain")

                                let resp = client.PostAsync($"http://127.0.0.1:{port}/upload", content).Result

                                Expect.equal (int resp.StatusCode) 413 "An over-sized body must be rejected with 413"

                                let body = resp.Content.ReadAsStringAsync().Result
                                Expect.stringContains body "exceeds maximum allowed size" "The 413 must say why")

                        testCase "Body limits - accepts a body exactly at the limit"
                        <| fun () ->
                            let payload = String.replicate 64 "y"
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            withTestHttpServerMaxBody 64L routes (fun port ->
                                use client = new HttpClient()

                                let content =
                                    new StringContent(payload, Encoding.UTF8, "text/plain")

                                let resp = client.PostAsync($"http://127.0.0.1:{port}/upload", content).Result

                                Expect.equal (int resp.StatusCode) 200 "A body exactly at the limit is allowed"
                                Expect.equal (resp.Content.ReadAsStringAsync().Result) payload "Body must round-trip intact")

                        testCase "Body limits - survives a truncated request body and keeps serving"
                        <| fun () ->
                            let routes = get "/ping" (HttpHandler.text "pong")

                            withTestHttpServer routes (fun port ->
                                use tcp = new TcpClient()
                                tcp.Connect("127.0.0.1", port)
                                use stream = tcp.GetStream()

                                let request =
                                    String.concat "\r\n" [
                                        "POST /upload HTTP/1.1"
                                        "Host: 127.0.0.1"
                                        "Content-Type: text/plain"
                                        "Content-Length: 100"
                                        "Connection: close"
                                        ""
                                        "short" ]

                                let bytes = Encoding.ASCII.GetBytes request
                                stream.Write(bytes, 0, bytes.Length)
                                stream.Flush()
                                tcp.Client.Shutdown SocketShutdown.Send
                                stream.ReadTimeout <- 5000
                                (try stream.ReadByte() |> ignore with _ -> ())

                                use client = new HttpClient()
                                let body = client.GetStringAsync($"http://127.0.0.1:{port}/ping").Result
                                Expect.equal body "pong" "A truncated request must not wedge the server")
                    ]

                testList
                    "Response writing"
                    [
                        testCase "Response writing - HEAD suppresses the body but keeps Content-Length"
                        <| fun () ->
                            let routes = get "/text" (HttpHandler.text "plain text")

                            withTestHttpServer routes (fun port ->
                                use tcp = new TcpClient()
                                tcp.Connect("127.0.0.1", port)
                                use stream = tcp.GetStream()

                                let request =
                                    String.concat "\r\n" [
                                        "HEAD /text HTTP/1.1"
                                        "Host: 127.0.0.1"
                                        "Connection: close"
                                        ""
                                        "" ]

                                let bytes = Encoding.ASCII.GetBytes request
                                stream.Write(bytes, 0, bytes.Length)
                                stream.Flush()

                                stream.ReadTimeout <- 5000
                                use reader = new IO.StreamReader(stream, Encoding.ASCII)
                                let raw = try reader.ReadToEnd() with _ -> ""

                                Expect.stringContains raw "200" "HEAD must still succeed"
                                Expect.stringContains raw "Content-Length: 10" "HEAD must report the length a GET would return"

                                let idx = raw.IndexOf "\r\n\r\n"
                                let body = if idx >= 0 then raw.Substring(idx + 4) else ""
                                Expect.equal body "" "HEAD must not write a response body")

                        testCase "Response writing - serves a raw byte response body"
                        <| fun () ->
                            let payload = [| 0uy; 1uy; 2uy; 253uy; 254uy; 255uy |]

                            let routes =
                                get "/bytes" (fun _ ->
                                    FIO.succeed
                                        { HttpResponse.create HttpStatusCode.OK with
                                            Body = ResponseBody.Bytes payload })

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()
                                let resp = client.GetAsync($"http://127.0.0.1:{port}/bytes").Result

                                Expect.equal (int resp.StatusCode) 200 "200"
                                Expect.sequenceEqual
                                    (resp.Content.ReadAsByteArrayAsync().Result)
                                    payload
                                    "Raw bytes must round-trip unmodified")

                        testCase "Response writing - a throwing handler still produces a well-formed response"
                        <| fun () ->
                            let routes = get "/boom" (fun _ -> FIO.attempt (fun () -> failwith "handler exploded") id)

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()
                                let resp = client.GetAsync($"http://127.0.0.1:{port}/boom").Result

                                Expect.isTrue
                                    (int resp.StatusCode >= 500)
                                    $"A throwing handler must surface as a server error, got {int resp.StatusCode}")

                        testCase "Response writing - rejects a path-traversal segment with 400"
                        <| fun () ->
                            let routes = get "/safe" (HttpHandler.text "ok")

                            withTestHttpServer routes (fun port ->
                                use client = new HttpClient()
                                use request =
                                    new HttpRequestMessage(
                                        HttpMethod.Get,
                                        Uri("http://127.0.0.1:" + string port + "/..%2f..%2fetc/passwd", UriKind.Absolute))

                                let resp = client.SendAsync(request).Result

                                Expect.isTrue
                                    (int resp.StatusCode = 400 || int resp.StatusCode = 404)
                                    $"A traversal segment must not be served, got {int resp.StatusCode}")
                    ]

                testList
                    "ServerBuilder"
                    [
                        testCase "ServerBuilder - setters compose onto a configuration"
                        <| fun () ->
                            let config =
                                ServerConfig.create "127.0.0.1" 1
                                |> ServerBuilder.host "0.0.0.0"
                                |> ServerBuilder.port 9999
                                |> ServerBuilder.maxBodySize 4096L

                            Expect.equal config.Host "0.0.0.0" "host must be applied"
                            Expect.equal config.Port 9999 "port must be applied"
                            Expect.equal config.MaxRequestBodySize 4096L "maxBodySize must be applied"

                        testCase "ServerBuilder - setters are independent"
                        <| fun () ->
                            let baseConfig = ServerConfig.create "127.0.0.1" 8080
                            let hostOnly = baseConfig |> ServerBuilder.host "example.test"

                            Expect.equal hostOnly.Port baseConfig.Port "host must not disturb the port"
                            Expect.equal
                                hostOnly.MaxRequestBodySize
                                baseConfig.MaxRequestBodySize
                                "host must not disturb the body limit"
                            Expect.equal baseConfig.Host "127.0.0.1" "the original config must be unchanged"

                        testCase "ServerBuilder.startNow - serves a request and can be stopped"
                        <| fun () ->
                            let port = findAvailablePort ()
                            let routes = get "/built" (HttpHandler.text "from builder")
                            use runtime = new DefaultRuntime()

                            let config =
                                ServerConfig.create "127.0.0.1" 1
                                |> ServerBuilder.host "127.0.0.1"
                                |> ServerBuilder.port port

                            let server = runWithTimeout (runtime :> FIORuntime) (ServerBuilder.startNow routes config)

                            try
                                let body = getWhenListening $"http://127.0.0.1:{port}/built"
                                Expect.equal body "from builder" "ServerBuilder.startNow must serve the routes it was given"
                            finally
                                runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                    ]

                testList
                    "Server lifecycle"
                    [
                        testCase "startServer - serves a request, then stop tears it down"
                        <| fun () ->
                            let port = findAvailablePort ()
                            let config = ServerConfig.create "127.0.0.1" port
                            let routes = get "/ping" (HttpHandler.text "pong")
                            use runtime = new DefaultRuntime()

                            let server =
                                runWithTimeout (runtime :> FIORuntime) (Server.startServer config routes)

                            try
                                System.Threading.Thread.Sleep 200
                                use client = new HttpClient()
                                let body = client.GetStringAsync($"http://127.0.0.1:{port}/ping").Result
                                Expect.equal body "pong" "Served via Server.startServer"
                            finally
                                runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                    ]
            ]
    )
