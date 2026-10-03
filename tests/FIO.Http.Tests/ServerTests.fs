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

                            let status, body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/").Result
                                    let body = resp.Content.ReadAsStringAsync().Result
                                    int resp.StatusCode, body)

                            Expect.equal status 200 "200"
                            Expect.equal body "hello" "Body"

                        testCase "Routing - routes to the matching handler"
                        <| fun () ->
                            let routes =
                                get "/a" (HttpHandler.text "A")
                                |> Routes.combine (get "/b" (HttpHandler.text "B"))

                            let bodyA, bodyB =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let bodyA = client.GetStringAsync($"http://127.0.0.1:{port}/a").Result
                                    let bodyB = client.GetStringAsync($"http://127.0.0.1:{port}/b").Result
                                    bodyA, bodyB)

                            Expect.equal bodyA "A" "Route A"
                            Expect.equal bodyB "B" "Route B"

                        testCase "Routing - returns 404 for an unknown path"
                        <| fun () ->
                            let routes = get "/known" (HttpHandler.text "known")

                            let status =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/unknown").Result
                                    int resp.StatusCode)

                            Expect.equal status 404 "404"
                    ]

                testList
                    "Body handling"
                    [
                        testCase "Body handling - serves a JSON response body"
                        <| fun () ->
                            let routes = get "/json" (HttpHandler.okJson {| message = "hello" |})

                            let status, ct, body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/json").Result
                                    let ct = resp.Content.Headers.ContentType.ToString()
                                    let body = resp.Content.ReadAsStringAsync().Result
                                    int resp.StatusCode, ct, body)

                            Expect.equal status 200 "200"
                            Expect.stringContains ct "application/json" "JSON content-type"
                            Expect.stringContains body "hello" "JSON body"

                        testCase "Body handling - accepts a POST with a request body"
                        <| fun () ->
                            let routes =
                                post "/echo" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            let status, body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let content =
                                        new StringContent("test payload", Encoding.UTF8, "text/plain")
                                    let resp = client.PostAsync($"http://127.0.0.1:{port}/echo", content).Result
                                    let body = resp.Content.ReadAsStringAsync().Result
                                    int resp.StatusCode, body)

                            Expect.equal status 200 "200"
                            Expect.equal body "test payload" "Echoed body"

                        testCase "Body handling - serves a text response body"
                        <| fun () ->
                            let routes = get "/text" (HttpHandler.text "plain text")

                            let body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    client.GetStringAsync($"http://127.0.0.1:{port}/text").Result)

                            Expect.equal body "plain text" "Text body"
                    ]

                testList
                    "Request body limits and rejection paths"
                    [
                        testCase "Body limits - rejects a body larger than the limit with 413"
                        <| fun () ->
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            let status, body =
                                withTestHttpServerMaxBody 64L routes (fun port ->
                                    use client = new HttpClient()
                                    let content =
                                        new StringContent(String.replicate 500 "x", Encoding.UTF8, "text/plain")
                                    let resp = client.PostAsync($"http://127.0.0.1:{port}/upload", content).Result
                                    let body = resp.Content.ReadAsStringAsync().Result
                                    int resp.StatusCode, body)

                            Expect.equal status 413 "An over-sized body must be rejected with 413"
                            Expect.stringContains body "exceeds maximum allowed size" "The 413 must say why"

                        testCase "Body limits - accepts a body exactly at the limit"
                        <| fun () ->
                            let payload = String.replicate 64 "y"
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                            let status, body =
                                withTestHttpServerMaxBody 64L routes (fun port ->
                                    use client = new HttpClient()
                                    let content =
                                        new StringContent(payload, Encoding.UTF8, "text/plain")
                                    let resp = client.PostAsync($"http://127.0.0.1:{port}/upload", content).Result
                                    let body = resp.Content.ReadAsStringAsync().Result
                                    int resp.StatusCode, body)

                            Expect.equal status 200 "A body exactly at the limit is allowed"
                            Expect.equal body payload "Body must round-trip intact"

                        testCase "Body limits - rejects a chunked body over the limit with 413"
                        <| fun () ->
                            let port = findAvailablePort ()
                            let config = ServerConfig.create "127.0.0.1" port |> ServerConfig.withMaxBodySize 16L
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))
                                |> Routes.combine (get "/ping" (HttpHandler.text "pong"))
                            use runtime = new DefaultRuntime()
                            let server = runWithTimeout (runtime :> FIORuntime) (Server.startServer config routes)

                            try
                                getWhenListening $"http://127.0.0.1:{port}/ping" |> ignore

                                let raw =
                                    sendRawRequest port (String.concat "\r\n" [
                                        "POST /upload HTTP/1.1"
                                        "Host: 127.0.0.1"
                                        "Content-Type: text/plain"
                                        "Transfer-Encoding: chunked"
                                        "Connection: close"
                                        ""
                                        "20"
                                        String.replicate 32 "x"
                                        "0"
                                        ""
                                        "" ])

                                Expect.stringStarts raw "HTTP/1.1 413" "A chunked body over the limit must be rejected with 413"
                            finally
                                runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore

                        testCase "Body limits - survives a truncated request body and keeps serving"
                        <| fun () ->
                            let routes = get "/ping" (HttpHandler.text "pong")

                            let body =
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
                                    client.GetStringAsync($"http://127.0.0.1:{port}/ping").Result)

                            Expect.equal body "pong" "A truncated request must not wedge the server"

                        testCase "Path validation - rejects a backslash segment with 400"
                        <| fun () ->
                            let routes = get "/safe" (HttpHandler.text "ok")

                            let raw =
                                withTestHttpServer routes (fun port ->
                                    sendRawRequest port (String.concat "\r\n" [
                                        "GET /safe%5C..%5Cetc HTTP/1.1"
                                        "Host: 127.0.0.1"
                                        "Connection: close"
                                        ""
                                        "" ]))

                            Expect.stringStarts raw "HTTP/1.1 400" "A backslash segment must be rejected with 400"
                            Expect.stringContains raw "Invalid path segment" "The 400 must come from the path guard"
                    ]

                testList
                    "Response writing"
                    [
                        testCase "Response writing - HEAD suppresses the body but keeps Content-Length"
                        <| fun () ->
                            let routes = get "/text" (HttpHandler.text "plain text")

                            let raw =
                                withTestHttpServer routes (fun port ->
                                    sendRawRequest port (String.concat "\r\n" [
                                        "HEAD /text HTTP/1.1"
                                        "Host: 127.0.0.1"
                                        "Connection: close"
                                        ""
                                        "" ]))
                            let idx = raw.IndexOf "\r\n\r\n"
                            let body = if idx >= 0 then raw.Substring(idx + 4) else ""

                            Expect.stringContains raw "200" "HEAD must still succeed"
                            Expect.stringContains raw "Content-Length: 10" "HEAD must report the length a GET would return"
                            Expect.equal body "" "HEAD must not write a response body"

                        testCase "Response writing - serves a raw byte response body"
                        <| fun () ->
                            let payload = [| 0uy; 1uy; 2uy; 253uy; 254uy; 255uy |]
                            let routes =
                                get "/bytes" (fun _ ->
                                    FIO.succeed
                                        { HttpResponse.create HttpStatusCode.OK with
                                            Body = ResponseBody.Bytes payload })

                            let status, bytes =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/bytes").Result
                                    let bytes = resp.Content.ReadAsByteArrayAsync().Result
                                    int resp.StatusCode, bytes)

                            Expect.equal status 200 "200"
                            Expect.sequenceEqual bytes payload "Raw bytes must round-trip unmodified"

                        testCase "Response writing - serves a stream response body with its Content-Length"
                        <| fun () ->
                            let payload = [| 1uy .. 64uy |]
                            let routes =
                                get "/stream" (fun _ ->
                                    FIO.succeed (
                                        Response.okStream
                                            (new IO.MemoryStream(payload))
                                            (Some(int64 payload.Length))
                                            "application/octet-stream"))

                            let status, contentLength, bytes =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/stream").Result
                                    let contentLength = resp.Content.Headers.ContentLength
                                    let bytes = resp.Content.ReadAsByteArrayAsync().Result
                                    int resp.StatusCode, contentLength, bytes)

                            Expect.equal status 200 "200"
                            Expect.equal
                                contentLength
                                (Nullable(int64 payload.Length))
                                "The stream's length must become the Content-Length"
                            Expect.sequenceEqual bytes payload "The stream must be copied unmodified"

                        testCase "Response writing - aborts the connection when a stream without a length fails mid-body"
                        <| fun () ->
                            let routes =
                                get "/broken" (fun _ ->
                                    FIO.succeed (Response.okStream (new FailingStream(Array.create (256 * 1024) 1uy)) None "application/octet-stream"))

                            let outcome =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    try
                                        Ok (client.GetByteArrayAsync($"http://127.0.0.1:{port}/broken").Result.Length)
                                    with ex ->
                                        Error (ex.GetBaseException().Message))

                            match outcome with
                            | Error _ -> ()
                            | Ok length -> failtest $"A body that failed mid-copy must not arrive as a complete response, got {length} bytes"

                        testCase "Response writing - a throwing handler still produces a well-formed response"
                        <| fun () ->
                            let routes = get "/boom" (fun _ -> FIO.attempt (fun () -> failwith "handler exploded") id)

                            let status =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/boom").Result
                                    int resp.StatusCode)

                            Expect.isTrue
                                (status >= 500)
                                $"A throwing handler must surface as a server error, got {status}"
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
                        testCase "start - fails with the bind error and leaves the server unstarted"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let config = ServerConfig.create "192.0.2.1" (findAvailablePort ())
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let server =
                                runWithTimeout (runtime :> FIORuntime) (Server.createWithRuntime config routes runtime)
                            let rec socketErrorOf (ex: exn) =
                                if isNull ex then
                                    None
                                else
                                    match ex with
                                    | :? SocketException as socketEx -> Some socketEx.SocketErrorCode
                                    | _ -> socketErrorOf ex.InnerException

                            let started = runtime.Run(Server.start server).Task()
                            let settled = started.Wait(TimeSpan.FromSeconds 30.0)

                            Expect.isTrue settled "start must settle"
                            match started.Result with
                            | Failed error ->
                                Expect.equal
                                    (socketErrorOf error)
                                    (Some SocketError.AddressNotAvailable)
                                    $"start must fail with the bind error, got {error}"
                            | Succeeded _ ->
                                runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                                failtest "start succeeded on 192.0.2.1, which must not bind unless the host allows non-local binds"
                            | Interrupted ex -> failtest $"start must fail with a typed error, not be interrupted: {ex.Message}"

                            let result = runtime.Run(Server.run server).UnsafeResult()

                            match result with
                            | Failed notStarted ->
                                Expect.stringContains
                                    notStarted.Message
                                    "not started"
                                    "A server whose start failed must not count as started"
                            | other -> failtest $"run after a failed start must fail, got {other}"

                        testCase "stop - releases the server's own runtime even when start failed"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let config = ServerConfig.create "192.0.2.1" (findAvailablePort ())
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let server = runWithTimeout (runtime :> FIORuntime) (Server.create config routes)
                            let started =
                                runWithTimeout
                                    (runtime :> FIORuntime)
                                    ((Server.start server).Map(fun _ -> true).CatchAll(fun _ -> FIO.succeed false))

                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore

                            Expect.isFalse started "start must fail on 192.0.2.1"
                            Expect.throwsT<ObjectDisposedException>
                                (fun () -> (Server.runtimeOf server).Run(FIO.unit () : FIO<unit, exn>) |> ignore)
                                "stop must release the runtime the server created, even though start failed"

                        testCase "stop - leaves the server stopped: a second stop is a no-op and run fails as not started"
                        <| fun () ->
                            let port = findAvailablePort ()
                            use runtime = new DefaultRuntime()
                            let config = ServerConfig.create "127.0.0.1" port
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let server =
                                runWithTimeout (runtime :> FIORuntime) (Server.startServerWithRuntime config routes runtime)

                            let body = getWhenListening $"http://127.0.0.1:{port}/ping"

                            Expect.equal body "pong" "The server must serve before it is stopped"

                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                            let result = runtime.Run(Server.run server).UnsafeResult()

                            match result with
                            | Failed error -> Expect.stringContains error.Message "not started" "A stopped server must not count as started"
                            | other -> failtest $"run after stop must fail, got {other}"

                        testCase "run - fails when the server has not been started"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let config = ServerConfig.create "127.0.0.1" (findAvailablePort ())
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let server =
                                runWithTimeout (runtime :> FIORuntime) (Server.createWithRuntime config routes runtime)

                            let result = runtime.Run(Server.run server).UnsafeResult()

                            match result with
                            | Failed error -> Expect.stringContains error.Message "Call Server.start first" "run must say how to start the server"
                            | other -> failtest $"run before start must fail, got {other}"

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

                        testCase "runServerWithRuntime - stops when interrupted and leaves the given runtime running"
                        <| fun () ->
                            let port = findAvailablePort ()
                            use driverRuntime = new DefaultRuntime()
                            use serverRuntime = new DefaultRuntime()
                            let config = ServerConfig.create "127.0.0.1" port
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let served = Threading.Tasks.TaskCompletionSource()
                            let driver =
                                fio {
                                    let! server = (Server.runServerWithRuntime config routes serverRuntime).Fork()
                                    do! FIO.awaitUnitTask served.Task id
                                    do! server.InterruptNow()
                                }

                            let driverFiber = driverRuntime.Run driver
                            let body = getWhenListening $"http://127.0.0.1:{port}/ping"

                            Expect.equal body "pong" "The server must serve while it runs"

                            served.SetResult()
                            let finished = driverFiber.Task().Wait(TimeSpan.FromSeconds 30.0)

                            Expect.isTrue finished "The driver must finish once the server has unwound"

                            let listening = isListening port
                            let answer = runWithTimeout (serverRuntime :> FIORuntime) (FIO.succeed 42)

                            Expect.isFalse listening "Interrupting runServerWithRuntime must stop the server"
                            Expect.equal answer 42 "The caller's runtime must keep running effects"

                        testCase "runServer - serves until interrupted, then stops the server"
                        <| fun () ->
                            let port = findAvailablePort ()
                            use runtime = new DefaultRuntime()
                            let config = ServerConfig.create "127.0.0.1" port
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let served = Threading.Tasks.TaskCompletionSource()
                            let driver =
                                fio {
                                    let! server = (Server.runServer config routes).Fork()
                                    do! FIO.awaitUnitTask served.Task id
                                    do! server.InterruptNow()
                                }

                            let driverFiber = runtime.Run driver
                            let body = getWhenListening $"http://127.0.0.1:{port}/ping"

                            Expect.equal body "pong" "The server must serve while it runs"

                            served.SetResult()
                            let finished = driverFiber.Task().Wait(TimeSpan.FromSeconds 30.0)

                            Expect.isTrue finished "The driver must finish once the server has unwound"

                            let listening = isListening port

                            Expect.isFalse listening "Interrupting runServer must stop the server"
                    ]
            ]
    )
