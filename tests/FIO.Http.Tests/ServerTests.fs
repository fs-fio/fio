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
open System.Threading
open System.Net.Sockets
open System.Threading.Tasks

open Expecto
open System.Diagnostics

let private testUnix name (body: unit -> unit) =
    testCase name (fun () ->
        if OperatingSystem.IsWindows() then
            skiptest "A POSIX signal cannot be sent to a single process on Windows"

        body ())

let private stuckClient = new HttpClient(Timeout = Threading.Timeout.InfiniteTimeSpan)

// The port can be taken before the child binds it, so a child that exits without listening is retried on a fresh one.
let rec private startListeningChild scenario (shutdownTimeout: int) remaining =
    let port = findAvailablePort ()
    let child = new ChildProcess.ChildProcess([ scenario; string port; string shutdownTimeout ])
    let listening = child.WaitForLine $"FIO HTTP Server listening on http://127.0.0.1:{port}" (TimeSpan.FromSeconds 60.0)

    if not listening && remaining > 0 && (child.WaitForExit TimeSpan.Zero).IsSome then
        (child :> IDisposable).Dispose()
        startListeningChild scenario shutdownTimeout (remaining - 1)
    else
        child, port, listening

// Runs runServer in a child process with one request that never finishes, so its stop must wait for it or cut it.
let private withStuckServer scenario (shutdownTimeout: int) (test: ChildProcess.ChildProcess -> int -> Task<HttpResponseMessage> -> 'T) =
    let started, port, listening = startListeningChild scenario shutdownTimeout 10
    use child = started
    let stuck = stuckClient.GetAsync $"http://127.0.0.1:{port}/stuck"

    if not (listening && child.WaitForLine "entered" (TimeSpan.FromSeconds 10.0)) then
        failtest $"The child server should start and take the request; output: {child.Output}"

    test child port stuck

// A stopping server answers new requests 503, which shows that its stop has begun.
let private waitForRefusal (port: int) =
    use client = new HttpClient(Timeout = TimeSpan.FromSeconds 2.0)
    let deadline = DateTime.UtcNow.AddSeconds 10.0
    let mutable refused = false

    while not refused && DateTime.UtcNow < deadline do
        refused <-
            try
                int (client.GetAsync($"http://127.0.0.1:{port}/ok").Result.StatusCode) = 503
            with _ ->
                false

        if not refused then
            Threading.Thread.Sleep 25

    refused

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
                                withConfiguredServer (ServerConfig.withMaxBodySize 64L) routes (fun port _ ->
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
                                withConfiguredServer (ServerConfig.withMaxBodySize 64L) routes (fun port _ ->
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
                            let routes =
                                post "/upload" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))
                                |> Routes.combine (get "/ping" (HttpHandler.text "pong"))

                            let raw =
                                withConfiguredServer (ServerConfig.withMaxBodySize 16L) routes (fun port _ ->
                                    getWhenListening $"http://127.0.0.1:{port}/ping" |> ignore
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
                                        "" ]))

                            Expect.stringStarts raw "HTTP/1.1 413" "A chunked body over the limit must be rejected with 413"

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

                            Expect.stringStarts raw "HTTP/1.1 200" "HEAD must still succeed"
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

                        testCase "Response writing - a handler's typed failure is answered 500 and logged without the query string"
                        <| fun () ->
                            let routes = get "/boom" (fun _ -> FIO.attempt (fun () -> failwith "handler exploded") id)

                            let status, logged =
                                withCapturedError (fun () ->
                                    withTestHttpServer routes (fun port ->
                                        use client = new HttpClient()
                                        let resp = client.GetAsync($"http://127.0.0.1:{port}/boom?token=SECRET").Result
                                        int resp.StatusCode))

                            Expect.equal status 500 "A failing handler must be answered 500"
                            Expect.stringContains logged "handler exploded" "The failure should be logged"
                            Expect.isFalse (logged.Contains "SECRET") "The query string must stay out of the log"

                        testCase "Response writing - a handler that throws inside its effect is answered 500 and logged"
                        <| fun () ->
                            let crashing : HttpHandler<exn> =
                                fun _ -> (FIO.succeed 1).FlatMap(fun _ -> failwith "handler crashed")
                            let routes =
                                get "/boom" crashing
                                |> Routes.combine (Middleware.before (fun _ -> FIO.unit ()) (get "/guarded" crashing))

                            let (plain, guarded), logged =
                                withCapturedError (fun () ->
                                    withTestHttpServer routes (fun port ->
                                        use client = new HttpClient()
                                        let plain = client.GetAsync($"http://127.0.0.1:{port}/boom").Result
                                        let guarded = client.GetAsync($"http://127.0.0.1:{port}/guarded").Result
                                        int plain.StatusCode, int guarded.StatusCode))

                            Expect.equal plain 500 "A handler that throws is a server error, not a 503"
                            Expect.equal guarded 500 "The same holds behind a middleware"
                            Expect.stringContains logged "handler crashed" "The thrown exception should be logged"

                        testCase "Response writing - a handler that interrupts itself is answered 503"
                        <| fun () ->
                            let routes = get "/busy" (fun _ -> FIO.interruptNow ())

                            let status =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    int (client.GetAsync($"http://127.0.0.1:{port}/busy").Result.StatusCode))

                            Expect.equal status 503 "An interruption that was asked for means the service is unavailable"

                        testCase "Response writing - a 204 with a body is answered 204 without it"
                        <| fun () ->
                            let routes =
                                get "/empty" (fun _ -> FIO.succeed { Response.noContent with Body = ResponseBody.Text "not sent" })

                            let status, body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    let resp = client.GetAsync($"http://127.0.0.1:{port}/empty").Result
                                    int resp.StatusCode, resp.Content.ReadAsStringAsync().Result)

                            Expect.equal status 204 "The handler's status should reach the client"
                            Expect.equal body "" "A 204 carries no body"

                        testCase "Body limits - accepts a body over Kestrel's own default when the configured limit allows it"
                        <| fun () ->
                            let routes = post "/upload" (fun req -> FIO.succeed (Response.okText (string (req.Body.AsBytes().Length))))
                            let size = 30_500_000

                            let status, body =
                                withTestHttpServer routes (fun port ->
                                    use client = new HttpClient()
                                    use content = new ByteArrayContent(Array.zeroCreate<byte> size)
                                    let resp = client.PostAsync($"http://127.0.0.1:{port}/upload", content).Result
                                    int resp.StatusCode, resp.Content.ReadAsStringAsync().Result)

                            Expect.equal status 200 "The server's limit, not Kestrel's 30,000,000-byte default, decides"
                            Expect.equal body (string size) "The whole body should reach the handler"

                        testCase "Logging - a path that carries a line break is logged on one line"
                        <| fun () ->
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let forged = "/a%5Cb%0D%0A[2026-01-01%2000:00:00.000]%20ERROR%20[HandlerError]%20GET%20/admin%20-%20FORGED"

                            let _, logged =
                                withCapturedError (fun () ->
                                    withTestHttpServer routes (fun port ->
                                        sendRawRequest port $"GET {forged} HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"))
                            let lines = logged.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

                            Expect.equal lines.Length 1 $"The rejected request should be one log line, got: {logged}"
                            Expect.stringContains lines[0] "\\x0D\\x0A" "The line break should be logged escaped"

                        testCase "Logging - a client that resets its connection in the middle of a body is not logged as an error"
                        <| fun () ->
                            let routes = post "/upload" (fun req -> FIO.succeed (Response.okText (string (req.Body.AsBytes().Length))))

                            let _, logged =
                                withCapturedError (fun () ->
                                    withTestHttpServer routes (fun port ->
                                        let client = new TcpClient()
                                        client.Connect("127.0.0.1", port)
                                        let head = Encoding.ASCII.GetBytes "POST /upload HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: 1000\r\n\r\npartial"
                                        client.GetStream().Write(head, 0, head.Length)
                                        Thread.Sleep 200
                                        client.LingerState <- LingerOption(true, 0)
                                        client.Close()
                                        Thread.Sleep 500))

                            Expect.equal logged "" "A client that left is not an error of the server"
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
                                |> ServerBuilder.shutdownTimeout 750

                            Expect.equal config.Host "0.0.0.0" "host must be applied"
                            Expect.equal config.Port 9999 "port must be applied"
                            Expect.equal config.MaxRequestBodySize 4096L "maxBodySize must be applied"
                            Expect.equal config.ShutdownTimeout 750 "shutdownTimeout must be applied"

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
                            let routes = get "/built" (HttpHandler.text "from builder")
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    ServerConfig.create "127.0.0.1" 1
                                    |> ServerBuilder.host "127.0.0.1"
                                    |> ServerBuilder.port port
                                    |> ServerBuilder.startNow routes)

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
                            use runtime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    Server.startServerWithRuntime (ServerConfig.create "127.0.0.1" port) routes runtime)

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
                            let routes = get "/ping" (HttpHandler.text "pong")
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    Server.startServer (ServerConfig.create "127.0.0.1" port) routes)

                            let body, afterStop =
                                try
                                    Thread.Sleep 200
                                    use client = new HttpClient()
                                    let body = client.GetStringAsync($"http://127.0.0.1:{port}/ping").Result
                                    runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                                    use fresh = new HttpClient(Timeout = TimeSpan.FromSeconds 5.0)
                                    let afterStop =
                                        try
                                            Ok(int (fresh.GetAsync($"http://127.0.0.1:{port}/ping").GetAwaiter().GetResult().StatusCode))
                                        with ex ->
                                            Error(ex.GetBaseException())
                                    body, afterStop
                                finally
                                    runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore

                            Expect.equal body "pong" "Served via Server.startServer"
                            match afterStop with
                            | Error(:? SocketException as refused) ->
                                Expect.equal refused.SocketErrorCode SocketError.ConnectionRefused "Nothing should listen on the port after stop"
                            | other -> failtest $"A request after stop must be refused because nothing listens, got %A{other}"

                        testCase "runServerWithRuntime - stops when interrupted and leaves the given runtime running"
                        <| fun () ->
                            use driverRuntime = new DefaultRuntime()
                            use serverRuntime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let served = TaskCompletionSource()
                            let port, driverFiber =
                                forkServerOnFreePort (driverRuntime :> FIORuntime) served.Task "/ping" (fun port ->
                                    Server.runServerWithRuntime (ServerConfig.create "127.0.0.1" port) routes serverRuntime)

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
                            use runtime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let served = TaskCompletionSource()
                            let port, driverFiber =
                                forkServerOnFreePort (runtime :> FIORuntime) served.Task "/ping" (fun port ->
                                    Server.runServer (ServerConfig.create "127.0.0.1" port) routes)

                            let body = getWhenListening $"http://127.0.0.1:{port}/ping"

                            Expect.equal body "pong" "The server must serve while it runs"

                            served.SetResult()
                            let finished = driverFiber.Task().Wait(TimeSpan.FromSeconds 30.0)

                            Expect.isTrue finished "The driver must finish once the server has unwound"

                            let listening = isListening port

                            Expect.isFalse listening "Interrupting runServer must stop the server"

                        testCase "start - fails on a server that was stopped"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    Server.startServerWithRuntime (ServerConfig.create "127.0.0.1" port) routes runtime)
                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore

                            let restarted = runtime.Run(Server.start server).UnsafeResult()
                            let listening = isListening port

                            match restarted with
                            | Failed error -> Expect.stringContains error.Message "cannot be started again" "start should say why it failed"
                            | other -> failtest $"A stopped server must not start again, got {other}"
                            Expect.isFalse listening "Nothing should listen after a refused start"

                        testCase "stop - racing a start leaves nothing listening"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")

                            let stillListening =
                                [
                                    for round in 1..8 ->
                                        onFreePort (fun port ->
                                            let server =
                                                runWithTimeout (runtime :> FIORuntime) (Server.create (ServerConfig.create "127.0.0.1" port) routes)
                                            let start, stop = (Server.start server).Result(), (Server.stop server).Result()
                                            let racing =
                                                if round % 2 = 0 then start.ZipPar stop
                                                else (stop.ZipPar start).Map(fun (stopped, started) -> started, stopped)
                                            let started, _ = runWithTimeout (runtime :> FIORuntime) racing
                                            let listening = isListening port
                                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                                            match started with
                                            | Error error when isAddressInUse error -> None
                                            | _ -> Some listening)
                                ]

                            Expect.allEqual stillListening false "Whichever of start and stop ran first, the server must end stopped"

                        testCase "runServer - interrupted while it starts, leaves nothing listening"
                        <| fun () ->
                            use runtime = new DefaultRuntime()
                            let routes = get "/ping" (HttpHandler.text "pong")
                            let random = Random 7

                            let stillListening =
                                [
                                    for _ in 1..20 ->
                                        onFreePort (fun port ->
                                            let delay = random.Next(0, 60)
                                            let driver =
                                                fio {
                                                    let! server = (Server.runServer (ServerConfig.create "127.0.0.1" port) routes).Fork()
                                                    do! FIO.sleep (TimeSpan.FromMilliseconds(float delay))
                                                    return! server.InterruptAwaitNow()
                                                }
                                            let outcome = runWithTimeout (runtime :> FIORuntime) driver
                                            let listening = isListening port
                                            match outcome with
                                            | Failed error when isAddressInUse error -> None
                                            | _ -> Some listening)
                                ]

                            Expect.allEqual stillListening false "An interruption during startup must not leave a server behind"

                        testCase "stop - lets a request in flight finish within the shutdown timeout"
                        <| fun () ->
                            let entered = new ManualResetEventSlim false
                            let routes =
                                get "/slow" (fun _ ->
                                    FIO.succeedWith(fun () -> entered.Set())
                                        .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 400.0))
                                        .FlatMap(fun () -> FIO.succeed (Response.okText "finished")))
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    Server.startServerWithRuntime (ServerConfig.create "127.0.0.1" port) routes runtime)
                            use client = new HttpClient()
                            let response = client.GetAsync $"http://127.0.0.1:{port}/slow"
                            let inFlight = entered.Wait(TimeSpan.FromSeconds 5.0)

                            let clock = Stopwatch.StartNew()
                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                            let elapsed = clock.ElapsedMilliseconds
                            let status = int response.Result.StatusCode
                            let body = response.Result.Content.ReadAsStringAsync().Result

                            Expect.isTrue inFlight "The request should reach its handler"
                            Expect.equal (status, body) (200, "finished") "A request that finishes in time gets its answer"
                            Expect.isLessThan elapsed 3_000L "stop should not wait longer than the request took"

                        testCase "stop - interrupts a request still running at the shutdown timeout and answers it 503"
                        <| fun () ->
                            let entered = new ManualResetEventSlim false
                            let finalized = new ManualResetEventSlim false
                            let routes =
                                get "/stuck" (fun _ ->
                                    (FIO.succeedWith(fun () -> entered.Set()).FlatMap(fun () -> FIO.never ()))
                                        .Ensuring(FIO.succeedWith (fun () -> finalized.Set())))
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    let config = ServerConfig.create "127.0.0.1" port |> ServerConfig.withShutdownTimeout 300
                                    Server.startServerWithRuntime config routes runtime)
                            use client = new HttpClient()
                            let response = client.GetAsync $"http://127.0.0.1:{port}/stuck"
                            let inFlight = entered.Wait(TimeSpan.FromSeconds 5.0)

                            let clock = Stopwatch.StartNew()
                            runWithTimeout (runtime :> FIORuntime) (Server.stop server) |> ignore
                            let elapsed = clock.ElapsedMilliseconds
                            let status = int response.Result.StatusCode
                            let ranFinalizer = finalized.Wait(TimeSpan.FromSeconds 5.0)

                            Expect.isTrue inFlight "The request should reach its handler"
                            Expect.equal status 503 "A request cut off by the shutdown is answered 503"
                            Expect.isTrue ranFinalizer "The interrupted handler's finalizer should have run"
                            Expect.isLessThan elapsed 3_000L "stop should take about its shutdown timeout, not Kestrel's 30 s"

                        testCase "stop - answers 503 at the cut to a handler that returned but awaits its children"
                        <| fun () ->
                            let started = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
                            let child: FIO<unit, exn> =
                                (FIO.succeedWith(fun () -> started.TrySetResult() |> ignore).FlatMap(fun () -> FIO.never ()))
                                    .Ensuring(FIO.sleep (TimeSpan.FromSeconds 3.0))
                            let routes =
                                get "/scoped" (fun _ ->
                                    child.Fork().FlatMap(fun _ -> FIO.awaitUnitTask started.Task id).Map(fun () -> Response.okText "done"))
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    let config = ServerConfig.create "127.0.0.1" port |> ServerConfig.withShutdownTimeout 300
                                    Server.startServerWithRuntime config routes runtime)
                            use client = new HttpClient()
                            let response = client.GetAsync $"http://127.0.0.1:{port}/scoped"
                            let inFlight = started.Task.Wait(TimeSpan.FromSeconds 5.0)

                            let clock = Diagnostics.Stopwatch.StartNew()
                            let stopping = runtime.Run(Server.stop server)
                            let status = int response.Result.StatusCode
                            let answeredAfter = clock.ElapsedMilliseconds
                            stopping.UnsafeResult() |> ignore

                            Expect.isTrue inFlight "The handler's child should start"
                            Expect.equal status 503 "A request still in flight at the cut is answered 503"
                            Expect.isLessThan answeredAfter 2_000L "The answer should not wait for the child's finalizer"

                        testCase "stop - answers 503 to a request that arrives while it waits for one in flight"
                        <| fun () ->
                            let entered = new ManualResetEventSlim false
                            let routes =
                                get "/slow" (fun _ ->
                                    FIO.succeedWith(fun () -> entered.Set())
                                        .FlatMap(fun () -> FIO.sleep (TimeSpan.FromMilliseconds 1200.0))
                                        .FlatMap(fun () -> FIO.succeed (Response.okText "finished")))
                            use runtime = new DefaultRuntime()
                            let port, server =
                                startOnFreePort (runtime :> FIORuntime) (fun port ->
                                    Server.startServerWithRuntime (ServerConfig.create "127.0.0.1" port) routes runtime)
                            use client = new HttpClient()
                            let first = client.GetAsync $"http://127.0.0.1:{port}/slow"
                            let inFlight = entered.Wait(TimeSpan.FromSeconds 5.0)

                            let stopping = runtime.Run(Server.stop server)
                            Thread.Sleep 300
                            use late = new HttpClient()
                            let refused = int (late.GetAsync($"http://127.0.0.1:{port}/slow").Result.StatusCode)
                            stopping.UnsafeResult() |> ignore
                            let firstStatus = int first.Result.StatusCode

                            Expect.isTrue inFlight "The first request should reach its handler"
                            Expect.equal refused 503 "A request that arrives during the stop is refused"
                            Expect.equal firstStatus 200 "The request already in flight still finishes"

                        testCase "handler - is interrupted when its client disconnects"
                        <| fun () ->
                            let entered = new ManualResetEventSlim false
                            let finalized = new ManualResetEventSlim false
                            let routes =
                                get "/stuck" (fun _ ->
                                    (FIO.succeedWith(fun () -> entered.Set()).FlatMap(fun () -> FIO.never ()))
                                        .Ensuring(FIO.succeedWith (fun () -> finalized.Set())))

                            let inFlight, interrupted =
                                withTestHttpServer routes (fun port ->
                                    let client = new TcpClient()
                                    client.Connect("127.0.0.1", port)
                                    let request = Encoding.ASCII.GetBytes "GET /stuck HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
                                    client.GetStream().Write(request, 0, request.Length)
                                    let inFlight = entered.Wait(TimeSpan.FromSeconds 5.0)
                                    client.Close()
                                    inFlight, finalized.Wait(TimeSpan.FromSeconds 5.0))

                            Expect.isTrue inFlight "The request should reach its handler"
                            Expect.isTrue interrupted "A handler nobody waits for any more should be interrupted and run its finalizer"

                        testCase "start - configuration found at run time adds no listener"
                        <| fun () ->
                            let routes = get "/ping" (HttpHandler.text "pong")

                            let extraListening =
                                onFreePort (fun extraPort ->
                                    Environment.SetEnvironmentVariable("Kestrel__Endpoints__Extra__Url", $"http://127.0.0.1:{extraPort}")
                                    try
                                        withTestHttpServer routes (fun _ ->
                                            let listening = isListening extraPort
                                            if listening && tryGet extraPort "/ping" <> Some "pong" then None else Some listening)
                                    finally
                                        Environment.SetEnvironmentVariable("Kestrel__Endpoints__Extra__Url", null))

                            Expect.isFalse extraListening "Only the configured address should be bound"
                    ]

                testList
                    "Signals"
                    [
                        testUnix "runServer - the first SIGTERM stops the server, which cuts the request still running and returns" (fun () ->
                            let exitCode, status, output =
                                withStuckServer "server" 500 (fun child _ stuck ->
                                    child.Signal "TERM"
                                    let exitCode = child.WaitForExit(TimeSpan.FromSeconds 20.0)
                                    exitCode, int stuck.Result.StatusCode, child.Output)

                            Expect.equal exitCode (Some 0) $"runServer should return once the server has stopped; output: {output}"
                            Expect.equal status 503 "The request still running at the cut is answered 503")

                        testUnix "runServer - a second SIGTERM ends the process while the stop waits for a request" (fun () ->
                            let refused, exitCode =
                                withStuckServer "server" 0 (fun child port _ ->
                                    child.Signal "TERM"
                                    let refused = waitForRefusal port
                                    child.Signal "TERM"
                                    refused, child.WaitForExit(TimeSpan.FromSeconds 10.0))

                            Expect.isTrue refused "The first SIGTERM should start the stop"
                            Expect.isSome exitCode "A second SIGTERM must end the process while the stop waits for a request")

                        testUnix "runServer - in a FIOApp, a SIGTERM stops the server and the app exits as interrupted" (fun () ->
                            let exitCode, status, output =
                                withStuckServer "app-server" 500 (fun child _ stuck ->
                                    child.Signal "TERM"
                                    let exitCode = child.WaitForExit(TimeSpan.FromSeconds 20.0)
                                    exitCode, int stuck.Result.StatusCode, child.Output)

                            Expect.equal exitCode (Some 130) $"The app should end as interrupted once the server has stopped; output: {output}"
                            Expect.equal status 503 "The request still running at the cut is answered 503")

                        testUnix "runServer - in a FIOApp, a second Ctrl+C (SIGINT) ends the process while the stop waits for a request" (fun () ->
                            let refused, exitCode =
                                withStuckServer "app-server" 0 (fun child port _ ->
                                    child.Signal "INT"
                                    let refused = waitForRefusal port
                                    child.Signal "INT"
                                    refused, child.WaitForExit(TimeSpan.FromSeconds 10.0))

                            Expect.isTrue refused "The first Ctrl+C should start the stop"
                            Expect.isSome exitCode "A second Ctrl+C must end the process while the stop waits for a request")
                    ]
            ]
    )
