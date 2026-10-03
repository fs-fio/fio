module FIO.Http.Tests.KestrelBridgeTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Http.SimpleRoutes
open FIO.Runtime.Default

open System
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features

open Expecto

type private TrackingStream(bytes: byte array) =
    inherit MemoryStream(bytes)

    member val Disposed = false with get, set

    override this.Dispose (disposing: bool) =
        this.Disposed <- true
        base.Dispose disposing

type private RejectingStream(statusCode: int) =
    inherit Stream()

    let rejection () =
        BadHttpRequestException("Request body too large.", statusCode)

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush () = ()
    override _.Read (_: byte array, _: int, _: int) : int = raise (rejection ())

    override _.ReadAsync (_: byte array, _: int, _: int, _: CancellationToken) : Task<int> =
        Task.FromException<int>(rejection ())

    override _.Seek (_: int64, _: SeekOrigin) : int64 = raise (NotSupportedException())
    override _.SetLength (_: int64) = raise (NotSupportedException())
    override _.Write (_: byte array, _: int, _: int) = raise (NotSupportedException())

type private StartedResponseFeature() =
    inherit HttpResponseFeature()

    override _.HasStarted = true

type private RecordingLifetimeFeature() =
    let mutable requestAborted = CancellationToken.None

    member val Aborted = false with get, set

    interface IHttpRequestLifetimeFeature with
        member _.RequestAborted
            with get () = requestAborted
            and set value = requestAborted <- value

        member this.Abort () = this.Aborted <- true

let private requestContext (method: string) (path: string) (contentLength: int64 option) (body: byte array) =
    let ctx = DefaultHttpContext()
    ctx.Request.Method <- method
    ctx.Request.Path <- PathString(path)
    contentLength |> Option.iter (fun length -> ctx.Request.ContentLength <- Nullable length)
    ctx.Request.Body <- new MemoryStream(body)
    ctx.Response.Body <- new MemoryStream()
    ctx

let private responseText (ctx: DefaultHttpContext) =
    Encoding.UTF8.GetString((ctx.Response.Body :?> MemoryStream).ToArray())

let private convert (ctx: DefaultHttpContext) (maxBodySize: int64) =
    (KestrelBridge.convertRequestAsync ctx maxBodySize).GetAwaiter().GetResult()

let private handle (runtime: DefaultRuntime) (routes: Routes<exn>) (ctx: DefaultHttpContext) =
    let handled = KestrelBridge.handleRequest runtime routes 1024L ctx
    Expect.isTrue (handled.Wait(TimeSpan.FromSeconds 10.0)) "The request should be handled within 10 s"

[<Tests>]
let kestrelBridgeTests =
    testList
        "KestrelBridge"
        [
            testList
                "convertRequestAsync"
                [
                    testCase "convertRequestAsync - rejects a Content-Length over the limit with 413 before reading the body"
                    <| fun () ->
                        let ctx = requestContext "POST" "/upload" (Some 100L) (Array.create 100 (byte 'x'))

                        let result = convert ctx 64L

                        match result with
                        | Error(status, message) ->
                            Expect.equal status 413 "A declared length over the limit must be rejected with 413"
                            Expect.stringContains message "exceeds maximum allowed size" "The 413 must say why"
                        | Ok _ -> failtest "A declared length over the limit must be rejected"
                        Expect.equal
                            ctx.Request.Body.Position
                            0L
                            "The body must not be read once its declared length is over the limit"

                    testList
                        "convertRequestAsync - rejects a malicious path segment with 400"
                        [
                            for name, path in
                                [
                                    "a '..' segment", "/files/../secret"
                                    "a '.' segment", "/files/./secret"
                                    "a segment containing a NUL", "/files/a\u0000b"
                                    "a segment containing a backslash", "/files/a\\b"
                                ] ->
                                testCase name (fun () ->
                                    let ctx = requestContext "GET" path None [||]

                                    let result = convert ctx 1024L

                                    match result with
                                    | Error(status, message) ->
                                        Expect.equal status 400 "A malicious path segment must be rejected with 400"
                                        Expect.stringContains message "Invalid path segment" "The 400 must come from the path guard"
                                    | Ok _ -> failtest $"The path {path} must be rejected")
                        ]

                    testCase "convertRequestAsync - rejects a Content-Length beyond Int32.MaxValue with 413"
                    <| fun () ->
                        let ctx = requestContext "POST" "/upload" (Some(int64 Int32.MaxValue + 1L)) [||]

                        let result = convert ctx Int64.MaxValue

                        match result with
                        | Error(status, message) ->
                            Expect.equal status 413 "A length no buffer can hold must be rejected with 413"
                            Expect.stringContains message "exceeds supported buffer size" "The 413 must come from the buffer-size guard"
                        | Ok _ -> failtest "A length beyond Int32.MaxValue must be rejected"

                    testCase "convertRequestAsync - rejects a body that ends before its Content-Length with 400"
                    <| fun () ->
                        let ctx = requestContext "POST" "/upload" (Some 100L) (Encoding.ASCII.GetBytes "short")

                        let result = convert ctx 1024L

                        match result with
                        | Error(status, message) ->
                            Expect.equal status 400 "A truncated body must be rejected with 400"
                            Expect.stringContains message "received 5 of 100 declared bytes" "The 400 must say how much arrived"
                        | Ok _ -> failtest "A truncated body must be rejected"

                    testCase "convertRequestAsync - rejects a body without Content-Length that exceeds the limit with 413"
                    <| fun () ->
                        let ctx = requestContext "POST" "/upload" None (Array.create 500 (byte 'x'))

                        let result = convert ctx 64L

                        match result with
                        | Error(status, message) ->
                            Expect.equal status 413 "An undeclared body over the limit must be rejected with 413"
                            Expect.stringContains message "exceeds maximum allowed size" "The 413 must say why"
                        | Ok _ -> failtest "An undeclared body over the limit must be rejected"

                    testCase "convertRequestAsync - reads a body without Content-Length within the limit"
                    <| fun () ->
                        let payload = "an undeclared payload"
                        let ctx = requestContext "POST" "/upload" None (Encoding.UTF8.GetBytes payload)

                        let result = convert ctx 64L

                        match result with
                        | Ok request -> Expect.equal (request.Body.AsString()) payload "The body must arrive intact"
                        | Error(status, message) -> failtest $"An undeclared body within the limit must be read, got {status}: {message}"
                ]

            testList
                "writeResponse"
                [
                    testCase "writeResponse - fails for a null stream"
                    <| fun () ->
                        let ctx = requestContext "GET" "/stream" None [||]
                        let response = { Response.ok with Body = ResponseBody.Stream(null, None) }

                        Expect.throwsT<ArgumentException>
                            (fun () -> (KestrelBridge.writeResponse ctx response).GetAwaiter().GetResult())
                            "A null response stream must be rejected"

                    testCase "writeResponse - copies a stream body with its length and disposes the stream"
                    <| fun () ->
                        let payload = Encoding.UTF8.GetBytes "a streamed body"
                        let stream = new TrackingStream(payload)
                        let ctx = requestContext "GET" "/stream" None [||]
                        let response = Response.okStream stream (Some(int64 payload.Length)) "application/octet-stream"

                        (KestrelBridge.writeResponse ctx response).GetAwaiter().GetResult()

                        Expect.equal ctx.Response.ContentLength (Nullable(int64 payload.Length)) "The stream's length must become the Content-Length"
                        Expect.equal (responseText ctx) "a streamed body" "The stream must be copied to the response"
                        Expect.isTrue stream.Disposed "The response stream must be disposed once written"

                    testCase "writeResponse - HEAD skips a stream body but still disposes the stream"
                    <| fun () ->
                        let stream = new TrackingStream(Encoding.UTF8.GetBytes "a streamed body")
                        let ctx = requestContext "HEAD" "/stream" None [||]
                        let response = Response.okStream stream None "application/octet-stream"

                        (KestrelBridge.writeResponse ctx response).GetAwaiter().GetResult()

                        Expect.equal (responseText ctx) "" "HEAD must not copy the stream"
                        Expect.isTrue stream.Disposed "The response stream must be disposed even when HEAD skips it"
                ]

            testList
                "handleRequest"
                [
                    testCase "handleRequest - aborts a response that fails after it has started"
                    <| fun () ->
                        use runtime = new DefaultRuntime(testConfig)
                        let routes = get "/fail" (fun _ -> FIO.fail (exn "The handler failed."))
                        let ctx = requestContext "GET" "/fail" None [||]
                        let lifetime = RecordingLifetimeFeature()
                        ctx.Features.Set<IHttpResponseFeature>(StartedResponseFeature())
                        ctx.Features.Set<IHttpRequestLifetimeFeature> lifetime

                        handle runtime routes ctx

                        Expect.isTrue lifetime.Aborted "A response that fails after it started must be aborted, not left to end cleanly"
                        Expect.equal ctx.Response.StatusCode 200 "A started response's status must be left alone"
                        Expect.equal (responseText ctx) "" "Nothing may be written into a started response"

                    testCase "handleRequest - an error body replaces the failed response's headers"
                    <| fun () ->
                        use runtime = new DefaultRuntime(testConfig)
                        let response =
                            Response.okStream (new FailingStream [||]) (Some 1000L) "application/pdf"
                            |> HttpResponse.withHeader "ETag" "\"v1\""
                        let routes = get "/file" (fun _ -> FIO.succeed response)
                        let ctx = requestContext "GET" "/file" None [||]

                        handle runtime routes ctx

                        Expect.equal ctx.Response.StatusCode 500 "A body that fails before its first byte must be answered with 500"
                        Expect.isFalse (ctx.Response.Headers.ContainsKey "ETag") "The error body must not keep the failed response's headers"
                        Expect.equal ctx.Response.ContentType "text/plain; charset=utf-8" "The error body must not keep the failed response's content type"
                        Expect.equal ctx.Response.ContentLength (Nullable 21L) "The error body must not keep the failed response's length"
                        Expect.equal (responseText ctx) "Internal Server Error" "Only the error body may be written"

                    testCase "handleRequest - answers 503 when the handler is interrupted"
                    <| fun () ->
                        use runtime = new DefaultRuntime(testConfig)
                        let routes = get "/stop" (fun _ -> FIO.interrupt ExplicitInterrupt "The handler was stopped.")
                        let ctx = requestContext "GET" "/stop" None [||]

                        handle runtime routes ctx

                        Expect.equal ctx.Response.StatusCode 503 "An interrupted handler must be answered with 503"
                        Expect.equal (responseText ctx) "Service Unavailable" "The 503 must carry its reason"

                    testCase "handleRequest - answers a BadHttpRequestException with its own status code"
                    <| fun () ->
                        use runtime = new DefaultRuntime(testConfig)
                        let routes = post "/upload" (fun _ -> FIO.succeed Response.ok)
                        let ctx = requestContext "POST" "/upload" None [||]
                        ctx.Request.Body <- new RejectingStream 413

                        handle runtime routes ctx

                        Expect.equal ctx.Response.StatusCode 413 "A request the server rejected must keep the server's status"
                        Expect.equal (responseText ctx) "Request body too large." "The rejection's reason must be passed on"
                ]
        ]
