namespace FIO.Http

open FIO.DSL

open System
open System.IO
open System.Text
open System.Buffers
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

open Microsoft.AspNetCore.Http

// The requests in flight on one server: its stop refuses new ones, waits for these, and interrupts what is left.
[<Sealed; AllowNullLiteral>]
type internal RequestTracker (runtime: FIO.Runtime.Default.DefaultRuntime) =
    let handlers = ConcurrentDictionary<Guid, Fiber<FIO.Http.HttpResponse, exn>>()

    let drained = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let cut = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

    let mutable inFlight = 0

    let mutable stopping = 0

    [<VolatileField>]
    let mutable cutting = false

    // A fiber is interrupted by an effect, and the runtime may already be gone.
    static member Interrupt (runtime: FIO.Runtime.Default.DefaultRuntime, handler: Fiber<FIO.Http.HttpResponse, exn>) =
        try
            runtime.Run(handler.InterruptNow(): FIO<unit, exn>) |> ignore
        with _ ->
            ()

    member this.TryEnter () =
        Interlocked.Increment &inFlight |> ignore

        if Volatile.Read &stopping = 1 then
            this.Leave()
            false
        else
            true

    member _.Leave () =
        if Interlocked.Decrement &inFlight = 0 && Volatile.Read &stopping = 1 then
            drained.TrySetResult() |> ignore

    member _.Register (handler: Fiber<FIO.Http.HttpResponse, exn>) =
        handlers[handler.Id] <- handler

        if cutting then
            RequestTracker.Interrupt(runtime, handler)

    member _.Unregister (handler: Fiber<FIO.Http.HttpResponse, exn>) =
        handlers.TryRemove handler.Id |> ignore

    // Refuses new requests from here on; the task completes once none is in flight.
    member _.BeginStop () : Task =
        Interlocked.Exchange(&stopping, 1) |> ignore

        if Volatile.Read &inFlight = 0 then
            drained.TrySetResult() |> ignore

        drained.Task

    // Completes at the cut, so a request is answered even when its handler, having returned, waits for its children.
    member _.CutTask : Task =
        cut.Task

    member _.Cut () =
        cutting <- true
        cut.TrySetResult() |> ignore

        for handler in handlers.Values do
            RequestTracker.Interrupt(runtime, handler)

[<RequireQualifiedAccess>]
module KestrelBridge =

    /// The default JSON serializer options (camelCase).
    let defaultJsonOptions : JsonSerializerOptions =
        let options =
            JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
        options.TypeInfoResolver <- Serialization.Metadata.DefaultJsonTypeInfoResolver()
        options.MakeReadOnly()
        options

    let private isValidPathSegment (segment: string) =
        not (
            segment = "."
            || segment = ".."
            || segment.Contains '\x00'
            || segment.Contains '\\'
        )

    let private readBodyAsync (body: Stream) (maxBodySize: int64) (declaredLength: int option) =
        task {
            let bufferSize =
                match declaredLength with
                | Some length -> min length 81920
                | None -> 8192

            let buffer = ArrayPool<byte>.Shared.Rent bufferSize
            use stream = new MemoryStream()

            try
                let mutable totalRead = 0L
                let mutable reading = true
                let mutable failure = None

                while reading do
                    let toRead =
                        match declaredLength with
                        | Some length -> min bufferSize (length - int totalRead)
                        | None -> bufferSize

                    if toRead <= 0 then
                        reading <- false
                    else
                        let! bytesRead = body.ReadAsync(buffer, 0, toRead)

                        if bytesRead = 0 then
                            reading <- false

                            match declaredLength with
                            | Some length when totalRead < int64 length ->
                                failure <-
                                    Some(
                                        400,
                                        $"Request body truncated: received {totalRead} of {length} declared bytes")
                            | _ -> ()
                        else
                            totalRead <- totalRead + int64 bytesRead

                            if totalRead > maxBodySize then
                                failure <-
                                    Some(
                                        413,
                                        $"Request body size ({totalRead} bytes) exceeds maximum allowed size ({maxBodySize} bytes)")
                                reading <- false
                            else
                                do! stream.WriteAsync(buffer, 0, bytesRead)

                match failure with
                | Some error -> return Error error
                | None when totalRead = 0L -> return Ok RequestBody.Empty
                | None -> return Ok <| RequestBody.Bytes(stream.ToArray())
            finally
                ArrayPool<byte>.Shared.Return(buffer, true)
        }

    /// Converts a Kestrel HttpContext into an HttpRequest, enforcing the maximum body size.
    let convertRequestAsync (ctx: HttpContext) (maxBodySize: int64) : Task<Result<FIO.Http.HttpRequest, int * string>> =
        task {
            if
                ctx.Request.ContentLength.HasValue
                && ctx.Request.ContentLength.Value > maxBodySize
            then
                return
                    Error(
                        413,
                        $"Request body size ({ctx.Request.ContentLength.Value} bytes) exceeds maximum allowed size ({maxBodySize} bytes)")
            else
                let method = HttpMethod.fromString ctx.Request.Method

                let path =
                    match ctx.Request.Path.Value with
                    | null -> "/"
                    | value -> value

                let segments =
                    path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

                match segments |> List.tryFind (fun seg -> not (isValidPathSegment seg)) with
                | Some _ ->
                    return
                        Error(400, "Invalid path segment detected: path traversal or malicious characters not allowed")
                | None ->
                    let queryParams =
                        ctx.Request.Query
                        |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toList)
                        |> Map.ofSeq

                    let headers =
                        ctx.Request.Headers
                        |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toList)
                        |> Map.ofSeq

                    let! bodyResult =
                        task {
                            if ctx.Request.ContentLength.HasValue then
                                let contentLength = ctx.Request.ContentLength.Value

                                if contentLength <= 0L then
                                    return Ok RequestBody.Empty
                                elif contentLength > int64 Int32.MaxValue then
                                    return
                                        Error(
                                            413,
                                            $"Request body size ({contentLength} bytes) exceeds supported buffer size ({Int32.MaxValue} bytes)")
                                else
                                    return! readBodyAsync ctx.Request.Body maxBodySize (Some(int contentLength))
                            else
                                return! readBodyAsync ctx.Request.Body maxBodySize None
                        }

                    match bodyResult with
                    | Error error -> return Error error
                    | Ok body ->
                        let requestId = Guid.NewGuid().ToString()
                        return
                            Ok
                            {
                                Method = method
                                Path = path
                                PathSegments = segments
                                QueryParams = queryParams
                                Headers = headers
                                Body = body
                                Metadata = Map.empty |> Map.add "RequestId" (box requestId)
                            }
        }

    /// Writes an HttpResponse to the Kestrel context using the given JSON options.
    let writeResponseWithOptions
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext)
        (response: FIO.Http.HttpResponse) : Task<unit> =
        task {
            let! bodyBytes =
                task {
                    match response.Body with
                    | ResponseBody.Empty -> return None
                    | ResponseBody.Text text ->
                        let bytes = Encoding.UTF8.GetBytes text
                        return Some bytes
                    | ResponseBody.Bytes bytes -> return Some bytes
                    | ResponseBody.Json obj ->
                        return Some(JsonSerializer.SerializeToUtf8Bytes(obj, jsonOptions))
                    | ResponseBody.Stream _ -> return None
                }

            let streamToDispose =
                match response.Body with
                | ResponseBody.Stream(stream, _) when not (isNull stream) -> Some stream
                | _ -> None

            try
                ctx.Response.StatusCode <- int response.Status

                for kvp in response.Headers do
                    for value in kvp.Value do
                        ctx.Response.Headers.Append(kvp.Key, value)

                let isHead = HttpMethods.IsHead ctx.Request.Method
                let status = int response.Status
                // These statuses carry no body, and Kestrel rejects a write or a nonzero Content-Length on them.
                let bodiless = status = 204 || status = 205 || status = 304 || (status >= 100 && status < 200)

                match response.Body with
                | _ when bodiless -> ()
                | ResponseBody.Empty -> ()
                | ResponseBody.Text _ ->
                    match bodyBytes with
                    | Some bytes ->
                        ctx.Response.ContentLength <- int64 bytes.Length
                        if not isHead then
                            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                    | None -> ()
                | ResponseBody.Bytes _ ->
                    match bodyBytes with
                    | Some bytes ->
                        ctx.Response.ContentLength <- int64 bytes.Length
                        if not isHead then
                            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                    | None -> ()
                | ResponseBody.Json _ ->
                    match bodyBytes with
                    | Some bytes ->
                        if not (HeaderHelpers.contains "Content-Type" response.Headers) then
                            ctx.Response.ContentType <- "application/json; charset=utf-8"
                        ctx.Response.ContentLength <- int64 bytes.Length
                        if not isHead then
                            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                    | None -> ()
                | ResponseBody.Stream(stream, lengthOpt) ->
                    if isNull stream then
                        invalidArg "stream" "Response stream cannot be null"
                    match lengthOpt with
                    | Some length -> ctx.Response.ContentLength <- Nullable<int64> length
                    | None -> ()
                    if not isHead then
                        do! stream.CopyToAsync ctx.Response.Body
            finally
                streamToDispose |> Option.iter (fun stream -> stream.Dispose())
        }

    /// Writes an HttpResponse to the Kestrel context, using the given JSON options or the defaults.
    let writeResponseWith
        (jsonOptions: JsonSerializerOptions option)
        (ctx: HttpContext)
        (response: FIO.Http.HttpResponse) : Task<unit> =
        match jsonOptions with
        | Some options -> writeResponseWithOptions options ctx response
        | None -> writeResponseWithOptions defaultJsonOptions ctx response

    /// Writes an HttpResponse to the Kestrel context using the default JSON options.
    let writeResponse (ctx: HttpContext) (response: FIO.Http.HttpResponse) : Task<unit> =
        writeResponseWith None ctx response

    // What a client sent is logged with its control characters escaped, so it cannot begin a log line of its own.
    let private printable (text: string) =
        let breaksLine (c: char) =
            Char.IsControl c || c = '\u2028' || c = '\u2029'

        if isNull text || not (String.exists breaksLine text) then
            text
        else
            let builder = StringBuilder text.Length

            for c in text do
                if breaksLine c then builder.Append($"\\x{int c:X2}") |> ignore
                else builder.Append c |> ignore

            builder.ToString()

    // The query string stays out of the log: it is where tokens travel.
    let private logError (ctx: HttpContext) (errorType: string) (message: string) (error: exn option) =
        let timestamp = DateTime.UtcNow.ToString "yyyy-MM-dd HH:mm:ss.fff"
        let method = printable ctx.Request.Method
        let path = printable ctx.Request.Path.Value
        let builder = StringBuilder()

        builder.Append $"[{timestamp}] ERROR [{errorType}] {method} {path} - {printable message}"
        |> ignore

        match error with
        | Some ex when not (isNull ex.StackTrace) ->
            builder.Append $"{Environment.NewLine}  Stack trace: {ex.StackTrace}" |> ignore
        | _ -> ()

        Console.Error.WriteLine(builder.ToString())

    let internal handleTracked
        (tracker: RequestTracker)
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext) : Task<unit> =
        task {
            let writeStatusBody (status: int) (text: string) =
                task {
                    if ctx.Response.HasStarted then
                        ctx.Abort()
                    else
                        ctx.Response.Clear()
                        ctx.Response.StatusCode <- status
                        ctx.Response.ContentType <- "text/plain; charset=utf-8"
                        let bytes = Encoding.UTF8.GetBytes text
                        ctx.Response.ContentLength <- Nullable(int64 bytes.Length)
                        do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                }

            if not (isNull tracker) && not (tracker.TryEnter()) then
                do! writeStatusBody 503 "Service Unavailable"
            else
                try
                    try
                        let! requestResult = convertRequestAsync ctx maxBodySize

                        match requestResult with
                        | Error(status, errorMsg) ->
                            logError ctx "RequestValidation" errorMsg None
                            do! writeStatusBody status errorMsg
                        | Ok request ->
                            let fiber = runtime.Run(Routes.dispatch request routes)

                            if not (isNull tracker) then
                                tracker.Register fiber

                            // Nobody waits for the answer once the client has gone, so its handler is interrupted.
                            use _ = ctx.RequestAborted.Register(fun () -> RequestTracker.Interrupt(runtime, fiber))

                            try
                                let result = fiber.Task()

                                if not result.IsCompleted then
                                    if isNull tracker then
                                        let! _ = result
                                        ()
                                    else
                                        let! _ = Task.WhenAny(result, tracker.CutTask)
                                        ()

                                if not result.IsCompleted then
                                    do! writeStatusBody 503 "Service Unavailable"
                                else
                                    match result.Result with
                                    | Succeeded response -> do! writeResponseWithOptions jsonOptions ctx response
                                    | Failed ex ->
                                        logError ctx "HandlerError" ex.Message (Some ex)
                                        do! writeStatusBody 500 "Internal Server Error"
                                    | Interrupted interruption ->
                                        match interruption.cause with
                                        | Defect thrown ->
                                            logError ctx "HandlerDefect" thrown.Message (Some thrown)
                                            do! writeStatusBody 500 "Internal Server Error"
                                        | InvalidArgument _
                                        | ResourceExhaustion _ ->
                                            logError ctx "HandlerDefect" interruption.Message None
                                            do! writeStatusBody 500 "Internal Server Error"
                                        | ExplicitInterrupt
                                        | ParentInterrupted _ ->
                                            if not ctx.RequestAborted.IsCancellationRequested then
                                                do! writeStatusBody 503 "Service Unavailable"
                            finally
                                if not (isNull tracker) then
                                    tracker.Unregister fiber
                    with
                    // Kestrel rejects a request it won't read, such as a body over the limit, by throwing this from the read.
                    | :? BadHttpRequestException as rejection ->
                        logError ctx "RequestValidation" rejection.Message None
                        do! writeStatusBody rejection.StatusCode rejection.Message
                    | :? Microsoft.AspNetCore.Connections.ConnectionResetException -> ()
                    | ex ->
                        // A client that left mid-request is not an error of the server, and nobody can be answered.
                        if not ctx.RequestAborted.IsCancellationRequested then
                            logError ctx "UnhandledException" ex.Message (Some ex)
                            do! writeStatusBody 500 "Internal Server Error"
                finally
                    if not (isNull tracker) then
                        tracker.Leave()
        }

    /// Handles a Kestrel request end-to-end with the given runtime, routes, body limit, and JSON options.
    let handleRequestWithOptions
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext) : Task<unit> =
        handleTracked null runtime routes maxBodySize jsonOptions ctx

    /// Handles a Kestrel request end-to-end, using the given JSON options or the defaults.
    let handleRequestWith
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions option)
        (ctx: HttpContext) : Task<unit> =
        match jsonOptions with
        | Some options -> handleRequestWithOptions runtime routes maxBodySize options ctx
        | None -> handleRequestWithOptions runtime routes maxBodySize defaultJsonOptions ctx

    /// Handles a Kestrel request end-to-end using the default JSON options.
    let handleRequest
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (ctx: HttpContext) : Task =
        handleRequestWithOptions runtime routes maxBodySize defaultJsonOptions ctx
