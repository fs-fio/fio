namespace FIO.Http

open FIO.DSL

open System
open System.IO
open System.Text
open System.Buffers
open System.Text.Json
open System.Threading.Tasks

open Microsoft.AspNetCore.Http

[<RequireQualifiedAccess>]
module KestrelBridge =

    /// The default JSON serializer options (camelCase).
    let defaultJsonOptions =
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

    /// Converts a Kestrel HttpContext into an HttpRequest, enforcing the maximum body size.
    let convertRequestAsync (ctx: HttpContext) (maxBodySize: int64) =
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
                                    let length = int contentLength
                                    let buffer = ArrayPool<byte>.Shared.Rent length

                                    try
                                        let mutable totalRead = 0
                                        let mutable eof = false

                                        while totalRead < length && not eof do
                                            let! bytesRead =
                                                ctx.Request.Body.ReadAsync(buffer, totalRead, length - totalRead)

                                            if bytesRead = 0 then
                                                eof <- true
                                            else
                                                totalRead <- totalRead + bytesRead

                                        if eof && totalRead < length then
                                            return
                                                Error(
                                                    400,
                                                    $"Request body truncated: received {totalRead} of {length} declared bytes")
                                        else
                                            let result = GC.AllocateUninitializedArray<byte> totalRead
                                            Buffer.BlockCopy(buffer, 0, result, 0, totalRead)

                                            if totalRead = 0 then
                                                return Ok RequestBody.Empty
                                            else
                                                return Ok <| RequestBody.Bytes result
                                    finally
                                        ArrayPool<byte>.Shared.Return(buffer)
                            else
                                let bufferSize = 8192
                                let buffer = ArrayPool<byte>.Shared.Rent bufferSize
                                use stream = new MemoryStream()

                                try
                                    let mutable totalRead = 0L
                                    let mutable loop = true
                                    let mutable sizeError = None

                                    while loop do
                                        let! bytesRead = ctx.Request.Body.ReadAsync(buffer, 0, bufferSize)

                                        if bytesRead = 0 then
                                            loop <- false
                                        else
                                            totalRead <- totalRead + int64 bytesRead

                                            if totalRead > maxBodySize then
                                                sizeError <-
                                                    Some(
                                                        413,
                                                        $"Request body size ({totalRead} bytes) exceeds maximum allowed size ({maxBodySize} bytes)")

                                                loop <- false
                                            else
                                                do! stream.WriteAsync(buffer, 0, bytesRead)

                                    match sizeError with
                                    | Some error -> return Error error
                                    | None ->
                                        if totalRead = 0L then
                                            return Ok RequestBody.Empty
                                        else
                                            return Ok <| RequestBody.Bytes(stream.ToArray())
                                finally
                                    ArrayPool<byte>.Shared.Return(buffer)
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
        (response: FIO.Http.HttpResponse) =
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

                match response.Body with
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
        (response: FIO.Http.HttpResponse) =
        match jsonOptions with
        | Some options -> writeResponseWithOptions options ctx response
        | None -> writeResponseWithOptions defaultJsonOptions ctx response

    /// Writes an HttpResponse to the Kestrel context using the default JSON options.
    let writeResponse (ctx: HttpContext) (response: FIO.Http.HttpResponse) =
        writeResponseWith None ctx response

    let private logError (ctx: HttpContext) (errorType: string) (message: string) (error: exn option) =
        let timestamp = DateTime.UtcNow.ToString "yyyy-MM-dd HH:mm:ss.fff"
        let method = ctx.Request.Method
        let path = ctx.Request.Path.Value
        let query = ctx.Request.QueryString.Value
        let builder = StringBuilder()

        builder.Append $"[{timestamp}] ERROR [{errorType}] {method} {path}{query} - {message}"
        |> ignore

        match error with
        | Some ex when not (isNull ex.StackTrace) ->
            builder.Append $"{Environment.NewLine}  Stack trace: {ex.StackTrace}" |> ignore
        | _ -> ()

        Console.Error.WriteLine(builder.ToString())

    /// Handles a Kestrel request end-to-end with the given runtime, routes, body limit, and JSON options.
    let handleRequestWithOptions
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext) =
        task {
            let writeStatusBody (status: int) (text: string) =
                task {
                    if not ctx.Response.HasStarted then
                        ctx.Response.StatusCode <- status
                        let bytes = Encoding.UTF8.GetBytes text
                        do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                }

            try
                let! requestResult = convertRequestAsync ctx maxBodySize

                match requestResult with
                | Error(status, errorMsg) ->
                    logError ctx "RequestValidation" errorMsg None
                    do! writeStatusBody status errorMsg
                | Ok request ->
                    let effect = Routes.dispatch request routes
                    let fiber = runtime.Run effect

                    match! fiber.Task() with
                    | Succeeded response -> do! writeResponseWithOptions jsonOptions ctx response
                    | Failed ex ->
                        logError ctx "HandlerError" ex.Message (Some ex)
                        do! writeStatusBody 500 "Internal Server Error"
                    | Interrupted _ -> do! writeStatusBody 503 "Service Unavailable"
            with ex ->
                logError ctx "UnhandledException" ex.Message (Some ex)
                do! writeStatusBody 500 "Internal Server Error"
        }

    /// Handles a Kestrel request end-to-end, using the given JSON options or the defaults.
    let handleRequestWith
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions option)
        (ctx: HttpContext) =
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
