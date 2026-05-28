namespace FIO.Http

open FIO.DSL

open System
open System.IO
open System.Web
open System.Text
open System.Buffers
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

/// <summary>Provides functions for converting between ASP.NET Core and FIO HTTP types.</summary>
module KestrelBridge =

    /// <summary>Returns pre-configured JSON serializer options with camelCase naming for response serialization.</summary>
    /// <returns>The default JSON serialization options.</returns>
    let DefaultJsonOptions =
        let options =
            JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

        options.TypeInfoResolver <- Serialization.Metadata.DefaultJsonTypeInfoResolver()
        options.MakeReadOnly()
        options

    /// <summary>Returns whether the given path segment is safe from path-traversal and control-character attacks.</summary>
    /// <param name="segment">The path segment to validate.</param>
    /// <returns><c>true</c> if the segment contains no traversal patterns or malicious characters; <c>false</c> otherwise.</returns>
    let private isValidPathSegment (segment: string) : bool =
        not (
            segment.Contains ".."
            || segment.Contains '\x00'
            || segment.Contains '\\'
            || segment.StartsWith '.'
        )

    /// <summary>Returns whether the given string is a valid HTTP header name containing only alphanumeric characters, hyphens, and underscores.</summary>
    /// <param name="name">The header name to validate.</param>
    /// <returns><c>true</c> if the name is non-empty and contains only valid characters; <c>false</c> otherwise.</returns>
    let private isValidHeaderName (name: string) : bool =
        not (String.IsNullOrWhiteSpace name)
        && name
           |> Seq.forall (fun c ->
               c >= 'a' && c <= 'z'
               || c >= 'M' && c <= 'Z'
               || c >= '0' && c <= '9'
               || c = '-'
               || c = '_')

    /// <summary>Returns the URL-decoded form of a query parameter value, falling back to the original on decode failure.</summary>
    /// <param name="value">The URL-encoded query parameter value.</param>
    /// <returns>The decoded string, or the original value if decoding fails.</returns>
    let private decodeQueryValue (value: string) : string =
        try
            HttpUtility.UrlDecode value
        with _ ->
            value // If decode fails, return original

    /// <summary>Creates an FIO HTTP request by converting an ASP.NET Core HttpContext.</summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    /// <returns>A task returning Ok with the request or Error with a message.</returns>
    let convertRequestAsync (ctx: HttpContext) (maxBodySize: int64) : Task<Result<FIO.Http.HttpRequest, string>> =
        task {
            if
                ctx.Request.ContentLength.HasValue
                && ctx.Request.ContentLength.Value > maxBodySize
            then
                return
                    Error
                        $"Request body size ({ctx.Request.ContentLength.Value} bytes) exceeds maximum allowed size ({maxBodySize} bytes)"
            else
                let method = HttpMethod.fromString ctx.Request.Method
                let path = ctx.Request.Path.Value

                let segments =
                    path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

                match segments |> List.tryFind (fun seg -> not (isValidPathSegment seg)) with
                | Some _ ->
                    return Error "Invalid path segment detected: path traversal or malicious characters not allowed"
                | None ->
                    let queryParams =
                        ctx.Request.Query
                        |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.map decodeQueryValue |> Seq.toList)
                        |> Map.ofSeq

                    let headers =
                        ctx.Request.Headers
                        |> Seq.filter (fun kvp -> isValidHeaderName kvp.Key)
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
                                        Error
                                            $"Request body size ({contentLength} bytes) exceeds supported buffer size ({Int32.MaxValue} bytes)"
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
                                                Error
                                                    $"Request body truncated: received {totalRead} of {length} declared bytes"
                                        else
                                            let result = Array.zeroCreate<byte> totalRead
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
                                                    Some
                                                        $"Request body size ({totalRead} bytes) exceeds maximum allowed size ({maxBodySize} bytes)"

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

    /// <summary>Builds an ASP.NET Core response from an FIO HTTP response using the specified JSON options.</summary>
    /// <param name="jsonOptions">JSON serializer options for response serialization.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="response">The FIO HTTP response to write.</param>
    /// <returns>A task that completes when the response has been written.</returns>
    let writeResponseWithOptions
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext)
        (response: FIO.Http.HttpResponse)
        : Task =
        task {
            let! bodyBytes =
                task {
                    match response.Body with
                    | Empty -> return None
                    | Text text ->
                        let bytes = Encoding.UTF8.GetBytes text
                        return Some bytes
                    | Bytes bytes -> return Some bytes
                    | Json obj ->
                        use ms = new MemoryStream(256)
                        do! JsonSerializer.SerializeAsync(ms, obj, jsonOptions)
                        return Some(ms.ToArray())
                    | Stream _ -> return None
                }

            ctx.Response.StatusCode <- int response.Status

            for kvp in response.Headers do
                for value in kvp.Value do
                    ctx.Response.Headers.Append(kvp.Key, value)

            match response.Body with
            | Empty -> ()
            | Text _ ->
                match bodyBytes with
                | Some bytes ->
                    ctx.Response.ContentLength <- int64 bytes.Length
                    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                | None -> ()
            | Bytes _ ->
                match bodyBytes with
                | Some bytes ->
                    ctx.Response.ContentLength <- int64 bytes.Length
                    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                | None -> ()
            | Json _ ->
                match bodyBytes with
                | Some bytes ->
                    ctx.Response.ContentType <- "application/json; charset=utf-8"
                    ctx.Response.ContentLength <- int64 bytes.Length
                    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                | None -> ()
            | Stream(stream, lengthOpt) ->
                if isNull stream then
                    invalidArg "stream" "Response stream cannot be null"

                match lengthOpt with
                | Some length -> ctx.Response.ContentLength <- Nullable<int64> length
                | None -> ()

                try
                    do! stream.CopyToAsync ctx.Response.Body
                finally
                    stream.Dispose()
        }

    /// <summary>Builds an ASP.NET Core response from an FIO HTTP response using optional JSON options.</summary>
    /// <param name="jsonOptions">Optional JSON serializer options. Uses DefaultJsonOptions if None.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="response">The FIO HTTP response to write.</param>
    /// <returns>A task that completes when the response has been written.</returns>
    let writeResponseWith
        (jsonOptions: JsonSerializerOptions option)
        (ctx: HttpContext)
        (response: FIO.Http.HttpResponse)
        : Task =
        match jsonOptions with
        | Some options -> writeResponseWithOptions options ctx response
        | None -> writeResponseWithOptions DefaultJsonOptions ctx response

    /// <summary>Builds an ASP.NET Core response from an FIO HTTP response using default JSON options.</summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="response">The FIO HTTP response to write.</param>
    /// <returns>A task that completes when the response has been written.</returns>
    let writeResponse (ctx: HttpContext) (response: FIO.Http.HttpResponse) : Task = writeResponseWith None ctx response

    /// <summary>Transforms the error output by logging the exception details to the console.</summary>
    /// <param name="ctx">The ASP.NET Core HTTP context for the current request.</param>
    /// <param name="errorType">A short label categorizing the error source.</param>
    /// <param name="error">The optional exception whose message and stack trace are logged.</param>
    let private logError (ctx: HttpContext) (errorType: string) (error: exn option) : unit =
        let timestamp = System.DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss.fff")
        let method = ctx.Request.Method
        let path = ctx.Request.Path.Value
        let query = ctx.Request.QueryString.Value

        let errorMsg =
            error |> Option.map (fun e -> e.Message) |> Option.defaultValue "Unknown error"

        eprintfn $"[{timestamp}] ERROR [{errorType}] {method} {path}{query} - {errorMsg}"

        match error with
        | Some ex when not (isNull ex.StackTrace) -> eprintfn $"  Stack trace: {ex.StackTrace}"
        | _ -> ()

    /// <summary>Builds a request-handling pipeline that converts, dispatches, and writes an HTTP response using the given JSON options.</summary>
    /// <param name="runtime">The FIO runtime to execute effects.</param>
    /// <param name="routes">The routes to dispatch to.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    /// <param name="jsonOptions">JSON serializer options for response serialization.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <returns>A task that completes when the request has been processed.</returns>
    let handleRequestWithOptions
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions)
        (ctx: HttpContext)
        : Task =
        task {
            try
                let! requestResult = convertRequestAsync ctx maxBodySize

                match requestResult with
                | Error errorMsg ->
                    logError ctx "RequestValidation" None

                    ctx.Response.StatusCode <-
                        if errorMsg.Contains "body size" then 413 // Payload Too Large
                        elif errorMsg.Contains "path segment" then 400 // Bad Request
                        else 400

                    let errorBytes = Encoding.UTF8.GetBytes errorMsg
                    do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
                | Ok request ->
                    let effect = Routes.dispatch request routes
                    let fiber = runtime.Run effect

                    match! fiber.Task() with
                    | Succeeded response -> do! writeResponseWithOptions jsonOptions ctx response
                    | Failed exn ->
                        logError ctx "HandlerError" (Some exn)
                        ctx.Response.StatusCode <- 500
                        let errorBytes = Encoding.UTF8.GetBytes "Internal Server Error"
                        do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
                    | Interrupted _ ->
                        ctx.Response.StatusCode <- 503
                        let errorBytes = Encoding.UTF8.GetBytes "Service Unavailable"
                        do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
            with exn ->
                logError ctx "UnhandledException" (Some exn)
                ctx.Response.StatusCode <- 500
                let errorBytes = Encoding.UTF8.GetBytes "Internal Server Error"
                do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
        }

    /// <summary>Builds a request-handling pipeline that converts, dispatches, and writes an HTTP response using optional JSON options.</summary>
    /// <param name="runtime">The FIO runtime to execute effects.</param>
    /// <param name="routes">The routes to dispatch to.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    /// <param name="jsonOptions">Optional JSON serializer options. Uses DefaultJsonOptions if None.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <returns>A task that completes when the request has been processed.</returns>
    let handleRequestWith
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (jsonOptions: JsonSerializerOptions option)
        (ctx: HttpContext)
        : Task =
        match jsonOptions with
        | Some options -> handleRequestWithOptions runtime routes maxBodySize options ctx
        | None -> handleRequestWithOptions runtime routes maxBodySize DefaultJsonOptions ctx

    /// <summary>Builds a request-handling pipeline that converts, dispatches, and writes an HTTP response using default JSON options.</summary>
    /// <param name="runtime">The FIO runtime to execute effects.</param>
    /// <param name="routes">The routes to dispatch to.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <returns>A task that completes when the request has been processed.</returns>
    let handleRequest
        (runtime: FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
        (ctx: HttpContext)
        : Task =
        handleRequestWithOptions runtime routes maxBodySize DefaultJsonOptions ctx
