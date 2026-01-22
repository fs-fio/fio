namespace FSharp.FIO.Http

open FSharp.FIO.DSL

open System
open System.IO
open System.Web
open System.Text
open System.Buffers
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

/// <summary>
/// Functions for converting between ASP.NET Core and FIO HTTP types.
/// </summary>
module KestrelBridge =

    /// <summary>
    /// Validates a path segment for security issues.
    /// </summary>
    let private isValidPathSegment (segment: string) : bool =
        not (segment.Contains ".." ||
             segment.Contains '\x00' ||
             segment.Contains '\\' ||
             segment.StartsWith '.')

    /// <summary>
    /// Validates a header name according to RFC 7230.
    /// </summary>
    let private isValidHeaderName (name: string) : bool =
        not (String.IsNullOrWhiteSpace name) &&
        name |> Seq.forall (fun c ->
            c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c = '-' || c = '_')

    /// <summary>
    /// URL-decodes query parameter values.
    /// </summary>
    let private decodeQueryValue (value: string) : string =
        try
            HttpUtility.UrlDecode value
        with
        | _ -> value // If decode fails, return original

    /// <summary>
    /// Converts an ASP.NET Core HttpContext to an FIO HttpRequest asynchronously.
    /// </summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    let convertRequestAsync (ctx: HttpContext) (maxBodySize: int64) : Task<Result<FSharp.FIO.Http.HttpRequest, string>> =
        task {
            // Validate size BEFORE buffering
            if ctx.Request.ContentLength.HasValue && ctx.Request.ContentLength.Value > maxBodySize then
                return Error $"Request body size ({ctx.Request.ContentLength.Value} bytes) exceeds maximum allowed size ({maxBodySize} bytes)"
            else
                let method = HttpMethod.FromString ctx.Request.Method
                let path = ctx.Request.Path.Value

                let segments =
                    path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.toList

                match segments |> List.tryFind (fun seg -> not (isValidPathSegment seg)) with
                | Some _ ->
                    return Error "Invalid path segment detected: path traversal or malicious characters not allowed"
                | None ->
                    let queryParams =
                        ctx.Request.Query
                        |> Seq.map (fun kvp ->
                            kvp.Key,
                            kvp.Value |> Seq.map decodeQueryValue |> Seq.toList)
                        |> Map.ofSeq

                    let headers =
                        ctx.Request.Headers
                        |> Seq.filter (fun kvp -> isValidHeaderName kvp.Key)
                        |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toList)
                        |> Map.ofSeq

                    let! body =
                        task {
                            if ctx.Request.ContentLength.HasValue && ctx.Request.ContentLength.Value > 0L then
                                // Use ArrayPool for efficient buffer management
                                let length = int ctx.Request.ContentLength.Value
                                let buffer = ArrayPool<byte>.Shared.Rent(length)
                                try
                                    let mutable totalRead = 0
                                    while totalRead < length do
                                        let! bytesRead = ctx.Request.Body.ReadAsync(buffer, totalRead, length - totalRead)
                                        if bytesRead = 0 then
                                            totalRead <- length // Exit loop on EOF
                                        else
                                            totalRead <- totalRead + bytesRead
                                    // Copy exact bytes needed (avoid returning pooled buffer)
                                    let result = Array.zeroCreate<byte> totalRead
                                    Buffer.BlockCopy(buffer, 0, result, 0, totalRead)
                                    return RequestBody.Bytes result
                                finally
                                    ArrayPool<byte>.Shared.Return(buffer)
                            else
                                return RequestBody.Empty
                        }

                    // Generate a unique request ID for tracking
                    let requestId = Guid.NewGuid().ToString()

                    return Ok {
                        Method = method
                        Path = path
                        PathSegments = segments
                        QueryParams = queryParams
                        Headers = headers
                        Body = body
                        Metadata = Map.empty |> Map.add "RequestId" (box requestId)
                    }
        }

    /// <summary>
    /// Writes an FIO HttpResponse to an ASP.NET Core HttpContext.
    /// Note: Stream bodies are NOT automatically disposed. Callers are responsible for disposing streams.
    /// </summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="response">The FIO HTTP response to write.</param>
    let writeResponse (ctx: HttpContext) (response: FSharp.FIO.Http.HttpResponse) : Task =
        task {
            let! bodyBytes =
                task {
                    match response.Body with
                    | Empty ->
                        return None
                    | Text text ->
                        let bytes = Encoding.UTF8.GetBytes text
                        return Some bytes
                    | Bytes bytes ->
                        return Some bytes
                    | Json obj ->
                        // Pre-allocate with reasonable initial capacity for JSON
                        use ms = new MemoryStream(256)
                        do! JsonSerializer.SerializeAsync(ms, obj)
                        return Some (ms.ToArray())
                    | Stream _ ->
                        return None
                }

            ctx.Response.StatusCode <- int response.Status

            for kvp in response.Headers do
                for value in kvp.Value do
                    ctx.Response.Headers.Append(kvp.Key, value)

            // Write the body
            match response.Body with
            | Empty ->
                ()
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
            | Stream (stream, lengthOpt) ->
                if isNull stream then
                    invalidArg "stream" "Response stream cannot be null"

                match lengthOpt with
                | Some length -> ctx.Response.ContentLength <- Nullable<int64> length
                | None -> () // No Content-Length header; Kestrel will use chunked transfer encoding

                // Copy stream to response body
                // Note: Caller is responsible for disposing the stream
                do! stream.CopyToAsync ctx.Response.Body
        }

    /// <summary>
    /// Logs an error with request context for diagnostics.
    /// </summary>
    let private logError (ctx: HttpContext) (errorType: string) (error: exn option) : unit =
        let timestamp = System.DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss.fff")
        let method = ctx.Request.Method
        let path = ctx.Request.Path.Value
        let query = ctx.Request.QueryString.Value
        let errorMsg = error |> Option.map (fun e -> e.Message) |> Option.defaultValue "Unknown error"

        eprintfn $"[{timestamp}] ERROR [{errorType}] {method} {path}{query} - {errorMsg}"

        // Log full exception details to stderr for debugging (not sent to client)
        match error with
        | Some ex when not (isNull ex.StackTrace) ->
            eprintfn $"  Stack trace: {ex.StackTrace}"
        | _ -> ()

    /// <summary>
    /// Handles an incoming HTTP request using the FIO runtime and routes.
    /// </summary>
    /// <param name="runtime">The FIO runtime to execute effects.</param>
    /// <param name="routes">The routes to dispatch to.</param>
    /// <param name="maxBodySize">Maximum allowed request body size in bytes.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    let handleRequest
        (runtime: FSharp.FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (maxBodySize: int64)
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
                        else 400 // Bad Request
                    let errorBytes = Encoding.UTF8.GetBytes errorMsg
                    do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
                | Ok request ->
                    let effect = Routes.dispatch request routes
                    let fiber = runtime.Run effect
                    match! fiber.Task() with
                    | Succeeded response ->
                        do! writeResponse ctx response
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
