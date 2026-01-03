/// <summary>
/// Bridge between ASP.NET Core Kestrel and FIO HTTP types.
/// </summary>
namespace FSharp.FIO.Experimental.Http

open System
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

/// <summary>
/// Functions for converting between ASP.NET Core and FIO HTTP types.
/// </summary>
module KestrelBridge =

    /// <summary>
    /// Converts an ASP.NET Core HttpContext to an FIO HttpRequest.
    /// </summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    let convertRequest (ctx: HttpContext) : FSharp.FIO.Experimental.Http.HttpRequest =
        let method = HttpMethod.FromString ctx.Request.Method
        let path = ctx.Request.Path.Value

        let segments =
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        let queryParams =
            ctx.Request.Query
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toList)
            |> Map.ofSeq

        let headers =
            ctx.Request.Headers
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toList)
            |> Map.ofSeq

        // Buffer the request body into memory to avoid stream-once issues
        let body =
            if ctx.Request.ContentLength.HasValue && ctx.Request.ContentLength.Value > 0L then
                use ms = new System.IO.MemoryStream()
                ctx.Request.Body.CopyTo(ms)
                RequestBody.Bytes (ms.ToArray())
            else
                RequestBody.Empty

        // Generate a unique request ID for tracking
        let requestId = System.Guid.NewGuid().ToString()

        {
            Method = method
            Path = path
            PathSegments = segments
            QueryParams = queryParams
            Headers = headers
            Body = body
            Metadata = Map.empty |> Map.add "RequestId" (box requestId)
        }

    /// <summary>
    /// Writes an FIO HttpResponse to an ASP.NET Core HttpContext.
    /// </summary>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    /// <param name="response">The FIO HTTP response to write.</param>
    let writeResponse (ctx: HttpContext) (response: FSharp.FIO.Experimental.Http.HttpResponse) : Task =
        task {
            ctx.Response.StatusCode <- int response.Status

            for kvp in response.Headers do
                for value in kvp.Value do
                    ctx.Response.Headers.Append(kvp.Key, value)

            match response.Body with
            | Empty ->
                ()
            | Text text ->
                let bytes = Encoding.UTF8.GetBytes text
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            | Bytes bytes ->
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            | Json obj ->
                ctx.Response.ContentType <- "application/json; charset=utf-8"
                do! JsonSerializer.SerializeAsync(ctx.Response.Body, obj)
            | Stream (stream, lengthOpt) ->
                match lengthOpt with
                | Some length -> ctx.Response.ContentLength <- Nullable<int64> length
                | None -> () // No Content-Length header; Kestrel will use chunked transfer encoding
                do! stream.CopyToAsync ctx.Response.Body
        }

    /// <summary>
    /// Handles an incoming HTTP request using the FIO runtime and routes.
    /// </summary>
    /// <param name="runtime">The FIO runtime to execute effects.</param>
    /// <param name="routes">The routes to dispatch to.</param>
    /// <param name="ctx">The ASP.NET Core HTTP context.</param>
    let handleRequest
        (runtime: FSharp.FIO.Runtime.Default.DefaultRuntime)
        (routes: Routes<exn>)
        (ctx: HttpContext)
        : Task =
        task {
            try
                let request = convertRequest ctx
                let effect = Routes.dispatch request routes
                let fiber = runtime.Run effect
                let! result = fiber.Task()
                match result with
                | Ok response ->
                    do! writeResponse ctx response
                | Error ex ->
                    ctx.Response.StatusCode <- 500
                    let errorBytes = Encoding.UTF8.GetBytes $"Error: {ex.Message}"
                    do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
            with ex ->
                ctx.Response.StatusCode <- 500
                let errorBytes = Encoding.UTF8.GetBytes $"Fatal error: {ex.Message}"
                do! ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
        }
