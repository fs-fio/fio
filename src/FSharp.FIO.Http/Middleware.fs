/// <summary>
/// HTTP middleware types and combinators for request/response pipeline.
/// </summary>
namespace FSharp.FIO.Http

open FSharp.FIO.DSL

open System

/// <summary>
/// Middleware - a function that transforms routes.
/// </summary>
type Middleware<'E> = Routes<'E> -> Routes<'E>

/// <summary>
/// Functions for creating and composing middleware.
/// </summary>
module Middleware =
    /// <summary>
    /// Applies middleware to routes.
    /// </summary>
    /// <param name="middleware">The middleware to apply.</param>
    /// <param name="routes">The routes to transform.</param>
    let apply (middleware: Middleware<'E>) (routes: Routes<'E>) : Routes<'E> =
        middleware routes

    /// <summary>
    /// Composes two middleware functions.
    /// The first middleware (outer) is applied after the second (inner).
    /// </summary>
    /// <param name="outer">The outer middleware (applied second).</param>
    /// <param name="inner">The inner middleware (applied first).</param>
    let compose (outer: Middleware<'E>) (inner: Middleware<'E>) : Middleware<'E> =
        fun routes -> outer (inner routes)

    /// <summary>
    /// Creates middleware from a handler transformation function.
    /// </summary>
    /// <param name="transform">The handler transformation function.</param>
    let create (transform: HttpHandler<'E> -> HttpHandler<'E>) : Middleware<'E> =
        fun routes -> Routes.transform transform routes
    
    /// <summary>
    /// Creates middleware that runs an effect before each handler.
    /// </summary>
    /// <param name="effect">The effect to run before handling.</param>
    let before (effect: HttpRequest -> FIO<unit, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    do! effect request
                    return! handler request
                })

    /// <summary>
    /// Creates middleware that runs an effect after each handler.
    /// </summary>
    /// <param name="effect">The effect to run after handling.</param>
    let after (effect: HttpRequest -> HttpResponse -> FIO<unit, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    do! effect request response
                    return response
                })

    /// <summary>
    /// Creates middleware using a wrapper function.
    /// </summary>
    /// <param name="wrapper">The wrapper function for handlers.</param>
    let wrap (wrapper: HttpHandler<'E> -> HttpHandler<'E>) : Middleware<'E> =
        create wrapper

    /// <summary>
    /// Creates middleware that adds a header to all responses.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="value">The header value.</param>
    let addHeader (name: string) (value: string) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    return HttpResponse.withHeader name value response
                })

    /// <summary>
    /// Creates middleware that adds multiple headers to all responses.
    /// </summary>
    /// <param name="headers">The list of header name-value pairs.</param>
    let addHeaders (headers: (string * string) list) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    let withHeaders =
                        headers
                        |> List.fold (fun resp (name, value) ->
                            HttpResponse.withHeader name value resp) response
                    return withHeaders
                })

    /// <summary>
    /// Creates logging middleware that logs requests.
    /// </summary>
    /// <param name="logger">The logging effect.</param>
    let logging (logger: HttpRequest -> FIO<unit, 'E>) : Middleware<'E> =
        before logger

    /// <summary>
    /// Creates logging middleware that logs requests and responses.
    /// </summary>
    /// <param name="logger">The logging effect receiving request and response.</param>
    let loggingFull (logger: HttpRequest -> HttpResponse -> FIO<unit, 'E>) : Middleware<'E> =
        after logger

    /// <summary>
    /// Creates middleware that generates and attaches request IDs.
    /// </summary>
    /// <param name="generator">The request ID generator function.</param>
    let requestId (generator: unit -> string) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                let reqId = generator()
                let requestWithId = HttpRequest.withMetadata "RequestId" (box reqId) request
                fio {
                    let! response = handler requestWithId
                    return HttpResponse.withHeader "X-Request-ID" reqId response
                })

    /// <summary>
    /// Creates middleware that times out requests after a duration.
    /// </summary>
    /// <param name="duration">The timeout duration.</param>
    let timeout (duration: TimeSpan) : Middleware<exn> =
        create (fun handler ->
            fun request ->
                fio {
                    let effect = handler request
                    let timeoutEffect = FIO.Sleep(duration, id) >>= fun () -> FIO.Succeed Response.requestTimeout
                    let! response = effect <|> timeoutEffect
                    return response
                })

    /// <summary>
    /// Creates CORS middleware with support for preflight OPTIONS requests.
    /// </summary>
    /// <param name="allowedOrigins">The list of allowed origins (use "*" for all).</param>
    /// <param name="allowedMethods">The list of allowed HTTP methods.</param>
    /// <param name="allowedHeaders">The list of allowed headers.</param>
    let cors (allowedOrigins: string list) (allowedMethods: string list) (allowedHeaders: string list) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                let origin = HttpRequest.header "Origin" request
                let isAllowed =
                    match origin with
                    | Some o -> allowedOrigins |> List.contains o || allowedOrigins |> List.contains "*"
                    | None -> true

                if not isAllowed then
                    FIO.Succeed Response.forbidden
                else
                    // Handle preflight OPTIONS requests without calling the handler
                    if request.Method = HttpMethod.OPTIONS then
                        let preflightResponse =
                            Response.ok
                            |> HttpResponse.withHeader "Access-Control-Allow-Origin" (origin |> Option.defaultValue "*")
                            |> HttpResponse.withHeader "Access-Control-Allow-Methods" (String.concat ", " allowedMethods)
                            |> HttpResponse.withHeader "Access-Control-Allow-Headers" (String.concat ", " allowedHeaders)
                            |> HttpResponse.withHeader "Access-Control-Max-Age" "86400"
                        FIO.Succeed preflightResponse
                    else
                        fio {
                            let! response = handler request
                            let withCors =
                                response
                                |> HttpResponse.withHeader "Access-Control-Allow-Origin" (origin |> Option.defaultValue "*")
                                |> HttpResponse.withHeader "Access-Control-Allow-Methods" (String.concat ", " allowedMethods)
                                |> HttpResponse.withHeader "Access-Control-Allow-Headers" (String.concat ", " allowedHeaders)
                            return withCors
                        })

    /// <summary>
    /// Creates HTTP Basic authentication middleware.
    /// </summary>
    /// <param name="authenticate">The authentication function checking username and password.</param>
    let basicAuth (authenticate: string -> string -> FIO<bool, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when authHeader.StartsWith "Basic " ->
                        let encoded = authHeader.Substring 6
                        let decoded = Text.Encoding.UTF8.GetString(Convert.FromBase64String encoded)
                        // Split only on the first colon to allow colons in passwords
                        match decoded.Split([|':'|], 2) with
                        | [| username; password |] ->
                            let! isValid = authenticate username password
                            if isValid then
                                return! handler request
                            else
                                return Response.unauthorizedWith "Basic realm=\"Protected\""
                        | _ ->
                            return Response.unauthorizedWith "Basic realm=\"Protected\""
                    | _ ->
                        return Response.unauthorizedWith "Basic realm=\"Protected\""
                })

    /// <summary>
    /// Creates Bearer token authentication middleware.
    /// </summary>
    /// <param name="authenticate">The authentication function validating the token.</param>
    let bearerAuth (authenticate: string -> FIO<'User option, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when authHeader.StartsWith "Bearer " ->
                        let token = authHeader.Substring 7
                        let! userOpt = authenticate token
                        match userOpt with
                        | Some user ->
                            let requestWithUser = HttpRequest.withMetadata "User" (box user) request
                            return! handler requestWithUser
                        | None ->
                            return Response.unauthorized
                    | _ ->
                        return Response.unauthorized
                })

    /// <summary>
    /// Creates compression middleware (placeholder implementation).
    /// </summary>
    /// <param name="level">The compression level.</param>
    let compression (level: int) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    // Actual compression would go here
                    return HttpResponse.withHeader "Content-Encoding" "identity" response
                })

    /// <summary>
    /// Creates error handling middleware that catches errors and converts them to HTTP responses.
    /// </summary>
    /// <param name="handleError">The error to response mapping function.</param>
    let errorHandler (handleError: 'E -> HttpResponse) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                (handler request).CatchAll(fun error -> FIO.Succeed (handleError error)))

/// <summary>
/// Operators for middleware composition.
/// </summary>
module MiddlewareOperators =

    /// <summary>
    /// Applies middleware to routes.
    /// </summary>
    /// <param name="routes">The routes to transform.</param>
    /// <param name="middleware">The middleware to apply.</param>
    let (@@) (routes: Routes<'E>) (middleware: Middleware<'E>) : Routes<'E> =
        Middleware.apply middleware routes

    /// <summary>
    /// Composes two middleware functions.
    /// </summary>
    /// <param name="middleware1">The first middleware.</param>
    /// <param name="middleware2">The second middleware.</param>
    let (+++) (middleware1: Middleware<'E>) (middleware2: Middleware<'E>) : Middleware<'E> =
        Middleware.compose middleware1 middleware2
