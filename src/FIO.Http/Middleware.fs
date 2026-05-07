namespace FIO.Http

open FIO.DSL

open System

/// <summary>Represents middleware that transforms a route collection.</summary>
type Middleware<'E> = Routes<'E> -> Routes<'E>

/// <summary>Provides functions for creating and composing middleware.</summary>
module Middleware =
    /// <summary>Transforms a route collection by applying the given middleware.</summary>
    /// <param name="middleware">The middleware to apply.</param>
    /// <param name="routes">The routes to transform.</param>
    /// <returns>The transformed routes.</returns>
    let apply (middleware: Middleware<'E>) (routes: Routes<'E>) : Routes<'E> = middleware routes

    /// <summary>Combines two middleware functions, applying the inner first then the outer.</summary>
    /// <param name="outer">The outer middleware (applied second).</param>
    /// <param name="inner">The inner middleware (applied first).</param>
    /// <returns>A composed middleware that applies inner then outer.</returns>
    let compose (outer: Middleware<'E>) (inner: Middleware<'E>) : Middleware<'E> = fun routes -> outer (inner routes)

    /// <summary>Creates middleware from a handler transformation function.</summary>
    /// <param name="transform">The function that transforms HTTP handlers.</param>
    /// <returns>Middleware that applies the transformation to all route handlers.</returns>
    let create (transform: HttpHandler<'E> -> HttpHandler<'E>) : Middleware<'E> =
        fun routes -> Routes.transform transform routes

    /// <summary>Creates middleware that runs an effect before each handler.</summary>
    /// <param name="effect">The effect to run before each handler, receiving the request.</param>
    /// <returns>Middleware that runs the effect before delegating to the handler.</returns>
    let before (effect: HttpRequest -> FIO<unit, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    do! effect request
                    return! handler request
                })

    /// <summary>Creates middleware that runs an effect after each handler.</summary>
    /// <param name="effect">The effect to run after each handler, receiving the request and response.</param>
    /// <returns>Middleware that runs the handler then executes the effect.</returns>
    let after (effect: HttpRequest -> HttpResponse -> FIO<unit, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    do! effect request response
                    return response
                })

    /// <summary>Creates middleware from a handler wrapper function.</summary>
    /// <param name="wrapper">The function that wraps HTTP handlers.</param>
    /// <returns>Middleware that applies the wrapper to all route handlers.</returns>
    let wrap (wrapper: HttpHandler<'E> -> HttpHandler<'E>) : Middleware<'E> = create wrapper

    /// <summary>Creates middleware that adds a header to all responses.</summary>
    /// <param name="name">The header name.</param>
    /// <param name="value">The header value.</param>
    /// <returns>Middleware that adds the specified header to every response.</returns>
    let addHeader (name: string) (value: string) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    return HttpResponse.withHeader name value response
                })

    /// <summary>Creates middleware that adds multiple headers to all responses.</summary>
    /// <param name="headers">The list of header name-value pairs.</param>
    /// <returns>Middleware that adds the specified headers to every response.</returns>
    let addHeaders (headers: (string * string) list) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    let! response = handler request

                    let withHeaders =
                        headers
                        |> List.fold (fun resp (name, value) -> HttpResponse.withHeader name value resp) response

                    return withHeaders
                })

    /// <summary>Creates middleware that runs a logging effect before each handler.</summary>
    /// <param name="logger">The logging effect receiving the request.</param>
    /// <returns>Middleware that logs before each handler.</returns>
    let logging (logger: HttpRequest -> FIO<unit, 'E>) : Middleware<'E> = before logger

    /// <summary>Creates middleware that runs a logging effect after each handler with request and response.</summary>
    /// <param name="logger">The logging effect receiving request and response.</param>
    /// <returns>Middleware that logs after each handler.</returns>
    let loggingFull (logger: HttpRequest -> HttpResponse -> FIO<unit, 'E>) : Middleware<'E> = after logger

    /// <summary>Creates middleware that generates a request ID and attaches it to metadata and the X-Request-ID response header.</summary>
    /// <param name="generator">The request ID generator function.</param>
    /// <returns>Middleware that attaches a generated request ID to metadata and X-Request-ID header.</returns>
    let requestId (generator: unit -> string) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                let reqId = generator ()
                let requestWithId = HttpRequest.withMetadata "RequestId" (box reqId) request

                fio {
                    let! response = handler requestWithId
                    return HttpResponse.withHeader "X-Request-ID" reqId response
                })

    /// <summary>Creates middleware that races the handler against a timeout, returning whichever completes first.</summary>
    /// <param name="duration">The timeout duration.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>Middleware that races the handler against a timeout effect.</returns>
    let timeout (duration: TimeSpan) (onError: exn -> 'E) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                let handlerEffect = handler request

                let timeoutEffect =
                    FIO.sleep(duration, onError).FlatMap(fun () -> FIO.succeed Response.requestTimeout)

                handlerEffect.Race(timeoutEffect))

    /// <summary>Creates CORS middleware that adds cross-origin headers and responds to preflight OPTIONS requests.</summary>
    /// <param name="allowedOrigins">The list of allowed origins (use "*" for all).</param>
    /// <param name="allowedMethods">The list of allowed HTTP methods.</param>
    /// <param name="allowedHeaders">The list of allowed headers.</param>
    /// <returns>Middleware that adds CORS headers and produces preflight responses.</returns>
    let cors
        (allowedOrigins: string list)
        (allowedMethods: string list)
        (allowedHeaders: string list)
        : Middleware<'E> =
        create (fun handler ->
            fun request ->
                let origin = HttpRequest.header "Origin" request

                let isAllowed =
                    match origin with
                    | Some o -> allowedOrigins |> List.contains o || allowedOrigins |> List.contains "*"
                    | None -> true

                if not isAllowed then
                    FIO.succeed Response.forbidden
                elif request.Method = HttpMethod.OPTIONS then
                    // Short-circuit preflight: return 204 with CORS headers without calling handler
                    let preflightResponse =
                        Response.noContent
                        |> HttpResponse.withHeader "Access-Control-Allow-Origin" (origin |> Option.defaultValue "*")
                        |> HttpResponse.withHeader "Access-Control-Allow-Methods" (String.concat ", " allowedMethods)
                        |> HttpResponse.withHeader "Access-Control-Allow-Headers" (String.concat ", " allowedHeaders)
                        |> HttpResponse.withHeader "Access-Control-Max-Age" "86400"

                    FIO.succeed preflightResponse
                else
                    fio {
                        let! response = handler request

                        let withCors =
                            response
                            |> HttpResponse.withHeader
                                "Access-Control-Allow-Origin"
                                (origin |> Option.defaultValue "*")
                            |> HttpResponse.withHeader
                                "Access-Control-Allow-Methods"
                                (String.concat ", " allowedMethods)
                            |> HttpResponse.withHeader
                                "Access-Control-Allow-Headers"
                                (String.concat ", " allowedHeaders)

                        return withCors
                    })

    /// <summary>Creates middleware that validates HTTP Basic authentication credentials before delegating to the handler.</summary>
    /// <param name="authenticate">The authentication function checking username and password.</param>
    /// <returns>Middleware that validates Basic auth credentials before delegating to the handler.</returns>
    let basicAuth (authenticate: string -> string -> FIO<bool, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when authHeader.StartsWith "Basic " ->
                        let encoded = authHeader.Substring 6

                        let decoded =
                            try
                                Some(Text.Encoding.UTF8.GetString(Convert.FromBase64String encoded))
                            with _ ->
                                None

                        match decoded with
                        | Some credentials ->
                            // Split only on the first colon to allow colons in passwords
                            match credentials.Split([| ':' |], 2) with
                            | [| username; password |] ->
                                let! isValid = authenticate username password

                                if isValid then
                                    return! handler request
                                else
                                    return Response.unauthorizedWith "Basic realm=\"Protected\""
                            | _ -> return Response.unauthorizedWith "Basic realm=\"Protected\""
                        | None -> return Response.unauthorizedWith "Basic realm=\"Protected\""
                    | _ -> return Response.unauthorizedWith "Basic realm=\"Protected\""
                })

    /// <summary>Creates middleware that validates Bearer token credentials and attaches the authenticated user to request metadata.</summary>
    /// <param name="authenticate">The authentication function validating the token.</param>
    /// <returns>Middleware that validates Bearer tokens before delegating to the handler.</returns>
    let bearerAuth (authenticate: string -> FIO<'User option, 'E>) : Middleware<'E> =
        create (fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when authHeader.StartsWith "Bearer " && authHeader.Length > 7 ->
                        let token = authHeader.Substring 7
                        let! userOpt = authenticate token

                        match userOpt with
                        | Some user ->
                            let requestWithUser = HttpRequest.withMetadata "User" (box user) request
                            return! handler requestWithUser
                        | None -> return Response.unauthorized
                    | _ -> return Response.unauthorized
                })

    /// <summary>Creates middleware that catches handler errors and converts them to HTTP responses.</summary>
    /// <param name="handleError">The error to response mapping function.</param>
    /// <returns>Middleware that catches handler errors and converts them to HTTP responses.</returns>
    let errorHandler (handleError: 'E -> HttpResponse) : Middleware<'E> =
        create (fun handler -> fun request -> (handler request).CatchAll(fun error -> FIO.succeed (handleError error)))

/// <summary>Provides operators for middleware composition.</summary>
module MiddlewareOperators =

    /// <summary>Transforms a route collection by applying the given middleware.</summary>
    /// <param name="routes">The routes to transform.</param>
    /// <param name="middleware">The middleware to apply.</param>
    /// <returns>The transformed routes.</returns>
    let (@@) (routes: Routes<'E>) (middleware: Middleware<'E>) : Routes<'E> = Middleware.apply middleware routes

    /// <summary>Combines two middleware functions into one.</summary>
    /// <param name="middleware1">The first middleware (applied second).</param>
    /// <param name="middleware2">The second middleware (applied first).</param>
    /// <returns>A composed middleware.</returns>
    let (+++) (middleware1: Middleware<'E>) (middleware2: Middleware<'E>) : Middleware<'E> =
        Middleware.compose middleware1 middleware2
