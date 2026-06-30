namespace FIO.Http

open FIO.DSL

open System

/// A transformation applied to a route collection.
type Middleware<'E> = Routes<'E> -> Routes<'E>

[<RequireQualifiedAccess>]
module Middleware =

    /// Applies a middleware to a route collection.
    let apply (middleware: Middleware<'E>) (routes: Routes<'E>) =
        middleware routes

    /// Composes two middlewares, running the inner one first.
    let compose (outer: Middleware<'E>) (inner: Middleware<'E>) =
        fun routes -> outer (inner routes)

    /// Creates a middleware that transforms every handler in a route collection.
    let create (transform: HttpHandler<'E> -> HttpHandler<'E>) =
        fun routes -> Routes.transform transform routes

    /// Creates a middleware that runs an effect before each handler.
    let before (effect: HttpRequest -> FIO<unit, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    do! effect request
                    return! handler request
                }

    /// Creates a middleware that runs an effect after each handler, given the request and response.
    let after (effect: HttpRequest -> HttpResponse -> FIO<unit, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    do! effect request response
                    return response
                }

    /// Creates a middleware that wraps each handler with the given function.
    let wrap (wrapper: HttpHandler<'E> -> HttpHandler<'E>) =
        create wrapper

    /// Creates a middleware that adds a header to every response.
    let addHeader (name: string) (value: string) =
        create <| fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    return HttpResponse.withHeader name value response
                }

    /// Creates a middleware that adds several headers to every response.
    let addHeaders (headers: (string * string) list) =
        create <| fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    let withHeaders =
                        headers
                        |> List.fold (fun resp (name, value) -> HttpResponse.withHeader name value resp) response
                    return withHeaders
                }

    /// Creates a middleware that logs each request.
    let logging (logger: HttpRequest -> FIO<unit, 'E>) =
        before logger

    /// Creates a middleware that logs each request and its response.
    let loggingFull (logger: HttpRequest -> HttpResponse -> FIO<unit, 'E>) =
        after logger

    /// Creates a middleware that assigns a request id and echoes it in an X-Request-ID header.
    let requestId (generator: unit -> string) =
        create <| fun handler ->
            fun request ->
                let reqId = generator ()
                let requestWithId = HttpRequest.withMetadata "RequestId" (box reqId) request
                fio {
                    let! response = handler requestWithId
                    return HttpResponse.withHeader "X-Request-ID" reqId response
                }

    /// Creates a middleware that fails over to 408 Request Timeout if a handler exceeds the duration.
    let timeout (duration: TimeSpan) (onError: exn -> 'E) =
        create <| fun handler ->
            fun request ->
                let handlerEffect = handler request
                let timeoutEffect = (FIO.succeed Response.requestTimeout).Delay duration onError
                handlerEffect.RaceFirst timeoutEffect

    /// Creates a middleware that applies CORS headers and handles preflight requests.
    let cors
        (allowedOrigins: string list)
        (allowedMethods: string list)
        (allowedHeaders: string list) =
        create <| fun handler ->
            fun request ->
                let origin = HttpRequest.header "Origin" request

                let isAllowed =
                    match origin with
                    | Some o -> allowedOrigins |> List.contains o || allowedOrigins |> List.contains "*"
                    | None -> true

                if not isAllowed then
                    FIO.succeed Response.forbidden
                elif request.Method = HttpMethod.OPTIONS then
                    let preflightResponse =
                        Response.noContent
                        |> HttpResponse.withHeader "Access-Control-Allow-Origin" (origin |> Option.defaultValue "*")
                        |> HttpResponse.withHeader "Access-Control-Allow-Methods" (String.concat ", " allowedMethods)
                        |> HttpResponse.withHeader "Access-Control-Allow-Headers" (String.concat ", " allowedHeaders)
                        |> HttpResponse.withHeader "Access-Control-Max-Age" "86400"
                        |> HttpResponse.withHeader "Vary" "Origin"

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
                            |> HttpResponse.withHeader "Vary" "Origin"

                        return withCors
                    }

    /// Creates a middleware that enforces HTTP Basic authentication.
    let basicAuth (authenticate: string -> string -> FIO<bool, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when
                        authHeader.StartsWith("Basic ", StringComparison.OrdinalIgnoreCase)
                        && authHeader.Length > 6
                        ->
                        let encoded = authHeader.Substring 6
                        let decoded =
                            try
                                Some(Text.Encoding.UTF8.GetString(Convert.FromBase64String encoded))
                            with _ ->
                                None
                        match decoded with
                        | Some credentials ->
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
                }

    /// Creates a middleware that enforces Bearer-token authentication, attaching the authenticated user to the request.
    let bearerAuth (authenticate: string -> FIO<'A option, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    match HttpRequest.header "Authorization" request with
                    | Some authHeader when
                        authHeader.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase)
                        && authHeader.Length > 7
                        ->
                        let token = authHeader.Substring 7
                        let! userOpt = authenticate token

                        match userOpt with
                        | Some user ->
                            let requestWithUser = HttpRequest.withMetadata "User" (box user) request
                            return! handler requestWithUser
                        | None -> return Response.unauthorized
                    | _ -> return Response.unauthorized
                }

    /// Creates a middleware that converts handler errors into responses.
    let errorHandler (handleError: 'E -> HttpResponse) =
        create <| fun handler -> 
            fun request -> 
                (handler request).CatchAll(fun error -> FIO.succeed (handleError error))

module MiddlewareOperators =

    /// Applies a middleware to a route collection. Operator form of <c>Middleware.apply</c>.
    let (@@) (routes: Routes<'E>) (middleware: Middleware<'E>) =
        Middleware.apply middleware routes

    /// Composes two middlewares. Operator form of <c>Middleware.compose</c>.
    let (+++) (middleware: Middleware<'E>) (middleware': Middleware<'E>) =
        Middleware.compose middleware middleware'
