namespace FIO.Http

open FIO.DSL

open System

type Middleware<'E> = Routes<'E> -> Routes<'E>

module Middleware =

    let apply (middleware: Middleware<'E>) (routes: Routes<'E>) =
        middleware routes

    let compose (outer: Middleware<'E>) (inner: Middleware<'E>) =
        fun routes -> outer (inner routes)

    let create (transform: HttpHandler<'E> -> HttpHandler<'E>) =
        fun routes -> Routes.transform transform routes

    let before (effect: HttpRequest -> FIO<unit, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    do! effect request
                    return! handler request
                }

    let after (effect: HttpRequest -> HttpResponse -> FIO<unit, 'E>) =
        create <| fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    do! effect request response
                    return response
                }

    let wrap (wrapper: HttpHandler<'E> -> HttpHandler<'E>) =
        create wrapper

    let addHeader (name: string) (value: string) =
        create <| fun handler ->
            fun request ->
                fio {
                    let! response = handler request
                    return HttpResponse.withHeader name value response
                }

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

    let logging (logger: HttpRequest -> FIO<unit, 'E>) =
        before logger

    let loggingFull (logger: HttpRequest -> HttpResponse -> FIO<unit, 'E>) =
        after logger

    let requestId (generator: unit -> string) =
        create <| fun handler ->
            fun request ->
                let reqId = generator ()
                let requestWithId = HttpRequest.withMetadata "RequestId" (box reqId) request
                fio {
                    let! response = handler requestWithId
                    return HttpResponse.withHeader "X-Request-ID" reqId response
                }

    let timeout (duration: TimeSpan) (onError: exn -> 'E) =
        create <| fun handler ->
            fun request ->
                let handlerEffect = handler request
                let timeoutEffect = FIO.succeed(Response.requestTimeout).Delay duration onError
                handlerEffect.RaceFirst timeoutEffect

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

    let bearerAuth (authenticate: string -> FIO<'User option, 'E>) =
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

    let errorHandler (handleError: 'E -> HttpResponse) =
        create <| fun handler -> 
            fun request -> 
                (handler request).CatchAll(fun error -> FIO.succeed (handleError error))

module MiddlewareOperators =

    let (@@) (routes: Routes<'E>) (middleware: Middleware<'E>) =
        Middleware.apply middleware routes

    let (+++) (middleware1: Middleware<'E>) (middleware2: Middleware<'E>) =
        Middleware.compose middleware1 middleware2
