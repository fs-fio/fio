namespace FIO.Http

open FIO.DSL

open System.Text.Json

/// <summary>
/// HTTP handler - a function from Request to FIO Response.
/// </summary>
type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>

/// <summary>
/// Functions for creating and composing HTTP handlers.
/// </summary>
[<RequireQualifiedAccess>]
module HttpHandler =

    /// <summary>
    /// Creates a handler that always returns the given response.
    /// </summary>
    /// <param name="response">The response to return.</param>
    /// <returns>The HTTP handler.</returns>
    let succeed (response: HttpResponse) : HttpHandler<'E> = fun _ -> FIO.succeed response

    /// <summary>
    /// Creates a handler that always fails with the given error.
    /// </summary>
    /// <param name="error">The error to return.</param>
    /// <returns>The HTTP handler.</returns>
    let fail (error: 'E) : HttpHandler<'E> = fun _ -> FIO.fail error

    /// <summary>
    /// Creates a handler from an FIO effect.
    /// </summary>
    /// <param name="effect">The FIO effect to run.</param>
    /// <returns>The HTTP handler.</returns>
    let fromFIO (effect: FIO<HttpResponse, 'E>) : HttpHandler<'E> = fun _ -> effect

    /// <summary>
    /// Creates a handler from a pure function.
    /// </summary>
    /// <param name="f">The function to wrap.</param>
    /// <returns>The HTTP handler.</returns>
    let fromFunc (f: HttpRequest -> HttpResponse) : HttpHandler<exn> =
        fun request -> FIO.attempt ((fun () -> f request), id)

    /// <summary>
    /// Creates a handler from a function returning an FIO effect.
    /// </summary>
    /// <param name="f">The function to wrap.</param>
    /// <returns>The HTTP handler.</returns>
    let fromFuncZIO (f: HttpRequest -> FIO<HttpResponse, 'E>) : HttpHandler<'E> = f

    /// <summary>
    /// Handler that returns HTTP 200 OK.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let ok<'E> : HttpHandler<'E> = succeed Response.ok

    /// <summary>
    /// Handler that returns HTTP 200 OK with JSON body.
    /// </summary>
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>The HTTP handler.</returns>
    let okJson (value: 'T) : HttpHandler<'E> = succeed (Response.okJson value)

    /// <summary>
    /// Handler that returns HTTP 200 OK with plain text body.
    /// </summary>
    /// <param name="text">The text content.</param>
    /// <returns>The HTTP handler.</returns>
    let text (text: string) : HttpHandler<'E> = succeed (Response.okText text)

    /// <summary>
    /// Handler that returns HTTP 200 OK with HTML body.
    /// </summary>
    /// <param name="html">The HTML content.</param>
    /// <returns>The HTTP handler.</returns>
    let html (html: string) : HttpHandler<'E> = succeed (Response.okHtml html)

    /// <summary>
    /// Handler that returns HTTP 200 OK with binary body.
    /// </summary>
    /// <param name="bytes">The binary content.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>The HTTP handler.</returns>
    let bytes (bytes: byte[]) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okBytes bytes contentType)

    /// <summary>
    /// Handler that returns HTTP 200 OK with stream body.
    /// </summary>
    /// <param name="stream">The stream to send.</param>
    /// <param name="length">The optional content length.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>The HTTP handler.</returns>
    let stream (stream: System.IO.Stream) (length: int64 option) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okStream stream length contentType)

    /// <summary>
    /// Handler that returns HTTP 204 No Content.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let noContent<'E> : HttpHandler<'E> = succeed Response.noContent

    /// <summary>
    /// Handler that returns HTTP 404 Not Found.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let notFound<'E> : HttpHandler<'E> = succeed Response.notFound

    /// <summary>
    /// Handler that returns HTTP 404 Not Found with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns>The HTTP handler.</returns>
    let notFoundText (message: string) : HttpHandler<'E> = succeed (Response.notFoundText message)

    /// <summary>
    /// Handler that returns HTTP 400 Bad Request.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let badRequest<'E> : HttpHandler<'E> = succeed Response.badRequest

    /// <summary>
    /// Handler that returns HTTP 400 Bad Request with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns>The HTTP handler.</returns>
    let badRequestText (message: string) : HttpHandler<'E> =
        succeed (Response.badRequestText message)

    /// <summary>
    /// Handler that returns HTTP 400 Bad Request with JSON error.
    /// </summary>
    /// <param name="error">The error to serialize as JSON.</param>
    /// <returns>The HTTP handler.</returns>
    let badRequestJson (error: 'T) : HttpHandler<'E> = succeed (Response.badRequestJson error)

    /// <summary>
    /// Handler that returns HTTP 500 Internal Server Error.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let serverError<'E> : HttpHandler<'E> = succeed Response.internalServerError

    /// <summary>
    /// Handler that returns HTTP 500 Internal Server Error with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns>The HTTP handler.</returns>
    let serverErrorText (message: string) : HttpHandler<'E> =
        succeed (Response.internalServerErrorText message)

    /// <summary>
    /// Handler that returns HTTP 401 Unauthorized.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let unauthorized<'E> : HttpHandler<'E> = succeed Response.unauthorized

    /// <summary>
    /// Handler that returns HTTP 403 Forbidden.
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let forbidden<'E> : HttpHandler<'E> = succeed Response.forbidden

    /// <summary>
    /// Handler that returns a redirect response.
    /// </summary>
    /// <param name="location">The redirect location URI.</param>
    /// <param name="permanent">Whether the redirect is permanent (301) or temporary (302).</param>
    /// <returns>The HTTP handler.</returns>
    let redirect (location: string) (permanent: bool) : HttpHandler<'E> =
        if permanent then
            succeed (Response.movedPermanently location)
        else
            succeed (Response.found location)

    /// <summary>
    /// Maps a function over the response.
    /// </summary>
    /// <param name="f">The transformation function.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>The transformed HTTP handler.</returns>
    let map (f: HttpResponse -> HttpResponse) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return f response
            }

    /// <summary>
    /// Binds a handler to a function that produces a new handler.
    /// </summary>
    /// <param name="f">The function producing the next handler.</param>
    /// <param name="handler">The initial handler.</param>
    /// <returns>The composed HTTP handler.</returns>
    let bind (f: HttpResponse -> HttpHandler<'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return! f response request
            }

    /// <summary>
    /// Runs two handlers and combines their responses.
    /// </summary>
    /// <param name="combiner">The function to combine responses.</param>
    /// <param name="handler1">The first handler.</param>
    /// <param name="handler2">The second handler.</param>
    /// <returns>The combined HTTP handler.</returns>
    let zipWith
        (combiner: HttpResponse -> HttpResponse -> HttpResponse)
        (handler1: HttpHandler<'E>)
        (handler2: HttpHandler<'E>)
        : HttpHandler<'E> =
        fun request ->
            fio {
                let! response1 = handler1 request
                let! response2 = handler2 request
                return combiner response1 response2
            }

    /// <summary>
    /// Tries the first handler, falling back to the second on failure.
    /// </summary>
    /// <param name="handler2">The fallback handler.</param>
    /// <param name="handler1">The primary handler.</param>
    /// <returns>The HTTP handler with fallback.</returns>
    let orElse (handler2: HttpHandler<'E>) (handler1: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler1 request <|> handler2 request

    /// <summary>
    /// Maps a function over the error type.
    /// </summary>
    /// <param name="f">The error transformation function.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>The HTTP handler with mapped error type.</returns>
    let mapError (f: 'E1 -> 'E2) (handler: HttpHandler<'E1>) : HttpHandler<'E2> =
        fun request -> (handler request).MapError f

    /// <summary>
    /// Runs a side effect after the handler without changing the response.
    /// </summary>
    /// <param name="f">The side effect function.</param>
    /// <param name="handler">The handler to wrap.</param>
    /// <returns>The HTTP handler with side effect.</returns>
    let tap (f: HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                do! f response
                return response
            }

    /// <summary>
    /// Runs a side effect with request and response without changing the response.
    /// </summary>
    /// <param name="f">The side effect function receiving request and response.</param>
    /// <param name="handler">The handler to wrap.</param>
    /// <returns>The HTTP handler with side effect.</returns>
    let tapWithRequest (f: HttpRequest -> HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                do! f request response
                return response
            }

    /// <summary>
    /// Handler that returns HTTP 200 OK (reader pattern).
    /// </summary>
    /// <returns>The HTTP handler.</returns>
    let ask<'E> : HttpHandler<'E> = fun _ -> FIO.succeed Response.ok

    /// <summary>
    /// Extracts a value from the request.
    /// </summary>
    /// <param name="f">The extraction function.</param>
    /// <returns>A function from request to effect returning the extracted value.</returns>
    let asks (f: HttpRequest -> 'T) : HttpRequest -> FIO<'T, 'E> = fun request -> FIO.succeed (f request)

    /// <summary>
    /// Runs a handler with a modified request.
    /// </summary>
    /// <param name="f">The request transformation function.</param>
    /// <param name="handler">The handler to run.</param>
    /// <returns>The HTTP handler with modified request.</returns>
    let local (f: HttpRequest -> HttpRequest) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler (f request)

    /// <summary>
    /// Runs a handler conditionally, returning fallback otherwise.
    /// </summary>
    /// <param name="predicate">The condition to check.</param>
    /// <param name="handler">The handler to run when condition is true.</param>
    /// <param name="fallback">The response when condition is false.</param>
    /// <returns>The conditional HTTP handler.</returns>
    let when' (predicate: HttpRequest -> bool) (handler: HttpHandler<'E>) (fallback: HttpResponse) : HttpHandler<'E> =
        fun request ->
            if predicate request then
                handler request
            else
                FIO.succeed fallback

    /// <summary>
    /// Runs one of two handlers based on a condition.
    /// </summary>
    /// <param name="predicate">The condition to check.</param>
    /// <param name="trueHandler">The handler when condition is true.</param>
    /// <param name="falseHandler">The handler when condition is false.</param>
    /// <returns>The conditional HTTP handler.</returns>
    let ifElse
        (predicate: HttpRequest -> bool)
        (trueHandler: HttpHandler<'E>)
        (falseHandler: HttpHandler<'E>)
        : HttpHandler<'E> =
        fun request ->
            if predicate request then
                trueHandler request
            else
                falseHandler request

    /// <summary>
    /// Parses the request body as JSON.
    /// </summary>
    /// <param name="options">Optional JSON serializer options.</param>
    /// <returns>A function from request to effect returning the parsed body.</returns>
    let parseJsonBody<'T> (options: JsonSerializerOptions option) : HttpRequest -> FIO<'T, exn> =
        fun request ->
            FIO.attempt (
                (fun () ->
                    let bodyStr = request.Body.AsString()

                    match options with
                    | Some opts -> JsonSerializer.Deserialize<'T>(bodyStr, opts)
                    | None -> JsonSerializer.Deserialize<'T> bodyStr),
                id
            )

    /// <summary>
    /// Handler that parses JSON body and processes it.
    /// </summary>
    /// <param name="f">The function to process the parsed body.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>The HTTP handler.</returns>
    let jsonBody<'T, 'E> (f: 'T -> FIO<HttpResponse, 'E>) (onError: exn -> 'E) : HttpHandler<'E> =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> None request).MapError onError
                return! f body
            }

    /// <summary>
    /// Handler that parses JSON body with custom options and processes it.
    /// </summary>
    /// <param name="options">The JSON serializer options.</param>
    /// <param name="f">The function to process the parsed body.</param>
    /// <param name="onError">Maps exceptions to the error type.</param>
    /// <returns>The HTTP handler.</returns>
    let jsonBodyWith<'T, 'E>
        (options: JsonSerializerOptions)
        (f: 'T -> FIO<HttpResponse, 'E>)
        (onError: exn -> 'E)
        : HttpHandler<'E> =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> (Some options) request).MapError onError
                return! f body
            }

/// <summary>
/// Operators for HTTP handler composition.
/// </summary>
module HttpHandlerOperators =

    /// <summary>
    /// Maps a function over the response.
    /// </summary>
    /// <param name="f">The function to apply to the response.</param>
    /// <param name="handler">The HTTP handler to map over.</param>
    /// <returns>The transformed HTTP handler.</returns>
    let (<!>) f handler = HttpHandler.map f handler

    /// <summary>
    /// Binds a handler to a function producing a new handler.
    /// </summary>
    /// <param name="handler">The HTTP handler to bind.</param>
    /// <param name="f">The function producing a new handler.</param>
    /// <returns>The composed HTTP handler.</returns>
    let (>>=) handler f = HttpHandler.bind f handler

    /// <summary>
    /// Tries the first handler, falling back to the second on failure.
    /// </summary>
    /// <param name="handler1">The primary HTTP handler.</param>
    /// <param name="handler2">The fallback HTTP handler.</param>
    /// <returns>The HTTP handler with fallback.</returns>
    let (<|>) handler1 handler2 = HttpHandler.orElse handler2 handler1
