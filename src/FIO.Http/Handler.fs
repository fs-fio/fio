namespace FIO.Http

open FIO.DSL

open System.Text.Json

/// HTTP handler - a function from Request to FIO Response.
type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>

/// Functions for creating and composing HTTP handlers.
[<RequireQualifiedAccess>]
module HttpHandler =

    /// Creates a handler that always returns the given response.
    /// <param name="response">The HTTP response to return.</param>
    /// <returns>A handler that produces the given response.</returns>
    let succeed (response: HttpResponse) : HttpHandler<'E> = fun _ -> FIO.succeed response

    /// Creates a handler that always fails with the given error.
    /// <param name="error">The error to fail with.</param>
    /// <returns>A handler that always fails.</returns>
    let fail (error: 'E) : HttpHandler<'E> = fun _ -> FIO.fail error

    /// Creates a handler from an FIO effect.
    /// <param name="effect">The FIO effect producing the response.</param>
    /// <returns>A handler that ignores the request and runs the given effect.</returns>
    let fromFIO (effect: FIO<HttpResponse, 'E>) : HttpHandler<'E> = fun _ -> effect

    /// Creates a handler from a pure function.
    /// <param name="f">The function that transforms a request into a response.</param>
    /// <returns>A handler that applies the function, catching exceptions as errors.</returns>
    let fromFunc (f: HttpRequest -> HttpResponse) : HttpHandler<exn> =
        fun request -> FIO.attempt ((fun () -> f request), id)

    /// Creates a handler from a function returning an FIO effect.
    /// <param name="f">The function that transforms a request into an FIO effect.</param>
    /// <returns>A handler wrapping the given function.</returns>
    let fromFuncZIO (f: HttpRequest -> FIO<HttpResponse, 'E>) : HttpHandler<'E> = f

    /// Handler that returns HTTP 200 OK.
    /// <returns>A handler that produces a 200 OK response.</returns>
    let ok<'E> : HttpHandler<'E> = succeed Response.ok

    /// Handler that returns HTTP 200 OK with JSON body.
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>A handler that produces a 200 OK response with JSON body.</returns>
    let okJson (value: 'T) : HttpHandler<'E> = succeed (Response.okJson value)

    /// Handler that returns HTTP 200 OK with plain text body.
    /// <param name="text">The text content.</param>
    /// <returns>A handler that produces a 200 OK response with text body.</returns>
    let text (text: string) : HttpHandler<'E> = succeed (Response.okText text)

    /// Handler that returns HTTP 200 OK with HTML body.
    /// <param name="html">The HTML content.</param>
    /// <returns>A handler that produces a 200 OK response with HTML body.</returns>
    let html (html: string) : HttpHandler<'E> = succeed (Response.okHtml html)

    /// Handler that returns HTTP 200 OK with binary body.
    /// <param name="bytes">The binary content.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A handler that produces a 200 OK response with binary body.</returns>
    let bytes (bytes: byte[]) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okBytes bytes contentType)

    /// Handler that returns HTTP 200 OK with stream body.
    /// <param name="stream">The stream to send.</param>
    /// <param name="length">The optional content length.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A handler that produces a 200 OK response with stream body.</returns>
    let stream (stream: System.IO.Stream) (length: int64 option) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okStream stream length contentType)

    /// Handler that returns HTTP 204 No Content.
    /// <returns>A handler that produces a 204 No Content response.</returns>
    let noContent<'E> : HttpHandler<'E> = succeed Response.noContent

    /// Handler that returns HTTP 404 Not Found.
    /// <returns>A handler that produces a 404 Not Found response.</returns>
    let notFound<'E> : HttpHandler<'E> = succeed Response.notFound

    /// Handler that returns HTTP 404 Not Found with text message.
    /// <param name="message">The not-found message.</param>
    /// <returns>A handler that produces a 404 Not Found response with text body.</returns>
    let notFoundText (message: string) : HttpHandler<'E> = succeed (Response.notFoundText message)

    /// Handler that returns HTTP 400 Bad Request.
    /// <returns>A handler that produces a 400 Bad Request response.</returns>
    let badRequest<'E> : HttpHandler<'E> = succeed Response.badRequest

    /// Handler that returns HTTP 400 Bad Request with text message.
    /// <param name="message">The error message.</param>
    /// <returns>A handler that produces a 400 Bad Request response with text body.</returns>
    let badRequestText (message: string) : HttpHandler<'E> =
        succeed (Response.badRequestText message)

    /// Handler that returns HTTP 400 Bad Request with JSON error.
    /// <param name="error">The error value to serialize as JSON.</param>
    /// <returns>A handler that produces a 400 Bad Request response with JSON body.</returns>
    let badRequestJson (error: 'T) : HttpHandler<'E> = succeed (Response.badRequestJson error)

    /// Handler that returns HTTP 500 Internal Server Error.
    /// <returns>A handler that produces a 500 Internal Server Error response.</returns>
    let serverError<'E> : HttpHandler<'E> = succeed Response.internalServerError

    /// Handler that returns HTTP 500 Internal Server Error with text message.
    /// <param name="message">The error message.</param>
    /// <returns>A handler that produces a 500 Internal Server Error response with text body.</returns>
    let serverErrorText (message: string) : HttpHandler<'E> =
        succeed (Response.internalServerErrorText message)

    /// Handler that returns HTTP 401 Unauthorized.
    /// <returns>A handler that produces a 401 Unauthorized response.</returns>
    let unauthorized<'E> : HttpHandler<'E> = succeed Response.unauthorized

    /// Handler that returns HTTP 403 Forbidden.
    /// <returns>A handler that produces a 403 Forbidden response.</returns>
    let forbidden<'E> : HttpHandler<'E> = succeed Response.forbidden

    /// Handler that returns a redirect response.
    /// <param name="location">The redirect location URI.</param>
    /// <param name="permanent">Whether the redirect is permanent (301) or temporary (302).</param>
    /// <returns>A handler that produces a redirect response.</returns>
    let redirect (location: string) (permanent: bool) : HttpHandler<'E> =
        if permanent then
            succeed (Response.movedPermanently location)
        else
            succeed (Response.found location)

    /// Maps a function over the response.
    /// <param name="f">The function to apply to the response.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>A handler that applies the function to the original handler's response.</returns>
    let map (f: HttpResponse -> HttpResponse) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return f response
            }

    /// Binds a handler to a function that produces a new handler.
    /// <param name="f">The function that takes a response and produces a new handler.</param>
    /// <param name="handler">The handler to bind.</param>
    /// <returns>A handler that chains the original handler's response through the function.</returns>
    let bind (f: HttpResponse -> HttpHandler<'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return! f response request
            }

    /// Runs two handlers and combines their responses.
    /// <param name="combiner">The function that merges two responses into one.</param>
    /// <param name="handler1">The first handler to run.</param>
    /// <param name="handler2">The second handler to run.</param>
    /// <returns>A handler that runs both handlers and combines their responses.</returns>
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

    /// Tries the first handler, falling back to the second on failure.
    /// <param name="handler2">The fallback handler to use on failure.</param>
    /// <param name="handler1">The primary handler to try first.</param>
    /// <returns>A handler that attempts handler1 and falls back to handler2 on failure.</returns>
    let orElse (handler2: HttpHandler<'E>) (handler1: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler1 request <|> handler2 request

    /// Maps a function over the error type.
    /// <param name="f">The function to apply to the error.</param>
    /// <param name="handler">The handler whose error type to transform.</param>
    /// <returns>A handler with the error type mapped by the function.</returns>
    let mapError (f: 'E1 -> 'E2) (handler: HttpHandler<'E1>) : HttpHandler<'E2> =
        fun request -> (handler request).MapError f

    /// Runs a side effect after the handler without changing the response.
    /// <param name="f">The side effect to run with the response.</param>
    /// <param name="handler">The handler to tap into.</param>
    /// <returns>A handler that runs the original handler, executes the side effect, and returns the original response.</returns>
    let tap (f: HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                do! f response
                return response
            }

    /// Runs a side effect with request and response without changing the response.
    /// <param name="f">Side effect function receiving request and response.</param>
    /// <param name="handler">The handler to tap into.</param>
    /// <returns>A handler that runs the original handler, executes the side effect, and returns the original response.</returns>
    let tapWithRequest (f: HttpRequest -> HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                do! f request response
                return response
            }

    /// Handler that returns HTTP 200 OK (reader pattern).
    /// <returns>A handler that produces a 200 OK response.</returns>
    let ask<'E> : HttpHandler<'E> = fun _ -> FIO.succeed Response.ok

    /// Extracts a value from the request.
    /// <param name="f">The function that extracts a value from the request.</param>
    /// <returns>A function that produces an FIO effect with the extracted value.</returns>
    let asks (f: HttpRequest -> 'T) : HttpRequest -> FIO<'T, 'E> = fun request -> FIO.succeed (f request)

    /// Runs a handler with a modified request.
    /// <param name="f">The function that transforms the request before passing it to the handler.</param>
    /// <param name="handler">The handler to run with the modified request.</param>
    /// <returns>A handler that transforms the request and delegates to the original handler.</returns>
    let local (f: HttpRequest -> HttpRequest) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler (f request)

    /// Runs a handler conditionally, returning fallback otherwise.
    /// <param name="predicate">The condition to check against the request.</param>
    /// <param name="handler">The handler to run when the predicate is true.</param>
    /// <param name="fallback">The response when condition is false.</param>
    /// <returns>A handler that runs conditionally based on the predicate.</returns>
    let when' (predicate: HttpRequest -> bool) (handler: HttpHandler<'E>) (fallback: HttpResponse) : HttpHandler<'E> =
        fun request ->
            if predicate request then
                handler request
            else
                FIO.succeed fallback

    /// Runs one of two handlers based on a condition.
    /// <param name="predicate">The condition to check against the request.</param>
    /// <param name="trueHandler">The handler to run when the predicate is true.</param>
    /// <param name="falseHandler">The handler to run when the predicate is false.</param>
    /// <returns>A handler that delegates to one of two handlers based on the predicate.</returns>
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

    /// Parses the request body as JSON.
    /// <param name="options">Optional JSON serializer options.</param>
    /// <returns>A function that parses the request body as JSON, producing an FIO effect.</returns>
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

    /// Handler that parses JSON body and processes it.
    /// <param name="f">The function that processes the deserialized body and produces a response effect.</param>
    /// <param name="onError">Maps parse exceptions to the error type.</param>
    /// <returns>A handler that deserializes the JSON body and processes it.</returns>
    let jsonBody<'T, 'E> (f: 'T -> FIO<HttpResponse, 'E>) (onError: exn -> 'E) : HttpHandler<'E> =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> None request).MapError onError
                return! f body
            }

    /// Handler that parses JSON body with custom options and processes it.
    /// <param name="options">The JSON serializer options to use.</param>
    /// <param name="f">The function that processes the deserialized body and produces a response effect.</param>
    /// <param name="onError">Maps parse exceptions to the error type.</param>
    /// <returns>A handler that deserializes the JSON body with custom options and processes it.</returns>
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

/// Operators for HTTP handler composition.
module HttpHandlerOperators =

    /// Maps a function over the handler's response.
    /// <param name="f">The function to apply to the response.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>A handler with the response mapped by the function.</returns>
    let (<!>) f handler = HttpHandler.map f handler

    /// Binds a handler to a function producing a new handler.
    /// <param name="handler">The handler to bind.</param>
    /// <param name="f">The function that takes a response and produces a new handler.</param>
    /// <returns>A handler that chains the original handler's response through the function.</returns>
    let (>>=) handler f = HttpHandler.bind f handler

    /// Tries the first handler, falling back to the second on failure.
    /// <param name="handler1">The primary handler to try first.</param>
    /// <param name="handler2">The fallback handler to use on failure.</param>
    /// <returns>A handler that attempts handler1 and falls back to handler2 on failure.</returns>
    let (<|>) handler1 handler2 = HttpHandler.orElse handler2 handler1
