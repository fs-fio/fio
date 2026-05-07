namespace FIO.Http

open FIO.DSL

open System.Text.Json

/// <summary>Represents an HTTP handler that transforms a request into a response effect.</summary>
type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>

/// <summary>Provides functions for creating and composing HTTP handlers.</summary>
[<RequireQualifiedAccess>]
module HttpHandler =

    /// <summary>Creates a handler that always returns the given response.</summary>
    /// <param name="response">The HTTP response to return.</param>
    /// <returns>A handler that produces the given response.</returns>
    let succeed (response: HttpResponse) : HttpHandler<'E> = fun _ -> FIO.succeed response

    /// <summary>Creates a handler that always fails with the given error.</summary>
    /// <param name="error">The error to fail with.</param>
    /// <returns>A handler that always fails.</returns>
    let fail (error: 'E) : HttpHandler<'E> = fun _ -> FIO.fail error

    /// <summary>Creates a handler from an FIO effect.</summary>
    /// <param name="effect">The FIO effect producing the response.</param>
    /// <returns>A handler that ignores the request and runs the given effect.</returns>
    let fromFIO (effect: FIO<HttpResponse, 'E>) : HttpHandler<'E> = fun _ -> effect

    /// <summary>Creates a handler from a pure function, catching exceptions as errors.</summary>
    /// <param name="f">The function that transforms a request into a response.</param>
    /// <returns>A handler that applies the function, catching exceptions as errors.</returns>
    let fromFunc (f: HttpRequest -> HttpResponse) : HttpHandler<exn> =
        fun request -> FIO.attempt ((fun () -> f request), id)

    /// <summary>Creates a handler from a function returning an FIO effect.</summary>
    /// <param name="f">The function that transforms a request into an FIO effect.</param>
    /// <returns>A handler wrapping the given function.</returns>
    let fromFuncZIO (f: HttpRequest -> FIO<HttpResponse, 'E>) : HttpHandler<'E> = f

    /// <summary>Creates a handler that returns an HTTP 200 OK response.</summary>
    /// <returns>A handler that produces a 200 OK response.</returns>
    let ok<'E> : HttpHandler<'E> = succeed Response.ok

    /// <summary>Creates a handler that returns an HTTP 200 OK response with a JSON body.</summary>
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>A handler that produces a 200 OK response with JSON body.</returns>
    let okJson (value: 'T) : HttpHandler<'E> = succeed (Response.okJson value)

    /// <summary>Creates a handler that returns an HTTP 200 OK response with a plain text body.</summary>
    /// <param name="text">The text content.</param>
    /// <returns>A handler that produces a 200 OK response with text body.</returns>
    let text (text: string) : HttpHandler<'E> = succeed (Response.okText text)

    /// <summary>Creates a handler that returns an HTTP 200 OK response with an HTML body.</summary>
    /// <param name="html">The HTML content.</param>
    /// <returns>A handler that produces a 200 OK response with HTML body.</returns>
    let html (html: string) : HttpHandler<'E> = succeed (Response.okHtml html)

    /// <summary>Creates a handler that returns an HTTP 200 OK response with a binary body.</summary>
    /// <param name="bytes">The binary content.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A handler that produces a 200 OK response with binary body.</returns>
    let bytes (bytes: byte[]) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okBytes bytes contentType)

    /// <summary>Creates a handler that returns an HTTP 200 OK response with a stream body.</summary>
    /// <param name="stream">The stream to send.</param>
    /// <param name="length">The optional content length.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A handler that produces a 200 OK response with stream body.</returns>
    let stream (stream: System.IO.Stream) (length: int64 option) (contentType: string) : HttpHandler<'E> =
        succeed (Response.okStream stream length contentType)

    /// <summary>Creates a handler that returns an HTTP 204 No Content response.</summary>
    /// <returns>A handler that produces a 204 No Content response.</returns>
    let noContent<'E> : HttpHandler<'E> = succeed Response.noContent

    /// <summary>Creates a handler that returns an HTTP 404 Not Found response.</summary>
    /// <returns>A handler that produces a 404 Not Found response.</returns>
    let notFound<'E> : HttpHandler<'E> = succeed Response.notFound

    /// <summary>Creates a handler that returns an HTTP 404 Not Found response with a text body.</summary>
    /// <param name="message">The not-found message.</param>
    /// <returns>A handler that produces a 404 Not Found response with text body.</returns>
    let notFoundText (message: string) : HttpHandler<'E> = succeed (Response.notFoundText message)

    /// <summary>Creates a handler that returns an HTTP 400 Bad Request response.</summary>
    /// <returns>A handler that produces a 400 Bad Request response.</returns>
    let badRequest<'E> : HttpHandler<'E> = succeed Response.badRequest

    /// <summary>Creates a handler that returns an HTTP 400 Bad Request response with a text body.</summary>
    /// <param name="message">The error message.</param>
    /// <returns>A handler that produces a 400 Bad Request response with text body.</returns>
    let badRequestText (message: string) : HttpHandler<'E> =
        succeed (Response.badRequestText message)

    /// <summary>Creates a handler that returns an HTTP 400 Bad Request response with a JSON body.</summary>
    /// <param name="error">The error value to serialize as JSON.</param>
    /// <returns>A handler that produces a 400 Bad Request response with JSON body.</returns>
    let badRequestJson (error: 'T) : HttpHandler<'E> = succeed (Response.badRequestJson error)

    /// <summary>Creates a handler that returns an HTTP 500 Internal Server Error response.</summary>
    /// <returns>A handler that produces a 500 Internal Server Error response.</returns>
    let serverError<'E> : HttpHandler<'E> = succeed Response.internalServerError

    /// <summary>Creates a handler that returns an HTTP 500 Internal Server Error response with a text body.</summary>
    /// <param name="message">The error message.</param>
    /// <returns>A handler that produces a 500 Internal Server Error response with text body.</returns>
    let serverErrorText (message: string) : HttpHandler<'E> =
        succeed (Response.internalServerErrorText message)

    /// <summary>Creates a handler that returns an HTTP 401 Unauthorized response.</summary>
    /// <returns>A handler that produces a 401 Unauthorized response.</returns>
    let unauthorized<'E> : HttpHandler<'E> = succeed Response.unauthorized

    /// <summary>Creates a handler that returns an HTTP 403 Forbidden response.</summary>
    /// <returns>A handler that produces a 403 Forbidden response.</returns>
    let forbidden<'E> : HttpHandler<'E> = succeed Response.forbidden

    /// <summary>Creates a handler that returns a redirect response.</summary>
    /// <param name="location">The redirect location URI.</param>
    /// <param name="permanent">Whether the redirect is permanent (301) or temporary (302).</param>
    /// <returns>A handler that produces a redirect response.</returns>
    let redirect (location: string) (permanent: bool) : HttpHandler<'E> =
        if permanent then
            succeed (Response.movedPermanently location)
        else
            succeed (Response.found location)

    /// <summary>Transforms a handler by applying a function to its response.</summary>
    /// <param name="f">The function to apply to the response.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>A handler that applies the function to the original handler's response.</returns>
    let map (f: HttpResponse -> HttpResponse) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return f response
            }

    /// <summary>Transforms a handler by chaining its response through a function that produces a new handler.</summary>
    /// <param name="f">The function that takes a response and produces a new handler.</param>
    /// <param name="handler">The handler to bind.</param>
    /// <returns>A handler that chains the original handler's response through the function.</returns>
    let bind (f: HttpResponse -> HttpHandler<'E>) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request ->
            fio {
                let! response = handler request
                return! f response request
            }

    /// <summary>Combines two handlers by running both and merging their responses with a combiner function.</summary>
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

    /// <summary>Combines two handlers by trying the first and falling back to the second on failure.</summary>
    /// <param name="handler2">The fallback handler to use on failure.</param>
    /// <param name="handler1">The primary handler to try first.</param>
    /// <returns>A handler that attempts handler1 and falls back to handler2 on failure.</returns>
    let orElse (handler2: HttpHandler<'E>) (handler1: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler1 request <|> handler2 request

    /// <summary>Transforms the error type of a handler by applying a mapping function.</summary>
    /// <param name="f">The function to apply to the error.</param>
    /// <param name="handler">The handler whose error type to transform.</param>
    /// <returns>A handler with the error type mapped by the function.</returns>
    let mapError (f: 'E1 -> 'E2) (handler: HttpHandler<'E1>) : HttpHandler<'E2> =
        fun request -> (handler request).MapError f

    /// <summary>Transforms a handler by running a side effect after it without changing the response.</summary>
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

    /// <summary>Transforms a handler by running a side effect with request and response without changing the response.</summary>
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

    /// <summary>Creates a handler that returns an HTTP 200 OK response, ignoring the request.</summary>
    /// <returns>A handler that produces a 200 OK response.</returns>
    let ask<'E> : HttpHandler<'E> = fun _ -> FIO.succeed Response.ok

    /// <summary>Returns an effect that extracts a value from the request using the given function.</summary>
    /// <param name="f">The function that extracts a value from the request.</param>
    /// <returns>A function that produces an FIO effect with the extracted value.</returns>
    let asks (f: HttpRequest -> 'T) : HttpRequest -> FIO<'T, 'E> = fun request -> FIO.succeed (f request)

    /// <summary>Transforms a handler by applying a function to the request before passing it to the handler.</summary>
    /// <param name="f">The function that transforms the request before passing it to the handler.</param>
    /// <param name="handler">The handler to run with the modified request.</param>
    /// <returns>A handler that transforms the request and delegates to the original handler.</returns>
    let local (f: HttpRequest -> HttpRequest) (handler: HttpHandler<'E>) : HttpHandler<'E> =
        fun request -> handler (f request)

    /// <summary>Creates a handler that runs conditionally based on a predicate, returning a fallback response otherwise.</summary>
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

    /// <summary>Creates a handler that delegates to one of two handlers based on a predicate.</summary>
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

    /// <summary>Creates a function that parses the request body as JSON.</summary>
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

    /// <summary>Creates a handler that deserializes the JSON request body and processes it with the given function.</summary>
    /// <param name="f">The function that processes the deserialized body and produces a response effect.</param>
    /// <param name="onError">Maps parse exceptions to the error type.</param>
    /// <returns>A handler that deserializes the JSON body and processes it.</returns>
    let jsonBody<'T, 'E> (f: 'T -> FIO<HttpResponse, 'E>) (onError: exn -> 'E) : HttpHandler<'E> =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> None request).MapError onError
                return! f body
            }

    /// <summary>Creates a handler that deserializes the JSON request body with custom options and processes it with the given function.</summary>
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

/// <summary>Provides operators for HTTP handler composition.</summary>
module HttpHandlerOperators =

    /// <summary>Transforms a handler by applying a function to its response.</summary>
    /// <param name="f">The function to apply to the response.</param>
    /// <param name="handler">The handler to transform.</param>
    /// <returns>A handler with the response mapped by the function.</returns>
    let (<!>) f handler = HttpHandler.map f handler

    /// <summary>Transforms a handler by chaining its response through a function that produces a new handler.</summary>
    /// <param name="handler">The handler to bind.</param>
    /// <param name="f">The function that takes a response and produces a new handler.</param>
    /// <returns>A handler that chains the original handler's response through the function.</returns>
    let (>>=) handler f = HttpHandler.bind f handler

    /// <summary>Combines two handlers by trying the first and falling back to the second on failure.</summary>
    /// <param name="handler1">The primary handler to try first.</param>
    /// <param name="handler2">The fallback handler to use on failure.</param>
    /// <returns>A handler that attempts handler1 and falls back to handler2 on failure.</returns>
    let (<|>) handler1 handler2 = HttpHandler.orElse handler2 handler1
