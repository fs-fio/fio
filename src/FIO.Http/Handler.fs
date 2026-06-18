namespace FIO.Http

open FIO.DSL

open System.IO
open System.Text.Json

/// A function that turns an HTTP request into an effect producing a response.
type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>

[<RequireQualifiedAccess>]
module HttpHandler =

    /// Creates a handler that always returns the given response.
    let succeed (response: HttpResponse) =
        fun _ -> FIO.succeed response

    /// Creates a handler that always fails with the given error.
    let fail (error: 'E) =
        fun _ -> FIO.fail error

    /// Creates a handler that ignores the request and runs the given effect.
    let fromFIO (effect: FIO<HttpResponse, 'E>) =
        fun _ -> effect

    /// Creates a handler from a pure request-to-response function, capturing exceptions.
    let fromFunc (func: HttpRequest -> HttpResponse) =
        fun request -> FIO.attempt (fun () -> func request) id

    /// Creates a handler from a request-to-effect function.
    let fromFuncFIO (func: HttpRequest -> FIO<HttpResponse, 'E>) =
        func

    /// A handler that returns 200 OK.
    let ok<'E> : HttpHandler<'E> =
        succeed Response.ok

    /// Creates a handler that returns 200 OK with a JSON body.
    let okJson (value: 'A) : HttpHandler<'E> =
        succeed <| Response.okJson value

    /// Creates a handler that returns 200 OK with a plain-text body.
    let text (text: string) : HttpHandler<'E> =
        succeed <| Response.okText text

    /// Creates a handler that returns 200 OK with an HTML body.
    let html (html: string) : HttpHandler<'E> =
        succeed <| Response.okHtml html

    /// Creates a handler that returns 200 OK with a raw byte body.
    let bytes (bytes: byte[]) (contentType: string) : HttpHandler<'E> =
        succeed <| Response.okBytes bytes contentType

    /// Creates a handler that returns 200 OK streaming the given stream.
    let stream (stream: Stream) (length: int64 option) (contentType: string) : HttpHandler<'E> =
        succeed <| Response.okStream stream length contentType

    /// A handler that returns 204 No Content.
    let noContent<'E> : HttpHandler<'E> =
        succeed <| Response.noContent

    /// A handler that returns 404 Not Found.
    let notFound<'E> : HttpHandler<'E> =
        succeed <| Response.notFound

    /// Creates a handler that returns 404 Not Found with a plain-text body.
    let notFoundText (message: string) : HttpHandler<'E> =
        succeed <| Response.notFoundText message

    /// A handler that returns 400 Bad Request.
    let badRequest<'E> : HttpHandler<'E> =
        succeed <| Response.badRequest

    /// Creates a handler that returns 400 Bad Request with a plain-text body.
    let badRequestText (message: string) : HttpHandler<'E> =
        succeed <| Response.badRequestText message

    /// Creates a handler that returns 400 Bad Request with a JSON body.
    let badRequestJson (error: 'A) : HttpHandler<'E> =
        succeed <| Response.badRequestJson error

    /// A handler that returns 500 Internal Server Error.
    let serverError<'E> : HttpHandler<'E> =
        succeed <| Response.internalServerError

    /// Creates a handler that returns 500 Internal Server Error with a plain-text body.
    let serverErrorText (message: string) : HttpHandler<'E> =
        succeed <| Response.internalServerErrorText message

    /// A handler that returns 401 Unauthorized.
    let unauthorized<'E> : HttpHandler<'E> =
        succeed <| Response.unauthorized

    /// A handler that returns 403 Forbidden.
    let forbidden<'E> : HttpHandler<'E> =
        succeed <| Response.forbidden

    /// Creates a handler that redirects to the given location, permanently or temporarily.
    let redirect (location: string) (permanent: bool) : HttpHandler<'E> =
        if permanent then
            succeed <| Response.movedPermanently location
        else
            succeed <| Response.found location

    /// Transforms a handler's response with the given function.
    let map (mapper: HttpResponse -> HttpResponse) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                return mapper response
            }

    /// Chains a handler into another handler that depends on its response.
    let bind (cont: HttpResponse -> HttpHandler<'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                return! cont response request
            }

    /// Combines two handlers' responses for the same request with the given function.
    let zipWith
        (combiner: HttpResponse -> HttpResponse -> HttpResponse)
        (handler: HttpHandler<'E>)
        (handler': HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                let! response' = handler' request
                return combiner response response'
            }

    /// Returns a handler that falls back to another if the first fails.
    let orElse (handler': HttpHandler<'E>) (handler: HttpHandler<'E>) =
        fun request -> handler request <|> handler' request

    /// Transforms a handler's error with the given function.
    let mapError (mapper: 'E -> 'E1) (handler: HttpHandler<'E>) =
        fun request -> (handler request).MapError mapper

    /// Runs a side effect on a handler's response, keeping the response.
    let tap (func: HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                do! func response
                return response
            }

    /// Runs a side effect on a handler's request and response, keeping the response.
    let tapWithRequest (func: HttpRequest -> HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                do! func request response
                return response
            }

    /// Creates a handler that derives a value from the request.
    let asks (func: HttpRequest -> 'A) =
        fun request -> FIO.succeed <| func request

    /// Returns a handler that transforms the request before passing it on.
    let local (func: HttpRequest -> HttpRequest) (handler: HttpHandler<'E>) =
        fun request -> handler <| func request

    /// Runs the handler when the predicate holds, otherwise returns the fallback response.
    let when' (predicate: HttpRequest -> bool) (handler: HttpHandler<'E>) (fallback: HttpResponse) =
        fun request ->
            if predicate request then
                handler request
            else
                FIO.succeed fallback

    /// Runs one of two handlers depending on the predicate.
    let ifElse
        (predicate: HttpRequest -> bool)
        (trueHandler: HttpHandler<'E>)
        (falseHandler: HttpHandler<'E>) =
        fun request ->
            if predicate request then
                trueHandler request
            else
                falseHandler request

    /// Parses the request body as JSON into the given type.
    let parseJsonBody<'A> (options: JsonSerializerOptions option) : HttpRequest -> FIO<'A, exn> =
        fun request ->
            FIO.attempt (fun () ->
                let bodyStr = request.Body.AsString()
                if System.String.IsNullOrWhiteSpace bodyStr then
                    raise (JsonException "Request body is empty")
                let result =
                    match options with
                    | Some opts -> JsonSerializer.Deserialize<'A>(bodyStr, opts)
                    | None -> JsonSerializer.Deserialize<'A> bodyStr
                if obj.ReferenceEquals(box result, null) then
                    raise (JsonException "Request body deserialized to null")
                result)
                id

    /// Creates a handler that parses the JSON request body and passes it to the given function.
    let jsonBody<'A, 'E> (func: 'A -> FIO<HttpResponse, 'E>) (onError: exn -> 'E) =
        fun request ->
            fio {
                let! body = (parseJsonBody<'A> None request).MapError onError
                return! func body
            }

    /// Creates a handler that parses the JSON request body with the given options and passes it to the given function.
    let jsonBodyWith<'A, 'E>
        (options: JsonSerializerOptions)
        (func: 'A -> FIO<HttpResponse, 'E>)
        (onError: exn -> 'E) =
        fun request ->
            fio {
                let! body = (parseJsonBody<'A> (Some options) request).MapError onError
                return! func body
            }

module HttpHandlerOperators =

    /// Maps a handler's response. Operator form of <c>HttpHandler.map</c>.
    let (<!>) func handler =
        HttpHandler.map func handler

    /// Chains a handler into a response-dependent handler. Operator form of <c>HttpHandler.bind</c>.
    let (>>=) handler func =
        HttpHandler.bind func handler

    /// Falls back to another handler on failure. Operator form of <c>HttpHandler.orElse</c>.
    let (<|>) handler handler' =
        HttpHandler.orElse handler' handler
