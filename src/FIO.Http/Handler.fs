namespace FIO.Http

open FIO.DSL

open System.IO
open System.Text.Json

type HttpHandler<'E> = HttpRequest -> FIO<HttpResponse, 'E>

[<RequireQualifiedAccess>]
module HttpHandler =

    let succeed (response: HttpResponse) =
        fun _ -> FIO.succeed response

    let fail (error: 'E) =
        fun _ -> FIO.fail error

    let fromFIO (effect: FIO<HttpResponse, 'E>) =
        fun _ -> effect

    let fromFunc (func: HttpRequest -> HttpResponse) =
        fun request -> FIO.attempt (fun () -> func request) id

    let fromFuncFIO (func: HttpRequest -> FIO<HttpResponse, 'E>) =
        func

    let ok<'E> : HttpHandler<'E> =
        succeed Response.ok

    let okJson (value: 'T) : HttpHandler<'E> =
        succeed <| Response.okJson value

    let text (text: string) : HttpHandler<'E> =
        succeed <| Response.okText text

    let html (html: string) : HttpHandler<'E> =
        succeed <| Response.okHtml html

    let bytes (bytes: byte[]) (contentType: string) : HttpHandler<'E> =
        succeed <| Response.okBytes bytes contentType

    let stream (stream: Stream) (length: int64 option) (contentType: string) : HttpHandler<'E> =
        succeed <| Response.okStream stream length contentType

    let noContent<'E> : HttpHandler<'E> =
        succeed <| Response.noContent

    let notFound<'E> : HttpHandler<'E> =
        succeed <| Response.notFound

    let notFoundText (message: string) : HttpHandler<'E> =
        succeed <| Response.notFoundText message

    let badRequest<'E> : HttpHandler<'E> =
        succeed <| Response.badRequest

    let badRequestText (message: string) : HttpHandler<'E> =
        succeed <| Response.badRequestText message

    let badRequestJson (error: 'T) : HttpHandler<'E> =
        succeed <| Response.badRequestJson error

    let serverError<'E> : HttpHandler<'E> =
        succeed <| Response.internalServerError

    let serverErrorText (message: string) : HttpHandler<'E> =
        succeed <| Response.internalServerErrorText message

    let unauthorized<'E> : HttpHandler<'E> =
        succeed <| Response.unauthorized

    let forbidden<'E> : HttpHandler<'E> =
        succeed <| Response.forbidden

    let redirect (location: string) (permanent: bool) : HttpHandler<'E> =
        if permanent then
            succeed <| Response.movedPermanently location
        else
            succeed <| Response.found location

    let map (func: HttpResponse -> HttpResponse) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                return func response
            }

    let bind (func: HttpResponse -> HttpHandler<'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                return! func response request
            }

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

    let orElse (handler': HttpHandler<'E>) (handler: HttpHandler<'E>) =
        fun request -> handler request <|> handler' request

    let mapError (func: 'E1 -> 'E2) (handler: HttpHandler<'E1>) =
        fun request -> (handler request).MapError func

    let tap (func: HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                do! func response
                return response
            }

    let tapWithRequest (func: HttpRequest -> HttpResponse -> FIO<unit, 'E>) (handler: HttpHandler<'E>) =
        fun request ->
            fio {
                let! response = handler request
                do! func request response
                return response
            }

    let asks (func: HttpRequest -> 'T) =
        fun request -> FIO.succeed <| func request

    let local (func: HttpRequest -> HttpRequest) (handler: HttpHandler<'E>) =
        fun request -> handler <| func request

    let when' (predicate: HttpRequest -> bool) (handler: HttpHandler<'E>) (fallback: HttpResponse) =
        fun request ->
            if predicate request then
                handler request
            else
                FIO.succeed fallback

    let ifElse
        (predicate: HttpRequest -> bool)
        (trueHandler: HttpHandler<'E>)
        (falseHandler: HttpHandler<'E>) =
        fun request ->
            if predicate request then
                trueHandler request
            else
                falseHandler request

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

    let jsonBody<'T, 'E> (func: 'T -> FIO<HttpResponse, 'E>) (onError: exn -> 'E) =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> None request).MapError onError
                return! func body
            }

    let jsonBodyWith<'T, 'E>
        (options: JsonSerializerOptions)
        (func: 'T -> FIO<HttpResponse, 'E>)
        (onError: exn -> 'E) =
        fun request ->
            fio {
                let! body = (parseJsonBody<'T> (Some options) request).MapError onError
                return! func body
            }

module HttpHandlerOperators =

    let (<!>) func handler =
        HttpHandler.map func handler

    let (>>=) handler func =
        HttpHandler.bind func handler

    let (<|>) handler handler' =
        HttpHandler.orElse handler' handler
