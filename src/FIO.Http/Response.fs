namespace FIO.Http

[<RequireQualifiedAccess>]
module Response =

    /// A 200 OK response.
    let ok : HttpResponse = HttpResponse.create HttpStatusCode.OK

    /// Creates a 200 OK response with a JSON body.
    let okJson (value: 'A) : HttpResponse =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)

    /// Creates a 200 OK response with a plain-text body.
    let okText (text: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text text)

    /// Creates a 200 OK response with an HTML body.
    let okHtml (html: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/html; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text html)

    /// Creates a 200 OK response with a raw byte body and the given content type.
    let okBytes (bytes: byte[]) contentType : HttpResponse =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (ResponseBody.Bytes bytes)

    /// Creates a 200 OK response that streams the given stream.
    let okStream stream length contentType : HttpResponse =
        if isNull stream then
            invalidArg "stream" "Stream cannot be null"
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (ResponseBody.Stream(stream, length))

    /// A 201 Created response.
    let created : HttpResponse = HttpResponse.create HttpStatusCode.Created

    /// Creates a 201 Created response with a Location header.
    let createdAt location : HttpResponse =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Location" location

    /// Creates a 201 Created response with a JSON body.
    let createdJson (value: 'A) : HttpResponse =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)

    /// A 202 Accepted response.
    let accepted : HttpResponse = HttpResponse.create HttpStatusCode.Accepted

    /// A 204 No Content response.
    let noContent : HttpResponse = HttpResponse.create HttpStatusCode.NoContent

    /// Creates a 301 Moved Permanently response to the given location.
    let movedPermanently location : HttpResponse =
        HttpResponse.create HttpStatusCode.MovedPermanently
        |> HttpResponse.withHeader "Location" location

    /// Creates a 302 Found response to the given location.
    let found location : HttpResponse =
        HttpResponse.create HttpStatusCode.Found
        |> HttpResponse.withHeader "Location" location

    /// Creates a 303 See Other response to the given location.
    let seeOther location : HttpResponse =
        HttpResponse.create HttpStatusCode.SeeOther
        |> HttpResponse.withHeader "Location" location

    /// A 304 Not Modified response.
    let notModified : HttpResponse = HttpResponse.create HttpStatusCode.NotModified

    /// Creates a 307 Temporary Redirect response to the given location.
    let temporaryRedirect location : HttpResponse =
        HttpResponse.create HttpStatusCode.TemporaryRedirect
        |> HttpResponse.withHeader "Location" location

    /// Creates a 308 Permanent Redirect response to the given location.
    let permanentRedirect location : HttpResponse =
        HttpResponse.create HttpStatusCode.PermanentRedirect
        |> HttpResponse.withHeader "Location" location

    /// A 400 Bad Request response.
    let badRequest : HttpResponse = HttpResponse.create HttpStatusCode.BadRequest

    /// Creates a 400 Bad Request response with a plain-text body.
    let badRequestText (message: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a 400 Bad Request response with a JSON body.
    let badRequestJson (error: 'A) : HttpResponse =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json error)

    /// A 401 Unauthorized response.
    let unauthorized : HttpResponse = HttpResponse.create HttpStatusCode.Unauthorized

    /// Creates a 401 Unauthorized response with a WWW-Authenticate header.
    let unauthorizedWith scheme : HttpResponse =
        HttpResponse.create HttpStatusCode.Unauthorized
        |> HttpResponse.withHeader "WWW-Authenticate" scheme

    /// A 403 Forbidden response.
    let forbidden : HttpResponse = HttpResponse.create HttpStatusCode.Forbidden

    /// Creates a 403 Forbidden response with a plain-text body.
    let forbiddenText (message: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.Forbidden
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 404 Not Found response.
    let notFound : HttpResponse = HttpResponse.create HttpStatusCode.NotFound

    /// Creates a 404 Not Found response with a plain-text body.
    let notFoundText (message: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.NotFound
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a 405 Method Not Allowed response with an Allow header.
    let methodNotAllowed allowedMethods : HttpResponse =
        HttpResponse.create HttpStatusCode.MethodNotAllowed
        |> HttpResponse.withHeader "Allow" (String.concat ", " allowedMethods)

    /// A 408 Request Timeout response.
    let requestTimeout : HttpResponse = HttpResponse.create HttpStatusCode.RequestTimeout

    /// A 409 Conflict response.
    let conflict : HttpResponse = HttpResponse.create HttpStatusCode.Conflict

    /// Creates a 409 Conflict response with a plain-text body.
    let conflictText (message: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.Conflict
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 415 Unsupported Media Type response.
    let unsupportedMediaType : HttpResponse = HttpResponse.create HttpStatusCode.UnsupportedMediaType

    /// A 422 Unprocessable Entity response.
    let unprocessableEntity : HttpResponse = HttpResponse.create HttpStatusCode.UnprocessableEntity

    /// Creates a 422 Unprocessable Entity response with a JSON body.
    let unprocessableEntityJson (errors: 'A) : HttpResponse =
        HttpResponse.create HttpStatusCode.UnprocessableEntity
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json errors)

    /// A 429 Too Many Requests response.
    let tooManyRequests : HttpResponse = HttpResponse.create HttpStatusCode.TooManyRequests

    /// Creates a 429 Too Many Requests response with a Retry-After header.
    let tooManyRequestsAfter retryAfterSeconds : HttpResponse =
        HttpResponse.create HttpStatusCode.TooManyRequests
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// A 500 Internal Server Error response.
    let internalServerError : HttpResponse = HttpResponse.create HttpStatusCode.InternalServerError

    /// Creates a 500 Internal Server Error response with a plain-text body.
    let internalServerErrorText (message: string) : HttpResponse =
        HttpResponse.create HttpStatusCode.InternalServerError
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 501 Not Implemented response.
    let notImplemented : HttpResponse = HttpResponse.create HttpStatusCode.NotImplemented

    /// A 502 Bad Gateway response.
    let badGateway : HttpResponse = HttpResponse.create HttpStatusCode.BadGateway

    /// A 503 Service Unavailable response.
    let serviceUnavailable : HttpResponse = HttpResponse.create HttpStatusCode.ServiceUnavailable

    /// Creates a 503 Service Unavailable response with a Retry-After header.
    let serviceUnavailableAfter retryAfterSeconds : HttpResponse =
        HttpResponse.create HttpStatusCode.ServiceUnavailable
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// A 504 Gateway Timeout response.
    let gatewayTimeout : HttpResponse = HttpResponse.create HttpStatusCode.GatewayTimeout

    /// Creates a response with the given status code.
    let status code : HttpResponse = HttpResponse.create code

    /// Creates a response with the given status code and a plain-text body.
    let statusText code message : HttpResponse =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a response with the given status code and a JSON body.
    let statusJson code value : HttpResponse =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)
