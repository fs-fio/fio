namespace FIO.Http

[<RequireQualifiedAccess>]
module Response =

    /// A 200 OK response.
    let ok = HttpResponse.create HttpStatusCode.OK

    /// Creates a 200 OK response with a JSON body.
    let okJson (value: 'A) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)

    /// Creates a 200 OK response with a plain-text body.
    let okText (text: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text text)

    /// Creates a 200 OK response with an HTML body.
    let okHtml (html: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/html; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text html)

    /// Creates a 200 OK response with a raw byte body and the given content type.
    let okBytes (bytes: byte[]) contentType =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (ResponseBody.Bytes bytes)

    /// Creates a 200 OK response that streams the given stream.
    let okStream stream length contentType =
        if isNull stream then
            invalidArg "stream" "Stream cannot be null"
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (ResponseBody.Stream(stream, length))

    /// A 201 Created response.
    let created = HttpResponse.create HttpStatusCode.Created

    /// Creates a 201 Created response with a Location header.
    let createdAt location =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Location" location

    /// Creates a 201 Created response with a JSON body.
    let createdJson (value: 'A) =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)

    /// A 202 Accepted response.
    let accepted = HttpResponse.create HttpStatusCode.Accepted

    /// A 204 No Content response.
    let noContent = HttpResponse.create HttpStatusCode.NoContent

    /// Creates a 301 Moved Permanently response to the given location.
    let movedPermanently location =
        HttpResponse.create HttpStatusCode.MovedPermanently
        |> HttpResponse.withHeader "Location" location

    /// Creates a 302 Found response to the given location.
    let found location =
        HttpResponse.create HttpStatusCode.Found
        |> HttpResponse.withHeader "Location" location

    /// Creates a 303 See Other response to the given location.
    let seeOther location =
        HttpResponse.create HttpStatusCode.SeeOther
        |> HttpResponse.withHeader "Location" location

    /// A 304 Not Modified response.
    let notModified = HttpResponse.create HttpStatusCode.NotModified

    /// Creates a 307 Temporary Redirect response to the given location.
    let temporaryRedirect location =
        HttpResponse.create HttpStatusCode.TemporaryRedirect
        |> HttpResponse.withHeader "Location" location

    /// Creates a 308 Permanent Redirect response to the given location.
    let permanentRedirect location =
        HttpResponse.create HttpStatusCode.PermanentRedirect
        |> HttpResponse.withHeader "Location" location

    /// A 400 Bad Request response.
    let badRequest = HttpResponse.create HttpStatusCode.BadRequest

    /// Creates a 400 Bad Request response with a plain-text body.
    let badRequestText (message: string) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a 400 Bad Request response with a JSON body.
    let badRequestJson (error: 'A) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json error)

    /// A 401 Unauthorized response.
    let unauthorized = HttpResponse.create HttpStatusCode.Unauthorized

    /// Creates a 401 Unauthorized response with a WWW-Authenticate header.
    let unauthorizedWith scheme =
        HttpResponse.create HttpStatusCode.Unauthorized
        |> HttpResponse.withHeader "WWW-Authenticate" scheme

    /// A 403 Forbidden response.
    let forbidden = HttpResponse.create HttpStatusCode.Forbidden

    /// Creates a 403 Forbidden response with a plain-text body.
    let forbiddenText (message: string) =
        HttpResponse.create HttpStatusCode.Forbidden
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 404 Not Found response.
    let notFound = HttpResponse.create HttpStatusCode.NotFound

    /// Creates a 404 Not Found response with a plain-text body.
    let notFoundText (message: string) =
        HttpResponse.create HttpStatusCode.NotFound
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a 405 Method Not Allowed response with an Allow header.
    let methodNotAllowed allowedMethods =
        HttpResponse.create HttpStatusCode.MethodNotAllowed
        |> HttpResponse.withHeader "Allow" (String.concat ", " allowedMethods)

    /// A 408 Request Timeout response.
    let requestTimeout = HttpResponse.create HttpStatusCode.RequestTimeout

    /// A 409 Conflict response.
    let conflict = HttpResponse.create HttpStatusCode.Conflict

    /// Creates a 409 Conflict response with a plain-text body.
    let conflictText (message: string) =
        HttpResponse.create HttpStatusCode.Conflict
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 415 Unsupported Media Type response.
    let unsupportedMediaType = HttpResponse.create HttpStatusCode.UnsupportedMediaType

    /// A 422 Unprocessable Entity response.
    let unprocessableEntity = HttpResponse.create HttpStatusCode.UnprocessableEntity

    /// Creates a 422 Unprocessable Entity response with a JSON body.
    let unprocessableEntityJson (errors: 'A) =
        HttpResponse.create HttpStatusCode.UnprocessableEntity
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json errors)

    /// A 429 Too Many Requests response.
    let tooManyRequests = HttpResponse.create HttpStatusCode.TooManyRequests

    /// Creates a 429 Too Many Requests response with a Retry-After header.
    let tooManyRequestsAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.TooManyRequests
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// A 500 Internal Server Error response.
    let internalServerError = HttpResponse.create HttpStatusCode.InternalServerError

    /// Creates a 500 Internal Server Error response with a plain-text body.
    let internalServerErrorText (message: string) =
        HttpResponse.create HttpStatusCode.InternalServerError
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// A 501 Not Implemented response.
    let notImplemented = HttpResponse.create HttpStatusCode.NotImplemented

    /// A 502 Bad Gateway response.
    let badGateway = HttpResponse.create HttpStatusCode.BadGateway

    /// A 503 Service Unavailable response.
    let serviceUnavailable = HttpResponse.create HttpStatusCode.ServiceUnavailable

    /// Creates a 503 Service Unavailable response with a Retry-After header.
    let serviceUnavailableAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.ServiceUnavailable
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// A 504 Gateway Timeout response.
    let gatewayTimeout = HttpResponse.create HttpStatusCode.GatewayTimeout

    /// Creates a response with the given status code.
    let status code = HttpResponse.create code

    /// Creates a response with the given status code and a plain-text body.
    let statusText code message =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Text message)

    /// Creates a response with the given status code and a JSON body.
    let statusJson code value =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (ResponseBody.Json value)
