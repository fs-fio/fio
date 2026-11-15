namespace FIO.Http

[<RequireQualifiedAccess>]
module Response =

    let private withContent status contentType body =
        HttpResponse.create status
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody body

    let private text status (message: string) =
        withContent status "text/plain; charset=utf-8" (ResponseBody.Text message)

    let private json status (value: 'A) =
        withContent status "application/json; charset=utf-8" (ResponseBody.Json value)

    let private withHeader status name value =
        HttpResponse.create status |> HttpResponse.withHeader name value

    /// A 200 OK response.
    let ok: HttpResponse = HttpResponse.create HttpStatusCode.OK

    /// Creates a 200 OK response with a JSON body.
    let okJson (value: 'A) : HttpResponse = json HttpStatusCode.OK value

    /// Creates a 200 OK response with a plain-text body.
    let okText (text': string) : HttpResponse = text HttpStatusCode.OK text'

    /// Creates a 200 OK response with an HTML body.
    let okHtml (html: string) : HttpResponse =
        withContent HttpStatusCode.OK "text/html; charset=utf-8" (ResponseBody.Text html)

    /// Creates a 200 OK response with a raw byte body and the given content type.
    let okBytes (bytes: byte[]) contentType : HttpResponse =
        withContent HttpStatusCode.OK contentType (ResponseBody.Bytes bytes)

    /// Creates a 200 OK response that streams the given stream.
    let okStream stream length contentType : HttpResponse =
        if isNull stream then
            invalidArg "stream" "Stream cannot be null"
        withContent HttpStatusCode.OK contentType (ResponseBody.Stream(stream, length))

    /// A 201 Created response.
    let created: HttpResponse = HttpResponse.create HttpStatusCode.Created

    /// Creates a 201 Created response with a Location header.
    let createdAt location : HttpResponse =
        withHeader HttpStatusCode.Created "Location" location

    /// Creates a 201 Created response with a JSON body.
    let createdJson (value: 'A) : HttpResponse = json HttpStatusCode.Created value

    /// A 202 Accepted response.
    let accepted: HttpResponse = HttpResponse.create HttpStatusCode.Accepted

    /// A 204 No Content response.
    let noContent: HttpResponse = HttpResponse.create HttpStatusCode.NoContent

    /// Creates a 301 Moved Permanently response to the given location.
    let movedPermanently location : HttpResponse =
        withHeader HttpStatusCode.MovedPermanently "Location" location

    /// Creates a 302 Found response to the given location.
    let found location : HttpResponse =
        withHeader HttpStatusCode.Found "Location" location

    /// Creates a 303 See Other response to the given location.
    let seeOther location : HttpResponse =
        withHeader HttpStatusCode.SeeOther "Location" location

    /// A 304 Not Modified response.
    let notModified: HttpResponse = HttpResponse.create HttpStatusCode.NotModified

    /// Creates a 307 Temporary Redirect response to the given location.
    let temporaryRedirect location : HttpResponse =
        withHeader HttpStatusCode.TemporaryRedirect "Location" location

    /// Creates a 308 Permanent Redirect response to the given location.
    let permanentRedirect location : HttpResponse =
        withHeader HttpStatusCode.PermanentRedirect "Location" location

    /// A 400 Bad Request response.
    let badRequest: HttpResponse = HttpResponse.create HttpStatusCode.BadRequest

    /// Creates a 400 Bad Request response with a plain-text body.
    let badRequestText (message: string) : HttpResponse = text HttpStatusCode.BadRequest message

    /// Creates a 400 Bad Request response with a JSON body.
    let badRequestJson (error: 'A) : HttpResponse = json HttpStatusCode.BadRequest error

    /// A 401 Unauthorized response.
    let unauthorized: HttpResponse = HttpResponse.create HttpStatusCode.Unauthorized

    /// Creates a 401 Unauthorized response with a WWW-Authenticate header.
    let unauthorizedWith scheme : HttpResponse =
        withHeader HttpStatusCode.Unauthorized "WWW-Authenticate" scheme

    /// A 403 Forbidden response.
    let forbidden: HttpResponse = HttpResponse.create HttpStatusCode.Forbidden

    /// Creates a 403 Forbidden response with a plain-text body.
    let forbiddenText (message: string) : HttpResponse = text HttpStatusCode.Forbidden message

    /// A 404 Not Found response.
    let notFound: HttpResponse = HttpResponse.create HttpStatusCode.NotFound

    /// Creates a 404 Not Found response with a plain-text body.
    let notFoundText (message: string) : HttpResponse = text HttpStatusCode.NotFound message

    /// Creates a 405 Method Not Allowed response with an Allow header.
    let methodNotAllowed allowedMethods : HttpResponse =
        withHeader HttpStatusCode.MethodNotAllowed "Allow" (String.concat ", " allowedMethods)

    /// A 408 Request Timeout response.
    let requestTimeout: HttpResponse = HttpResponse.create HttpStatusCode.RequestTimeout

    /// A 409 Conflict response.
    let conflict: HttpResponse = HttpResponse.create HttpStatusCode.Conflict

    /// Creates a 409 Conflict response with a plain-text body.
    let conflictText (message: string) : HttpResponse = text HttpStatusCode.Conflict message

    /// A 415 Unsupported Media Type response.
    let unsupportedMediaType: HttpResponse =
        HttpResponse.create HttpStatusCode.UnsupportedMediaType

    /// A 422 Unprocessable Entity response.
    let unprocessableEntity: HttpResponse =
        HttpResponse.create HttpStatusCode.UnprocessableEntity

    /// Creates a 422 Unprocessable Entity response with a JSON body.
    let unprocessableEntityJson (errors: 'A) : HttpResponse =
        json HttpStatusCode.UnprocessableEntity errors

    /// A 429 Too Many Requests response.
    let tooManyRequests: HttpResponse =
        HttpResponse.create HttpStatusCode.TooManyRequests

    /// Creates a 429 Too Many Requests response with a Retry-After header.
    let tooManyRequestsAfter retryAfterSeconds : HttpResponse =
        withHeader HttpStatusCode.TooManyRequests "Retry-After" (string retryAfterSeconds)

    /// A 500 Internal Server Error response.
    let internalServerError: HttpResponse =
        HttpResponse.create HttpStatusCode.InternalServerError

    /// Creates a 500 Internal Server Error response with a plain-text body.
    let internalServerErrorText (message: string) : HttpResponse =
        text HttpStatusCode.InternalServerError message

    /// A 501 Not Implemented response.
    let notImplemented: HttpResponse = HttpResponse.create HttpStatusCode.NotImplemented

    /// A 502 Bad Gateway response.
    let badGateway: HttpResponse = HttpResponse.create HttpStatusCode.BadGateway

    /// A 503 Service Unavailable response.
    let serviceUnavailable: HttpResponse =
        HttpResponse.create HttpStatusCode.ServiceUnavailable

    /// Creates a 503 Service Unavailable response with a Retry-After header.
    let serviceUnavailableAfter retryAfterSeconds : HttpResponse =
        withHeader HttpStatusCode.ServiceUnavailable "Retry-After" (string retryAfterSeconds)

    /// A 504 Gateway Timeout response.
    let gatewayTimeout: HttpResponse = HttpResponse.create HttpStatusCode.GatewayTimeout

    /// Creates a response with the given status code.
    let status code : HttpResponse = HttpResponse.create code

    /// Creates a response with the given status code and a plain-text body.
    let statusText code message : HttpResponse = text code message

    /// Creates a response with the given status code and a JSON body.
    let statusJson code value : HttpResponse = json code value
