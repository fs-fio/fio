namespace FIO.Http

/// Convenience functions for creating HTTP responses.
[<RequireQualifiedAccess>]
module Response =

    /// Creates an HTTP 200 OK response.
    /// <returns>A 200 OK response.</returns>
    let ok = HttpResponse.create HttpStatusCode.OK

    /// Creates an HTTP 200 OK response with JSON body.
    /// Note: JSON serialization errors will occur during response writing (in KestrelBridge),
    /// not at response creation time. Ensure the value is serializable.
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>A 200 OK response with JSON body.</returns>
    let okJson (value: 'T) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    /// Creates an HTTP 200 OK response with plain text body.
    /// <param name="text">The response body text.</param>
    /// <returns>A 200 OK response with plain text body.</returns>
    let okText (text: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text text)

    /// Creates an HTTP 200 OK response with HTML body.
    /// <param name="html">The HTML content.</param>
    /// <returns>A 200 OK response with HTML body.</returns>
    let okHtml (html: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/html; charset=utf-8"
        |> HttpResponse.withBody (Text html)

    /// Creates an HTTP 200 OK response with binary body.
    /// <param name="bytes">The binary content.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A 200 OK response with binary body.</returns>
    let okBytes (bytes: byte[]) contentType =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Bytes bytes)

    /// Creates an HTTP 200 OK response with stream body.
    /// IMPORTANT: The caller is responsible for disposing the stream after the response is sent.
    /// The stream will not be automatically disposed by the framework.
    /// <param name="stream">The stream to send. Must not be null.</param>
    /// <param name="length">The optional content length.</param>
    /// <param name="contentType">The content type header value.</param>
    /// <returns>A 200 OK response with stream body.</returns>
    let okStream stream length contentType =
        if isNull stream then
            invalidArg "stream" "Stream cannot be null"

        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Stream(stream, length))

    /// Creates an HTTP 201 Created response.
    /// <returns>A 201 Created response.</returns>
    let created = HttpResponse.create HttpStatusCode.Created

    /// Creates an HTTP 201 Created response with Location header.
    /// <param name="location">The URI of the created resource.</param>
    /// <returns>A 201 Created response with Location header.</returns>
    let createdAt location =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 201 Created response with JSON body.
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>A 201 Created response with JSON body.</returns>
    let createdJson (value: 'T) =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    /// Creates an HTTP 202 Accepted response.
    /// <returns>A 202 Accepted response.</returns>
    let accepted = HttpResponse.create HttpStatusCode.Accepted

    /// Creates an HTTP 204 No Content response.
    /// <returns>A 204 No Content response.</returns>
    let noContent = HttpResponse.create HttpStatusCode.NoContent

    /// Creates an HTTP 301 Moved Permanently redirect response.
    /// <param name="location">The new location URI.</param>
    /// <returns>A 301 Moved Permanently redirect response.</returns>
    let movedPermanently location =
        HttpResponse.create HttpStatusCode.MovedPermanently
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 302 Found redirect response.
    /// <param name="location">The redirect location URI.</param>
    /// <returns>A 302 Found redirect response.</returns>
    let found location =
        HttpResponse.create HttpStatusCode.Found
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 303 See Other redirect response.
    /// <param name="location">The redirect location URI.</param>
    /// <returns>A 303 See Other redirect response.</returns>
    let seeOther location =
        HttpResponse.create HttpStatusCode.SeeOther
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 304 Not Modified response.
    /// <returns>A 304 Not Modified response.</returns>
    let notModified = HttpResponse.create HttpStatusCode.NotModified

    /// Creates an HTTP 307 Temporary Redirect response.
    /// <param name="location">The redirect location URI.</param>
    /// <returns>A 307 Temporary Redirect response.</returns>
    let temporaryRedirect location =
        HttpResponse.create HttpStatusCode.TemporaryRedirect
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 308 Permanent Redirect response.
    /// <param name="location">The redirect location URI.</param>
    /// <returns>A 308 Permanent Redirect response.</returns>
    let permanentRedirect location =
        HttpResponse.create HttpStatusCode.PermanentRedirect
        |> HttpResponse.withHeader "Location" location

    /// Creates an HTTP 400 Bad Request response.
    /// <returns>A 400 Bad Request response.</returns>
    let badRequest = HttpResponse.create HttpStatusCode.BadRequest

    /// Creates an HTTP 400 Bad Request response with text message.
    /// <param name="message">The error message text.</param>
    /// <returns>A 400 Bad Request response with plain text body.</returns>
    let badRequestText (message: string) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP 400 Bad Request response with JSON error.
    /// <param name="error">The error object to serialize as JSON.</param>
    /// <returns>A 400 Bad Request response with JSON body.</returns>
    let badRequestJson (error: 'T) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json error)

    /// Creates an HTTP 401 Unauthorized response.
    /// <returns>A 401 Unauthorized response.</returns>
    let unauthorized = HttpResponse.create HttpStatusCode.Unauthorized

    /// Creates an HTTP 401 Unauthorized response with WWW-Authenticate header.
    /// <param name="scheme">The authentication scheme.</param>
    /// <returns>A 401 Unauthorized response with WWW-Authenticate header.</returns>
    let unauthorizedWith scheme =
        HttpResponse.create HttpStatusCode.Unauthorized
        |> HttpResponse.withHeader "WWW-Authenticate" scheme

    /// Creates an HTTP 403 Forbidden response.
    /// <returns>A 403 Forbidden response.</returns>
    let forbidden = HttpResponse.create HttpStatusCode.Forbidden

    /// Creates an HTTP 403 Forbidden response with text message.
    /// <param name="message">The error message text.</param>
    /// <returns>A 403 Forbidden response with plain text body.</returns>
    let forbiddenText (message: string) =
        HttpResponse.create HttpStatusCode.Forbidden
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP 404 Not Found response.
    /// <returns>A 404 Not Found response.</returns>
    let notFound = HttpResponse.create HttpStatusCode.NotFound

    /// Creates an HTTP 404 Not Found response with text message.
    /// <param name="message">The error message text.</param>
    /// <returns>A 404 Not Found response with plain text body.</returns>
    let notFoundText (message: string) =
        HttpResponse.create HttpStatusCode.NotFound
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP 405 Method Not Allowed response.
    /// <param name="allowedMethods">The list of allowed HTTP methods.</param>
    /// <returns>A 405 Method Not Allowed response with Allow header.</returns>
    let methodNotAllowed allowedMethods =
        HttpResponse.create HttpStatusCode.MethodNotAllowed
        |> HttpResponse.withHeader "Allow" (String.concat ", " allowedMethods)

    /// Creates an HTTP 408 Request Timeout response.
    /// <returns>A 408 Request Timeout response.</returns>
    let requestTimeout = HttpResponse.create HttpStatusCode.RequestTimeout

    /// Creates an HTTP 409 Conflict response.
    /// <returns>A 409 Conflict response.</returns>
    let conflict = HttpResponse.create HttpStatusCode.Conflict

    /// Creates an HTTP 409 Conflict response with text message.
    /// <param name="message">The error message text.</param>
    /// <returns>A 409 Conflict response with plain text body.</returns>
    let conflictText (message: string) =
        HttpResponse.create HttpStatusCode.Conflict
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP 415 Unsupported Media Type response.
    /// <returns>A 415 Unsupported Media Type response.</returns>
    let unsupportedMediaType = HttpResponse.create HttpStatusCode.UnsupportedMediaType

    /// Creates an HTTP 422 Unprocessable Entity response.
    /// <returns>A 422 Unprocessable Entity response.</returns>
    let unprocessableEntity = HttpResponse.create HttpStatusCode.UnprocessableEntity

    /// Creates an HTTP 422 Unprocessable Entity response with JSON errors.
    /// <param name="errors">The validation errors to serialize as JSON.</param>
    /// <returns>A 422 Unprocessable Entity response with JSON body.</returns>
    let unprocessableEntityJson (errors: 'T) =
        HttpResponse.create HttpStatusCode.UnprocessableEntity
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json errors)

    /// Creates an HTTP 429 Too Many Requests response.
    /// <returns>A 429 Too Many Requests response.</returns>
    let tooManyRequests = HttpResponse.create HttpStatusCode.TooManyRequests

    /// Creates an HTTP 429 Too Many Requests response with Retry-After header.
    /// <param name="retryAfterSeconds">The number of seconds to wait before retrying.</param>
    /// <returns>A 429 Too Many Requests response with Retry-After header.</returns>
    let tooManyRequestsAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.TooManyRequests
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// Creates an HTTP 500 Internal Server Error response.
    /// <returns>A 500 Internal Server Error response.</returns>
    let internalServerError = HttpResponse.create HttpStatusCode.InternalServerError

    /// Creates an HTTP 500 Internal Server Error response with text message.
    /// <param name="message">The error message text.</param>
    /// <returns>A 500 Internal Server Error response with plain text body.</returns>
    let internalServerErrorText (message: string) =
        HttpResponse.create HttpStatusCode.InternalServerError
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP 501 Not Implemented response.
    /// <returns>A 501 Not Implemented response.</returns>
    let notImplemented = HttpResponse.create HttpStatusCode.NotImplemented

    /// Creates an HTTP 502 Bad Gateway response.
    /// <returns>A 502 Bad Gateway response.</returns>
    let badGateway = HttpResponse.create HttpStatusCode.BadGateway

    /// Creates an HTTP 503 Service Unavailable response.
    /// <returns>A 503 Service Unavailable response.</returns>
    let serviceUnavailable = HttpResponse.create HttpStatusCode.ServiceUnavailable

    /// Creates an HTTP 503 Service Unavailable response with Retry-After header.
    /// <param name="retryAfterSeconds">The number of seconds to wait before retrying.</param>
    /// <returns>A 503 Service Unavailable response with Retry-After header.</returns>
    let serviceUnavailableAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.ServiceUnavailable
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// Creates an HTTP 504 Gateway Timeout response.
    /// <returns>A 504 Gateway Timeout response.</returns>
    let gatewayTimeout = HttpResponse.create HttpStatusCode.GatewayTimeout

    /// Creates an HTTP response with the specified status code.
    /// <param name="code">The HTTP status code.</param>
    /// <returns>An HTTP response with the specified status code.</returns>
    let status code = HttpResponse.create code

    /// Creates an HTTP response with the specified status code and text message.
    /// <param name="code">The HTTP status code.</param>
    /// <param name="message">The response body text.</param>
    /// <returns>An HTTP response with the specified status code and plain text body.</returns>
    let statusText code message =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// Creates an HTTP response with the specified status code and JSON body.
    /// <param name="code">The HTTP status code.</param>
    /// <param name="value">The value to serialize as JSON.</param>
    /// <returns>An HTTP response with the specified status code and JSON body.</returns>
    let statusJson code value =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)
