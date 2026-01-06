/// <summary>
/// Pre-built HTTP response factory functions.
/// </summary>
namespace FSharp.FIO.Http

/// <summary>
/// Convenience functions for creating HTTP responses.
/// </summary>
[<RequireQualifiedAccess>]
module Response =

    /// <summary>
    /// Creates an HTTP 200 OK response.
    /// </summary>
    let ok =
        HttpResponse.create HttpStatusCode.OK

    /// <summary>
    /// Creates an HTTP 200 OK response with JSON body.
    /// Note: JSON serialization errors will occur during response writing (in KestrelBridge),
    /// not at response creation time. Ensure the value is serializable.
    /// </summary>
    /// <param name="value">The value to serialize as JSON.</param>
    let okJson (value: 'T) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    /// <summary>
    /// Creates an HTTP 200 OK response with plain text body.
    /// </summary>
    /// <param name="text">The text content.</param>
    let okText (text: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text text)

    /// <summary>
    /// Creates an HTTP 200 OK response with HTML body.
    /// </summary>
    /// <param name="html">The HTML content.</param>
    let okHtml (html: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/html; charset=utf-8"
        |> HttpResponse.withBody (Text html)

    /// <summary>
    /// Creates an HTTP 200 OK response with binary body.
    /// </summary>
    /// <param name="bytes">The binary content.</param>
    /// <param name="contentType">The content type header value.</param>
    let okBytes (bytes: byte[]) contentType =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Bytes bytes)

    /// <summary>
    /// Creates an HTTP 200 OK response with stream body.
    /// </summary>
    /// <param name="stream">The stream to send.</param>
    /// <param name="length">The optional content length.</param>
    /// <param name="contentType">The content type header value.</param>
    let okStream stream length contentType =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Stream (stream, length))

    /// <summary>
    /// Creates an HTTP 201 Created response.
    /// </summary>
    let created =
        HttpResponse.create HttpStatusCode.Created

    /// <summary>
    /// Creates an HTTP 201 Created response with Location header.
    /// </summary>
    /// <param name="location">The URI of the created resource.</param>
    let createdAt location =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 201 Created response with JSON body.
    /// </summary>
    /// <param name="value">The value to serialize as JSON.</param>
    let createdJson (value: 'T) =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    /// <summary>
    /// Creates an HTTP 202 Accepted response.
    /// </summary>
    let accepted =
        HttpResponse.create HttpStatusCode.Accepted

    /// <summary>
    /// Creates an HTTP 204 No Content response.
    /// </summary>
    let noContent =
        HttpResponse.create HttpStatusCode.NoContent

    /// <summary>
    /// Creates an HTTP 301 Moved Permanently redirect response.
    /// </summary>
    /// <param name="location">The new location URI.</param>
    let movedPermanently location =
        HttpResponse.create HttpStatusCode.MovedPermanently
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 302 Found redirect response.
    /// </summary>
    /// <param name="location">The redirect location URI.</param>
    let found location =
        HttpResponse.create HttpStatusCode.Found
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 303 See Other redirect response.
    /// </summary>
    /// <param name="location">The redirect location URI.</param>
    let seeOther location =
        HttpResponse.create HttpStatusCode.SeeOther
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 304 Not Modified response.
    /// </summary>
    let notModified =
        HttpResponse.create HttpStatusCode.NotModified

    /// <summary>
    /// Creates an HTTP 307 Temporary Redirect response.
    /// </summary>
    /// <param name="location">The redirect location URI.</param>
    let temporaryRedirect location =
        HttpResponse.create HttpStatusCode.TemporaryRedirect
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 308 Permanent Redirect response.
    /// </summary>
    /// <param name="location">The redirect location URI.</param>
    let permanentRedirect location =
        HttpResponse.create HttpStatusCode.PermanentRedirect
        |> HttpResponse.withHeader "Location" location

    /// <summary>
    /// Creates an HTTP 400 Bad Request response.
    /// </summary>
    let badRequest =
        HttpResponse.create HttpStatusCode.BadRequest

    /// <summary>
    /// Creates an HTTP 400 Bad Request response with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    let badRequestText (message: string) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP 400 Bad Request response with JSON error.
    /// </summary>
    /// <param name="error">The error object to serialize as JSON.</param>
    let badRequestJson (error: 'T) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json error)

    /// <summary>
    /// Creates an HTTP 401 Unauthorized response.
    /// </summary>
    let unauthorized =
        HttpResponse.create HttpStatusCode.Unauthorized

    /// <summary>
    /// Creates an HTTP 401 Unauthorized response with WWW-Authenticate header.
    /// </summary>
    /// <param name="scheme">The authentication scheme.</param>
    let unauthorizedWith scheme =
        HttpResponse.create HttpStatusCode.Unauthorized
        |> HttpResponse.withHeader "WWW-Authenticate" scheme

    /// <summary>
    /// Creates an HTTP 403 Forbidden response.
    /// </summary>
    let forbidden =
        HttpResponse.create HttpStatusCode.Forbidden

    /// <summary>
    /// Creates an HTTP 403 Forbidden response with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    let forbiddenText (message: string) =
        HttpResponse.create HttpStatusCode.Forbidden
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP 404 Not Found response.
    /// </summary>
    let notFound =
        HttpResponse.create HttpStatusCode.NotFound

    /// <summary>
    /// Creates an HTTP 404 Not Found response with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    let notFoundText (message: string) =
        HttpResponse.create HttpStatusCode.NotFound
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP 405 Method Not Allowed response.
    /// </summary>
    /// <param name="allowedMethods">The list of allowed HTTP methods.</param>
    let methodNotAllowed allowedMethods =
        HttpResponse.create HttpStatusCode.MethodNotAllowed
        |> HttpResponse.withHeader "Allow" (String.concat ", " allowedMethods)

    /// <summary>
    /// Creates an HTTP 408 Request Timeout response.
    /// </summary>
    let requestTimeout =
        HttpResponse.create HttpStatusCode.RequestTimeout

    /// <summary>
    /// Creates an HTTP 409 Conflict response.
    /// </summary>
    let conflict =
        HttpResponse.create HttpStatusCode.Conflict

    /// <summary>
    /// Creates an HTTP 409 Conflict response with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    let conflictText (message: string) =
        HttpResponse.create HttpStatusCode.Conflict
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP 415 Unsupported Media Type response.
    /// </summary>
    let unsupportedMediaType =
        HttpResponse.create HttpStatusCode.UnsupportedMediaType

    /// <summary>
    /// Creates an HTTP 422 Unprocessable Entity response.
    /// </summary>
    let unprocessableEntity =
        HttpResponse.create HttpStatusCode.UnprocessableEntity

    /// <summary>
    /// Creates an HTTP 422 Unprocessable Entity response with JSON errors.
    /// </summary>
    /// <param name="errors">The validation errors to serialize as JSON.</param>
    let unprocessableEntityJson (errors: 'T) =
        HttpResponse.create HttpStatusCode.UnprocessableEntity
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json errors)

    /// <summary>
    /// Creates an HTTP 429 Too Many Requests response.
    /// </summary>
    let tooManyRequests =
        HttpResponse.create HttpStatusCode.TooManyRequests

    /// <summary>
    /// Creates an HTTP 429 Too Many Requests response with Retry-After header.
    /// </summary>
    /// <param name="retryAfterSeconds">The number of seconds to wait before retrying.</param>
    let tooManyRequestsAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.TooManyRequests
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// <summary>
    /// Creates an HTTP 500 Internal Server Error response.
    /// </summary>
    let internalServerError =
        HttpResponse.create HttpStatusCode.InternalServerError

    /// <summary>
    /// Creates an HTTP 500 Internal Server Error response with text message.
    /// </summary>
    /// <param name="message">The error message.</param>
    let internalServerErrorText (message: string) =
        HttpResponse.create HttpStatusCode.InternalServerError
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP 501 Not Implemented response.
    /// </summary>
    let notImplemented =
        HttpResponse.create HttpStatusCode.NotImplemented

    /// <summary>
    /// Creates an HTTP 502 Bad Gateway response.
    /// </summary>
    let badGateway =
        HttpResponse.create HttpStatusCode.BadGateway

    /// <summary>
    /// Creates an HTTP 503 Service Unavailable response.
    /// </summary>
    let serviceUnavailable =
        HttpResponse.create HttpStatusCode.ServiceUnavailable

    /// <summary>
    /// Creates an HTTP 503 Service Unavailable response with Retry-After header.
    /// </summary>
    /// <param name="retryAfterSeconds">The number of seconds to wait before retrying.</param>
    let serviceUnavailableAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.ServiceUnavailable
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    /// <summary>
    /// Creates an HTTP 504 Gateway Timeout response.
    /// </summary>
    let gatewayTimeout =
        HttpResponse.create HttpStatusCode.GatewayTimeout

    /// <summary>
    /// Creates an HTTP response with the specified status code.
    /// </summary>
    /// <param name="code">The HTTP status code.</param>
    let status code =
        HttpResponse.create code

    /// <summary>
    /// Creates an HTTP response with the specified status code and text message.
    /// </summary>
    /// <param name="code">The HTTP status code.</param>
    /// <param name="message">The text message.</param>
    let statusText code message =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    /// <summary>
    /// Creates an HTTP response with the specified status code and JSON body.
    /// </summary>
    /// <param name="code">The HTTP status code.</param>
    /// <param name="value">The value to serialize as JSON.</param>
    let statusJson code value =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)
