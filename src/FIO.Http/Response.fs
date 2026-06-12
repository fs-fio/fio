namespace FIO.Http

[<RequireQualifiedAccess>]
module Response =

    let ok = HttpResponse.create HttpStatusCode.OK

    let okJson (value: 'T) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    let okText (text: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text text)

    let okHtml (html: string) =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" "text/html; charset=utf-8"
        |> HttpResponse.withBody (Text html)

    let okBytes (bytes: byte[]) contentType =
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Bytes bytes)

    let okStream stream length contentType =
        if isNull stream then
            invalidArg "stream" "Stream cannot be null"
        HttpResponse.create HttpStatusCode.OK
        |> HttpResponse.withHeader "Content-Type" contentType
        |> HttpResponse.withBody (Stream(stream, length))

    let created = HttpResponse.create HttpStatusCode.Created

    let createdAt location =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Location" location

    let createdJson (value: 'T) =
        HttpResponse.create HttpStatusCode.Created
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)

    let accepted = HttpResponse.create HttpStatusCode.Accepted

    let noContent = HttpResponse.create HttpStatusCode.NoContent

    let movedPermanently location =
        HttpResponse.create HttpStatusCode.MovedPermanently
        |> HttpResponse.withHeader "Location" location

    let found location =
        HttpResponse.create HttpStatusCode.Found
        |> HttpResponse.withHeader "Location" location

    let seeOther location =
        HttpResponse.create HttpStatusCode.SeeOther
        |> HttpResponse.withHeader "Location" location

    let notModified = HttpResponse.create HttpStatusCode.NotModified

    let temporaryRedirect location =
        HttpResponse.create HttpStatusCode.TemporaryRedirect
        |> HttpResponse.withHeader "Location" location

    let permanentRedirect location =
        HttpResponse.create HttpStatusCode.PermanentRedirect
        |> HttpResponse.withHeader "Location" location

    let badRequest = HttpResponse.create HttpStatusCode.BadRequest

    let badRequestText (message: string) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let badRequestJson (error: 'T) =
        HttpResponse.create HttpStatusCode.BadRequest
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json error)

    let unauthorized = HttpResponse.create HttpStatusCode.Unauthorized

    let unauthorizedWith scheme =
        HttpResponse.create HttpStatusCode.Unauthorized
        |> HttpResponse.withHeader "WWW-Authenticate" scheme

    let forbidden = HttpResponse.create HttpStatusCode.Forbidden

    let forbiddenText (message: string) =
        HttpResponse.create HttpStatusCode.Forbidden
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let notFound = HttpResponse.create HttpStatusCode.NotFound

    let notFoundText (message: string) =
        HttpResponse.create HttpStatusCode.NotFound
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let methodNotAllowed allowedMethods =
        HttpResponse.create HttpStatusCode.MethodNotAllowed
        |> HttpResponse.withHeader "Allow" (String.concat ", " allowedMethods)

    let requestTimeout = HttpResponse.create HttpStatusCode.RequestTimeout

    let conflict = HttpResponse.create HttpStatusCode.Conflict

    let conflictText (message: string) =
        HttpResponse.create HttpStatusCode.Conflict
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let unsupportedMediaType = HttpResponse.create HttpStatusCode.UnsupportedMediaType

    let unprocessableEntity = HttpResponse.create HttpStatusCode.UnprocessableEntity

    let unprocessableEntityJson (errors: 'T) =
        HttpResponse.create HttpStatusCode.UnprocessableEntity
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json errors)

    let tooManyRequests = HttpResponse.create HttpStatusCode.TooManyRequests

    let tooManyRequestsAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.TooManyRequests
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    let internalServerError = HttpResponse.create HttpStatusCode.InternalServerError

    let internalServerErrorText (message: string) =
        HttpResponse.create HttpStatusCode.InternalServerError
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let notImplemented = HttpResponse.create HttpStatusCode.NotImplemented

    let badGateway = HttpResponse.create HttpStatusCode.BadGateway

    let serviceUnavailable = HttpResponse.create HttpStatusCode.ServiceUnavailable

    let serviceUnavailableAfter retryAfterSeconds =
        HttpResponse.create HttpStatusCode.ServiceUnavailable
        |> HttpResponse.withHeader "Retry-After" (string retryAfterSeconds)

    let gatewayTimeout = HttpResponse.create HttpStatusCode.GatewayTimeout

    let status code = HttpResponse.create code

    let statusText code message =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "text/plain; charset=utf-8"
        |> HttpResponse.withBody (Text message)

    let statusJson code value =
        HttpResponse.create code
        |> HttpResponse.withHeader "Content-Type" "application/json; charset=utf-8"
        |> HttpResponse.withBody (Json value)
