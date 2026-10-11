namespace FIO.Http

open System
open System.IO
open System.Text

module internal JsonDefaults =

    let options =
        let options = Json.JsonSerializerOptions Json.JsonSerializerDefaults.Web
        options.TypeInfoResolver <- Json.Serialization.Metadata.DefaultJsonTypeInfoResolver()
        options.MakeReadOnly()
        options

module internal HeaderHelpers =

    let isValidHeaderName (name: string) =
        if String.IsNullOrWhiteSpace name then
            false
        else
            name
            |> Seq.forall (fun c ->
                c >= 'a' && c <= 'z'
                || (c >= 'A' && c <= 'Z')
                || c >= '0' && c <= '9'
                || c = '!'
                || c = '#'
                || c = '$'
                || c = '%'
                || c = '&'
                || c = '\''
                || c = '*'
                || c = '+'
                || c = '-'
                || c = '.'
                || c = '^'
                || c = '_'
                || c = '`'
                || c = '|'
                || c = '~')

    let tryFind (name: string) (map: Map<string, string list>) =
        map
        |> Map.tryPick (fun key value ->
            if String.Equals(key, name, StringComparison.OrdinalIgnoreCase) then Some value else None)

    let contains (name: string) (map: Map<string, string list>) =
        map |> Map.exists (fun key _ -> String.Equals(key, name, StringComparison.OrdinalIgnoreCase))

/// An error for modelling failures inside a route handler; the server itself never produces one.
type HttpError =
    /// The request did not match any registered route.
    | InvalidRoute of pattern: string
    /// Parsing the request failed.
    | ParsingFailed of message: string * exn
    /// A route handler raised an exception.
    | HandlerFailed of path: string * exn
    /// A middleware raised an exception.
    | MiddlewareFailed of name: string * exn
    /// The server failed to start or run.
    | ServerFailed of exn
    /// Reading the request body failed.
    | BodyReadFailed of exn
    /// JSON serialization or deserialization failed.
    | JsonFailed of exn
    /// The request timed out.
    | TimeoutError of message: string
    /// An otherwise-unclassified error.
    | GeneralError of exn

    override this.ToString () : string =
        match this with
        | InvalidRoute pattern -> $"Invalid route: {pattern}"
        | ParsingFailed(message, ex) -> $"Parsing failed: {message} - {ex.Message}"
        | HandlerFailed(path, ex) -> $"Handler failed [{path}]: {ex.Message}"
        | MiddlewareFailed(name, ex) -> $"Middleware {name} failed: {ex.Message}"
        | ServerFailed ex -> $"Server error: {ex.Message}"
        | BodyReadFailed ex -> $"Body read failed: {ex.Message}"
        | JsonFailed ex -> $"JSON error: {ex.Message}"
        | TimeoutError message -> $"Timeout: {message}"
        | GeneralError ex -> $"HTTP error: {ex.Message}"

[<RequireQualifiedAccess>]
module HttpError =

    /// Wraps an exception as a general HTTP error.
    let fromException (ex: exn) : HttpError =
        GeneralError ex

    /// Converts an HTTP error back into an exception: a GeneralError's own exception, otherwise a new one that keeps
    /// any wrapped exception as its inner exception.
    let toException (error: HttpError) : exn =
        match error with
        | GeneralError ex -> ex
        | ParsingFailed(_, ex)
        | HandlerFailed(_, ex)
        | MiddlewareFailed(_, ex)
        | ServerFailed ex
        | BodyReadFailed ex
        | JsonFailed ex -> Exception(error.ToString(), ex)
        | InvalidRoute _
        | TimeoutError _ -> Exception <| error.ToString()

/// An HTTP request method.
[<RequireQualifiedAccess>]
type HttpMethod =
    /// The GET method.
    | GET
    /// The POST method.
    | POST
    /// The PUT method.
    | PUT
    /// The DELETE method.
    | DELETE
    /// The PATCH method.
    | PATCH
    /// The HEAD method.
    | HEAD
    /// The OPTIONS method.
    | OPTIONS
    /// The TRACE method.
    | TRACE
    /// The CONNECT method.
    | CONNECT
    /// Any other method, by name.
    | Custom of string

    override this.ToString () : string =
        match this with
        | GET -> "GET"
        | POST -> "POST"
        | PUT -> "PUT"
        | DELETE -> "DELETE"
        | PATCH -> "PATCH"
        | HEAD -> "HEAD"
        | OPTIONS -> "OPTIONS"
        | TRACE -> "TRACE"
        | CONNECT -> "CONNECT"
        | Custom string -> string

[<RequireQualifiedAccess>]
module HttpMethod =

    /// Parses an HTTP method from a string, treating unknown values as Custom.
    let fromString (str: string) : HttpMethod =
        match str.ToUpperInvariant() with
        | "GET" -> HttpMethod.GET
        | "POST" -> HttpMethod.POST
        | "PUT" -> HttpMethod.PUT
        | "DELETE" -> HttpMethod.DELETE
        | "PATCH" -> HttpMethod.PATCH
        | "HEAD" -> HttpMethod.HEAD
        | "OPTIONS" -> HttpMethod.OPTIONS
        | "TRACE" -> HttpMethod.TRACE
        | "CONNECT" -> HttpMethod.CONNECT
        | str -> HttpMethod.Custom str

/// An HTTP response status code.
[<RequireQualifiedAccess>]
type HttpStatusCode =
    /// 100: the client should continue sending its request.
    | Continue = 100
    /// 101: the server is switching to the protocol the client asked for.
    | SwitchingProtocols = 101
    /// 102: the server has accepted the request but not yet finished it.
    | Processing = 102

    /// 200: the request succeeded.
    | OK = 200
    /// 201: the request succeeded and created a new resource.
    | Created = 201
    /// 202: the request was accepted for processing, which has not finished.
    | Accepted = 202
    /// 203: the request succeeded, with content modified by a proxy.
    | NonAuthoritativeInformation = 203
    /// 204: the request succeeded and there is no content to send.
    | NoContent = 204
    /// 205: the request succeeded; the client should reset its view of the document.
    | ResetContent = 205
    /// 206: the server is sending only the requested range of the resource.
    | PartialContent = 206

    /// 300: the resource has several representations to choose from.
    | MultipleChoices = 300
    /// 301: the resource has moved permanently to another URI.
    | MovedPermanently = 301
    /// 302: the resource is temporarily at another URI.
    | Found = 302
    /// 303: the client should get the result from another URI with GET.
    | SeeOther = 303
    /// 304: the client's cached copy is still valid.
    | NotModified = 304
    /// 305: deprecated; the resource must be reached through a proxy.
    | UseProxy = 305
    /// 307: the resource is temporarily at another URI; repeat the request unchanged there.
    | TemporaryRedirect = 307
    /// 308: the resource has moved permanently; repeat the request unchanged at the new URI.
    | PermanentRedirect = 308

    /// 400: the server will not process the request because of an apparent client error.
    | BadRequest = 400
    /// 401: the request needs valid authentication.
    | Unauthorized = 401
    /// 402: reserved for future use.
    | PaymentRequired = 402
    /// 403: the server understood the request but refuses to fulfil it.
    | Forbidden = 403
    /// 404: the server cannot find the requested resource.
    | NotFound = 404
    /// 405: the resource does not support the request's method.
    | MethodNotAllowed = 405
    /// 406: no representation matches the request's Accept headers.
    | NotAcceptable = 406
    /// 407: the client must authenticate with the proxy.
    | ProxyAuthenticationRequired = 407
    /// 408: the server timed out waiting for the request.
    | RequestTimeout = 408
    /// 409: the request conflicts with the current state of the resource.
    | Conflict = 409
    /// 410: the resource is no longer available, and likely never will be again.
    | Gone = 410
    /// 411: the request must carry a Content-Length header.
    | LengthRequired = 411
    /// 412: a precondition in the request's headers is false.
    | PreconditionFailed = 412
    /// 413: the request body is larger than the server accepts.
    | PayloadTooLarge = 413
    /// 414: the request URI is longer than the server accepts.
    | URITooLong = 414
    /// 415: the server does not support the request body's media type.
    | UnsupportedMediaType = 415
    /// 416: the requested range lies outside the resource.
    | RangeNotSatisfiable = 416
    /// 417: the server cannot meet the request's Expect header.
    | ExpectationFailed = 417
    /// 418: reserved and unused; RFC 2324's April Fools' teapot.
    | ImATeapot = 418
    /// 422: the request is well-formed but its content cannot be processed.
    | UnprocessableEntity = 422
    /// 423: the resource is locked.
    | Locked = 423
    /// 424: the request failed because an action it depended on failed.
    | FailedDependency = 424
    /// 425: the server will not process a request that might be replayed.
    | TooEarly = 425
    /// 426: the client must switch to another protocol.
    | UpgradeRequired = 426
    /// 428: the server requires the request to be conditional.
    | PreconditionRequired = 428
    /// 429: the client has sent too many requests in a given time.
    | TooManyRequests = 429
    /// 431: the request's headers are too large.
    | RequestHeaderFieldsTooLarge = 431
    /// 451: the resource is unavailable for legal reasons.
    | UnavailableForLegalReasons = 451

    /// 500: the server hit an unexpected condition.
    | InternalServerError = 500
    /// 501: the server does not support the functionality the request needs.
    | NotImplemented = 501
    /// 502: the server, acting as a gateway, got an invalid response upstream.
    | BadGateway = 502
    /// 503: the server cannot handle the request right now.
    | ServiceUnavailable = 503
    /// 504: the server, acting as a gateway, got no response upstream in time.
    | GatewayTimeout = 504
    /// 505: the server does not support the request's HTTP version.
    | HTTPVersionNotSupported = 505
    /// 506: the server has a content-negotiation configuration error.
    | VariantAlsoNegotiates = 506
    /// 507: the server cannot store what the request needs.
    | InsufficientStorage = 507
    /// 508: the server found an infinite loop while processing the request.
    | LoopDetected = 508
    /// 510: the request needs further extensions to be fulfilled.
    | NotExtended = 510
    /// 511: the client must authenticate to gain network access.
    | NetworkAuthenticationRequired = 511

/// The body of an HTTP request.
[<RequireQualifiedAccess>]
type RequestBody =
    /// No body.
    | Empty
    /// A raw byte body.
    | Bytes of byte[]
    /// A text body.
    | Text of string

    /// Returns the body as a byte array.
    member this.AsBytes () : byte[] =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    /// Returns the body as a string.
    member this.AsString () : string =
        match this with
        | Empty -> ""
        | Text text -> text
        | Bytes bytes -> Encoding.UTF8.GetString bytes

/// The body of an HTTP response.
[<RequireQualifiedAccess>]
type ResponseBody =
    /// No body.
    | Empty
    /// A streamed body, with an optional known length.
    | Stream of stream: Stream * length: int64 option
    /// A raw byte body.
    | Bytes of byte[]
    /// A text body.
    | Text of string
    /// A value to be serialized as JSON.
    | Json of obj

    /// The body's content length in bytes, if known.
    member this.ContentLength : int64 option =
        match this with
        | Empty -> Some 0L
        | Bytes bytes -> Some <| int64 bytes.Length
        | Text text -> Some <| int64 (Encoding.UTF8.GetByteCount text)
        | Stream(_, length) -> length
        | Json _ -> None

/// An incoming HTTP request.
type HttpRequest =
    {
        /// The request method.
        Method: HttpMethod
        /// The request path.
        Path: string
        /// The path split into non-empty segments.
        PathSegments: string list
        /// Query-string parameters, each mapping to its values.
        QueryParams: Map<string, string list>
        /// Request headers, each mapping to its values.
        Headers: Map<string, string list>
        /// The request body.
        Body: RequestBody
        /// Arbitrary per-request metadata.
        Metadata: Map<string, obj>
    }

[<RequireQualifiedAccess>]
module HttpRequest =

    /// Creates a request with the given method and path.
    let create method (path: string) : HttpRequest =
        {
            Method = method
            Path = path
            PathSegments = path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            QueryParams = Map.empty
            Headers = Map.empty
            Body = RequestBody.Empty
            Metadata = Map.empty
        }

    /// Adds a query parameter value to a request.
    let withQueryParam name value request : HttpRequest =
        let values =
            request.QueryParams
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]
        { request with
            QueryParams = Map.add name values request.QueryParams
        }

    /// Adds a header value to a request.
    let withHeader name value request : HttpRequest =
        if not (HeaderHelpers.isValidHeaderName name) then
            invalidArg
                "name"
                $"Invalid HTTP header name: '{name}'. Header names must consist of visible ASCII characters (RFC 7230)."
        let values =
            request.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]
        { request with
            Headers = Map.add name values request.Headers
        }

    /// Sets a request's body.
    let withBody body request : HttpRequest =
        { request with Body = body }

    /// Attaches a metadata value to a request.
    let withMetadata key value request : HttpRequest =
        { request with
            Metadata = Map.add key value request.Metadata
        }

    /// Returns the first value of a query parameter, if present.
    let queryParam name request : string option =
        request.QueryParams
        |> Map.tryFind name
        |> Option.bind List.tryHead

    /// Returns all values of a query parameter.
    let queryParams name request : string list =
        request.QueryParams
        |> Map.tryFind name
        |> Option.defaultValue []

    /// Returns the first value of a header, if present.
    let header name request : string option =
        HeaderHelpers.tryFind name request.Headers
        |> Option.bind List.tryHead

    /// Returns all values of a header.
    let headers name request : string list =
        HeaderHelpers.tryFind name request.Headers
        |> Option.defaultValue []

    let private encodingFromContentType (contentType: string option) =
        match contentType with
        | Some value ->
            value.Split ';'
            |> Array.tryPick (fun part ->
                let part = part.Trim()
                if part.StartsWith("charset=", StringComparison.OrdinalIgnoreCase) then
                    let name = part.Substring(8).Trim().Trim '"'
                    try Some(Encoding.GetEncoding name) with _ -> None
                else
                    None)
            |> Option.defaultValue Encoding.UTF8
        | None -> Encoding.UTF8

    /// Returns the request body decoded as text.
    let bodyText (request: HttpRequest) : string =
        match request.Body with
        | RequestBody.Empty -> ""
        | RequestBody.Text text -> text
        | RequestBody.Bytes bytes ->
            let encoding = encodingFromContentType (header "Content-Type" request)
            encoding.GetString bytes

    /// Returns a typed metadata value, if present and of the expected type.
    let metadata<'A> key request : 'A option =
        request.Metadata
        |> Map.tryFind key
        |> Option.bind (fun o ->
            match o with
            | :? 'A as value -> Some value
            | _ -> None)

/// An outgoing HTTP response.
type HttpResponse =
    {
        /// The response status code.
        Status: HttpStatusCode
        /// Response headers, each mapping to its values.
        Headers: Map<string, string list>
        /// The response body.
        Body: ResponseBody
    }

[<RequireQualifiedAccess>]
module HttpResponse =

    /// Creates a response with the given status and an empty body.
    let create status : HttpResponse =
        { Status = status; Headers = Map.empty; Body = ResponseBody.Empty }

    /// Adds a header value to a response.
    let withHeader name value response : HttpResponse =
        if not (HeaderHelpers.isValidHeaderName name) then
            invalidArg
                "name"
                $"Invalid HTTP header name: '{name}'. Header names must consist of visible ASCII characters (RFC 7230)."
        let values =
            response.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]
        { response with
            Headers = Map.add name values response.Headers
        }

    /// Sets a response's body.
    let withBody (body: ResponseBody) (response: HttpResponse) : HttpResponse =
        { response with Body = body }

    /// Sets a response's status code.
    let withStatus status response : HttpResponse =
        { response with Status = status }

    /// Returns the first value of a header, if present.
    let header name response : string option =
        HeaderHelpers.tryFind name response.Headers
        |> Option.bind List.tryHead

    /// Returns all values of a header.
    let headers name response : string list =
        HeaderHelpers.tryFind name response.Headers
        |> Option.defaultValue []
