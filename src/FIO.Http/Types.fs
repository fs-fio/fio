namespace FIO.Http

open System
open System.IO
open System.Text

/// <summary>Represents an error from an HTTP operation.</summary>
type HttpError =
    /// <summary>Represents an invalid route pattern error.</summary>
    | InvalidRoute of pattern: string
    /// <summary>Represents a request parsing failure.</summary>
    | ParsingFailed of message: string * exn
    /// <summary>Represents a handler execution failure.</summary>
    | HandlerFailed of path: string * exn
    /// <summary>Represents a middleware execution failure.</summary>
    | MiddlewareFailed of name: string * exn
    /// <summary>Represents a server startup failure.</summary>
    | ServerFailed of exn
    /// <summary>Represents a request body reading failure.</summary>
    | BodyReadFailed of exn
    /// <summary>Represents a JSON serialization or deserialization failure.</summary>
    | JsonFailed of exn
    /// <summary>Represents a request timeout.</summary>
    | TimeoutError of message: string
    /// <summary>Represents a general HTTP error.</summary>
    | GeneralError of exn

    /// <summary>Returns a human-readable error message.</summary>
    /// <returns>A human-readable string describing the error.</returns>
    override this.ToString() =
        match this with
        | InvalidRoute pattern -> $"Invalid route: {pattern}"
        | ParsingFailed(msg, exn) -> $"Parsing failed: {msg} - {exn.Message}"
        | HandlerFailed(path, exn) -> $"Handler failed [{path}]: {exn.Message}"
        | MiddlewareFailed(name, exn) -> $"Middleware {name} failed: {exn.Message}"
        | ServerFailed exn -> $"Server error: {exn.Message}"
        | BodyReadFailed exn -> $"Body read failed: {exn.Message}"
        | JsonFailed exn -> $"JSON error: {exn.Message}"
        | TimeoutError msg -> $"Timeout: {msg}"
        | GeneralError exn -> $"HTTP error: {exn.Message}"

/// <summary>Provides functions for converting HTTP errors.</summary>
module HttpError =

    /// <summary>Lifts an exception into an HttpError.</summary>
    /// <param name="exn">The exception to wrap.</param>
    /// <returns>A GeneralError wrapping the exception.</returns>
    let fromException (exn: exn) : HttpError = GeneralError exn

    /// <summary>Returns the underlying exception for an HTTP error.</summary>
    /// <param name="err">The HTTP error to convert.</param>
    /// <returns>The underlying exception, or a new exception wrapping the error message.</returns>
    let toException (err: HttpError) : exn =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// <summary>Represents an HTTP request method.</summary>
[<RequireQualifiedAccess>]
type HttpMethod =
    /// <summary>Represents the HTTP GET method.</summary>
    | GET
    /// <summary>Represents the HTTP POST method.</summary>
    | POST
    /// <summary>Represents the HTTP PUT method.</summary>
    | PUT
    /// <summary>Represents the HTTP DELETE method.</summary>
    | DELETE
    /// <summary>Represents the HTTP PATCH method.</summary>
    | PATCH
    /// <summary>Represents the HTTP HEAD method.</summary>
    | HEAD
    /// <summary>Represents the HTTP OPTIONS method.</summary>
    | OPTIONS
    /// <summary>Represents the HTTP TRACE method.</summary>
    | TRACE
    /// <summary>Represents the HTTP CONNECT method.</summary>
    | CONNECT
    /// <summary>Represents a custom HTTP method.</summary>
    | Custom of string

    /// <summary>Returns the string representation of this HTTP method.</summary>
    /// <returns>The HTTP method name as a string.</returns>
    override this.ToString() =
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

/// <summary>Provides functions for parsing HTTP methods.</summary>
module HttpMethod =

    /// <summary>Creates an HTTP method from its string representation.</summary>
    /// <param name="str">The string representation of the HTTP method.</param>
    /// <returns>The parsed HttpMethod, or Custom for non-standard methods.</returns>
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
        | s -> HttpMethod.Custom s

/// <summary>Represents a standard HTTP status code.</summary>
[<RequireQualifiedAccess>]
type HttpStatusCode =
    /// <summary>Represents the HTTP 100 Continue status code.</summary>
    | Continue = 100
    /// <summary>Represents the HTTP 101 Switching Protocols status code.</summary>
    | SwitchingProtocols = 101
    /// <summary>Represents the HTTP 102 Processing status code.</summary>
    | Processing = 102

    /// <summary>Represents the HTTP 200 OK status code.</summary>
    | OK = 200
    /// <summary>Represents the HTTP 201 Created status code.</summary>
    | Created = 201
    /// <summary>Represents the HTTP 202 Accepted status code.</summary>
    | Accepted = 202
    /// <summary>Represents the HTTP 203 Non-Authoritative Information status code.</summary>
    | NonAuthoritativeInformation = 203
    /// <summary>Represents the HTTP 204 No Content status code.</summary>
    | NoContent = 204
    /// <summary>Represents the HTTP 205 Reset Content status code.</summary>
    | ResetContent = 205
    /// <summary>Represents the HTTP 206 Partial Content status code.</summary>
    | PartialContent = 206

    /// <summary>Represents the HTTP 300 Multiple Choices status code.</summary>
    | MultipleChoices = 300
    /// <summary>Represents the HTTP 301 Moved Permanently status code.</summary>
    | MovedPermanently = 301
    /// <summary>Represents the HTTP 302 Found status code.</summary>
    | Found = 302
    /// <summary>Represents the HTTP 303 See Other status code.</summary>
    | SeeOther = 303
    /// <summary>Represents the HTTP 304 Not Modified status code.</summary>
    | NotModified = 304
    /// <summary>Represents the HTTP 305 Use Proxy status code.</summary>
    | UseProxy = 305
    /// <summary>Represents the HTTP 307 Temporary Redirect status code.</summary>
    | TemporaryRedirect = 307
    /// <summary>Represents the HTTP 308 Permanent Redirect status code.</summary>
    | PermanentRedirect = 308

    /// <summary>Represents the HTTP 400 Bad Request status code.</summary>
    | BadRequest = 400
    /// <summary>Represents the HTTP 401 Unauthorized status code.</summary>
    | Unauthorized = 401
    /// <summary>Represents the HTTP 402 Payment Required status code.</summary>
    | PaymentRequired = 402
    /// <summary>Represents the HTTP 403 Forbidden status code.</summary>
    | Forbidden = 403
    /// <summary>Represents the HTTP 404 Not Found status code.</summary>
    | NotFound = 404
    /// <summary>Represents the HTTP 405 Method Not Allowed status code.</summary>
    | MethodNotAllowed = 405
    /// <summary>Represents the HTTP 406 Not Acceptable status code.</summary>
    | NotAcceptable = 406
    /// <summary>Represents the HTTP 407 Proxy Authentication Required status code.</summary>
    | ProxyAuthenticationRequired = 407
    /// <summary>Represents the HTTP 408 Request Timeout status code.</summary>
    | RequestTimeout = 408
    /// <summary>Represents the HTTP 409 Conflict status code.</summary>
    | Conflict = 409
    /// <summary>Represents the HTTP 410 Gone status code.</summary>
    | Gone = 410
    /// <summary>Represents the HTTP 411 Length Required status code.</summary>
    | LengthRequired = 411
    /// <summary>Represents the HTTP 412 Precondition Failed status code.</summary>
    | PreconditionFailed = 412
    /// <summary>Represents the HTTP 413 Payload Too Large status code.</summary>
    | PayloadTooLarge = 413
    /// <summary>Represents the HTTP 414 URI Too Long status code.</summary>
    | URITooLong = 414
    /// <summary>Represents the HTTP 415 Unsupported Media Type status code.</summary>
    | UnsupportedMediaType = 415
    /// <summary>Represents the HTTP 416 Range Not Satisfiable status code.</summary>
    | RangeNotSatisfiable = 416
    /// <summary>Represents the HTTP 417 Expectation Failed status code.</summary>
    | ExpectationFailed = 417
    /// <summary>Represents the HTTP 418 I'm a Teapot status code.</summary>
    | ImATeapot = 418
    /// <summary>Represents the HTTP 422 Unprocessable Entity status code.</summary>
    | UnprocessableEntity = 422
    /// <summary>Represents the HTTP 423 Locked status code.</summary>
    | Locked = 423
    /// <summary>Represents the HTTP 424 Failed Dependency status code.</summary>
    | FailedDependency = 424
    /// <summary>Represents the HTTP 425 Too Early status code.</summary>
    | TooEarly = 425
    /// <summary>Represents the HTTP 426 Upgrade Required status code.</summary>
    | UpgradeRequired = 426
    /// <summary>Represents the HTTP 428 Precondition Required status code.</summary>
    | PreconditionRequired = 428
    /// <summary>Represents the HTTP 429 Too Many Requests status code.</summary>
    | TooManyRequests = 429
    /// <summary>Represents the HTTP 431 Request Header Fields Too Large status code.</summary>
    | RequestHeaderFieldsTooLarge = 431
    /// <summary>Represents the HTTP 451 Unavailable For Legal Reasons status code.</summary>
    | UnavailableForLegalReasons = 451

    /// <summary>Represents the HTTP 500 Internal Server Error status code.</summary>
    | InternalServerError = 500
    /// <summary>Represents the HTTP 501 Not Implemented status code.</summary>
    | NotImplemented = 501
    /// <summary>Represents the HTTP 502 Bad Gateway status code.</summary>
    | BadGateway = 502
    /// <summary>Represents the HTTP 503 Service Unavailable status code.</summary>
    | ServiceUnavailable = 503
    /// <summary>Represents the HTTP 504 Gateway Timeout status code.</summary>
    | GatewayTimeout = 504
    /// <summary>Represents the HTTP 505 HTTP Version Not Supported status code.</summary>
    | HTTPVersionNotSupported = 505
    /// <summary>Represents the HTTP 506 Variant Also Negotiates status code.</summary>
    | VariantAlsoNegotiates = 506
    /// <summary>Represents the HTTP 507 Insufficient Storage status code.</summary>
    | InsufficientStorage = 507
    /// <summary>Represents the HTTP 508 Loop Detected status code.</summary>
    | LoopDetected = 508
    /// <summary>Represents the HTTP 510 Not Extended status code.</summary>
    | NotExtended = 510
    /// <summary>Represents the HTTP 511 Network Authentication Required status code.</summary>
    | NetworkAuthenticationRequired = 511

/// <summary>Represents the body of an HTTP request.</summary>
/// <remarks>Request bodies are automatically buffered at the Kestrel boundary.</remarks>
type RequestBody =
    /// <summary>Represents an empty request body.</summary>
    | Empty
    /// <summary>Represents a raw binary request body.</summary>
    | Bytes of byte[]
    /// <summary>Represents a UTF-8 text request body.</summary>
    | Text of string

    /// <summary>Returns the request body as a byte array.</summary>
    /// <returns>A byte array representing the body content, or an empty array for an empty body.</returns>
    member this.AsBytes() =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    /// <summary>Returns the request body as a UTF-8 string.</summary>
    /// <returns>A string representing the body content, or an empty string for an empty body.</returns>
    member this.AsString() =
        match this with
        | Empty -> ""
        | Text text -> text
        | Bytes bytes -> Encoding.UTF8.GetString bytes

/// <summary>Represents the body of an HTTP response.</summary>
/// <remarks>For Stream bodies, the caller is responsible for disposing the stream. Streams must not be null.</remarks>
type ResponseBody =
    /// <summary>Represents an empty response body.</summary>
    | Empty
    /// <summary>Represents a streamed response body with optional content length.</summary>
    | Stream of stream: Stream * length: int64 option
    /// <summary>Represents a raw binary response body.</summary>
    | Bytes of byte[]
    /// <summary>Represents a UTF-8 text response body.</summary>
    | Text of string
    /// <summary>Represents a JSON-serializable object response body.</summary>
    | Json of obj

    /// <summary>Returns the content length in bytes, or None if not determinable before serialization.</summary>
    /// <returns>The content length if the body has a known size; None for streaming or empty bodies.</returns>
    member this.ContentLength =
        match this with
        | Empty -> Some 0L
        | Bytes bytes -> Some(int64 bytes.Length)
        | Text text -> Some(int64 (Encoding.UTF8.GetByteCount text))
        | Stream(_, length) -> length
        | Json _ -> None // Will be determined after serialization

/// <summary>Represents an HTTP request.</summary>
type HttpRequest =
    {
        /// <summary>Represents the HTTP method of the request.</summary>
        Method: HttpMethod
        /// <summary>Represents the request path.</summary>
        Path: string
        /// <summary>Represents the parsed path segments.</summary>
        PathSegments: string list
        /// <summary>Represents the parsed query string parameters.</summary>
        QueryParams: Map<string, string list>
        /// <summary>Represents the request headers.</summary>
        Headers: Map<string, string list>
        /// <summary>Represents the request body.</summary>
        Body: RequestBody
        /// <summary>Represents the request metadata for passing data between middleware and handlers.</summary>
        Metadata: Map<string, obj>
    }

/// <summary>Provides functions for creating and querying HTTP requests.</summary>
module HttpRequest =

    /// <summary>Creates a new HTTP request with the given method and path.</summary>
    /// <param name="method">The HTTP method for the request.</param>
    /// <param name="path">The request path.</param>
    /// <returns>A new HttpRequest with empty query params, headers, body, and metadata.</returns>
    let create method (path: string) =
        {
            Method = method
            Path = path
            PathSegments = path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            QueryParams = Map.empty
            Headers = Map.empty
            Body = RequestBody.Empty
            Metadata = Map.empty
        }

    /// <summary>Creates a new request with the given query parameter appended.</summary>
    /// <param name="name">The query parameter name.</param>
    /// <param name="value">The query parameter value.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the query parameter appended.</returns>
    let withQueryParam name value request =
        let values =
            request.QueryParams
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]

        { request with
            QueryParams = Map.add name values request.QueryParams
        }

    /// <summary>Creates a new request with the given header value appended.</summary>
    /// <param name="name">The header name.</param>
    /// <param name="value">The header value.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the header appended.</returns>
    let withHeader name value request =
        let values =
            request.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]

        { request with
            Headers = Map.add name values request.Headers
        }

    /// <summary>Creates a new request with the specified body.</summary>
    /// <param name="body">The request body to set.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the specified body.</returns>
    let withBody body request = { request with Body = body }

    /// <summary>Creates a new request with the given metadata entry added or replaced.</summary>
    /// <param name="key">The metadata key.</param>
    /// <param name="value">The metadata value.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the metadata entry added or replaced.</returns>
    let withMetadata key value request =
        { request with
            Metadata = Map.add key value request.Metadata
        }

    /// <summary>Returns the first value for a query parameter, or None if not found.</summary>
    /// <param name="name">The query parameter name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>The first value for the parameter, or None if not found.</returns>
    let queryParam name request =
        request.QueryParams |> Map.tryFind name |> Option.bind List.tryHead

    /// <summary>Returns all values for a query parameter, or an empty list if not found.</summary>
    /// <param name="name">The query parameter name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>All values for the parameter, or an empty list if not found.</returns>
    let queryParams name request =
        request.QueryParams |> Map.tryFind name |> Option.defaultValue []

    /// <summary>Returns the first value for a header, or None if not found.</summary>
    /// <param name="name">The header name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>The first value for the header, or None if not found.</returns>
    let header name request =
        request.Headers |> Map.tryFind name |> Option.bind List.tryHead

    /// <summary>Returns all values for a header, or an empty list if not found.</summary>
    /// <param name="name">The header name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>All values for the header, or an empty list if not found.</returns>
    let headers name request =
        request.Headers |> Map.tryFind name |> Option.defaultValue []

    /// <summary>Returns typed metadata from the request, or None if the key is missing or the value cannot be cast.</summary>
    /// <param name="key">The metadata key to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>The typed metadata value, or None if not found or not castable.</returns>
    let metadata<'T> key request =
        request.Metadata
        |> Map.tryFind key
        |> Option.bind (fun o ->
            match o with
            | :? 'T as t -> Some t
            | _ -> None)

/// <summary>Represents an HTTP response.</summary>
type HttpResponse =
    {
        /// <summary>Represents the HTTP status code of the response.</summary>
        Status: HttpStatusCode
        /// <summary>Represents the response headers.</summary>
        Headers: Map<string, string list>
        /// <summary>Represents the response body.</summary>
        Body: ResponseBody
    }

/// <summary>Provides functions for creating and modifying HTTP responses.</summary>
module HttpResponse =

    /// <summary>Returns whether the given string is a valid HTTP header name according to RFC 7230.</summary>
    /// <param name="name">The header name to validate.</param>
    /// <returns><c>true</c> if every character is a valid token character; <c>false</c> otherwise.</returns>
    let private isValidHeaderName (name: string) =
        if String.IsNullOrWhiteSpace name then
            false
        else
            // RFC 7230: token = 1*tchar
            // tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
            //         "0"-"9" / "A"-"Z" / "^" / "_" / "`" / "a"-"z" / "|" / "~"
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

    /// <summary>Creates a new HTTP response with the specified status code.</summary>
    /// <param name="status">The HTTP status code.</param>
    /// <returns>A new HttpResponse with empty headers and no body.</returns>
    let create status =
        { Status = status; Headers = Map.empty; Body = Empty }

    /// <summary>Creates a new response with the given header value appended.</summary>
    /// <param name="name">The header name (must conform to RFC 7230).</param>
    /// <param name="value">The header value.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the header appended.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the header name is invalid according to RFC 7230.</exception>
    let withHeader name value response =
        if not (isValidHeaderName name) then
            invalidArg
                "name"
                (sprintf
                    "Invalid HTTP header name: '%s'. Header names must consist of visible ASCII characters (RFC 7230)."
                    name)

        let values =
            response.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]

        { response with
            Headers = Map.add name values response.Headers
        }

    /// <summary>Creates a new response with the specified body.</summary>
    /// <param name="body">The response body to set.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the specified body.</returns>
    let withBody (body: ResponseBody) (response: HttpResponse) : HttpResponse = { response with Body = body }

    /// <summary>Creates a new response with the specified status code.</summary>
    /// <param name="status">The HTTP status code to set.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the specified status code.</returns>
    let withStatus status response = { response with Status = status }

    /// <summary>Returns the first value for a header, or None if not found.</summary>
    /// <param name="name">The header name to look up.</param>
    /// <param name="response">The response to search.</param>
    /// <returns>The first value for the header, or None if not found.</returns>
    let header name response =
        response.Headers |> Map.tryFind name |> Option.bind List.tryHead

    /// <summary>Returns all values for a header, or an empty list if not found.</summary>
    /// <param name="name">The header name to look up.</param>
    /// <param name="response">The response to search.</param>
    /// <returns>All values for the header, or an empty list if not found.</returns>
    let headers name response =
        response.Headers |> Map.tryFind name |> Option.defaultValue []
