namespace FIO.Http

open System
open System.IO
open System.Text

/// HTTP operation errors.
type HttpError =
    /// Invalid route pattern
    | InvalidRoute of pattern: string
    /// Request parsing failed
    | ParsingFailed of message: string * exn
    /// Handler execution failed
    | HandlerFailed of path: string * exn
    /// Middleware execution failed
    | MiddlewareFailed of name: string * exn
    /// Server startup failed
    | ServerFailed of exn
    /// Request body reading failed
    | BodyReadFailed of exn
    /// JSON serialization/deserialization failed
    | JsonFailed of exn
    /// Request timeout
    | TimeoutError of message: string
    /// General HTTP error
    | GeneralError of exn

    /// Gets a human-readable error message.
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

/// Module functions for working with HttpError.
module HttpError =

    /// Converts an exception to an HttpError.
    /// <param name="exn">The exception to wrap.</param>
    /// <returns>A GeneralError wrapping the exception.</returns>
    let fromException (exn: exn) : HttpError = GeneralError exn

    /// Converts an HttpError to an exception.
    /// <param name="err">The HTTP error to convert.</param>
    /// <returns>The underlying exception, or a new exception wrapping the error message.</returns>
    let toException (err: HttpError) : exn =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// HTTP request methods.
[<RequireQualifiedAccess>]
type HttpMethod =
    /// HTTP GET method.
    | GET
    /// HTTP POST method.
    | POST
    /// HTTP PUT method.
    | PUT
    /// HTTP DELETE method.
    | DELETE
    /// HTTP PATCH method.
    | PATCH
    /// HTTP HEAD method.
    | HEAD
    /// HTTP OPTIONS method.
    | OPTIONS
    /// HTTP TRACE method.
    | TRACE
    /// HTTP CONNECT method.
    | CONNECT
    /// Custom HTTP method.
    | Custom of string

    /// Returns the string representation of the HTTP method.
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

/// Module functions for working with HttpMethod.
module HttpMethod =

    /// Parses an HTTP method from a string.
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

/// HTTP status codes.
[<RequireQualifiedAccess>]
type HttpStatusCode =
    | Continue = 100
    | SwitchingProtocols = 101
    | Processing = 102

    | OK = 200
    | Created = 201
    | Accepted = 202
    | NonAuthoritativeInformation = 203
    | NoContent = 204
    | ResetContent = 205
    | PartialContent = 206

    | MultipleChoices = 300
    | MovedPermanently = 301
    | Found = 302
    | SeeOther = 303
    | NotModified = 304
    | UseProxy = 305
    | TemporaryRedirect = 307
    | PermanentRedirect = 308

    | BadRequest = 400
    | Unauthorized = 401
    | PaymentRequired = 402
    | Forbidden = 403
    | NotFound = 404
    | MethodNotAllowed = 405
    | NotAcceptable = 406
    | ProxyAuthenticationRequired = 407
    | RequestTimeout = 408
    | Conflict = 409
    | Gone = 410
    | LengthRequired = 411
    | PreconditionFailed = 412
    | PayloadTooLarge = 413
    | URITooLong = 414
    | UnsupportedMediaType = 415
    | RangeNotSatisfiable = 416
    | ExpectationFailed = 417
    | ImATeapot = 418
    | UnprocessableEntity = 422
    | Locked = 423
    | FailedDependency = 424
    | TooEarly = 425
    | UpgradeRequired = 426
    | PreconditionRequired = 428
    | TooManyRequests = 429
    | RequestHeaderFieldsTooLarge = 431
    | UnavailableForLegalReasons = 451

    | InternalServerError = 500
    | NotImplemented = 501
    | BadGateway = 502
    | ServiceUnavailable = 503
    | GatewayTimeout = 504
    | HTTPVersionNotSupported = 505
    | VariantAlsoNegotiates = 506
    | InsufficientStorage = 507
    | LoopDetected = 508
    | NotExtended = 510
    | NetworkAuthenticationRequired = 511

/// Represents the body of an HTTP request.
/// Request bodies are automatically buffered at the Kestrel boundary.
type RequestBody =
    /// No body content.
    | Empty
    /// Raw binary body.
    | Bytes of byte[]
    /// UTF-8 text body.
    | Text of string

    /// Converts the request body to a byte array.
    member this.AsBytes() =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    /// Converts the request body to a string.
    member this.AsString() =
        match this with
        | Empty -> ""
        | Text text -> text
        | Bytes bytes -> Encoding.UTF8.GetString bytes

/// Represents the body of an HTTP response.
/// Note: For Stream bodies, the caller is responsible for disposing the stream.
/// Streams must not be null.
type ResponseBody =
    /// No body content.
    | Empty
    /// Streamed body with optional content length. Caller is responsible for disposing the stream.
    | Stream of stream: Stream * length: int64 option
    /// Raw binary body.
    | Bytes of byte[]
    /// UTF-8 text body.
    | Text of string
    /// JSON-serializable object body.
    | Json of obj

    /// Gets the content length, or None if not determinable.
    member this.ContentLength =
        match this with
        | Empty -> Some 0L
        | Bytes bytes -> Some(int64 bytes.Length)
        | Text text -> Some(int64 (Encoding.UTF8.GetByteCount text))
        | Stream(_, length) -> length
        | Json _ -> None // Will be determined after serialization

/// Represents an HTTP request.
type HttpRequest =
    {
        Method: HttpMethod
        Path: string
        PathSegments: string list
        QueryParams: Map<string, string list>
        Headers: Map<string, string list>
        Body: RequestBody
        Metadata: Map<string, obj>
    }

/// Functions for working with HTTP requests.
module HttpRequest =

    /// Creates a new HTTP request.
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

    /// Adds a query parameter to the request.
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

    /// Adds a header to the request.
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

    /// Sets the body of the request.
    /// <param name="body">The request body to set.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the specified body.</returns>
    let withBody body request = { request with Body = body }

    /// Adds metadata to the request.
    /// <param name="key">The metadata key.</param>
    /// <param name="value">The metadata value.</param>
    /// <param name="request">The request to modify.</param>
    /// <returns>A new request with the metadata entry added or replaced.</returns>
    let withMetadata key value request =
        { request with
            Metadata = Map.add key value request.Metadata
        }

    /// Gets a single query parameter value.
    /// <param name="name">The query parameter name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>The first value for the parameter, or None if not found.</returns>
    let queryParam name request =
        request.QueryParams |> Map.tryFind name |> Option.bind List.tryHead

    /// Gets all values for a query parameter.
    /// <param name="name">The query parameter name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>All values for the parameter, or an empty list if not found.</returns>
    let queryParams name request =
        request.QueryParams |> Map.tryFind name |> Option.defaultValue []

    /// Gets a single header value.
    /// <param name="name">The header name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>The first value for the header, or None if not found.</returns>
    let header name request =
        request.Headers |> Map.tryFind name |> Option.bind List.tryHead

    /// Gets all values for a header.
    /// <param name="name">The header name to look up.</param>
    /// <param name="request">The request to search.</param>
    /// <returns>All values for the header, or an empty list if not found.</returns>
    let headers name request =
        request.Headers |> Map.tryFind name |> Option.defaultValue []

    /// Gets typed metadata from the request. Returns None if the key is not found or the value cannot be cast.
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

/// Represents an HTTP response.
type HttpResponse =
    {
        Status: HttpStatusCode
        Headers: Map<string, string list>
        Body: ResponseBody
    }

/// Functions for working with HTTP responses.
module HttpResponse =

    /// Validates that a header name conforms to RFC 7230.
    /// Header names must consist of visible ASCII characters except delimiters.
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

    /// Creates a new HTTP response with the specified status code.
    /// <param name="status">The HTTP status code.</param>
    /// <returns>A new HttpResponse with empty headers and no body.</returns>
    let create status =
        { Status = status; Headers = Map.empty; Body = Empty }

    /// Adds a header to the response.
    /// Throws ArgumentException if the header name is invalid according to RFC 7230.
    /// <param name="name">The header name (must conform to RFC 7230).</param>
    /// <param name="value">The header value.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the header appended.</returns>
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

    /// Sets the body of the response.
    /// <param name="body">The response body to set.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the specified body.</returns>
    let withBody (body: ResponseBody) (response: HttpResponse) : HttpResponse = { response with Body = body }

    /// Sets the status code of the response.
    /// <param name="status">The HTTP status code to set.</param>
    /// <param name="response">The response to modify.</param>
    /// <returns>A new response with the specified status code.</returns>
    let withStatus status response = { response with Status = status }

    /// Gets a single header value from the response.
    /// <param name="name">The header name to look up.</param>
    /// <param name="response">The response to search.</param>
    /// <returns>The first value for the header, or None if not found.</returns>
    let header name response =
        response.Headers |> Map.tryFind name |> Option.bind List.tryHead

    /// Gets all values for a header from the response.
    /// <param name="name">The header name to look up.</param>
    /// <param name="response">The response to search.</param>
    /// <returns>All values for the header, or an empty list if not found.</returns>
    let headers name response =
        response.Headers |> Map.tryFind name |> Option.defaultValue []
