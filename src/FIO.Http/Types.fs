namespace FIO.Http

open System
open System.IO
open System.Text

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

/// An error produced by the HTTP server or a route handler.
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

    override this.ToString () =
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
    let fromException (ex: exn) =
        GeneralError ex

    /// Converts an HTTP error back into an exception.
    let toException (error: HttpError) =
        match error with
        | GeneralError ex -> ex
        | _ -> Exception <| error.ToString()

/// An HTTP request method.
[<RequireQualifiedAccess>]
type HttpMethod =
    | GET
    | POST
    | PUT
    | DELETE
    | PATCH
    | HEAD
    | OPTIONS
    | TRACE
    | CONNECT
    | Custom of string

    override this.ToString () =
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
    let fromString (str: string) =
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
    member this.AsBytes () =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    /// Returns the body as a string.
    member this.AsString () =
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
    member this.ContentLength =
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

    /// Adds a query parameter value to a request.
    let withQueryParam name value request =
        let values =
            request.QueryParams
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]
        { request with
            QueryParams = Map.add name values request.QueryParams
        }

    /// Adds a header value to a request.
    let withHeader name value request =
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
    let withBody body request =
        { request with Body = body }

    /// Attaches a metadata value to a request.
    let withMetadata key value request =
        { request with
            Metadata = Map.add key value request.Metadata
        }

    /// Returns the first value of a query parameter, if present.
    let queryParam name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.bind List.tryHead

    /// Returns all values of a query parameter.
    let queryParams name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.defaultValue []

    /// Returns the first value of a header, if present.
    let header name request =
        HeaderHelpers.tryFind name request.Headers
        |> Option.bind List.tryHead

    /// Returns all values of a header.
    let headers name request =
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
    let bodyText (request: HttpRequest) =
        match request.Body with
        | RequestBody.Empty -> ""
        | RequestBody.Text text -> text
        | RequestBody.Bytes bytes ->
            let encoding = encodingFromContentType (header "Content-Type" request)
            encoding.GetString bytes

    /// Returns a typed metadata value, if present and of the expected type.
    let metadata<'A> key request =
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
    let create status =
        { Status = status; Headers = Map.empty; Body = ResponseBody.Empty }

    /// Adds a header value to a response.
    let withHeader name value response =
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
    let withBody (body: ResponseBody) (response: HttpResponse) =
        { response with Body = body }

    /// Sets a response's status code.
    let withStatus status response =
        { response with Status = status }

    /// Returns the first value of a header, if present.
    let header name response =
        HeaderHelpers.tryFind name response.Headers
        |> Option.bind List.tryHead

    /// Returns all values of a header.
    let headers name response =
        HeaderHelpers.tryFind name response.Headers
        |> Option.defaultValue []
