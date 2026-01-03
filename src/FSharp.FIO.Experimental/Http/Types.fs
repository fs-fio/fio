/// <summary>
/// Core HTTP types for the FIO HTTP server.
/// </summary>
namespace FSharp.FIO.Experimental.Http

open System
open System.IO
open System.Text

/// <summary>
/// HTTP request methods.
/// </summary>
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
    /// <summary>
    /// Custom HTTP method.
    /// </summary>
    | Custom of string

    /// <summary>
    /// Returns the string representation of the HTTP method.
    /// </summary>
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

    /// <summary>
    /// Parses an HTTP method from a string.
    /// </summary>
    /// <param name="string">The HTTP method string.</param>
    static member FromString(string: string) =
        match string.ToUpperInvariant() with
        | "GET" -> GET
        | "POST" -> POST
        | "PUT" -> PUT
        | "DELETE" -> DELETE
        | "PATCH" -> PATCH
        | "HEAD" -> HEAD
        | "OPTIONS" -> OPTIONS
        | "TRACE" -> TRACE
        | "CONNECT" -> CONNECT
        | string -> Custom string

/// <summary>
/// HTTP status codes.
/// </summary>
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

/// <summary>
/// Represents the body of an HTTP request.
/// Request bodies are automatically buffered at the Kestrel boundary.
/// </summary>
type RequestBody =
    | Empty
    | Bytes of byte[]
    | Text of string

    /// <summary>
    /// Converts the request body to a byte array.
    /// </summary>
    member this.AsBytes() =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    /// <summary>
    /// Converts the request body to a string.
    /// </summary>
    member this.AsString() =
        match this with
        | Empty -> ""
        | Text text -> text
        | Bytes bytes -> Encoding.UTF8.GetString bytes

/// <summary>
/// Represents the body of an HTTP response.
/// </summary>
type ResponseBody =
    | Empty
    | Stream of stream: Stream * length: int64 option
    | Bytes of byte[]
    | Text of string
    | Json of obj

    /// <summary>
    /// Gets the content length of the response body.
    /// </summary>
    member this.ContentLength =
        match this with
        | Empty -> Some 0L
        | Bytes bytes -> Some (int64 bytes.Length)
        | Text text -> Some (int64 (Encoding.UTF8.GetByteCount text))
        | Stream (_, length) -> length
        | Json _ -> None // Will be determined after serialization

/// <summary>
/// Represents an HTTP request.
/// </summary>
type HttpRequest = {
    Method: HttpMethod
    Path: string
    PathSegments: string list
    QueryParams: Map<string, string list>
    Headers: Map<string, string list>
    Body: RequestBody
    Metadata: Map<string, obj>
}

/// <summary>
/// Functions for working with HTTP requests.
/// </summary>
module HttpRequest =

    /// <summary>
    /// Creates a new HTTP request.
    /// </summary>
    /// <param name="method">The HTTP method.</param>
    /// <param name="path">The request path.</param>
    let create method (path: string) =
        {
            Method = method
            Path = path
            PathSegments =
                path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
            QueryParams = Map.empty
            Headers = Map.empty
            Body = RequestBody.Empty
            Metadata = Map.empty
        }

    /// <summary>
    /// Adds a query parameter to the request.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="value">The parameter value.</param>
    /// <param name="request">The request to modify.</param>
    let withQueryParam name value request =
        let values =
            request.QueryParams
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [value]

        { request with QueryParams = Map.add name values request.QueryParams }

    /// <summary>
    /// Adds a header to the request.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="value">The header value.</param>
    /// <param name="request">The request to modify.</param>
    let withHeader name value request =
        let values =
            request.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [value]

        { request with Headers = Map.add name values request.Headers }

    /// <summary>
    /// Sets the body of the request.
    /// </summary>
    /// <param name="body">The request body.</param>
    /// <param name="request">The request to modify.</param>
    let withBody body request =
        { request with Body = body }

    /// <summary>
    /// Adds metadata to the request.
    /// </summary>
    /// <param name="key">The metadata key.</param>
    /// <param name="value">The metadata value.</param>
    /// <param name="request">The request to modify.</param>
    let withMetadata key value request =
        { request with Metadata = Map.add key value request.Metadata }

    /// <summary>
    /// Gets a single query parameter value.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="request">The request to query.</param>
    let queryParam name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.bind List.tryHead

    /// <summary>
    /// Gets all values for a query parameter.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="request">The request to query.</param>
    let queryParams name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.defaultValue []

    /// <summary>
    /// Gets a single header value.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="request">The request to query.</param>
    let header name request =
        request.Headers
        |> Map.tryFind name
        |> Option.bind List.tryHead

    /// <summary>
    /// Gets all values for a header.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="request">The request to query.</param>
    let headers name request =
        request.Headers
        |> Map.tryFind name
        |> Option.defaultValue []

    /// <summary>
    /// Gets typed metadata from the request.
    /// Returns None if the key is not found or the value cannot be cast to type 'T.
    /// </summary>
    /// <param name="key">The metadata key.</param>
    /// <param name="request">The request to query.</param>
    let metadata<'T> key request =
        request.Metadata
        |> Map.tryFind key
        |> Option.bind (fun o ->
            match o with
            | :? 'T as t -> Some t
            | _ -> None)

/// <summary>
/// Represents an HTTP response.
/// </summary>
type HttpResponse = {
    Status: HttpStatusCode
    Headers: Map<string, string list>
    Body: ResponseBody
}

/// <summary>
/// Functions for working with HTTP responses.
/// </summary>
module HttpResponse =

    /// <summary>
    /// Validates that a header name conforms to RFC 7230.
    /// Header names must consist of visible ASCII characters except delimiters.
    /// </summary>
    /// <param name="name">The header name to validate.</param>
    let private isValidHeaderName (name: string) =
        if String.IsNullOrWhiteSpace name then
            false
        else
            // RFC 7230: token = 1*tchar
            // tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
            //         "0"-"9" / "A"-"Z" / "^" / "_" / "`" / "a"-"z" / "|" / "~"
            name |> Seq.forall (fun c ->
                c >= 'a' && c <= 'z' || (c >= 'A' && c <= 'Z') || c >= '0' && c <= '9' ||
                c = '!' || c = '#' || c = '$' || c = '%' || c = '&' || c = '\'' || c = '*' ||
                c = '+' || c = '-' || c = '.' || c = '^' || c = '_' || c = '`' || c = '|' || c = '~')

    /// <summary>
    /// Creates a new HTTP response with the specified status code.
    /// </summary>
    /// <param name="status">The HTTP status code.</param>
    let create status =
        {
            Status = status
            Headers = Map.empty
            Body = Empty
        }

    /// <summary>
    /// Adds a header to the response.
    /// Throws ArgumentException if the header name is invalid according to RFC 7230.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="value">The header value.</param>
    /// <param name="response">The response to modify.</param>
    let withHeader name value response =
        if not (isValidHeaderName name) then
            invalidArg "name" (sprintf "Invalid HTTP header name: '%s'. Header names must consist of visible ASCII characters (RFC 7230)." name)

        let values =
            response.Headers
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [value]

        { response with Headers = Map.add name values response.Headers }

    /// <summary>
    /// Sets the body of the response.
    /// </summary>
    /// <param name="body">The response body.</param>
    /// <param name="response">The response to modify.</param>
    let withBody (body: ResponseBody) (response: HttpResponse) : HttpResponse =
        { response with Body = body }

    /// <summary>
    /// Sets the status code of the response.
    /// </summary>
    /// <param name="status">The HTTP status code.</param>
    /// <param name="response">The response to modify.</param>
    let withStatus status response =
        { response with Status = status }

    /// <summary>
    /// Gets a single header value from the response.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="response">The response to query.</param>
    let header name response =
        response.Headers
        |> Map.tryFind name
        |> Option.bind List.tryHead

    /// <summary>
    /// Gets all values for a header from the response.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="response">The response to query.</param>
    let headers name response =
        response.Headers
        |> Map.tryFind name
        |> Option.defaultValue []
