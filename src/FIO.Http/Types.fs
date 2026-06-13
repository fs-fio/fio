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

type HttpError =
    | InvalidRoute of pattern: string
    | ParsingFailed of message: string * exn
    | HandlerFailed of path: string * exn
    | MiddlewareFailed of name: string * exn
    | ServerFailed of exn
    | BodyReadFailed of exn
    | JsonFailed of exn
    | TimeoutError of message: string
    | GeneralError of exn

    override this.ToString () =
        match this with
        | InvalidRoute pattern -> $"Invalid route: {pattern}"
        | ParsingFailed(message, exn) -> $"Parsing failed: {message} - {exn.Message}"
        | HandlerFailed(path, exn) -> $"Handler failed [{path}]: {exn.Message}"
        | MiddlewareFailed(name, exn) -> $"Middleware {name} failed: {exn.Message}"
        | ServerFailed exn -> $"Server error: {exn.Message}"
        | BodyReadFailed exn -> $"Body read failed: {exn.Message}"
        | JsonFailed exn -> $"JSON error: {exn.Message}"
        | TimeoutError message -> $"Timeout: {message}"
        | GeneralError exn -> $"HTTP error: {exn.Message}"

[<RequireQualifiedAccess>]
module HttpError =

    let fromException (exn: exn) =
        GeneralError exn

    let toException (error: HttpError) =
        match error with
        | GeneralError exn -> exn
        | _ -> Exception <| error.ToString()

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

[<RequireQualifiedAccess>]
type RequestBody =
    | Empty
    | Bytes of byte[]
    | Text of string

    member this.AsBytes () =
        match this with
        | Empty -> Array.empty
        | Bytes bytes -> bytes
        | Text text -> Encoding.UTF8.GetBytes text

    member this.AsString () =
        match this with
        | Empty -> ""
        | Text text -> text
        | Bytes bytes -> Encoding.UTF8.GetString bytes

[<RequireQualifiedAccess>]
type ResponseBody =
    | Empty
    | Stream of stream: Stream * length: int64 option
    | Bytes of byte[]
    | Text of string
    | Json of obj

    member this.ContentLength =
        match this with
        | Empty -> Some 0L
        | Bytes bytes -> Some <| int64 bytes.Length
        | Text text -> Some <| int64 (Encoding.UTF8.GetByteCount text)
        | Stream(_, length) -> length
        | Json _ -> None

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

[<RequireQualifiedAccess>]
module HttpRequest =

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

    let withQueryParam name value request =
        let values =
            request.QueryParams
            |> Map.tryFind name
            |> Option.defaultValue []
            |> fun existing -> existing @ [ value ]
        { request with
            QueryParams = Map.add name values request.QueryParams
        }

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

    let withBody body request =
        { request with Body = body }

    let withMetadata key value request =
        { request with
            Metadata = Map.add key value request.Metadata
        }

    let queryParam name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.bind List.tryHead

    let queryParams name request =
        request.QueryParams
        |> Map.tryFind name
        |> Option.defaultValue []

    let header name request =
        HeaderHelpers.tryFind name request.Headers
        |> Option.bind List.tryHead

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

    let bodyText (request: HttpRequest) =
        match request.Body with
        | RequestBody.Empty -> ""
        | RequestBody.Text text -> text
        | RequestBody.Bytes bytes ->
            let encoding = encodingFromContentType (header "Content-Type" request)
            encoding.GetString bytes

    let metadata<'A> key request =
        request.Metadata
        |> Map.tryFind key
        |> Option.bind (fun o ->
            match o with
            | :? 'A as value -> Some value
            | _ -> None)

type HttpResponse =
    {
        Status: HttpStatusCode
        Headers: Map<string, string list>
        Body: ResponseBody
    }

[<RequireQualifiedAccess>]
module HttpResponse =

    let create status =
        { Status = status; Headers = Map.empty; Body = ResponseBody.Empty }

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

    let withBody (body: ResponseBody) (response: HttpResponse) =
        { response with Body = body }

    let withStatus status response =
        { response with Status = status }

    let header name response =
        HeaderHelpers.tryFind name response.Headers
        |> Option.bind List.tryHead

    let headers name response =
        HeaderHelpers.tryFind name response.Headers
        |> Option.defaultValue []
