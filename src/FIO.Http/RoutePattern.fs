namespace FIO.Http

open System

/// A pattern describing how a request path is matched.
type RoutePath =
    /// Matches a path equal to these segments exactly.
    | Exact of string list
    /// Matches a path beginning with these segments.
    | Prefix of string list
    /// Matches a path with a custom function that also extracts parameters.
    | Pattern of (string list -> (obj list * string list) option)

[<RequireQualifiedAccess>]
module RoutePath =

    /// Creates a path that matches the given segments exactly.
    let exact (segments: string list) =
        Exact segments

    /// Creates a path that matches any request beginning with the given segments.
    let prefix (segments: string list) =
        Prefix segments

    /// Parses a path string into a route path, treating `:name` segments as parameters.
    let parse (path: string) =
        let segments =
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

        if segments |> List.exists (fun (segment: string) -> segment.StartsWith ":") then
            Pattern <| fun requestSegments ->
                let rec matchSegments
                    (patternSegments: string list)
                    (urlSegments: string list)
                    (captured: obj list) =
                    match patternSegments, urlSegments with
                    | [], [] ->
                        Some(List.rev captured, [])
                    | patternSegment :: patternRest, urlSegment :: urlRest when patternSegment.StartsWith ":" ->
                        matchSegments patternRest urlRest (box urlSegment :: captured)
                    | patternSegment :: patternRest, urlSegment :: urlRest when patternSegment = urlSegment ->
                        matchSegments patternRest urlRest captured
                    | _ ->
                        None
                matchSegments segments requestSegments []
        else
            Exact segments

    /// Parses a path string into a route path.
    let fromString (path: string) =
        parse path

    /// Tries to match segments against a route path, returning captured parameters and any remainder.
    let tryMatch (routePath: RoutePath) (segments: string list) =
        match routePath with
        | Exact expected -> if segments = expected then Some([], []) else None
        | Prefix expected ->
            let rec matchPrefix exp seg =
                match exp, seg with
                | [], remaining ->
                    Some([], remaining)
                | e :: et, s :: st when e = s ->
                    matchPrefix et st
                | _ ->
                    None
            matchPrefix expected segments
        | Pattern matcher ->
            matcher segments

    /// Creates a pattern matching an integer parameter between the given before and after segments.
    let withInt (before: string list) (after: string list) =
        Pattern <| fun segments ->
            let rec matchBefore b s =
                match b, s with
                | [], remaining ->
                    Some remaining
                | bh :: bt, sh :: st when bh = sh ->
                    matchBefore bt st
                | _ ->
                    None

            let rec matchAfter a s acc =
                match a, s with
                | [], remaining ->
                    Some(List.rev acc, remaining)
                | ah :: at, sh :: st when ah = sh ->
                    matchAfter at st acc
                | _ ->
                    None

            match matchBefore before segments with
            | Some(param :: remaining) ->
                match Int32.TryParse param with
                | true, value ->
                    matchAfter after remaining [ box value ]
                | false, _ ->
                    None
            | _ ->
                None

    /// Creates a pattern matching a string parameter between the given before and after segments.
    let withString (before: string list) (after: string list) =
        Pattern <| fun segments ->
            let rec matchBefore b s =
                match b, s with
                | [], remaining ->
                    Some remaining
                | bh :: bt, sh :: st when bh = sh ->
                    matchBefore bt st
                | _ ->
                    None

            let rec matchAfter a s acc =
                match a, s with
                | [], remaining ->
                    Some(List.rev acc, remaining)
                | ah :: at, sh :: st when ah = sh ->
                    matchAfter at st acc
                | _ ->
                    None

            match matchBefore before segments with
            | Some(param :: remaining) ->
                matchAfter after remaining [ box param ]
            | _ ->
                None

/// A route pattern: a method plus a path matcher that extracts parameters.
type RoutePattern =
    {
        /// The HTTP method this pattern matches.
        Method: HttpMethod
        /// The path this pattern matches.
        Path: RoutePath
        /// Extracts captured parameters from a request's path segments, if it matches.
        ParamExtractor: string list -> obj list option
    }

[<RequireQualifiedAccess>]
module RoutePattern =

    /// Creates a route pattern for the given method and path.
    let create (method: HttpMethod) (path: RoutePath) =
        {
            Method = method
            Path = path
            ParamExtractor =
                fun segments ->
                    match RoutePath.tryMatch path segments with
                    | Some(parameters, remaining) ->
                        match path with
                        | Prefix _ ->
                            Some parameters
                        | _ ->
                            if List.isEmpty remaining then Some parameters
                            else None
                    | None -> None
        }

    /// Creates a GET route pattern for the given path.
    let get (path: RoutePath) =
        create HttpMethod.GET path

    /// Creates a POST route pattern for the given path.
    let post (path: RoutePath) =
        create HttpMethod.POST path

    /// Creates a PUT route pattern for the given path.
    let put (path: RoutePath) =
        create HttpMethod.PUT path

    /// Creates a DELETE route pattern for the given path.
    let delete (path: RoutePath) =
        create HttpMethod.DELETE path

    /// Creates a PATCH route pattern for the given path.
    let patch (path: RoutePath) =
        create HttpMethod.PATCH path

    /// Creates a HEAD route pattern for the given path.
    let head (path: RoutePath) =
        create HttpMethod.HEAD path

    /// Creates an OPTIONS route pattern for the given path.
    let options (path: RoutePath) =
        create HttpMethod.OPTIONS path

    /// Tries to match a request against a pattern, returning captured parameters.
    let tryMatch (pattern: RoutePattern) (request: HttpRequest) =
        if request.Method = pattern.Method then
            pattern.ParamExtractor request.PathSegments
        else
            None

[<RequireQualifiedAccess>]
module Route =

    /// Parses a `METHOD /path` route string into a route pattern.
    let fromString (routeStr: string) =
        let parts = routeStr.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [| methodStr; pathStr |] ->
            let method = HttpMethod.fromString methodStr
            RoutePattern.create method (RoutePath.parse pathStr)
        | _ ->
            let errorMessage =
                $"Invalid route string format: '{routeStr}'. Expected format: 'METHOD /path' (e.g., 'GET /users' or 'POST /users/:id')"
            invalidArg "routeStr" errorMessage

    /// Creates a GET route pattern from a path string.
    let get (path: string) =
        RoutePattern.get (RoutePath.fromString path)

    /// Creates a POST route pattern from a path string.
    let post (path: string) =
        RoutePattern.post (RoutePath.fromString path)

    /// Creates a PUT route pattern from a path string.
    let put (path: string) =
        RoutePattern.put (RoutePath.fromString path)

    /// Creates a DELETE route pattern from a path string.
    let delete (path: string) =
        RoutePattern.delete (RoutePath.fromString path)

    /// Creates a PATCH route pattern from a path string.
    let patch (path: string) =
        RoutePattern.patch (RoutePath.fromString path)

    /// Creates a HEAD route pattern from a path string.
    let head (path: string) =
        RoutePattern.head (RoutePath.fromString path)

    /// Creates an OPTIONS route pattern from a path string.
    let options (path: string) =
        RoutePattern.options (RoutePath.fromString path)

module RouteOperators =

    /// Creates a route pattern from a method and a path string.
    let (=>) (method: HttpMethod) (path: string) =
        RoutePattern.create method (RoutePath.fromString path)

    /// Creates a GET route pattern from a path string.
    let get path =
        Route.get path

    /// Creates a POST route pattern from a path string.
    let post path =
        Route.post path

    /// Creates a PUT route pattern from a path string.
    let put path =
        Route.put path

    /// Creates a DELETE route pattern from a path string.
    let delete path =
        Route.delete path

    /// Creates a PATCH route pattern from a path string.
    let patch path =
        Route.patch path

    /// Creates a HEAD route pattern from a path string.
    let head path =
        Route.head path

    /// Creates an OPTIONS route pattern from a path string.
    let options path =
        Route.options path
