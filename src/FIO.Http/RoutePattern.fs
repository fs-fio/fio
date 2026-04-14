namespace FIO.Http

open System

/// <summary>
/// Represents a route path pattern for URL matching.
/// </summary>
type RoutePath =
    /// <summary>
    /// Exact path match.
    /// </summary>
    | Exact of string list
    /// <summary>
    /// Prefix path match.
    /// </summary>
    | Prefix of string list
    /// <summary>
    /// Custom pattern matcher.
    /// </summary>
    | Pattern of (string list -> (obj list * string list) option)

/// <summary>
/// Functions for creating and matching route paths.
/// </summary>
module RoutePath =
    /// <summary>
    /// Creates an exact path match.
    /// </summary>
    /// <param name="segments">The path segments to match exactly.</param>
    /// <returns>The exact route path.</returns>
    let exact (segments: string list) : RoutePath = Exact segments

    /// <summary>
    /// Creates a prefix path match.
    /// </summary>
    /// <param name="segments">The path segments to match as prefix.</param>
    /// <returns>The prefix route path.</returns>
    let prefix (segments: string list) : RoutePath = Prefix segments

    /// <summary>
    /// Parses a path string into a route path.
    /// </summary>
    /// <param name="path">The path string to parse.</param>
    /// <returns>The parsed route path.</returns>
    let ofString (path: string) : RoutePath =
        let segments =
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

        Exact segments

    /// <summary>
    /// Attempts to match a route path against URL segments.
    /// </summary>
    /// <param name="routePath">The route path pattern.</param>
    /// <param name="segments">The URL segments to match.</param>
    /// <returns>Matched parameters and remaining segments, or None.</returns>
    let tryMatch (routePath: RoutePath) (segments: string list) : (obj list * string list) option =
        match routePath with
        | Exact expected -> if segments = expected then Some([], []) else None
        | Prefix expected ->
            let rec matchPrefix exp seg =
                match exp, seg with
                | [], remaining -> Some([], remaining)
                | e :: et, s :: st when e = s -> matchPrefix et st
                | _ -> None

            matchPrefix expected segments
        | Pattern matcher -> matcher segments

    /// <summary>
    /// Creates a route path with an integer parameter.
    /// </summary>
    /// <param name="before">The path segments before the parameter.</param>
    /// <param name="after">The path segments after the parameter.</param>
    /// <returns>The parameterized route path.</returns>
    let withInt (before: string list) (after: string list) : RoutePath =
        Pattern(fun segments ->
            let rec matchBefore b s =
                match b, s with
                | [], remaining -> Some remaining
                | bh :: bt, sh :: st when bh = sh -> matchBefore bt st
                | _ -> None

            let rec matchAfter a s acc =
                match a, s with
                | [], remaining -> Some(List.rev acc, remaining)
                | ah :: at, sh :: st when ah = sh -> matchAfter at st acc
                | _ -> None

            match matchBefore before segments with
            | Some(param :: remaining) ->
                match Int32.TryParse param with
                | true, value -> matchAfter after remaining [ box value ]
                | false, _ -> None
            | _ -> None)

    /// <summary>
    /// Creates a route path with a string parameter.
    /// </summary>
    /// <param name="before">The path segments before the parameter.</param>
    /// <param name="after">The path segments after the parameter.</param>
    /// <returns>The parameterized route path.</returns>
    let withString (before: string list) (after: string list) : RoutePath =
        Pattern(fun segments ->
            let rec matchBefore b s =
                match b, s with
                | [], remaining -> Some remaining
                | bh :: bt, sh :: st when bh = sh -> matchBefore bt st
                | _ -> None

            let rec matchAfter a s acc =
                match a, s with
                | [], remaining -> Some(List.rev acc, remaining)
                | ah :: at, sh :: st when ah = sh -> matchAfter at st acc
                | _ -> None

            match matchBefore before segments with
            | Some(param :: remaining) -> matchAfter after remaining [ box param ]
            | _ -> None)

/// <summary>
/// Represents an HTTP route pattern combining method and path matching.
/// </summary>
type RoutePattern =
    {
        /// <summary>
        /// The HTTP method to match.
        /// </summary>
        Method: HttpMethod
        /// <summary>
        /// The path pattern to match.
        /// </summary>
        Path: RoutePath
        /// <summary>
        /// Function to extract parameters from matched path segments.
        /// </summary>
        ParamExtractor: string list -> obj list option
    }

/// <summary>
/// Functions for creating and matching route patterns.
/// </summary>
module RoutePattern =

    /// <summary>
    /// Creates a route pattern from method and path.
    /// </summary>
    /// <param name="method">The HTTP method.</param>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let create (method: HttpMethod) (path: RoutePath) : RoutePattern =
        {
            Method = method
            Path = path
            ParamExtractor =
                fun segments ->
                    match RoutePath.tryMatch path segments with
                    | Some(parameters, []) -> Some parameters
                    | _ -> None
        }

    /// <summary>
    /// Creates a GET route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let get (path: RoutePath) : RoutePattern = create HttpMethod.GET path

    /// <summary>
    /// Creates a POST route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let post (path: RoutePath) : RoutePattern = create HttpMethod.POST path

    /// <summary>
    /// Creates a PUT route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let put (path: RoutePath) : RoutePattern = create HttpMethod.PUT path

    /// <summary>
    /// Creates a DELETE route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let delete (path: RoutePath) : RoutePattern = create HttpMethod.DELETE path

    /// <summary>
    /// Creates a PATCH route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let patch (path: RoutePath) : RoutePattern = create HttpMethod.PATCH path

    /// <summary>
    /// Creates a HEAD route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let head (path: RoutePath) : RoutePattern = create HttpMethod.HEAD path

    /// <summary>
    /// Creates an OPTIONS route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    /// <returns>The route pattern.</returns>
    let options (path: RoutePath) : RoutePattern = create HttpMethod.OPTIONS path

    /// <summary>
    /// Attempts to match a route pattern against an HTTP request.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="request">The HTTP request to match.</param>
    /// <returns>The matched parameters, or None.</returns>
    let tryMatch (pattern: RoutePattern) (request: HttpRequest) : obj list option =
        if request.Method = pattern.Method then
            pattern.ParamExtractor request.PathSegments
        else
            None

/// <summary>
/// Functions for creating route patterns from string paths.
/// </summary>
module Route =

    /// <summary>
    /// Parses a route string into a route pattern.
    /// </summary>
    /// <param name="routeStr">The route string in format "METHOD /path" or "METHOD /path/:param".</param>
    /// <returns>The parsed route pattern.</returns>
    let ofString (routeStr: string) : RoutePattern =
        let parts = routeStr.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

        match parts with
        | [| methodStr; pathStr |] ->
            let method = HttpMethod.fromString methodStr

            let segments =
                pathStr.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

            // Build path using accumulator to correctly handle parameters
            let rec buildPath (acc: string list) (segs: string list) =
                match segs with
                | [] -> RoutePath.exact (List.rev acc)
                | seg :: rest when seg.StartsWith ":" ->
                    // Found a parameter - use withString with accumulated prefix and remaining suffix
                    let before = List.rev acc
                    let after = rest
                    RoutePath.withString before after
                | seg :: rest ->
                    // Regular segment, accumulate and continue
                    buildPath (seg :: acc) rest

            RoutePattern.create method (buildPath [] segments)
        | _ ->
            let errorMsg =
                sprintf
                    "Invalid route string format: '%s'. Expected format: 'METHOD /path' (e.g., 'GET /users' or 'POST /users/:id')"
                    routeStr

            invalidArg "routeStr" errorMsg

    /// <summary>
    /// Creates a GET route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let get (path: string) : RoutePattern =
        RoutePattern.get (RoutePath.ofString path)

    /// <summary>
    /// Creates a POST route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let post (path: string) : RoutePattern =
        RoutePattern.post (RoutePath.ofString path)

    /// <summary>
    /// Creates a PUT route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let put (path: string) : RoutePattern =
        RoutePattern.put (RoutePath.ofString path)

    /// <summary>
    /// Creates a DELETE route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let delete (path: string) : RoutePattern =
        RoutePattern.delete (RoutePath.ofString path)

    /// <summary>
    /// Creates a PATCH route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let patch (path: string) : RoutePattern =
        RoutePattern.patch (RoutePath.ofString path)

    /// <summary>
    /// Creates a HEAD route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let head (path: string) : RoutePattern =
        RoutePattern.head (RoutePath.ofString path)

    /// <summary>
    /// Creates an OPTIONS route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let options (path: string) : RoutePattern =
        RoutePattern.options (RoutePath.ofString path)

/// <summary>
/// Operators for creating route patterns.
/// </summary>
module RouteOperators =
    /// <summary>
    /// Creates a route pattern from method and path.
    /// </summary>
    /// <param name="method">The HTTP method.</param>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let (=>) (method: HttpMethod) (path: string) : RoutePattern =
        RoutePattern.create method (RoutePath.ofString path)

    /// <summary>
    /// Creates a GET route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let get path = Route.get path

    /// <summary>
    /// Creates a POST route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let post path = Route.post path

    /// <summary>
    /// Creates a PUT route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let put path = Route.put path

    /// <summary>
    /// Creates a DELETE route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let delete path = Route.delete path

    /// <summary>
    /// Creates a PATCH route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let patch path = Route.patch path

    /// <summary>
    /// Creates a HEAD route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let head path = Route.head path

    /// <summary>
    /// Creates an OPTIONS route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <returns>The route pattern.</returns>
    let options path = Route.options path
