/// <summary>
/// Route pattern types for HTTP request matching.
/// </summary>
namespace FSharp.FIO.Experimental.Http

open System

/// <summary>
/// Represents an HTTP route pattern combining method and path matching.
/// </summary>
type RoutePattern =
    {
        /// <summary>The HTTP method to match.</summary>
        Method: HttpMethod
        /// <summary>The path pattern to match.</summary>
        Path: RoutePath
        /// <summary>Function to extract parameters from matched path segments.</summary>
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
    let create (method: HttpMethod) (path: RoutePath) : RoutePattern =
        {
            Method = method
            Path = path
            ParamExtractor = fun segments ->
                match RoutePath.tryMatch path segments with
                | Some (parameters, _) -> Some parameters
                | None -> None
        }

    /// <summary>
    /// Creates a GET route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let get (path: RoutePath) : RoutePattern =
        create HttpMethod.GET path

    /// <summary>
    /// Creates a POST route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let post (path: RoutePath) : RoutePattern =
        create HttpMethod.POST path

    /// <summary>
    /// Creates a PUT route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let put (path: RoutePath) : RoutePattern =
        create HttpMethod.PUT path

    /// <summary>
    /// Creates a DELETE route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let delete (path: RoutePath) : RoutePattern =
        create HttpMethod.DELETE path

    /// <summary>
    /// Creates a PATCH route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let patch (path: RoutePath) : RoutePattern =
        create HttpMethod.PATCH path

    /// <summary>
    /// Creates a HEAD route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let head (path: RoutePath) : RoutePattern =
        create HttpMethod.HEAD path

    /// <summary>
    /// Creates an OPTIONS route pattern.
    /// </summary>
    /// <param name="path">The route path pattern.</param>
    let options (path: RoutePath) : RoutePattern =
        create HttpMethod.OPTIONS path

    /// <summary>
    /// Attempts to match a route pattern against an HTTP request.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="request">The HTTP request to match.</param>
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
    let ofString (routeStr: string) : RoutePattern =
        let parts = routeStr.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [| methodStr; pathStr |] ->
            let method = HttpMethod.FromString methodStr
            let segments =
                pathStr.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList

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
            let errorMsg = sprintf "Invalid route string format: '%s'. Expected format: 'METHOD /path' (e.g., 'GET /users' or 'POST /users/:id')" routeStr
            invalidArg "routeStr" errorMsg

    /// <summary>
    /// Creates a GET route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let get (path: string) : RoutePattern =
        RoutePattern.get (RoutePath.ofString path)

    /// <summary>
    /// Creates a POST route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let post (path: string) : RoutePattern =
        RoutePattern.post (RoutePath.ofString path)

    /// <summary>
    /// Creates a PUT route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let put (path: string) : RoutePattern =
        RoutePattern.put (RoutePath.ofString path)

    /// <summary>
    /// Creates a DELETE route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let delete (path: string) : RoutePattern =
        RoutePattern.delete (RoutePath.ofString path)

    /// <summary>
    /// Creates a PATCH route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let patch (path: string) : RoutePattern =
        RoutePattern.patch (RoutePath.ofString path)

    /// <summary>
    /// Creates a HEAD route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
    let head (path: string) : RoutePattern =
        RoutePattern.head (RoutePath.ofString path)

    /// <summary>
    /// Creates an OPTIONS route pattern from a path string.
    /// </summary>
    /// <param name="path">The path string.</param>
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
    let (=>) (method: HttpMethod) (path: string) : RoutePattern =
        RoutePattern.create method (RoutePath.ofString path)

    /// <summary>
    /// Creates a GET route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let GET path = Route.get path

    /// <summary>
    /// Creates a POST route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let POST path = Route.post path

    /// <summary>
    /// Creates a PUT route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let PUT path = Route.put path

    /// <summary>
    /// Creates a DELETE route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let DELETE path = Route.delete path

    /// <summary>
    /// Creates a PATCH route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let PATCH path = Route.patch path

    /// <summary>
    /// Creates a HEAD route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let HEAD path = Route.head path

    /// <summary>
    /// Creates an OPTIONS route pattern.
    /// </summary>
    /// <param name="path">The path string.</param>
    let OPTIONS path = Route.options path
