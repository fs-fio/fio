namespace FIO.Http

open FIO.DSL

/// <summary>
/// Represents a single route with pattern and handler.
/// Note: Parameters are type-erased to obj list. Use pattern matching to safely cast them:
/// <code>
/// Routes.single pattern (fun parameters ->
///     match parameters with
///     | [ :? int as id ] -> handler id
///     | _ -> HttpHandler.badRequest)
/// </code>
/// </summary>
type Route<'E> =
    {
        /// <summary>The route pattern to match.</summary>
        Pattern: RoutePattern
        /// <summary>The handler function receiving extracted parameters as obj list.</summary>
        Handler: obj list -> HttpHandler<'E>
    }

/// <summary>
/// Represents a collection of routes with a fallback handler.
/// Internally maintains an index for O(1) exact route lookups.
/// </summary>
type Routes<'E> =
    private
        {
            /// <summary>The list of routes.</summary>
            RouteList: Route<'E> list
            /// <summary>The handler for unmatched requests.</summary>
            NotFoundHandler: HttpHandler<'E>
            /// <summary>Index for exact path matches (Method * Path -> Handler).</summary>
            ExactMatchIndex: Map<HttpMethod * string, obj list -> HttpHandler<'E>>
            /// <summary>Routes with parameters that require pattern matching.</summary>
            ParameterizedRoutes: Route<'E> list
        }

/// <summary>
/// Functions for creating and composing route collections.
/// </summary>
module Routes =

    /// <summary>
    /// Helper to determine if a route pattern is an exact match (no parameters).
    /// </summary>
    let private isExactMatch (pattern: RoutePattern) : bool =
        match pattern.Path with
        | Exact _ -> true
        | _ -> false

    /// <summary>
    /// Helper to extract the exact path from a route pattern.
    /// </summary>
    let private getExactPath (pattern: RoutePattern) : string option =
        match pattern.Path with
        | Exact segments -> Some ("/" + String.concat "/" segments)
        | _ -> None

    /// <summary>
    /// Builds the index and parameterized route lists from a route list.
    /// </summary>
    let private buildIndex (routes: Route<'E> list) =
        let exactMatches, parameterized =
            routes |> List.partition (fun r -> isExactMatch r.Pattern)

        let index =
            exactMatches
            |> List.choose (fun route ->
                match getExactPath route.Pattern with
                | Some path -> Some ((route.Pattern.Method, path), route.Handler)
                | None -> None)
            |> Map.ofList

        index, parameterized

    /// <summary>
    /// Creates an empty route collection.
    /// </summary>
    let empty<'E> : Routes<'E> =
        {
            RouteList = []
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = Map.empty
            ParameterizedRoutes = []
        }

    /// <summary>
    /// Creates a route collection with a single parameterized route.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    let single (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        let route = { Pattern = pattern; Handler = handler }
        let index, parameterized = buildIndex [route]
        {
            RouteList = [route]
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// <summary>
    /// Creates a route collection with a single route.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The request handler.</param>
    let route (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> =
        single pattern (fun _ -> handler)

    /// <summary>
    /// Combines two route collections.
    /// </summary>
    /// <param name="routes1">The first route collection.</param>
    /// <param name="routes2">The second route collection.</param>
    let combine (routes1: Routes<'E>) (routes2: Routes<'E>) : Routes<'E> =
        let combinedRoutes = routes1.RouteList @ routes2.RouteList
        let index, parameterized = buildIndex combinedRoutes
        {
            RouteList = combinedRoutes
            NotFoundHandler = routes1.NotFoundHandler
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// <summary>
    /// Sets the not-found handler for a route collection.
    /// </summary>
    /// <param name="handler">The handler for unmatched requests.</param>
    /// <param name="routes">The route collection.</param>
    let withNotFound (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        { routes with NotFoundHandler = handler }

    /// <summary>
    /// Dispatches a request to the matching route.
    /// Uses O(1) hash map lookup for exact matches, falls back to O(n) for parameterized routes.
    /// </summary>
    /// <param name="request">The HTTP request.</param>
    /// <param name="routes">The route collection.</param>
    let dispatch (request: HttpRequest) (routes: Routes<'E>) : FIO<HttpResponse, 'E> =
        let key = request.Method, request.Path
        match Map.tryFind key routes.ExactMatchIndex with
        | Some handler ->
            handler [] request
        | None ->
            let rec tryRoutes routeList =
                match routeList with
                | [] -> routes.NotFoundHandler request
                | route :: rest ->
                    match RoutePattern.tryMatch route.Pattern request with
                    | Some parameters ->
                        route.Handler parameters request
                    | None ->
                        tryRoutes rest

            tryRoutes routes.ParameterizedRoutes

    /// <summary>
    /// Adds a parameterized route to a route collection.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <param name="routes">The route collection.</param>
    let add (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        let newRoute = { Pattern = pattern; Handler = handler }
        let updatedRoutes = routes.RouteList @ [newRoute]
        let index, parameterized = buildIndex updatedRoutes
        {
            routes with
                RouteList = updatedRoutes
                ExactMatchIndex = index
                ParameterizedRoutes = parameterized
        }

    /// <summary>
    /// Adds a route to a route collection.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The request handler.</param>
    /// <param name="routes">The route collection.</param>
    let addRoute (pattern: RoutePattern) (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        add pattern (fun _ -> handler) routes

    /// <summary>
    /// Creates a route collection from a list of pattern-handler pairs.
    /// </summary>
    /// <param name="routes">The list of pattern-handler pairs.</param>
    let ofList (routes: (RoutePattern * HttpHandler<'E>) list) : Routes<'E> =
        let routeList = routes |> List.map (fun (pattern, handler) -> { Pattern = pattern; Handler = fun _ -> handler })
        let index, parameterized = buildIndex routeList
        {
            RouteList = routeList
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// <summary>
    /// Transforms all handlers in a route collection.
    /// </summary>
    /// <param name="f">The transformation function.</param>
    /// <param name="routes">The route collection.</param>
    let map (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        let transformedRoutes =
            routes.RouteList |> List.map (fun route ->
                { route with Handler = fun parameters -> f (route.Handler parameters) })
        let index, parameterized = buildIndex transformedRoutes
        {
            RouteList = transformedRoutes
            NotFoundHandler = f routes.NotFoundHandler
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// <summary>
    /// Transforms all handlers in a route collection.
    /// </summary>
    /// <param name="f">The transformation function.</param>
    /// <param name="routes">The route collection.</param>
    let transform (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        map f routes

/// <summary>
/// Computation expression builder for routes.
/// </summary>
module RouteBuilder =

    /// <summary>
    /// Computation expression builder for collecting routes.
    /// </summary>
    type RouteCollector<'E>() =
    
        /// <summary>
        /// Yields an empty route set for the computation expression.
        /// </summary>
        member _.Yield _ = Routes.empty<'E>

        /// <summary>
        /// Combines two route sets into one.
        /// </summary>
        member _.Combine(routes1: Routes<'E>, routes2: Routes<'E>) =
            Routes.combine routes1 routes2

        /// <summary>
        /// Delays evaluation of a route expression.
        /// </summary>
        member _.Delay(f: unit -> Routes<'E>) = f()

        /// <summary>
        /// Returns an empty route set for zero case.
        /// </summary>
        member _.Zero() = Routes.empty<'E>

    /// <summary>
    /// Routes computation expression builder.
    /// </summary>
    let routes<'E> = RouteCollector<'E>()

/// <summary>
/// Operators for route composition.
/// </summary>
module RoutesOperators =
    
    /// <summary>
    /// Combines two route collections.
    /// </summary>
    /// <param name="routes1">The first route collection.</param>
    /// <param name="routes2">The second route collection.</param>
    let (++) (routes1: Routes<'E>) (routes2: Routes<'E>) : Routes<'E> =
        Routes.combine routes1 routes2

    /// <summary>
    /// Creates a route from pattern and handler.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The request handler.</param>
    let (=>) (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route pattern handler

    /// <summary>
    /// Creates a parameterized route from pattern and handler.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    let (==>) (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        Routes.single pattern handler

/// <summary>
/// Type-safe route creation functions.
/// </summary>
module TypedRoutes =

    /// <summary>
    /// Creates a GET route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.get path) handler

    /// <summary>
    /// Creates a POST route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.post path) handler

    /// <summary>
    /// Creates a PUT route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.put path) handler

    /// <summary>
    /// Creates a DELETE route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.delete path) handler

    /// <summary>
    /// Creates a PATCH route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.patch path) handler

    /// <summary>
    /// Creates a GET route with an integer parameter.
    /// </summary>
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    let getInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.get (RoutePath.withInt before after)
        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// <summary>
    /// Creates a GET route with a string parameter.
    /// </summary>
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the string parameter.</param>
    let getString (before: string list) (after: string list) (handler: string -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.get (RoutePath.withString before after)
        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? string as value ] -> handler value
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// <summary>
    /// Creates a POST route with an integer parameter.
    /// </summary>
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    let postInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.post (RoutePath.withInt before after)
        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// <summary>
    /// Creates a PUT route with an integer parameter.
    /// </summary>
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    let putInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.put (RoutePath.withInt before after)
        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// <summary>
    /// Creates a DELETE route with an integer parameter.
    /// </summary>
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    let deleteInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.delete (RoutePath.withInt before after)
        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

/// <summary>
/// Simple route creation functions auto-opened for convenience.
/// </summary>
[<AutoOpen>]
module SimpleRoutes =

    /// <summary>
    /// Creates a GET route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.get path handler

    /// <summary>
    /// Creates a POST route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.post path handler

    /// <summary>
    /// Creates a PUT route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.put path handler

    /// <summary>
    /// Creates a DELETE route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.delete path handler

    /// <summary>
    /// Creates a PATCH route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.patch path handler
