namespace FIO.Http

open FIO.DSL

/// Represents a single route with pattern and handler.
/// Note: Parameters are type-erased to obj list. Use pattern matching to safely cast them.
type Route<'E> =
    {
        /// The route pattern to match.
        Pattern: RoutePattern
        /// The handler function receiving extracted parameters as obj list.
        Handler: obj list -> HttpHandler<'E>
    }

/// Represents a collection of routes with a fallback handler.
/// Internally maintains an index for O(1) exact route lookups.
type Routes<'E> =
    private
        {
            /// The list of routes.
            RouteList: Route<'E> list
            /// The handler for unmatched requests.
            NotFoundHandler: HttpHandler<'E>
            /// Index for exact path matches (Method * Path -> Handler).
            ExactMatchIndex: Map<HttpMethod * string, obj list -> HttpHandler<'E>>
            /// Routes with parameters that require pattern matching.
            ParameterizedRoutes: Route<'E> list
        }

/// Functions for creating and composing route collections.
module Routes =

    /// Helper to determine if a route pattern is an exact match (no parameters).
    let private isExactMatch (pattern: RoutePattern) : bool =
        match pattern.Path with
        | Exact _ -> true
        | _ -> false

    /// Helper to extract the exact path from a route pattern.
    let private getExactPath (pattern: RoutePattern) : string option =
        match pattern.Path with
        | Exact segments -> Some("/" + String.concat "/" segments)
        | _ -> None

    /// Builds the index and parameterized route lists from a route list.
    let private buildIndex (routes: Route<'E> list) =
        let exactMatches, parameterized =
            routes |> List.partition (fun r -> isExactMatch r.Pattern)

        let index =
            exactMatches
            |> List.choose (fun route ->
                match getExactPath route.Pattern with
                | Some path -> Some((route.Pattern.Method, path), route.Handler)
                | None -> None)
            |> Map.ofList

        index, parameterized

    /// Creates an empty route collection.
    /// <returns>An empty route collection with default not-found handler.</returns>
    let empty<'E> : Routes<'E> =
        {
            RouteList = []
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = Map.empty
            ParameterizedRoutes = []
        }

    /// Creates a route collection with a single parameterized route.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <returns>A route collection containing the single route.</returns>
    let single (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        let route = { Pattern = pattern; Handler = handler }
        let index, parameterized = buildIndex [ route ]

        {
            RouteList = [ route ]
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Creates a route collection with a single route.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection containing the single route.</returns>
    let route (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> = single pattern (fun _ -> handler)

    /// Combines two route collections.
    /// <param name="routes1">The first route collection.</param>
    /// <param name="routes2">The second route collection.</param>
    /// <returns>A combined route collection.</returns>
    let combine (routes1: Routes<'E>) (routes2: Routes<'E>) : Routes<'E> =
        let combinedRoutes = routes1.RouteList @ routes2.RouteList
        let index, parameterized = buildIndex combinedRoutes

        {
            RouteList = combinedRoutes
            NotFoundHandler = routes1.NotFoundHandler
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Sets the not-found handler for a route collection.
    /// <param name="handler">The handler for unmatched requests.</param>
    /// <param name="routes">The route collection to update.</param>
    /// <returns>The route collection with the updated not-found handler.</returns>
    let withNotFound (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        { routes with NotFoundHandler = handler }

    /// Dispatches a request to the matching route.
    /// Uses O(1) hash map lookup for exact matches, falls back to O(n) for parameterized routes.
    /// <param name="request">The incoming HTTP request.</param>
    /// <param name="routes">The route collection to dispatch against.</param>
    /// <returns>An effect producing the HTTP response from the matched handler.</returns>
    let dispatch (request: HttpRequest) (routes: Routes<'E>) : FIO<HttpResponse, 'E> =
        let normalizedPath = "/" + String.concat "/" request.PathSegments
        let key = request.Method, normalizedPath

        match Map.tryFind key routes.ExactMatchIndex with
        | Some handler -> handler [] request
        | None ->
            let rec tryRoutes routeList =
                match routeList with
                | [] -> routes.NotFoundHandler request
                | route :: rest ->
                    match RoutePattern.tryMatch route.Pattern request with
                    | Some parameters -> route.Handler parameters request
                    | None -> tryRoutes rest

            tryRoutes routes.ParameterizedRoutes

    /// Adds a parameterized route to a route collection.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <param name="routes">The route collection to add to.</param>
    /// <returns>The route collection with the new route added.</returns>
    let add (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        let newRoute = { Pattern = pattern; Handler = handler }
        let updatedRoutes = routes.RouteList @ [ newRoute ]
        let index, parameterized = buildIndex updatedRoutes

        { routes with
            RouteList = updatedRoutes
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Adds a route to a route collection.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <param name="routes">The route collection to add to.</param>
    /// <returns>The route collection with the new route added.</returns>
    let addRoute (pattern: RoutePattern) (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        add pattern (fun _ -> handler) routes

    /// Creates a route collection from a list of pattern-handler pairs.
    /// <param name="routes">The list of route pattern and handler pairs.</param>
    /// <returns>A route collection containing all provided routes.</returns>
    let ofList (routes: (RoutePattern * HttpHandler<'E>) list) : Routes<'E> =
        let routeList =
            routes
            |> List.map (fun (pattern, handler) -> { Pattern = pattern; Handler = fun _ -> handler })

        let index, parameterized = buildIndex routeList

        {
            RouteList = routeList
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Transforms all handlers in a route collection.
    /// <param name="f">The function to apply to each handler.</param>
    /// <param name="routes">The route collection to transform.</param>
    /// <returns>A route collection with transformed handlers.</returns>
    let map (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        let transformedRoutes =
            routes.RouteList
            |> List.map (fun route ->
                { route with
                    Handler = fun parameters -> f (route.Handler parameters)
                })

        let index, parameterized = buildIndex transformedRoutes

        {
            RouteList = transformedRoutes
            NotFoundHandler = f routes.NotFoundHandler
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Transforms all handlers in a route collection.
    /// <param name="f">The function to apply to each handler.</param>
    /// <param name="routes">The route collection to transform.</param>
    /// <returns>A route collection with transformed handlers.</returns>
    let transform (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> = map f routes

/// Computation expression builder for routes.
module RouteBuilder =

    /// Computation expression builder for collecting routes.
    type RouteCollector<'E>() =

        /// Yields an empty route set for the computation expression.
        /// <param name="_">Unused yield value.</param>
        /// <returns>An empty route collection.</returns>
        member _.Yield _ = Routes.empty<'E>

        /// Combines two route sets into one.
        /// <param name="routes1">The first route collection.</param>
        /// <param name="routes2">The second route collection.</param>
        /// <returns>A combined route collection.</returns>
        member _.Combine(routes1: Routes<'E>, routes2: Routes<'E>) = Routes.combine routes1 routes2

        /// Delays evaluation of a route expression.
        /// <param name="f">The function to evaluate.</param>
        /// <returns>The evaluated route collection.</returns>
        member _.Delay(f: unit -> Routes<'E>) = f ()

        /// Returns an empty route set for zero case.
        /// <returns>An empty route collection.</returns>
        member _.Zero() = Routes.empty<'E>

    /// Routes computation expression builder.
    /// <returns>A RouteCollector computation expression builder instance.</returns>
    let routes<'E> = RouteCollector<'E>()

/// Operators for route composition.
module RoutesOperators =

    /// Combines two route collections.
    /// <param name="routes1">The first route collection.</param>
    /// <param name="routes2">The second route collection.</param>
    /// <returns>A combined route collection.</returns>
    let (++) (routes1: Routes<'E>) (routes2: Routes<'E>) : Routes<'E> = Routes.combine routes1 routes2

    /// Creates a route from pattern and handler.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection containing the single route.</returns>
    let (=>) (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route pattern handler

    /// Creates a parameterized route from pattern and handler.
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <returns>A route collection containing the single parameterized route.</returns>
    let (==>) (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        Routes.single pattern handler

/// Type-safe route creation functions.
module TypedRoutes =

    /// Creates a GET route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the GET route.</returns>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.get path) handler

    /// Creates a POST route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the POST route.</returns>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.post path) handler

    /// Creates a PUT route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PUT route.</returns>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.put path) handler

    /// Creates a DELETE route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the DELETE route.</returns>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.delete path) handler

    /// Creates a PATCH route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PATCH route.</returns>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.patch path) handler

    /// Creates a GET route with an integer parameter.
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    /// <returns>A route collection with the parameterized GET route.</returns>
    let getInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.get (RoutePath.withInt before after)

        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// Creates a GET route with a string parameter.
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the string parameter.</param>
    /// <returns>A route collection with the parameterized GET route.</returns>
    let getString (before: string list) (after: string list) (handler: string -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.get (RoutePath.withString before after)

        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? string as value ] -> handler value
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// Creates a POST route with an integer parameter.
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    /// <returns>A route collection with the parameterized POST route.</returns>
    let postInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.post (RoutePath.withInt before after)

        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// Creates a PUT route with an integer parameter.
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    /// <returns>A route collection with the parameterized PUT route.</returns>
    let putInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.put (RoutePath.withInt before after)

        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

    /// Creates a DELETE route with an integer parameter.
    /// <param name="before">Path segments before the parameter.</param>
    /// <param name="after">Path segments after the parameter.</param>
    /// <param name="handler">The handler receiving the integer parameter.</param>
    /// <returns>A route collection with the parameterized DELETE route.</returns>
    let deleteInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) : Routes<'E> =
        let pattern = RoutePattern.delete (RoutePath.withInt before after)

        Routes.single pattern (fun parameters ->
            match parameters with
            | [ :? int as id ] -> handler id
            | _ -> HttpHandler.badRequestText "Invalid parameters")

/// Simple route creation functions auto-opened for convenience.
[<AutoOpen>]
module SimpleRoutes =

    /// Creates a GET route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the GET route.</returns>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.get path handler

    /// Creates a POST route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the POST route.</returns>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.post path handler

    /// Creates a PUT route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PUT route.</returns>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.put path handler

    /// Creates a DELETE route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the DELETE route.</returns>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.delete path handler

    /// Creates a PATCH route.
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PATCH route.</returns>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.patch path handler
