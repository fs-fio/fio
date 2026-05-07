namespace FIO.Http

open FIO.DSL

/// <summary>Represents a single route with pattern and handler.</summary>
type Route<'E> =
    {
        /// <summary>Represents the route pattern to match against incoming requests.</summary>
        Pattern: RoutePattern
        /// <summary>Represents the handler function that receives extracted parameters and produces a response.</summary>
        Handler: obj list -> HttpHandler<'E>
    }

/// <summary>Represents a collection of routes with a fallback handler.</summary>
type Routes<'E> =
    private
        {
            /// <summary>Represents the ordered list of registered routes.</summary>
            RouteList: Route<'E> list
            /// <summary>Represents the handler invoked when no route matches.</summary>
            NotFoundHandler: HttpHandler<'E>
            /// <summary>Represents the index for constant-time lookup of exact-match routes.</summary>
            ExactMatchIndex: Map<HttpMethod * string, obj list -> HttpHandler<'E>>
            /// <summary>Represents the list of routes containing path parameters.</summary>
            ParameterizedRoutes: Route<'E> list
        }

/// <summary>Provides functions for creating and composing route collections.</summary>
module Routes =

    /// <summary>Returns whether the given route pattern matches only exact (non-parameterized) paths.</summary>
    /// <param name="pattern">The route pattern to inspect.</param>
    /// <returns><c>true</c> if the pattern uses an <c>Exact</c> path; <c>false</c> otherwise.</returns>
    let private isExactMatch (pattern: RoutePattern) : bool =
        match pattern.Path with
        | Exact _ -> true
        | _ -> false

    /// <summary>Returns the reconstructed path string for an exact route pattern, or <c>None</c> for parameterized patterns.</summary>
    /// <param name="pattern">The route pattern to extract the path from.</param>
    /// <returns>The slash-joined path string when the pattern is exact; <c>None</c> otherwise.</returns>
    let private getExactPath (pattern: RoutePattern) : string option =
        match pattern.Path with
        | Exact segments -> Some("/" + String.concat "/" segments)
        | _ -> None

    /// <summary>Builds a lookup index for exact-match routes and a separate list of parameterized routes.</summary>
    /// <param name="routes">The list of routes to partition and index.</param>
    /// <returns>A tuple of the exact-match index map and the parameterized route list.</returns>
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

    /// <summary>Creates an empty route collection.</summary>
    /// <returns>An empty route collection with default not-found handler.</returns>
    let empty<'E> : Routes<'E> =
        {
            RouteList = []
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = Map.empty
            ParameterizedRoutes = []
        }

    /// <summary>Creates a route collection with a single parameterized route.</summary>
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

    /// <summary>Creates a route collection with a single route.</summary>
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection containing the single route.</returns>
    let route (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> = single pattern (fun _ -> handler)

    /// <summary>Combines two route collections into one.</summary>
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

    /// <summary>Creates a route collection with the specified not-found handler.</summary>
    /// <param name="handler">The handler for unmatched requests.</param>
    /// <param name="routes">The route collection to update.</param>
    /// <returns>The route collection with the updated not-found handler.</returns>
    let withNotFound (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        { routes with NotFoundHandler = handler }

    /// <summary>Returns the response effect from the first matching route, or the not-found handler if no route matches.</summary>
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

    /// <summary>Creates a route collection with the given parameterized route appended.</summary>
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

    /// <summary>Creates a route collection with the given route appended.</summary>
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <param name="routes">The route collection to add to.</param>
    /// <returns>The route collection with the new route added.</returns>
    let addRoute (pattern: RoutePattern) (handler: HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        add pattern (fun _ -> handler) routes

    /// <summary>Creates a route collection from a list of pattern-handler pairs.</summary>
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

    /// <summary>Transforms all handlers in a route collection by applying a function to each.</summary>
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

    /// <summary>Transforms all handlers in a route collection by applying a function to each.</summary>
    /// <param name="f">The function to apply to each handler.</param>
    /// <param name="routes">The route collection to transform.</param>
    /// <returns>A route collection with transformed handlers.</returns>
    let transform (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> = map f routes

/// <summary>Provides a computation expression builder for composing routes.</summary>
module RouteBuilder =

    /// <summary>Represents a computation expression builder for composing route collections.</summary>
    type RouteCollector<'E>() =

        /// <summary>Returns an empty route collection for the computation expression.</summary>
        /// <param name="_">Unused yield value.</param>
        /// <returns>An empty route collection.</returns>
        member _.Yield _ = Routes.empty<'E>

        /// <summary>Combines two route collections into one.</summary>
        /// <param name="routes1">The first route collection.</param>
        /// <param name="routes2">The second route collection.</param>
        /// <returns>A combined route collection.</returns>
        member _.Combine(routes1: Routes<'E>, routes2: Routes<'E>) = Routes.combine routes1 routes2

        /// <summary>Returns the evaluated route collection from a delayed expression.</summary>
        /// <param name="f">The function to evaluate.</param>
        /// <returns>The evaluated route collection.</returns>
        member _.Delay(f: unit -> Routes<'E>) = f ()

        /// <summary>Returns an empty route collection for the zero case.</summary>
        /// <returns>An empty route collection.</returns>
        member _.Zero() = Routes.empty<'E>

    /// <summary>Creates a computation expression builder for composing routes.</summary>
    /// <returns>A RouteCollector computation expression builder instance.</returns>
    let routes<'E> = RouteCollector<'E>()

/// <summary>Provides operators for route composition.</summary>
module RoutesOperators =

    /// <summary>Combines two route collections into one.</summary>
    /// <param name="routes1">The first route collection.</param>
    /// <param name="routes2">The second route collection.</param>
    /// <returns>A combined route collection.</returns>
    let (++) (routes1: Routes<'E>) (routes2: Routes<'E>) : Routes<'E> = Routes.combine routes1 routes2

    /// <summary>Creates a route collection from a pattern and handler.</summary>
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection containing the single route.</returns>
    let (=>) (pattern: RoutePattern) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route pattern handler

    /// <summary>Creates a route collection from a pattern and parameterized handler.</summary>
    /// <param name="pattern">The route pattern to match.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <returns>A route collection containing the single parameterized route.</returns>
    let (==>) (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        Routes.single pattern handler

/// <summary>Provides type-safe route creation functions.</summary>
module TypedRoutes =

    /// <summary>Creates a GET route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the GET route.</returns>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.get path) handler

    /// <summary>Creates a POST route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the POST route.</returns>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.post path) handler

    /// <summary>Creates a PUT route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PUT route.</returns>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.put path) handler

    /// <summary>Creates a DELETE route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the DELETE route.</returns>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        Routes.route (Route.delete path) handler

    /// <summary>Creates a PATCH route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PATCH route.</returns>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> = Routes.route (Route.patch path) handler

    /// <summary>Creates a GET route with an integer parameter.</summary>
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

    /// <summary>Creates a GET route with a string parameter.</summary>
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

    /// <summary>Creates a POST route with an integer parameter.</summary>
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

    /// <summary>Creates a PUT route with an integer parameter.</summary>
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

    /// <summary>Creates a DELETE route with an integer parameter.</summary>
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

/// <summary>Provides auto-opened convenience functions for creating routes.</summary>
[<AutoOpen>]
module SimpleRoutes =

    /// <summary>Creates a GET route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the GET route.</returns>
    let get (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.get path handler

    /// <summary>Creates a POST route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the POST route.</returns>
    let post (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.post path handler

    /// <summary>Creates a PUT route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PUT route.</returns>
    let put (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.put path handler

    /// <summary>Creates a DELETE route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the DELETE route.</returns>
    let delete (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.delete path handler

    /// <summary>Creates a PATCH route.</summary>
    /// <param name="path">The URL path to match.</param>
    /// <param name="handler">The handler for matched requests.</param>
    /// <returns>A route collection with the PATCH route.</returns>
    let patch (path: string) (handler: HttpHandler<'E>) : Routes<'E> = TypedRoutes.patch path handler
