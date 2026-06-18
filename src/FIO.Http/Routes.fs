namespace FIO.Http

open FIO.DSL

/// A single route: a pattern and a handler that receives captured parameters.
type Route<'E> =
    {
        /// The pattern this route matches.
        Pattern: RoutePattern
        /// Builds the handler from the captured path parameters.
        Handler: obj list -> HttpHandler<'E>
    }

/// A collection of routes with a not-found fallback.
type Routes<'E> =
    private
        {
            RouteList: Route<'E> list
            NotFoundHandler: HttpHandler<'E>
            ExactMatchIndex: Map<HttpMethod * string, obj list -> HttpHandler<'E>>
            ParameterizedRoutes: Route<'E> list
        }

[<RequireQualifiedAccess>]
module Routes =

    let private isExactMatch (pattern: RoutePattern) =
        match pattern.Path with
        | Exact _ -> true
        | _ -> false

    let private getExactPath (pattern: RoutePattern) =
        match pattern.Path with
        | Exact segments -> Some("/" + String.concat "/" segments)
        | _ -> None

    let private buildIndex (routes: Route<'E> list) =
        let exactMatches, parameterized =
            routes |> List.partition (fun r -> isExactMatch r.Pattern)

        let index =
            exactMatches
            |> List.choose (fun route ->
                match getExactPath route.Pattern with
                | Some path ->
                    Some((route.Pattern.Method, path), route.Handler)
                | None ->
                    None)
            |> List.fold
                (fun acc (key, handler) ->
                    if Map.containsKey key acc then acc
                    else Map.add key handler acc)
                Map.empty
        index, parameterized

    /// The empty route collection.
    let empty<'E> : Routes<'E> =
        {
            RouteList = []
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = Map.empty
            ParameterizedRoutes = []
        }

    /// Creates a route collection from one pattern and a parameter-aware handler.
    let single (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) =
        let route = { Pattern = pattern; Handler = handler }
        let index, parameterized = buildIndex [ route ]
        {
            RouteList = [ route ]
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Creates a route collection from one pattern and a handler.
    let route (pattern: RoutePattern) (handler: HttpHandler<'E>) =
        single pattern (fun _ -> handler)

    /// Combines two route collections, preferring the first on conflicts.
    let combine (routes: Routes<'E>) (routes': Routes<'E>) =
        let mergedIndex =
            routes'.ExactMatchIndex
            |> Map.fold
                (fun acc key handler -> if Map.containsKey key acc then acc else Map.add key handler acc)
                routes.ExactMatchIndex
        {
            RouteList = routes.RouteList @ routes'.RouteList
            NotFoundHandler = routes.NotFoundHandler
            ExactMatchIndex = mergedIndex
            ParameterizedRoutes = routes.ParameterizedRoutes @ routes'.ParameterizedRoutes
        }

    /// Sets the handler used when no route matches.
    let withNotFound (handler: HttpHandler<'E>) (routes: Routes<'E>) =
        { routes with NotFoundHandler = handler }

    let private allowedMethodsFor (request: HttpRequest) (routes: Routes<'E>) =
        let normalizedPath = "/" + String.concat "/" request.PathSegments
        let exact =
            routes.ExactMatchIndex
            |> Map.toList
            |> List.choose (fun ((method, path), _) -> if path = normalizedPath then Some method else None)
        let parameterized =
            routes.ParameterizedRoutes
            |> List.choose (fun route ->
                match route.Pattern.ParamExtractor request.PathSegments with
                | Some _ -> Some route.Pattern.Method
                | None -> None)
        exact @ parameterized |> List.distinct

    /// Dispatches a request to the matching route, falling back to not-found or 405 Method Not Allowed.
    let dispatch (request: HttpRequest) (routes: Routes<'E>) =
        let tryDispatch (req: HttpRequest) =
            let normalizedPath = "/" + String.concat "/" req.PathSegments
            let key = req.Method, normalizedPath
            match Map.tryFind key routes.ExactMatchIndex with
            | Some handler -> Some(handler [] req)
            | None ->
                let rec tryRoutes routeList =
                    match routeList with
                    | [] -> None
                    | route :: rest ->
                        match RoutePattern.tryMatch route.Pattern req with
                        | Some parameters -> Some(route.Handler parameters req)
                        | None -> tryRoutes rest
                tryRoutes routes.ParameterizedRoutes

        match tryDispatch request with
        | Some effect -> effect
        | None ->
            let headFallback =
                if request.Method = HttpMethod.HEAD then
                    tryDispatch { request with Method = HttpMethod.GET }
                else
                    None
            match headFallback with
            | Some effect -> effect
            | None ->
                if request.Method = HttpMethod.OPTIONS then
                    routes.NotFoundHandler request
                else
                    match allowedMethodsFor request routes with
                    | [] -> routes.NotFoundHandler request
                    | methods ->
                        FIO.succeed (Response.methodNotAllowed (methods |> List.map (fun m -> m.ToString())))

    /// Adds a pattern and parameter-aware handler to a route collection.
    let add (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) (routes: Routes<'E>) =
        let newRoute = { Pattern = pattern; Handler = handler }
        let updatedRoutes = routes.RouteList @ [ newRoute ]
        if isExactMatch pattern then
            match getExactPath pattern with
            | Some path ->
                let key = pattern.Method, path
                let index =
                    if Map.containsKey key routes.ExactMatchIndex then routes.ExactMatchIndex
                    else Map.add key handler routes.ExactMatchIndex
                { routes with
                    RouteList = updatedRoutes
                    ExactMatchIndex = index
                }
            | None ->
                { routes with RouteList = updatedRoutes }
        else
            { routes with
                RouteList = updatedRoutes
                ParameterizedRoutes = routes.ParameterizedRoutes @ [ newRoute ]
            }

    /// Adds a pattern and handler to a route collection.
    let addRoute (pattern: RoutePattern) (handler: HttpHandler<'E>) (routes: Routes<'E>) =
        add pattern (fun _ -> handler) routes

    /// Builds a route collection from a list of pattern/handler pairs.
    let fromList (routes: (RoutePattern * HttpHandler<'E>) list) =
        let routeList =
            routes |> List.map (fun (pattern, handler) ->
                { Pattern = pattern; Handler = fun _ -> handler })
        let index, parameterized = buildIndex routeList
        {
            RouteList = routeList
            NotFoundHandler = HttpHandler.notFound
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Transforms every handler in a route collection, including the not-found handler.
    let map (func: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) =
        let transformedRoutes =
            routes.RouteList
            |> List.map (fun route ->
                { route with
                    Handler = fun parameters -> func (route.Handler parameters)
                })
        let index, parameterized = buildIndex transformedRoutes
        {
            RouteList = transformedRoutes
            NotFoundHandler = func routes.NotFoundHandler
            ExactMatchIndex = index
            ParameterizedRoutes = parameterized
        }

    /// Transforms every handler in a route collection. Alias for map.
    let transform (func: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) =
        map func routes

module RouteBuilder =

    type RouteCollector<'E>() =

        member _.Yield _ =
            Routes.empty<'E>

        member _.Combine (routes: Routes<'E>, routes': Routes<'E>) =
            Routes.combine routes routes'

        member _.Delay (func: unit -> Routes<'E>) =
            func ()

        member _.Zero () =
            Routes.empty<'E>

    /// A computation expression for composing route collections.
    let routes<'E> = RouteCollector<'E>()

module RoutesOperators =

    /// Combines two route collections. Operator form of <c>Routes.combine</c>.
    let (++) (routes: Routes<'E>) (routes': Routes<'E>) =
        Routes.combine routes routes'

    /// Pairs a pattern with a handler. Operator form of <c>Routes.route</c>.
    let (=>) (pattern: RoutePattern) (handler: HttpHandler<'E>) =
        Routes.route pattern handler

    /// Pairs a pattern with a parameter-aware handler. Operator form of <c>Routes.single</c>.
    let (==>) (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) =
        Routes.single pattern handler

module TypedRoutes =

    /// Creates a GET route from a path and handler.
    let get (path: string) (handler: HttpHandler<'E>) =
        Routes.route (Route.get path) handler

    /// Creates a POST route from a path and handler.
    let post (path: string) (handler: HttpHandler<'E>) =
        Routes.route (Route.post path) handler

    /// Creates a PUT route from a path and handler.
    let put (path: string) (handler: HttpHandler<'E>) =
        Routes.route (Route.put path) handler

    /// Creates a DELETE route from a path and handler.
    let delete (path: string) (handler: HttpHandler<'E>) =
        Routes.route (Route.delete path) handler

    /// Creates a PATCH route from a path and handler.
    let patch (path: string) (handler: HttpHandler<'E>) =
        Routes.route (Route.patch path) handler

    /// Creates a GET route capturing an integer path parameter.
    let getInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) =
        let pattern = RoutePattern.get <| RoutePath.withInt before after
        Routes.single pattern <| fun parameters ->
            match parameters with
            | [ :? int as id ] ->
                handler id
            | _ ->
                HttpHandler.badRequestText "Invalid parameters"

    /// Creates a GET route capturing a string path parameter.
    let getString (before: string list) (after: string list) (handler: string -> HttpHandler<'E>) =
        let pattern = RoutePattern.get <| RoutePath.withString before after
        Routes.single pattern <| fun parameters ->
            match parameters with
            | [ :? string as value ] ->
                handler value
            | _ ->
                HttpHandler.badRequestText "Invalid parameters"

    /// Creates a POST route capturing an integer path parameter.
    let postInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) =
        let pattern = RoutePattern.post <| RoutePath.withInt before after
        Routes.single pattern <| fun parameters ->
            match parameters with
            | [ :? int as id ] ->
                handler id
            | _ ->
                HttpHandler.badRequestText "Invalid parameters"

    /// Creates a PUT route capturing an integer path parameter.
    let putInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) =
        let pattern = RoutePattern.put <| RoutePath.withInt before after
        Routes.single pattern <| fun parameters ->
            match parameters with
            | [ :? int as id ] ->
                handler id
            | _ ->
                HttpHandler.badRequestText "Invalid parameters"

    /// Creates a DELETE route capturing an integer path parameter.
    let deleteInt (before: string list) (after: string list) (handler: int -> HttpHandler<'E>) =
        let pattern = RoutePattern.delete <| RoutePath.withInt before after
        Routes.single pattern <| fun parameters ->
            match parameters with
            | [ :? int as id ] ->
                handler id
            | _ ->
                HttpHandler.badRequestText "Invalid parameters"

module SimpleRoutes =

    /// Creates a GET route from a path and handler.
    let get (path: string) (handler: HttpHandler<'E>) =
        TypedRoutes.get path handler

    /// Creates a POST route from a path and handler.
    let post (path: string) (handler: HttpHandler<'E>) =
        TypedRoutes.post path handler

    /// Creates a PUT route from a path and handler.
    let put (path: string) (handler: HttpHandler<'E>) =
        TypedRoutes.put path handler

    /// Creates a DELETE route from a path and handler.
    let delete (path: string) (handler: HttpHandler<'E>) =
        TypedRoutes.delete path handler

    /// Creates a PATCH route from a path and handler.
    let patch (path: string) (handler: HttpHandler<'E>) =
        TypedRoutes.patch path handler
