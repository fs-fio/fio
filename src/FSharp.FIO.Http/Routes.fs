/// <summary>
/// Route collection types for HTTP routing.
/// </summary>
namespace FSharp.FIO.Http

open FSharp.FIO.DSL

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
/// </summary>
type Routes<'E> =
    private
        {
            /// <summary>The list of routes.</summary>
            RouteList: Route<'E> list
            /// <summary>The handler for unmatched requests.</summary>
            NotFoundHandler: HttpHandler<'E>
        }

/// <summary>
/// Functions for creating and composing route collections.
/// </summary>
module Routes =

    /// <summary>
    /// Creates an empty route collection.
    /// </summary>
    let empty<'E> : Routes<'E> =
        {
            RouteList = []
            NotFoundHandler = HttpHandler.notFound
        }

    /// <summary>
    /// Creates a route collection with a single parameterized route.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    let single (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) : Routes<'E> =
        {
            RouteList = [{ Pattern = pattern; Handler = handler }]
            NotFoundHandler = HttpHandler.notFound
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
        {
            RouteList = routes1.RouteList @ routes2.RouteList
            NotFoundHandler = routes1.NotFoundHandler
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
    /// </summary>
    /// <param name="request">The HTTP request.</param>
    /// <param name="routes">The route collection.</param>
    let dispatch (request: HttpRequest) (routes: Routes<'E>) : FIO<HttpResponse, 'E> =
        let rec tryRoutes routeList =
            match routeList with
            | [] -> routes.NotFoundHandler request
            | route :: rest ->
                match RoutePattern.tryMatch route.Pattern request with
                | Some parameters ->
                    route.Handler parameters request
                | None ->
                    tryRoutes rest

        tryRoutes routes.RouteList

    /// <summary>
    /// Adds a parameterized route to a route collection.
    /// </summary>
    /// <param name="pattern">The route pattern.</param>
    /// <param name="handler">The handler receiving extracted parameters.</param>
    /// <param name="routes">The route collection.</param>
    let add (pattern: RoutePattern) (handler: obj list -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        {
            routes with
                RouteList = routes.RouteList @ [{ Pattern = pattern; Handler = handler }]
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
        {
            RouteList = routes |> List.map (fun (pattern, handler) -> { Pattern = pattern; Handler = fun _ -> handler })
            NotFoundHandler = HttpHandler.notFound
        }

    /// <summary>
    /// Transforms all handlers in a route collection.
    /// </summary>
    /// <param name="f">The transformation function.</param>
    /// <param name="routes">The route collection.</param>
    let map (f: HttpHandler<'E> -> HttpHandler<'E>) (routes: Routes<'E>) : Routes<'E> =
        {
            RouteList = routes.RouteList |> List.map (fun route ->
                { route with Handler = fun parameters -> f (route.Handler parameters) })
            NotFoundHandler = f routes.NotFoundHandler
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
        member _.Yield _ = Routes.empty<'E>

        member _.Combine(routes1: Routes<'E>, routes2: Routes<'E>) =
            Routes.combine routes1 routes2

        member _.Delay(f: unit -> Routes<'E>) = f()

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
    let GET (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.get path handler

    /// <summary>
    /// Creates a POST route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let POST (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.post path handler
    
    /// <summary>
    /// Creates a PUT route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let PUT (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.put path handler

    /// <summary>
    /// Creates a DELETE route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let DELETE (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.delete path handler

    /// <summary>
    /// Creates a PATCH route.
    /// </summary>
    /// <param name="path">The path string.</param>
    /// <param name="handler">The request handler.</param>
    let PATCH (path: string) (handler: HttpHandler<'E>) : Routes<'E> =
        TypedRoutes.patch path handler
