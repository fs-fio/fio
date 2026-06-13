namespace FIO.Http

open System

type RoutePath =
    | Exact of string list
    | Prefix of string list
    | Pattern of (string list -> (obj list * string list) option)

[<RequireQualifiedAccess>]
module RoutePath =

    let exact (segments: string list) =
        Exact segments

    let prefix (segments: string list) =
        Prefix segments

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

    let fromString (path: string) =
        parse path

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

type RoutePattern =
    {
        Method: HttpMethod
        Path: RoutePath
        ParamExtractor: string list -> obj list option
    }

[<RequireQualifiedAccess>]
module RoutePattern =

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

    let get (path: RoutePath) =
        create HttpMethod.GET path

    let post (path: RoutePath) =
        create HttpMethod.POST path

    let put (path: RoutePath) =
        create HttpMethod.PUT path

    let delete (path: RoutePath) =
        create HttpMethod.DELETE path

    let patch (path: RoutePath) =
        create HttpMethod.PATCH path

    let head (path: RoutePath) =
        create HttpMethod.HEAD path

    let options (path: RoutePath) =
        create HttpMethod.OPTIONS path

    let tryMatch (pattern: RoutePattern) (request: HttpRequest) =
        if request.Method = pattern.Method then
            pattern.ParamExtractor request.PathSegments
        else
            None

[<RequireQualifiedAccess>]
module Route =

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

    let get (path: string) =
        RoutePattern.get (RoutePath.fromString path)

    let post (path: string) =
        RoutePattern.post (RoutePath.fromString path)

    let put (path: string) =
        RoutePattern.put (RoutePath.fromString path)

    let delete (path: string) =
        RoutePattern.delete (RoutePath.fromString path)

    let patch (path: string) =
        RoutePattern.patch (RoutePath.fromString path)

    let head (path: string) =
        RoutePattern.head (RoutePath.fromString path)

    let options (path: string) =
        RoutePattern.options (RoutePath.fromString path)

module RouteOperators =

    let (=>) (method: HttpMethod) (path: string) =
        RoutePattern.create method (RoutePath.fromString path)

    let get path =
        Route.get path

    let post path =
        Route.post path

    let put path =
        Route.put path

    let delete path =
        Route.delete path

    let patch path =
        Route.patch path

    let head path =
        Route.head path

    let options path =
        Route.options path
