/// <summary>
/// Path codec types for type-safe URL path parsing and generation.
/// </summary>
namespace FSharp.FIO.Http

open System

/// <summary>
/// Result of attempting to match a path codec against URL segments.
/// </summary>
type PathMatchResult<'T> =
    /// <summary>
    /// Successful match with extracted value and remaining segments.
    /// </summary>
    | Match of value: 'T * remaining: string list
    /// <summary>
    /// No match.
    /// </summary>
    | NoMatch

/// <summary>
/// Codec for URL paths enabling type-safe path parsing.
/// Note: Encoding (URL generation) is not fully supported for parameterized codecs.
/// This codec is primarily designed for decoding (matching) URLs.
/// </summary>
type PathCodec<'T> =
    private
        {
            Encode: 'T -> string list
            Decode: string list -> PathMatchResult<'T>
        }

    /// <summary>
    /// Attempts to match the codec against URL segments.
    /// </summary>
    /// <param name="segments">The URL path segments to match.</param>
    member this.Match(segments: string list) =
        this.Decode segments

    /// <summary>
    /// Encodes a value to URL path segments.
    /// Warning: This is not supported for codecs with parameters (int, string, guid).
    /// Only works for simple literal path codecs.
    /// </summary>
    /// <param name="value">The value to encode.</param>
    member this.ToSegments(value: 'T) =
        this.Encode value

/// <summary>
/// Functions for creating and composing path codecs.
/// </summary>
module PathCodec =

    /// <summary>
    /// Creates a path codec from encode and decode functions.
    /// </summary>
    /// <param name="encode">The encoding function.</param>
    /// <param name="decode">The decoding function.</param>
    let create encode decode =
        { Encode = encode; Decode = decode }

    /// <summary>
    /// Path codec that matches the root path (empty path only).
    /// </summary>
    let root : PathCodec<unit> =
        create
            (fun () -> [])
            (fun segments ->
                match segments with
                | [] -> Match((), [])
                | _ -> NoMatch)

    /// <summary>
    /// Creates a path codec that matches a literal path segment.
    /// </summary>
    /// <param name="literal">The literal segment to match.</param>
    /// <param name="next">The next codec in the chain.</param>
    let segment (literal: string) (next: PathCodec<'T>) : PathCodec<'T> =
        create
            (fun value -> literal :: next.ToSegments value)
            (fun segments ->
                match segments with
                | head :: tail when head = literal -> next.Match tail
                | _ -> NoMatch)

    /// <summary>
    /// Creates a path codec that captures an integer parameter.
    /// Note: Encoding is not supported for parameterized codecs - use for decoding only.
    /// </summary>
    /// <param name="next">The next codec in the chain.</param>
    let int (next: PathCodec<'T>) : PathCodec<int -> 'T> =
        create
            (fun f ->
                invalidOp "PathCodec encoding is not supported for parameterized paths. Use this codec for URL matching/decoding only.")
            (fun segments ->
                match segments with
                | head :: tail ->
                    match Int32.TryParse head with
                    | true, value ->
                        match next.Match tail with
                        | Match(rest, remaining) -> Match((fun i -> rest), remaining)
                        | NoMatch -> NoMatch
                    | false, _ -> NoMatch
                | [] -> NoMatch)

    /// <summary>
    /// Creates a path codec that captures a string parameter.
    /// Note: Encoding is not supported for parameterized codecs - use for decoding only.
    /// </summary>
    /// <param name="next">The next codec in the chain.</param>
    let string (next: PathCodec<'T>) : PathCodec<string -> 'T> =
        create
            (fun f ->
                invalidOp "PathCodec encoding is not supported for parameterized paths. Use this codec for URL matching/decoding only.")
            (fun segments ->
                match segments with
                | head :: tail ->
                    match next.Match(tail) with
                    | Match(rest, remaining) -> Match((fun s -> rest), remaining)
                    | NoMatch -> NoMatch
                | [] -> NoMatch)

    /// <summary>
    /// Creates a path codec that captures a GUID parameter.
    /// Note: Encoding is not supported for parameterized codecs - use for decoding only.
    /// </summary>
    /// <param name="next">The next codec in the chain.</param>
    let guid (next: PathCodec<'T>) : PathCodec<Guid -> 'T> =
        create
            (fun f ->
                invalidOp "PathCodec encoding is not supported for parameterized paths. Use this codec for URL matching/decoding only.")
            (fun segments ->
                match segments with
                | head :: tail ->
                    match Guid.TryParse(head) with
                    | true, value ->
                        match next.Match(tail) with
                        | Match(rest, remaining) -> Match((fun g -> rest), remaining)
                        | NoMatch -> NoMatch
                    | false, _ -> NoMatch
                | [] -> NoMatch)

    /// <summary>
    /// Path codec that captures all remaining path segments.
    /// </summary>
    let rest : PathCodec<string list> =
        create
            (fun segments -> segments)
            (fun segments -> Match(segments, []))

    /// <summary>
    /// Combines two path codecs sequentially.
    /// Note: Encoding is not supported for parameterized codecs - use for decoding only.
    /// </summary>
    /// <param name="codec1">The first codec.</param>
    /// <param name="codec2">The second codec.</param>
    let combine (codec1: PathCodec<'A -> 'B>) (codec2: PathCodec<'A>) : PathCodec<'B> =
        create
            (fun value ->
                invalidOp "PathCodec encoding is not supported for combined parameterized paths. Use this codec for URL matching/decoding only.")
            (fun segments ->
                match codec1.Match(segments) with
                | Match(f, remaining) ->
                    match codec2.Match(remaining) with
                    | Match(value, final) -> Match(f value, final)
                    | NoMatch -> NoMatch
                | NoMatch -> NoMatch)

/// <summary>
/// Convenience functions and operators for path codec construction.
/// </summary>
module Path =
    /// <summary>
    /// Root path codec.
    /// </summary>
    let root = PathCodec.root

    /// <summary>
    /// Appends a literal segment to a path codec.
    /// </summary>
    /// <param name="codec">The path codec.</param>
    /// <param name="literal">The literal segment to append.</param>
    let (/) (codec: PathCodec<'T>) (literal: string) : PathCodec<'T> =
        PathCodec.segment literal codec

    /// <summary>
    /// Captures an integer parameter from the path.
    /// </summary>
    /// <param name="codec">The path codec.</param>
    let int (codec: PathCodec<'T>) : PathCodec<int -> 'T> =
        PathCodec.int codec

    /// <summary>
    /// Captures a string parameter from the path.
    /// </summary>
    /// <param name="codec">The path codec.</param>
    let str (codec: PathCodec<'T>) : PathCodec<string -> 'T> =
        PathCodec.string codec

    /// <summary>
    /// Captures a GUID parameter from the path.
    /// </summary>
    /// <param name="codec">The path codec.</param>
    let guid (codec: PathCodec<'T>) : PathCodec<Guid -> 'T> =
        PathCodec.guid codec

    /// <summary>
    /// Captures all remaining path segments.
    /// </summary>
    let rest = PathCodec.rest

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
    let exact (segments: string list) : RoutePath =
        Exact segments

    /// <summary>
    /// Creates a prefix path match.
    /// </summary>
    /// <param name="segments">The path segments to match as prefix.</param>
    let prefix (segments: string list) : RoutePath =
        Prefix segments

    /// <summary>
    /// Parses a path string into a route path.
    /// </summary>
    /// <param name="path">The path string to parse.</param>
    let ofString (path: string) : RoutePath =
        let segments =
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        Exact segments

    /// <summary>
    /// Attempts to match a route path against URL segments.
    /// </summary>
    /// <param name="routePath">The route path pattern.</param>
    /// <param name="segments">The URL segments to match.</param>
    let tryMatch (routePath: RoutePath) (segments: string list) : (obj list * string list) option =
        match routePath with
        | Exact expected ->
            if segments = expected then
                Some([], [])
            else
                None
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
            | Some (param :: remaining) ->
                match Int32.TryParse(param) with
                | true, value -> matchAfter after remaining [box value]
                | false, _ -> None
            | _ -> None)

    /// <summary>
    /// Creates a route path with a string parameter.
    /// </summary>
    /// <param name="before">The path segments before the parameter.</param>
    /// <param name="after">The path segments after the parameter.</param>
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
            | Some (param :: remaining) -> matchAfter after remaining [box param]
            | _ -> None)
