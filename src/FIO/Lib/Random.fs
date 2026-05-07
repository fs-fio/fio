/// <summary>Provides random number generation as FIO effects.</summary>
[<RequireQualifiedAccess>]
module FIO.Random.Random

open FIO.DSL

open System

/// <summary>Transforms an exception by re-raising it, used as the error-mapping function when side effects cannot fail with a typed error.</summary>
/// <param name="ex">The exception to re-raise.</param>
/// <returns>Never returns; always throws <paramref name="ex"/>.</returns>
let inline private rethrow (ex: exn) : 'E = raise ex

/// <summary>Returns a random non-negative 32-bit integer in <c>[0, Int32.MaxValue)</c>.</summary>
/// <returns>An effect that completes with a random non-negative integer.</returns>
let inline nextInt<'E> () : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next()), rethrow)

/// <summary>Returns a random 32-bit integer in <c>[0, max)</c>.</summary>
/// <param name="max">The exclusive upper bound; must be non-negative.</param>
/// <returns>An effect that completes with a random integer below <paramref name="max"/>.</returns>
let inline nextIntBounded<'E> (max: int) : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next max), rethrow)

/// <summary>Returns a random 32-bit integer in <c>[min, max)</c>.</summary>
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound; must be greater than <paramref name="min"/>.</param>
/// <returns>An effect that completes with a random integer in the requested range.</returns>
let inline nextIntRange<'E> (min: int, max: int) : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next(min, max)), rethrow)

/// <summary>Returns a random non-negative 64-bit integer in <c>[0, Int64.MaxValue)</c>.</summary>
/// <returns>An effect that completes with a random non-negative 64-bit integer.</returns>
let inline nextInt64<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64()), rethrow)

/// <summary>Returns a random 64-bit integer in <c>[0, max)</c>.</summary>
/// <param name="max">The exclusive upper bound; must be non-negative.</param>
/// <returns>An effect that completes with a random 64-bit integer below <paramref name="max"/>.</returns>
let inline nextInt64Bounded<'E> (max: int64) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64 max), rethrow)

/// <summary>Returns a random 64-bit integer in <c>[min, max)</c>.</summary>
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound; must be greater than <paramref name="min"/>.</param>
/// <returns>An effect that completes with a random 64-bit integer in the requested range.</returns>
let inline nextInt64Range<'E> (min: int64, max: int64) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64(min, max)), rethrow)

/// <summary>Returns a random double-precision value in <c>[0.0, 1.0)</c>.</summary>
/// <returns>An effect that completes with a random double in the unit interval.</returns>
let inline nextDouble<'E> () : FIO<float, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextDouble()), rethrow)

/// <summary>Returns a random double-precision value in <c>[0.0, max)</c>.</summary>
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that completes with a random double below <paramref name="max"/>.</returns>
let inline nextDoubleBounded<'E> (max: float) : FIO<float, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextDouble() * max), rethrow)

/// <summary>Returns a random double-precision value in <c>[min, max)</c>.</summary>
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound; must be greater than <paramref name="min"/>.</param>
/// <returns>An effect that completes with a random double in the requested range.</returns>
let inline nextDoubleRange<'E> (min: float, max: float) : FIO<float, 'E> =
    FIO.attempt ((fun () -> min + Random.Shared.NextDouble() * (max - min)), rethrow)

/// <summary>Returns a freshly allocated array of random bytes.</summary>
/// <param name="count">The number of bytes to generate; must be non-negative.</param>
/// <returns>An effect that completes with a byte array of length <paramref name="count"/>.</returns>
let inline nextBytes<'E> (count: int) : FIO<byte[], 'E> =
    FIO.attempt (
        (fun () ->
            let bytes = Array.zeroCreate<byte> count
            Random.Shared.NextBytes bytes
            bytes),
        rethrow
    )

/// <summary>Returns a freshly generated random GUID.</summary>
/// <returns>An effect that completes with a new <c>Guid</c>.</returns>
let inline nextGuid<'E> () : FIO<Guid, 'E> =
    FIO.attempt ((fun () -> Guid.NewGuid()), rethrow)

/// <summary>Returns a random boolean as a coin flip.</summary>
/// <returns>An effect that completes with <c>true</c> or <c>false</c> with equal probability.</returns>
let inline nextBool<'E> () : FIO<bool, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next 2 = 1), rethrow)

/// <summary>Transforms a list into a randomly ordered list using the Fisher-Yates algorithm.</summary>
/// <typeparam name="'T">The element type of the list.</typeparam>
/// <param name="items">The list to shuffle; the input is not mutated.</param>
/// <returns>An effect that completes with a new list containing the same elements in random order.</returns>
let inline shuffle<'T, 'E> (items: 'T list) : FIO<'T list, 'E> =
    FIO.attempt (
        (fun () ->
            let arr = Array.ofList items
            let n = arr.Length

            for i = n - 1 downto 1 do
                let j = Random.Shared.Next(i + 1)
                let temp = arr.[i]
                arr.[i] <- arr.[j]
                arr.[j] <- temp

            List.ofArray arr),
        rethrow
    )

/// <summary>Returns a random element from a list, or <c>None</c> when the list is empty.</summary>
/// <typeparam name="'T">The element type of the list.</typeparam>
/// <param name="items">The list to pick from.</param>
/// <returns>An effect that completes with <c>Some</c> wrapping a random element, or <c>None</c> when <paramref name="items"/> is empty.</returns>
let inline choice<'T, 'E> (items: 'T list) : FIO<'T option, 'E> =
    FIO.attempt (
        (fun () ->
            match items with
            | [] -> None
            | _ -> Some items.[Random.Shared.Next(items.Length)]),
        rethrow
    )

/// <summary>Returns a random element from a list, failing through a supplied function when the list is empty.</summary>
/// <typeparam name="'T">The element type of the list.</typeparam>
/// <param name="items">The list to pick from.</param>
/// <param name="onEmpty">A function producing the typed error when <paramref name="items"/> is empty.</param>
/// <returns>An effect that completes with a random element, or fails with the result of <paramref name="onEmpty"/> when the list is empty.</returns>
let inline choiceOrFail<'T, 'E> (items: 'T list, onEmpty: unit -> 'E) : FIO<'T, 'E> =
    choice(items)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.fail (onEmpty ())
        )
