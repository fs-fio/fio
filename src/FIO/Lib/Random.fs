/// Random number generation as FIO effects.
[<RequireQualifiedAccess>]
module FIO.Random.Random

open FIO.DSL

open System

let inline private rethrow (ex: exn) : 'E = raise ex

/// Generates a random non-negative integer in [0, Int32.MaxValue).
/// <returns>An effect that produces a random non-negative integer.</returns>
let inline nextInt<'E> () : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next()), rethrow)

/// Generates a random integer in [0, max).
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random integer in [0, max).</returns>
let inline nextIntBounded<'E> (max: int) : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next max), rethrow)

/// Generates a random integer in [min, max).
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random integer in [min, max).</returns>
let inline nextIntRange<'E> (min: int, max: int) : FIO<int, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next(min, max)), rethrow)

/// Generates a random non-negative 64-bit integer in [0, Int64.MaxValue).
/// <returns>An effect that produces a random non-negative 64-bit integer.</returns>
let inline nextInt64<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64()), rethrow)

/// Generates a random 64-bit integer in [0, max).
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random 64-bit integer in [0, max).</returns>
let inline nextInt64Bounded<'E> (max: int64) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64 max), rethrow)

/// Generates a random 64-bit integer in [min, max).
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random 64-bit integer in [min, max).</returns>
let inline nextInt64Range<'E> (min: int64, max: int64) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextInt64(min, max)), rethrow)

/// Generates a random double in [0.0, 1.0).
/// <returns>An effect that produces a random double in [0.0, 1.0).</returns>
let inline nextDouble<'E> () : FIO<float, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextDouble()), rethrow)

/// Generates a random double in [0.0, max).
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random double in [0.0, max).</returns>
let inline nextDoubleBounded<'E> (max: float) : FIO<float, 'E> =
    FIO.attempt ((fun () -> Random.Shared.NextDouble() * max), rethrow)

/// Generates a random double in [min, max).
/// <param name="min">The inclusive lower bound.</param>
/// <param name="max">The exclusive upper bound.</param>
/// <returns>An effect that produces a random double in [min, max).</returns>
let inline nextDoubleRange<'E> (min: float, max: float) : FIO<float, 'E> =
    FIO.attempt ((fun () -> min + Random.Shared.NextDouble() * (max - min)), rethrow)

/// Generates an array of random bytes.
/// <param name="count">The number of random bytes to generate.</param>
/// <returns>An effect that produces a byte array of the specified length.</returns>
let inline nextBytes<'E> (count: int) : FIO<byte[], 'E> =
    FIO.attempt (
        (fun () ->
            let bytes = Array.zeroCreate<byte> count
            Random.Shared.NextBytes bytes
            bytes),
        rethrow
    )

/// Generates a random GUID.
/// <returns>An effect that produces a random GUID.</returns>
let inline nextGuid<'E> () : FIO<Guid, 'E> =
    FIO.attempt ((fun () -> Guid.NewGuid()), rethrow)

/// Generates a random boolean (coin flip).
/// <returns>An effect that produces a random boolean value.</returns>
let inline nextBool<'E> () : FIO<bool, 'E> =
    FIO.attempt ((fun () -> Random.Shared.Next 2 = 1), rethrow)

/// Shuffles a list randomly using the Fisher-Yates algorithm.
/// <param name="items">The list to shuffle.</param>
/// <returns>An effect that produces a new list with elements in random order.</returns>
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

/// Picks a random element from a list, returning None if the list is empty.
/// <param name="items">The list to pick from.</param>
/// <returns>An effect that produces Some random element, or None if the list is empty.</returns>
let inline choice<'T, 'E> (items: 'T list) : FIO<'T option, 'E> =
    FIO.attempt (
        (fun () ->
            match items with
            | [] -> None
            | _ -> Some items.[Random.Shared.Next(items.Length)]),
        rethrow
    )

/// Picks a random element from a list, failing with a typed error if the list is empty.
/// <param name="items">The list to pick from.</param>
/// <param name="onEmpty">Error factory invoked when the list is empty.</param>
/// <returns>An effect that produces a random element, or fails if the list is empty.</returns>
let inline choiceOrFail<'T, 'E> (items: 'T list, onEmpty: unit -> 'E) : FIO<'T, 'E> =
    choice(items)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.fail (onEmpty ())
        )
