/// <summary>
/// Random number generation as FIO effects.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Random

open FSharp.FIO.DSL

open System

/// <summary>Generates a random non-negative integer.</summary>
/// <returns>Effect returning a random non-negative integer.</returns>
let nextInt<'E> () : FIO<int, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.Next())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0)

/// <summary>Generates a random integer in the range [0, max).</summary>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random integer in [0, max).</returns>
let nextIntBounded<'E> (max: int) : FIO<int, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.Next max)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0)

/// <summary>Generates a random integer in the range [min, max).</summary>
/// <param name="min">Inclusive lower bound.</param>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random integer in [min, max).</returns>
let nextIntRange<'E> (min: int, max: int) : FIO<int, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.Next(min, max))
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed min)

/// <summary>Generates a random non-negative 64-bit integer.</summary>
/// <returns>Effect returning a random non-negative int64.</returns>
let nextInt64<'E> () : FIO<int64, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.NextInt64())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0L)

/// <summary>Generates a random 64-bit integer in the range [0, max).</summary>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random int64 in [0, max).</returns>
let nextInt64Bounded<'E> (max: int64) : FIO<int64, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.NextInt64 max)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0L)

/// <summary>Generates a random 64-bit integer in the range [min, max).</summary>
/// <param name="min">Inclusive lower bound.</param>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random int64 in [min, max).</returns>
let nextInt64Range<'E> (min: int64, max: int64) : FIO<int64, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.NextInt64(min, max))
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed min)

/// <summary>Generates a random floating-point number in the range [0.0, 1.0).</summary>
/// <returns>Effect returning a random float in [0.0, 1.0).</returns>
let nextDouble<'E> () : FIO<float, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.NextDouble())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0.0)

/// <summary>Generates a random floating-point number in the range [0.0, max).</summary>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random float in [0.0, max).</returns>
let nextDoubleBounded<'E> (max: float) : FIO<float, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.NextDouble() * max)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed 0.0)

/// <summary>Generates a random floating-point number in the range [min, max).</summary>
/// <param name="min">Inclusive lower bound.</param>
/// <param name="max">Exclusive upper bound.</param>
/// <returns>Effect returning a random float in [min, max).</returns>
let nextDoubleRange<'E> (min: float, max: float) : FIO<float, 'E> =
    FIO.attemptExn(fun () -> min + Random.Shared.NextDouble() * (max - min))
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed min)

/// <summary>Generates an array of random bytes.</summary>
/// <param name="count">Number of bytes to generate.</param>
/// <returns>Effect returning a byte array filled with random bytes.</returns>
let nextBytes<'E> (count: int) : FIO<byte[], 'E> =
    FIO.attemptExn(fun () ->
        let bytes = Array.zeroCreate<byte> count
        Random.Shared.NextBytes bytes
        bytes)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed [||])

/// <summary>Generates a random GUID.</summary>
/// <returns>Effect returning a random GUID.</returns>
let nextGuid<'E> () : FIO<Guid, 'E> =
    FIO.attemptExn(fun () -> Guid.NewGuid())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed Guid.Empty)

/// <summary>Generates a random boolean value (coin flip).</summary>
/// <returns>Effect returning true or false with equal probability.</returns>
let nextBool<'E> () : FIO<bool, 'E> =
    FIO.attemptExn(fun () -> Random.Shared.Next 2 = 1)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed false)

/// <summary>Shuffles a list randomly using Fisher-Yates algorithm.</summary>
/// <param name="items">List to shuffle.</param>
/// <returns>Effect returning a new list with elements in random order.</returns>
let shuffle<'T, 'E> (items: 'T list) : FIO<'T list, 'E> =
    FIO.attemptExn(fun () ->
        let arr = Array.ofList items
        let n = arr.Length
        for i = n - 1 downto 1 do
            let j = Random.Shared.Next(i + 1)
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
        List.ofArray arr)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed items)

/// <summary>Picks a random element from a list.</summary>
/// <param name="items">List to pick from.</param>
/// <returns>Effect returning Some(element) if list is non-empty, None otherwise.</returns>
let choice<'T, 'E> (items: 'T list) : FIO<'T option, 'E> =
    FIO.attemptExn(fun () ->
        match items with
        | [] -> None
        | _ -> Some items.[Random.Shared.Next(items.Length)])
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed None)

/// <summary>Picks a random element from a list, failing if the list is empty.</summary>
/// <param name="items">List to pick from.</param>
/// <param name="onEmpty">Function to create error when list is empty.</param>
/// <returns>Effect returning a random element or failing with custom error.</returns>
let choiceOrFail<'T, 'E> (items: 'T list, onEmpty: unit -> 'E) : FIO<'T, 'E> =
    choice(items).FlatMap(
        function
        | Some value -> FIO.succeed value
        | None -> FIO.fail(onEmpty()))
