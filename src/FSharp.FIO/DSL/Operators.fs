(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional operators for FIO effects.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Operators

/// <summary>
/// Sequences two effects, ignoring the result of the first effect (zipRight).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( *> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> =
    eff.ZipRight eff'

/// <summary>
/// Sequences two effects, returning the result of the first (zipLeft).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( <* ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> =
    eff.ZipLeft eff'

/// <summary>
/// Sequences two effects and succeeds with a tuple of their results (zip).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( <*> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
    eff.Zip eff'

/// <summary>
/// Executes two effects in parallel, returning a tuple of their results (zipPar).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( <&> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> =
    eff.ZipPar eff'

/// <summary>
/// Executes two effects in parallel, returning the result of the second (zipParRight).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( &> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> =
    eff.ZipParRight eff'

/// <summary>
/// Executes two effects in parallel, returning the result of the first (zipParLeft).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The second effect.</param>
let inline ( <& ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> =
    eff.ZipParLeft eff'

/// <summary>
/// Tries the first effect, falling back to the second on error (orElse).
/// </summary>
/// <param name="eff">The first effect.</param>
/// <param name="eff'">The fallback effect.</param>
let inline ( <|> ) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E1> =
    eff.OrElse eff'

/// <summary>
/// Chains an effect with a continuation function (flatMap/bind).
/// </summary>
/// <param name="eff">The effect to bind.</param>
/// <param name="cont">The continuation function.</param>
let inline ( >>= ) (eff: FIO<'R, 'E>) (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> =
    eff.FlatMap cont

/// <summary>
/// Maps a function over the result of the effect (fmap).
/// </summary>
/// <param name="mapper">The function to apply.</param>
/// <param name="eff">The effect to map.</param>
let inline ( <!> ) (mapper: 'R -> 'R1) (eff: FIO<'R, 'E>) : FIO<'R1, 'E> =
    eff.Map mapper
