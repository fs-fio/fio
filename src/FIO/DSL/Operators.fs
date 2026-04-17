/// Functional operators for FIO effects.
[<AutoOpen>]
module FIO.DSL.Operators

/// Sequences two effects, returning the second result.
/// <param name="eff">The left effect to evaluate first.</param>
/// <param name="eff'">The right effect to evaluate second.</param>
/// <returns>An effect that evaluates both sequentially and returns the second result.</returns>
let inline ( *> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipRight eff'

/// Sequences two effects, returning the first result.
/// <param name="eff">The left effect to evaluate first.</param>
/// <param name="eff'">The right effect to evaluate second.</param>
/// <returns>An effect that evaluates both sequentially and returns the first result.</returns>
let inline (<*) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipLeft eff'

/// Sequences two effects, returning a tuple of results.
/// <param name="eff">The left effect to evaluate first.</param>
/// <param name="eff'">The right effect to evaluate second.</param>
/// <returns>An effect that evaluates both sequentially and returns a tuple of their results.</returns>
let inline (<*>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.Zip eff'

/// Executes two effects in parallel, returning a tuple of results.
/// <param name="eff">The left effect to fork.</param>
/// <param name="eff'">The right effect to fork.</param>
/// <returns>An effect that runs both concurrently and returns a tuple of their results.</returns>
/// <remarks>Both effects run concurrently in fiber runtimes. If either fails, the other is interrupted.</remarks>
let inline (<&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.ZipPar eff'

/// Executes two effects in parallel, returning unit.
/// <param name="eff">The left effect to fork.</param>
/// <param name="eff'">The right effect to fork.</param>
/// <returns>An effect that runs both concurrently and discards both results.</returns>
/// <remarks>Both effects run concurrently. Useful for fire-and-forget parallel side effects.</remarks>
let inline (<&&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<unit, 'E> = eff.ZipPar(eff').Unit()

/// Executes two effects in parallel, returning the second result.
/// <param name="eff">The left effect to fork.</param>
/// <param name="eff'">The right effect to fork.</param>
/// <returns>An effect that runs both concurrently and returns the second result.</returns>
/// <remarks>Both effects run concurrently; the first result is discarded.</remarks>
let inline (&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipParRight eff'

/// Executes two effects in parallel, returning the first result.
/// <param name="eff">The left effect to fork.</param>
/// <param name="eff'">The right effect to fork.</param>
/// <returns>An effect that runs both concurrently and returns the first result.</returns>
/// <remarks>Both effects run concurrently; the second result is discarded.</remarks>
let inline (<&) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipParLeft eff'

/// Tries the first effect, falling back to the second on error.
/// <param name="eff">The first effect to attempt.</param>
/// <param name="eff'">The fallback effect evaluated if the first fails.</param>
/// <returns>An effect that succeeds with the first result or falls back to the second.</returns>
let inline (<|>) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E1> = eff.OrElse eff'

/// Chains an effect with a continuation function.
/// <param name="eff">The first effect to evaluate.</param>
/// <param name="cont">Continuation function applied to the success result.</param>
/// <returns>An effect that chains the two computations.</returns>
let inline (>>=) (eff: FIO<'R, 'E>) (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap cont

/// Maps a function over the result of an effect.
/// <param name="mapper">The function to apply to the effect's result.</param>
/// <param name="eff">The effect whose result is transformed.</param>
/// <returns>An effect whose result is the mapped value.</returns>
let inline (<!>) (mapper: 'R -> 'R1) (eff: FIO<'R, 'E>) : FIO<'R1, 'E> = eff.Map mapper
