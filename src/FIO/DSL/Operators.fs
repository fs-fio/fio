/// <summary>Provides functional operators for FIO effect composition.</summary>
[<AutoOpen>]
module FIO.DSL.Operators

/// <summary>Combines two effects sequentially and returns the second effect's result.</summary>
/// <param name="eff">The effect to evaluate first; its result is discarded.</param>
/// <param name="eff'">The effect to evaluate second; its result is propagated.</param>
/// <returns>An effect that runs <paramref name="eff"/> then <paramref name="eff'"/> and completes with the second result.</returns>
let inline ( *> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipRight eff'

/// <summary>Combines two effects sequentially and returns the first effect's result.</summary>
/// <param name="eff">The effect to evaluate first; its result is propagated.</param>
/// <param name="eff'">The effect to evaluate second; its result is discarded.</param>
/// <returns>An effect that runs <paramref name="eff"/> then <paramref name="eff'"/> and completes with the first result.</returns>
let inline (<*) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipLeft eff'

/// <summary>Combines two effects sequentially and returns both results as a tuple.</summary>
/// <param name="eff">The effect to evaluate first.</param>
/// <param name="eff'">The effect to evaluate second.</param>
/// <returns>An effect that runs both in order and completes with a pair of their results.</returns>
let inline (<*>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.Zip eff'

/// <summary>Combines two effects in parallel and returns both results as a tuple.</summary>
/// <param name="eff">The first effect to fork.</param>
/// <param name="eff'">The second effect to fork.</param>
/// <returns>An effect that runs both concurrently and completes with a pair of their results.</returns>
/// <remarks>The two effects run on separate fibers in fiber runtimes; if either fails, the other is interrupted.</remarks>
let inline (<&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.ZipPar eff'

/// <summary>Combines two effects in parallel and discards both results.</summary>
/// <param name="eff">The first effect to fork.</param>
/// <param name="eff'">The second effect to fork.</param>
/// <returns>An effect that runs both concurrently and completes with unit.</returns>
/// <remarks>Useful for fire-and-forget parallel side effects where neither result is needed.</remarks>
let inline (<&&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<unit, 'E> = eff.ZipPar(eff').Unit()

/// <summary>Combines two effects in parallel and returns the second effect's result.</summary>
/// <param name="eff">The first effect to fork; its result is discarded.</param>
/// <param name="eff'">The second effect to fork; its result is propagated.</param>
/// <returns>An effect that runs both concurrently and completes with the second result.</returns>
let inline (&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipParRight eff'

/// <summary>Combines two effects in parallel and returns the first effect's result.</summary>
/// <param name="eff">The first effect to fork; its result is propagated.</param>
/// <param name="eff'">The second effect to fork; its result is discarded.</param>
/// <returns>An effect that runs both concurrently and completes with the first result.</returns>
let inline (<&) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipParLeft eff'

/// <summary>Combines two effects as a fallback, attempting the first and using the second on error.</summary>
/// <param name="eff">The effect to attempt first.</param>
/// <param name="eff'">The fallback effect to evaluate when <paramref name="eff"/> fails.</param>
/// <returns>An effect that completes with the first result on success, or with the fallback effect's outcome on failure.</returns>
let inline (<|>) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E1> = eff.OrElse eff'

/// <summary>Combines an effect with a continuation that runs on its successful result.</summary>
/// <param name="eff">The effect whose success value is passed to the continuation.</param>
/// <param name="cont">A function from the success value of <paramref name="eff"/> to the next effect to run.</param>
/// <returns>An effect that runs <paramref name="eff"/> and, on success, runs the effect produced by <paramref name="cont"/>.</returns>
let inline (>>=) (eff: FIO<'R, 'E>) (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap cont

/// <summary>Transforms the success value of an effect with a pure function.</summary>
/// <param name="mapper">A pure function from the original success value to the new success value.</param>
/// <param name="eff">The effect whose success value is transformed.</param>
/// <returns>An effect that completes with <paramref name="mapper"/> applied to the original success value.</returns>
let inline (<!>) (mapper: 'R -> 'R1) (eff: FIO<'R, 'E>) : FIO<'R1, 'E> = eff.Map mapper
