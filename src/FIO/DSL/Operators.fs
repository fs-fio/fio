/// Functional operators for FIO effects.
[<AutoOpen>]
module FIO.DSL.Operators

/// Sequences two effects, returning the second result.
let inline ( *> ) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipRight eff'

/// Sequences two effects, returning the first result.
let inline (<*) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipLeft eff'

/// Sequences two effects, returning a tuple of results.
let inline (<*>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.Zip eff'

/// Executes two effects in parallel, returning a tuple of results.
let inline (<&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R * 'R1, 'E> = eff.ZipPar eff'

/// Executes two effects in parallel, returning unit.
let inline (<&&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<unit, 'E> = eff.ZipPar(eff').Unit()

/// Executes two effects in parallel, returning the second result.
let inline (&>) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.ZipParRight eff'

/// Executes two effects in parallel, returning the first result.
let inline (<&) (eff: FIO<'R, 'E>) (eff': FIO<'R1, 'E>) : FIO<'R, 'E> = eff.ZipParLeft eff'

/// Tries the first effect, falling back to the second on error.
let inline (<|>) (eff: FIO<'R, 'E>) (eff': FIO<'R, 'E1>) : FIO<'R, 'E1> = eff.OrElse eff'

/// Chains an effect with a continuation function.
let inline (>>=) (eff: FIO<'R, 'E>) (cont: 'R -> FIO<'R1, 'E>) : FIO<'R1, 'E> = eff.FlatMap cont

/// Maps a function over the result of an effect.
let inline (<!>) (mapper: 'R -> 'R1) (eff: FIO<'R, 'E>) : FIO<'R1, 'E> = eff.Map mapper
