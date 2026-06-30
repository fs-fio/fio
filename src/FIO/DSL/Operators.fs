namespace FIO.DSL

[<AutoOpen>]
module Operators =

    /// Runs two effects in sequence and keeps the second's value. Operator form of <c>ZipRight</c>.
    let inline ( *> ) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.ZipRight effect'

    /// Runs two effects in sequence and keeps the first's value. Operator form of <c>ZipLeft</c>.
    let inline (<*) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
        effect.ZipLeft effect'

    /// Runs two effects in sequence and pairs their values. Operator form of <c>Zip</c>.
    let inline (<*>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        effect.Zip effect'

    /// Runs two effects concurrently and pairs their values. Operator form of <c>ZipPar</c>.
    let inline (<&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        effect.ZipPar effect'

    /// Runs two effects concurrently and discards both values. Operator form of <c>ZipPar</c> then <c>Unit</c>.
    let inline (<&&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<unit, 'E> =
        effect.ZipPar(effect').Unit()

    /// Runs two effects concurrently and keeps the second's value. Operator form of <c>ZipParRight</c>.
    let inline (&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.ZipParRight effect'

    /// Runs two effects concurrently and keeps the first's value. Operator form of <c>ZipParLeft</c>.
    let inline (<&) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
        effect.ZipParLeft effect'

    /// Returns the first effect, or the second if the first fails. Operator form of <c>OrElse</c>.
    let inline (<|>) (effect: FIO<'A, 'E>) (effect': FIO<'A, 'E1>) : FIO<'A, 'E1> =
        effect.OrElse effect'

    /// Returns the first effect's success, or the second's as a Choice if the first fails. Operator form of <c>OrElseEither</c>.
    let inline (<+>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E1>) : FIO<Choice<'A, 'A1>, 'E1> =
        effect.OrElseEither effect'

    /// Races two effects and yields the winner as a Choice. Operator form of <c>RaceEither</c>.
    let inline (<?>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<Choice<'A, 'A1>, 'E> =
        effect.RaceEither effect'

    /// Passes the first effect's success value into the function. Operator form of <c>FlatMap</c>.
    let inline (>>=) (effect: FIO<'A, 'E>) (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.FlatMap cont

    /// Applies a function to an effect's success value. Operator form of <c>Map</c>.
    let inline (<!>) (mapper: 'A -> 'A1) (effect: FIO<'A, 'E>) : FIO<'A1, 'E> =
        effect.Map mapper
