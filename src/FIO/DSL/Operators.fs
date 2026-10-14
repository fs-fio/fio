namespace FIO.DSL

[<AutoOpen>]
module Operators =

    /// <summary>Runs two effects in sequence and keeps the second's value. Operator form of <c>ZipRight</c>.</summary>
    let inline ( *> ) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.ZipRight effect'

    /// <summary>Runs two effects in sequence and keeps the first's value. Operator form of <c>ZipLeft</c>.</summary>
    let inline (<*) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
        effect.ZipLeft effect'

    /// <summary>Runs two effects in sequence and pairs their values. Operator form of <c>Zip</c>.</summary>
    let inline (<*>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        effect.Zip effect'

    /// <summary>Runs two effects concurrently and pairs their values. Operator form of <c>ZipPar</c>.</summary>
    let inline (<&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
        effect.ZipPar effect'

    /// <summary>Runs two effects concurrently and discards both values. Operator form of <c>ZipPar</c> then <c>Unit</c>.</summary>
    let inline (<&&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<unit, 'E> =
        effect.ZipPar(effect').Unit()

    /// <summary>Runs two effects concurrently and keeps the second's value. Operator form of <c>ZipParRight</c>.</summary>
    let inline (&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.ZipParRight effect'

    /// <summary>Runs two effects concurrently and keeps the first's value. Operator form of <c>ZipParLeft</c>.</summary>
    let inline (<&) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
        effect.ZipParLeft effect'

    /// <summary>Returns the first effect, or the second if the first fails. Operator form of <c>OrElse</c>.</summary>
    let inline (<|>) (effect: FIO<'A, 'E>) (effect': FIO<'A, 'E1>) : FIO<'A, 'E1> =
        effect.OrElse effect'

    /// <summary>Returns the first effect's success, or the second's as a Choice if the first fails. Operator form of <c>OrElseEither</c>.</summary>
    let inline (<+>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E1>) : FIO<Choice<'A, 'A1>, 'E1> =
        effect.OrElseEither effect'

    /// <summary>Races two effects and yields the winner as a Choice. Operator form of <c>RaceEither</c>.</summary>
    let inline (<?>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<Choice<'A, 'A1>, 'E> =
        effect.RaceEither effect'

    /// <summary>Passes the first effect's success value into the function. Operator form of <c>FlatMap</c>.</summary>
    let inline (>>=) (effect: FIO<'A, 'E>) (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
        effect.FlatMap cont

    /// <summary>Applies a function to an effect's success value. Operator form of <c>Map</c>.</summary>
    let inline (<!>) (mapper: 'A -> 'A1) (effect: FIO<'A, 'E>) : FIO<'A1, 'E> =
        effect.Map mapper
