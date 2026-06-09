[<AutoOpen>]
module FIO.DSL.Operators

let inline ( *> ) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
    effect.ZipRight effect'

let inline (<*) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
    effect.ZipLeft effect'

let inline (<*>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
    effect.Zip effect'

let inline (<&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A * 'A1, 'E> =
    effect.ZipPar effect'

let inline (<&&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<unit, 'E> =
    effect.ZipPar(effect').Unit()

let inline (&>) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A1, 'E> =
    effect.ZipParRight effect'

let inline (<&) (effect: FIO<'A, 'E>) (effect': FIO<'A1, 'E>) : FIO<'A, 'E> =
    effect.ZipParLeft effect'

let inline (<|>) (effect: FIO<'A, 'E>) (effect': FIO<'A, 'E1>) : FIO<'A, 'E1> =
    effect.OrElse effect'

let inline (>>=) (effect: FIO<'A, 'E>) (cont: 'A -> FIO<'A1, 'E>) : FIO<'A1, 'E> =
    effect.FlatMap cont

let inline (<!>) (mapper: 'A -> 'A1) (effect: FIO<'A, 'E>) : FIO<'A1, 'E> =
    effect.Map mapper
