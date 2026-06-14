[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fibonacci

open FIO.DSL

let rec private fibSequence n =
    if n < 2 then n
    else fibSequence (n - 1) + fibSequence (n - 2)

let rec private fibEffect n threshold =
    if n <= threshold then
        FIO.attempt (fun () -> fibSequence n) id
    else
        (fibEffect (n - 1) threshold).Fork().FlatMap <| fun leftFiber ->
            (fibEffect (n - 2) threshold).FlatMap <| fun right ->
                leftFiber.Join().Map <| fun left -> left + right

let effect n threshold : FIO<unit, exn> =
    (fibEffect n threshold).Unit()
