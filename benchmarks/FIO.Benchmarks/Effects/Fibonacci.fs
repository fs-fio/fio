[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fibonacci

open FIO.DSL

// Computes Fibonacci sequentially (used below the parallel threshold).
let rec private fibSequence n =
    if n < 2 then n
    else fibSequence (n - 1) + fibSequence (n - 2)

// Computes Fibonacci in parallel, forking a fiber per branch until n reaches the threshold.
let rec private fibEffect n threshold =
    if n <= threshold then
        FIO.attempt (fun () -> fibSequence n) id
    else
        (fibEffect (n - 1) threshold).Fork().FlatMap <| fun leftFiber ->
            (fibEffect (n - 2) threshold).FlatMap <| fun right ->
                leftFiber.Join().Map <| fun left -> left + right

// Builds the Fibonacci workload: recursive parallel computation of fib n.
let effect n threshold : FIO<unit, exn> =
    (fibEffect n threshold).Unit()
