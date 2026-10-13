namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module internal Fibonacci =

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
    let effect n threshold =
        (fibEffect n threshold).Unit()

// Measures recursive parallel Fibonacci, forking a fiber per branch above a threshold.
[<MemoryDiagnoser>]
[<RankColumn>]
type FibonacciBenchmark() =
    inherit FIOBenchmark()

    member _.Ns =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_N" [| 25; 30 |]

    member _.Thresholds =
        RuntimeParam.intParams "FIO_BENCH_FIBONACCI_THRESHOLD" [| 12 |]

    [<ParamsSource("Ns")>]
    member val N = 0 with get, set

    [<ParamsSource("Thresholds")>]
    member val Threshold = 0 with get, set

    override this.CreateEffect () =
        Fibonacci.effect this.N this.Threshold
