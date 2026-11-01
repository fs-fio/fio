namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

open System

[<RequireQualifiedAccess>]
module internal Trapezoidal =

    // Integrates one contiguous slice of the interval with the trapezoidal rule.
    let private partialEffect a h lo hi =
        FIO.attempt
            (fun () ->
                let mutable acc = 0.0
                for i in lo .. hi - 1 do
                    let x = a + (float i + 0.5) * h
                    acc <- acc + 4.0 / (1.0 + x * x)
                acc)
            id

    // Builds the Trapezoidal workload: parallel numerical integration approximating pi.
    let effect workerCount pointCount =
        fio {
            let a, b = 0.0, 1.0
            let h = (b - a) / float pointCount
            let chunk = pointCount / workerCount

            let workers =
                [ for workerIndex in 0 .. workerCount - 1 ->
                      let lo = workerIndex * chunk
                      let hi =
                        if workerIndex = workerCount - 1 then pointCount
                        else (workerIndex + 1) * chunk
                      partialEffect a h lo hi ]

            let! partials = FIO.collectAllPar workers
            let result = List.sum partials * h

            if abs (result - Math.PI) > 1e-3 then
                return! FIO.fail (InvalidOperationException $"Trapezoidal: {result} is not close to pi")
        }

// Measures parallel trapezoidal integration approximating pi across worker fibers.
[<MemoryDiagnoser>]
[<RankColumn>]
type TrapezoidalBenchmark() =
    inherit FIOBenchmark()

    member _.WorkerCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_WORKERS" [| 8; 16 |]

    member _.PointCounts =
        RuntimeParam.intParams "FIO_BENCH_TRAPEZOIDAL_POINTS" [| 1_000_000 |]

    [<ParamsSource("WorkerCounts")>]
    member val WorkerCount = 0 with get, set

    [<ParamsSource("PointCounts")>]
    member val PointCount = 0 with get, set

    override this.CreateEffect () =
        Trapezoidal.effect this.WorkerCount this.PointCount
