namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Runtime
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

open System

// Shared runtime parameter, setup, cleanup and measured Run; each benchmark keeps its own class, which reports key on.
[<AbstractClass>]
type FIOBenchmark() =

    let mutable runtime = Unchecked.defaultof<FIORuntime>

    let mutable effect = Unchecked.defaultof<FIO<unit, exn>>

    member _.Runtimes = RuntimeParam.runtimes ()

    [<ParamsSource("Runtimes")>]
    member val Runtime = "" with get, set

    abstract CreateEffect: unit -> FIO<unit, exn>

    [<GlobalSetup>]
    member this.Setup () =
        runtime <- RuntimeParam.create this.Runtime
        effect <- this.CreateEffect ()

    [<GlobalCleanup>]
    member _.Cleanup () =
        match box runtime with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Benchmark>]
    member _.Run () =
        RuntimeParam.run runtime effect
