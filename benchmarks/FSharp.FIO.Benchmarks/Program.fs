module private FSharp.FIO.Benchmarks.Program

open FSharp.FIO.Benchmarks.ArgParser
open FSharp.FIO.Benchmarks.Suite.BenchmarkRunner

open System
open System.Threading

module private ThreadPoolConfig =
    let configure () =
        let cores = Environment.ProcessorCount
        let minWorkerThreads = cores * 2
        let maxWorkerThreads = cores * 50
        let minIOThreads = cores
        let maxIOThreads = cores * 10
        
        ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
        ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

do ThreadPoolConfig.configure ()

[<EntryPoint>]
let main args =
    printArgs args
    let task =
        runBenchmarks
        <| parseArgs args
    task.Wait ()
    0
