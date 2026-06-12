module internal FIO.Benchmarks.RuntimeParam

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System

let private defaultRuntimes =
    [| "Direct"; "Cooperative-12-200-1"; "Concurrent-12-200-1" |]

let create (spec: string) : FIORuntime =
    match spec.Split '-' with
    | [| "Direct" |] -> new DirectRuntime()
    | [| "Cooperative"; ewc; ews; bwc |] -> new CooperativeRuntime { EvaluationWorkers = int ewc; EvaluationSteps = int ews; BlockingWorkers = int bwc }
    | [| "Concurrent"; ewc; ews; bwc |] -> new ConcurrentRuntime { EvaluationWorkers = int ewc; EvaluationSteps = int ews; BlockingWorkers = int bwc }
    | _ -> raise (ArgumentException $"Unknown runtime spec: {spec}")

let runtimes () =
    match Environment.GetEnvironmentVariable "FIO_BENCH_RUNTIMES" with
    | null
    | "" -> defaultRuntimes
    | value -> value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let intParams (envVar: string) (defaults: int array) =
    match Environment.GetEnvironmentVariable envVar with
    | null
    | "" -> defaults
    | value ->
        value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map int
