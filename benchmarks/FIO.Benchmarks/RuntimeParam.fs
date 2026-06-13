[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.RuntimeParam

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System
open System.Threading.Tasks

let private defaultRuntimes =
    [| "Direct"; "Cooperative-12-200-1"; "Concurrent-12-200-1" |]

let private parseWorker (spec: string) (part: string) =
    match Int32.TryParse part with
    | true, value -> value
    | _ -> raise (ArgumentException $"Invalid worker count '{part}' in runtime spec '{spec}'")

let create (spec: string) : FIORuntime =
    match spec.Split '-' with
    | [| "Direct" |] -> new DirectRuntime()
    | [| "Cooperative"; ewc; ews; bwc |] ->
        new CooperativeRuntime
            {
                EvaluationWorkers = parseWorker spec ewc
                EvaluationSteps = parseWorker spec ews
                BlockingWorkers = parseWorker spec bwc
            }
    | [| "Concurrent"; ewc; ews; bwc |] ->
        new ConcurrentRuntime
            {
                EvaluationWorkers = parseWorker spec ewc
                EvaluationSteps = parseWorker spec ews
                BlockingWorkers = parseWorker spec bwc
            }
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
        |> Array.map (fun part ->
            match Int32.TryParse part with
            | true, n -> n
            | _ ->
                raise (ArgumentException $"Invalid integer '{part}' in {envVar} (expected comma-separated integers)"))

let run (runtime: FIORuntime) (effect: FIO<unit, exn>) : Task =
    (task {
        match! runtime.Run(effect).Task() with
        | Succeeded _ -> ()
        | Failed error -> raise error
        | Interrupted ex -> raise ex
    })
    :> Task
