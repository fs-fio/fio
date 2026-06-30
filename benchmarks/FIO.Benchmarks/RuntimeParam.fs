[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.RuntimeParam

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System

let private defaultRuntimes =
    [| "Direct"; "Polling-12-200-1"; "Signaling-12-200-1"; "WorkStealing-12-200-1" |]

// Parses a positive worker count from a runtime spec, raising on invalid input.
let private parseWorker (spec: string) (part: string) =
    match Int32.TryParse part with
    | true, value when value > 0 -> value
    | true, _ -> raise (ArgumentException $"Worker count must be greater than 0 in runtime spec '{spec}': '{part}'")
    | _ -> raise (ArgumentException $"Invalid worker count '{part}' in runtime spec '{spec}'")

// Creates a runtime from a spec string like "Direct" or "WorkStealing-12-200-1".
let create (spec: string) : FIORuntime =
    match spec.Split '-' with
    | [| "Direct" |] -> new DirectRuntime()
    | [| "Polling"; ewc; ews; bwc |] ->
        new PollingRuntime
            {
                EvaluationWorkers = parseWorker spec ewc
                EvaluationSteps = parseWorker spec ews
                BlockingWorkers = parseWorker spec bwc
            }
    | [| "Signaling"; ewc; ews; bwc |] ->
        new SignalingRuntime
            {
                EvaluationWorkers = parseWorker spec ewc
                EvaluationSteps = parseWorker spec ews
                BlockingWorkers = parseWorker spec bwc
            }
    | [| "WorkStealing"; ewc; ews; bwc |] ->
        new WorkStealingRuntime
            {
                EvaluationWorkers = parseWorker spec ewc
                EvaluationSteps = parseWorker spec ews
                BlockingWorkers = parseWorker spec bwc
            }
    | _ -> raise (ArgumentException $"Unknown runtime spec: {spec}")

// The runtime specs to benchmark, from FIO_BENCH_RUNTIMES or a default set.
let runtimes () =
    match Environment.GetEnvironmentVariable "FIO_BENCH_RUNTIMES" with
    | null
    | "" -> defaultRuntimes
    | value -> value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

// Parses a comma-separated integer parameter list from an env var, or returns the defaults.
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

// Runs an effect on the runtime, raising if it fails or is interrupted.
let run (runtime: FIORuntime) effect =
    task {
        match! runtime.Run(effect).Task() with
        | Succeeded _ -> ()
        | Failed error -> raise error
        | Interrupted ex -> raise ex
    }
