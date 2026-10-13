module FIO.Tests.Utilities

open System
open System.Threading

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open Expecto
open FsCheck.FSharp

let testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

let allRuntimes () =
    [
        new DirectRuntime() :> FIORuntime
        new WorkStealingRuntime(testConfig) :> FIORuntime
    ]

let resultWithin (timeout: TimeSpan) (fiber: FIO.DSL.Fiber<'A, 'E>) =
    let task = fiber.Task()
    if task.Wait timeout then Some task.Result else None

let waitForFlag (flag: bool ref) =
    let deadline = DateTime.UtcNow.AddSeconds 5.0
    while not flag.Value && DateTime.UtcNow < deadline do
        Thread.Sleep 1
    flag.Value

let private disposeRuntime (runtime: FIORuntime) =
    match box runtime with
    | :? IDisposable as disposable -> disposable.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
    testList
        name
        [
            for runtime in allRuntimes () ->
                testCase (runtime.GetType().Name) (fun () ->
                    try
                        f runtime
                    finally
                        disposeRuntime runtime)
        ]

let testAllRuntimesSequenced name (f: FIORuntime -> unit) =
    testSequenced <| testAllRuntimes name f

module FsCheckProperties =

    type Generators =

        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new WorkStealingRuntime(testConfig) :> FIORuntime)
                ]
            |> Arb.fromGen

    let stressEnabled =
        Environment.GetEnvironmentVariable "FIO_RUN_STRESS" = "1"

    let stressTestCase name f =
        if stressEnabled then testCase name f else ptestCase name f

    let stressTestAllRuntimes name (f: FIORuntime -> unit) =
        if stressEnabled then testAllRuntimes name f else ptestCase name ignore

    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }
