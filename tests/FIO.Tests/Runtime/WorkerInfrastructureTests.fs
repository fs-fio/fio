module FIO.Tests.WorkerInfrastructureTests

open FIO.Runtime
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open System

open Expecto

[<Tests>]
let workerInfrastructureTests =
    testList
        "WorkerInfrastructure"
        [
            testList
                "new - rejects a non-positive count with ArgumentException"
                [
                    let invalid =
                        [
                            "EvaluationWorkers = 0", { WorkerConfig.Default with EvaluationWorkers = 0 }, "EWC: 0"
                            "EvaluationSteps = 0", { WorkerConfig.Default with EvaluationSteps = 0 }, "EWS: 0"
                            "BlockingWorkers = 0", { WorkerConfig.Default with BlockingWorkers = 0 }, "BWC: 0"
                        ]

                    for runtimeName, make in
                        [
                            "PollingRuntime", (fun config -> new PollingRuntime(config) :> FIORuntime)
                            "SignalingRuntime", (fun config -> new SignalingRuntime(config) :> FIORuntime)
                            "WorkStealingRuntime", (fun config -> new WorkStealingRuntime(config) :> FIORuntime)
                        ] do
                        for field, config, shown in invalid ->
                            testCase $"{runtimeName}, {field}" (fun () ->
                                let thrown =
                                    try
                                        use _runtime = make config
                                        None
                                    with ex ->
                                        Some ex

                                match thrown with
                                | Some(:? ArgumentException as ex) ->
                                    Expect.stringContains ex.Message "Invalid worker configuration" "The message should say what is wrong"
                                    Expect.stringContains ex.Message shown "The message should show the offending count"
                                | other -> failtest $"Expected ArgumentException, got {other}")
                ]
        ]
