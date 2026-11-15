module FIO.Tests.WorkerInfrastructureTests

open FIO.Runtime
open FIO.Runtime.WorkStealing

open Expecto

open System

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
                            "EvaluationWorkers = 0",
                            { WorkerConfig.Default with
                                EvaluationWorkers = 0
                            },
                            "EWC: 0"
                            "EvaluationSteps = 0",
                            { WorkerConfig.Default with
                                EvaluationSteps = 0
                            },
                            "EWS: 0"
                        ]

                    for field, config, shown in invalid ->
                        testCase $"WorkStealingRuntime - rejects {field}" (fun () ->
                            let thrown =
                                try
                                    use _runtime = new WorkStealingRuntime(config)
                                    None
                                with ex ->
                                    Some ex

                            match thrown with
                            | Some(:? ArgumentException as ex) ->
                                Expect.stringContains
                                    ex.Message
                                    "Invalid worker configuration"
                                    "The message should say what is wrong"
                                Expect.stringContains ex.Message shown "The message should show the offending count"
                            | other -> failtest $"Expected ArgumentException, got {other}")
                ]
        ]
