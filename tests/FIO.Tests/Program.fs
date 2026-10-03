module FIO.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    match args with
    | [| "--child"; scenario |] -> ChildProcess.run scenario
    | _ -> runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
