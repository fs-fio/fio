module FIO.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    match List.ofArray args with
    | [ "--child"; scenario ] -> ChildProcess.run scenario
    | "--child" :: other ->
        eprintfn $"Expected one child scenario, got: %A{other}"
        2
    | _ -> runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
