/// <summary>Provides the entry point for the FIO HTTP test suite.</summary>
module FIO.Http.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
