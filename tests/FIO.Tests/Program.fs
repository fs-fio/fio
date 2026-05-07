/// <summary>Provides the entry point for the FIO core test suite.</summary>
module FIO.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
