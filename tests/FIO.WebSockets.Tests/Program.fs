/// <summary>Provides the entry point for the FIO WebSockets test suite.</summary>
module FIO.WebSockets.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
