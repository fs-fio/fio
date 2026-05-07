/// <summary>Provides the entry point for the FIO sockets test suite.</summary>
module FIO.Sockets.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [ Parallel; Summary; Colours 256 ] args
