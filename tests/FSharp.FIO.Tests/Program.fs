open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [Parallel; Summary; Colours 256;] args
