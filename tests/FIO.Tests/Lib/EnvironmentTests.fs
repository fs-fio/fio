module FIO.Tests.EnvironmentTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL

open Expecto

open FIO.Runtime

module Environment = FIO.Environment.Environment

[<Tests>]
let environmentGetTests =
    testList "Environment.get" [

        testPropertyWithConfig fsCheckConfig "Environment.get returns existing env var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.get<exn>("PATH", fun () -> exn "PATH not found")
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNotNull result "PATH should exist"
            Expect.isNonEmpty result "PATH should not be empty"

        testPropertyWithConfig fsCheckConfig "Environment.get fails on missing env var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.get<string>("THIS_ENV_VAR_DEFINITELY_DOES_NOT_EXIST_12345", fun () -> "not found")
            let result = runtime.Run(eff).UnsafeError()
            Expect.equal result "not found" "Should fail on missing env var"
    ]

[<Tests>]
let environmentGetOptionTests =
    testList "Environment.getOption" [

        testPropertyWithConfig fsCheckConfig "Environment.getOption returns Some for existing env var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.getOption<exn> "PATH"
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isSome result "PATH should exist"

        testPropertyWithConfig fsCheckConfig "Environment.getOption returns None for missing env var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.getOption<exn> "THIS_ENV_VAR_DEFINITELY_DOES_NOT_EXIST_12345"
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNone result "Missing env var should return None"
    ]

[<Tests>]
let environmentGetOrDefaultTests =
    testList "Environment.getOrDefault" [

        testPropertyWithConfig fsCheckConfig "Environment.getOrDefault returns value for existing env var"
        <| fun (runtime: FIORuntime, defaultValue: string) ->
            let eff = Environment.getOrDefault<exn>("PATH", defaultValue)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.notEqual result defaultValue "Should return actual PATH, not default"

        testPropertyWithConfig fsCheckConfig "Environment.getOrDefault returns default for missing env var"
        <| fun (runtime: FIORuntime, defaultValue: string) ->
            let eff = Environment.getOrDefault<exn>("THIS_ENV_VAR_DEFINITELY_DOES_NOT_EXIST_12345", defaultValue)
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result defaultValue "Should return default for missing env var"
    ]

[<Tests>]
let environmentGetIntTests =
    testList "Environment.getInt" [

        testPropertyWithConfig fsCheckConfig "Environment.getInt parses numeric env var"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_INT", "42")
            try
                let eff = Environment.getInt<string>("FIO_TEST_INT", (fun () -> "not found"), (fun s -> $"parse error: {s}"))
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.equal result 42 "Should parse int correctly"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_INT", null)

        testPropertyWithConfig fsCheckConfig "Environment.getInt fails on non-numeric env var"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_NON_INT", "not a number")
            try
                let eff = Environment.getInt<string>("FIO_TEST_NON_INT", (fun () -> "not found"), (fun s -> $"parse error: {s}"))
                let result = runtime.Run(eff).UnsafeError()
                Expect.stringStarts result "parse error" "Should fail on non-numeric"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_NON_INT", null)
    ]

[<Tests>]
let environmentGetBoolTests =
    testList "Environment.getBool" [

        testPropertyWithConfig fsCheckConfig "Environment.getBool parses true"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_TRUE", "true")
            try
                let eff = Environment.getBool<string>("FIO_TEST_BOOL_TRUE", (fun () -> "not found"), (fun _ -> "parse error"))
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.isTrue result "Should parse 'true' as true"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_TRUE", null)

        testPropertyWithConfig fsCheckConfig "Environment.getBool parses false"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_FALSE", "false")
            try
                let eff = Environment.getBool<string>("FIO_TEST_BOOL_FALSE", (fun () -> "not found"), (fun _ -> "parse error"))
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.isFalse result "Should parse 'false' as false"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_FALSE", null)

        testPropertyWithConfig fsCheckConfig "Environment.getBool fails on invalid bool"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_INVALID", "maybe")
            try
                let eff = Environment.getBool<string>("FIO_TEST_BOOL_INVALID", (fun () -> "not found"), (fun _ -> "parse error"))
                let result = runtime.Run(eff).UnsafeError()
                Expect.equal result "parse error" "Should fail on invalid bool"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL_INVALID", null)
    ]

[<Tests>]
let environmentGetAllTests =
    testList "Environment.getAll" [

        testPropertyWithConfig fsCheckConfig "Environment.getAll returns map with entries"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.getAll<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isGreaterThan (result |> Map.count) 0 "Should have some env vars"
            Expect.isTrue (result |> Map.containsKey "PATH") "PATH should be in env vars"
    ]

[<Tests>]
let environmentCurrentDirectoryTests =
    testList "Environment.currentDirectory" [

        testPropertyWithConfig fsCheckConfig "Environment.currentDirectory returns valid path"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.currentDirectory<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNotNull result "Current directory should not be null"
            Expect.isNonEmpty result "Current directory should not be empty"
            Expect.isTrue (System.IO.Directory.Exists result) "Current directory should exist"
    ]

[<Tests>]
let environmentMachineNameTests =
    testList "Environment.machineName" [

        testPropertyWithConfig fsCheckConfig "Environment.machineName returns valid name"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.machineName<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNotNull result "Machine name should not be null"
            Expect.isNonEmpty result "Machine name should not be empty"
    ]

[<Tests>]
let environmentUserNameTests =
    testList "Environment.userName" [

        testPropertyWithConfig fsCheckConfig "Environment.userName returns valid name"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.userName<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isNotNull result "User name should not be null"
            Expect.isNonEmpty result "User name should not be empty"
    ]

[<Tests>]
let environmentProcessorCountTests =
    testList "Environment.processorCount" [

        testPropertyWithConfig fsCheckConfig "Environment.ProcessorCount is positive"
        <| fun (_: FIORuntime) ->
            Expect.isGreaterThan Environment.ProcessorCount 0 "Processor count should be > 0"
    ]

[<Tests>]
let environmentIsSetTests =
    testList "Environment.isSet" [

        testPropertyWithConfig fsCheckConfig "Environment.isSet returns true for existing var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.isSet<exn> "PATH"
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "PATH should be set"

        testPropertyWithConfig fsCheckConfig "Environment.isSet returns false for missing var"
        <| fun (runtime: FIORuntime) ->
            let eff = Environment.isSet<exn> "THIS_ENV_VAR_DEFINITELY_DOES_NOT_EXIST_12345"
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isFalse result "Missing var should not be set"
    ]
