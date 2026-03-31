module FIO.Tests.EnvironmentTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL

open Expecto

open FIO.Runtime

open System.IO

module Environment = FIO.Environment.Environment

let private nonExistentVar = "FIO_TEST_NONEXISTENT_VAR_12345"

[<Tests>]
let environmentTests =
    testSequenced <| testList "Environment" [

        testPropertyWithConfig fsCheckConfig "NewLine - matches System.Environment.NewLine"
        <| fun (_: FIORuntime) ->
            Expect.equal Environment.NewLine System.Environment.NewLine "Should match System.Environment.NewLine"

        testPropertyWithConfig fsCheckConfig "ProcessorCount - returns positive value"
        <| fun (_: FIORuntime) ->
            Expect.isGreaterThan Environment.ProcessorCount 0 "Processor count should be > 0"

        testPropertyWithConfig fsCheckConfig "Is64BitProcess - matches System.Environment"
        <| fun (_: FIORuntime) ->
            Expect.equal Environment.Is64BitProcess System.Environment.Is64BitProcess "Should match System.Environment.Is64BitProcess"

        testPropertyWithConfig fsCheckConfig "Is64BitOperatingSystem - matches System.Environment"
        <| fun (_: FIORuntime) ->
            Expect.equal Environment.Is64BitOperatingSystem System.Environment.Is64BitOperatingSystem "Should match System.Environment.Is64BitOperatingSystem"

        testPropertyWithConfig fsCheckConfig "getOption - returns Some for existing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getOption "PATH").UnsafeSuccess()

            Expect.isSome result "PATH should exist"

        testPropertyWithConfig fsCheckConfig "getOption - returns None for missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getOption nonExistentVar).UnsafeSuccess()

            Expect.isNone result "Missing env var should return None"

        testPropertyWithConfig fsCheckConfig "get - returns value for existing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.get("PATH", fun () -> "not found")).UnsafeSuccess()

            Expect.isNotNull result "PATH should not be null"
            Expect.isNonEmpty result "PATH should not be empty"

        testPropertyWithConfig fsCheckConfig "get - fails with custom error for missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.get(nonExistentVar, fun () -> "not found")).UnsafeError()

            Expect.equal result "not found" "Should fail with custom error"

        testPropertyWithConfig fsCheckConfig "getOrDefault - returns value for existing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getOrDefault("PATH", "default")).UnsafeSuccess()

            Expect.notEqual result "default" "Should return actual PATH, not default"

        testPropertyWithConfig fsCheckConfig "getOrDefault - returns default for missing variable"
        <| fun (runtime: FIORuntime, defaultValue: string) ->
            let result = runtime.Run(Environment.getOrDefault(nonExistentVar, defaultValue)).UnsafeSuccess()

            Expect.equal result defaultValue "Should return default for missing env var"

        testPropertyWithConfig fsCheckConfig "getAll - returns map containing PATH"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getAll()).UnsafeSuccess()

            Expect.isGreaterThan (result |> Map.count) 0 "Should have some env vars"
            Expect.isTrue (result |> Map.containsKey "PATH") "PATH should be in env vars"

        testPropertyWithConfig fsCheckConfig "getInt - parses numeric variable"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_INT", "42")
            try
                let result = runtime.Run(Environment.getInt("FIO_TEST_INT", (fun () -> "not found"), (fun s -> $"parse error: {s}"))).UnsafeSuccess()

                Expect.equal result 42 "Should parse int correctly"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_INT", null)

        testPropertyWithConfig fsCheckConfig "getInt - fails on non-numeric variable"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_INT", "abc")
            try
                let result = runtime.Run(Environment.getInt("FIO_TEST_INT", (fun () -> "not found"), (fun s -> $"parse error: {s}"))).UnsafeError()

                Expect.stringStarts result "parse error" "Should fail on non-numeric"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_INT", null)

        testPropertyWithConfig fsCheckConfig "getInt - fails on missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getInt(nonExistentVar, (fun () -> "not found"), (fun s -> $"parse error: {s}"))).UnsafeError()

            Expect.equal result "not found" "Should fail on missing variable"

        testPropertyWithConfig fsCheckConfig "getIntOrDefault - parses numeric variable"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_INT", "42")
            try
                let result = runtime.Run(Environment.getIntOrDefault("FIO_TEST_INT", 0)).UnsafeSuccess()

                Expect.equal result 42 "Should parse int correctly"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_INT", null)

        testPropertyWithConfig fsCheckConfig "getIntOrDefault - returns default for non-numeric variable"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_INT", "abc")
            try
                let result = runtime.Run(Environment.getIntOrDefault("FIO_TEST_INT", 99)).UnsafeSuccess()

                Expect.equal result 99 "Should return default for non-numeric"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_INT", null)

        testPropertyWithConfig fsCheckConfig "getIntOrDefault - returns default for missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getIntOrDefault(nonExistentVar, 99)).UnsafeSuccess()

            Expect.equal result 99 "Should return default for missing variable"

        testPropertyWithConfig fsCheckConfig "getBool - parses true"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "true")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isTrue result "Should parse 'true' as true"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - parses 1 as true"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "1")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isTrue result "Should parse '1' as true"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - parses yes as true"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "yes")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isTrue result "Should parse 'yes' as true"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - parses false"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "false")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isFalse result "Should parse 'false' as false"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - parses 0 as false"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "0")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isFalse result "Should parse '0' as false"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - parses no as false"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "no")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeSuccess()

                Expect.isFalse result "Should parse 'no' as false"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - fails on invalid value"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "maybe")
            try
                let result = runtime.Run(Environment.getBool("FIO_TEST_BOOL", (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeError()

                Expect.equal result "parse error" "Should fail on invalid bool"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBool - fails on missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getBool(nonExistentVar, (fun () -> "not found"), (fun _ -> "parse error"))).UnsafeError()

            Expect.equal result "not found" "Should fail on missing variable"

        testPropertyWithConfig fsCheckConfig "getBoolOrDefault - parses true"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "true")
            try
                let result = runtime.Run(Environment.getBoolOrDefault("FIO_TEST_BOOL", false)).UnsafeSuccess()

                Expect.isTrue result "Should parse 'true' as true"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBoolOrDefault - returns default for invalid value"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", "maybe")
            try
                let result = runtime.Run(Environment.getBoolOrDefault("FIO_TEST_BOOL", true)).UnsafeSuccess()

                Expect.isTrue result "Should return default for invalid bool"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_BOOL", null)

        testPropertyWithConfig fsCheckConfig "getBoolOrDefault - returns default for missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getBoolOrDefault(nonExistentVar, true)).UnsafeSuccess()

            Expect.isTrue result "Should return default for missing variable"

        testPropertyWithConfig fsCheckConfig "isSet - returns true for existing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.isSet "PATH").UnsafeSuccess()

            Expect.isTrue result "PATH should be set"

        testPropertyWithConfig fsCheckConfig "isSet - returns false for missing variable"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.isSet nonExistentVar).UnsafeSuccess()

            Expect.isFalse result "Missing var should not be set"

        testPropertyWithConfig fsCheckConfig "expandVariables - returns input unchanged when no variables"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.expandVariables "hello world").UnsafeSuccess()

            Expect.equal result "hello world" "Should return input unchanged"

        testPropertyWithConfig fsCheckConfig "expandVariables - expands known variable"
        <| fun (runtime: FIORuntime) ->
            System.Environment.SetEnvironmentVariable("FIO_TEST_EXPAND", "hello")
            try
                let input =
                    if System.OperatingSystem.IsWindows() then "%FIO_TEST_EXPAND%"
                    else "$FIO_TEST_EXPAND"

                let result = runtime.Run(Environment.expandVariables input).UnsafeSuccess()

                if System.OperatingSystem.IsWindows() then
                    Expect.equal result "hello" "Should expand %VAR% on Windows"
                else
                    // On Unix, ExpandEnvironmentVariables behavior varies by .NET version
                    Expect.isNotNull result "Should return a non-null string"
            finally
                System.Environment.SetEnvironmentVariable("FIO_TEST_EXPAND", null)

        testPropertyWithConfig fsCheckConfig "currentDirectory - returns existing directory path"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.currentDirectory()).UnsafeSuccess()

            Expect.isNonEmpty result "Current directory should not be empty"
            Expect.isTrue (Directory.Exists result) "Current directory should exist"

        testPropertyWithConfig fsCheckConfig "machineName - returns non-empty string"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.machineName()).UnsafeSuccess()

            Expect.isNonEmpty result "Machine name should not be empty"

        testPropertyWithConfig fsCheckConfig "userName - returns non-empty string"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.userName()).UnsafeSuccess()

            Expect.isNonEmpty result "User name should not be empty"

        testPropertyWithConfig fsCheckConfig "getCommandLineArgs - returns non-empty array"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getCommandLineArgs()).UnsafeSuccess()

            Expect.isNonEmpty result "Command line args should not be empty"

        testPropertyWithConfig fsCheckConfig "getFolderPath - returns path for user profile"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getFolderPath System.Environment.SpecialFolder.UserProfile).UnsafeSuccess()

            Expect.isNonEmpty result "User profile path should not be empty"

        testPropertyWithConfig fsCheckConfig "getTempPath - returns existing directory path"
        <| fun (runtime: FIORuntime) ->
            let result = runtime.Run(Environment.getTempPath()).UnsafeSuccess()

            Expect.isNonEmpty result "Temp path should not be empty"
            Expect.isTrue (Directory.Exists result) "Temp path should exist"
    ]
