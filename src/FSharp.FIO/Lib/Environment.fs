/// <summary>
/// Environment variable access and system information as FIO effects.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Environment.Environment

open FSharp.FIO.DSL

open System
open System.IO
open System.Collections

/// <summary>Platform-specific newline string (\n on Unix, \r\n on Windows).</summary>
let NewLine = Environment.NewLine

/// <summary>Number of processors available to the current process.</summary>
let ProcessorCount = Environment.ProcessorCount

/// <summary>True if the current process is 64-bit.</summary>
let Is64BitProcess = Environment.Is64BitProcess

/// <summary>True if the operating system is 64-bit.</summary>
let Is64BitOperatingSystem = Environment.Is64BitOperatingSystem

/// <summary>Gets an environment variable, returning None if not set or empty.</summary>
/// <param name="name">Environment variable name.</param>
/// <returns>Effect returning Some(value) if set, or None otherwise.</returns>
let getOption<'E> (name: string) : FIO<string option, 'E> =
    FIO.attemptExn(fun () ->
        match Environment.GetEnvironmentVariable name with
        | null
        | "" -> None
        | value -> Some value)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed None)

/// <summary>Gets an environment variable, failing with a custom error if not set.</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="onNotFound">Function to create error when variable is not set.</param>
/// <returns>Effect returning the value or failing with custom error.</returns>
let get<'E> (name: string, onNotFound: unit -> 'E) : FIO<string, 'E> =
    getOption(name).FlatMap(
        function
        | Some value -> FIO.succeed value
        | None -> FIO.fail(onNotFound()))

/// <summary>Gets an environment variable with a default value if not set.</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="defaultValue">Default value if variable is not set.</param>
/// <returns>Effect returning the value or default.</returns>
let getOrDefault<'E> (name: string, defaultValue: string) : FIO<string, 'E> =
    getOption(name).FlatMap(
        function
        | Some value -> FIO.succeed value
        | None -> FIO.succeed defaultValue)

/// <summary>Gets all environment variables as a map.</summary>
/// <returns>Effect returning a map of all environment variables.</returns>
let getAll<'E> () : FIO<Map<string, string>, 'E> =
    FIO.attemptExn(fun () ->
        Environment.GetEnvironmentVariables()
        |> Seq.cast<DictionaryEntry>
        |> Seq.map (fun de -> string de.Key, string de.Value)
        |> Map.ofSeq)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed Map.empty)

/// <summary>Gets an environment variable as an integer, failing if not set or invalid.</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="onNotFound">Function to create error when variable is not set.</param>
/// <param name="onParseError">Function to create error when value cannot be parsed as integer.</param>
/// <returns>Effect returning the integer value or failing with custom error.</returns>
let getInt<'E> (name: string, onNotFound: unit -> 'E, onParseError: string -> 'E) : FIO<int, 'E> =
    get(name, onNotFound).FlatMap(fun value ->
        match Int32.TryParse value with
        | true, intValue -> FIO.succeed intValue
        | false, _ -> FIO.fail(onParseError value))

/// <summary>Gets an environment variable as an integer with a default value.</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="defaultValue">Default value if variable is not set or invalid.</param>
/// <returns>Effect returning the integer value or default.</returns>
let getIntOrDefault<'E> (name: string, defaultValue: int) : FIO<int, 'E> =
    getOption(name).FlatMap(fun opt ->
        match opt with
        | Some value ->
            match Int32.TryParse value with
            | true, intValue -> FIO.succeed intValue
            | false, _ -> FIO.succeed defaultValue
        | None -> FIO.succeed defaultValue)

/// <summary>Gets an environment variable as a boolean, failing if not set. Recognizes "true", "1", "yes" as true and "false", "0", "no" as false (case-insensitive).</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="onNotFound">Function to create error when variable is not set.</param>
/// <param name="onParseError">Function to create error when value cannot be parsed as boolean.</param>
/// <returns>Effect returning the boolean value or failing with custom error.</returns>
let getBool<'E> (name: string, onNotFound: unit -> 'E, onParseError: string -> 'E) : FIO<bool, 'E> =
    get(name, onNotFound).FlatMap(fun value ->
        match value.ToLowerInvariant() with
        | "true"
        | "1"
        | "yes" -> FIO.succeed true
        | "false"
        | "0"
        | "no" -> FIO.succeed false
        | _ -> FIO.fail(onParseError value))

/// <summary>Gets an environment variable as a boolean with a default value. Recognizes "true", "1", "yes" as true and "false", "0", "no" as false (case-insensitive).</summary>
/// <param name="name">Environment variable name.</param>
/// <param name="defaultValue">Default value if variable is not set or invalid.</param>
/// <returns>Effect returning the boolean value or default.</returns>
let getBoolOrDefault<'E> (name: string, defaultValue: bool) : FIO<bool, 'E> =
    getOption(name).FlatMap(fun opt ->
        match opt with
        | Some value ->
            match value.ToLowerInvariant() with
            | "true"
            | "1"
            | "yes" -> FIO.succeed true
            | "false"
            | "0"
            | "no" -> FIO.succeed false
            | _ -> FIO.succeed defaultValue
        | None -> FIO.succeed defaultValue)

/// <summary>Checks if an environment variable is set (not null and not empty).</summary>
/// <param name="name">Environment variable name.</param>
/// <returns>Effect returning true if variable is set, false otherwise.</returns>
let isSet<'E> (name: string) : FIO<bool, 'E> =
    getOption(name).FlatMap(
        fun opt -> FIO.succeed(Option.isSome opt))

/// <summary>Expands environment variable references in a string. On Windows, expands %VAR% syntax. On Unix, expands $VAR syntax.</summary>
/// <param name="input">String containing environment variable references.</param>
/// <returns>Effect returning the expanded string.</returns>
let expandVariables<'E> (input: string) : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Environment.ExpandEnvironmentVariables input)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed input)

/// <summary>Gets the current working directory.</summary>
/// <returns>Effect returning the current directory path.</returns>
let currentDirectory<'E> () : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Environment.CurrentDirectory)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed ".")

/// <summary>Gets the NetBIOS name of the local computer.</summary>
/// <returns>Effect returning the machine name.</returns>
let machineName<'E> () : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Environment.MachineName)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// <summary>Gets the user name of the person currently logged on to the operating system.</summary>
/// <returns>Effect returning the user name.</returns>
let userName<'E> () : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Environment.UserName)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// <summary>Gets the command-line arguments for the current process. The first element is the executable name.</summary>
/// <returns>Effect returning the command-line arguments array.</returns>
let getCommandLineArgs<'E> () : FIO<string array, 'E> =
    FIO.attemptExn(fun () -> Environment.GetCommandLineArgs())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed [||])

/// <summary>Gets the path to a system special folder.</summary>
/// <param name="folder">Special folder to get path for (e.g., Desktop, Documents, ApplicationData).</param>
/// <returns>Effect returning the folder path, or empty string if not available.</returns>
let getFolderPath<'E> (folder: Environment.SpecialFolder) : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Environment.GetFolderPath folder)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")

/// <summary>Gets the path to the system's temporary folder.</summary>
/// <returns>Effect returning the temp folder path.</returns>
let getTempPath<'E> () : FIO<string, 'E> =
    FIO.attemptExn(fun () -> Path.GetTempPath())
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")
