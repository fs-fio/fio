/// Environment variable access and system information as FIO effects.
[<RequireQualifiedAccess>]
module FIO.Environment.Environment

open FIO.DSL

open System
open System.IO
open System.Collections

/// Platform-specific newline string (\n on Unix, \r\n on Windows).
let NewLine = Environment.NewLine

/// Number of processors available to the current process.
let ProcessorCount = Environment.ProcessorCount

/// True if the current process is 64-bit.
let Is64BitProcess = Environment.Is64BitProcess

/// True if the operating system is 64-bit.
let Is64BitOperatingSystem = Environment.Is64BitOperatingSystem

/// Gets an environment variable, returning None if not set or empty.
let inline getOption<'E> (name: string) : FIO<string option, 'E> =
    FIO.attempt (
        (fun () ->
            match Environment.GetEnvironmentVariable name with
            | null
            | "" -> None
            | value -> Some value),
        id
    )
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed None)

/// Gets an environment variable, failing if not set.
/// <param name="onNotFound">Function to create error when variable is not set.</param>
let inline get<'E> (name: string, onNotFound: unit -> 'E) : FIO<string, 'E> =
    getOption(name)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.fail (onNotFound ())
        )

/// Gets an environment variable with a fallback if not set.
let inline getOrDefault<'E> (name: string, defaultValue: string) : FIO<string, 'E> =
    getOption(name)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.succeed defaultValue
        )

/// Gets all environment variables as a map.
let inline getAll<'E> () : FIO<Map<string, string>, 'E> =
    FIO.attempt (
        (fun () ->
            Environment.GetEnvironmentVariables()
            |> Seq.cast<DictionaryEntry>
            |> Seq.map (fun de -> string de.Key, string de.Value)
            |> Map.ofSeq),
        id
    )
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed Map.empty)

/// Gets an environment variable as an integer, failing if not set or invalid.
/// <param name="onNotFound">Function to create error when variable is not set.</param>
/// <param name="onParseError">Function to create error when value cannot be parsed as integer.</param>
let inline getInt<'E> (name: string, onNotFound: unit -> 'E, onParseError: string -> 'E) : FIO<int, 'E> =
    get(name, onNotFound)
        .FlatMap(fun value ->
            match Int32.TryParse value with
            | true, intValue -> FIO.succeed intValue
            | false, _ -> FIO.fail (onParseError value))

/// Gets an environment variable as an integer with a default value.
let inline getIntOrDefault<'E> (name: string, defaultValue: int) : FIO<int, 'E> =
    getOption(name)
        .FlatMap(fun opt ->
            match opt with
            | Some value ->
                match Int32.TryParse value with
                | true, intValue -> FIO.succeed intValue
                | false, _ -> FIO.succeed defaultValue
            | None -> FIO.succeed defaultValue)

/// Gets an environment variable as a boolean, failing if not set. Recognizes "true", "1", "yes" as true and "false", "0", "no" as false (case-insensitive).
/// <param name="onNotFound">Function to create error when variable is not set.</param>
/// <param name="onParseError">Function to create error when value cannot be parsed as boolean.</param>
let inline getBool<'E> (name: string, onNotFound: unit -> 'E, onParseError: string -> 'E) : FIO<bool, 'E> =
    get(name, onNotFound)
        .FlatMap(fun value ->
            match value.ToLowerInvariant() with
            | "true"
            | "1"
            | "yes" -> FIO.succeed true
            | "false"
            | "0"
            | "no" -> FIO.succeed false
            | _ -> FIO.fail (onParseError value))

/// Gets an environment variable as a boolean with a default value. Recognizes "true", "1", "yes" as true and "false", "0", "no" as false (case-insensitive).
let inline getBoolOrDefault<'E> (name: string, defaultValue: bool) : FIO<bool, 'E> =
    getOption(name)
        .FlatMap(fun opt ->
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

/// Checks if an environment variable is set.
let inline isSet<'E> (name: string) : FIO<bool, 'E> =
    getOption(name).FlatMap(fun opt -> FIO.succeed (Option.isSome opt))

/// Expands environment variable references (%VAR% on Windows, $VAR on Unix).
let inline expandVariables<'E> (input: string) : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.ExpandEnvironmentVariables input), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed input)

/// Gets the current working directory.
let inline currentDirectory<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.CurrentDirectory), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed ".")

/// Gets the local machine name.
let inline machineName<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.MachineName), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// Gets the current user name.
let inline userName<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.UserName), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// Gets the command-line arguments. The first element is the executable name.
let inline getCommandLineArgs<'E> () : FIO<string array, 'E> =
    FIO.attempt ((fun () -> Environment.GetCommandLineArgs()), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed [||])

/// Gets the path to a system special folder.
let inline getFolderPath<'E> (folder: Environment.SpecialFolder) : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.GetFolderPath folder), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")

/// Gets the temporary folder path.
let inline getTempPath<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Path.GetTempPath()), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")
