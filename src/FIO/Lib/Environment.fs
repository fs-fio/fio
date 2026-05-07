/// <summary>Provides environment variable access and system information as FIO effects.</summary>
[<RequireQualifiedAccess>]
module FIO.Environment.Environment

open FIO.DSL

open System
open System.IO
open System.Collections

/// <summary>Transforms an exception by re-raising it, used as the error-mapping function when side effects cannot fail with a typed error.</summary>
/// <param name="ex">The exception to re-raise.</param>
/// <returns>Never returns; always throws <paramref name="ex"/>.</returns>
let inline private rethrow (ex: exn) : 'E = raise ex

/// <summary>Returns the platform-specific newline string.</summary>
/// <returns>An effect that completes with <c>\n</c> on Unix and <c>\r\n</c> on Windows.</returns>
let inline newLine<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.NewLine), rethrow)

/// <summary>Returns the number of processors available to the current process.</summary>
/// <returns>An effect that completes with the processor count reported by the runtime.</returns>
let inline processorCount<'E> () : FIO<int, 'E> =
    FIO.attempt ((fun () -> Environment.ProcessorCount), rethrow)

/// <summary>Returns whether the current process is running as 64-bit.</summary>
/// <returns>An effect that completes with <c>true</c> when the process is 64-bit; <c>false</c> otherwise.</returns>
let inline is64BitProcess<'E> () : FIO<bool, 'E> =
    FIO.attempt ((fun () -> Environment.Is64BitProcess), rethrow)

/// <summary>Returns whether the host operating system is 64-bit.</summary>
/// <returns>An effect that completes with <c>true</c> when the OS is 64-bit; <c>false</c> otherwise.</returns>
let inline is64BitOperatingSystem<'E> () : FIO<bool, 'E> =
    FIO.attempt ((fun () -> Environment.Is64BitOperatingSystem), rethrow)

/// <summary>Returns an environment variable's value as an option, treating missing or empty values as <c>None</c>.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <returns>An effect that completes with <c>Some value</c> when the variable is set to a non-empty string, or <c>None</c> otherwise.</returns>
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

/// <summary>Returns an environment variable's value, failing through a supplied function when it is not set.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="onNotFound">A function producing the typed error when the variable is missing or empty.</param>
/// <returns>An effect that completes with the variable's value, or fails with the result of <paramref name="onNotFound"/>.</returns>
let inline get<'E> (name: string, onNotFound: unit -> 'E) : FIO<string, 'E> =
    getOption(name)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.fail (onNotFound ())
        )

/// <summary>Returns an environment variable's value, falling back to a default when it is not set.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="defaultValue">The value to return when the variable is missing or empty.</param>
/// <returns>An effect that completes with the variable's value, or with <paramref name="defaultValue"/> when not set.</returns>
let inline getOrDefault<'E> (name: string, defaultValue: string) : FIO<string, 'E> =
    getOption(name)
        .FlatMap(
            function
            | Some value -> FIO.succeed value
            | None -> FIO.succeed defaultValue
        )

/// <summary>Returns all environment variables as a map of names to values.</summary>
/// <returns>An effect that completes with the full set of environment variables as a <c>Map</c>; an empty map is returned if access fails.</returns>
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

/// <summary>Returns an environment variable parsed as a 32-bit integer, failing through supplied functions on error.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="onNotFound">A function producing the typed error when the variable is missing.</param>
/// <param name="onParseError">A function from the raw string value to the typed error when parsing fails.</param>
/// <returns>An effect that completes with the parsed integer, or fails when the variable is missing or unparseable.</returns>
let inline getInt<'E> (name: string, onNotFound: unit -> 'E, onParseError: string -> 'E) : FIO<int, 'E> =
    get(name, onNotFound)
        .FlatMap(fun value ->
            match Int32.TryParse value with
            | true, intValue -> FIO.succeed intValue
            | false, _ -> FIO.fail (onParseError value))

/// <summary>Returns an environment variable parsed as a 32-bit integer, falling back to a default on missing or invalid values.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="defaultValue">The integer value to return when the variable is missing or unparseable.</param>
/// <returns>An effect that completes with the parsed integer, or with <paramref name="defaultValue"/> on failure.</returns>
let inline getIntOrDefault<'E> (name: string, defaultValue: int) : FIO<int, 'E> =
    getOption(name)
        .FlatMap(fun opt ->
            match opt with
            | Some value ->
                match Int32.TryParse value with
                | true, intValue -> FIO.succeed intValue
                | false, _ -> FIO.succeed defaultValue
            | None -> FIO.succeed defaultValue)

/// <summary>Returns an environment variable parsed as a boolean, failing through supplied functions on error.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="onNotFound">A function producing the typed error when the variable is missing.</param>
/// <param name="onParseError">A function from the raw string value to the typed error when parsing fails.</param>
/// <returns>An effect that completes with the parsed boolean, or fails when the variable is missing or unparseable.</returns>
/// <remarks>The strings <c>"true"</c>, <c>"1"</c>, <c>"yes"</c> and <c>"false"</c>, <c>"0"</c>, <c>"no"</c> are recognized case-insensitively.</remarks>
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

/// <summary>Returns an environment variable parsed as a boolean, falling back to a default on missing or invalid values.</summary>
/// <param name="name">The environment variable name to look up.</param>
/// <param name="defaultValue">The boolean value to return when the variable is missing or unparseable.</param>
/// <returns>An effect that completes with the parsed boolean, or with <paramref name="defaultValue"/> on failure.</returns>
/// <remarks>The strings <c>"true"</c>, <c>"1"</c>, <c>"yes"</c> and <c>"false"</c>, <c>"0"</c>, <c>"no"</c> are recognized case-insensitively.</remarks>
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

/// <summary>Returns whether the named environment variable is set to a non-empty value.</summary>
/// <param name="name">The environment variable name to check.</param>
/// <returns>An effect that completes with <c>true</c> when the variable is set; <c>false</c> otherwise.</returns>
let inline isSet<'E> (name: string) : FIO<bool, 'E> =
    getOption(name).FlatMap(fun opt -> FIO.succeed (Option.isSome opt))

/// <summary>Transforms a string by expanding embedded environment variable references.</summary>
/// <param name="input">The input string; references use <c>%VAR%</c> on Windows and <c>$VAR</c> on Unix.</param>
/// <returns>An effect that completes with the expanded string, or with the original input if expansion fails.</returns>
let inline expandVariables<'E> (input: string) : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.ExpandEnvironmentVariables input), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed input)

/// <summary>Returns the current working directory of the process.</summary>
/// <returns>An effect that completes with the working directory path, or with <c>"."</c> if access fails.</returns>
let inline currentDirectory<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.CurrentDirectory), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed ".")

/// <summary>Returns the local machine name.</summary>
/// <returns>An effect that completes with the machine name, or with <c>"unknown"</c> if access fails.</returns>
let inline machineName<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.MachineName), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// <summary>Returns the current user's account name.</summary>
/// <returns>An effect that completes with the user name, or with <c>"unknown"</c> if access fails.</returns>
let inline userName<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.UserName), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "unknown")

/// <summary>Returns the command-line arguments passed to the current process.</summary>
/// <returns>An effect that completes with the argument array (the first element is the executable name), or with an empty array on failure.</returns>
let inline getCommandLineArgs<'E> () : FIO<string array, 'E> =
    FIO.attempt ((fun () -> Environment.GetCommandLineArgs()), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed [||])

/// <summary>Returns the path to a system special folder.</summary>
/// <param name="folder">The special folder identifier.</param>
/// <returns>An effect that completes with the folder path, or with the empty string when no path is configured.</returns>
let inline getFolderPath<'E> (folder: Environment.SpecialFolder) : FIO<string, 'E> =
    FIO.attempt ((fun () -> Environment.GetFolderPath folder), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")

/// <summary>Returns the path to the system temporary folder.</summary>
/// <returns>An effect that completes with the temporary folder path, or with the empty string on failure.</returns>
let inline getTempPath<'E> () : FIO<string, 'E> =
    FIO.attempt ((fun () -> Path.GetTempPath()), id)
    |> fun eff -> eff.CatchAll(fun _ -> FIO.succeed "")
