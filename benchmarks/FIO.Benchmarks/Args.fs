/// <summary>Provides command-line argument parsing for benchmark configuration.</summary>
module internal FIO.Benchmarks.Args

open FIO.Runtime
open FIO.Benchmarks.Suite

open Argu

open System
open System.IO

/// <summary>Represents all supported command-line arguments.</summary>
type private Arguments =
    /// <summary>Represents the Direct runtime selection.</summary>
    | [<Unique>] Direct_Runtime
    /// <summary>Represents the Cooperative runtime selection with worker configuration.</summary>
    | [<Unique>] Cooperative_Runtime of ewc: int * ews: int * bwc: int
    /// <summary>Represents the Concurrent runtime selection with worker configuration.</summary>
    | [<Unique>] Concurrent_Runtime of ewc: int * ews: int * bwc: int
    /// <summary>Represents the number of benchmark runs.</summary>
    | [<Unique>] Runs of runs: int
    /// <summary>Represents the number of warmup runs before measured runs.</summary>
    | [<Unique>] Warmup_Runs of runs: int
    /// <summary>Represents the ultra-minimal benchmark logging flag.</summary>
    | [<Unique>] Quiet
    /// <summary>Represents the detailed per-run benchmark logging flag.</summary>
    | [<Unique>] Detailed
    /// <summary>Represents the actor count increment value and application count.</summary>
    | [<Unique>] Actor_Increment of actorInc: int * times: int
    /// <summary>Represents the round count increment value and application count.</summary>
    | [<Unique>] Round_Increment of roundInc: int * times: int
    /// <summary>Represents the Pingpong benchmark with specified round count.</summary>
    | [<Unique>] Pingpong of roundCount: int
    /// <summary>Represents the Threadring benchmark with specified actor and round counts.</summary>
    | [<Unique>] Threadring of actorCount: int * roundCount: int
    /// <summary>Represents the Big benchmark with specified actor and round counts.</summary>
    | [<Unique>] Big of actorCount: int * roundCount: int
    /// <summary>Represents the Bang benchmark with specified actor and round counts.</summary>
    | [<Unique>] Bang of actorCount: int * roundCount: int
    /// <summary>Represents the Fork benchmark with specified actor count.</summary>
    | [<Unique>] Fork of actorCount: int
    /// <summary>Represents the flag to save benchmark results to CSV output.</summary>
    | [<Unique>] Save
    /// <summary>Represents the absolute path for saving CSV results.</summary>
    | [<Unique>] Save_Path of absolutePath: string

    /// <summary>Returns the usage description for this argument.</summary>
    /// <returns>The usage text string.</returns>
    interface IArgParserTemplate with
        /// <summary>Returns the usage description for this argument.</summary>
        /// <returns>The usage text string.</returns>
        member this.Usage =
            match this with
            | Direct_Runtime -> "specify Direct runtime"
            | Cooperative_Runtime _ -> "specify Cooperative runtime with ewc, ews and bwc"
            | Concurrent_Runtime _ -> "specify Concurrent runtime with ewc, ews and bwc"
            | Runs _ -> "specify number of runs for each benchmark (must be >= 1)"
            | Warmup_Runs _ -> "specify number of warmup runs before measured runs (must be >= 0)"
            | Quiet -> "enable ultra-minimal benchmark output"
            | Detailed -> "enable detailed per-run benchmark output (cannot be combined with --quiet)"
            | Actor_Increment _ -> "specify actor increment and increment count (both must be >= 0)"
            | Round_Increment _ -> "specify round increment and increment count (both must be >= 0)"
            | Pingpong _ -> "specify number of rounds for Pingpong benchmark (must be >= 1)"
            | Threadring _ -> "specify number of actors and rounds for Threadring benchmark (actors >= 2, rounds >= 1)"
            | Big _ -> "specify number of actors and rounds for Big benchmark (actors >= 2, rounds >= 1)"
            | Bang _ -> "specify number of actors and rounds for Bang benchmark (actors >= 1, rounds >= 1)"
            | Fork _ -> "specify number of actors for Fork benchmark (actors >= 1)"
            | Save -> "save benchmark results to CSV files"
            | Save_Path _ -> "specify absolute output directory for CSV results (requires --save)"

/// <summary>Represents a parse outcome for benchmark CLI parsing.</summary>
type internal ParseOutcome =
    /// <summary>Represents successfully parsed benchmark arguments.</summary>
    | Parsed of BenchmarkArgs
    /// <summary>Represents a help request with usage text to display.</summary>
    | HelpRequested of usageText: string
    /// <summary>Represents invalid arguments with error and usage text.</summary>
    | InvalidArgs of errorText: string * usageText: string

/// <summary>Returns the Argu parser instance for benchmark command-line arguments.</summary>
/// <returns>The configured argument parser.</returns>
let private parser =
    ArgumentParser.Create<Arguments>(programName = "FIO.Benchmarks")

/// <summary>Returns the usage text for the benchmark CLI.</summary>
/// <returns>Formatted usage text string.</returns>
let internal usageText () = parser.PrintUsage()

/// <summary>Returns the default save path for benchmark CSV results.</summary>
/// <returns>Absolute path to the default results directory.</returns>
let private defaultSavePath () =
    let projectDirPath =
        Directory.GetCurrentDirectory()
        |> Directory.GetParent
        |> (fun di -> di.Parent)
        |> (fun di -> di.Parent)
        |> function
            | null -> failwith "Unexpected directory structure!"
            | di -> di.FullName

    Path.Combine(projectDirPath, "results")

/// <summary>Creates an InvalidArgs parse outcome with the current usage text.</summary>
/// <param name="errorText">Error message describing the validation failure.</param>
/// <returns>InvalidArgs parse outcome.</returns>
let private invalid errorText = InvalidArgs(errorText, usageText ())

/// <summary>Represents a computation expression builder for chaining Result values.</summary>
type private ResultBuilder() =
    /// <summary>Transforms a result by applying the binder function if Ok.</summary>
    /// <param name="result">The result to bind.</param>
    /// <param name="binder">The function to apply to the Ok value.</param>
    /// <returns>The result of applying the binder, or the original Error.</returns>
    member _.Bind(result, binder) = Result.bind binder result

    /// <summary>Lifts a value into an Ok result.</summary>
    /// <param name="value">The value to wrap.</param>
    /// <returns>An Ok result containing the value.</returns>
    member _.Return value = Ok value

    /// <summary>Returns the given result unchanged.</summary>
    /// <param name="result">The result to return.</param>
    /// <returns>The original result.</returns>
    member _.ReturnFrom result = result

/// <summary>Returns the Result computation expression instance for validation pipelines.</summary>
/// <returns>The result builder instance.</returns>
let private result = ResultBuilder()

/// <summary>Returns Ok if all checks pass, or the first Error encountered.</summary>
/// <param name="checks">List of validation results to check.</param>
/// <returns>Ok if all pass, or the first Error encountered.</returns>
let private validateAll (checks: Result<unit, string> list) : Result<unit, string> =
    checks
    |> List.tryPick (function
        | Error err -> Some err
        | Ok() -> None)
    |> function
        | Some err -> Error err
        | None -> Ok()

/// <summary>Returns Ok if a value is at least the specified minimum, or Error with a descriptive message.</summary>
/// <param name="name">Parameter name for error messages.</param>
/// <param name="minimum">Minimum allowed value.</param>
/// <param name="value">Value to validate.</param>
/// <returns>Ok if valid, or Error with a descriptive message.</returns>
let private validateAtLeast (name: string) minimum value =
    if value < minimum then
        Error $"{name} must be >= {minimum}. {name} = {value}"
    else
        Ok()

/// <summary>Returns Ok if a value is non-negative, or Error with a descriptive message.</summary>
/// <param name="name">Parameter name for error messages.</param>
/// <param name="value">Value to validate.</param>
/// <returns>Ok if valid, or Error with a descriptive message.</returns>
let private validateNonNegative (name: string) value = validateAtLeast name 0 value

/// <summary>Returns Ok if at least one runtime is selected in the parsed arguments, or Error with guidance.</summary>
/// <param name="results">Parsed command-line arguments.</param>
/// <returns>Ok if at least one runtime is selected, or Error with guidance.</returns>
let private validateRuntimeSelection (results: ParseResults<Arguments>) =
    let selectionCount =
        [
            results.Contains Direct_Runtime
            results.Contains Cooperative_Runtime
            results.Contains Concurrent_Runtime
        ]
        |> List.filter id
        |> List.length

    if selectionCount >= 1 then
        Ok()
    else
        Error "Specify at least one runtime: --direct-runtime, --cooperative-runtime, or --concurrent-runtime."

/// <summary>Creates a WorkerConfig from EWC, EWS, and BWC values after validation.</summary>
/// <param name="ewc">Evaluation worker count (must be >= 1).</param>
/// <param name="ews">Evaluation worker steps (must be >= 1).</param>
/// <param name="bwc">Blocking worker count (must be >= 1).</param>
/// <returns>Ok with WorkerConfig, or Error with validation message.</returns>
let private buildWorkerConfig (ewc, ews, bwc) =
    validateAll
        [
            validateAtLeast "ewc" 1 ewc
            validateAtLeast "ews" 1 ews
            validateAtLeast "bwc" 1 bwc
        ]
    |> Result.map (fun () -> { EWC = ewc; EWS = ews; BWC = bwc; MaxFibers = None })

/// <summary>Creates the list of runtime selections from parsed arguments.</summary>
/// <param name="results">Parsed command-line arguments.</param>
/// <returns>Ok with list of runtime selections, or Error with validation message.</returns>
let private buildRuntimeSelections (results: ParseResults<Arguments>) : Result<RuntimeSelection list, string> =
    let directSelection =
        if results.Contains Direct_Runtime then
            Some Direct
        else
            None

    let cooperativeSelection =
        match results.TryGetResult Cooperative_Runtime with
        | None -> Ok None
        | Some workerConfig -> buildWorkerConfig workerConfig |> Result.map (Cooperative >> Some)

    let concurrentSelection =
        match results.TryGetResult Concurrent_Runtime with
        | None -> Ok None
        | Some workerConfig -> buildWorkerConfig workerConfig |> Result.map (Concurrent >> Some)

    result {
        let! cooperativeSelection = cooperativeSelection
        let! concurrentSelection = concurrentSelection

        return [ directSelection; cooperativeSelection; concurrentSelection ] |> List.choose id
    }

/// <summary>Returns Ok if the benchmark configuration parameters meet minimum requirements, or Error with a validation message.</summary>
/// <param name="config">Benchmark configuration to validate.</param>
/// <returns>Ok if valid, or Error with validation message.</returns>
let private validateBenchmarkConfig config =
    match config with
    | PingpongConfig roundCount -> validateAtLeast "pingpong roundCount" 1 roundCount
    | ThreadringConfig(actorCount, roundCount) ->
        validateAll
            [
                validateAtLeast "threadring actorCount" 2 actorCount
                validateAtLeast "threadring roundCount" 1 roundCount
            ]
    | BigConfig(actorCount, roundCount) ->
        validateAll
            [
                validateAtLeast "big actorCount" 2 actorCount
                validateAtLeast "big roundCount" 1 roundCount
            ]
    | BangConfig(actorCount, roundCount) ->
        validateAll
            [
                validateAtLeast "bang actorCount" 1 actorCount
                validateAtLeast "bang roundCount" 1 roundCount
            ]
    | ForkConfig actorCount -> validateAtLeast "fork actorCount" 1 actorCount

/// <summary>Returns validated benchmark configurations from parsed arguments.</summary>
/// <param name="results">Parsed command-line arguments.</param>
/// <returns>Ok with list of benchmark configs, or Error if none specified or invalid.</returns>
let private collectBenchmarkConfigs (results: ParseResults<Arguments>) : Result<BenchmarkConfig list, string> =
    let configs =
        [
            results.TryGetResult Pingpong |> Option.map PingpongConfig
            results.TryGetResult Threadring |> Option.map ThreadringConfig
            results.TryGetResult Big |> Option.map BigConfig
            results.TryGetResult Bang |> Option.map BangConfig
            results.TryGetResult Fork |> Option.map ForkConfig
        ]
        |> List.choose id

    if configs.IsEmpty then
        Error "Specify at least one benchmark: --pingpong, --threadring, --big, --bang, or --fork."
    else
        configs
        |> List.map validateBenchmarkConfig
        |> validateAll
        |> Result.map (fun () -> configs)

/// <summary>Returns the resolved save path for CSV output, using a default if not specified.</summary>
/// <param name="saveToCsv">Whether CSV saving is enabled.</param>
/// <param name="savePathOpt">Optional explicit save path.</param>
/// <returns>Ok with resolved absolute path, or Error if path is invalid.</returns>
let private resolveSavePath (saveToCsv: bool) (savePathOpt: string option) : Result<string, string> =
    match savePathOpt with
    | Some _ when not saveToCsv -> Error "--save-path requires --save."
    | Some path when not (Path.IsPathFullyQualified path) -> Error "--save-path must be an absolute path."
    | _ -> Ok(savePathOpt |> Option.defaultValue (defaultSavePath ()))

/// <summary>Returns a concise error summary from an Argu parse exception message.</summary>
/// <param name="message">Raw exception message from Argu.</param>
/// <returns>First line of the error summary, or a default message.</returns>
let private parseErrorSummary (message: string) =
    if String.IsNullOrWhiteSpace message then
        "Invalid command line arguments."
    else
        let usageIndex = message.IndexOf("USAGE:", StringComparison.OrdinalIgnoreCase)

        let rawSummary =
            if usageIndex > 0 then
                message.Substring(0, usageIndex).Trim()
            else
                message.Trim()

        if String.IsNullOrWhiteSpace rawSummary then
            "Invalid command line arguments."
        else
            rawSummary.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.head

/// <summary>Creates validated BenchmarkArgs from parsed command-line results.</summary>
/// <param name="results">Parsed command-line arguments.</param>
/// <returns>Ok with validated BenchmarkArgs, or Error with validation message.</returns>
let private buildBenchmarkArgs (results: ParseResults<Arguments>) : Result<BenchmarkArgs, string> =
    result {
        do! validateRuntimeSelection results
        let! runtimes = buildRuntimeSelections results

        let runs = results.TryGetResult Runs |> Option.defaultValue 1
        let warmupRuns = results.TryGetResult Warmup_Runs |> Option.defaultValue 1
        let quiet = results.Contains Quiet
        let detailed = results.Contains Detailed
        let actorInc = results.TryGetResult Actor_Increment |> Option.defaultValue (0, 0)
        let roundInc = results.TryGetResult Round_Increment |> Option.defaultValue (0, 0)
        let saveToCsv = results.Contains Save
        let savePathOpt = results.TryGetResult Save_Path
        let actorIncValue, actorIncTimes = actorInc
        let roundIncValue, roundIncTimes = roundInc

        do!
            if quiet && detailed then
                Error "--quiet and --detailed cannot be used together."
            else
                Ok()

        do!
            validateAll
                [
                    validateAtLeast "runs" 1 runs
                    validateNonNegative "warmup-runs" warmupRuns
                    validateNonNegative "actor-increment value" actorIncValue
                    validateNonNegative "actor-increment times" actorIncTimes
                    validateNonNegative "round-increment value" roundIncValue
                    validateNonNegative "round-increment times" roundIncTimes
                ]

        let! savePath = resolveSavePath saveToCsv savePathOpt
        let! benchmarkConfigs = collectBenchmarkConfigs results

        return
            {
                Runtimes = runtimes
                Runs = runs
                WarmupRuns = warmupRuns
                Quiet = quiet
                Detailed = detailed
                ActorIncrement = actorInc
                RoundIncrement = roundInc
                BenchmarkConfigs = benchmarkConfigs
                SaveToCsv = saveToCsv
                SavePath = savePath
            }
    }

/// <summary>Returns a ParseOutcome from command-line arguments.</summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Parsed benchmark arguments configuration or parse outcome.</returns>
let parse args =
    try
        let results = parser.Parse(args, raiseOnUsage = false)

        if results.IsUsageRequested then
            HelpRequested(usageText ())
        else
            match buildBenchmarkArgs results with
            | Ok benchmarkArgs -> Parsed benchmarkArgs
            | Error errorText -> invalid errorText
    with :? ArguParseException as ex ->
        invalid (parseErrorSummary ex.Message)
