/// <summary>
/// Command-line argument parsing for plot generation.
/// </summary>
module internal FIO.Benchmarks.Plots.Args

open Argu

open System
open System.IO

/// <summary>
/// Default folder name for box plot data files.
/// </summary>
let defaultBoxPlotFolder = "boxplot_data"

/// <summary>
/// Default folder name for line plot data files.
/// </summary>
let defaultLinePlotFolder = "lineplot_data"

/// <summary>
/// Discriminated union representing supported command-line arguments.
/// </summary>
type private Arguments =
    /// <summary>Generate box plot visualization.</summary>
    | [<Unique; AltCommandLine("-b")>] BoxPlot
    /// <summary>Generate line plot visualization.</summary>
    | [<Unique; AltCommandLine("-l")>] LinePlot
    /// <summary>Path to load benchmark data files from.</summary>
    | [<Unique; AltCommandLine("-p")>] LoadPath of absolutePath: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BoxPlot -> "generate box plot"
            | LinePlot -> "generate line plot"
            | LoadPath _ ->
                "specify absolute path to load benchmark data files from (default uses benchmark subfolder in current directory)"

/// <summary>
/// Parse outcome for plotting CLI parsing.
/// </summary>
type internal ParseOutcome =
    /// <summary>Successfully parsed plot arguments.</summary>
    | Parsed of PlotArgs
    /// <summary>Help was requested, contains usage text to display.</summary>
    | HelpRequested of usageText: string
    /// <summary>Invalid arguments were provided, contains error and usage text.</summary>
    | InvalidArgs of errorText: string * usageText: string

/// <summary>
/// Argu parser instance for plot command-line arguments.
/// </summary>
let private parser =
    ArgumentParser.Create<Arguments>(programName = "FIO.Benchmarks.Plots")

/// <summary>
/// Returns the usage text for the plot CLI.
/// </summary>
/// <returns>Formatted usage text string.</returns>
let internal usageText () = parser.PrintUsage()

/// <summary>
/// Creates an InvalidArgs parse outcome with the current usage text.
/// </summary>
/// <param name="errorText">Error message describing the validation failure.</param>
/// <returns>InvalidArgs parse outcome.</returns>
let private invalid errorText = InvalidArgs(errorText, usageText ())

/// <summary>
/// Extracts a concise error summary from an Argu parse exception message.
/// </summary>
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

/// <summary>
/// Builds and validates PlotArgs from parsed command-line results.
/// </summary>
/// <param name="results">Parsed command-line arguments.</param>
/// <returns>Ok with validated PlotArgs, or Error with validation message.</returns>
let private buildPlotArgs (results: ParseResults<Arguments>) : Result<PlotArgs, string> =
    let boxPlotSelected = results.Contains BoxPlot
    let linePlotSelected = results.Contains LinePlot

    let plotTypeResult =
        match boxPlotSelected, linePlotSelected with
        | true, false -> Ok PlotType.BoxPlot
        | false, true -> Ok PlotType.LinePlot
        | false, false -> Error "Specify exactly one plot type: --boxplot or --lineplot."
        | true, true -> Error "Specify only one plot type: --boxplot or --lineplot."

    match plotTypeResult with
    | Error err -> Error err
    | Ok plotType ->
        let loadPath =
            match results.TryGetResult LoadPath with
            | Some path when not (Path.IsPathFullyQualified path) -> Error "--loadpath must be an absolute path."
            | Some path -> Ok path
            | None ->
                let subfolder =
                    match plotType with
                    | PlotType.BoxPlot -> defaultBoxPlotFolder
                    | PlotType.LinePlot -> defaultLinePlotFolder

                Ok(Path.Combine(Directory.GetCurrentDirectory(), subfolder))

        match loadPath with
        | Error err -> Error err
        | Ok absolutePath -> Ok { PlotType = plotType; LoadPath = absolutePath }

/// <summary>
/// Parses command-line arguments into a PlotArgs configuration.
/// </summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Parsed plot arguments configuration or parse outcome.</returns>
let parse args =
    try
        let results = parser.Parse(args, raiseOnUsage = false)

        if results.IsUsageRequested then
            HelpRequested(usageText ())
        else
            match buildPlotArgs results with
            | Ok plotArgs -> Parsed plotArgs
            | Error errorText -> invalid errorText
    with :? ArguParseException as ex ->
        invalid (parseErrorSummary ex.Message)
